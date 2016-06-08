%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Utility functions used in subscriptions tests.
%%% @end
%%%-------------------------------------------------------------------
-module(subscriptions_test_utils).
-author("Michał Zmuda").

-include("registered_names.hrl").
-include("subscriptions_test_utils.hrl").
-include("subscriptions/subscriptions.hrl").
-include_lib("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([save/3, update_document/4, delete_document/3, get_rev/3,
    create_provider/3, call_worker/2]).
-export([expectation/2, public_only_user_expectation/2, group_expectation/8,
    privileges_as_binaries/1, expectation_with_rev/2]).
-export([verify_messages_present/2, verify_messages_absent/2, init_messages/3,
    flush_messages/2, flush/0, verify_messages/3]).


call_worker(Node, Req) ->
    rpc:call(Node, worker_proxy, call, [?SUBSCRIPTIONS_WORKER_NAME, Req]).

%%%===================================================================
%%% Datastore setup
%%%===================================================================

save(Node, ID, Value) ->
    ?assertMatch({ok, ID}, rpc:call(Node, element(1, Value), save,
        [#document{key = ID, value = Value}])).

update_document(Node, Model, ID, Diff) ->
    ?assertMatch({ok, _}, rpc:call(Node, Model, update, [ID, Diff])).

delete_document(Node, Model, ID) ->
    ?assertMatch(ok, rpc:call(Node, Model, delete, [ID])).

get_rev(Node, Model, ID) ->
    Result = rpc:call(Node, Model, get, [ID]),
    ?assertMatch({ok, _}, Result),
    {ok, #document{rev = Rev}} = Result,
    Rev.

create_provider(Node, Name, Spaces) ->
    {_, CSRFile, _} = generate_cert_files(),
    {ok, CSR} = file:read_file(CSRFile),
    Params = [Name, [<<"127.0.0.1">>], <<"https://127.0.0.1:443">>, CSR],
    {ok, ID, _} = rpc:call(Node, provider_logic, create, Params),
    {ok, ID} = rpc:call(Node, provider, update, [ID, #{spaces => Spaces}]),
    ID.


generate_cert_files() ->
    {MegaSec, Sec, MiliSec} = erlang:now(),
    Prefix = lists:foldl(fun(Int, Acc) ->
        Acc ++ integer_to_list(Int) end, "provider", [MegaSec, Sec, MiliSec]),
    KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
    CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
    CertFile = filename:join(?TEMP_DIR, Prefix ++ "_cert.pem"),
    os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
    os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
    {KeyFile, CSRFile, CertFile}.


%%%===================================================================
%%% Message expectations
%%%===================================================================

expectation(ID, #space{name = Name, providers_supports = Supports,
    groups = Groups, users = Users}) ->
    space_expectation(ID, Name, Users, Groups, Supports);
expectation(ID, #onedata_user{name = Name, groups = Groups, space_names = SpaceNames,
    default_space = DefaultSpace, effective_groups = EGroups}) ->
    user_expectation(ID, Name, maps:to_list(SpaceNames), Groups, EGroups, case DefaultSpace of
                                                                              undefined -> <<"undefined">>;
                                                                              _ -> DefaultSpace
                                                                          end);
expectation(ID, #user_group{name = Name, type = Type, users = Users, spaces = Spaces,
    effective_users = EUsers, nested_groups = NGroups, parent_groups = PGroups}) ->
    group_expectation(ID, Name, Type, Users, EUsers, Spaces, NGroups, PGroups).

space_expectation(ID, Name, Users, Groups, Supports) ->
    [{<<"id">>, ID}, {<<"space">>, [
        {<<"id">>, ID},
        {<<"name">>, Name},
        {<<"providers_supports">>, Supports},
        {<<"users">>, privileges_as_binaries(Users)},
        {<<"groups">>, privileges_as_binaries(Groups)}
    ]}].

user_expectation(ID, Name, Spaces, Groups, EGroups, DefaultSpace) ->
    [{<<"id">>, ID}, {<<"user">>, [
        {<<"name">>, Name},
        {<<"space_names">>, Spaces},
        {<<"group_ids">>, Groups},
        {<<"effective_group_ids">>, EGroups},
        {<<"default_space">>, DefaultSpace},
        {<<"public_only">>, false}
    ]}].

public_only_user_expectation(ID, Name) ->
    [{<<"id">>, ID}, {<<"user">>, [
        {<<"name">>, Name},
        {<<"space_ids">>, []},
        {<<"group_ids">>, []},
        {<<"effective_group_ids">>, []},
        {<<"default_space">>, <<"undefined">>},
        {<<"public_only">>, true}
    ]}].

group_expectation(ID, Name, Type, Users, EUsers, Spaces, NGroups, PGroups) ->
    [{<<"id">>, ID}, {<<"group">>, [
        {<<"name">>, Name},
        {<<"type">>, atom_to_binary(Type, latin1)},
        {<<"spaces">>, Spaces},
        {<<"users">>, privileges_as_binaries(Users)},
        {<<"effective_users">>, privileges_as_binaries(EUsers)},
        {<<"nested_groups">>, privileges_as_binaries(NGroups)},
        {<<"parent_groups">>, PGroups}
    ]}].

privileges_as_binaries(IDsWithPrivileges) ->
    lists:map(fun({ID, Privileges}) ->
        {ID, lists:map(fun(Privilege) ->
            atom_to_binary(Privilege, latin1)
                       end, Privileges)}
              end, IDsWithPrivileges).

expectation_with_rev(Revs, Expectation) ->
    [{<<"revs">>, Revs} | Expectation].


%%%===================================================================
%%% Internal: Message presence/absence verification
%%%===================================================================

verify_messages_present(Context, Expected) ->
    verify_messages_present(Context, Expected, ?MESSAGES_RECEIVE_ATTEMPTS).

verify_messages_present(Context, Expected, AttemptsLimit) ->
    verify_messages(Context, AttemptsLimit, Expected, []).

verify_messages_absent(Context, Forbidden) ->
    verify_messages(Context, ?MESSAGES_RECEIVE_ATTEMPTS, [], Forbidden).

init_messages(Node, ProviderID, Users) ->
    call_worker(Node, {add_connection, ProviderID, self()}),

    Start = case rpc:call(Node, changes_cache, newest_seq, []) of
                {ok, Val} -> Val; _ -> 0
            end,


    #subs_ctx{node = Node, provider = ProviderID,
        users = Users, resume_at = Start, missing = []}.

flush_messages(Context, LastExpected) ->
    UpdatedContext = verify_messages(Context#subs_ctx{
        %% if changes were actually send before init, we could miss last expected
        resume_at = Context#subs_ctx.resume_at - 3
    }, [LastExpected], []),
    flush(),
    UpdatedContext.

flush() ->
    receive _ -> flush() after ?MESSAGES_WAIT_TIMEOUT -> ok end.

verify_messages(Context, Expected, Forbidden) ->
    verify_messages(Context, ?MESSAGES_RECEIVE_ATTEMPTS, Expected, Forbidden).

verify_messages(Context, _, [], []) ->
    Context;
verify_messages(Context, 0, Expected, _) ->
    ?assertMatch(Expected, []),
    Context;
verify_messages(Context, Retries, Expected, Forbidden) ->
    #subs_ctx{node = Node, provider = ProviderID,
        users = Users, resume_at = ResumeAt, missing = Missing} = Context,

    call_worker(Node, {update_users, ProviderID, Users}),
    call_worker(Node, {update_missing_seq, ProviderID, ResumeAt, Missing}),
    All = lists:append(get_messages()),

    ct:run("~p", [All, Expected, Forbidden]),

    Seqs = extract_seqs(All),
    NextResumeAt = largest([ResumeAt | Seqs]),
    NewExpectedSeqs = new_expected_seqs(NextResumeAt, ResumeAt),
    NextMissing = (Missing ++ NewExpectedSeqs) -- Seqs,
    NextContext = Context#subs_ctx{
        resume_at = NextResumeAt,
        missing = NextMissing
    },

    ?assertMatch(Forbidden, remove_matched_expectations(Forbidden, All)),
    RemainingExpected = remove_matched_expectations(Expected, All),
    verify_messages(NextContext, Retries - 1, RemainingExpected, Forbidden).

get_messages() ->
    receive {push, Messages} ->
        [json_utils:decode(Messages)]
    after ?MESSAGES_WAIT_TIMEOUT -> [] end.


new_expected_seqs(NextResumeAt, ResumeAt) ->
    case NextResumeAt > (ResumeAt + 1) of
        true -> lists:seq(ResumeAt + 1, NextResumeAt);
        false -> []
    end.

largest(List) ->
    hd(lists:reverse(lists:usort(List))).

extract_seqs(Messages) ->
    lists:map(fun(Message) ->
        proplists:get_value(<<"seq">>, Message, -2)
              end, Messages).

remove_matched_expectations(Expected, Messages) ->
    lists:filter(fun(Exp) ->
        lists:all(fun(Msg) -> length(Exp -- Msg) =/= 0 end, Messages)
                 end, Expected).

