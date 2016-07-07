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
    create_provider/3, call_worker/2, generate_group_ids/1, generate_user_ids/1,
    generate_space_ids/1, create_users/3, create_spaces/4, create_groups/4, id/1, empty_cache/1, create_provider/4, delete_all/2, list/2]).
-export([expectation/2, public_only_user_expectation/2, group_expectation/8,
    privileges_as_binaries/1, expectation_with_rev/2, public_only_provider_expectation/3]).
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

delete_all(Node, Documents) ->
    lists:foreach(fun(#document{key= Key, value=Value}) ->
        subscriptions_test_utils:delete_document(Node, element(1, Value), Key)
    end, Documents).

list(Node, Model) ->
    {ok, Documents} = rpc:call(Node, Model, list, []),
    Documents.

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
    create_provider(Node, Name, Spaces, [<<"127.0.0.1">>]).
create_provider(Node, Name, Spaces, URLs) ->
    {_, CSRFile, _} = generate_cert_files(),
    {ok, CSR} = file:read_file(CSRFile),
    Params = [Name, URLs, <<"https://127.0.0.1:443">>, CSR],
    {ok, ID, _} = rpc:call(Node, provider_logic, create, Params),
    {ok, ID} = rpc:call(Node, provider, update, [ID, #{spaces => Spaces}]),
    ID.

generate_space_ids(Number) ->
    generate_ids("s", Number).

generate_group_ids(Number) ->
    generate_ids("g", Number).

generate_user_ids(Number) ->
    generate_ids("u", Number).

generate_ids(Prefix, Number) ->
    [?ID(list_to_atom(Prefix ++ integer_to_list(N))) || N <- lists:seq(1, Number)].

create_spaces(SIDs, UIDs, GIDs, Node) ->
    Groups = [{GID, []} || GID <- GIDs],
    Users =  [{UID, []} || UID <- UIDs],
    lists:map(fun({SID, N}) -> {
        Space = #space{
            name = list_to_binary("s" ++ integer_to_list(N) ++ integer_to_list(erlang:system_time(micro_seconds))),
            groups = Groups,
            users = Users
        }},
        subscriptions_test_utils:save(Node, SID, Space),
        {SID, Space}
    end, lists:zip(SIDs, lists:seq(1, length(SIDs)))).

create_users(UIDs, GIDs, Node) ->
    lists:map(fun({UID, N}) ->{
        User = #onedata_user{
            name=list_to_binary("u" ++ integer_to_list(N)),
            groups = GIDs
        }},
        subscriptions_test_utils:save(Node, UID, User),
        {UID, User}
    end, lists:zip(UIDs, lists:seq(1, length(UIDs)))).

create_groups(GIDs, UIDs, SIDs, Node) ->
    Users = [{UID, []} || UID <- UIDs],
    lists:map(fun({GID, N}) ->{
        Group = #user_group{
            name = list_to_binary("g" ++ integer_to_list(N)),
            users = Users,
            spaces = SIDs
        }},
        subscriptions_test_utils:save(Node, GID, Group),
        {GID, Group}
end, lists:zip(GIDs, lists:seq(1, length(GIDs)))).


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
    group_expectation(ID, Name, Type, Users, EUsers, Spaces, NGroups, PGroups);

expectation(ID, #provider{client_name = Name, urls = URLs, spaces = SpaceIDs}) ->
    [{<<"id">>, ID}, {<<"provider">>, [
        {<<"client_name">>, Name},
        {<<"urls">>, URLs},
        {<<"space_ids">>, SpaceIDs},
        {<<"public_only">>, false}
    ]}].

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

public_only_provider_expectation(ID, Name, URLs) ->
    [{<<"id">>, ID}, {<<"provider">>, [
        {<<"client_name">>, Name},
        {<<"urls">>, URLs},
        {<<"space_ids">>, []},
        {<<"public_only">>, true}
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

%%    ct:print("ALL: ~p~nEXPECTED: ~p~nFORBIDDEN: ~p~n", [All, Expected, Forbidden]),

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


id(Id) when is_atom(Id) ->
    ?ID(Id);
id(Id) ->
    ?ID(list_to_atom(Id)).

empty_cache(Node) ->
    subscriptions_test_utils:update_document(Node, subscriptions_state, ?SUBSCRIPTIONS_STATE_KEY, #{
        cache => gb_trees:empty()
    }).