%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(subscriptions_test_SUITE).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([simple_space_update_test/1, change_bridge_restarts/1, simple_user_update_test/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
%%    change_bridge_restarts,
    simple_space_update_test,
    simple_user_update_test
]).

change_bridge_restarts(_Config) ->
    ?assertNotEqual(undefined, global:whereis_name(changes_bridge)),

    gen_server:cast({global, changes_bridge}, stop),
    timer:sleep(200),
    ?assertNotEqual(undefined, global:whereis_name(changes_bridge)),

    gen_server:cast({global, changes_bridge}, stop),
    timer:sleep(200),
    ?assertNotEqual(undefined, global:whereis_name(changes_bridge)),

    gen_server:cast({global, changes_bridge}, stop),
    timer:sleep(200),
    ?assertNotEqual(undefined, global:whereis_name(changes_bridge)),
    ok.

simple_space_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, <<"p1">>, [<<"s1">>]),
    create_space(Node, <<"s1">>, [P1], [], []),

    % when
    Context = init_messages(Node, P1, []),
    update_document(Node, space, <<"s1">>, #{name => <<"updated">>}),

    % then
    verify_messages(Context, [
        space_expectation(<<"s1">>, <<"updated">>)
    ], []),
    ok.

simple_user_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, <<"p1">>, [<<"s1">>]),
    create_user(Node, <<"u1">>, [], []),
    call_worker(Node, {add_connection, P1, self()}),

    % when
    Context = init_messages(Node, P1, [<<"u1">>]),
    update_document(Node, onedata_user, <<"u1">>, #{name => <<"updated">>}),

    % then
    verify_messages(Context, [
        user_expectation(<<"u1">>, <<"updated">>, [], [])
    ], []),
    ok.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")).

init_per_testcase(_, _Config) ->
    _Config.

end_per_testcase(_, _Config) ->
    ok.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).


%%%===================================================================
%%% Internal functions
%%%===================================================================

space_expectation(ID, Name) ->
    [{<<"id">>, ID}, {<<"space">>, [{<<"id">>, ID}, {<<"name">>, Name}]}].

user_expectation(ID, Name, Spaces, Groups) ->
    [{<<"id">>, ID}, {<<"user">>, [
        {<<"name">>, Name}, {<<"space_ids">>, Spaces}, {<<"group_ids">>, Groups}
    ]}].

create_group(Node, Name, Users, Spaces) ->
    ?assertMatch({ok, Name}, rpc:call(Node, user_group, save, [#document{
        key = Name,
        value = #user_group{name = Name,
            users = zip_with(Users, []), spaces = Spaces}
    }])).

create_space(Node, Name, Providers, Users, Groups) ->
    ?assertMatch({ok, Name}, rpc:call(Node, space, save, [#document{
        key = Name,
        value = #space{name = Name, users = zip_with(Users, []),
            groups = zip_with(Groups, []), providers = zip_with(Providers, 1)}
    }])).

create_user(Node, Name, Groups, Spaces) ->
    ?assertMatch({ok, Name}, rpc:call(Node, onedata_user, save, [#document{
        key = Name,
        value = #onedata_user{alias = Name, name = Name,
            groups = Groups, spaces = Spaces}
    }])).

update_document(Node, Model, ID, Diff) ->
    ?assertMatch({ok, _}, rpc:call(Node, Model, update, [ID, Diff])).

create_provider(Node, Name, Spaces) ->
    {_, CSRFile, _} = generate_cert_files(),
    {ok, CSR} = file:read_file(CSRFile),
    Params = [Name, [<<"127.0.0.1">>], <<"https://127.0.0.1:443">>, CSR],
    {ok, ID, _} = rpc:call(Node, provider_logic, create, Params),
    {ok, ID} = rpc:call(Node, provider, update, [ID, fun(P) ->
        {ok, P#provider{spaces = lists:append(P#provider.spaces, Spaces)}}
    end]),
    ID.

zip_with(IDs, Val) ->
    lists:map(fun(ID) -> {ID, Val} end, IDs).

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

verify_messages(Context, Expected, Forbidden) ->
    verify_messages(Context, 5, Expected, Forbidden).

init_messages(Node, ProviderID, Users) ->
    call_worker(Node, {add_connection, ProviderID, self()}),
    verify_messages({Node, ProviderID, Users, 1, []}, [], [["flushy flushy"]]).

verify_messages(Context, _, [], []) ->
    Context;
verify_messages(Context, 0, Expected, _) ->
    ?assertMatch([], Expected),
    Context;
verify_messages(Context, Retries, Expected, Forbidden) ->
    {Node, ProviderID, Users, ResumeAt, Missing} = Context,

    call_worker(Node, {update_users, ProviderID, Users}),
    call_worker(Node, {update_missing_seq, ProviderID, ResumeAt, Missing}),
    All = lists:append(get_messages(20, [])),

    Seqs = extract_sequence_numbers(All),
    NextResumeAt = largest([ResumeAt | Seqs]),
    NextMissing = (Missing ++ new_expected_seqs(NextResumeAt, ResumeAt)) -- Seqs,
    NextContext = {Node, ProviderID, Users, NextResumeAt, NextMissing},

    ?assertMatch(Forbidden, Forbidden -- All),
    RemainingExpected = remaining_expected(Expected, All),
    verify_messages(NextContext, Retries - 1, RemainingExpected, Forbidden).

new_expected_seqs(NextResumeAt, ResumeAt) ->
    case NextResumeAt > (ResumeAt + 1) of
        true -> lists:seq(ResumeAt + 1, NextResumeAt);
        false -> []
    end.

largest(List) ->
    hd(lists:reverse(lists:usort(List))).

extract_sequence_numbers(All) ->
    lists:map(fun(Message) ->
        proplists:get_value(<<"seq">>, Message)
    end, All).

remaining_expected(Expected, All) ->
    lists:filter(fun(Exp) ->
        lists:all(fun(Msg) -> length(Exp -- Msg) =/= 0 end, All)
    end, Expected).

call_worker(Node, Req) ->
    rpc:call(Node, worker_proxy, call, [subscriptions_worker, Req]).

get_messages(0, Acc) -> Acc;
get_messages(Retries, Acc) ->
    receive
        {push, Messages} ->
            get_messages(Retries - 1, [json_utils:decode(Messages) | Acc])
    after timer:seconds(2) -> Acc
    end.