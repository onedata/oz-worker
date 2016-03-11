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
-include("subscriptions/subscriptions.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([
    space_update_through_support_test/1,
    space_update_through_users_test/1,
    no_space_update_test/1,
    user_update_test/1,
    no_user_update_test/1,
    group_update_through_users_test/1,
    group_update_through_spaces_test/1,
    no_group_update_test/1,
    multiple_updates_test/1,
    updates_for_added_user_test/1,
    updates_for_added_user_have_revisions_test/1,
    updates_have_revisions_test/1,
    fetches_changes_older_than_in_cache/1,
    fetches_changes_from_both_cache_and_db/1,
    fetches_changes_when_cache_has_gaps/1]).

-define(MESSAGES_WAIT_TIMEOUT, timer:seconds(3)).
-define(MESSAGES_RECEIVE_ATTEMPTS, 30).

%% appends function name to id (atom) and yields binary accepted by the db
-define(ID(Id), list_to_binary(
    atom_to_list(Id) ++ " # " ++
    atom_to_list(element(2, element(2, process_info(self(), current_function))))
)).

%% helper record for maintaining subscription progress between message receives
-record(subs_ctx, {
    node :: node(),
    provider :: binary(),
    users :: [binary()],
    resume_at :: seq(),
    missing :: [seq()]
}).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    multiple_updates_test,
    no_space_update_test,
    space_update_through_support_test,
    space_update_through_users_test,
    no_user_update_test,
    user_update_test,
    no_group_update_test,
    group_update_through_users_test,
    group_update_through_spaces_test,
    updates_for_added_user_test,
    updates_have_revisions_test,
    updates_for_added_user_have_revisions_test,
    fetches_changes_older_than_in_cache,
    fetches_changes_from_both_cache_and_db,
    fetches_changes_when_cache_has_gaps
]).

no_space_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, ?ID(p1), []),
    create_space(Node, ?ID(s1), [], [], []),

    % when
    Context = init_messages(Node, P1, []),
    update_document(Node, space, ?ID(s1), #{name => <<"updated">>}),

    % then
    verify_messages_absent(Context, [
        space_expectation(?ID(s1), <<"updated">>)
    ]),
    ok.

space_update_through_support_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, ?ID(p1), [?ID(s1)]),
    create_space(Node, ?ID(s1), [P1], [], []),

    % when
    Context1 = init_messages(Node, P1, []),
    Context = flush_messages(Context1, space_expectation(?ID(s1), ?ID(s1))),
    update_document(Node, space, ?ID(s1), #{name => <<"updated">>}),

    % then
    verify_messages_present(Context, [
        space_expectation(?ID(s1), <<"updated">>)
    ]),
    ok.

space_update_through_users_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, ?ID(p1), []),
    create_user(Node, ?ID(u1), [], []),
    create_space(Node, ?ID(s1), [P1], [?ID(u1)], []),

    % when
    Context1 = init_messages(Node, P1, []),
    Context = flush_messages(Context1, space_expectation(?ID(s1), ?ID(s1))),
    update_document(Node, space, ?ID(s1), #{name => <<"updated">>}),

    % then
    verify_messages_present(Context, [
        space_expectation(?ID(s1), <<"updated">>)
    ]),
    ok.

no_user_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, ?ID(p1), [?ID(s1)]),
    create_user(Node, ?ID(u1), [], []),
    call_worker(Node, {add_connection, P1, self()}),

    % when
    Context = init_messages(Node, P1, [?ID(u1)]),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated">>}),

    % then
    verify_messages_absent(Context, [
        user_expectation(?ID(u1), <<"updated">>, [], [])
    ]),
    ok.

user_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, ?ID(p1), [?ID(s1)]),
    create_user(Node, ?ID(u1), [], []),
    call_worker(Node, {add_connection, P1, self()}),

    % when
    Context1 = init_messages(Node, P1, [?ID(u1)]),
    Context = flush_messages(Context1, user_expectation(?ID(u1), ?ID(u1), [], [])),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated">>}),

    % then
    verify_messages(Context, [
        user_expectation(?ID(u1), <<"updated">>, [], [])
    ], []),
    ok.

multiple_updates_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, ?ID(p1), [?ID(s1)]),
    create_user(Node, ?ID(u1), [], []),
    call_worker(Node, {add_connection, P1, self()}),

    % when
    Context1 = init_messages(Node, P1, [?ID(u1)]),
    Context = flush_messages(Context1, user_expectation(?ID(u1), ?ID(u1), [], [])),

    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated1">>}),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated2">>}),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated3">>}),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated4">>}),

    % then
    verify_messages_present(Context, [
        user_expectation(?ID(u1), <<"updated4">>, [], [])
    ]),
    ok.

no_group_update_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, ?ID(p1), []),
    create_group(Node, ?ID(g1), [], []),
    call_worker(Node, {add_connection, P1, self()}),

    % when
    Context = init_messages(Node, P1, [?ID(u1)]),
    update_document(Node, user_group, ?ID(g1), #{name => <<"updated">>}),

    % then
    verify_messages_absent(Context, [
        group_expectation(?ID(g1), <<"updated">>)
    ]),
    ok.

group_update_through_users_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, ?ID(p1), []),
    create_user(Node, ?ID(u1), [], []),
    create_group(Node, ?ID(g1), [?ID(u1)], []),
    call_worker(Node, {add_connection, P1, self()}),

    % when
    Context1 = init_messages(Node, P1, [?ID(u1)]),
    Context = flush_messages(Context1, group_expectation(?ID(g1), ?ID(g1))),
    update_document(Node, user_group, ?ID(g1), #{name => <<"updated">>}),

    % then
    verify_messages_present(Context, [
        group_expectation(?ID(g1), <<"updated">>)
    ]),
    ok.

group_update_through_spaces_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, ?ID(p1), [?ID(s1)]),
    create_space(Node, ?ID(s1), [P1], [], [?ID(g1)]),
    create_group(Node, ?ID(g1), [], [?ID(s1)]),
    call_worker(Node, {add_connection, P1, self()}),

    % when
    Context1 = init_messages(Node, P1, [?ID(u1)]),
    Context = flush_messages(Context1, group_expectation(?ID(g1), ?ID(g1))),
    update_document(Node, user_group, ?ID(g1), #{name => <<"updated">>}),

    % then
    verify_messages_present(Context, [
        group_expectation(?ID(g1), <<"updated">>)
    ]),
    ok.

updates_for_added_user_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, ?ID(p1), [?ID(s1), ?ID(s2)]),
    create_user(Node, ?ID(u1), [?ID(g1)], [?ID(s2)]),
    create_group(Node, ?ID(g1), [], [?ID(s1)]),
    create_space(Node, ?ID(s1), [P1], [], [?ID(g1)]),
    create_space(Node, ?ID(s2), [P1], [?ID(u1)], []),
    call_worker(Node, {add_connection, P1, self()}),

    Context1 = init_messages(Node, P1, []),
    Context = flush_messages(Context1, space_expectation(?ID(s2), ?ID(s2))),

    % when & then
    verify_messages_present(Context#subs_ctx{users = [?ID(u1)]}, [
        user_expectation(?ID(u1), ?ID(u1), [?ID(s2)], [?ID(g1)]),
        group_expectation(?ID(g1), ?ID(g1)),
        space_expectation(?ID(s1), ?ID(s1)),
        space_expectation(?ID(s2), ?ID(s2))
    ]),
    ok.

updates_for_added_user_have_revisions_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, ?ID(p1), []),
    create_user(Node, ?ID(u1), [], []),
    Rev0 = get_rev(Node, onedata_user, ?ID(u1)),
    call_worker(Node, {add_connection, P1, self()}),

    % when
    Context = init_messages(Node, P1, []),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated1">>}),
    Rev1 = get_rev(Node, onedata_user, ?ID(u1)),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated2">>}),
    Rev2 = get_rev(Node, onedata_user, ?ID(u1)),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated3">>}),
    Rev3 = get_rev(Node, onedata_user, ?ID(u1)),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated4">>}),
    Rev4 = get_rev(Node, onedata_user, ?ID(u1)),

    % then
    verify_messages_present(Context#subs_ctx{users = [(?ID(u1))]}, [
        expectation_with_rev(
            [Rev4, Rev3, Rev2, Rev1, Rev0],
            user_expectation(?ID(u1), <<"updated4">>, [], []))
    ]),
    ok.

updates_have_revisions_test(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, ?ID(p1), []),
    create_user(Node, ?ID(u1), [], []),
    Rev0 = get_rev(Node, onedata_user, ?ID(u1)),
    call_worker(Node, {add_connection, P1, self()}),

    % when
    Context1 = init_messages(Node, P1, [(?ID(u1))]),
    Context = flush_messages(Context1, user_expectation(?ID(u1), ?ID(u1), [], [])),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated1">>}),
    Rev1 = get_rev(Node, onedata_user, ?ID(u1)),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated2">>}),
    Rev2 = get_rev(Node, onedata_user, ?ID(u1)),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated3">>}),
    Rev3 = get_rev(Node, onedata_user, ?ID(u1)),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated4">>}),
    Rev4 = get_rev(Node, onedata_user, ?ID(u1)),

    % then
    verify_messages_present(Context, [
        expectation_with_rev(
            [Rev4, Rev3, Rev2, Rev1, Rev0],
            user_expectation(?ID(u1), <<"updated4">>, [], []))
    ]),
    ok.

fetches_changes_older_than_in_cache(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, ?ID(p1), []),
    create_user(Node, ?ID(u1), [], []),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated1">>}),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated2">>}),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated3">>}),
    update_document(Node, onedata_user, ?ID(u1), #{name => <<"updated4">>}),

    % when
    Context = init_messages(Node, P1, [(?ID(u1))]),
    _ForgottenContext = flush_messages(Context,
        user_expectation(?ID(u1), <<"updated4">>, [], [])),

    empty_cache(Node),

    % then
    verify_messages_present(Context, [
        user_expectation(?ID(u1), <<"updated4">>, [], [])
    ]),
    ok.


fetches_changes_from_both_cache_and_db(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, ?ID(p1), [
        ?ID(s1), ?ID(s2), ?ID(s3), ?ID(s4), ?ID(s5), ?ID(s6)
    ]),
    create_space(Node, ?ID(s1), [P1], [], []),
    create_space(Node, ?ID(s2), [P1], [], []),
    create_space(Node, ?ID(s3), [P1], [], []),
    create_space(Node, ?ID(s4), [P1], [], []),
    create_space(Node, ?ID(s5), [P1], [], []),
    create_space(Node, ?ID(s6), [P1], [], []),
    create_space(Node, ?ID(s7), [P1], [], []),
    create_space(Node, ?ID(s8), [P1], [], []),
    create_space(Node, ?ID(s9), [P1], [], []),
    call_worker(Node, {add_connection, P1, self()}),

    % when
    Copy = Context = init_messages(Node, P1, []),
    flush_messages(Context, space_expectation(?ID(s9), ?ID(s9))),
    empty_first_half_of_cache(Node),

    % then
    verify_messages_present(Copy, [
        space_expectation(?ID(s1), ?ID(s1)),
        space_expectation(?ID(s2), ?ID(s2)),
        space_expectation(?ID(s3), ?ID(s3)),
        space_expectation(?ID(s4), ?ID(s4)),
        space_expectation(?ID(s5), ?ID(s5)),
        space_expectation(?ID(s6), ?ID(s6)),
        space_expectation(?ID(s7), ?ID(s7)),
        space_expectation(?ID(s8), ?ID(s8)),
        space_expectation(?ID(s9), ?ID(s9))
    ]),
    ok.

fetches_changes_when_cache_has_gaps(Config) ->
    % given
    [Node | _] = ?config(oz_worker_nodes, Config),
    P1 = create_provider(Node, ?ID(p1), [
        ?ID(s1), ?ID(s2), ?ID(s3), ?ID(s4), ?ID(s5), ?ID(s6)
    ]),
    create_space(Node, ?ID(s1), [P1], [], []),
    create_space(Node, ?ID(s2), [P1], [], []),
    create_space(Node, ?ID(s3), [P1], [], []),
    create_space(Node, ?ID(s4), [P1], [], []),
    create_space(Node, ?ID(s5), [P1], [], []),
    create_space(Node, ?ID(s6), [P1], [], []),
    create_space(Node, ?ID(s7), [P1], [], []),
    create_space(Node, ?ID(s8), [P1], [], []),
    create_space(Node, ?ID(s9), [P1], [], []),
    call_worker(Node, {add_connection, P1, self()}),

    % when
    Copy = Context = init_messages(Node, P1, []),
    flush_messages(Context, space_expectation(?ID(s9), ?ID(s9))),
    empty_odd_seqs_in_cache(Node),

    % then
    verify_messages_present(Copy, [
        space_expectation(?ID(s1), ?ID(s1)),
        space_expectation(?ID(s2), ?ID(s2)),
        space_expectation(?ID(s3), ?ID(s3)),
        space_expectation(?ID(s4), ?ID(s4)),
        space_expectation(?ID(s5), ?ID(s5)),
        space_expectation(?ID(s6), ?ID(s6)),
        space_expectation(?ID(s7), ?ID(s7)),
        space_expectation(?ID(s8), ?ID(s8)),
        space_expectation(?ID(s9), ?ID(s9))
    ]),
    ok.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")).

init_per_testcase(_, _Config) ->
    _Config.

end_per_testcase(_, _Config) ->
    flush(),
    ok.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).


%%%===================================================================
%%% Internal functions
%%%===================================================================

empty_cache(Node) ->
    update_document(Node, subscriptions_state, ?SUBSCRIPTIONS_STATE_KEY, #{
        cache => gb_trees:empty()
    }).

empty_first_half_of_cache(Node) ->
    {ok, #document{value = #subscriptions_state{cache = Cache}}} =
        rpc:call(Node, subscriptions_state, get, [?SUBSCRIPTIONS_STATE_KEY]),

    AsList = gb_trees:to_list(Cache),
    {MedianSeq, _} = lists:nth(length(AsList) div 2, AsList),
    Filtered = lists:filter(fun({Seq, _}) -> Seq > MedianSeq end, AsList),

    UpdatedCache = lists:foldl(fun({Seq, Val}, Acc) ->
        gb_trees:enter(Seq, Val, Acc)
    end, gb_trees:empty(), Filtered),

    update_document(Node, subscriptions_state, ?SUBSCRIPTIONS_STATE_KEY, #{
        cache => UpdatedCache
    }).

empty_odd_seqs_in_cache(Node) ->
    {ok, #document{value = #subscriptions_state{cache = Cache}}} =
        rpc:call(Node, subscriptions_state, get, [?SUBSCRIPTIONS_STATE_KEY]),

    AsList = gb_trees:to_list(Cache),
    Filter = fun({Seq, _}) -> (Seq rem 2) =:= 0 end,
    Filtered = lists:filter(Filter, AsList),

    UpdatedCache = lists:foldl(fun({Seq, Val}, Acc) ->
        gb_trees:enter(Seq, Val, Acc)
    end, gb_trees:empty(), Filtered),

    update_document(Node, subscriptions_state, ?SUBSCRIPTIONS_STATE_KEY, #{
        cache => UpdatedCache
    }).

call_worker(Node, Req) ->
    rpc:call(Node, worker_proxy, call, [?SUBSCRIPTIONS_WORKER_NAME, Req]).

%%%===================================================================
%%% Internal: datastore setup
%%%===================================================================

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
            groups = zip_with(Groups, []), providers = Providers}
    }])).

create_user(Node, Name, Groups, Spaces) ->
    ?assertMatch({ok, Name}, rpc:call(Node, onedata_user, save, [#document{
        key = Name,
        value = #onedata_user{alias = Name, name = Name,
            groups = Groups, spaces = Spaces}
    }])).

update_document(Node, Model, ID, Diff) ->
    ?assertMatch({ok, _}, rpc:call(Node, Model, update, [ID, Diff])).

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


%%%===================================================================
%%% Internal: Message expectations
%%%===================================================================

space_expectation(ID, Name) ->
    [{<<"id">>, ID}, {<<"space">>, [{<<"id">>, ID}, {<<"name">>, Name}]}].

user_expectation(ID, Name, Spaces, Groups) ->
    [{<<"id">>, ID}, {<<"user">>, [
        {<<"name">>, Name}, {<<"space_ids">>, Spaces}, {<<"group_ids">>, Groups}
    ]}].

group_expectation(ID, Name) ->
    [{<<"id">>, ID}, {<<"group">>, [{<<"name">>, Name}]}].

expectation_with_rev(Revs, Expectation) ->
    [{<<"revs">>, Revs} | Expectation].

%%%===================================================================
%%% Internal: Message presence/absence verification
%%%===================================================================

verify_messages_present(Context, Expected) ->
    verify_messages(Context, ?MESSAGES_RECEIVE_ATTEMPTS, Expected, []).

verify_messages_absent(Context, Forbidden) ->
    verify_messages(Context, ?MESSAGES_RECEIVE_ATTEMPTS, [], Forbidden).

init_messages(Node, ProviderID, Users) ->
    call_worker(Node, {add_connection, ProviderID, self()}),
    #subs_ctx{node = Node, provider = ProviderID,
        users = Users, resume_at = 1, missing = []}.

flush_messages(Context, LastExpected) ->
    UpdatedContext = verify_messages(Context, [LastExpected], []),
    flush(),
    UpdatedContext.

flush() ->
    receive _ -> flush() after ?MESSAGES_WAIT_TIMEOUT -> ok end.

verify_messages(Context, Expected, Forbidden) ->
    verify_messages(Context, ?MESSAGES_RECEIVE_ATTEMPTS, Expected, Forbidden).

verify_messages(Context, _, [], []) ->
    Context;
verify_messages(Context, 0, Expected, _) ->
    ?assertMatch([], Expected),
    Context;
verify_messages(Context, Retries, Expected, Forbidden) ->
    #subs_ctx{node = Node, provider = ProviderID,
        users = Users, resume_at = ResumeAt, missing = Missing} = Context,

    call_worker(Node, {update_users, ProviderID, Users}),
    call_worker(Node, {update_missing_seq, ProviderID, ResumeAt, Missing}),
    All = lists:append(get_messages()),

    ct:print("Context ~p", [Context]),
    ct:print("All ~p", [All]),

    Seqs = extract_seqs(All),
    NextResumeAt = largest([ResumeAt | Seqs]),
    NewExpectedSeqs = new_expected_seqs(NextResumeAt, ResumeAt),
    NextMissing = (Missing ++ NewExpectedSeqs) -- Seqs,
    NextContext = Context#subs_ctx{
        resume_at = NextResumeAt,
        missing = NextMissing
    },

    ?assertMatch(Forbidden, Forbidden -- All),
    RemainingExpected = remaining_expected(Expected, All),
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
        proplists:get_value(<<"seq">>, Message)
    end, Messages).

remaining_expected(Expected, Messages) ->
    lists:filter(fun(Exp) ->
        lists:all(fun(Msg) -> length(Exp -- Msg) =/= 0 end, Messages)
    end, Expected).