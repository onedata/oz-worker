%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for db_browser module.
%%% @end
%%%-------------------------------------------------------------------
-module(db_browser_test_SUITE).
-author("Lukasz Opiola").

-include("entity_logic.hrl").
-include("api_test_utils.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    db_browser_test/1
]).

all() -> ?ALL([
    db_browser_test
]).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ozt:init_per_suite(Config).

init_per_testcase(_, Config) ->
    ozt_mocks:mock_handle_proxy(),
    ozt_mocks:mock_harvester_plugins(),
    ozt_mocks:mock_time(),
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unmock_handle_proxy(),
    ozt_mocks:unmock_time(),
    ozt_mocks:unmock_harvester_plugins().

end_per_suite(_Config) ->
    ok.

%%%===================================================================
%%% Helper functions
%%%===================================================================

% Number of entities of each type to create
-define(ENTITY_COUNT, 40).
% Number of members in each (applicable) entity
-define(MEMBERS_COUNT, 5).

-define(RAND_SUBLIST(List), lists_utils:random_sublist(List)).
-define(RAND_SUBLIST(List, Len), lists_utils:random_sublist(List, Len, Len)).

-define(NAMES_A, [
    <<"Amazing">>, <<"Competent">>, <<"Ecstatic">>, <<"Flamboyant">>,
    <<"Hopeful">>, <<"Jolly">>, <<"Lucid">>, <<"Naughty">>,
    <<"Pedantic">>, <<"Quirky">>, <<"Romantic">>, <<"Silly">>,
    <<"Thirsty">>, <<"Vigilant">>, <<"Wonderful">>, <<"Zealous">>,
    <<"πęß"/utf8>>
]).
-define(NAMES_B, [
    <<"Archimedes">>, <<"Banach">>, <<"Dijkstra">>, <<"Fermat">>,
    <<"Hamilton">>, <<"Kepler">>, <<"Matsumoto">>, <<"Nobel">>,
    <<"Panini">>, <<"Roentgen">>, <<"Satoshi">>, <<"Tesla">>,
    <<"Villani">>, <<"Wozniak">>, <<"Yonath">>, <<"Zhukovsky">>,
    <<"źµł"/utf8>>
]).

-define(GEN_NAME(), <<(lists_utils:random_element(?NAMES_A))/binary, " ", (lists_utils:random_element(?NAMES_B))/binary>>).

-define(GEN_QOS_PARAMS, #{
    <<"country">> => lists_utils:random_element([<<"PL">>, <<"DE">>, <<"NL">>, <<"UK">>, <<"UA">>, <<"IT">>, <<"FR">>]),
    <<"type">> => lists_utils:random_element([<<"posix">>, <<"object">>, <<"tape">>, <<"scratch">>, <<"archive">>]),
    <<"speed">> => integer_to_binary(rand:uniform(9999999999)),
    <<"tier">> => <<"t", (integer_to_binary(rand:uniform(8)))/binary>>
}).

-define(EMAIL_DOMAINS, [<<"example.com">>, <<"mail.com">>, <<"email.org">>, <<"inbox.org">>]).

-record(environment, {
    users = [] :: [od_user:id()],
    groups = [] :: [od_group:id()],
    spaces = [] :: [od_space:id()],
    shares = [] :: [od_share:id()],
    providers = [] :: [od_provider:id()],
    storages = [] :: [od_storage:id()],
    clusters = [] :: [od_cluster:id()],
    handle_services = [] :: [od_handle_service:id()],
    handles = [] :: [od_handle:id()],
    harvesters = [] :: [od_harvester:id()]
}).


simulate_random_delay() ->
    ozt_mocks:simulate_time_passing(rand:uniform(2592000) * lists_utils:random_element([-1, 1])).


set_up_environment() ->
    lists:foldl(fun(Fun, Acc) -> Fun(Acc) end, #environment{}, [
        fun set_up_users/1,
        fun set_up_groups/1,
        fun set_up_spaces_and_shares/1,
        fun set_up_providers_and_clusters/1,
        fun set_up_storages/1,
        fun set_up_provider_sync_progress/1,
        fun set_up_handle_services_and_handles/1,
        fun set_up_harvesters/1
    ]).


set_up_users(Environment) ->
    Environment#environment{
        users = lists:map(fun(_) ->
            Name = ?GEN_NAME(),
            User = ozt_users:create(#{
                <<"fullName">> => ?GEN_NAME(), <<"username">> => str_utils:rand_hex(4)
            }),
            simulate_random_delay(),
            Emails = ?RAND_SUBLIST(lists:map(fun(Domain) ->
                str_utils:format_bin("~s@~s", [string:replace(string:lowercase(Name), " ", ".", all), Domain])
            end, ?EMAIL_DOMAINS)),
            LinkedAccounts = case rand:uniform(5) of
                1 ->
                    [];
                _ ->
                    IdP = lists_utils:random_element([aai_org, sso_com, firstIdP, anotherIdP]),
                    [#linked_account{idp = IdP, emails = ?RAND_SUBLIST(Emails)}]
            end,
            ozt:rpc(od_user, update, [User, fun(UserRecord) ->
                {ok, UserRecord#od_user{emails = Emails, linked_accounts = LinkedAccounts}}
            end]),
            % Simulate graph sync connections of some users
            case rand:uniform(8) of
                1 ->
                    ok;
                _ ->
                    simulate_random_delay(),
                    ozt:rpc(user_connections, add, [User, <<"sess-id">>, self()]),
                    case rand:uniform(4) of
                        1 ->
                            ok;
                        _ ->
                            ozt:rpc(user_connections, remove, [User, <<"sess-id">>, self()])
                    end
            end,
            case rand:uniform(5) of
                1 -> ozt_users:grant_oz_privileges(User, ?RAND_SUBLIST(privileges:oz_privileges()));
                _ -> ok
            end,
            User
        end, lists:seq(1, ?ENTITY_COUNT))
    }.


set_up_groups(Environment = #environment{users = Users}) ->
    Groups = lists:map(fun(_) ->
        Group = ozt_groups:create(#{
            <<"name">> => ?GEN_NAME(),
            <<"type">> => lists_utils:random_element([organization, unit, team, role_holders])
        }),
        simulate_random_delay(),
        generate_members(od_group, Group, od_user, Users),
        case rand:uniform(8) of
            1 -> ozt_groups:grant_oz_privileges(Group, ?RAND_SUBLIST(privileges:oz_privileges()));
            _ -> ok
        end,
        Group
    end, lists:seq(1, ?ENTITY_COUNT)),

    TopLevelGroups = lists:sublist(Groups, 1, ?ENTITY_COUNT div 3),
    MidLevelGroups = lists:sublist(Groups, ?ENTITY_COUNT div 3 + 1, ?ENTITY_COUNT div 3),
    BotLevelGroups = lists:sublist(Groups, 2 * ?ENTITY_COUNT div 3 + 1, ?ENTITY_COUNT - (2 * ?ENTITY_COUNT div 3)),

    lists:foreach(fun(Group) ->
        generate_members(od_group, Group, od_group, MidLevelGroups)
    end, TopLevelGroups),

    lists:foreach(fun(Group) ->
        generate_members(od_group, Group, od_group, BotLevelGroups)
    end, MidLevelGroups),

    Environment#environment{
        groups = Groups
    }.


set_up_spaces_and_shares(Environment = #environment{users = Users, groups = Groups}) ->
    Spaces = lists:map(fun(_) ->
        Space = ozt_spaces:create(?GEN_NAME()),
        simulate_random_delay(),
        generate_members(od_space, Space, od_user, Users),
        generate_members(od_space, Space, od_group, Groups),
        Space
    end, lists:seq(1, ?ENTITY_COUNT)),

    Environment#environment{
        spaces = Spaces,
        shares = lists:flatmap(fun(Space) ->
            lists:map(fun(_) ->
                simulate_random_delay(),
                ozt_spaces:create_share(Space, ?GEN_NAME())
            end, lists:seq(1, ?MEMBERS_COUNT))
        end, Spaces)
    }.


set_up_providers_and_clusters(Environment = #environment{users = Users, groups = Groups}) ->
    Providers = lists:map(fun(_) ->
        Provider = ozt_providers:create_for_admin_user(lists_utils:random_element(Users), ?GEN_NAME()),
        simulate_random_delay(),
        Cluster = Provider,
        generate_members(od_cluster, Cluster, od_user, Users),
        generate_members(od_cluster, Cluster, od_group, Groups),
        % Simulate graph sync connections of some providers
        case rand:uniform(8) of
            1 ->
                ok;
            _ ->
                simulate_random_delay(),
                ozt:rpc(provider_connections, add, [Provider, self()]),
                case rand:uniform(4) of
                    1 ->
                        ok;
                    _ ->
                        ozt:rpc(provider_connections, remove, [Provider, self()])
                end
        end,
        Provider
    end, lists:seq(1, ?ENTITY_COUNT)),
    generate_members(od_cluster, ?ONEZONE_CLUSTER_ID, od_user, Users),
    generate_members(od_cluster, ?ONEZONE_CLUSTER_ID, od_group, Groups),
    Environment#environment{
        providers = Providers,
        clusters = [?ONEZONE_CLUSTER_ID | Providers]
    }.


set_up_storages(Environment = #environment{providers = Providers, spaces = Spaces}) ->
    Environment#environment{
        storages = lists:flatmap(fun(Provider) ->
            RandomQosParams = ?GEN_QOS_PARAMS,
            QosParams = maps:with(lists_utils:random_sublist(maps:keys(RandomQosParams)), RandomQosParams),
            Storages = lists:map(fun(_) ->
                ozt_providers:create_storage(Provider, ?GEN_NAME(), QosParams)
            end, lists:seq(1, ?MEMBERS_COUNT)),
            lists:map(fun(Space) ->
                SupportSize = ozt_spaces:minimum_support_size() + case rand:uniform(3) of
                    1 -> rand:uniform(1000000000000);
                    2 -> rand:uniform(10000000000);
                    3 -> rand:uniform(10000000)
                end,
                ozt_providers:support_space(Provider, lists_utils:random_element(Storages), Space, SupportSize)
            end, ?RAND_SUBLIST(Spaces, ?MEMBERS_COUNT)),
            Storages
        end, Providers)
    }.


set_up_provider_sync_progress(Environment = #environment{spaces = Spaces}) ->
    ozt:reconcile_entity_graph(),
    lists:foreach(fun(Space) ->
        EffProviders = maps:keys((ozt_spaces:get(Space))#od_space.eff_providers),
        lists:foreach(fun(Provider) ->
            ozt:rpc(space_logic, update_provider_sync_progress, [?ROOT, Space, Provider, maps:from_list(
                lists:map(fun(OtherProvider) ->
                    {OtherProvider, {rand:uniform(10000), ozt:cluster_time_seconds() - rand:uniform(50000)}}
                end, EffProviders)
            )]),
            simulate_random_delay()
        end, EffProviders)
    end, Spaces),
    Environment.


set_up_handle_services_and_handles(Environment = #environment{users = Users, groups = Groups, shares = Shares}) ->
    HandleServices = lists:map(fun(_) ->
        HService = ozt_handle_services:create(?GEN_NAME()),
        simulate_random_delay(),
        generate_members(od_handle_service, HService, od_user, Users),
        generate_members(od_handle_service, HService, od_group, Groups),
        HService
    end, lists:seq(1, ?ENTITY_COUNT)),

    HandlesCountPerService = ?MEMBERS_COUNT div 2 + 1,
    % Shuffle the shares and assign a portion (HandlesCountPerService) to each handle service
    ShuffledShares = lists_utils:shuffle(Shares),
    {HServicesAndShares, _} = lists:mapfoldl(fun(Hservice, SharesToAssign) ->
        {SharesForHService, RemainingToAssign} = lists:split(HandlesCountPerService, SharesToAssign),
        {{Hservice, SharesForHService}, RemainingToAssign}
    end, ShuffledShares, HandleServices),

    Environment#environment{
        handle_services = HandleServices,
        handles = lists:flatmap(fun({HService, SharesToAssign}) ->
            lists:map(fun(Share) ->
                Handle = ozt_handle_services:create_handle(HService, Share),
                simulate_random_delay(),
                generate_members(od_handle, Handle, od_user, Users),
                generate_members(od_handle, Handle, od_group, Groups),
                Handle
            end, SharesToAssign)
        end, HServicesAndShares)
    }.


set_up_harvesters(Environment = #environment{users = Users, groups = Groups, spaces = Spaces}) ->
    Environment#environment{
        harvesters = lists:map(fun(_) ->
            Harvester = ozt_harvesters:create(?GEN_NAME()),
            simulate_random_delay(),
            generate_members(od_harvester, Harvester, od_user, Users),
            generate_members(od_harvester, Harvester, od_group, Groups),
            lists:map(fun(Space) ->
                ozt_harvesters:add_space(Harvester, Space)
            end, ?RAND_SUBLIST(Spaces, ?MEMBERS_COUNT)),
            Harvester
        end, lists:seq(1, ?ENTITY_COUNT))
    }.


generate_members(ParentType, ParentId, MemberType, AllMembers) ->
    [add_member(ParentType, ParentId, MemberType, M) || M <- ?RAND_SUBLIST(AllMembers, ?MEMBERS_COUNT)].


add_member(ParentType, ParentId, MemberType, MemberId) ->
    Privileges = case ParentType of
        od_group -> privileges:group_privileges();
        od_space -> privileges:space_privileges();
        od_handle_service -> privileges:handle_service_privileges();
        od_handle -> privileges:handle_privileges();
        od_cluster -> privileges:cluster_privileges();
        od_harvester -> privileges:harvester_privileges()
    end,
    Module = case ParentType of
        od_group -> group_logic;
        od_space -> space_logic;
        od_handle_service -> handle_service_logic;
        od_handle -> handle_logic;
        od_cluster -> cluster_logic;
        od_harvester -> harvester_logic
    end,
    Function = case MemberType of
        od_user -> add_user;
        od_group -> add_group
    end,
    ozt:rpc(Module, Function, [?ROOT, ParentId, MemberId, ?RAND_SUBLIST(Privileges)]).

%%%===================================================================
%%% Tests
%%%===================================================================

db_browser_test(_) ->
    try
        db_browser_test_unsafe()
    catch Type:Reason ->
        ct:pal("db_browser test failed with ~w:~w~nStacktrace: ~ts", [
            Type, Reason, lager:pr_stacktrace(erlang:get_stacktrace())
        ]),
        error(test_failed)
    end.


db_browser_test_unsafe() ->
    Env = set_up_environment(),
    AllCollections = ozt:rpc(db_browser, all_collections, []),
    lists:foreach(fun(Collection) ->
        print_collection(Env, Collection)
    end, [help | AllCollections]),

    % Check that script API for db_browser.sh works as expected
    TmpPath = ozt:rpc(mochitemp, mkdtemp, []),
    OutputFile = filename:join(TmpPath, "dump.txt"),
    ozt:rpc(db_browser, call_from_script, [OutputFile, "users username desc"]),
    ExpectedResult = str_utils:unicode_list_to_binary(ozt:rpc(
        db_browser, format, [users, username, desc])
    ),
    {ok, OutputFileContent} = ozt:rpc(file, read_file, [OutputFile]),
    ?assertEqual(OutputFileContent, ExpectedResult),

    % Check that error handling and help works the same for internal use and the script
    ozt:rpc(db_browser, call_from_script, [OutputFile, "sdf &SD F^sadf6asDF5asd"]),
    ExpectedResult2 = str_utils:unicode_list_to_binary(ozt:rpc(
        db_browser, format, ['sdf', '&SD', 'F^sadf6asDF5asd'])
    ),
    {ok, OutputFileContent2} = ozt:rpc(file, read_file, [OutputFile]),
    % The stacktrace at the beginning of the output is different - check if the
    % usage help is the same.
    ?assertEqual(string:find(OutputFileContent2, "Usage"), string:find(ExpectedResult2, "Usage")).


print_collection(Env, Collection) ->
    CollectionWithExistingId = case Collection of
        {C, <<"user_id">>} -> {C, lists_utils:random_element(Env#environment.users)};
        {C, <<"group_id">>} -> {C, lists_utils:random_element(Env#environment.groups)};
        {C, <<"space_id">>} -> {C, lists_utils:random_element(Env#environment.spaces)};
        {C, <<"provider_id">>} -> {C, lists_utils:random_element(Env#environment.providers)};
        {C, <<"cluster_id">>} -> {C, lists_utils:random_element(Env#environment.clusters)};
        {C, <<"handle_service_id">>} -> {C, lists_utils:random_element(Env#environment.handle_services)};
        {C, <<"handle_id">>} -> {C, lists_utils:random_element(Env#environment.handles)};
        {C, <<"harvester_id">>} -> {C, lists_utils:random_element(Env#environment.harvesters)};
        {C, <<"storage_id">>} -> {C, lists_utils:random_element(Env#environment.storages)};
        C when is_atom(C) -> C
    end,
    CollectionAtom = case CollectionWithExistingId of
        {Atom, Binary} -> list_to_atom(str_utils:format("~s@~s", [Atom, Binary]));
        Atom when is_atom(Atom) -> Atom
    end,
    SortBy = lists_utils:random_element([default | example_column_names()]),
    SortOrder = lists_utils:random_element([asc, desc]),
    Args = case rand:uniform(4) of
        1 -> [CollectionAtom];
        2 -> [CollectionAtom, SortBy];
        3 -> [CollectionAtom, SortOrder];
        4 -> [CollectionAtom, SortBy, SortOrder]
    end,
    ?assertEqual(ok, ozt:rpc(db_browser, pr, Args)),
    Result = ozt:rpc(db_browser, format, Args),
    case string:find(Result, "\n0 entries in total") of
        nomatch ->
            % Okay, dump the results to logs so that they can be examined
            ct:pal("~w:~n~ts", [Args, ozt:rpc(db_browser, format, Args)]);
        _ ->
            % Repeat until collection that have at least one entry is found
            print_collection(Env, Collection)
    end.


% The test might randomize columns that are not present in a collection,
% in such case default sorting column is taken.
example_column_names() -> [
    id, name, full_name, username, type, email, created, admin_privs,
    eff_users, eff_groups, users, groups, children, parents, spaces, shares,
    providers, eff_providers, clusters, handle_services, handles, harvesters,
    support, handle, online, version, domain, proxy_endpoint, public_handle
].