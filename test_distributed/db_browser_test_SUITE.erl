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
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

init_per_testcase(_, Config) ->
    oz_test_utils:mock_handle_proxy(Config),
    oz_test_utils:mock_harvester_plugins(Config, ?HARVESTER_MOCK_PLUGIN),
    oz_test_utils:mock_time(Config),
    Config.

end_per_testcase(_, Config) ->
    oz_test_utils:unmock_handle_proxy(Config),
    oz_test_utils:unmock_time(Config),
    oz_test_utils:unmock_harvester_plugins(Config, ?HARVESTER_MOCK_PLUGIN).

end_per_suite(_Config) ->
    ok.

%%%===================================================================
%%% Helper functions
%%%===================================================================

% Number of entities of each type to create
-define(ENTITY_COUNT, 40).
% Number of members in each (applicable) entity
-define(MEMBERS_COUNT, 5).

-define(RAND_SUBLIST(List), random_sublist(List)).
-define(RAND_SUBLIST(List, Len), random_sublist(List, Len, Len)).

-define(NAMES_A, [
    <<"Amazing">>, <<"Competent">>, <<"Ecstatic">>, <<"Flamboyant">>,
    <<"Hopeful">>, <<"Jolly">>, <<"Lucid">>, <<"Naughty">>,
    <<"Pedantic">>, <<"Quirky">>, <<"Romantic">>, <<"Silly">>,
    <<"Thirsty">>, <<"Vigilant">>, <<"Wonderful">>, <<"Zealous">>
]).
-define(NAMES_B, [
    <<"Archimedes">>, <<"Banach">>, <<"Dijkstra">>, <<"Fermat">>,
    <<"Hamilton">>, <<"Kepler">>, <<"Matsumoto">>, <<"Nobel">>,
    <<"Panini">>, <<"Roentgen">>, <<"Satoshi">>, <<"Tesla">>,
    <<"Villani">>, <<"Wozniak">>, <<"Yonath">>, <<"Zhukovsky">>
]).

-define(GEN_NAME(), <<(utils:random_element(?NAMES_A))/binary, " ", (utils:random_element(?NAMES_B))/binary>>).

-record(environment, {
    users = [] :: [od_user:id()],
    groups = [] :: [od_group:id()],
    spaces = [] :: [od_space:id()],
    shares = [] :: [od_share:id()],
    providers = [] :: [od_provider:id()],
    clusters = [] :: [od_cluster:id()],
    handle_services = [] :: [od_handle_service:id()],
    handles = [] :: [od_handle:id()],
    harvesters = [] :: [od_harvester:id()]
}).

random_sublist(List) ->
    random_sublist(List, 0, length(List)).

random_sublist(List, MinLength, MaxLength) ->
    Shuffled = utils:random_shuffle(List),
    lists:sublist(Shuffled, MinLength + rand:uniform(MaxLength - MinLength + 1) - 1).


simulate_random_delay(Config) ->
    oz_test_utils:simulate_time_passing(Config, rand:uniform(2592000) * utils:random_element([-1, 1])).


set_up_environment(Config) ->
    Env1 = set_up_users(Config, #environment{}),
    Env2 = set_up_groups(Config, Env1),
    Env3 = set_up_spaces_and_shares(Config, Env2),
    Env4 = set_up_providers_and_clusters(Config, Env3),
    Env5 = set_up_handle_services_and_handles(Config, Env4),
    set_up_harvesters(Config, Env5).


set_up_users(Config, Environment) ->
    Environment#environment{
        users = lists:map(fun(_) ->
            {ok, User} = oz_test_utils:create_user(Config, #{
                <<"fullName">> => ?GEN_NAME(), <<"username">> => str_utils:rand_hex(4)
            }),
            simulate_random_delay(Config),
            case rand:uniform(5) of
                1 ->
                    Privs = ?RAND_SUBLIST(privileges:oz_privileges()),
                    oz_test_utils:user_set_oz_privileges(Config, User, Privs, []);
                _ ->
                    ok
            end,
            User
        end, lists:seq(1, ?ENTITY_COUNT))
    }.


set_up_groups(Config, Environment = #environment{users = Users}) ->
    Groups = lists:map(fun(_) ->
        {ok, Group} = oz_test_utils:create_group(Config, ?ROOT, #{
            <<"name">> => ?GEN_NAME(),
            <<"type">> => utils:random_element([organization, unit, team, role_holders])
        }),
        simulate_random_delay(Config),
        generate_members(Config, od_group, Group, od_user, Users),
        case rand:uniform(8) of
            1 ->
                Privs = ?RAND_SUBLIST(privileges:oz_privileges()),
                oz_test_utils:group_set_oz_privileges(Config, Group, Privs, []);
            _ ->
                ok
        end,
        Group
    end, lists:seq(1, ?ENTITY_COUNT)),

    TopLevelGroups = lists:sublist(Groups, 1, ?ENTITY_COUNT div 3),
    MidLevelGroups = lists:sublist(Groups, ?ENTITY_COUNT div 3 + 1, ?ENTITY_COUNT div 3),
    BotLevelGroups = lists:sublist(Groups, 2 * ?ENTITY_COUNT div 3 + 1, ?ENTITY_COUNT - (2 * ?ENTITY_COUNT div 3)),

    lists:foreach(fun(Group) ->
        generate_members(Config, od_group, Group, od_group, MidLevelGroups)
    end, TopLevelGroups),

    lists:foreach(fun(Group) ->
        generate_members(Config, od_group, Group, od_group, BotLevelGroups)
    end, MidLevelGroups),

    Environment#environment{
        groups = Groups
    }.


set_up_spaces_and_shares(Config, Environment = #environment{users = Users, groups = Groups}) ->
    Spaces = lists:map(fun(_) ->
        {ok, Space} = oz_test_utils:create_space(Config, ?ROOT, ?GEN_NAME()),
        simulate_random_delay(Config),
        generate_members(Config, od_space, Space, od_user, Users),
        generate_members(Config, od_space, Space, od_group, Groups),
        Space
    end, lists:seq(1, ?ENTITY_COUNT)),

    Environment#environment{
        spaces = Spaces,
        shares = lists:flatmap(fun(Space) ->
            lists:map(fun(_) ->
                {ok, Share} = oz_test_utils:create_share(
                    Config, ?ROOT, str_utils:rand_hex(16), ?GEN_NAME(), ?ROOT_FILE_ID, Space
                ),
                simulate_random_delay(Config),
                Share
            end, lists:seq(1, ?MEMBERS_COUNT))
        end, Spaces)
    }.


set_up_providers_and_clusters(Config, Environment = #environment{users = Users, groups = Groups, spaces = Spaces}) ->
    Providers = lists:map(fun(_) ->
        {ok, {Provider, _}} = oz_test_utils:create_provider(Config, undefined, ?GEN_NAME()),
        simulate_random_delay(Config),
        Cluster = Provider,
        generate_members(Config, od_cluster, Cluster, od_user, Users),
        generate_members(Config, od_cluster, Cluster, od_group, Groups),
        lists:map(fun(Space) ->
            SupportSize = oz_test_utils:minimum_support_size(Config) + case rand:uniform(3) of
                1 -> rand:uniform(1000000000000);
                2 -> rand:uniform(10000000000);
                3 -> rand:uniform(10000000)
            end,
            oz_test_utils:support_space(Config, Provider, Space, SupportSize)
        end, ?RAND_SUBLIST(Spaces, ?MEMBERS_COUNT)),
        Provider
    end, lists:seq(1, ?ENTITY_COUNT)),
    generate_members(Config, od_cluster, ?ONEZONE_CLUSTER_ID, od_user, Users),
    generate_members(Config, od_cluster, ?ONEZONE_CLUSTER_ID, od_group, Groups),
    Environment#environment{
        providers = Providers,
        clusters = [?ONEZONE_CLUSTER_ID | Providers]
    }.


set_up_handle_services_and_handles(Config, Environment = #environment{users = Users, groups = Groups, shares = Shares}) ->
    HandleServices = lists:map(fun(_) ->
        Data = maps:merge(?DOI_SERVICE, #{<<"name">> => ?GEN_NAME()}),
        {ok, HService} = oz_test_utils:create_handle_service(Config, ?ROOT, Data),
        simulate_random_delay(Config),
        generate_members(Config, od_handle_service, HService, od_user, Users),
        generate_members(Config, od_handle_service, HService, od_group, Groups),
        HService
    end, lists:seq(1, ?ENTITY_COUNT)),

    HandlesCountPerService = ?MEMBERS_COUNT div 2 + 1,
    % Shuffle the shares and assign a portion (HandlesCountPerService) to each handle service
    ShuffledShares = utils:random_shuffle(Shares),
    {HServicesAndShares, _} = lists:mapfoldl(fun(Hservice, SharesToAssign) ->
        {SharesForHService, RemainingToAssign} = lists:split(HandlesCountPerService, SharesToAssign),
        {{Hservice, SharesForHService}, RemainingToAssign}
    end, ShuffledShares, HandleServices),

    Environment#environment{
        handle_services = HandleServices,
        handles = lists:flatmap(fun({HService, SharesToAssign}) ->
            lists:map(fun(Share) ->
                {ok, Handle} = oz_test_utils:create_handle(Config, ?ROOT, ?HANDLE(HService, Share)),
                simulate_random_delay(Config),
                generate_members(Config, od_handle, Handle, od_user, Users),
                generate_members(Config, od_handle, Handle, od_group, Groups),
                Handle
            end, SharesToAssign)
        end, HServicesAndShares)
    }.


set_up_harvesters(Config, Environment = #environment{users = Users, groups = Groups, spaces = Spaces}) ->
    Environment#environment{
        harvesters = lists:map(fun(_) ->
            Data = maps:merge(?HARVESTER_CREATE_DATA, #{<<"name">> => ?GEN_NAME()}),
            {ok, Harvester} = oz_test_utils:create_harvester(Config, ?ROOT, Data),
            simulate_random_delay(Config),
            generate_members(Config, od_harvester, Harvester, od_user, Users),
            generate_members(Config, od_harvester, Harvester, od_group, Groups),
            lists:map(fun(Space) ->
                oz_test_utils:harvester_add_space(Config, Harvester, Space)
            end, ?RAND_SUBLIST(Spaces, ?MEMBERS_COUNT)),
            Harvester
        end, lists:seq(1, ?ENTITY_COUNT))
    }.


generate_members(Config, ParentType, ParentId, MemberType, AllMembers) ->
    [add_member(Config, ParentType, ParentId, MemberType, M) || M <- ?RAND_SUBLIST(AllMembers, ?MEMBERS_COUNT)].


add_member(Config, ParentType, ParentId, MemberType, MemberId) ->
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
    oz_test_utils:call_oz(Config, Module, Function, [?ROOT, ParentId, MemberId, ?RAND_SUBLIST(Privileges)]).

%%%===================================================================
%%% Tests
%%%===================================================================

db_browser_test(Config) ->
    try
        db_browser_test_unsafe(Config)
    catch Type:Reason ->
        ct:pal("db_browser test failed with ~w:~w~nStacktrace: ~s", [
            Type, Reason, lager:pr_stacktrace(erlang:get_stacktrace())
        ]),
        error(test_failed)
    end.


db_browser_test_unsafe(Config) ->
    Env = set_up_environment(Config),
    AllCollections = oz_test_utils:call_oz(Config, db_browser, all_collections, []),
    lists:foreach(fun(Collection) ->
        print_collection(Config, Env, Collection)
    end, [help | AllCollections]).


print_collection(Config, Env, Collection) ->
    CollectionWithExistingId = case Collection of
        {C, <<"user_id">>} -> {C, utils:random_element(Env#environment.users)};
        {C, <<"group_id">>} -> {C, utils:random_element(Env#environment.groups)};
        {C, <<"space_id">>} -> {C, utils:random_element(Env#environment.spaces)};
        {C, <<"provider_id">>} -> {C, utils:random_element(Env#environment.providers)};
        {C, <<"cluster_id">>} -> {C, utils:random_element(Env#environment.clusters)};
        {C, <<"handle_service_id">>} -> {C, utils:random_element(Env#environment.handle_services)};
        {C, <<"handle_id">>} -> {C, utils:random_element(Env#environment.handles)};
        {C, <<"harvester_id">>} -> {C, utils:random_element(Env#environment.harvesters)};
        C when is_atom(C) -> C
    end,
    CollectionAtom = case CollectionWithExistingId of
        {Atom, Binary} -> list_to_atom(str_utils:format("~s@~s", [Atom, Binary]));
        Atom when is_atom(Atom) -> Atom
    end,
    SortBy = utils:random_element([default | example_column_names()]),
    SortOrder = utils:random_element([asc, desc]),
    Args = case rand:uniform(4) of
        1 -> [CollectionAtom];
        2 -> [CollectionAtom, SortBy];
        3 -> [CollectionAtom, SortOrder];
        4 -> [CollectionAtom, SortBy, SortOrder]
    end,
    Result = oz_test_utils:call_oz(Config, db_browser, format, Args),
    case string:find(Result, "\n0 entries in total") of
        nomatch ->
            % Okay, dump the results to logs so that they can be examined
            ct:pal("~w:~n~s", [Args, oz_test_utils:call_oz(Config, db_browser, format, Args)]);
        _ ->
            % Repeat until collection that have at least one entry is found
            print_collection(Config, Env, Collection)
    end.


% The test might randomize columns that are not present in a collection,
% in such case default sorting column is taken.
example_column_names() -> [
    id, name, full_name, username, type, email, created, admin_privs,
    eff_users, eff_groups, users, groups, children, parents, spaces, shares,
    providers, eff_providers, clusters, handle_services, handles, harvesters,
    support, handle, online, version, domain, proxy_endpoint, public_handle
].