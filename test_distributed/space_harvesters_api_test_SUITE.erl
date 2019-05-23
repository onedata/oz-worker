%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning space harvesters API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(space_harvesters_api_test_SUITE).
-author("Michal Stanisz").

-include("http/rest.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/api_errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_testcase/2, end_per_testcase/2,
    init_per_suite/1, end_per_suite/1
]).
-export([
    join_harvester_test/1,
    remove_harvester_test/1,
    list_harvesters_test/1,
    get_harvester_test/1,
    
    harvest_metadata_test/1,
    
    harvester_index_empty_stats_test/1,
    harvester_remove_add_space_stats_test/1,
    harvester_index_nonempty_stats_test/1
]).

all() ->
    ?ALL([
        join_harvester_test,
        remove_harvester_test,
        list_harvesters_test,
        get_harvester_test,

        harvest_metadata_test,
        
        harvester_index_empty_stats_test,
        harvester_remove_add_space_stats_test,
        harvester_index_nonempty_stats_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


join_harvester_test(Config) ->
    % create space with 2 users:
    %   U2 gets the SPACE_ADD_HARVESTER privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_ADD_HARVESTER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, HarvesterId} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),
        {ok, Macaroon} = oz_test_utils:harvester_invite_space_token(
            Config, ?ROOT, HarvesterId
        ),
        {ok, Token} = onedata_macaroons:serialize(Macaroon),
        #{
            harvesterId => HarvesterId,
            token => Token,
            macaroonId => macaroon:identifier(Macaroon)
        }
    end,
    VerifyEndFun = fun(ShouldSucceed, #{harvesterId := HarvesterId, macaroonId := MacaroonId} = _Env, _) ->
        {ok, Harvesters} = oz_test_utils:space_get_harvesters(Config, S1),
        ?assertEqual(lists:member(HarvesterId, Harvesters), ShouldSucceed),
        case ShouldSucceed of
            true ->
                oz_test_utils:assert_token_not_exists(Config, MacaroonId);
            false -> ok
        end
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_ADD_RELATIONSHIPS]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/spaces/">>, S1, <<"/harvesters/join">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(#{harvesterId := HarvesterId} = _Env, _) ->
                fun(#{<<"Location">> := Location} = _Headers) ->
                    ExpLocation = ?URL(Config,
                        [<<"/spaces/">>, S1, <<"/harvesters/">>, HarvesterId]
                    ),
                    ?assertMatch(ExpLocation, Location),
                    true
                end
            end)
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = join_harvester,
            args = [client, S1, data],
            expected_result = ?OK_ENV(fun(#{harvesterId := HarvesterId} = _Env, _) ->
                ?OK_BINARY(HarvesterId)
            end)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{
                <<"token">> => [fun(#{token := Token} = _Env) ->
                    Token
                end]
            },
            bad_values = [
                {<<"token">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"token">>)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>)},
                {<<"token">>, <<"123qwe">>,
                    ?ERROR_BAD_VALUE_TOKEN(<<"token">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % Check that token is not consumed upon failed operation
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, Harvester} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),
    {ok, Macaroon1} = oz_test_utils:harvester_invite_space_token(
        Config, ?ROOT, Harvester
    ),
    {ok, Token} = onedata_macaroons:serialize(Macaroon1),
    oz_test_utils:harvester_add_space(Config, Harvester, S1),

    ApiTestSpec1 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/spaces/">>, S1, <<"/harvesters/join">>],
            expected_code = ?HTTP_400_BAD_REQUEST
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = join_harvester,
            args = [client, S1, data],
            expected_result = ?ERROR_REASON(?ERROR_RELATION_ALREADY_EXISTS(od_harvester, Harvester, od_space, S1))
        },
        % TODO gs
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{<<"token">> => [Token]}
        }
    },
    VerifyEndFun1 = fun(_ShouldSucceed,_Env,_) ->
        oz_test_utils:assert_token_exists(Config, macaroon:identifier(Macaroon1))
    end,
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec1, undefined, undefined, VerifyEndFun1
    )).


remove_harvester_test(Config) ->
    % create space with 2 users:
    %   U2 gets the REMOVE_HARVESTER privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_REMOVE_HARVESTER
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, H1} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),
        {ok, S1} = oz_test_utils:harvester_add_space(Config, H1, S1),
        #{harvesterId => H1}
    end,
    DeleteEntityFun = fun(#{harvesterId := HarvesterId} = _Env) ->
        oz_test_utils:space_remove_harvester(Config, S1, HarvesterId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{harvesterId := HarvesterId} = _Env, _) ->
        {ok, Harvesters} = oz_test_utils:space_get_harvesters(Config, S1),
        ?assertEqual(lists:member(HarvesterId, Harvesters), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_REMOVE_RELATIONSHIPS, ?OZ_HARVESTERS_REMOVE_RELATIONSHIPS]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/spaces/">>, S1, <<"/harvesters/">>, harvesterId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = remove_harvester,
            args = [client, S1, harvesterId],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_harvesters_test(Config) ->
    % create space with 2 users:
    %   U2 gets the SPACE_VIEW privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    ExpHarvesters = lists:map(
        fun(_) ->
            {ok, HarvesterId} = oz_test_utils:create_harvester(
                Config, ?ROOT, ?HARVESTER_CREATE_DATA
            ),
            oz_test_utils:harvester_add_space(Config, HarvesterId, S1),
            HarvesterId
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_LIST_RELATIONSHIPS]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, S1, <<"/harvesters">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"harvesters">> => ExpHarvesters}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_harvesters,
            args = [client, S1],
            expected_result = ?OK_LIST(ExpHarvesters)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_harvester_test(Config) ->
    % create space with 2 users:
    %   U2 gets the SPACE_VIEW privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, H1} = oz_test_utils:create_harvester(
        Config, ?ROOT, ?HARVESTER_CREATE_DATA(?HARVESTER_NAME2)
    ),
    oz_test_utils:harvester_add_space(Config, H1, S1),


    ExpData = ?HARVESTER_PROTECTED_DATA(?HARVESTER_NAME2),
    
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_VIEW]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, S1, <<"/harvesters/">>, H1],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpData#{<<"harvesterId">> => H1}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_harvester,
            args = [client, S1, H1],
            expected_result = ?OK_MAP_CONTAINS(ExpData#{<<"plugin">> => ?HARVESTER_MOCK_PLUGIN})
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


harvest_metadata_test(Config) ->
    {ok, H1} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),
    {ok, H2} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),

    {ok, {P1, M1}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, {P2, M2}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
    {ok, S1} = oz_test_utils:support_space(Config, P1, S1, oz_test_utils:minimum_support_size(Config)),

    {ok, Index1} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),
    {ok, Index2} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA(<<"fail">>)),
    {ok, Index3} = oz_test_utils:harvester_create_index(Config, H2, ?HARVESTER_INDEX_CREATE_DATA),

    oz_test_utils:harvester_add_space(Config, H1, S1),
    oz_test_utils:harvester_add_space(Config, H2, S1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    
    NotExistingHarvester = <<"not_existing_harvester">>,
    Destination = #{H1 => [Index1, Index2], H2 => [Index3, <<"not_existing_index">>], NotExistingHarvester => []},
    Guid = file_id:pack_guid(<<"file_id">>, S1), 
    {ok, FileId} = file_id:guid_to_objectid(Guid),
    
    ExpectedResult = fun(Error) -> #{
        H1 => #{Index2 => 1}, 
        NotExistingHarvester => #{<<"error">> => Error}
    } end,
    
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {provider, P1, M1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {provider, P2, M2}
            ]
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = harvest_metadata,
            args = [client, S1, data],
            expected_result = ?OK_MAP(ExpectedResult(?ERROR_NOT_FOUND))
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_space, id = S1, aspect = harvest_metadata},
            expected_result = ?OK_MAP(ExpectedResult(gs_protocol_errors:error_to_json(0, ?ERROR_NOT_FOUND)))
        },
        data_spec = #data_spec{
            required = [<<"destination">>, <<"maxSeq">>, <<"batch">>],
            correct_values = #{
                <<"destination">> => [Destination],
                <<"maxSeq">> => [10],
                <<"batch">> => [?HARVESTER_BATCH(FileId)]
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


harvester_index_empty_stats_test(Config) ->
    {ok, H1} = oz_test_utils:create_harvester(Config, ?ROOT,
        ?HARVESTER_CREATE_DATA(?HARVESTER_NAME1, ?HARVESTER_MOCK_PLUGIN_BINARY)),
    {ok, H2} = oz_test_utils:create_harvester(Config, ?ROOT,
        ?HARVESTER_CREATE_DATA(?HARVESTER_NAME1, ?HARVESTER_MOCK_PLUGIN_BINARY)),

    {ok, {P1, _M1}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, {P2, _M2}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),

    {ok, Index1} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),
    {ok, Index2} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),
    {ok, Index3} = oz_test_utils:harvester_create_index(Config, H2, ?HARVESTER_INDEX_CREATE_DATA),

    % created indices have no stats
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?assertEqual({ok, #{}}, oz_test_utils:harvester_get_index_stats(Config, H1, Index1)),
    ?assertEqual({ok, #{}}, oz_test_utils:harvester_get_index_stats(Config, H1, Index2)),
    ?assertEqual({ok, #{}}, oz_test_utils:harvester_get_index_stats(Config, H2, Index3)),

    % adding space to harvester results in this space appearing in indices stats in given harvester
    oz_test_utils:harvester_add_space(Config, H1, S1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?assertEqual({ok, #{S1 => #{}}}, oz_test_utils:harvester_get_index_stats(Config, H1, Index1)),
    ?assertEqual({ok, #{S1 => #{}}}, oz_test_utils:harvester_get_index_stats(Config, H1, Index2)),
    ?assertEqual({ok, #{}}, oz_test_utils:harvester_get_index_stats(Config, H2, Index3)),

    oz_test_utils:harvester_add_space(Config, H2, S1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?assertEqual({ok, #{S1 => #{}}}, oz_test_utils:harvester_get_index_stats(Config, H2, Index3)),

    % adding space support results in provider appearing in indices stats in all harvesters of this space
    {ok, S1} = oz_test_utils:support_space(Config, P1, S1, oz_test_utils:minimum_support_size(Config)),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS}}}, 
        oz_test_utils:harvester_get_index_stats(Config, H1, Index1)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS}}}, 
        oz_test_utils:harvester_get_index_stats(Config, H1, Index2)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS}}}, 
        oz_test_utils:harvester_get_index_stats(Config, H2, Index3)),

    % index created in harvester with existing relations have adequate spaces and providers in stats
    {ok, Index4} = oz_test_utils:harvester_create_index(Config, H2, ?HARVESTER_INDEX_CREATE_DATA),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS}}}, 
        oz_test_utils:harvester_get_index_stats(Config, H2, Index4)),

    % adding another space support results in provider appearing in indices stats in all harvesters of this space
    {ok, S1} = oz_test_utils:support_space( Config, P2, S1, oz_test_utils:minimum_support_size(Config)),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS, P2 => ?EMPTY_INDEX_STATS}}},
        oz_test_utils:harvester_get_index_stats(Config, H1, Index1)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS, P2 => ?EMPTY_INDEX_STATS}}},
        oz_test_utils:harvester_get_index_stats(Config, H1, Index2)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS, P2 => ?EMPTY_INDEX_STATS}}},
        oz_test_utils:harvester_get_index_stats(Config, H2, Index3)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS, P2 => ?EMPTY_INDEX_STATS}}}, 
        oz_test_utils:harvester_get_index_stats(Config, H2, Index4)),

    % removing space support results in stats being marked archival for this provider in all harvesters of this space
    ok = oz_test_utils:unsupport_space(Config, P1, S1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS}}},
        oz_test_utils:harvester_get_index_stats(Config, H1, Index1)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS}}},
        oz_test_utils:harvester_get_index_stats(Config, H1, Index2)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS}}},
        oz_test_utils:harvester_get_index_stats(Config, H2, Index3)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS}}}, 
        oz_test_utils:harvester_get_index_stats(Config, H2, Index4)),
    
    % removing space from harvester marks all stats for this space in given harvester archival
    ok = oz_test_utils:harvester_remove_space(Config, H1, S1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS(true)}}},
        oz_test_utils:harvester_get_index_stats(Config, H1, Index1)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS(true)}}},
        oz_test_utils:harvester_get_index_stats(Config, H1, Index2)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS}}},
        oz_test_utils:harvester_get_index_stats(Config, H2, Index3)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS}}},
        oz_test_utils:harvester_get_index_stats(Config, H2, Index4)),

    % removing provider results in deletion of stats entries in all effective harvesters of this provider
    ok = oz_test_utils:delete_provider(Config, P2),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS(true)}}},
        oz_test_utils:harvester_get_index_stats(Config, H1, Index1)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS(true)}}},
        oz_test_utils:harvester_get_index_stats(Config, H1, Index2)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true)}}},
        oz_test_utils:harvester_get_index_stats(Config, H2, Index3)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true)}}},
        oz_test_utils:harvester_get_index_stats(Config, H2, Index4)),

    % restoring space support results in removal of archival flag in effective harvesters of this provider
    {ok, S1} = oz_test_utils:support_space(Config, P1, S1, oz_test_utils:minimum_support_size(Config)),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS(true)}}},
        oz_test_utils:harvester_get_index_stats(Config, H1, Index1)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS(true)}}},
        oz_test_utils:harvester_get_index_stats(Config, H1, Index2)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS}}},
        oz_test_utils:harvester_get_index_stats(Config, H2, Index3)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS}}},
        oz_test_utils:harvester_get_index_stats(Config, H2, Index4)),

    % removing space results in removing all stats from harvesters that belong to this space
    ok = oz_test_utils:delete_space(Config, S1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS(true)}}},
        oz_test_utils:harvester_get_index_stats(Config, H1, Index1)),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS(true)}}},
        oz_test_utils:harvester_get_index_stats(Config, H1, Index2)),
    ?assertEqual({ok, #{}}, 
        oz_test_utils:harvester_get_index_stats(Config, H2, Index3)),
    ?assertEqual({ok, #{}}, 
        oz_test_utils:harvester_get_index_stats(Config, H2, Index4)).


harvester_remove_add_space_stats_test(Config) ->
    {ok, H1} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),
    {ok, Index1} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),
    {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
    {ok, {P1, _M1}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, {P2, _M2}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, S1} = oz_test_utils:support_space(Config, P1, S1, oz_test_utils:minimum_support_size(Config)),
    {ok, S1} = oz_test_utils:support_space(Config, P2, S1, oz_test_utils:minimum_support_size(Config)),
    oz_test_utils:harvester_add_space(Config, H1, S1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS, P2 => ?EMPTY_INDEX_STATS}}},
        oz_test_utils:harvester_get_index_stats(Config, H1, Index1)),

    % removing space from harvester marks all stats for this space in given harvester archival
    ok = oz_test_utils:harvester_remove_space(Config, H1, S1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS(true)}}},
        oz_test_utils:harvester_get_index_stats(Config, H1, Index1)),

    % removing support from space does not change stats in harvester this space no longer belongs to
    ok = oz_test_utils:unsupport_space(Config, P1, S1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS(true)}}},
        oz_test_utils:harvester_get_index_stats(Config, H1, Index1)),

    % adding again space to harvester removes archival flag only in providers supporting this space
    oz_test_utils:harvester_add_space(Config, H1, S1),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ?assertEqual({ok, #{S1 => #{P1 => ?EMPTY_INDEX_STATS(true), P2 => ?EMPTY_INDEX_STATS}}},
        oz_test_utils:harvester_get_index_stats(Config, H1, Index1)).


harvester_index_nonempty_stats_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config),

    {ok, H1} = oz_test_utils:create_harvester(Config, ?ROOT,
        ?HARVESTER_CREATE_DATA(?HARVESTER_NAME1, ?HARVESTER_MOCK_PLUGIN_BINARY)),

    {ok, {P1, _M1}} = oz_test_utils:create_provider(Config, ?PROVIDER_NAME1),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S1} = oz_test_utils:support_space(Config, P1, S1, oz_test_utils:minimum_support_size(Config)),

    {ok, Index1} = oz_test_utils:harvester_create_index(Config, H1, ?HARVESTER_INDEX_CREATE_DATA),
    oz_test_utils:harvester_add_space(Config, H1, S1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ?assertEqual({ok, ?NO_FAILED_INDICES}, oz_test_utils:harvester_submit_batch(
        Config, P1, H1, [Index1], S1, [?HARVESTER_MOCK_BATCH_ENTRY(2, submit)], 10)),
    
    {ok, #{S1 := #{P1 := Stats1}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index1),
    ?assertEqual(false, maps:get(<<"archival">>, Stats1)),
    
    
    oz_test_utils:harvester_remove_space(Config, H1, S1),
    {ok, #{S1 := #{P1 := Stats2}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index1),
    ?assertEqual(true, maps:get(<<"archival">>, Stats2)),
    assert_equal_stats_fields([<<"currentSeq">>, <<"maxSeq">>, <<"error">>, <<"lastUpdate">>], Stats1, Stats2),

    oz_test_utils:harvester_add_space(Config, H1, S1),
    {ok, #{S1 := #{P1 := Stats3}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index1),
    ?assertEqual(false, maps:get(<<"archival">>, Stats3)),
    assert_equal_stats_fields([<<"currentSeq">>, <<"maxSeq">>, <<"error">>, <<"lastUpdate">>], Stats1, Stats3),
    
    oz_test_utils:unsupport_space(Config, P1, S1),
    {ok, #{S1 := #{P1 := Stats4}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index1),
    ?assertEqual(true, maps:get(<<"archival">>, Stats4)),
    assert_equal_stats_fields([<<"currentSeq">>, <<"maxSeq">>, <<"error">>, <<"lastUpdate">>], Stats1, Stats4),

    {ok, S1} = oz_test_utils:support_space(Config, P1, S1, oz_test_utils:minimum_support_size(Config)),
    {ok, #{S1 := #{P1 := Stats5}}} = oz_test_utils:harvester_get_index_stats(Config, H1, Index1),
    ?assertEqual(false, maps:get(<<"archival">>, Stats5)),
    assert_equal_stats_fields([<<"currentSeq">>, <<"maxSeq">>, <<"error">>, <<"lastUpdate">>], Stats1, Stats5).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

init_per_testcase(_, Config) ->
    oz_test_utils:mock_harvester_plugins(Config, ?HARVESTER_MOCK_PLUGIN).

end_per_testcase(_, Config) ->
    oz_test_utils:unmock_harvester_plugins(Config, ?HARVESTER_MOCK_PLUGIN).

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().


%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_equal_stats_fields(Fields, Stats1, Stats2) ->
    lists:foreach(fun(Field) ->
        ?assertEqual(maps:get(Field, Stats1), maps:get(Field, Stats2)) 
    end, Fields).

