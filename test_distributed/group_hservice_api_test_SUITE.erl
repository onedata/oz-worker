%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning group handle services
%%% API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(group_hservice_api_test_SUITE).
-author("Bartosz Walkowicz").

-include("http/rest.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    list_handle_services_test/1,
    create_handle_service_test/1,
    get_handle_service_details_test/1,
    leave_handle_service_test/1,
    list_eff_handle_services_test/1,
    get_eff_handle_service_details_test/1
]).

all() ->
    ?ALL([
        list_handle_services_test,
        create_handle_service_test,
        get_handle_service_details_test,
        leave_handle_service_test,
        list_eff_handle_services_test,
        get_eff_handle_service_details_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_handle_services_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    ExpHServices = lists:map(
        fun(_) ->
            {ok, HService} = oz_test_utils:create_handle_service(
                Config, ?ROOT, ?DOI_SERVICE
            ),
            {ok, G1} = oz_test_utils:handle_service_add_group(
                Config, HService, G1
            ),
            HService
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/handle_services">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handle_services">> => ExpHServices}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_handle_services,
            args = [auth, G1],
            expected_result = ?OK_LIST(ExpHServices)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_handle_service_test(Config) ->
    % create group with 2 users:
    %   U2 gets ?GROUP_CREATE_HANDLE_SERVICE privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_CREATE_HANDLE_SERVICE
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    % Both users get the privilege, but U1 should be forbidden to create a
    % handle service on behalf of the group as he lacks the
    % ?GROUP_CREATE_HANDLE_SERVICE privilege.
    oz_test_utils:user_set_oz_privileges(Config, U1, [
        ?OZ_HANDLE_SERVICES_CREATE
    ], []),
    oz_test_utils:user_set_oz_privileges(Config, U2, [
        ?OZ_HANDLE_SERVICES_CREATE
    ], []),

    AllPrivs = privileges:handle_service_privileges(),

    VerifyFun = fun(HServiceId) ->
        oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
        {ok, HS} = oz_test_utils:get_handle_service(Config, HServiceId),
        ?assertEqual(?HANDLE_SERVICE_NAME1, HS#od_handle_service.name),
        ?assertEqual(?PROXY_ENDPOINT, HS#od_handle_service.proxy_endpoint),
        ?assertEqual(
            ?DOI_SERVICE_PROPERTIES, HS#od_handle_service.service_properties
        ),

        ?assertEqual(#{G1 => AllPrivs}, HS#od_handle_service.groups),
        ?assertEqual(#{G1 => {AllPrivs, [{od_handle_service, <<"self">>}]}}, HS#od_handle_service.eff_groups),
        ?assertEqual(#{}, HS#od_handle_service.users),
        ?assertEqual(
            #{
                U1 => {AllPrivs, [{od_group, G1}]},
                U2 => {AllPrivs, [{od_group, G1}]}
            },
            HS#od_handle_service.eff_users
        ),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLE_SERVICES_CREATE, ?OZ_GROUPS_ADD_RELATIONSHIPS]},
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
            path = [<<"/groups/">>, G1, <<"/handle_services">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{?HDR_LOCATION := Location} = _Headers) ->
                BaseURL = ?URL(Config, [<<"/groups/">>, G1, <<"/handle_services/">>]),
                [HServiceId] = binary:split(Location, [BaseURL], [global, trim_all]),
                VerifyFun(HServiceId)
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create_handle_service,
            args = [auth, G1, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_handle_service, aspect = instance},
            auth_hint = ?AS_GROUP(G1),
            expected_result_op = ?OK_MAP_CONTAINS(#{
                <<"name">> => ?HANDLE_SERVICE_NAME1,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = gri:deserialize(EncodedGri),
                    VerifyFun(Id)
                end
            })
        },
        data_spec = #data_spec{
            required = [
                <<"name">>,
                <<"proxyEndpoint">>,
                <<"serviceProperties">>
            ],
            correct_values = #{
                <<"name">> => [?HANDLE_SERVICE_NAME1],
                <<"proxyEndpoint">> => [?PROXY_ENDPOINT],
                <<"serviceProperties">> => [?DOI_SERVICE_PROPERTIES]
            },
            bad_values = [
                {<<"proxyEndpoint">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"proxyEndpoint">>)},
                {<<"serviceProperties">>, 1234,
                    ?ERROR_BAD_VALUE_JSON(<<"serviceProperties">>)},
                {<<"serviceProperties">>, #{},
                    ?ERROR_BAD_VALUE_EMPTY(<<"serviceProperties">>)}
                | ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_handle_service_details_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_VIEW privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    {ok, HService} = oz_test_utils:create_handle_service(Config, ?ROOT, ?DOI_SERVICE),
    {ok, G1} = oz_test_utils:handle_service_add_group(Config, HService, G1),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HANDLE_SERVICES_VIEW]},
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/handle_services/">>, HService],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:protected_hservice(rest, HService, ?DOI_SERVICE, ?SUB(nobody))
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_handle_service,
            args = [auth, G1, HService],
            expected_result = api_test_expect:protected_hservice(logic, HService, ?DOI_SERVICE, ?SUB(nobody))
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


leave_handle_service_test(Config) ->
    % create group with 2 users:
    %   U2 gets the GROUP_LEAVE_HANDLE_SERVICE privilege
    %   U1 gets all remaining privileges
    {G1, U1, U2} = api_test_scenarios:create_basic_group_env(
        Config, ?GROUP_LEAVE_HANDLE_SERVICE
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    EnvSetUpFun = fun() ->
        {ok, HService} = oz_test_utils:create_handle_service(
            Config, ?ROOT, ?PID_SERVICE
        ),
        {ok, G1} = oz_test_utils:handle_service_add_group(
            Config, HService, G1
        ),
        #{hsid => HService}
    end,
    DeleteEntityFun = fun(#{hsid := HService} = _Env) ->
        oz_test_utils:handle_service_remove_group(Config, HService, G1)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{hsid := HService} = _Env, _) ->
        {ok, Groups} = oz_test_utils:handle_service_get_groups(
            Config, HService
        ),
        ?assertEqual(lists:member(G1, Groups), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {admin, [?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_HANDLE_SERVICES_REMOVE_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/groups/">>, G1, <<"/handle_services/">>, hsid],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = leave_handle_service,
            args = [auth, G1, hsid],
            expected_result = ?OK_RES
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_eff_handle_services_test(Config) ->
    {
        [{HS1, _}, {HS2, _}, {HS3, _}, {HS4, _}, {HS5, _}],
        [{G1, _} | _Groups], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_handle_services_env(Config),

    ExpHServices = [HS1, HS2, HS3, HS4, HS5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1},
                {admin, [?OZ_GROUPS_LIST_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/effective_handle_services">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handle_services">> => ExpHServices}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_handle_services,
            args = [auth, G1],
            expected_result = ?OK_LIST(ExpHServices)
        }
        % TODO VFS-4520 Tests for GraphSync API
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also group_logic:has_eff_handle_service function
    lists:foreach(
        fun(HService) ->
            ?assert(oz_test_utils:call_oz(
                Config, group_logic, has_eff_handle_service, [G1, HService])
            )
        end, ExpHServices
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, group_logic, has_eff_handle_service,
        [G1, <<"asdiucyaie827346w">>])
    ).


get_eff_handle_service_details_test(Config) ->
    {
        EffHServices, [{G1, _} | _Groups], {U1, _U2, NonAdmin}
    } = api_test_scenarios:create_eff_handle_services_env(Config),

    lists:foreach(fun({HServiceId, HServiceData}) ->
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {user, U1},
                    {admin, [?OZ_HANDLE_SERVICES_VIEW]}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {user, NonAdmin}
                ]
            },
            rest_spec = #rest_spec{
                method = get,
                path = [<<"/groups/">>, G1, <<"/effective_handle_services/">>, HServiceId],
                expected_code = ?HTTP_200_OK,
                expected_body = api_test_expect:protected_hservice(rest, HServiceId, HServiceData, ?SUB(nobody))
            },
            logic_spec = #logic_spec{
                module = group_logic,
                function = get_eff_handle_service,
                args = [auth, G1, HServiceId],
                expected_result = api_test_expect:protected_hservice(logic, HServiceId, HServiceData, ?SUB(nobody))
            }
            % TODO VFS-4520 Tests for GraphSync API
        },
        ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

    end, EffHServices).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    application:ensure_all_started(hackney),
    ozt:init_per_suite(Config).

end_per_suite(_Config) ->
    application:stop(hackney),
    ssl:stop().

init_per_testcase(_, Config) ->
    ozt_mocks:freeze_time(),
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unfreeze_time().
