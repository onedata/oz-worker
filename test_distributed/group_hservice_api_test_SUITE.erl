%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C): 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning groups handle services
%%% API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(group_hservice_api_test_SUITE).
-author("Bartosz Walkowicz").

-include("rest.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("cluster_worker/include/api_errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1
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
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, revoke, [
        ?GROUP_VIEW
    ]),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),

    ExpHandleServices = lists:map(
        fun(Idx) ->
            {ok, HServiceId} = oz_test_utils:create_handle_service(
                Config, ?ROOT, <<"HS", (Idx+48)/integer>>,
                <<"https://dot.com">>, #{asd => 1}
            ),
            {ok, G1} = oz_test_utils:add_group_to_handle_service(
                Config, HServiceId, G1
            ),
            HServiceId
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
            path = [<<"/groups/">>, G1, <<"/handle_services">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handle_services">> => ExpHandleServices}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_handle_services,
            args = [client, G1],
            expected_result = ?OK_LIST(ExpHandleServices)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_handle_service_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, U2, grant, [
        ?OZ_HANDLE_SERVICES_CREATE
    ]),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),

    ExpName = <<"HS">>,
    ExpProxyEndPoint = <<"http://dot.com">>,
    ExpProperties = #{<<"asd">> => 1},

    AllPrivs = oz_test_utils:call_oz(
        Config, privileges, handle_service_privileges, []
    ),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    VerifyFun = fun(HServiceId) ->
        {ok, HS} = oz_test_utils:get_handle_service(Config, HServiceId),
        ?assertEqual(ExpName, HS#od_handle_service.name),
        ?assertEqual(ExpProxyEndPoint, HS#od_handle_service.proxy_endpoint),
        ?assertEqual(ExpProperties, HS#od_handle_service.service_properties),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
            expected_headers = fun(#{<<"location">> := Location} = _Headers) ->
                [GroupId, HServiceId] = binary:split(
                    Location,
                    [<<"/groups/">>, <<"/handle_services/">>],
                    [trim_all, global]
                ),
                ?assertEqual(GroupId, G1),
                VerifyFun(HServiceId)
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create_handle_service,
            args = [client, G1, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_handle_service, aspect = instance},
            auth_hint = ?AS_GROUP(G1),
            expected_result = ?OK_MAP(#{
                <<"effectiveGroups">> => #{G1 => AllPrivsBin},
                <<"effectiveUsers">> => #{
                    U1 => AllPrivsBin,
                    U2 => AllPrivsBin
                },
                <<"name">> => ExpName,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
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
                <<"name">> => [ExpName],
                <<"proxyEndpoint">> => [ExpProxyEndPoint],
                <<"serviceProperties">> => [ExpProperties]
            },
            bad_values = [
                {<<"name">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"name">>)},
                {<<"name">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"name">>)},
                {<<"proxyEndpoint">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"proxyEndpoint">>)},
                {<<"serviceProperties">>, 1234,
                    ?ERROR_BAD_VALUE_JSON(<<"serviceProperties">>)},
                {<<"serviceProperties">>, #{},
                    ?ERROR_BAD_VALUE_EMPTY(<<"serviceProperties">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_handle_service_details_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, NonAdmin, grant, [
        ?OZ_HANDLE_SERVICES_LIST
    ]),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),

    ExpName = <<"HS">>,
    ExpProxyEndPoint = <<"http://dot.com">>,
    ExpProperties = #{<<"asd">> => 1},
    {ok, HServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ExpName, ExpProxyEndPoint, ExpProperties
    ),
    {ok, G1} = oz_test_utils:add_group_to_handle_service(
        Config, HServiceId, G1
    ),

    ExpMap = #{
        <<"name">> => ExpName,
        <<"proxyEndpoint">> => ExpProxyEndPoint,
        <<"serviceProperties">> => ExpProperties
    },
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
            path = [<<"/groups/">>, G1, <<"/handle_services/">>, HServiceId],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpMap#{
                <<"handleServiceId">> => HServiceId
            }
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_handle_service,
            args = [client, G1, HServiceId],
            expected_result = ?OK_MAP(ExpMap)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


leave_handle_service_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, revoke, [
        ?GROUP_UPDATE
    ]),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_UPDATE
    ]),

    EnvSetUpEnv = fun() ->
        {ok, HServiceId} = oz_test_utils:create_handle_service(
            Config, ?ROOT, <<"HS">>, <<"http://dot.com">>, #{<<"asd">> => 1}
        ),
        {ok, G1} = oz_test_utils:add_group_to_handle_service(
            Config, HServiceId, G1
        ),
        #{hsid => HServiceId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{hsid := HServiceId} = _Env, _) ->
        {ok, Groups} = oz_test_utils:get_handle_service_groups(
            Config, HServiceId
        ),
        ?assertEqual(lists:member(G1, Groups), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
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
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = leave_handle_service,
            args = [client, G1, hsid],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpEnv, undefined, VerifyEndFun
    )).


list_eff_handle_services_test(Config) ->
    {
        [{HS1, _}, {HS2, _}, {HS3, _}, {HS4, _}, {HS5, _}],
        [{G1, _} | _Groups], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_handle_services_env(Config),

    ExpHandleServices = [HS1, HS2, HS3, HS4, HS5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
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
            expected_body = #{<<"handle_services">> => ExpHandleServices}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_handle_services,
            args = [client, G1],
            expected_result = ?OK_LIST(ExpHandleServices)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also group_logic:has_eff_handle_service function
    lists:foreach(
        fun(HServiceId) ->
            ?assert(oz_test_utils:call_oz(
                Config, group_logic, has_eff_handle_service, [G1, HServiceId])
            )
        end, ExpHandleServices
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, group_logic, has_eff_handle_service,
        [G1, <<"asdiucyaie827346w">>])
    ).


get_eff_handle_service_details_test(Config) ->
    {
        EffHandleServicesList, [{G1, _} | _Groups], {U1, _U2, NonAdmin}
    } = api_test_scenarios:create_eff_handle_services_env(Config),

    lists:foreach(
        fun({HServiceId, HSDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, U1}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/groups/">>, G1,
                        <<"/effective_handle_services/">>, HServiceId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = HSDetails#{
                        <<"handleServiceId">> => HServiceId
                    }
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_eff_handle_service,
                    args = [client, G1, HServiceId],
                    expected_result = ?OK_MAP(HSDetails)
                }
                % TODO gs
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffHandleServicesList
    ).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().
