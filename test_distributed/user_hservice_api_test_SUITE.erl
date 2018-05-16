%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning user
%%% handle service API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(user_hservice_api_test_SUITE).
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
-include_lib("ctool/include/api_errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1
]).
-export([
    list_handle_services_test/1,
    create_handle_service_test/1,
    get_handle_service_test/1,
    leave_handle_service_test/1,

    list_eff_handle_services_test/1,
    get_eff_handle_service_test/1
]).

all() ->
    ?ALL([
        list_handle_services_test,
        create_handle_service_test,
        get_handle_service_test,
        leave_handle_service_test,

        list_eff_handle_services_test,
        get_eff_handle_service_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_handle_services_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, set, [
        ?OZ_HANDLE_SERVICES_CREATE
    ]),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    ExpHServices = lists:map(
        fun(_) ->
            {ok, HService} = oz_test_utils:create_handle_service(
                Config, ?USER(U1), ?DOI_SERVICE
            ),
            {ok, U2} = oz_test_utils:handle_service_add_user(
                Config, HService, U2
            ),
            HService
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/handle_services">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handle_services">> => ExpHServices}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
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
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_handle_services,
            args = [client, U1],
            expected_result = ?OK_LIST(ExpHServices)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


create_handle_service_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, grant, [
        ?OZ_HANDLE_SERVICES_CREATE
    ]),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U2, grant, [
        ?OZ_HANDLE_SERVICES_CREATE
    ]),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    ExpName = ?HANDLE_SERVICE_NAME1,
    ExpProxyEndPoint = ?PROXY_ENDPOINT,
    ExpProperties = ?DOI_SERVICE_PROPERTIES,

    AllPrivs = oz_test_utils:all_handle_service_privileges(Config),
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
                {user, U1},
                {user, U2}
            ],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/handle_services">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                BaseURL = ?URL(Config, [<<"/user/handle_services/">>]),
                [HServiceId] = binary:split(Location, [BaseURL], [global, trim_all]),
                VerifyFun(HServiceId)
            end
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
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
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
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = create_handle_service,
            args = [client, U1, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_handle_service, aspect = instance},
            auth_hint = ?AS_USER(U1),
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"effectiveGroups">> => #{},
                <<"effectiveUsers">> => #{U1 => AllPrivsBin},
                <<"name">> => ExpName,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    VerifyFun(Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


get_handle_service_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, set, [
        ?OZ_HANDLE_SERVICES_CREATE
    ]),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, HService} = oz_test_utils:create_handle_service(Config, ?USER(U1),
        ?HANDLE_SERVICE_NAME1, ?PROXY_ENDPOINT, ?DOI_SERVICE_PROPERTIES
    ),
    {ok, U2} = oz_test_utils:handle_service_add_user(Config, HService, U2),

    ExpHServiceDetails = #{
        <<"name">> => ?HANDLE_SERVICE_NAME1,
        <<"proxyEndpoint">> => ?PROXY_ENDPOINT,
        <<"serviceProperties">> => ?DOI_SERVICE_PROPERTIES
    },
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/handle_services/">>, HService],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpHServiceDetails#{
                <<"handleServiceId">> => HService
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
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
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_handle_service,
            args = [client, U1, HService],
            expected_result = ?OK_MAP(ExpHServiceDetails)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


leave_handle_service_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, set, [
        ?OZ_HANDLE_SERVICES_CREATE
    ]),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, HService} = oz_test_utils:create_handle_service(
            Config, ?USER(U1), ?DOI_SERVICE
        ),
        {ok, U2} = oz_test_utils:handle_service_add_user(
            Config, HService, U2
        ),
        #{hserviceId => HService}
    end,
    DeleteEntityFun = fun(#{hserviceId := HServiceId} = _Env) ->
        oz_test_utils:handle_service_remove_user(
            Config, HServiceId, U1
        )
    end,
    VerifyEndFun = fun(ShouldSucceed, #{hserviceId := HServiceId} = _Env, _) ->
        {ok, Users} = oz_test_utils:handle_service_get_users(
            Config, HServiceId
        ),
        ?assertEqual(lists:member(U1, Users), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{user, U1}]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/user/handle_services/">>, hserviceId],
            expected_code = ?HTTP_204_NO_CONTENT
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity, [
        Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun
    ])),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
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
        logic_spec = #logic_spec{
            module = user_logic,
            function = leave_handle_service,
            args = [client, U1, hserviceId],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity, [
        Config, ApiTestSpec2, EnvSetUpFun, VerifyEndFun, DeleteEntityFun
    ])).


list_eff_handle_services_test(Config) ->
    {
        [{HS1, _}, {HS2, _}, {HS3, _}, {HS4, _}, {HS5, _}],
        _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_handle_services_env(Config),

    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, U1} = oz_test_utils:handle_service_add_user(Config, HService, U1),
    {ok, U2} = oz_test_utils:handle_service_add_user(Config, HService, U2),

    ExpHServices = [HS1, HS2, HS3, HS4, HS5, HService],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/effective_handle_services">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handle_services">> => ExpHServices}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check that regular client can't make request on behalf of other client
    ApiTestSpec2 = #api_test_spec{
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
        rest_spec = undefined,
        logic_spec = #logic_spec{
            module = user_logic,
            function = get_eff_handle_services,
            args = [client, U1],
            expected_result = ?OK_LIST(ExpHServices)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % Check also user_logic:has_eff_handle_service function
    lists:foreach(
        fun(HServiceId) ->
            ?assert(oz_test_utils:call_oz(
                Config, user_logic, has_eff_handle_service, [U2, HServiceId])
            )
        end, ExpHServices
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, user_logic, has_eff_handle_service, [U2, <<"asdiuc827346w">>])
    ).


get_eff_handle_service_test(Config) ->
    {
        EffHServices, _Groups, {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_handle_services_env(Config),

    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?PID_SERVICE
    ),
    {ok, U1} = oz_test_utils:handle_service_add_user(Config, HService, U1),
    {ok, U2} = oz_test_utils:handle_service_add_user(Config, HService, U2),

    NewEffHServices = [{HService, ?PID_SERVICE} | EffHServices],
    lists:foreach(
        fun({HServiceId, HServiceDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        {user, U1},
                        {user, U2}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/user/effective_handle_services/">>, HServiceId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = HServiceDetails#{
                        <<"handleServiceId">> => HServiceId
                    }
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

            % Check that regular client can't make request
            % on behalf of other client
            ApiTestSpec2 = #api_test_spec{
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
                logic_spec = #logic_spec{
                    module = user_logic,
                    function = get_eff_handle_service,
                    args = [client, U1, HServiceId],
                    expected_result = ?OK_MAP(HServiceDetails)
                }
                % TODO gs
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec2))

        end, NewEffHServices
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
