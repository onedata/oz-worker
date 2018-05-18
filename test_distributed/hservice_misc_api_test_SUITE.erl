%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning
%%% handle service basic API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(hservice_misc_api_test_SUITE).
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
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    create_test/1,
    list_test/1,
    get_test/1,
    update_test/1,
    delete_test/1,

    list_handles_test/1,
    get_handle_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        get_test,
        update_test,
        delete_test,

        list_handles_test,
        get_handle_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


create_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U2, set, [
        ?OZ_HANDLE_SERVICES_CREATE
    ]),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U3, set, [
        ?OZ_HANDLE_SERVICES_CREATE
    ]),

    VerifyFun = fun(HServiceId, ExpProperties) ->
        {ok, HService} = oz_test_utils:get_handle_service(Config, HServiceId),
        ?assertEqual(?HANDLE_SERVICE_NAME1, HService#od_handle_service.name),
        ?assertEqual(?PROXY_ENDPOINT, HService#od_handle_service.proxy_endpoint),
        ?assertEqual(ExpProperties, HService#od_handle_service.service_properties),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {user, U3}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/handle_services">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = ?OK_ENV(fun(_Env, Data) ->
                ExpProperties = maps:get(<<"serviceProperties">>, Data),
                fun(#{<<"Location">> := Location} = _Headers) ->
                    BaseURL = ?URL(Config, [<<"/user/handle_services/">>]),
                    [HServiceId] = binary:split(Location, [BaseURL], [global, trim_all]),
                    VerifyFun(HServiceId, ExpProperties)
                end
            end)
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = create,
            args = [client, data],
            expected_result = ?OK_ENV(fun(_Env, Data) ->
                ExpProperties = maps:get(<<"serviceProperties">>, Data),
                ?OK_TERM(fun(Result) -> VerifyFun(Result, ExpProperties) end)
            end)
        },
        data_spec = #data_spec{
            required = [
                <<"name">>, <<"proxyEndpoint">>, <<"serviceProperties">>
            ],
            correct_values = #{
                <<"name">> => [?HANDLE_SERVICE_NAME1],
                <<"proxyEndpoint">> => [?PROXY_ENDPOINT],
                <<"serviceProperties">> => [
                    ?DOI_SERVICE_PROPERTIES, ?PID_SERVICE_PROPERTIES
                ]
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
                {user, U3}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, U2}
            ]
        },
        rest_spec = undefined,
        logic_spec = undefined,
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_handle_service, aspect = instance},
            auth_hint = ?AS_USER(U3),
            expected_result = ?OK_ENV(fun(_Env, Data) ->
                ExpProperties = maps:get(<<"serviceProperties">>, Data),
                ?OK_MAP_CONTAINS(#{
                    <<"name">> => ?HANDLE_SERVICE_NAME1,
                    <<"gri">> => fun(EncodedGri) ->
                        #gri{id = Id} = oz_test_utils:decode_gri(
                            Config, EncodedGri
                        ),
                        VerifyFun(Id, ExpProperties)
                    end
                })
            end)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


list_test(Config) ->
    % Make sure that handle services created in other tests are deleted.
    oz_test_utils:delete_all_entities(Config),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, set, [
        ?OZ_HANDLE_SERVICES_CREATE
    ]),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, set, [
        ?OZ_HANDLE_SERVICES_LIST
    ]),

    ExpHServices = lists:map(
        fun(ServiceDetails) ->
            {ok, HServiceId} = oz_test_utils:create_handle_service(
                Config, ?USER(U1), ServiceDetails
            ),
            HServiceId
        end, [?DOI_SERVICE, ?PID_SERVICE, ?DOI_SERVICE, ?PID_SERVICE]
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/handle_services">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handle_services">> => ExpHServices}
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = list,
            args = [client],
            expected_result = ?OK_LIST(ExpHServices)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also handle_service_logic:exist function
    lists:foreach(
        fun(HService) ->
            ?assert(oz_test_utils:call_oz(
                Config, handle_service_logic, exists, [HService])
            )
        end, ExpHServices
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, handle_service_logic, exists, [<<"asdiucyaie827346w">>])
    ).


get_test(Config) ->
    % create handle service with 2 users:
    %   U2 gets the HANDLE_SERVICE_VIEW privilege
    %   U1 gets all remaining privileges
    {HService, U1, U2} = api_test_scenarios:create_basic_doi_hservice_env(
        Config, ?HANDLE_SERVICE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, grant, [
        ?OZ_HANDLE_SERVICES_LIST
    ]),

    AllPrivs = oz_test_utils:all_handle_service_privileges(Config),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    % Get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, Admin},
                {user, NonAdmin},
                {user, U1}
            ]
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = get,
            args = [client, HService],
            expected_result = ?OK_TERM(
                fun(#od_handle_service{
                    name = Name, proxy_endpoint = ProxyEndpoint,
                    service_properties = ServiceProperties,
                    users = Users, groups = #{}, handles = [],
                    eff_users = EffUsers, eff_groups = #{},
                    bottom_up_dirty = false
                }) ->
                    ?assertEqual(?HANDLE_SERVICE_NAME1, Name),
                    ?assertEqual(?PROXY_ENDPOINT, ProxyEndpoint),
                    ?assertEqual(?DOI_SERVICE_PROPERTIES, ServiceProperties),
                    ?assertEqual(Users, #{
                        U1 => AllPrivs -- [?HANDLE_SERVICE_VIEW],
                        U2 => [?HANDLE_SERVICE_VIEW]}
                    ),
                    ?assertEqual(EffUsers, #{
                        U1 => {AllPrivs -- [?HANDLE_SERVICE_VIEW],
                            [{od_handle_service, HService}]},
                        U2 => {[?HANDLE_SERVICE_VIEW],
                            [{od_handle_service, HService}]}
                    })
                end
            )
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_handle_service, id = HService, aspect = instance
            },
            expected_result = ?OK_MAP(#{
                <<"name">> => ?HANDLE_SERVICE_NAME1,
                <<"effectiveUsers">> => #{
                    U1 => AllPrivsBin -- [<<"handle_service_view">>],
                    U2 => [<<"handle_service_view">>]
                },
                <<"effectiveGroups">> => #{},
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(HService, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPrivateDataApiTestSpec)),

    % Get and check protected data
    GetSharedDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
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
            path = [<<"/handle_services/">>, HService],
            expected_code = ?HTTP_200_OK,
            expected_body = ?DOI_SERVICE#{
                <<"handleServiceId">> => HService
            }
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = get_protected_data,
            args = [client, HService],
            expected_result = ?OK_MAP(?DOI_SERVICE)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, GetSharedDataApiTestSpec)).


update_test(Config) ->
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
        oz_test_utils:handle_service_set_user_privileges(Config, HService, U1,
            revoke, [?HANDLE_SERVICE_UPDATE]
        ),
        {ok, U2} = oz_test_utils:handle_service_add_user(
            Config, HService, U2
        ),
        oz_test_utils:handle_service_set_user_privileges(Config, HService, U2,
            set, [?HANDLE_SERVICE_UPDATE]
        ),
        #{hserviceId => HService}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{hserviceId := HServiceId}, Data) ->
        {ok, HService} = oz_test_utils:get_handle_service(Config, HServiceId),
        ExpName = case ShouldSucceed of
            false -> ?HANDLE_SERVICE_NAME1;
            true -> maps:get(<<"name">>, Data, ?HANDLE_SERVICE_NAME1)
        end,
        ?assertEqual(ExpName, HService#od_handle_service.name)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/handle_services/">>, hserviceId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = update,
            args = [client, hserviceId, data],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{
                type = od_handle_service, id = hserviceId, aspect = instance
            },
            expected_result = ?OK
        },
        data_spec = #data_spec{
            at_least_one = [
                <<"name">>, <<"proxyEndpoint">>, <<"serviceProperties">>
            ],
            correct_values = #{
                <<"name">> => [fun() -> ?UNIQUE_STRING end],
                <<"proxyEndpoint">> => [?PROXY_ENDPOINT],
                <<"serviceProperties">> => [?PID_SERVICE_PROPERTIES]
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
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
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
        oz_test_utils:handle_service_set_user_privileges(Config, HService, U1,
            revoke, [?HANDLE_SERVICE_DELETE]
        ),
        {ok, U2} = oz_test_utils:handle_service_add_user(
            Config, HService, U2
        ),
        oz_test_utils:handle_service_set_user_privileges(Config, HService, U2,
            set, [?HANDLE_SERVICE_DELETE]
        ),
        #{hserviceId => HService}
    end,
    DeleteEntityFun = fun(#{hserviceId := HService} = _Env) ->
        oz_test_utils:delete_handle_service(Config, HService)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{hserviceId := HService} = _Env, _) ->
        {ok, HServices} = oz_test_utils:list_handle_services(Config),
        ?assertEqual(lists:member(HService, HServices), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
            path = [<<"/handle_services/">>, hserviceId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = delete,
            args = [client, hserviceId],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{
                type = od_handle_service, id = hserviceId, aspect = instance
            },
            expected_result = ?OK
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_handles_test(Config) ->
    % create handle service with 2 users:
    %   U2 gets the HANDLE_SERVICE_VIEW privilege
    %   U1 gets all remaining privileges
    {HService, U1, U2} = api_test_scenarios:create_basic_doi_hservice_env(
%%        TODO ?HANDLE_SERVICE_LIST_HANDLES
        Config, ?HANDLE_SERVICE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(U2), ?SPACE_NAME2),

    % Create 3 handles, 2 for S1 and 1 for S2
    ExpHandles = lists:map(
        fun(SpaceId) ->
            ShareId = ?UNIQUE_STRING,
            {ok, ShareId} = oz_test_utils:create_share(
                Config, ?ROOT, ShareId, ?SHARE_NAME1, ?ROOT_FILE_ID, SpaceId
            ),
            {ok, HandleId} = oz_test_utils:create_handle(
                Config, ?ROOT, ?HANDLE(HService, ShareId)
            ),
            HandleId
        end, [S1, S1, S2]
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
            path = [<<"/handle_services/">>, HService, <<"/handles">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handles">> => ExpHandles}
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = get_handles,
            args = [client, HService],
            expected_result = ?OK_LIST(ExpHandles)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_handle_test(Config) ->
    % create handle service with 2 users:
    %   U2 gets the HANDLE_SERVICE_VIEW privilege
    %   U1 gets all remaining privileges
    {HService, U1, U2} = api_test_scenarios:create_basic_doi_hservice_env(
        Config, ?HANDLE_SERVICE_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ?SHARE_ID_1, ?SHARE_NAME1, ?ROOT_FILE_ID, S1
    ),

    HandleDetails = ?HANDLE(HService, ShareId),
    {ok, HandleId} = oz_test_utils:create_handle(Config, ?ROOT, HandleDetails),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [
                <<"/handle_services/">>, HService, <<"/handles/">>, HandleId
            ],
            expected_code = ?HTTP_200_OK,
            expected_body = {contains, HandleDetails}
        },
        logic_spec = #logic_spec{
            module = handle_service_logic,
            function = get_handle,
            args = [client, HService, HandleId],
            expected_result = ?OK_MAP_CONTAINS(HandleDetails)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


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


init_per_testcase(list_handles_test, Config) ->
    init_per_testcase(handle_tests, Config);
init_per_testcase(get_handle_test, Config) ->
    init_per_testcase(handle_tests, Config);
init_per_testcase(handle_tests, Config) ->
    oz_test_utils:mock_handle_proxy(Config),
    Config;
init_per_testcase(_, Config) ->
    Config.


end_per_testcase(list_handles_test, Config) ->
    end_per_testcase(handle_tests, Config);
end_per_testcase(get_handle_test, Config) ->
    end_per_testcase(handle_tests, Config);
end_per_testcase(handle_tests, Config) ->
    oz_test_utils:delete_all_entities(Config),
    oz_test_utils:unmock_handle_proxy(Config);
end_per_testcase(_, _Config) ->
    ok.
