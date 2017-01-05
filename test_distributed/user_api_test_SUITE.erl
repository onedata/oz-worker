%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning user API (REST + logic).
%%% @end
%%%-------------------------------------------------------------------
-module(user_api_test_SUITE).
-author("Lukasz Opiola").

-include("rest.hrl").
-include("errors.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-include("api_test_utils.hrl").


%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1
]).
-export([
    create_test/1,
    create_client_token_test/1,
    authorize_test/1,

    get_test/1,
    list_test/1,
    get_oz_privileges_test/1,
    get_default_space_test/1,
    get_space_alias_test/1,
    get_default_provider_test/1,

    update_test/1,
    update_oz_test/1,
    set_default_space_test/1,
    set_space_alias_test/1,
    set_default_provider_test/1,

    delete_test/1,
    delete_oz_privileges_test/1,
    delete_client_token_test/1,
    unset_default_space_test/1,
    delete_space_alias_test/1,
    unset_default_provider_test/1,

    join_group_test/1,
    join_space_test/1,
    get_groups_test/1,
    get_spaces_test/1,
    get_eff_providers_test/1,
    get_handle_services_test/1,
    get_handles_test/1,
    leave_group_test/1,
    leave_space_test/1,
    leave_handle_service_test/1,
    leave_handle_test/1
]).

all() ->
    ?ALL([
        create_test,
        create_client_token_test,
        authorize_test,

        get_test,
        list_test,
        get_oz_privileges_test,
        get_default_space_test,
        get_space_alias_test,
        get_default_provider_test,

        update_test,
        update_oz_test,
        set_default_space_test,
        set_space_alias_test,
        set_default_provider_test,

        delete_test,
        delete_oz_privileges_test,
        delete_client_token_test,
        unset_default_space_test,
        delete_space_alias_test,
        unset_default_provider_test,

        join_group_test,
        join_space_test,
        get_groups_test,
        get_spaces_test,
        get_eff_providers_test,
        get_handle_services_test,
        get_handles_test,
        leave_group_test,
        leave_space_test,
        leave_handle_service_test,
        leave_handle_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    UserRecord = #od_user{name = <<"Name">>, login = <<"login">>},
    {ok, UserId} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_user_logic, create, [UserRecord]
    )),
    {ok, User} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_user_logic, get, [?USER(UserId), UserId]
    )),
    #od_user{name = Name, login = Login} = User,
    ?assertEqual(Name, <<"Name">>),
    ?assertEqual(Login, <<"login">>),
    % Try to create a user with given Id
    PredefinedUserId = <<"ausdhf87adsga87ht2q7hrw">>,
    {ok, PredefinedUserId} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_user_logic, create, [UserRecord, PredefinedUserId]
    )),
    {ok, User2} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_user_logic, get, [?USER(PredefinedUserId), PredefinedUserId]
    )),
    #od_user{name = Name, login = Login} = User2,
    ?assertEqual(Name, <<"Name">>),
    ?assertEqual(Login, <<"login">>),
    % Second try should fail (such id exists)
    ?assertMatch(?ERROR_BAD_VALUE_ID_OCCUPIED(<<"userId">>), oz_test_utils:call_oz(
        Config, n_user_logic, create, [UserRecord, PredefinedUserId]
    )).


create_client_token_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{user, User}]
        },
        data_spec = #data_spec{},
        rest_spec = #rest_spec{
            method = post,
            path = <<"/user/client_tokens">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"token">> => {check_type, binary}}
        },
        logic_spec = #logic_spec{
            operation = create,
            module = n_user_logic,
            function = create_client_token,
            args = [client, User],
            expected_result = ?OK_BINARY
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    % The test should have created some client tokens, check if
    % they are valid
    {ok, #od_user{
        client_tokens = ClientTokens
    }} = oz_test_utils:get_user(Config, User),
    lists:foreach(
        fun(ClientToken) ->
            {ok, Macaroon} = oz_test_utils:call_oz(
                Config, token_utils, deserialize, [ClientToken]
            ),
            ?assertEqual({ok, User}, oz_test_utils:call_oz(
                Config,
                auth_logic,
                validate_token,
                [<<>>, Macaroon, [], undefined, undefined]
            ))
        end, ClientTokens).


authorize_test(Config) ->
    % Create a provider and a user.
    {ok, {Provider, _, _}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    % Generate an auth token, parse the token for 3rd party caveats and check
    % if authorize endpoint works as expected.
    AuthToken = oz_test_utils:call_oz(
        Config, auth_logic, gen_token, [User, Provider]
    ),
    {ok, Macaroon} = oz_test_utils:call_oz(
        Config, token_utils, deserialize, [AuthToken]
    ),
    Caveats = macaroon:third_party_caveats(Macaroon),
    lists:foreach(
        fun({_, CaveatId}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [{user, User}]
                },
                data_spec = #data_spec{
                    required = [<<"identifier">>],
                    correct_values = #{
                        <<"identifier">> => CaveatId
                    },
                    bad_values = [
                        {<<"identifier">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"identifier">>)},
                        {<<"identifier">>, 123, ?ERROR_BAD_VALUE_BINARY(<<"identifier">>)},
                        {<<"identifier">>, <<"Sdfsdf">>, ?ERROR_BAD_VALUE_IDENTIFIER(<<"identifier">>)}
                    ]
                },
                rest_spec = #rest_spec{
                    method = post,
                    path = <<"/user/authorize">>,
                    expected_code = ?HTTP_200_OK,
                    expected_body = {check_type, binary}
                },
                logic_spec = #logic_spec{
                    operation = create,
                    module = n_user_logic,
                    function = authorize,
                    args = [client, User, data],
                    expected_result = ?OK_BINARY
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))
        end, Caveats),
    ok.



get_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{
        name = ExpName = <<"Name">>,
        login = ExpLogin = <<"Login">>,
        alias = ExpAlias = <<"Alias">>,
        email_list = ExpEmailList = [<<"em1@google.com">>, <<"em2@google.com">>]
    }),
    % Create two users, grant one of them the privilege to list users.
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    ok = oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_USERS_LIST
    ]),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),

    ExpectedBody = #{
        <<"name">> => ExpName,
        <<"login">> => ExpLogin,
        <<"alias">> => ExpAlias,
        <<"emailList">> => ExpEmailList
    },
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User},
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/users/">>, User],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpectedBody#{<<"userId">> => User}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = n_user_logic,
            function = get_data,
            args = [client, User],
            expected_result = ?OK_MAP(ExpectedBody)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check REST endpoint dedicated for authorized user.
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [{user, User}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/user/">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"userId">> => User,
                <<"name">> => ExpName,
                <<"login">> => ExpLogin,
                <<"alias">> => ExpAlias,
                <<"emailList">> => ExpEmailList
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


list_test(Config) ->
    % Make sure that users created in other tests are deleted.
    ok = oz_test_utils:delete_all_entities(Config),
    % Create two users, grant one of them the privilege to list users.
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    ok = oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_USERS_LIST
    ]),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    % Create a provider, which should not have privileges to list users.
    {ok, {P1, KeyFile, CertFile}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),

    % For now, there are only two users
    ExpUsers = [Admin, NonAdmin],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}, {provider, P1, KeyFile, CertFile}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/users">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = n_user_logic,
            function = list,
            args = [client],
            expected_result = ?OK_LIST(ExpUsers)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Create some users
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    % Now, more users are expected
    ExpUsers2 = [Admin, NonAdmin, U1, U2, U3],
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}, {provider, P1, KeyFile, CertFile}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/users">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"users">> => ExpUsers2}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = n_user_logic,
            function = list,
            args = [client],
            expected_result = ?OK_LIST(ExpUsers2)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


get_oz_privileges_test(Config) ->
    % Create two users, grant one of them the privilege to view OZ privileges.
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    ok = oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_VIEW_PRIVILEGES
    ]),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    % By default, a new user should not have any oz privileges
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, User}
            ]
        },
        rest_spec = RestSpec = #rest_spec{
            method = get,
            path = [<<"/users/">>, User, <<"/privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => []}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = n_user_logic,
            function = get_data,
            args = [client, User],
            expected_result = ?OK_LIST([])
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    % Also, check path dedicated for user that presents auth
    ApiTestSpecForUser = ApiTestSpec#api_test_spec{
        rest_spec = RestSpec#rest_spec{
            path = <<"/user/privileges">>
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpecForUser)),

    ok.


get_default_space_test(Config) ->
    ok.


get_space_alias_test(Config) ->
    ok.


get_default_provider_test(Config) ->
    ok.



update_test(Config) ->
    ok.


update_oz_test(Config) ->
    ok.


set_default_space_test(Config) ->
    ok.


set_space_alias_test(Config) ->
    ok.


set_default_provider_test(Config) ->
    ok.



delete_test(Config) ->
    ok.


delete_oz_privileges_test(Config) ->
    ok.


delete_client_token_test(Config) ->
    ok.


unset_default_space_test(Config) ->
    ok.


delete_space_alias_test(Config) ->
    ok.


unset_default_provider_test(Config) ->
    ok.



join_group_test(Config) ->
    ok.


join_space_test(Config) ->
    ok.


get_groups_test(Config) ->
    ok.


get_spaces_test(Config) ->
    ok.


get_eff_providers_test(Config) ->
    ok.


get_handle_services_test(Config) ->
    ok.


get_handles_test(Config) ->
    ok.


leave_group_test(Config) ->
    ok.


leave_space_test(Config) ->
    ok.


leave_handle_service_test(Config) ->
    ok.


leave_handle_test(Config) ->
    ok.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    application:start(etls),
    hackney:start(),
    NewConfig = ?TEST_INIT(
        Config, ?TEST_FILE(Config, "env_desc.json"), [oz_test_utils]
    ),
    NewConfig.


end_per_suite(Config) ->
    hackney:stop(),
    application:stop(etls),
    ok.
%%    test_node_starter:clean_environment(Config).
