%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This file contains tests for automatic creation of predefined groups.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_api_test_SUITE).
-author("Lukasz Opiola").

-include("entity_logic.hrl").
-include("entity_logic_errors.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-include("api_test_utils.hrl").


%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    create_test/1,
    support_space_test/1,
    list_test/1,
    get_test/1,
    get_eff_users_test/1,
    get_eff_groups_test/1,
    get_spaces_test/1,
    update_test/1,
    delete_test/1
]).

all() ->
    ?ALL([
        create_test,
        support_space_test,
        list_test,
        get_test,
        get_users_test,
        get_eff_groups_test,
        get_spaces_test,
        update_test,
        delete_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    {_, CSRFile, _} = oz_test_utils:generate_provider_cert_files(),
    {ok, CSR} = file:read_file(CSRFile),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/provider">>,
            expected_code = 200,
            expected_body = #{
                <<"providerId">> => {check_type, binary},
                <<"certificate">> => {check_type, binary}
            }
        },
        logic_spec = #logic_spec{
            operation = create,
            module = n_provider_logic,
            function = create,
            args = [client, data],
            expected_result = ?OK_TERM(fun({B1, B2}) ->
                is_binary(B1) andalso is_binary(B2)
            end)
        },
        data_spec = #data_spec{
            required = [<<"name">>, <<"urls">>, <<"redirectionPoint">>, <<"csr">>],
            optional = [<<"latitude">>, <<"longitude">>],
            correct_values = #{
                <<"name">> => <<"ProvName">>,
                <<"urls">> => [<<"127.0.0.1">>],
                <<"redirectionPoint">> => <<"https://127.0.0.1">>,
                <<"csr">> => CSR,
                <<"latitude">> => 50.0,
                <<"longitude">> => -24.8
            },
            bad_values = [
                {<<"name">>, <<"">>, ?ERROR_EMPTY_DATA(<<"name">>)},
                {<<"name">>, 1234, ?ERROR_BAD_DATA(<<"name">>)},
                {<<"urls">>, [], ?ERROR_EMPTY_DATA(<<"urls">>)},
                {<<"urls">>, <<"127.0.0.1">>, ?ERROR_BAD_DATA(<<"urls">>)},
                {<<"urls">>, 1234, ?ERROR_BAD_DATA(<<"urls">>)},
                {<<"redirectionPoint">>, <<"">>, ?ERROR_EMPTY_DATA(<<"redirectionPoint">>)},
                {<<"redirectionPoint">>, 1234, ?ERROR_BAD_DATA(<<"redirectionPoint">>)},
                {<<"csr">>, <<"">>, ?ERROR_EMPTY_DATA(<<"csr">>)},
                {<<"csr">>, 1234, ?ERROR_BAD_DATA(<<"csr">>)},
                {<<"csr">>, <<"wrong-csr">>, ?ERROR_BAD_DATA(<<"csr">>)},
                {<<"latitude">>, -1500, ?ERROR_BAD_DATA(<<"latitude">>)},
                {<<"latitude">>, -90.1, ?ERROR_BAD_DATA(<<"latitude">>)},
                {<<"latitude">>, 90.1, ?ERROR_BAD_DATA(<<"latitude">>)},
                {<<"latitude">>, 1500, ?ERROR_BAD_DATA(<<"latitude">>)},
                {<<"longitude">>, -1500, ?ERROR_BAD_DATA(<<"longitude">>)},
                {<<"longitude">>, -180.1, ?ERROR_BAD_DATA(<<"longitude">>)},
                {<<"longitude">>, 180.1, ?ERROR_BAD_DATA(<<"longitude">>)},
                {<<"longitude">>, 1500, ?ERROR_BAD_DATA(<<"longitude">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


support_space_test(Config) ->
    RestPrefix = rest_test_utils:get_rest_api_prefix(Config),
    MinimumSupportSize = min_support_size(Config),
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(Config, <<"P1">>),
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(Config, <<"P2">>),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), <<"S1">>),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    {ok, BadMacaroon1} = oz_test_utils:space_invite_user_token(Config, ?USER(U1), S1),
    {ok, BadTokenType1} = token_logic:serialize(BadMacaroon1),

    % Reused in all specs
    BadValues = [
        {<<"token">>, <<"bad-token">>, ?ERROR_BAD_TOKEN(<<"token">>)},
        {<<"token">>, 1234, ?ERROR_BAD_TOKEN(<<"token">>)},
        {<<"token">>, BadTokenType1, ?ERROR_BAD_TOKEN_TYPE(<<"token">>)},
        {<<"size">>, <<"binary">>, ?ERROR_BAD_DATA(<<"size">>)},
        {<<"size">>, 0, ?ERROR_BAD_DATA(<<"size">>)},
        {<<"size">>, -1000, ?ERROR_BAD_DATA(<<"size">>)},
        {<<"size">>, MinimumSupportSize - 1, ?ERROR_BAD_DATA(<<"size">>)}
    ],

    % Check only REST first
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            % Both providers are correct clients, as this is an endpoint
            % dedicated for provider that presents a cert
            correct = [
                root,
                {provider, P1, KeyFile1, CertFile1},
                {provider, P2, KeyFile2, CertFile2}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/provider/spaces/support">>,
            expected_code = 201,
            expected_headers = {contains, #{
                <<"location">> => {match, <<RestPrefix/binary, "/provider/spaces/.*">>}
            }}
        },
        data_spec = #data_spec{
            required = [<<"token">>, <<"size">>],
            correct_values = #{
                <<"token">> => fun() ->
                    % Create a new space and token for every test case
                    % (this value is reused multiple times as many cases of
                    % the api test must be checked).
                    {ok, Space} = oz_test_utils:create_space(
                        Config, ?USER(U1), <<"space">>
                    ),
                    oz_test_utils:ensure_eff_graph_up_to_date(Config),
                    {ok, Macaroon} = oz_test_utils:space_invite_provider_token(
                        Config, ?USER(U1), Space
                    ),
                    {ok, TokenBin} = token_logic:serialize(Macaroon),
                    TokenBin
                end,
                <<"size">> => MinimumSupportSize
            },
            bad_values = BadValues
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Check logic endpoints - here we have to specify provider Id, so
    % different clients will be authorized.
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            % Only provider 1 is authorized to perform support operation on
            % behalf of provider 1.
            correct = [root, {provider, P1, KeyFile1, CertFile1}],
            unauthorized = [nobody],
            forbidden = [{user, U1}, {provider, P2, KeyFile2, CertFile2}]
        },
        logic_spec = #logic_spec{
            operation = create,
            module = n_provider_logic,
            function = support_space,
            args = [client, P1, data],
            expected_result = ?OK_TERM(fun(SpaceId) ->
                % Should return space id of the newly supported space
                {ok, #od_space{
                    providers = Providers
                }} = oz_test_utils:get_space(Config, ?ROOT, SpaceId),
                maps:is_key(P1, Providers)
            end)
        },
        rest_spec = undefined
        % data_spec is inherited from ApiTestSpec
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % provider_logic should also allow using non-serialized macaroons, check it
    {ok, BadMacaroon3} = oz_test_utils:space_invite_user_token(Config, ?USER(U1), S1),
    ApiTestSpec3 = ApiTestSpec2#api_test_spec{
        % client_spec and logic_spec are inherited from ApiTestSpec
        data_spec = #data_spec{
            required = [<<"token">>, <<"size">>],
            correct_values = #{
                <<"token">> => fun() ->
                    % Create a new space and token for every test case
                    % (this value is reused multiple times as many cases of
                    % the api test must be checked).
                    {ok, Space} = oz_test_utils:create_space(
                        Config, ?USER(U1), <<"space">>
                    ),
                    oz_test_utils:ensure_eff_graph_up_to_date(Config),
                    {ok, Macaroon} = oz_test_utils:space_invite_provider_token(
                        Config, ?USER(U1), Space
                    ),
                    Macaroon
                end,
                <<"size">> => MinimumSupportSize
            },
            bad_values = BadValues ++ [
                {<<"token">>, BadMacaroon3, ?ERROR_BAD_TOKEN_TYPE(<<"token">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec3)).


list_test(Config) ->
    % Make sure that providers created in other tests are deleted.
    ok = oz_test_utils:delete_all_entities(Config),
    % Register some providers
    {ok, {P1, KeyFile, CertFile}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    {ok, {P2, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P2">>),
    {ok, {P3, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P3">>),
    {ok, {P4, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P4">>),
    {ok, {P5, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P5">>),
    ExpProviders = [P1, P2, P3, P4, P5],
    % Create two users, grant one of them the privilege to list providers.
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    ok = oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        list_providers
    ]),

    oz_test_utils:ensure_eff_graph_up_to_date(Config),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}, {provider, P1, KeyFile, CertFile}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/providers">>,
            expected_code = 200,
            expected_body = #{<<"providers">> => ExpProviders}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = n_provider_logic,
            function = list,
            args = [client],
            expected_result = ?OK_LIST(ExpProviders)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_test(Config) ->
    % Create a provider manually
    {KeyFile1, CSRFile1, CertFile1} = oz_test_utils:generate_provider_cert_files(),
    {ok, CSR} = file:read_file(CSRFile1),
    {ok, {P1, Certificate}} = oz_test_utils:create_provider(Config, #{
        <<"name">> => <<"Provider 1">>,
        <<"urls">> => [<<"172.16.0.10">>, <<"172.16.0.11">>],
        <<"redirectionPoint">> => <<"https://hostname.com">>,
        <<"csr">> => CSR,
        <<"latitude">> => 14.78,
        <<"longitude">> => -106.12
    }),
    ok = file:write_file(CertFile1, Certificate),
    % Create a second provider
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P2">>
    ),
    % Create two users, grant one of them the privilege to list providers.
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    ok = oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        list_providers
    ]),

    oz_test_utils:ensure_eff_graph_up_to_date(Config),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {provider, P1, KeyFile1, CertFile1},
                {provider, P2, KeyFile2, CertFile2}
            ],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, P1],
            expected_code = 200,
            expected_body = #{
                <<"providerId">> => P1,
                <<"name">> => <<"Provider 1">>,
                <<"urls">> => [<<"172.16.0.10">>, <<"172.16.0.11">>],
                <<"redirectionPoint">> => <<"https://hostname.com">>,
                <<"latitude">> => 14.78,
                <<"longitude">> => -106.12
            }
        },
        logic_spec = #logic_spec{
            operation = get,
            module = n_provider_logic,
            function = get,
            args = [client, P1],
            expected_result = ?OK_TERM(fun(Entity) ->
                #od_provider{name = Name, urls = Urls,
                    redirection_point = RedPoint, latitude = Latitude,
                    longitude = Longitude, spaces = Spaces} = Entity,
                Name =:= <<"Provider 1">> andalso
                    Urls =:= [<<"172.16.0.10">>, <<"172.16.0.11">>] andalso
                    RedPoint =:= <<"https://hostname.com">> andalso
                    Latitude =:= 14.78 andalso
                    Longitude =:= -106.12 andalso
                    Spaces =:= []
            end)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    % Provider should be also able to retrieve this info using another path,
    % without id (id is deduced from authorization)
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, P1, KeyFile1, CertFile1}],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/provider/">>,
            expected_code = 200,
            expected_body = #{
                <<"providerId">> => P1,
                <<"name">> => <<"Provider 1">>,
                <<"urls">> => [<<"172.16.0.10">>, <<"172.16.0.11">>],
                <<"redirectionPoint">> => <<"https://hostname.com">>,
                <<"latitude">> => 14.78,
                <<"longitude">> => -106.12
            }
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


get_eff_users_test(Config) ->
    {ok, {P1, KeyFile, CertFile}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),

    % Create two users, grant one of them the privilege to list users.
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    ok = oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        list_users_of_provider
    ]),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),

    % There are no users yet
    ApiTestSpec = #api_test_spec{
        client_spec = ClientSpec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [{provider, P1, KeyFile, CertFile}, {user, NonAdmin}]
        },
        rest_spec = RestSpec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, P1, <<"/users">>],
            expected_code = 200,
            expected_body = #{<<"users">> => []}
        },
        logic_spec = LogicSpec = #logic_spec{
            operation = get,
            module = n_provider_logic,
            function = get_eff_users,
            args = [client, P1],
            expected_result = ?OK_LIST([])
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Create some spaces and support them
    {{S1, _}, {S2, _}, {S3, _}} = create_and_support_3_spaces(Config, P1),
    % Add some users to the spaces (9 users, 3 in each space)
    ExpUsers = lists:map(
        fun({Counter, Space}) ->
            UserName = str_utils:format_bin("u~B", [Counter]),
            {ok, User} = oz_test_utils:create_user(
                Config, #od_user{name = UserName}
            ),
            {ok, User} = oz_test_utils:add_user_to_space(
                Config, ?ROOT, Space, User
            ),
            {User, UserName}
        end, lists:zip(lists:seq(1, 9), [S1, S2, S3, S1, S2, S3, S1, S2, S3])),
    {ExpUserIds, _} = lists:unzip(ExpUsers),
    % Ensure user list is correct.
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = ClientSpec#client_spec{
            forbidden = [
                {user, lists:nth(1, ExpUserIds)},
                {user, NonAdmin},
                {provider, P1, KeyFile, CertFile}
            ]
        },
        rest_spec = RestSpec#rest_spec{
            expected_body = #{<<"users">> => ExpUserIds}
        },
        logic_spec = LogicSpec#logic_spec{
            expected_result = ?OK_LIST(ExpUserIds)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % Create some more spaces and support them
    {{S4, _}, {S5, _}, {S6, _}} = create_and_support_3_spaces(Config, P1),
    % Create two groups for every space and three users for every group
    ExpGroupUsers = lists:flatmap(
        fun({Counter, Space}) ->
            UserName1 = str_utils:format_bin("u1@g~B", [Counter]),
            {ok, User1} = oz_test_utils:create_user(
                Config, #od_user{name = UserName1}
            ),
            UserName2 = str_utils:format_bin("u2@g~B", [Counter]),
            {ok, User2} = oz_test_utils:create_user(
                Config, #od_user{name = UserName2}
            ),
            GroupName = str_utils:format_bin("g~B", [Counter]),
            {ok, Group} = oz_test_utils:create_group(
                Config, ?USER(User1), GroupName
            ),
            {ok, User2} = oz_test_utils:add_user_to_group(
                Config, ?ROOT, Group, User2
            ),
            {ok, Group} = oz_test_utils:add_group_to_space(
                Config, ?ROOT, Space, Group
            ),
            [{User1, UserName1}, {User2, UserName2}]
        end, lists:zip(lists:seq(1, 6), [S4, S5, S6, S4, S5, S6])),
    {ExpGroupUserIds, _} = lists:unzip(ExpGroupUsers),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    % Ensure user list is correct.
    ApiTestSpec3 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            forbidden = [
                {user, lists:nth(1, ExpGroupUserIds)},
                {user, NonAdmin},
                {provider, P1, KeyFile, CertFile}
            ]
        },
        rest_spec = RestSpec#rest_spec{
            expected_body = #{<<"users">> => ExpUserIds ++ ExpGroupUserIds}
        },
        logic_spec = LogicSpec#logic_spec{
            expected_result = ?OK_LIST(ExpUserIds ++ ExpGroupUserIds)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec3)),

    % Check every user one by one.
    lists:foreach(
        fun(UserId, UserName) ->
            ApiTestSpec4 = #api_test_spec{
                client_spec = #client_spec{
                    correct = [root, {user, Admin}],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, lists:nth(1, ExpUserIds)},
                        {user, lists:nth(1, ExpGroupUserIds)},
                        {user, NonAdmin},
                        {provider, P1, KeyFile, CertFile}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/providers/">>, P1, <<"/users/">>, UserId],
                    expected_code = 200,
                    expected_body = {contains, #{
                        <<"userId">> => UserId,
                        <<"name">> => UserName
                    }}
                },
                logic_spec = #logic_spec{
                    operation = get,
                    module = n_provider_logic,
                    function = get_eff_user,
                    args = [client, P1, UserId],
                    expected_result = ?OK_TERM(fun(#od_user{name = Name}) ->
                        Name =:= UserName
                    end)
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec4))
        end, ExpUserIds ++ ExpGroupUserIds),

    % Make sure that other users cannot be reached this way.
    {ok, OtherUser} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ApiTestSpec5 = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [
                {user, lists:nth(1, ExpUserIds)},
                {user, lists:nth(1, ExpGroupUserIds)},
                {user, NonAdmin},
                {provider, P1, KeyFile, CertFile}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, P1, <<"/users/">>, OtherUser],
            expected_code = 404
        },
        logic_spec = #logic_spec{
            operation = get,
            module = n_provider_logic,
            function = get_eff_user,
            args = [client, P1, OtherUser],
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec5)).


get_eff_groups_test(Config) ->
    {ok, {P1, KeyFile, CertFile}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    % Create two users, grant one of them the privilege to list groups.
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    ok = oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        list_groups_of_provider
    ]),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),

    % There are no groups yet
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [{provider, P1, KeyFile, CertFile}, {user, NonAdmin}]
        },
        rest_spec = RestSpec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, P1, <<"/groups">>],
            expected_code = 200,
            expected_body = #{<<"groups">> => []}
        },
        logic_spec = LogicSpec = #logic_spec{
            operation = get,
            module = n_provider_logic,
            function = get_eff_groups,
            args = [client, P1],
            expected_result = ?OK_LIST([])
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Create some spaces and support them
    {{S1, _}, {S2, _}, {S3, _}} = create_and_support_3_spaces(Config, P1),
    {{S4, _}, {S5, _}, {S6, _}} = create_and_support_3_spaces(Config, P1),
    % Create two groups for every space
    ExpGroups = lists:map(
        fun({Counter, Space}) ->
            GroupName = str_utils:format_bin("g~B", [Counter]),
            {ok, Group} = oz_test_utils:create_group(
                Config, ?ROOT, GroupName
            ),
            {ok, Group} = oz_test_utils:add_group_to_space(
                Config, ?ROOT, Space, Group
            ),
            {Group, GroupName}
        end, lists:zip(lists:seq(1, 6), [S1, S2, S3, S4, S5, S6])),
    ExpGroupIds = lists:unzip(ExpGroups),

    % Check if correct groups are returned
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ApiTestSpec2 = #api_test_spec{
        rest_spec = RestSpec#rest_spec{
            expected_body = #{<<"groups">> => ExpGroupIds}
        },
        logic_spec = LogicSpec#logic_spec{
            expected_result = ?OK_LIST(ExpGroupIds)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),

    % Check every group one by one.
    lists:foreach(
        fun(GroupId, GroupName) ->
            ApiTestSpec3 = #api_test_spec{
                client_spec = #client_spec{
                    correct = [root, {user, Admin}],
                    unauthorized = [nobody],
                    forbidden = [
                        {provider, P1, KeyFile, CertFile},
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/providers/">>, P1, <<"/groups/">>, GroupId],
                    expected_code = 200,
                    expected_body = {contains, #{
                        <<"groupId">> => GroupId,
                        <<"name">> => GroupName
                    }}
                },
                logic_spec = #logic_spec{
                    operation = get,
                    module = n_provider_logic,
                    function = get_eff_group,
                    args = [client, P1, GroupId],
                    expected_result = ?OK_TERM(fun(#od_group{name = Name}) ->
                        Name =:= GroupName
                    end)
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec3))
        end, ExpGroups),

    % Make sure that other groups cannot be reached this way.
    {ok, OtherGroup} = oz_test_utils:create_group(Config, ?ROOT, <<"other">>),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ApiTestSpec4 = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [
                {provider, P1, KeyFile, CertFile},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, P1, <<"/groups/">>, OtherGroup],
            expected_code = 404
        },
        logic_spec = #logic_spec{
            operation = get,
            module = n_provider_logic,
            function = get_eff_group,
            args = [client, P1, OtherGroup],
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec4)).


get_spaces_test(Config) ->
    {ok, {P1, KeyFile, CertFile}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    % Create two users, grant one of them the privilege to list spaces.
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    ok = oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        list_spaces_of_provider
    ]),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),

    % There are no spaces yet
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {user, Admin}, {provider, P1, KeyFile, CertFile}],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}]
        },
        rest_spec = RestSpec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, P1, <<"/spaces">>],
            expected_code = 200,
            expected_body = #{<<"spaces">> => []}
        },
        logic_spec = LogicSpec = #logic_spec{
            operation = get,
            module = n_provider_logic,
            function = get_eff_spaces,
            args = [client, P1],
            expected_result = ?OK_LIST([])
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    % Also check the endpoint dedicated for provider
    ApiTestSpecForProvider = #api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, P1, KeyFile, CertFile}]
        },
        rest_spec = RestSpecForProvider = #rest_spec{
            method = get,
            path = <<"/provider/spaces">>,
            expected_code = 200,
            expected_body = #{<<"spaces">> => []}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpecForProvider)),

    % Create some spaces
    ExpSpaces = create_and_support_3_spaces(Config, P1),
    {ExpSpaceIds, _} = lists:unzip(ExpSpaces),
    % Get spaces

    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    % Check if correct spaces are returned
    ApiTestSpec2 = #api_test_spec{
        rest_spec = RestSpec#rest_spec{
            expected_body = #{<<"spaces">> => ExpSpaceIds}
        },
        logic_spec = LogicSpec#logic_spec{
            expected_result = ?OK_LIST(ExpSpaceIds)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),
    % Also check the endpoint dedicated for provider
    ApiTestSpecForProvider2 = ApiTestSpecForProvider#api_test_spec{
        rest_spec = RestSpecForProvider#rest_spec{
            expected_body = #{<<"spaces">> => ExpSpaceIds}
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpecForProvider2)),

    % Check every space one by one.
    lists:foreach(
        fun(SpaceId, SpaceName) ->
            ApiTestSpec2 = #api_test_spec{
                client_spec = #client_spec{
                    correct = [root, {user, Admin}],
                    unauthorized = [nobody],
                    forbidden = [
                        {provider, P1, KeyFile, CertFile},
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [<<"/providers/">>, P1, <<"/spaces/">>, SpaceId],
                    expected_code = 200,
                    expected_body = {contains, #{
                        <<"spaceId">> => SpaceId,
                        <<"name">> => SpaceName
                    }}
                },
                logic_spec = #logic_spec{
                    operation = get,
                    module = n_provider_logic,
                    function = get_eff_space,
                    args = [client, P1, SpaceId],
                    expected_result = ?OK_TERM(fun(#od_space{name = Name}) ->
                        Name =:= SpaceName
                    end)
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)),
            % Also check the endpoint dedicated for provider
            ApiTestSpecForProvider3 = #api_test_spec{
                client_spec = #client_spec{
                    correct = [{provider, P1, KeyFile, CertFile}]
                },
                rest_spec = RestSpecForProvider = #rest_spec{
                    method = get,
                    path = [<<"/provider/spaces/">>, SpaceId],
                    expected_code = 200,
                    expected_body = {contains, #{
                        <<"spaceId">> => SpaceId,
                        <<"name">> => SpaceName
                    }}
                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpecForProvider3))
        end, ExpSpaces),

    % Make sure that other spaces cannot be reached this way.
    {ok, OtherSpace} = oz_test_utils:create_space(Config, ?ROOT, <<"other">>),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ApiTestSpec4 = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [
                {provider, P1, KeyFile, CertFile},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, P1, <<"/spaces/">>, OtherSpace],
            expected_code = 404
        },
        logic_spec = #logic_spec{
            operation = get,
            module = n_provider_logic,
            function = get_eff_space,
            args = [client, P1, OtherSpace],
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec4)),
    % Also check the endpoint dedicated for provider
    ApiTestSpecForProvider4 = #api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, P1, KeyFile, CertFile}]
        },
        rest_spec = RestSpecForProvider = #rest_spec{
            method = get,
            path = [<<"/provider/spaces/">>, OtherSpace],
            expected_code = 404
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpecForProvider4)).


update_test(Config) ->
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {provider, P1, KeyFile1, CertFile1}]
            % No need to check other clients - this endpoint is dedicated to the
            % provider that presents it certs.
            % root client is only checked in logic tests
        },
        rest_spec = #rest_spec{
            method = patch,
            path = <<"/provider">>,
            expected_code = 204
        },
        logic_spec = #logic_spec{
            operation = update,
            module = n_provider_logic,
            function = update,
            args = [client, P1, data],
            expected_result = ?OK
        },
        data_spec = #data_spec{
            required = [<<"name">>, <<"urls">>, <<"redirectionPoint">>],
            optional = [<<"latitude">>, <<"longitude">>],
            correct_values = #{
                <<"name">> => <<"New Prov name">>,
                <<"urls">> => [<<"new.url">>],
                <<"redirectionPoint">> => <<"https://new.url">>,
                <<"latitude">> => -12.87645,
                <<"longitude">> => -4.44
            },
            bad_values = [
                {<<"name">>, <<"">>, ?ERROR_EMPTY_DATA(<<"name">>)},
                {<<"name">>, 1234, ?ERROR_BAD_DATA(<<"name">>)},
                {<<"urls">>, [], ?ERROR_EMPTY_DATA(<<"urls">>)},
                {<<"urls">>, <<"127.0.0.1">>, ?ERROR_BAD_DATA(<<"urls">>)},
                {<<"urls">>, 1234, ?ERROR_BAD_DATA(<<"urls">>)},
                {<<"redirectionPoint">>, <<"">>, ?ERROR_EMPTY_DATA(<<"redirectionPoint">>)},
                {<<"redirectionPoint">>, 1234, ?ERROR_BAD_DATA(<<"redirectionPoint">>)},
                {<<"latitude">>, -1500, ?ERROR_BAD_DATA(<<"latitude">>)},
                {<<"latitude">>, -90.1, ?ERROR_BAD_DATA(<<"latitude">>)},
                {<<"latitude">>, 90.1, ?ERROR_BAD_DATA(<<"latitude">>)},
                {<<"latitude">>, 1500, ?ERROR_BAD_DATA(<<"latitude">>)},
                {<<"longitude">>, -1500, ?ERROR_BAD_DATA(<<"longitude">>)},
                {<<"longitude">>, -180.1, ?ERROR_BAD_DATA(<<"longitude">>)},
                {<<"longitude">>, 180.1, ?ERROR_BAD_DATA(<<"longitude">>)},
                {<<"longitude">>, 1500, ?ERROR_BAD_DATA(<<"longitude">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


delete_test(Config) ->
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, {provider, P1, KeyFile1, CertFile1}]
            % No need to check other clients - this endpoint is dedicated to the
            % provider that presents it certs.
            % root client is only checked in logic tests
        },
        rest_spec = #rest_spec{
            method = delete,
            path = <<"/provider">>,
            expected_code = 202
        },
        logic_spec = #logic_spec{
            operation = delete,
            module = n_provider_logic,
            function = delete,
            args = [client, P1],
            expected_result = ?OK
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    % There is also an admin endpoint for deleting providers, provider 1
    % should also be able to use it, but other providers should not.
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    % Create two users, grant one of them the privilege to remove providers.
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    ok = oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        remove_provider
    ]),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {provider, P1, KeyFile1, CertFile1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {provider, P2, KeyFile2, CertFile2}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/providers/">>, P1],
            expected_code = 202
        },
        logic_spec = #logic_spec{
            operation = delete,
            module = n_provider_logic,
            function = delete,
            args = [client, P1],
            expected_result = ?OK
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2)).


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


%%%===================================================================
%%% Helper functions
%%%===================================================================

min_support_size(Config) ->
    {ok, MinimumSupportSize} = oz_test_utils:call_oz(
        Config, application, get_env, [oz_worker, minimum_space_support_size]
    ),
    MinimumSupportSize.


create_and_support_3_spaces(Config, ProvId) ->
    MinimumSupportSize = min_support_size(Config),
    S1Name = <<"Space 1">>,
    S2Name = <<"Space 2">>,
    S3Name = <<"Space 3">>,
    {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, S1Name),
    {ok, S2} = oz_test_utils:create_space(Config, ?ROOT, S2Name),
    {ok, S3} = oz_test_utils:create_space(Config, ?ROOT, S3Name),
    % Support them by the provider
    {ok, Macaroon1} = oz_test_utils:space_invite_provider_token(Config, ?ROOT, S1),
    {ok, Macaroon2} = oz_test_utils:space_invite_provider_token(Config, ?ROOT, S2),
    {ok, Macaroon3} = oz_test_utils:space_invite_provider_token(Config, ?ROOT, S3),
    {ok, S1} = oz_test_utils:support_space(
        Config, ?PROVIDER(ProvId), ProvId, Macaroon1, MinimumSupportSize
    ),
    {ok, S2} = oz_test_utils:support_space(
        Config, ?PROVIDER(ProvId), ProvId, Macaroon2, MinimumSupportSize
    ),
    {ok, S3} = oz_test_utils:support_space(
        Config, ?PROVIDER(ProvId), ProvId, Macaroon3, MinimumSupportSize
    ),
    [
        {S1, S1Name},
        {S2, S2Name},
        {S3, S3Name}
    ].
