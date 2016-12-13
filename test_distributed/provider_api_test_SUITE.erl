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

-record(api_test_spec, {
    method = get :: get | patch | post | put | delete,
    client_spec = undefined :: undefined | #client_spec{},
    rest_spec = undefined :: undefined | #rest_spec{},
    logic_spec = undefined :: undefined | #logic_spec{},
    data_spec = undefined :: undefined | #data_spec{}
}).

-record(client_spec, {
    correct = [],
    unauthorized = [],
    forbidden = []
}).

-record(data_spec, {
    required = [],
    optional = [],
    at_least_one = [],
    correct_values = #{binary => binary},
    bad_values = [{key, value, error_type}]
}).

-record(rest_spec, {
    path = <<"">> :: binary() | [binary()],
    expected_code = undefined,
    expected_headers = undefined :: #{} | {contains, #{}},
    expected_body = undefined :: #{} | {contains, #{}}
}).

-record(logic_spec, {
    module = n_provider_logic,
    entity_id = undefined,
    resource = undefined,
    expected_result = undefined
}).

-define(OK_BINARY, fun(Result) ->
    case Result of
        {ok, Bin} when is_binary(Bin) -> true;
        _ -> false
    end
end).

-define(OK_BINARY(__ExactValue), fun(Result) ->
    case Result of
        {ok, __ExactValue} -> true;
        _ -> false
    end
end).

-define(OK_ENTITY(__VerifyFun), fun(Result) ->
    case Result of
        {ok, Entity} -> __VerifyFun(Entity);
        _ -> false
    end
end).

-define(OK_LIST(__ExpectedList), fun(Result) ->
    lists:sort(__ExpectedList) =:= lists:sort(Result)
end).

-define(ERROR_REASON(__ExpectedError), fun(Result) ->
    __ExpectedError =:= Result
end).


%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    create_test/1,
    support_space_test/1,
    list_test/1,
    get_test/1,
    get_eff_users_test/1,
    get_eff_groups_test/1,
    get_spaces_test/1
]).

all() ->
    ?ALL([
        create_test,
        support_space_test,
        list_test,
        get_test,
        get_users_test,
        get_eff_groups_test,
        get_spaces_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    RestPrefix = rest_test_utils:get_rest_api_prefix(Config),
    {_, CSRFile, _} = oz_test_utils:generate_provider_cert_files(),
    {ok, CSR} = file:read_file(CSRFile),
    ApiTestSpec = #api_test_spec{
        method = create,
        client_spec = #client_spec{
            correct = [root, nobody]
        },
        rest_spec = #rest_spec{
            path = <<"/providers">>,
            expected_code = 201,
            expected_headers = #{
                <<"location">> => {match, <<RestPrefix/binary, "/provider/.*">>}
            }
        },
        logic_spec = #logic_spec{
            module = n_provider_logic,
            entity_id = undefined,
            resource = entity,
            expected_result = ?OK_BINARY
        },
        data_spec = #data_spec{
            required = [<<"name">>, <<"urls">>, <<"redirectionPoint">>, <<"csr">>],
            optional = [<<"longitude">>, <<"longitude">>],
            correct_values = #{
                <<"name">> => <<"ProvName">>,
                <<"urls">> => [<<"127.0.0.1">>],
                <<"redirectionPoint">> => <<"https://127.0.0.1">>,
                <<"csr">> => CSR,
                <<"latitude">> => 50.0,
                <<"longitude">> => -24.8
            },
            bad_values = [
                {<<"name">>, <<"">>, empty},
                {<<"name">>, 1234, bad},
                {<<"urls">>, [], empty},
                {<<"urls">>, <<"127.0.0.1">>, bad},
                {<<"urls">>, 1234, bad},
                {<<"redirectionPoint">>, <<"">>, empty},
                {<<"redirectionPoint">>, 1234, bad},
                {<<"csr">>, <<"">>, empty},
                {<<"csr">>, 1234, bad},
                {<<"csr">>, <<"wrong-csr">>, bad},
                {<<"latitude">>, -1500, bad},
                {<<"latitude">>, -90.1, bad},
                {<<"latitude">>, 90.1, bad},
                {<<"latitude">>, 1500, bad},
                {<<"longitude">>, -1500, bad},
                {<<"longitude">>, -180.1, bad},
                {<<"longitude">>, 180.1, bad},
                {<<"longitude">>, 1500, bad}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(ApiTestSpec)).


support_space_test(Config) ->
    RestPrefix = rest_test_utils:get_rest_api_prefix(Config),
    MinimumSupportSize = min_support_size(Config),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), <<"S1">>),
    {ok, {P1, KeyFile1, CertFile1}} = oz_test_utils:create_provider_and_certs(Config, <<"P1">>),
    {ok, {P2, KeyFile2, CertFile2}} = oz_test_utils:create_provider_and_certs(Config, <<"P2">>),
    {ok, Macaroon1} = oz_test_utils:space_invite_provider_token(Config, ?USER(U1), S1),
    {ok, TokenBin} = token_logic:serialize(Macaroon1),
    {ok, BadMacaroon1} = oz_test_utils:space_invite_user_token(Config, ?USER(U1), S1),
    {ok, BadTokenType} = token_logic:serialize(BadMacaroon1),

    ApiTestSpec = #api_test_spec{
        method = create,
        client_spec = #client_spec{
            correct = [root, {provider, P1, KeyFile1, CertFile1}],
            unauthorized = [nobody],
            forbidden = [{user, U1}, {provider, P2, KeyFile2, CertFile2}]
        },
        rest_spec = #rest_spec{
            path = <<"/provider/spaces/support">>,
            expected_code = 201,
            expected_headers = #{
                <<"location">> => {match, <<RestPrefix/binary, "/provider/spaces/.*">>}
            }
        },
        logic_spec = #logic_spec{
            module = n_provider_logic,
            entity_id = P1,
            resource = support,
            expected_result = ?OK_BINARY(S1)
        },
        data_spec = #data_spec{
            required = [<<"token">>, <<"size">>],
            correct_values = #{
                <<"token">> => TokenBin,
                <<"size">> => MinimumSupportSize
            },
            bad_values = [
                {<<"token">>, <<"bad-token">>, bad_token},
                {<<"token">>, 1234, bad_token},
                {<<"token">>, BadTokenType, bad_token_type},
                {<<"size">>, <<"binary">>, bad},
                {<<"size">>, 0, bad},
                {<<"size">>, -1000, bad},
                {<<"size">>, MinimumSupportSize - 1, bad}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(ApiTestSpec)),

    % provider_logic should also allow using non-serialized macaroons, check it
    {ok, Macaroon2} = oz_test_utils:space_invite_provider_token(Config, ?USER(U1), S1),
    {ok, BadMacaroon2} = oz_test_utils:space_invite_user_token(Config, ?USER(U1), S1),
    ApiTestSpec2 = #api_test_spec{
        method = create,
        client_spec = #client_spec{
            correct = [root, {provider, P1, KeyFile1, CertFile1}],
            unauthorized = [nobody],
            forbidden = [{user, U1}, {provider, P2, KeyFile2, CertFile2}]
        },
        logic_spec = #logic_spec{
            module = n_provider_logic,
            entity_id = P1,
            resource = support,
            expected_result = ?OK_BINARY(S1)
        },
        data_spec = #data_spec{
            required = [<<"token">>, <<"size">>],
            correct_values = #{
                <<"token">> => Macaroon2,
                <<"size">> => MinimumSupportSize
            },
            bad_values = [
                {<<"token">>, <<"asd">>, bad_token},
                {<<"token">>, 1234, bad_token},
                {<<"token">>, BadMacaroon2, bad_token_type},
                {<<"size">>, <<"binary">>, bad},
                {<<"size">>, 0, bad},
                {<<"size">>, -1000, bad},
                {<<"size">>, MinimumSupportSize - 1, bad}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(ApiTestSpec2)).


list_test(Config) ->
    % Register some providers
    {ok, {P1, KeyFile, CertFile}} = oz_test_utils:create_provider_and_certs(
        Config, <<"P1">>
    ),
    {ok, {P2, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P2">>),
    {ok, {P3, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P3">>),
    {ok, {P4, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P4">>),
    {ok, {P5, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"P5">>),
    % Create two users, grant one of them the privilege to list providers.
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    ok = oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        list_providers]
    ),

    oz_test_utils:ensure_eff_graph_up_to_date(Config),

    ApiTestSpec = #api_test_spec{
        method = get,
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}, {provider, P1, KeyFile, CertFile}]
        },
        rest_spec = #rest_spec{
            path = <<"/providers">>,
            expected_code = 200,
            expected_body = #{<<"providers">> => [P1, P2, P3, P4, P5]}
        },
        logic_spec = #logic_spec{
            module = n_provider_logic,
            entity_id = undefined,
            resource = list,
            expected_result = ?OK_BINARY
        }
    },
    ?assert(api_test_utils:run_tests(ApiTestSpec)).


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
        method = get,
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
            module = n_provider_logic,
            entity_id = P1,
            resource = entity,
            expected_result = ?OK_ENTITY(fun(Entity) ->
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
    ?assert(api_test_utils:run_tests(ApiTestSpec)),
    % Provider should be also able to retrieve this info using another path,
    % without id (id is deduced from authorization)
    ApiTestSpec2 = #api_test_spec{
        method = get,
        client_spec = #client_spec{
            correct = [root, {provider, P1, KeyFile1, CertFile1}],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}]
        },
        rest_spec = #rest_spec{
            path = [<<"/provider/">>, P1],
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
    ?assert(api_test_utils:run_tests(ApiTestSpec2)).


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
        method = get,
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [{provider, P1, KeyFile, CertFile}, {user, NonAdmin}]
        },
        rest_spec = #rest_spec{
            path = [<<"/providers/">>, P1, <<"/users">>],
            expected_code = 200,
            expected_body = #{<<"users">> => []}
        },
        logic_spec = #logic_spec{
            module = n_provider_logic,
            entity_id = P1,
            resource = users,
            expected_result = ?OK_LIST([])
        }
    },
    ?assert(api_test_utils:run_tests(ApiTestSpec)),

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

    ApiTestSpec2 = #api_test_spec{
        method = get,
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [
                {user, lists:nth(1, ExpUserIds)},
                {user, NonAdmin},
                {provider, P1, KeyFile, CertFile}
            ]
        },
        rest_spec = #rest_spec{
            path = [<<"/providers/">>, P1, <<"/users">>],
            expected_code = 200,
            expected_body = #{<<"users">> => ExpUserIds}
        },
        logic_spec = #logic_spec{
            module = n_provider_logic,
            entity_id = P1,
            resource = users,
            expected_result = ?OK_LIST(ExpUserIds)
        }
    },
    ?assert(api_test_utils:run_tests(ApiTestSpec2)),

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
    ApiTestSpec3 = #api_test_spec{
        method = get,
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [
                {user, lists:nth(1, ExpGroupUserIds)},
                {user, NonAdmin},
                {provider, P1, KeyFile, CertFile}
            ]
        },
        rest_spec = #rest_spec{
            path = [<<"/providers/">>, P1, <<"/users">>],
            expected_code = 200,
            expected_body = #{<<"users">> => ExpUserIds ++ ExpGroupUserIds}
        },
        logic_spec = #logic_spec{
            module = n_provider_logic,
            entity_id = P1,
            resource = users,
            expected_result = ?OK_LIST(ExpUserIds ++ ExpGroupUserIds)
        }
    },
    ?assert(api_test_utils:run_tests(ApiTestSpec3)),

    % Check every user one by one.
    lists:foreach(
        fun(UserId, UserName) ->
            ApiTestSpec4 = #api_test_spec{
                method = get,
                client_spec = #client_spec{
                    correct = [root, {user, Admin}],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, lists:nth(1, ExpUserIds)},
                        {user, NonAdmin},
                        {provider, P1, KeyFile, CertFile}
                    ]
                },
                rest_spec = #rest_spec{
                    path = [<<"/providers/">>, P1, <<"/users/">>, UserId],
                    expected_code = 200,
                    expected_body = {contains, #{
                        <<"userId">> => UserId,
                        <<"name">> => UserName
                    }}
                },
                logic_spec = #logic_spec{
                    module = n_provider_logic,
                    entity_id = P1,
                    resource = {user, UserId},
                    expected_result = ?OK_ENTITY(fun(#od_user{name = Name}) ->
                        Name =:= UserName
                    end)
                }
            },
            ?assert(api_test_utils:run_tests(ApiTestSpec4))
        end, ExpUserIds ++ ExpGroupUserIds),

    % Make sure that other users cannot be reached this way.
    {ok, OtherUser} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ApiTestSpec5 = #api_test_spec{
        method = get,
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [
                {user, lists:nth(1, ExpUserIds)},
                {user, NonAdmin},
                {provider, P1, KeyFile, CertFile}
            ]
        },
        rest_spec = #rest_spec{
            path = [<<"/providers/">>, P1, <<"/users/">>, OtherUser],
            expected_code = 404
        },
        logic_spec = #logic_spec{
            module = n_provider_logic,
            entity_id = P1,
            resource = {user, OtherUser},
            expected_result = ?ERROR_REASON(?EL_NOT_FOUND)
        }
    },
    ?assert(api_test_utils:run_tests(ApiTestSpec5)).


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
        method = get,
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [{provider, P1, KeyFile, CertFile}, {user, NonAdmin}]
        },
        rest_spec = #rest_spec{
            path = [<<"/providers/">>, P1, <<"/groups">>],
            expected_code = 200,
            expected_body = #{<<"groups">> => []}
        },
        logic_spec = #logic_spec{
            module = n_provider_logic,
            entity_id = P1,
            resource = groups,
            expected_result = ?OK_LIST([])
        }
    },
    ?assert(api_test_utils:run_tests(ApiTestSpec)),

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
    ApiTestSpec = #api_test_spec{
        method = get,
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [
                {provider, P1, KeyFile, CertFile},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            path = [<<"/providers/">>, P1, <<"/groups">>],
            expected_code = 200,
            expected_body = #{<<"groups">> => ExpGroupIds}
        },
        logic_spec = #logic_spec{
            module = n_provider_logic,
            entity_id = P1,
            resource = groups,
            expected_result = ?OK_LIST(ExpGroupIds)
        }
    },
    ?assert(api_test_utils:run_tests(ApiTestSpec)),

    % Check every group one by one.
    lists:foreach(
        fun(GroupId, GroupName) ->
            ApiTestSpec2 = #api_test_spec{
                method = get,
                client_spec = #client_spec{
                    correct = [root, {user, Admin}],
                    unauthorized = [nobody],
                    forbidden = [
                        {provider, P1, KeyFile, CertFile},
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    path = [<<"/providers/">>, P1, <<"/groups/">>, GroupId],
                    expected_code = 200,
                    expected_body = {contains, #{
                        <<"groupId">> => GroupId,
                        <<"name">> => GroupName
                    }}
                },
                logic_spec = #logic_spec{
                    module = n_provider_logic,
                    entity_id = P1,
                    resource = {group, GroupId},
                    expected_result = ?OK_ENTITY(fun(#od_group{name = Name}) ->
                        Name =:= GroupName
                    end)
                }
            },
            ?assert(api_test_utils:run_tests(ApiTestSpec2))
        end, ExpGroups),

    % Make sure that other groups cannot be reached this way.
    {ok, OtherGroup} = oz_test_utils:create_group(Config, ?ROOT, <<"other">>),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ApiTestSpec3 = #api_test_spec{
        method = get,
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [
                {provider, P1, KeyFile, CertFile},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            path = [<<"/providers/">>, P1, <<"/groups/">>, OtherGroup],
            expected_code = 404
        },
        logic_spec = #logic_spec{
            module = n_provider_logic,
            entity_id = P1,
            resource = {group, OtherGroup},
            expected_result = ?ERROR_REASON(?EL_NOT_FOUND)
        }
    },
    ?assert(api_test_utils:run_tests(ApiTestSpec3)).


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
        method = get,
        client_spec = #client_spec{
            correct = [root, {user, Admin}, {provider, P1, KeyFile, CertFile}],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}]
        },
        rest_spec = #rest_spec{
            path = [<<"/providers/">>, P1, <<"/spaces">>],
            expected_code = 200,
            expected_body = #{<<"spaces">> => []}
        },
        logic_spec = #logic_spec{
            module = n_provider_logic,
            entity_id = P1,
            resource = spaces,
            expected_result = ?OK_LIST([])
        }
    },
    ?assert(api_test_utils:run_tests(ApiTestSpec)),

    % Create some spaces
    ExpSpaces = create_and_support_3_spaces(Config, P1),
    {ExpSpaceIds, _} = lists:unzip(ExpSpaces),
    % Get spaces

    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    % Check if correct spaces are returned
    ApiTestSpec2 = #api_test_spec{
        method = get,
        client_spec = #client_spec{
            correct = [root, {user, Admin}, {provider, P1, KeyFile, CertFile}],
            unauthorized = [nobody],
            forbidden = [{user, NonAdmin}]
        },
        rest_spec = #rest_spec{
            path = [<<"/providers/">>, P1, <<"/spaces">>],
            expected_code = 200,
            expected_body = #{<<"spaces">> => ExpSpaceIds}
        },
        logic_spec = #logic_spec{
            module = n_provider_logic,
            entity_id = P1,
            resource = spaces,
            expected_result = ?OK_LIST(ExpSpaceIds)
        }
    },
    ?assert(api_test_utils:run_tests(ApiTestSpec2)),

    % Check every space one by one.
    lists:foreach(
        fun(SpaceId, SpaceName) ->
            ApiTestSpec2 = #api_test_spec{
                method = get,
                client_spec = #client_spec{
                    correct = [root, {user, Admin}],
                    unauthorized = [nobody],
                    forbidden = [
                        {provider, P1, KeyFile, CertFile},
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    path = [<<"/providers/">>, P1, <<"/spaces/">>, SpaceId],
                    expected_code = 200,
                    expected_body = {contains, #{
                        <<"spaceId">> => SpaceId,
                        <<"name">> => SpaceName
                    }}
                },
                logic_spec = #logic_spec{
                    module = n_provider_logic,
                    entity_id = P1,
                    resource = {space, SpaceId},
                    expected_result = ?OK_ENTITY(fun(#od_space{name = Name}) ->
                        Name =:= SpaceName
                    end)
                }
            },
            ?assert(api_test_utils:run_tests(ApiTestSpec2))
        end, ExpSpaces),

    % Make sure that other spaces cannot be reached this way.
    {ok, OtherSpace} = oz_test_utils:create_space(Config, ?ROOT, <<"other">>),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),
    ApiTestSpec4 = #api_test_spec{
        method = get,
        client_spec = #client_spec{
            correct = [root, {user, Admin}],
            unauthorized = [nobody],
            forbidden = [
                {provider, P1, KeyFile, CertFile},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            path = [<<"/providers/">>, P1, <<"/spaces/">>, OtherSpace],
            expected_code = 404
        },
        logic_spec = #logic_spec{
            module = n_provider_logic,
            entity_id = P1,
            resource = {space, OtherSpace},
            expected_result = ?ERROR_REASON(?EL_NOT_FOUND)
        }
    },
    ?assert(api_test_utils:run_tests(ApiTestSpec4)).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(
        Config, ?TEST_FILE(Config, "env_desc.json"), [oz_test_utils]
    ),
    NewConfig.


end_per_suite(Config) ->
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
