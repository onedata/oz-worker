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
-module(provider_logic_test_SUITE).
-author("Lukasz Opiola").

-include("entity_logic_errors.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-define(CREATE_PROVIDER_DATA, begin
    {_, __CSRFile, _} = generate_cert_files(),
    {ok, __CSR} = file:read_file(__CSRFile),
    #{
        <<"name">> => <<"ProvName">>,
        <<"urls">> => [<<"127.0.0.1">>],
        <<"redirectionPoint">> => <<"https://127.0.0.1">>,
        <<"csr">> => __CSR,
        <<"latitude">> => 50.0,
        <<"longitude">> => -24.8
    }
end).

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    create_test/1,
    support_space_test/1,
    get_test/1,
    %% TODO list_test/1,
    get_spaces_test/1,
    get_eff_users_test/1,
    get_eff_groups_test/1
]).

all() ->
    ?ALL([
        create_test,
        support_space_test,
        get_test,
        get_spaces_test,
        get_eff_users_test,
        get_eff_groups_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_test(Config) ->
    ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA]
    )),
    ?assertMatch({error, ?EL_BAD_DATA(<<"csr">>)}, oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA#{
            <<"csr">> => <<"wrong-csr">>
        }]
    )),
    ?assertMatch({error, ?EL_BAD_DATA(<<"latitude">>)}, oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA#{
            <<"latitude">> => -91
        }]
    )),
    ?assertMatch({error, ?EL_BAD_DATA(<<"latitude">>)}, oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA#{
            <<"latitude">> => 91
        }]
    )),
    ?assertMatch({error, ?EL_BAD_DATA(<<"longitude">>)}, oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA#{
            <<"longitude">> => -181
        }]
    )),
    ?assertMatch({error, ?EL_BAD_DATA(<<"longitude">>)}, oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA#{
            <<"longitude">> => 181
        }]
    )).


support_space_test(Config) ->
    MinimumSupportSize = min_support_size(Config),
    {ok, U1} = oz_test_utils:call_oz(
        Config, n_user_logic, create, [#od_user{}]
    ),
    {ok, S1} = oz_test_utils:call_oz(
        Config, n_space_logic, create, [?USER(U1), #{
            <<"name">> => <<"Space1">>
        }]
    ),
    {ok, {P1, _}} = oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA]
    ),
    % Try bad token first
    ?assertMatch({error, ?EL_BAD_TOKEN(<<"token">>)}, oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(P1), P1, #{
            <<"token">> => <<"bad-token">>, <<"size">> => MinimumSupportSize
        }]
    )),
    % Bad token type
    {ok, BadMacaroon} = oz_test_utils:call_oz(
        Config, n_space_logic, create_invite_user_token, [?USER(U1), S1]
    ),
    ?assertMatch({error, ?EL_BAD_TOKEN_TYPE(<<"token">>)}, oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(P1), P1, #{
            <<"token">> => BadMacaroon, <<"size">> => MinimumSupportSize
        }]
    )),
    {ok, Macaroon} = oz_test_utils:call_oz(
        Config, n_space_logic, create_invite_provider_token, [?USER(U1), S1]
    ),
    % Bad support size
    {error, ?EL_BAD_DATA(<<"size">>)} = oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(P1), P1, #{
            <<"token">> => Macaroon, <<"size">> => 100
        }]
    ),
    % Correct request
    ?assertMatch({ok, S1}, oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(P1), P1, #{
            <<"token">> => Macaroon, <<"size">> => MinimumSupportSize
        }]
    )),
    % Try to support by already supporting provider
    {ok, Macaroon2} = oz_test_utils:call_oz(
        Config, n_space_logic, create_invite_provider_token, [?USER(U1), S1]
    ),
    ?assertMatch({error, ?EL_RELATION_EXISTS}, oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(P1), P1, #{
            <<"token">> => Macaroon2, <<"size">> => MinimumSupportSize
        }]
    )),
    % Create another space and support, this time by serialized token
    {ok, S2} = oz_test_utils:call_oz(
        Config, n_space_logic, create, [?USER(U1), #{
            <<"name">> => <<"Space2">>
        }]
    ),
    ensure_eff_graph_up_to_date(Config),
    {ok, Macaroon3} = oz_test_utils:call_oz(
        Config, n_space_logic, create_invite_provider_token, [?USER(U1), S2]
    ),
    {ok, Token} = token_logic:serialize(Macaroon3),
    ?assertMatch({ok, S2}, oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(P1), P1, #{
            <<"token">> => Token, <<"size">> => MinimumSupportSize
        }]
    )).


get_test(Config) ->
    Data = ?CREATE_PROVIDER_DATA,
    ExpName = maps:get(<<"name">>, Data),
    ExpUrls = maps:get(<<"urls">>, Data),
    ExpRedPoint = maps:get(<<"redirectionPoint">>, Data),
    ExpLat = maps:get(<<"latitude">>, Data),
    ExpLong = maps:get(<<"longitude">>, Data),

    {ok, {P1, _}} = oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, Data]
    ),

    {ok, GetResult} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_provider_logic, get, [?PROVIDER(P1), P1]
    )),
    ?assertEqual(ExpName, GetResult#od_provider.name),
    ?assertEqual(ExpUrls, GetResult#od_provider.urls),
    ?assertEqual(ExpRedPoint, GetResult#od_provider.redirection_point),
    ?assertEqual(ExpLat, GetResult#od_provider.latitude),
    ?assertEqual(ExpLong, GetResult#od_provider.longitude),
    ?assertEqual([], GetResult#od_provider.spaces),
    ?assertEqual(#{}, GetResult#od_provider.eff_users),
    ?assertEqual(#{}, GetResult#od_provider.eff_groups),

    {ok, {P2, _}} = oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, Data]
    ),
    % Provider 2 should be able to get data about provider 1
    {ok, GetResult2} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_provider_logic, get, [?PROVIDER(P2), P1]
    )),
    ?assertEqual(ExpName, GetResult2#od_provider.name),
    ?assertEqual(ExpUrls, GetResult2#od_provider.urls),
    ?assertEqual(ExpRedPoint, GetResult2#od_provider.redirection_point),
    ?assertEqual(ExpLat, GetResult2#od_provider.latitude),
    ?assertEqual(ExpLong, GetResult2#od_provider.longitude),
    ?assertEqual([], GetResult2#od_provider.spaces),
    ?assertEqual(#{}, GetResult2#od_provider.eff_users),
    ?assertEqual(#{}, GetResult2#od_provider.eff_groups),

    % But anyone should not
    ?assertMatch({error, ?EL_UNAUTHORIZED}, oz_test_utils:call_oz(
        Config, n_provider_logic, get, [?NOBODY, P1]
    )).


get_spaces_test(Config) ->
    {ok, {P1, _}} = oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA]
    ),
    ?assertMatch({ok, []}, oz_test_utils:call_oz(
        Config, n_provider_logic, get_spaces, [?PROVIDER(P1), P1]
    )),
    % Create some spaces
    {S1, S2, S3} = create_and_support_3_spaces(Config, P1),
    ExpectedSpaces = lists:sort([S1, S2, S3]),
    % Get spaces
    {ok, Spaces} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_provider_logic, get_spaces, [?PROVIDER(P1), P1]
    )),
    ?assertEqual(ExpectedSpaces, lists:sort(Spaces)),
    % Get each space
    ?assertMatch({ok, #od_space{name = <<"s1">>}}, oz_test_utils:call_oz(
        Config, n_provider_logic, get_space, [?PROVIDER(P1), P1, S1]
    )),
    ?assertMatch({ok, #od_space{name = <<"s2">>}}, oz_test_utils:call_oz(
        Config, n_provider_logic, get_space, [?PROVIDER(P1), P1, S2]
    )),
    ?assertMatch({ok, #od_space{name = <<"s3">>}}, oz_test_utils:call_oz(
        Config, n_provider_logic, get_space, [?PROVIDER(P1), P1, S3]
    )),
    % Check if getting random id does not work
    ?assertMatch({error, ?EL_NOT_FOUND}, oz_test_utils:call_oz(
        Config, n_provider_logic, get_space, [?PROVIDER(P1), P1, <<"asd">>]
    )).


get_eff_users_test(Config) ->
    {ok, {P1, _}} = oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA]
    ),
    % Create a user and grant him admin privileges
    {ok, U1} = oz_test_utils:call_oz(
        Config, n_user_logic, create, [#od_user{}]
    ),
    ok = oz_test_utils:call_oz(
        Config, n_user_logic, modify_oz_privileges, [?ROOT, U1, #{
            <<"operation">> => grant, <<"privileges">> => [list_users_of_provider]
        }]
    ),
    ensure_eff_graph_up_to_date(Config),
    % For now, there are no users
    ?assertMatch({ok, []}, oz_test_utils:call_oz(
        Config, n_provider_logic, get_eff_users, [?USER(U1), P1]
    )),
    % Create some spaces and support them
    {S1, S2, S3} = create_and_support_3_spaces(Config, P1),
    % Add some users to the spaces (9 users, 3 in each space)
    ExpectedUsers = lists:map(
        fun({Counter, Space}) ->
            UserName = str_utils:format_bin("u~B", [Counter]),
            {ok, User} = oz_test_utils:call_oz(
                Config, n_user_logic, create, [#od_user{name = UserName}]
            ),
            {ok, Space} = oz_test_utils:call_oz(
                Config, n_space_logic, add_user, [?ROOT, Space, User]
            ),
            User
        end, lists:zip(lists:seq(1, 9), [S1, S2, S3, S1, S2, S3, S1, S2, S3])),
    ensure_eff_graph_up_to_date(Config),
    {ok, Users} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_provider_logic, get_eff_users, [?USER(U1), P1]
    )),
    ?assertEqual(lists:sort(Users), lists:sort(ExpectedUsers)),
    % Create some more spaces and support them
    {S4, S5, S6} = create_and_support_3_spaces(Config, P1),
    % Create two groups for every space and three users for every group
    ExpectedUsersFromGroups = lists:flatmap(
        fun({Counter, Space}) ->
            UserName1 = str_utils:format_bin("u1@g~B", [Counter]),
            {ok, User1} = oz_test_utils:call_oz(
                Config, n_user_logic, create, [#od_user{name = UserName1}]
            ),
            UserName2 = str_utils:format_bin("u2@g~B", [Counter]),
            {ok, User2} = oz_test_utils:call_oz(
                Config, n_user_logic, create, [#od_user{name = UserName2}]
            ),
            GroupName = str_utils:format_bin("g~B", [Counter]),
            {ok, Group} = oz_test_utils:call_oz(
                Config, n_group_logic, create, [?USER(User1), #{<<"name">> => GroupName}]
            ),
            {ok, Group} = oz_test_utils:call_oz(
                Config, n_group_logic, add_user, [?ROOT, Group, User2]
            ),
            {ok, Space} = oz_test_utils:call_oz(
                Config, n_space_logic, add_group, [?ROOT, Space, Group]
            ),
            [User1, User2]
        end, lists:zip(lists:seq(1, 6), [S4, S5, S6, S4, S5, S6])),
    ensure_eff_graph_up_to_date(Config),
    {ok, Users2} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_provider_logic, get_eff_users, [?USER(U1), P1]
    )),
    ?assertEqual(lists:sort(Users2), lists:sort(ExpectedUsers ++ ExpectedUsersFromGroups)),
    ok.


get_eff_groups_test(Config) ->
    {ok, {P1, _}} = oz_test_utils:call_oz(
        Config, n_provider_logic, create, [?NOBODY, ?CREATE_PROVIDER_DATA]
    ),
    % Create a user and grant him admin privileges
    {ok, U1} = oz_test_utils:call_oz(
        Config, n_user_logic, create, [#od_user{}]
    ),
    ok = oz_test_utils:call_oz(
        Config, n_user_logic, modify_oz_privileges, [?ROOT, U1, #{
            <<"operation">> => grant, <<"privileges">> => [list_groups_of_provider]
        }]
    ),
    ensure_eff_graph_up_to_date(Config),
    % For now, there are no groups
    ?assertMatch({ok, []}, oz_test_utils:call_oz(
        Config, n_provider_logic, get_eff_groups, [?USER(U1), P1]
    )),
    % Create some spaces and support them
    {S1, S2, S3} = create_and_support_3_spaces(Config, P1),
    {S4, S5, S6} = create_and_support_3_spaces(Config, P1),
    % Create two groups for every space
    ExpectedGroups = lists:map(
        fun({Counter, Space}) ->
            GroupName = str_utils:format_bin("g~B", [Counter]),
            {ok, Group} = oz_test_utils:call_oz(
                Config, n_group_logic, create, [?ROOT, #{<<"name">> => GroupName}]
            ),
            {ok, Space} = oz_test_utils:call_oz(
                Config, n_space_logic, add_group, [?ROOT, Space, Group]
            ),
            Group
        end, lists:zip(lists:seq(1, 6), [S1, S2, S3, S4, S5, S6])),
    ensure_eff_graph_up_to_date(Config),
    {ok, Groups2} = ?assertMatch({ok, _}, oz_test_utils:call_oz(
        Config, n_provider_logic, get_eff_groups, [?USER(U1), P1]
    )),
    ?assertEqual(lists:sort(Groups2), lists:sort(ExpectedGroups)),
    % Check each group
    lists:foreach(fun({Counter, Group}) ->
        GroupName = str_utils:format_bin("g~B", [Counter]),
        ?assertMatch({ok, #od_group{name = GroupName}}, oz_test_utils:call_oz(
            Config, n_provider_logic, get_eff_group, [?USER(U1), P1, Group]
        ))
    end, lists:zip(lists:seq(1, 6), ExpectedGroups)).


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


create_and_support_3_spaces(Config, ProvId) ->
    MinimumSupportSize = min_support_size(Config),
    {ok, S1} = oz_test_utils:call_oz(
        Config, n_space_logic, create, [?ROOT, #{<<"name">> => <<"s1">>}]
    ),
    {ok, S2} = oz_test_utils:call_oz(
        Config, n_space_logic, create, [?ROOT, #{<<"name">> => <<"s2">>}]
    ),
    {ok, S3} = oz_test_utils:call_oz(
        Config, n_space_logic, create, [?ROOT, #{<<"name">> => <<"s3">>}]
    ),
    % Support them by the provider
    {ok, Macaroon1} = oz_test_utils:call_oz(
        Config, n_space_logic, create_invite_provider_token, [?ROOT, S1]
    ),
    {ok, Macaroon2} = oz_test_utils:call_oz(
        Config, n_space_logic, create_invite_provider_token, [?ROOT, S2]
    ),
    {ok, Macaroon3} = oz_test_utils:call_oz(
        Config, n_space_logic, create_invite_provider_token, [?ROOT, S3]
    ),
    {ok, _} = oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(ProvId), ProvId, #{
            <<"token">> => Macaroon1, <<"size">> => MinimumSupportSize
        }]
    ),
    {ok, _} = oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(ProvId), ProvId, #{
            <<"token">> => Macaroon2, <<"size">> => MinimumSupportSize
        }]
    ),
    {ok, _} = oz_test_utils:call_oz(
        Config, n_provider_logic, support_space, [?PROVIDER(ProvId), ProvId, #{
            <<"token">> => Macaroon3, <<"size">> => MinimumSupportSize
        }]
    ),
    {S1, S2, S3}.


ensure_eff_graph_up_to_date(Config) ->
    oz_test_utils:call_oz(Config, entity_graph, ensure_up_to_date, []).


min_support_size(Config) ->
    {ok, MinimumSupportSize} = oz_test_utils:call_oz(
        Config, application, get_env, [oz_worker, minimum_space_support_size]
    ),
    MinimumSupportSize.


generate_cert_files() ->
    Prefix = "provider" ++ integer_to_list(erlang:system_time(micro_seconds)),
    KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
    CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
    CertFile = filename:join(?TEMP_DIR, Prefix ++ "_cert.pem"),
    os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
    os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
    {KeyFile, CSRFile, CertFile}.