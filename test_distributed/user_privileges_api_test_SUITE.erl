%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning user privileges API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(user_privileges_api_test_SUITE).
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
    get_oz_privileges_test/1,
    get_self_oz_privileges_test/1,
    update_oz_privileges_test/1,
    update_self_oz_privileges_test/1,
    delete_oz_privileges_test/1,
    delete_self_oz_privileges_test/1,
    get_eff_oz_privileges_test/1,
    get_self_eff_oz_privileges_test/1
]).

all() ->
    ?ALL([
        get_oz_privileges_test,
        get_self_oz_privileges_test,
        update_oz_privileges_test,
        update_self_oz_privileges_test,
        delete_oz_privileges_test,
        delete_self_oz_privileges_test,
        get_eff_oz_privileges_test,
        get_self_eff_oz_privileges_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


get_oz_privileges_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User2} = oz_test_utils:create_user(Config, #od_user{}),

    InitialPrivs = [],
    AllPrivs = oz_test_utils:all_oz_privileges(Config),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        lists:foreach(fun(U) ->
            oz_test_utils:user_set_oz_privileges(Config, U, PrivsToGrant, PrivsToRevoke)
        end, [User, User2])
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_VIEW_PRIVILEGES]},
                {user, User}
            ],
            unauthorized = [nobody],

            forbidden = [
                {user, User2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/users/">>, User, <<"/privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivs}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = user_logic,
            function = get_oz_privileges,
            args = [client, User],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, User2}, ?OZ_VIEW_PRIVILEGES, true
    ])).


get_self_oz_privileges_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{user, User}]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/privileges">>],
            expected_code = ?HTTP_200_OK
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


update_oz_privileges_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    AllPrivs = oz_test_utils:all_oz_privileges(Config),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:user_set_oz_privileges(Config, User, PrivsToGrant, PrivsToRevoke)
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:user_get_oz_privileges(Config, User),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SET_PRIVILEGES]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/users/">>, User, <<"/privileges">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = update_oz_privileges,
            args = [client, User, data],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, User}, ?OZ_SET_PRIVILEGES
    ])).


update_self_oz_privileges_test(Config) ->
    % oz_set_privilege will be granted for user every time as const priv,
    % so he will be able to change his privileges
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    AllPrivs = oz_test_utils:all_oz_privileges(Config),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:user_set_oz_privileges(Config, User, PrivsToGrant, PrivsToRevoke)
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:user_get_oz_privileges(Config, User),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{user, User}]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = <<"/user/privileges">>,
            expected_code = ?HTTP_204_NO_CONTENT
        }
    },

    ?assert(api_test_scenarios:run_scenario(update_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, User}, ?OZ_SET_PRIVILEGES
    ])).


delete_oz_privileges_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    AllPrivs = oz_test_utils:all_oz_privileges(Config),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:user_set_oz_privileges(Config, User, PrivsToGrant, PrivsToRevoke)
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:user_get_oz_privileges(Config, User),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User},
                {admin, [?OZ_SET_PRIVILEGES]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/users/">>, User, <<"/privileges">>],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = user_logic,
            function = delete_oz_privileges,
            args = [client, User],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, User}, ?OZ_SET_PRIVILEGES
    ])).


delete_self_oz_privileges_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    AllPrivs = oz_test_utils:all_oz_privileges(Config),
    SetPrivsFun = fun(PrivsToGrant, PrivsToRevoke) ->
        oz_test_utils:user_set_oz_privileges(Config, User, PrivsToGrant, PrivsToRevoke)
    end,
    GetPrivsFun = fun() ->
        {ok, Privs} = oz_test_utils:user_get_oz_privileges(Config, User),
        Privs
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [{user, User}]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = <<"/user/privileges">>,
            expected_code = ?HTTP_204_NO_CONTENT
        }
    },

    ?assert(api_test_scenarios:run_scenario(delete_privileges, [
        Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs,
        {user, User}, ?OZ_SET_PRIVILEGES
    ])).


get_eff_oz_privileges_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, User2} = oz_test_utils:create_user(Config, #od_user{}),

    InitialPrivs = [],
    AllPrivs = oz_test_utils:all_oz_privileges(Config),
    SetPrivsFun = set_oz_privs_fun(Config, User, User2),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_VIEW_PRIVILEGES]},
                {user, User}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, User2}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/users/">>, User, <<"/effective_privileges">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"privileges">> => InitialPrivs}
        },
        logic_spec = #logic_spec{
            operation = get,
            module = user_logic,
            function = get_eff_oz_privileges,
            args = [client, User],
            expected_result = ?OK_LIST(InitialPrivs)
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(get_privileges, [
        Config, ApiTestSpec, SetPrivsFun, AllPrivs, [],
        {user, User2}, ?OZ_VIEW_PRIVILEGES, true
    ])).


get_self_eff_oz_privileges_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{correct = [{user, User}]},
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/user/effective_privileges">>],
            expected_code = ?HTTP_200_OK
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


set_oz_privs_fun(Config, User, User2) ->
    {BottomGroup, MidGroup, TopGroup} = oz_test_utils:create_3_nested_groups(
        Config, User
    ),
    lists:foreach(fun(Group) ->
        oz_test_utils:group_add_user(Config, Group, User2)
    end, [BottomGroup, MidGroup, TopGroup]),

    fun(PrivsToGrant, PrivsToRevoke) ->
        % In case of GRANT, randomly split privileges into four parts
        % and update the user and his groups with the privileges. His
        % eff_privileges should contain the sum of those. In case of revoke,
        % the privileges must be revoked for all 4 entities.
        #{
            1 := PrivsToGrant1, 2 := PrivsToGrant2, 3 := PrivsToGrant3, 4 := PrivsToGrant4
        } = lists:foldl(
            fun(Privilege, AccMap) ->
                Index = rand:uniform(4),
                AccMap#{Index => [Privilege | maps:get(Index, AccMap)]}
            end, #{1 => [], 2 => [], 3 => [], 4 => []}, PrivsToGrant
        ),

        oz_test_utils:user_set_oz_privileges(Config, User, PrivsToGrant1, PrivsToRevoke),
        oz_test_utils:user_set_oz_privileges(Config, User2, PrivsToGrant1, PrivsToRevoke),
        oz_test_utils:group_set_oz_privileges(Config, BottomGroup, PrivsToGrant2, PrivsToRevoke),
        oz_test_utils:group_set_oz_privileges(Config, MidGroup, PrivsToGrant3, PrivsToRevoke),
        oz_test_utils:group_set_oz_privileges(Config, TopGroup, PrivsToGrant4, PrivsToRevoke),

        oz_test_utils:ensure_entity_graph_is_up_to_date(Config)
    end.


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
