%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning harvester spaces API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(harvester_spaces_api_test_SUITE).
-author("Michal Stanisz").

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
    add_space_test/1,
    create_space_invite_token_test/1,
    remove_space_test/1,
    list_spaces_test/1,
    get_space_test/1
]).

all() ->
    ?ALL([
        add_space_test,
        create_space_invite_token_test,
        remove_space_test,
        list_spaces_test,
        get_space_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


add_space_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, UserNoAddSpacePriv} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, UserNoAddHarvesterPriv} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    oz_test_utils:user_set_oz_privileges(Config, User, [?OZ_HARVESTERS_CREATE], []),
    {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(User), ?HARVESTER_DATA),

    oz_test_utils:harvester_add_user(Config, H1, UserNoAddSpacePriv),
    oz_test_utils:space_add_user(Config, S1, UserNoAddSpacePriv),
    oz_test_utils:harvester_set_user_privileges(Config, H1, UserNoAddSpacePriv,
        privileges:harvester_privileges() -- [?HARVESTER_ADD_SPACE], [?HARVESTER_ADD_SPACE]
    ),
    oz_test_utils:space_set_user_privileges(Config, S1, UserNoAddSpacePriv,
        privileges:space_privileges(), []
    ),

    oz_test_utils:harvester_add_user(Config, H1, UserNoAddHarvesterPriv),
    oz_test_utils:space_add_user(Config, S1, UserNoAddHarvesterPriv),
    oz_test_utils:space_set_user_privileges(Config, S1, UserNoAddHarvesterPriv,
        privileges:space_privileges() -- [?SPACE_ADD_HARVESTER], [?SPACE_ADD_HARVESTER]
    ),
    oz_test_utils:harvester_set_user_privileges(Config, H1, UserNoAddHarvesterPriv,
        privileges:harvester_privileges(), []
    ),

    VerifyEndFun = fun
        (true = _ShouldSucceed, _, _) ->
            {ok, Spaces} = oz_test_utils:harvester_get_spaces(Config, H1),
            ?assert(lists:member(S1, Spaces)),
            oz_test_utils:harvester_remove_space(Config, H1, S1);
        (false = _ShouldSucceed, _, _) ->
            {ok, Spaces} = oz_test_utils:harvester_get_spaces(Config, H1),
            ?assertNot(lists:member(S1, Spaces))
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, User},
                root,
                {admin, [?OZ_HARVESTERS_ADD_RELATIONSHIPS, ?OZ_SPACES_ADD_RELATIONSHIPS]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, UserNoAddSpacePriv},
                {user, UserNoAddHarvesterPriv}
            ]
        },
        rest_spec = #rest_spec{
            method = put,
            path = [<<"/harvesters/">>, H1, <<"/spaces/">>, S1],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                ExpLocation = ?URL(Config, [<<"/harvesters/">>, H1, <<"/spaces/">>, S1]),
                ?assertEqual(ExpLocation, Location),
                true
            end
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = add_space,
            args = [client, H1, S1],
            expected_result = ?OK_BINARY(S1)
        },
        % TODO gs
        data_spec = #data_spec{
            required = [],
            correct_values = #{},
            bad_values = []
        }
    },

    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, undefined, undefined, VerifyEndFun
    )).

create_space_invite_token_test(Config) ->
    % create harvester with 2 users:
    %   U2 gets the HARVESTER_ADD_SPACE privilege
    %   U1 gets all remaining privileges
    {H1, U1, U2} = api_test_scenarios:create_basic_harvester_env(
        Config, ?HARVESTER_ADD_SPACE
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    VerifyFun = api_test_scenarios:collect_unique_tokens_fun(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_ADD_RELATIONSHIPS]},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/harvesters/">>, H1, <<"/spaces/token">>],
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token}) -> VerifyFun(Token) end
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = create_space_invite_token,
            args = [client, H1],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).



remove_space_test(Config) ->
    % create harvester with 2 users:
    %   U2 gets the HARVESTER_REMOVE_SPACE privilege
    %   U1 gets all remaining privileges
    {H1, U1, U2} = api_test_scenarios:create_basic_harvester_env(
        Config, ?HARVESTER_REMOVE_SPACE
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
        {ok, S1} = oz_test_utils:harvester_add_space(Config, H1, S1),
        #{spaceId => S1}
    end,
    DeleteEntityFun = fun(#{spaceId := SpaceId} = _Env) ->
        oz_test_utils:harvester_remove_space(Config, H1, SpaceId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, _) ->
        {ok, Spaces} = oz_test_utils:harvester_get_spaces(Config, H1),
        ?assertEqual(lists:member(SpaceId, Spaces), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_REMOVE_RELATIONSHIPS, ?OZ_SPACES_REMOVE_RELATIONSHIPS]},
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
            path = [<<"/harvesters/">>, H1, <<"/spaces/">>, spaceId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = remove_space,
            args = [client, H1, spaceId],
            expected_result = ?OK
        }
        % TODO gs
    },

    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
    )).


list_spaces_test(Config) ->
    % create harvester with 2 users:
    %   U2 gets the HARVESTER_VIEW privilege
    %   U1 gets all remaining privileges
    {H1, U1, U2} = api_test_scenarios:create_basic_harvester_env(
        Config, ?HARVESTER_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    ExpSpaces = lists:map(
        fun(_) ->
            {ok, SpaceId} = oz_test_utils:create_space(
                Config, ?ROOT, ?SPACE_NAME1
            ),
            oz_test_utils:harvester_add_space(Config, H1, SpaceId),
            SpaceId
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_HARVESTERS_LIST_RELATIONSHIPS]},
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
            path = [<<"/harvesters/">>, H1, <<"/spaces">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaces">> => ExpSpaces}
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_spaces,
            args = [client, H1],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_space_test(Config) ->
    % create harvester with 2 users:
    %   U2 gets the HARVESTER_VIEW privilege
    %   U1 gets all remaining privileges
    {H1, U1, U2} = api_test_scenarios:create_basic_harvester_env(
        Config, ?HARVESTER_VIEW
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, S1} = oz_test_utils:create_space(
        Config, ?ROOT, ?SPACE_NAME1
    ),
    oz_test_utils:harvester_add_space(Config, H1, S1),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_VIEW]},
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
            path = [<<"/harvesters/">>, H1, <<"/spaces/">>, S1],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"spaceId">> => S1,
                <<"name">> => ?SPACE_NAME1,
                <<"providers">> => #{}
            }
        },
        logic_spec = #logic_spec{
            module = harvester_logic,
            function = get_space,
            args = [client, H1, S1],
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"name">> => ?SPACE_NAME1,
                <<"providers">> => #{}
            })
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_space, id = S1, aspect = instance, scope = protected
            },
            auth_hint = ?THROUGH_HARVESTER(H1),
            expected_result = ?OK_MAP(#{
                <<"name">> => ?SPACE_NAME1,
                <<"providers">> => #{},
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(Id, S1)
                end
            })
        }
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

