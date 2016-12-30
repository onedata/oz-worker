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
    get_test/1
]).

all() ->
    ?ALL([
        create_test,
        get_test
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



get_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    % Create two users, grant one of them the privilege to list users.
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    ok = oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_USERS_LIST
    ]),
    oz_test_utils:ensure_eff_graph_up_to_date(Config),

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
            path = [<<"/user/">>, User],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"providerId">> => User,
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
                    Spaces =:= #{}
            end)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


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
