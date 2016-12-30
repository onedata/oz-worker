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
    #document{value = #od_user{name = Name, login = Login}} = User,
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
    #document{value = #od_user{name = Name, login = Login}} = User2,
    ?assertEqual(Name, <<"Name">>),
    ?assertEqual(Login, <<"login">>),
    % Second try should fail (such id exists)
    ?assertMatch(?ERROR_BAD_VALUE_ID_OCCUPIED(user_id), oz_test_utils:call_oz(
        Config, n_user_logic, create, [UserRecord, PredefinedUserId]
    )).


get_test(Config) ->
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root, nobody]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/provider">>,
            expected_code = ?HTTP_200_OK,
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
                {<<"name">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"name">>)},
                {<<"name">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"name">>)},
                {<<"urls">>, [], ?ERROR_BAD_VALUE_EMPTY(<<"urls">>)},
                {<<"urls">>, <<"127.0.0.1">>, ?ERROR_BAD_VALUE_LIST_OF_BINARIES(<<"urls">>)},
                {<<"urls">>, 1234, ?ERROR_BAD_VALUE_LIST_OF_BINARIES(<<"urls">>)},
                {<<"redirectionPoint">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"redirectionPoint">>)},
                {<<"redirectionPoint">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"redirectionPoint">>)},
                {<<"csr">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"csr">>)},
                {<<"csr">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"csr">>)},
                {<<"csr">>, <<"wrong-csr">>, ?ERROR_BAD_DATA(<<"csr">>)},
                {<<"latitude">>, <<"ASDASD">>, ?ERROR_BAD_VALUE_FLOAT(<<"latitude">>)},
                {<<"latitude">>, -1500, ?ERROR_BAD_VALUE_NOT_BETWEEN(<<"latitude">>, -90, 90)},
                {<<"latitude">>, -90.1, ?ERROR_BAD_VALUE_NOT_BETWEEN(<<"latitude">>, -90, 90)},
                {<<"latitude">>, 90.1, ?ERROR_BAD_VALUE_NOT_BETWEEN(<<"latitude">>, -90, 90)},
                {<<"latitude">>, 1500, ?ERROR_BAD_VALUE_NOT_BETWEEN(<<"latitude">>, -90, 90)},
                {<<"longitude">>, <<"ASDASD">>, ?ERROR_BAD_VALUE_FLOAT(<<"longitude">>)},
                {<<"longitude">>, -1500, ?ERROR_BAD_VALUE_NOT_BETWEEN(<<"longitude">>, -180, 180)},
                {<<"longitude">>, -180.1, ?ERROR_BAD_VALUE_NOT_BETWEEN(<<"longitude">>, -180, 180)},
                {<<"longitude">>, 180.1, ?ERROR_BAD_VALUE_NOT_BETWEEN(<<"longitude">>, -180, 180)},
                {<<"longitude">>, 1500, ?ERROR_BAD_VALUE_NOT_BETWEEN(<<"longitude">>, -180, 180)}
            ]
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
