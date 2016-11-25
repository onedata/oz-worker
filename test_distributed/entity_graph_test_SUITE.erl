%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This file contains tests for entity graph module logic.
%%% @end
%%%-------------------------------------------------------------------
-module(entity_graph_test_SUITE).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([eff_relations_test/1]).

all() ->
    ?ALL([
        eff_relations_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================


eff_relations_test(Config) ->
    {ok, U1} = oz_test_utils:call_oz(Config, n_user_logic, create, [#od_user{name = <<"U1">>}]),
    {ok, U2} = oz_test_utils:call_oz(Config, [#od_user{name = <<"U2">>}]),
    {ok, U3} = oz_test_utils:call_oz(Config, [#od_user{name = <<"U3">>}]),
    {ok, G1} = oz_test_utils:call_oz(Config, U1, <<"G1">>),
    oz_test_utils:call_oz(Config, {user, U2}, G1),
    oz_test_utils:call_oz(Config, {user, U3}, G1),

    {ok, U4} = oz_test_utils:call_oz(Config, [#od_user{name = <<"U4">>}]),
    {ok, G2} = oz_test_utils:call_oz(Config, U4, <<"G2">>),
    oz_test_utils:call_oz(Config, {group, G1}, G2),

    {ok, U5} = oz_test_utils:call_oz(Config, [#od_user{name = <<"U5">>}]),
    {ok, S1} = oz_test_utils:call_oz(Config, {user, U5}, <<"S1">>),
    oz_test_utils:call_oz(Config, {group, G2}, S1),

    {ok, P1} = oz_test_utils:call_oz(Config, <<"P1">>),
    oz_test_utils:call_oz(Config, P1, S1, 1000),
    ok.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(
        Config, ?TEST_FILE(Config, "env_desc.json"), [oz_test_utils]
    ),
    NewConfig.


end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).