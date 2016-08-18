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
-module(predefined_groups_test_SUITE).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([predefined_groups_test/1]).

all() ->
    ?ALL([
        predefined_groups_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================

% This test checks if the procedure creating predefined groups works as
% expected. It is normally called from node_manager_plugin:after_init callback -
% so will already have happened during environment initialization, before
% this test starts. This means that to test it we need to call the procedure
% once again after the environment has started.
predefined_groups_test(Config) ->
    [Node | _] = Nodes = ?config(oz_worker_nodes, Config),
    % Prepare config entry that will define what groups should be created, each
    % with different OZ API privileges.
    PredefinedGroups = [
        #{
            id => <<"group1">>,
            name => <<"Group 1">>,
            oz_api_privileges => {oz_api_privileges, all_privileges}
        },
        #{
            id => <<"group2">>,
            name => <<"Group 2">>,
            oz_api_privileges => [view_privileges, set_privileges]
        },
        #{
            id => <<"group3">>,
            name => <<"Group 3">>,
            oz_api_privileges => []
        }
    ],
    % Set the corresponding env variable on every node
    test_utils:set_env(Node, oz_worker, predefined_groups, PredefinedGroups),
    % Call the group creation procedure. The function reads from env and
    % creates the predefined groups
    ?assertEqual(ok, rpc:call(Node, group_logic, create_predefined_groups, [])),
    % Now, lets check if the groups are present in the system and have desired
    % privileges.
    CheckGroup = fun(ExpId, ExpName, ExpPrivileges) ->
        GroupResult = rpc:call(Node, user_group, get, [ExpId]),
        % Check if group was found by ID
        ?assertMatch({ok, _}, GroupResult),
        % Check if the name is correct
        {ok, #document{value = #user_group{name = ActualName}}} = GroupResult,
        ?assertEqual(ExpName, ActualName),
        % Check if OZ API privileges are correct
        PrivsResult = rpc:call(Node, oz_api_privileges_logic, get, [
            ExpId, user_group
        ]),
        % Check if privileges were found by group ID
        ?assertMatch({ok, _}, PrivsResult),
        % Check if the privileges is correct
        {ok, ActualPrivileges} = PrivsResult,
        ?assertEqual(ExpPrivileges, ActualPrivileges)
    end,
    AllPrivs = rpc:call(Node, oz_api_privileges, all_privileges, []),
    CheckGroup(<<"group1">>, <<"Group 1">>, AllPrivs),
    CheckGroup(<<"group2">>, <<"Group 2">>, [view_privileges, set_privileges]),
    CheckGroup(<<"group3">>, <<"Group 3">>, []),
    ok.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    NewConfig.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).