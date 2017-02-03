%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Basic tests that check connection to main parts of application
%%% @end
%%%-------------------------------------------------------------------
-module(rc12_to_rc13_upgrade_test_SUITE).
-author("Tomasz Lichon").

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([
    all/0, init_per_suite/1
]).
-export([
    user_upgrade_test/1,
    group_upgrade_test/1
    %% TODO
%%    space_upgrade_test/1,
%%    share_upgrade_test/1,
%%    provider_upgrade_test/1,
%%    handle_service_upgrade_test/1,
%%    handle_upgrade_test/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    user_upgrade_test,
    group_upgrade_test
    %% TODO
%%    space_upgrade_test,
%%    share_upgrade_test,
%%    provider_upgrade_test,
%%    handle_service_upgrade_test,
%%    handle_upgrade_test
]).


user_upgrade_test(Config) ->
    OldUserRecord = old_user_record(),
    {NewVersion, NewRecord} = oz_test_utils:call_oz(
        Config, od_user, record_upgrade, [1, OldUserRecord]
    ),
    ?assertEqual(2, NewVersion),
    ?assertEqual(NewRecord, new_user_record()).


group_upgrade_test(Config) ->
    OldGroupRecord = old_group_record(),
    {NewVersion, NewRecord} = oz_test_utils:call_oz(
        Config, od_group, record_upgrade, [1, OldGroupRecord]
    ),
    ?assertEqual(2, NewVersion),
    ?assertEqual(NewRecord, new_group_record()).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    [{?LOAD_MODULES, [oz_test_utils]} | Config].


%%%===================================================================
%%% Record definitions
%%%===================================================================

old_group_record() -> {od_group,
    <<"name">>,
    role,
    [ % oz_privileges
        view_privileges, set_privileges, list_users, add_member_to_space
    ],
    [], % eff_oz_privileges
    [<<"parent1">>, <<"parent2">>],
    [
        {<<"child1">>, [?GROUP_VIEW, ?GROUP_CREATE_SPACE, ?GROUP_INVITE_GROUP]},
        {<<"child2">>, [?GROUP_UPDATE, ?GROUP_DELETE, ?GROUP_JOIN_GROUP]}
    ],
    [], % eff_parents
    [], % eff_children
    [
        {<<"user1">>, [?GROUP_JOIN_GROUP, ?GROUP_CREATE_SPACE, ?GROUP_SET_PRIVILEGES]},
        {<<"user2">>, [?GROUP_UPDATE, ?GROUP_VIEW, ?GROUP_REMOVE_GROUP]}
    ],
    [<<"space1">>, <<"space2">>, <<"space3">>],
    [<<"handle_service1">>],
    [<<"handle1">>, <<"handle2">>],
    [], % eff_users
    [], % eff_spaces
    [], % eff_shares
    [], % eff_providers
    [], % eff_handle_services
    [], % eff_handles
    false, % top_down_dirty
    false  % bottom_up_dirty
}.

new_group_record() -> #od_group{
    name = <<"name">>,
    type = role,
    oz_privileges = [?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST, ?OZ_SPACES_ADD_MEMBERS],
    eff_oz_privileges = [],

    parents = [<<"parent1">>, <<"parent2">>],
    children = #{
        <<"child1">> => [?GROUP_VIEW, ?GROUP_CREATE_SPACE, ?GROUP_INVITE_GROUP],
        <<"child2">> => [?GROUP_UPDATE, ?GROUP_DELETE, ?GROUP_JOIN_GROUP]
    },
    eff_parents = #{},
    eff_children = #{},

    users = #{
        <<"user1">> => [?GROUP_JOIN_GROUP, ?GROUP_CREATE_SPACE, ?GROUP_SET_PRIVILEGES],
        <<"user2">> => [?GROUP_UPDATE, ?GROUP_VIEW, ?GROUP_REMOVE_GROUP]
    },
    spaces = [<<"space1">>, <<"space2">>, <<"space3">>],
    handle_services = [<<"handle_service1">>],
    handles = [<<"handle1">>, <<"handle2">>],

    eff_users = #{},
    eff_spaces = #{},
    eff_providers = #{},
    eff_handle_services = #{},
    eff_handles = #{},

    top_down_dirty = true,
    bottom_up_dirty = true
}.

old_user_record() -> {od_user,
    <<"name">>,
    <<"login">>,
    true,   % basic_auth_enabled
    <<"alias">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    [
        {oauth_account,
            google,  % provider_id
            <<"user_id1">>,
            <<"login1">>,
            <<"name1">>,
            [<<"email1@email.com">>]
        },
        {oauth_account,
            github,  % provider_id
            <<"user_id2">>,
            <<"login2">>,
            <<"name2">>,
            [<<"email2@email.com">>]
        }
    ],
    <<"default_space">>,
    <<"default_provider">>,
    <<"chosen_provider">>,
    [<<"token1">>, <<"token2">>],   % client_tokens
    #{    % space_aliases
        <<"sp1">> => <<"sp1Name">>,
        <<"sp2">> => <<"sp2Name">>
    },
    [  % oz_privileges
        view_privileges, set_privileges, list_users, add_member_to_space
    ],
    [],  % eff_oz_privileges
    [<<"group1">>, <<"group2">>, <<"group3">>],
    [<<"space1">>, <<"space2">>, <<"space3">>],
    [<<"hservice1">>, <<"hservice2">>, <<"hservice3">>],
    [<<"handle1">>, <<"handle2">>, <<"handle3">>],
    [<<"eff_group1">>, <<"eff_group2">>, <<"eff_group3">>, <<"eff_group4">>],
    [],  % eff_spaces
    [],  % eff_spaces
    [],  % eff_providers
    [],  % eff_handle_services
    [],  % eff_handles
    false  % top_down_dirty
}.

new_user_record() -> #od_user{
    name = <<"name">>,
    login = <<"login">>,
    alias = <<"alias">>,
    email_list = [<<"email1@email.com">>, <<"email2@email.com">>],
    basic_auth_enabled = true,
    connected_accounts = [
        #oauth_account{
            provider_id = google,
            user_id = <<"user_id1">>,
            login = <<"login1">>,
            name = <<"name1">>,
            email_list = [<<"email1@email.com">>]
        },
        #oauth_account{
            provider_id = github,
            user_id = <<"user_id2">>,
            login = <<"login2">>,
            name = <<"name2">>,
            email_list = [<<"email2@email.com">>]
        }
    ],
    default_space = <<"default_space">>,
    default_provider = <<"default_provider">>,
    chosen_provider = <<"chosen_provider">>,
    client_tokens = [<<"token1">>, <<"token2">>],
    space_aliases = #{
        <<"sp1">> => <<"sp1Name">>,
        <<"sp2">> => <<"sp2Name">>
    },
    oz_privileges = [
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES,
        ?OZ_USERS_LIST, ?OZ_SPACES_ADD_MEMBERS
    ],
    eff_oz_privileges = [],
    groups = [<<"group1">>, <<"group2">>, <<"group3">>],
    spaces = [<<"space1">>, <<"space2">>, <<"space3">>],
    handle_services = [<<"hservice1">>, <<"hservice2">>, <<"hservice3">>],
    handles = [<<"handle1">>, <<"handle2">>, <<"handle3">>],
    eff_groups = #{}, % eff groups should be empty, the entity will be recalculated anyway
    eff_spaces = #{},
    eff_providers = #{},
    eff_handle_services = #{},
    eff_handles = #{},
    top_down_dirty = true
}.