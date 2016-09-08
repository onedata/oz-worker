%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Integration tests of shares REST module in onezone.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_shares_test_SUITE).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").


-export([all/0, init_per_suite/1, end_per_suite/1, end_per_testcase/2]).
-export([
    view_share_test/1,
    view_shares_of_space_test/1,
    list_spaces_test/1,
    list_providers_test/1,
    list_providers_of_space_test/1,
    modify_space_members_test/1
]).


% Convenience macro to retry 10 times before failing
-define(assert_retry_10(_TestedValue), ?assertEqual(true, _TestedValue, 10)).


%%%===================================================================
%%% API functions
%%%===================================================================

all() ->
    % Below tests check all OZ API privileges
    ?ALL([
        % test case                   % tested_privileges
        % ---------------------------------------------------
        view_privileges_test,         % view_privileges
        set_privileges_test,          % set_privileges
        list_spaces_test,             % list_spaces
        list_providers_test,          % list_providers
        list_providers_of_space_test, % list_providers_of_space
        modify_space_members_test     % add_member_to_space,
        % remove_member_from_space
    ]).

%%%===================================================================
%%% Functions used to validate REST calls
%%%===================================================================
% Below functions are used in the tests for concise test code. They check if
% given REST operation ended with expected results.

% Tries to view privileges of SubjectType:SubjectId as Issuer
% and asserts if returned code and privileges matches expected Code
% and Privileges (use 'undefined' to skip privileges matching).
check_view_privileges(Code, Issuer, SubjectId, SubjectType, Privileges) ->
    ReqPath = case SubjectType of
        onedata_user -> [<<"/privileges/users/">>, SubjectId];
        user_group -> [<<"/privileges/groups/">>, SubjectId]
    end,
    ExpectedBody = case Privileges of
        undefined -> undefined;
        _ -> #{<<"privileges">> => Privileges}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => ReqPath,
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to set Privileges of SubjectType:SubjectId as Issuer
% and asserts if returned code matches expected Code.
check_set_privileges(Code, Issuer, SubjectId, SubjectType, Privileges) ->
    ReqPath = case SubjectType of
        onedata_user -> [<<"/privileges/users/">>, SubjectId];
        user_group -> [<<"/privileges/groups/">>, SubjectId]
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => patch,
            path => ReqPath,
            body => #{<<"privileges">> => Privileges},
            auth => Issuer
        },
        expect => #{
            code => Code
        }
    }).


% Tries to list all spaces in the system and asserts if returned code and body
% match the expected.
check_list_spaces(Code, Issuer, Spaces) ->
    ExpectedBody = case Spaces of
        undefined -> undefined;
        _ -> #{<<"spaces">> => Spaces}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/spaces">>],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to list all spaces in the system and asserts if returned code and body
% match the expected.
check_list_providers(Code, Issuer, Providers) ->
    ExpectedBody = case Providers of
        undefined -> undefined;
        _ -> #{<<"providers">> => Providers}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/providers">>],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to list all spaces in the system and asserts if returned code and body
% match the expected.
check_list_providers_of_space(Code, Issuer, SpaceId, Providers) ->
    ExpectedBody = case Providers of
        undefined -> undefined;
        _ -> #{<<"providers">> => Providers}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/spaces/">>, SpaceId, <<"/providers">>],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to add a user or group to a space and asserts if returned code
% matches the expected.
check_add_member_to_space(Code, Issuer, SubjectId, SubjectType, SpaceId) ->
    {ReqPath, ReqBody} = case SubjectType of
        onedata_user -> {
            [<<"/spaces/">>, SpaceId, <<"/users">>],
            #{<<"userId">> => SubjectId}
        };
        user_group -> {
            [<<"/spaces/">>, SpaceId, <<"/groups">>],
            #{<<"groupId">> => SubjectId}
        }
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => post,
            path => ReqPath,
            body => ReqBody,
            auth => Issuer
        },
        expect => #{
            code => Code
        }
    }).


% Tries to remove a user or group from a space and asserts if returned code
% matches the expected.
check_remove_member_from_space(Code, Issuer, SubjectId, SubjectType, SpaceId) ->
    ReqPath = case SubjectType of
        onedata_user ->
            [<<"/spaces/">>, SpaceId, <<"/users/">>, SubjectId];
        user_group ->
            [<<"/spaces/">>, SpaceId, <<"/groups/">>, SubjectId]

    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => delete,
            path => ReqPath,
            auth => Issuer
        },
        expect => #{
            code => Code
        }
    }).


%%%===================================================================
%%% Test functions
%%%===================================================================

view_privileges_test(Config) ->
    rest_test_utils:set_config(Config),
    {ok, User1} = oz_test_utils:create_user(Config, #onedata_user{}),
    % Unauthenticated requests should be discarded (401)
    ?assert(check_view_privileges(401, undefined, User1, onedata_user,
        undefined)),
    % User without permissions cannot view the OZ API privileges (403)
    ?assert(check_view_privileges(403, User1, User1, onedata_user, undefined)),
    % Give the user view privileges and check again
    set_privileges(Config, User1, onedata_user, [view_privileges]),
    ?assert(check_view_privileges(200, User1, User1, onedata_user,
        [<<"view_privileges">>])),
    % New users and groups should have no permissions by default
    {ok, User2} = oz_test_utils:create_user(Config, #onedata_user{}),
    {ok, Group2} = oz_test_utils:create_group(Config, User2, <<"gr">>),
    ?assert(check_view_privileges(200, User1, User2, onedata_user, [])),
    ?assert(check_view_privileges(200, User1, Group2, user_group, [])),
    % Checking the privileges of nonexistent user or group should return 404
    ?assert(check_view_privileges(404, User1, <<"bad_user">>, onedata_user,
        undefined)),
    ?assert(check_view_privileges(404, User1, <<"bad_group">>, user_group,
        undefined)).


set_privileges_test(Config) ->
    rest_test_utils:set_config(Config),
    {ok, User1} = oz_test_utils:create_user(Config, #onedata_user{}),
    % Give the user perms to view and set privileges
    set_privileges(
        Config, User1, onedata_user, [view_privileges, set_privileges]
    ),
    % First try some wrong perms
    ?assert(check_set_privileges(400, User1, User1, onedata_user,
        [inexistent, permissions])),
    % And now a nonexistent user
    ?assert(check_set_privileges(404, User1, <<"bad_user">>, onedata_user, [])),
    % Create a user and a group for testing
    {ok, User2} = oz_test_utils:create_user(Config, #onedata_user{}),
    {ok, Group2} = oz_test_utils:create_group(Config, User2, <<"gr">>),
    % Get all possible privileges
    [Node | _] = ?config(oz_worker_nodes, Config),
    AllPrivilegesAtoms = rpc:call(Node, oz_api_privileges, all_privileges, []),
    AllPrivileges = [atom_to_binary(P, utf8) || P <- AllPrivilegesAtoms],
    % Try to set all combinations of privileges, both for User2 and Group2.
    Combinations = all_combinations(AllPrivileges),
    lists:foreach(
        fun(Privileges) ->
            % Set the privileges
            ?assert(check_set_privileges(204, User1, User2, onedata_user,
                Privileges)),
            % View the privileges
            ?assert(check_view_privileges(200, User1, User2, onedata_user,
                Privileges))
        end, Combinations),
    % Now for Group2
    lists:foreach(
        fun(Privileges) ->
            % Set the privileges
            ?assert(check_set_privileges(204, User1, Group2, user_group,
                Privileges)),
            % View the privileges
            ?assert(check_view_privileges(200, User1, Group2, user_group,
                Privileges))
        end, Combinations).



list_spaces_test(Config) ->
    rest_test_utils:set_config(Config),
    % Create some spaces belonging to some users
    {ok, UserWithSpaces1} = oz_test_utils:create_user(Config, #onedata_user{}),
    {ok, UserWithSpaces2} = oz_test_utils:create_user(Config, #onedata_user{}),
    {ok, UserWithSpaces3} = oz_test_utils:create_user(Config, #onedata_user{}),
    {ok, Space1} = oz_test_utils:create_space(
        Config, {user, UserWithSpaces1}, <<"sp">>
    ),
    {ok, Space2} = oz_test_utils:create_space(
        Config, {user, UserWithSpaces1}, <<"sp">>
    ),
    {ok, Space3} = oz_test_utils:create_space(
        Config, {user, UserWithSpaces2}, <<"sp">>
    ),
    {ok, Space4} = oz_test_utils:create_space(
        Config, {user, UserWithSpaces3}, <<"sp">>
    ),
    ExpectedSpaces = [
        Space1,
        Space2,
        Space3,
        Space4
    ],
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #onedata_user{}),
    set_privileges(Config, Admin, onedata_user, [set_privileges]),
    % User will be used to test the functionality
    {ok, TestUser} = oz_test_utils:create_user(Config, #onedata_user{}),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to list spaces
    % as he does not yet have privs
    ?assert(check_list_spaces(403, TestUser, undefined)),
    % Lets grant privileges to the user
    ?assert(check_set_privileges(204, Admin, TestUser, onedata_user,
        [<<"list_spaces">>])),
    % Now he should be able to list spaces
    ?assert(check_list_spaces(200, TestUser, ExpectedSpaces)),
    % Revoke the privileges again
    ?assert(check_set_privileges(204, Admin, TestUser, onedata_user, [])),
    % He should no longer be able to list spaces
    ?assert(check_list_spaces(403, TestUser, undefined)),

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % list spaces.
    {ok, TestGroup} = oz_test_utils:create_group(Config, TestUser, <<"gr">>),
    % The user should still not be able to list spaces as the group
    % does not have privileges.
    ?assert(check_list_spaces(403, TestUser, undefined)),
    % But when we grant privileges to TestGroup, he should be able to
    % list spaces
    ?assert(check_set_privileges(204, Admin, TestGroup, user_group,
        [<<"list_spaces">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_spaces(200, TestUser, ExpectedSpaces)),
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestGroup, user_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_spaces(403, TestUser, undefined)),

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group, which belongs to a
    % group that has the privileges and check if he can list spaces.
    TopGroup = create_3_nested_groups(Config, TestUser),
    % The user should still not be able to list spaces as the group
    % does not have privileges.
    ?assert(check_list_spaces(403, TestUser, undefined)),
    % But when we grant privileges to ParentGroup, he should be able to
    % list spaces
    ?assert(check_set_privileges(204, Admin, TopGroup, user_group,
        [<<"list_spaces">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_spaces(200, TestUser, ExpectedSpaces)),
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TopGroup, user_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_spaces(403, TestUser, undefined)).


list_providers_test(Config) ->
    rest_test_utils:set_config(Config),
    {ok, Provider1, _} = oz_test_utils:create_provider(Config, <<"pr">>),
    {ok, Provider2, _} = oz_test_utils:create_provider(Config, <<"pr">>),
    {ok, Provider3, _} = oz_test_utils:create_provider(Config, <<"pr">>),
    ExpectedProviders = [
        Provider1,
        Provider2,
        Provider3
    ],
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #onedata_user{}),
    set_privileges(Config, Admin, onedata_user, [set_privileges]),
    % User will be used to test the functionality
    {ok, TestUser} = oz_test_utils:create_user(Config, #onedata_user{}),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to list providers
    % as he does not yet have privs
    ?assert(check_list_providers(403, TestUser, undefined)),
    % Lets grant privileges to the user
    ?assert(check_set_privileges(204, Admin, TestUser, onedata_user,
        [<<"list_providers">>])),
    % Now he should be able to list providers
    ?assert(check_list_providers(200, TestUser, ExpectedProviders)),
    % Revoke the privileges again
    ?assert(check_set_privileges(204, Admin, TestUser, onedata_user, [])),
    % He should no longer be able to list providers
    ?assert(check_list_providers(403, TestUser, undefined)),

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % list providers.
    {ok, TestGroup} = oz_test_utils:create_group(Config, TestUser, <<"gr">>),
    % The user should still not be able to list providers as the group
    % does not have privileges.
    ?assert(check_list_providers(403, TestUser, undefined)),
    % But when we grant privileges to TestGroup, he should be able to
    % list providers
    ?assert(check_set_privileges(204, Admin, TestGroup, user_group,
        [<<"list_providers">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_providers(200, TestUser, ExpectedProviders)),
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestGroup, user_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_providers(403, TestUser, undefined)),

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group, which belongs to a
    % group that has the privileges and check if he can list providers.
    TopGroup = create_3_nested_groups(Config, TestUser),
    % The user should still not be able to list providers as the group
    % does not have privileges.
    ?assert(check_list_providers(403, TestUser, undefined)),
    % But when we grant privileges to ParentGroup, he should be able to
    % list providers
    ?assert(check_set_privileges(204, Admin, TopGroup, user_group,
        [<<"list_providers">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_providers(200, TestUser, ExpectedProviders)),
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TopGroup, user_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_providers(403, TestUser, undefined)).


list_providers_of_space_test(Config) ->
    rest_test_utils:set_config(Config),
    {ok, UserWithSpaces} = oz_test_utils:create_user(Config, #onedata_user{}),
    {ok, Provider1, _} = oz_test_utils:create_provider(Config, <<"pr">>),
    {ok, Provider2, _} = oz_test_utils:create_provider(Config, <<"pr">>),
    {ok, Space1} = oz_test_utils:create_space(
        Config, {user, UserWithSpaces}, <<"sp">>
    ),
    ok = oz_test_utils:support_space(Config, Provider1, Space1, 100),
    ok = oz_test_utils:support_space(Config, Provider2, Space1, 100),
    ExpProviders = [
        Provider1,
        Provider2
    ],
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #onedata_user{}),
    set_privileges(Config, Admin, onedata_user, [set_privileges]),
    % User will be used to test the functionality
    {ok, TestUser} = oz_test_utils:create_user(Config, #onedata_user{}),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to list providers of space
    % as he does not yet have privs
    ?assert(check_list_providers_of_space(403, TestUser, Space1, undefined)),
    % Lets grant privileges to the user
    ?assert(check_set_privileges(204, Admin, TestUser, onedata_user,
        [<<"list_providers_of_space">>])),
    % Now he should be able to list providers of space
    ?assert(check_list_providers_of_space(200, TestUser, Space1, ExpProviders)),
    % Revoke the privileges again
    ?assert(check_set_privileges(204, Admin, TestUser, onedata_user, [])),
    % He should no longer be able to list providers of space
    ?assert(check_list_providers_of_space(403, TestUser, Space1, undefined)),

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % list providers of space.
    {ok, TestGroup} = oz_test_utils:create_group(Config, TestUser, <<"gr">>),
    % The user should still not be able to list providers of space as the group
    % does not have privileges.
    ?assert(check_list_providers_of_space(403, TestUser, Space1, undefined)),
    % But when we grant privileges to TestGroup, he should be able to
    % list providers of space
    ?assert(check_set_privileges(204, Admin, TestGroup, user_group,
        [<<"list_providers_of_space">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_providers_of_space(200, TestUser, Space1,
        ExpProviders)),
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestGroup, user_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_providers_of_space(403, TestUser, Space1,
        undefined)),

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group, which belongs to a
    % group that has the privileges and check if he can list providers of space.
    TopGroup = create_3_nested_groups(Config, TestUser),
    % The user should still not be able to list providers of space as the group
    % does not have privileges.
    ?assert(check_list_providers_of_space(403, TestUser, Space1, undefined)),
    % But when we grant privileges to ParentGroup, he should be able to
    % list providers of space
    ?assert(check_set_privileges(204, Admin, TopGroup, user_group,
        [<<"list_providers_of_space">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_providers_of_space(200, TestUser, Space1,
        ExpProviders)),
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TopGroup, user_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_providers_of_space(403, TestUser, Space1,
        undefined)).


modify_space_members_test(Config) ->
    rest_test_utils:set_config(Config),
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #onedata_user{}),
    set_privileges(Config, Admin, onedata_user, [set_privileges]),
    % TestUser will be used to test the privileges
    {ok, TestUser} = oz_test_utils:create_user(Config, #onedata_user{}),
    % AddedUser will be added to spaces and removed by TestUser
    {ok, AddedUser} = oz_test_utils:create_user(Config, #onedata_user{}),
    % AddedGroup will be added to spaces and removed by TestUser
    {ok, GroupOwner} = oz_test_utils:create_user(Config, #onedata_user{}),
    {ok, AddedGroup} = oz_test_utils:create_group(
        Config, GroupOwner, <<"gr">>
    ),
    % Create a space
    {ok, SpaceOwner} = oz_test_utils:create_user(Config, #onedata_user{}),
    {ok, TestSpace} = oz_test_utils:create_space(
        Config, {user, SpaceOwner}, <<"sp">>
    ),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to add or remove members from a space
    % as he does not yet have privs
    ?assert(check_add_member_to_space(403, TestUser, AddedUser,
        onedata_user, TestSpace)),
    ?assert(check_add_member_to_space(403, TestUser, AddedGroup,
        user_group, TestSpace)),
    % Give him the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestUser, onedata_user,
        [<<"add_member_to_space">>])),
    ?assert(check_add_member_to_space(204, TestUser, AddedUser,
        onedata_user, TestSpace)),
    ?assert(check_add_member_to_space(204, TestUser, AddedGroup,
        user_group, TestSpace)),
    % Make sure inexistent users and groups cannot be added
    ?assert(check_add_member_to_space(400, TestUser, <<"wrong_user_id">>,
        onedata_user, TestSpace)),
    ?assert(check_add_member_to_space(400, TestUser, <<"wrong_group_id">>,
        user_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(204, Admin, TestUser, onedata_user, [])),
    ?assert(check_add_member_to_space(403, TestUser, AddedUser,
        onedata_user, TestSpace)),
    ?assert(check_add_member_to_space(403, TestUser, AddedGroup,
        user_group, TestSpace)),
    % The user should not be able to delete users/groups without privileges
    ?assert(check_remove_member_from_space(403, TestUser, AddedUser,
        onedata_user, TestSpace)),
    ?assert(check_remove_member_from_space(403, TestUser, AddedGroup,
        user_group, TestSpace)),
    % Give him the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestUser, onedata_user,
        [<<"remove_member_from_space">>])),
    ?assert(check_remove_member_from_space(202, TestUser, AddedUser,
        onedata_user, TestSpace)),
    ?assert(check_remove_member_from_space(202, TestUser, AddedGroup,
        user_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(204, Admin, TestUser, onedata_user, [])),
    ?assert(check_remove_member_from_space(403, TestUser, AddedUser,
        onedata_user, TestSpace)),
    ?assert(check_remove_member_from_space(403, TestUser, AddedGroup,
        user_group, TestSpace)),

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % add or remove members from a space.
    {ok, TestGroup} = oz_test_utils:create_group(
        Config, TestUser, <<"gr">>
    ),
    % TestUser should not be able to perform add operations
    % as he does not yet have privs
    ?assert(check_add_member_to_space(403, TestUser, AddedUser,
        onedata_user, TestSpace)),
    ?assert(check_add_member_to_space(403, TestUser, AddedGroup,
        user_group, TestSpace)),
    % Give the privileges to his group and check again
    ?assert(check_set_privileges(204, Admin, TestGroup, user_group,
        [<<"add_member_to_space">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_add_member_to_space(204, TestUser, AddedUser,
        onedata_user, TestSpace)),
    ?assert_retry_10(check_add_member_to_space(204, TestUser, AddedGroup,
        user_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(204, Admin, TestGroup, user_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_add_member_to_space(403, TestUser, AddedUser,
        onedata_user, TestSpace)),
    ?assert_retry_10(check_add_member_to_space(403, TestUser, AddedGroup,
        user_group, TestSpace)),
    % The user should not be able to delete users/groups without privileges
    ?assert(check_remove_member_from_space(403, TestUser, AddedUser,
        onedata_user, TestSpace)),
    ?assert(check_remove_member_from_space(403, TestUser, AddedGroup,
        user_group, TestSpace)),
    % Give him the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestGroup, user_group,
        [<<"remove_member_from_space">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_remove_member_from_space(202, TestUser,
        AddedUser, onedata_user, TestSpace)),
    ?assert_retry_10(check_remove_member_from_space(202, TestUser,
        AddedGroup, user_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(204, Admin, TestGroup, user_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_remove_member_from_space(403, TestUser,
        AddedUser, onedata_user, TestSpace)),
    ?assert_retry_10(check_remove_member_from_space(403, TestUser,
        AddedGroup, user_group, TestSpace)),

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group,
    % which belongs to a group that has the privileges
    % and check if he can perform the operation.
    TopGroup = create_3_nested_groups(Config, TestUser),
    % TestUser should not be able to perform add operations
    % as he does not yet have privs
    ?assert(check_add_member_to_space(403, TestUser, AddedUser,
        onedata_user, TestSpace)),
    ?assert(check_add_member_to_space(403, TestUser, AddedGroup,
        user_group, TestSpace)),
    % Give the privileges to his group and check again
    ?assert(check_set_privileges(204, Admin, TopGroup, user_group,
        [<<"add_member_to_space">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_add_member_to_space(204, TestUser, AddedUser,
        onedata_user, TestSpace)),
    ?assert_retry_10(check_add_member_to_space(204, TestUser, AddedGroup,
        user_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(204, Admin, TopGroup, user_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_add_member_to_space(403, TestUser, AddedUser,
        onedata_user, TestSpace)),
    ?assert_retry_10(check_add_member_to_space(403, TestUser, AddedGroup,
        user_group, TestSpace)),
    % The user should not be able to delete users/groups without privileges
    ?assert(check_remove_member_from_space(403, TestUser, AddedUser,
        onedata_user, TestSpace)),
    ?assert(check_remove_member_from_space(403, TestUser, AddedGroup,
        user_group, TestSpace)),
    % Give him the privileges and check again
    ?assert(check_set_privileges(204, Admin, TopGroup, user_group,
        [<<"remove_member_from_space">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_remove_member_from_space(202, TestUser,
        AddedUser, onedata_user, TestSpace)),
    ?assert_retry_10(check_remove_member_from_space(202, TestUser,
        AddedGroup, user_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(204, Admin, TopGroup, user_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_remove_member_from_space(403, TestUser,
        AddedUser, onedata_user, TestSpace)),
    ?assert_retry_10(check_remove_member_from_space(403, TestUser,
        AddedGroup, user_group, TestSpace)).


%%%===================================================================
%%% Helper functions
%%%===================================================================

% Create a group for user which belongs to a group,
% which belongs to another group. The structure looks as follows:
% User -> G1 -> G2 -> G3
create_3_nested_groups(Config, TestUser) ->
    {ok, BottomGroup} = oz_test_utils:create_group(Config, TestUser, <<"gr">>),
    % Dummy user will be used only to create groups
    {ok, DummyUser} = oz_test_utils:create_user(Config, #onedata_user{}),
    {ok, MiddleGroup} = oz_test_utils:create_group(Config, DummyUser, <<"gr">>),
    {ok, TopGroup} = oz_test_utils:create_group(Config, DummyUser, <<"gr">>),
    ok = oz_test_utils:join_group(Config, {group, BottomGroup}, MiddleGroup),
    ok = oz_test_utils:join_group(Config, {group, MiddleGroup}, TopGroup),
    TopGroup.


set_privileges(Config, EntityId, EntityType, Privs) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    rpc:call(
        Node, oz_api_privileges_logic, modify, [EntityId, EntityType, Privs]
    ).


% Generates all possible combinations of given set (set = list),
% starting from length 0 and finishing with length of the set.
% Sequence of items does not matter. For example
% all_combinations([1,2,3) = [
%     [1,2,3],
%     [2,3],
%     [1,2],
%     [1,3],
%     [1],
%     [2],
%     [3]
% ]
all_combinations(Set) ->
    % Accumulate combinations of every possible length
    lists:foldl(
        fun(Len, Acc) ->
            combinations(Len, Set) ++ Acc
        end, [], lists:seq(0, length(Set))).

% Generates all possible combinations of given length of given set (set = list).
% Sequence of items does not matter.
combinations(0, _) ->
    [];

combinations(1, Set) ->
    [[Elem] || Elem <- Set];

combinations(Len, OriginalSet) ->
    {Res, _} = lists:foldl(
        fun(Elem, {Acc, Set}) ->
            case length(Set) > 0 of
                true ->
                    SetsWithoutElem = combinations(Len - 1, Set -- [Elem]),
                    NewSubsets = [[Elem | Subset] || Subset <- SetsWithoutElem],
                    {NewSubsets ++ Acc, Set -- [Elem]};
                false ->
                    {Acc, Set}
            end
        end, {[], OriginalSet}, OriginalSet),
    Res.

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
    test_node_starter:clean_environment(Config).

end_per_testcase(_, Config) ->
    % Remove everything that was created during a testcase
    oz_test_utils:remove_all_entities(Config).

