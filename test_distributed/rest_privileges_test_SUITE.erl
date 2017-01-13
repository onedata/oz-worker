%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Integration tests of privileges REST module in onezone.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_privileges_test_SUITE).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").


-export([all/0, init_per_suite/1, end_per_suite/1, end_per_testcase/2]).
-export([
    view_privileges_test/1,
    set_privileges_test/1,

    modify_space_members_test/1,

    list_users_test/1,

    list_groups_test/1,

    list_spaces_test/1,
    list_providers_of_space_test/1,

    list_providers_test/1,
    list_users_of_provider_test/1,
    list_groups_of_provider_test/1,
    list_spaces_of_provider_test/1
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

        modify_space_members_test,    % (add|remove)_member_to_space,

        list_users_test,              % list_users

        list_groups_test,             % list_groups

        list_spaces_test,             % list_spaces
        list_providers_of_space_test, % list_providers_of_space

        list_providers_test,          % list_providers
        list_users_of_provider_test,  % list_users_of_provider
        list_groups_of_provider_test, % list_groups_of_provider
        list_spaces_of_provider_test  % list_spaces_of_provider
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
        od_user -> [<<"/users/">>, SubjectId, <<"/privileges">>];
        od_group -> [<<"/groups/">>, SubjectId, <<"/privileges">>]
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
        od_user -> [<<"/users/">>, SubjectId, <<"/privileges">>];
        od_group -> [<<"/groups/">>, SubjectId, <<"/privileges">>]
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


% Tries to add a user or group to a space and asserts if returned code
% matches the expected.
check_add_member_to_space(Code, Issuer, SubjectId, SubjectType, SpaceId) ->
    {ReqPath, ReqBody} = case SubjectType of
        od_user -> {
            [<<"/spaces/">>, SpaceId, <<"/users">>],
            #{<<"userId">> => SubjectId}
        };
        od_group -> {
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
        od_user ->
            [<<"/spaces/">>, SpaceId, <<"/users/">>, SubjectId];
        od_group ->
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


% Tries to list all users in the system and asserts if returned code and body
% match the expected.
check_list_users(Code, Issuer, Users) ->
    ExpectedBody = case Users of
        undefined -> undefined;
        _ -> #{<<"users">> => Users}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/users">>],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to get details of a certain user and asserts if returned code and body
% match the expected.
check_get_user_data(Code, Issuer, UserId, UserName) ->
    ExpectedBody = case UserName of
        undefined ->
            undefined;
        _ ->
            {contains, #{
                <<"userId">> => UserId,
                <<"name">> => UserName
            }}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/users/">>, UserId],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to list all groups in the system and asserts if returned code and body
% match the expected.
check_list_groups(Code, Issuer, Groups) ->
    ExpectedBody = case Groups of
        undefined -> undefined;
        _ -> #{<<"groups">> => Groups}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/groups">>],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to get details of a certain group and asserts if returned code and body
% match the expected.
check_get_group_data(Code, Issuer, GroupId, GroupName) ->
    ExpectedBody = case GroupName of
        undefined ->
            undefined;
        _ ->
            {contains, #{
                <<"groupId">> => GroupId,
                <<"name">> => GroupName
            }}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/groups/">>, GroupId],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
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


% Tries to get details of a certain space and asserts if returned code and body
% match the expected.
check_get_space_data(Code, Issuer, SpaceId, SpaceName) ->
    ExpectedBody = case SpaceName of
        undefined ->
            undefined;
        _ ->
            {contains, #{
                <<"spaceId">> => SpaceId,
                <<"name">> => SpaceName
            }}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/spaces/">>, SpaceId],
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


% Tries to get details of a certain provider of space and asserts if
% returned code and body match the expected.
check_get_provider_of_space_data(Code, Issuer, SpaceId, ProvId, ProvName) ->
    ExpectedBody = case ProvName of
        undefined ->
            undefined;
        _ ->
            {contains, #{
                <<"providerId">> => ProvId,
                <<"clientName">> => ProvName
            }}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/spaces/">>, SpaceId, <<"/providers/">>, ProvId],
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


% Tries to get details of a certain provider and asserts if returned code and
% body match the expected.
check_get_provider_data(Code, Issuer, ProviderId, ProviderName) ->
    ExpectedBody = case ProviderName of
        undefined ->
            undefined;
        _ ->
            {contains, #{
                <<"providerId">> => ProviderId,
                <<"clientName">> => ProviderName
            }}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/providers/">>, ProviderId],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to list users of a provider and asserts if returned code and body
% match the expected.
check_list_users_of_provider(Code, Issuer, ProviderId, Users) ->
    ExpectedBody = case Users of
        undefined -> undefined;
        _ -> #{<<"users">> => Users}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/providers/">>, ProviderId, <<"/users">>],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to get details of a certain user of provider and asserts if
% returned code and body match the expected.
check_get_user_of_provider_data(Code, Issuer, ProviderId, UserId, UserName) ->
    ExpectedBody = case UserName of
        undefined ->
            undefined;
        _ ->
            {contains, #{
                <<"userId">> => UserId,
                <<"name">> => UserName
            }}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/providers/">>, ProviderId, <<"/users/">>, UserId],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to list groups of a provider and asserts if returned code and body
% match the expected.
check_list_groups_of_provider(Code, Issuer, ProviderId, Groups) ->
    ExpectedBody = case Groups of
        undefined -> undefined;
        _ -> #{<<"groups">> => Groups}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/providers/">>, ProviderId, <<"/groups">>],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to get details of a certain group of provider and asserts if
% returned code and body match the expected.
check_get_group_of_provider_data(Code, Issuer, ProviderId, GrId, GrName) ->
    ExpectedBody = case GrName of
        undefined ->
            undefined;
        _ ->
            {contains, #{
                <<"groupId">> => GrId,
                <<"name">> => GrName
            }}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/providers/">>, ProviderId, <<"/groups/">>, GrId],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to list spaces of a provider and asserts if returned code and body
% match the expected.
check_list_spaces_of_provider(Code, Issuer, ProviderId, Spaces) ->
    ExpectedBody = case Spaces of
        undefined -> undefined;
        _ -> #{<<"spaces">> => Spaces}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/providers/">>, ProviderId, <<"/spaces">>],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to get details of a certain space of provider and asserts if
% returned code and body match the expected.
check_get_space_of_provider_data(Code, Issuer, ProviderId, SpaceId, SpaceName) ->
    ExpectedBody = case SpaceName of
        undefined ->
            undefined;
        _ ->
            {contains, #{
                <<"spaceId">> => SpaceId,
                <<"name">> => SpaceName
            }}
    end,
    rest_test_utils:check_rest_call(#{
        request => #{
            method => get,
            path => [<<"/providers/">>, ProviderId, <<"/spaces/">>, SpaceId],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


%%%===================================================================
%%% Test functions
%%%===================================================================

view_privileges_test(Config) ->
    rest_test_utils:set_config(Config),
    {ok, User1} = oz_test_utils:create_user(Config, #od_user{}),
    % Unauthenticated requests should be discarded (401)
    ?assert(check_view_privileges(401, undefined, User1, od_user,
        undefined)),
    % User without permissions cannot view the OZ API privileges (403)
    ?assert(check_view_privileges(403, User1, User1, od_user, undefined)),
    % Give the user view privileges and check again
    oz_test_utils:set_user_oz_privileges(Config, User1, [view_privileges]),
    ?assert(check_view_privileges(200, User1, User1, od_user,
        [<<"view_privileges">>])),
    % New users and groups should have no permissions by default
    {ok, User2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Group2} = oz_test_utils:create_group(Config, User2, <<"gr">>),
    ?assert(check_view_privileges(200, User1, User2, od_user, [])),
    ?assert(check_view_privileges(200, User1, Group2, od_group, [])),
    % Checking the privileges of nonexistent user or group should return 404
    ?assert(check_view_privileges(404, User1, <<"bad_user">>, od_user,
        undefined)),
    ?assert(check_view_privileges(404, User1, <<"bad_group">>, od_group,
        undefined)).


set_privileges_test(Config) ->
    rest_test_utils:set_config(Config),
    {ok, User1} = oz_test_utils:create_user(Config, #od_user{}),
    % Give the user perms to view and set privileges
    oz_test_utils:set_user_oz_privileges(
        Config, User1, [view_privileges, set_privileges]
    ),
    % First try some wrong perms
    ?assert(check_set_privileges(400, User1, User1, od_user,
        [inexistent, permissions])),
    % And now a nonexistent user
    ?assert(check_set_privileges(404, User1, <<"bad_user">>, od_user, [])),
    % Create a user and a group for testing
    {ok, User2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Group2} = oz_test_utils:create_group(Config, User2, <<"gr">>),
    % Get all possible privileges
    [Node | _] = ?config(oz_worker_nodes, Config),
    AllPrivilegesAtoms = rpc:call(Node, privileges, oz_privileges, []),
    AllPrivs = [atom_to_binary(P, utf8) || P <- AllPrivilegesAtoms],
    % Try to set all sublists of privileges, both for User2 and Group2.
    Sublists = [lists:sublist(AllPrivs, I) || I <- lists:seq(1, length(AllPrivs))],
    lists:foreach(
        fun(Privileges) ->
            % Set the privileges
            ?assert(check_set_privileges(204, User1, User2, od_user,
                Privileges)),
            % View the privileges
            ?assert(check_view_privileges(200, User1, User2, od_user,
                Privileges))
        end, Sublists),
    % Now for Group2
    lists:foreach(
        fun(Privileges) ->
            % Set the privileges
            ?assert(check_set_privileges(204, User1, Group2, od_group,
                Privileges)),
            % View the privileges
            ?assert(check_view_privileges(200, User1, Group2, od_group,
                Privileges))
        end, Sublists).


modify_space_members_test(Config) ->
    rest_test_utils:set_config(Config),
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, [set_privileges]),
    % TestUser will be used to test the privileges
    {ok, TestUser} = oz_test_utils:create_user(Config, #od_user{}),
    % AddedUser will be added to spaces and removed by TestUser
    {ok, AddedUser} = oz_test_utils:create_user(Config, #od_user{}),
    % AddedGroup will be added to spaces and removed by TestUser
    {ok, GroupOwner} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, AddedGroup} = oz_test_utils:create_group(
        Config, GroupOwner, <<"gr">>
    ),
    % Create a space
    {ok, SpaceOwner} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, TestSpace} = oz_test_utils:create_space(
        Config, {user, SpaceOwner}, <<"sp">>
    ),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to add or remove members from a space
    % as he does not yet have privs
    ?assert(check_add_member_to_space(403, TestUser, AddedUser,
        od_user, TestSpace)),
    ?assert(check_add_member_to_space(403, TestUser, AddedGroup,
        od_group, TestSpace)),
    % Give him the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestUser, od_user,
        [<<"add_member_to_space">>])),
    ?assert(check_add_member_to_space(204, TestUser, AddedUser,
        od_user, TestSpace)),
    ?assert(check_add_member_to_space(204, TestUser, AddedGroup,
        od_group, TestSpace)),
    % Make sure inexistent users and groups cannot be added
    ?assert(check_add_member_to_space(400, TestUser, <<"wrong_user_id">>,
        od_user, TestSpace)),
    ?assert(check_add_member_to_space(400, TestUser, <<"wrong_group_id">>,
        od_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(204, Admin, TestUser, od_user, [])),
    ?assert(check_add_member_to_space(403, TestUser, AddedUser,
        od_user, TestSpace)),
    ?assert(check_add_member_to_space(403, TestUser, AddedGroup,
        od_group, TestSpace)),
    % The user should not be able to delete users/groups without privileges
    ?assert(check_remove_member_from_space(403, TestUser, AddedUser,
        od_user, TestSpace)),
    ?assert(check_remove_member_from_space(403, TestUser, AddedGroup,
        od_group, TestSpace)),
    % Give him the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestUser, od_user,
        [<<"remove_member_from_space">>])),
    ?assert(check_remove_member_from_space(202, TestUser, AddedUser,
        od_user, TestSpace)),
    ?assert(check_remove_member_from_space(202, TestUser, AddedGroup,
        od_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(204, Admin, TestUser, od_user, [])),
    ?assert(check_remove_member_from_space(403, TestUser, AddedUser,
        od_user, TestSpace)),
    ?assert(check_remove_member_from_space(403, TestUser, AddedGroup,
        od_group, TestSpace)),

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % add or remove members from a space.
    {ok, TestGroup} = oz_test_utils:create_group(
        Config, TestUser, <<"gr">>
    ),
    % TestUser should not be able to perform add operations
    % as he does not yet have privs
    ?assert(check_add_member_to_space(403, TestUser, AddedUser,
        od_user, TestSpace)),
    ?assert(check_add_member_to_space(403, TestUser, AddedGroup,
        od_group, TestSpace)),
    % Give the privileges to his group and check again
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group,
        [<<"add_member_to_space">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_add_member_to_space(204, TestUser, AddedUser,
        od_user, TestSpace)),
    ?assert_retry_10(check_add_member_to_space(204, TestUser, AddedGroup,
        od_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_add_member_to_space(403, TestUser, AddedUser,
        od_user, TestSpace)),
    ?assert_retry_10(check_add_member_to_space(403, TestUser, AddedGroup,
        od_group, TestSpace)),
    % The user should not be able to delete users/groups without privileges
    ?assert(check_remove_member_from_space(403, TestUser, AddedUser,
        od_user, TestSpace)),
    ?assert(check_remove_member_from_space(403, TestUser, AddedGroup,
        od_group, TestSpace)),
    % Give him the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group,
        [<<"remove_member_from_space">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_remove_member_from_space(202, TestUser,
        AddedUser, od_user, TestSpace)),
    ?assert_retry_10(check_remove_member_from_space(202, TestUser,
        AddedGroup, od_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_remove_member_from_space(403, TestUser,
        AddedUser, od_user, TestSpace)),
    ?assert_retry_10(check_remove_member_from_space(403, TestUser,
        AddedGroup, od_group, TestSpace)),

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group,
    % which belongs to a group that has the privileges
    % and check if he can perform the operation.
    {_, _, TopGroup} = create_3_nested_groups(Config, TestUser),
    % TestUser should not be able to perform add operations
    % as he does not yet have privs
    ?assert(check_add_member_to_space(403, TestUser, AddedUser,
        od_user, TestSpace)),
    ?assert(check_add_member_to_space(403, TestUser, AddedGroup,
        od_group, TestSpace)),
    % Give the privileges to his group and check again
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group,
        [<<"add_member_to_space">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_add_member_to_space(204, TestUser, AddedUser,
        od_user, TestSpace)),
    ?assert_retry_10(check_add_member_to_space(204, TestUser, AddedGroup,
        od_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_add_member_to_space(403, TestUser, AddedUser,
        od_user, TestSpace)),
    ?assert_retry_10(check_add_member_to_space(403, TestUser, AddedGroup,
        od_group, TestSpace)),
    % The user should not be able to delete users/groups without privileges
    ?assert(check_remove_member_from_space(403, TestUser, AddedUser,
        od_user, TestSpace)),
    ?assert(check_remove_member_from_space(403, TestUser, AddedGroup,
        od_group, TestSpace)),
    % Give him the privileges and check again
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group,
        [<<"remove_member_from_space">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_remove_member_from_space(202, TestUser,
        AddedUser, od_user, TestSpace)),
    ?assert_retry_10(check_remove_member_from_space(202, TestUser,
        AddedGroup, od_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_remove_member_from_space(403, TestUser,
        AddedUser, od_user, TestSpace)),
    ?assert_retry_10(check_remove_member_from_space(403, TestUser,
        AddedGroup, od_group, TestSpace)).


list_users_test(Config) ->
    rest_test_utils:set_config(Config),
    % Create some users
    {ok, User1} = oz_test_utils:create_user(Config, #od_user{name = <<"u1">>}),
    {ok, User2} = oz_test_utils:create_user(Config, #od_user{name = <<"u2">>}),
    {ok, User3} = oz_test_utils:create_user(Config, #od_user{name = <<"u3">>}),
    {ok, User4} = oz_test_utils:create_user(Config, #od_user{name = <<"u4">>}),
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{name = <<"adm">>}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, [set_privileges]),
    % User will be used to test the functionality
    {ok, TestUser} = oz_test_utils:create_user(
        Config, #od_user{name = <<"tu">>}
    ),

    ExpectedUsers = [
        {Admin, <<"adm">>},
        {TestUser, <<"tu">>},
        {User1, <<"u1">>},
        {User2, <<"u2">>},
        {User3, <<"u3">>},
        {User4, <<"u4">>}
    ],
    {ExpectedUserIds, _} = lists:unzip(ExpectedUsers),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to list users
    % as he does not yet have privs
    ?assert(check_list_users(403, TestUser, undefined)),
    % As well as get data of those users
    [
        ?assert(check_get_user_data(
            403, TestUser, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ],
    % Lets grant privileges to the user
    ?assert(check_set_privileges(204, Admin, TestUser, od_user,
        [<<"list_users">>])),
    % Now he should be able to list users
    ?assert(check_list_users(200, TestUser, ExpectedUserIds)),
    [
        ?assert(check_get_user_data(
            200, TestUser, UId, UName)
        ) || {UId, UName} <- ExpectedUsers
    ],
    % Revoke the privileges again
    ?assert(check_set_privileges(204, Admin, TestUser, od_user, [])),
    % He should no longer be able to list users
    ?assert(check_list_users(403, TestUser, undefined)),
    [
        ?assert(check_get_user_data(
            403, TestUser, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ],

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % list users.
    {ok, TestGroup} = oz_test_utils:create_group(Config, TestUser, <<"gr">>),
    % The user should still not be able to list users as the group
    % does not have privileges.
    ?assert(check_list_users(403, TestUser, undefined)),
    [
        ?assert(check_get_user_data(
            403, TestUser, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ],
    % But when we grant privileges to TestGroup, he should be able to
    % list users
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group,
        [<<"list_users">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_users(200, TestUser, ExpectedUserIds)),
    [
        ?assert(check_get_user_data(
            200, TestUser, UId, UName)
        ) || {UId, UName} <- ExpectedUsers
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_users(403, TestUser, undefined)),
    [
        ?assert(check_get_user_data(
            403, TestUser, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ],

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group, which belongs to a
    % group that has the privileges and check if he can list users.
    {_, _, TopGroup} = create_3_nested_groups(Config, TestUser),
    % The user should still not be able to list users as the group
    % does not have privileges.
    ?assert(check_list_users(403, TestUser, undefined)),
    [
        ?assert(check_get_user_data(
            403, TestUser, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ],
    % But when we grant privileges to ParentGroup, he should be able to
    % list users
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group,
        [<<"list_users">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_users(200, TestUser, ExpectedUserIds)),
    [
        ?assert(check_get_user_data(
            200, TestUser, UId, UName)
        ) || {UId, UName} <- ExpectedUsers
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_users(403, TestUser, undefined)),
    [
        ?assert(check_get_user_data(
            403, TestUser, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ].


list_groups_test(Config) ->
    rest_test_utils:set_config(Config),
    % Remove predefined groups as they spoil the test results
    ok = oz_test_utils:delete_all_entities(Config, true),
    % Create some groups with some users
    {ok, UserWithGroups1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, UserWithGroups2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, UserWithGroups3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Group1} = oz_test_utils:create_group(
        Config, UserWithGroups1, <<"gr1">>
    ),
    {ok, Group2} = oz_test_utils:create_group(
        Config, UserWithGroups1, <<"gr2">>
    ),
    {ok, Group3} = oz_test_utils:create_group(
        Config, UserWithGroups2, <<"gr3">>
    ),
    {Group4, Group5, Group6} = create_3_nested_groups(
        Config, UserWithGroups3, <<"gr4">>, <<"gr5">>, <<"gr6">>
    ),
    ExpectedGroups = [
        {Group1, <<"gr1">>},
        {Group2, <<"gr2">>},
        {Group3, <<"gr3">>},
        {Group4, <<"gr4">>},
        {Group5, <<"gr5">>},
        {Group6, <<"gr6">>}
    ],
    {ExpectedGroupIds, _} = lists:unzip(ExpectedGroups),
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, [set_privileges]),
    % User will be used to test the functionality
    {ok, TestUser} = oz_test_utils:create_user(Config, #od_user{}),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to list groups
    % as he does not yet have privs
    ?assert(check_list_groups(403, TestUser, undefined)),
    % As well as get data of those groups
    [
        ?assert(check_get_group_data(
            403, TestUser, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ],
    % Lets grant privileges to the user
    ?assert(check_set_privileges(204, Admin, TestUser, od_user,
        [<<"list_groups">>])),
    % Now he should be able to list groups
    ?assert(check_list_groups(200, TestUser, ExpectedGroupIds)),
    [
        ?assert(check_get_group_data(
            200, TestUser, GrId, GrName)
        ) || {GrId, GrName} <- ExpectedGroups
    ],
    % Revoke the privileges again
    ?assert(check_set_privileges(204, Admin, TestUser, od_user, [])),
    % He should no longer be able to list groups
    ?assert(check_list_groups(403, TestUser, undefined)),
    [
        ?assert(check_get_group_data(
            403, TestUser, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ],

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % list groups.
    {ok, TestGroup} = oz_test_utils:create_group(Config, TestUser, <<"tgr">>),
    % As we have created a new group, the list is now longer. However, we do not
    % check the get_group_data functionality for the TestGroup, as the
    % TestUser belongs to it (so he has view privileges anyway).
    ExpectedGroupIds2 = [TestGroup | ExpectedGroupIds],
    % The user should still not be able to list groups as the group
    % does not have privileges.
    ?assert(check_list_groups(403, TestUser, undefined)),
    [
        ?assert(check_get_group_data(
            403, TestUser, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ],
    % But when we grant privileges to TestGroup, he should be able to
    % list groups
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group,
        [<<"list_groups">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_groups(200, TestUser, ExpectedGroupIds2)),
    [
        ?assert(check_get_group_data(
            200, TestUser, GrId, GrName)
        ) || {GrId, GrName} <- ExpectedGroups
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_groups(403, TestUser, undefined)),
    [
        ?assert(check_get_group_data(
            403, TestUser, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ],

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group, which belongs to a
    % group that has the privileges and check if he can list groups.
    {BotGroup, MidGroup, TopGroup} = create_3_nested_groups(Config, TestUser),
    % As we have created a new group, the list is now longer. However, we do not
    % check the get_group_data functionality for these 3 groups, as the
    % TestUser belongs to them (so he has view privileges anyway).
    ExpectedGroupIds3 = [BotGroup, MidGroup, TopGroup | ExpectedGroupIds2],
    % The user should still not be able to list groups as the group
    % does not have privileges.
    ?assert(check_list_groups(403, TestUser, undefined)),
    [
        ?assert(check_get_group_data(
            403, TestUser, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ],
    % But when we grant privileges to ParentGroup, he should be able to
    % list groups
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group,
        [<<"list_groups">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_groups(200, TestUser, ExpectedGroupIds3)),
    [
        ?assert(check_get_group_data(
            200, TestUser, GrId, GrName)
        ) || {GrId, GrName} <- ExpectedGroups
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_groups(403, TestUser, undefined)),
    [
        ?assert(check_get_group_data(
            403, TestUser, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ].


list_spaces_test(Config) ->
    rest_test_utils:set_config(Config),
    % Create some spaces belonging to some users
    {ok, UserWithSpaces1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, UserWithSpaces2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, UserWithSpaces3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Space1} = oz_test_utils:create_space(
        Config, {user, UserWithSpaces1}, <<"sp1">>
    ),
    {ok, Space2} = oz_test_utils:create_space(
        Config, {user, UserWithSpaces1}, <<"sp2">>
    ),
    {ok, Space3} = oz_test_utils:create_space(
        Config, {user, UserWithSpaces2}, <<"sp3">>
    ),
    {ok, Space4} = oz_test_utils:create_space(
        Config, {user, UserWithSpaces3}, <<"sp4">>
    ),
    ExpectedSpaces = [
        {Space1, <<"sp1">>},
        {Space2, <<"sp2">>},
        {Space3, <<"sp3">>},
        {Space4, <<"sp4">>}
    ],
    {ExpectedSpaceIds, _} = lists:unzip(ExpectedSpaces),
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, [set_privileges]),
    % User will be used to test the functionality
    {ok, TestUser} = oz_test_utils:create_user(Config, #od_user{}),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to list spaces
    % as he does not yet have privs
    ?assert(check_list_spaces(403, TestUser, undefined)),
    % As well as get data of those spaces
    [
        ?assert(check_get_space_data(
            403, TestUser, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ],
    % Lets grant privileges to the user
    ?assert(check_set_privileges(204, Admin, TestUser, od_user,
        [<<"list_spaces">>])),
    % Now he should be able to list spaces
    ?assert(check_list_spaces(200, TestUser, ExpectedSpaceIds)),
    [
        ?assert(check_get_space_data(
            200, TestUser, SpId, SpName)
        ) || {SpId, SpName} <- ExpectedSpaces
    ],
    % Revoke the privileges again
    ?assert(check_set_privileges(204, Admin, TestUser, od_user, [])),
    % He should no longer be able to list spaces
    ?assert(check_list_spaces(403, TestUser, undefined)),
    [
        ?assert(check_get_space_data(
            403, TestUser, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ],

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % list spaces.
    {ok, TestGroup} = oz_test_utils:create_group(Config, TestUser, <<"gr">>),
    % The user should still not be able to list spaces as the group
    % does not have privileges.
    ?assert(check_list_spaces(403, TestUser, undefined)),
    [
        ?assert(check_get_space_data(
            403, TestUser, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ],
    % But when we grant privileges to TestGroup, he should be able to
    % list spaces
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group,
        [<<"list_spaces">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_spaces(200, TestUser, ExpectedSpaceIds)),
    [
        ?assert(check_get_space_data(
            200, TestUser, SpId, SpName)
        ) || {SpId, SpName} <- ExpectedSpaces
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_spaces(403, TestUser, undefined)),
    [
        ?assert(check_get_space_data(
            403, TestUser, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ],

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group, which belongs to a
    % group that has the privileges and check if he can list spaces.
    {_, _, TopGroup} = create_3_nested_groups(Config, TestUser),
    % The user should still not be able to list spaces as the group
    % does not have privileges.
    ?assert(check_list_spaces(403, TestUser, undefined)),
    [
        ?assert(check_get_space_data(
            403, TestUser, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ],
    % But when we grant privileges to ParentGroup, he should be able to
    % list spaces
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group,
        [<<"list_spaces">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_spaces(200, TestUser, ExpectedSpaceIds)),
    [
        ?assert(check_get_space_data(
            200, TestUser, SpId, SpName)
        ) || {SpId, SpName} <- ExpectedSpaces
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_spaces(403, TestUser, undefined)),
    [
        ?assert(check_get_space_data(
            403, TestUser, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ].


list_providers_of_space_test(Config) ->
    rest_test_utils:set_config(Config),
    {ok, UserWithSpaces} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Provider1, _} = oz_test_utils:create_provider(Config, <<"pr1">>),
    {ok, Provider2, _} = oz_test_utils:create_provider(Config, <<"pr2">>),
    {ok, Space1} = oz_test_utils:create_space(
        Config, {user, UserWithSpaces}, <<"sp">>
    ),
    {ok, _} = oz_test_utils:support_space(Config, Provider1, Space1, 100),
    {ok, _} = oz_test_utils:support_space(Config, Provider2, Space1, 100),
    ExpectedProviders = [
        {Provider1, <<"pr1">>},
        {Provider2, <<"pr2">>}
    ],
    {ExpectedProviderIds, _} = lists:unzip(ExpectedProviders),
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, [set_privileges]),
    % User will be used to test the functionality
    {ok, TestUser} = oz_test_utils:create_user(Config, #od_user{}),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to list providers of space
    % as he does not yet have privs
    ?assert(check_list_providers_of_space(403, TestUser, Space1, undefined)),
    % As well as get data of those providers
    [
        ?assert(check_get_provider_of_space_data(
            403, TestUser, Space1, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],
    % Lets grant privileges to the user
    ?assert(check_set_privileges(204, Admin, TestUser, od_user,
        [<<"list_providers_of_space">>])),
    % Now he should be able to list providers of space
    ?assert(check_list_providers_of_space(200, TestUser, Space1,
        ExpectedProviderIds)),
    [
        ?assert(check_get_provider_of_space_data(
            200, TestUser, Space1, PrId, PrName)
        ) || {PrId, PrName} <- ExpectedProviders
    ],
    % Revoke the privileges again
    ?assert(check_set_privileges(204, Admin, TestUser, od_user, [])),
    % He should no longer be able to list providers of space
    ?assert(check_list_providers_of_space(403, TestUser, Space1, undefined)),
    [
        ?assert(check_get_provider_of_space_data(
            403, TestUser, Space1, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % list providers of space.
    {ok, TestGroup} = oz_test_utils:create_group(Config, TestUser, <<"gr">>),
    % The user should still not be able to list providers of space as the group
    % does not have privileges.
    ?assert(check_list_providers_of_space(403, TestUser, Space1, undefined)),
    [
        ?assert(check_get_provider_of_space_data(
            403, TestUser, Space1, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],
    % But when we grant privileges to TestGroup, he should be able to
    % list providers of space
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group,
        [<<"list_providers_of_space">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_providers_of_space(200, TestUser, Space1,
        ExpectedProviderIds)),
    [
        ?assert(check_get_provider_of_space_data(
            200, TestUser, Space1, PrId, PrName)
        ) || {PrId, PrName} <- ExpectedProviders
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_providers_of_space(403, TestUser, Space1,
        undefined)),
    [
        ?assert(check_get_provider_of_space_data(
            403, TestUser, Space1, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group, which belongs to a
    % group that has the privileges and check if he can list providers of space.
    {_, _, TopGroup} = create_3_nested_groups(Config, TestUser),
    % The user should still not be able to list providers of space as the group
    % does not have privileges.
    ?assert(check_list_providers_of_space(403, TestUser, Space1, undefined)),
    [
        ?assert(check_get_provider_of_space_data(
            403, TestUser, Space1, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],
    % But when we grant privileges to ParentGroup, he should be able to
    % list providers of space
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group,
        [<<"list_providers_of_space">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_providers_of_space(200, TestUser, Space1,
        ExpectedProviderIds)),
    [
        ?assert(check_get_provider_of_space_data(
            200, TestUser, Space1, PrId, PrName)
        ) || {PrId, PrName} <- ExpectedProviders
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_providers_of_space(403, TestUser, Space1,
        undefined)),
    [
        ?assert(check_get_provider_of_space_data(
            403, TestUser, Space1, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ].


list_providers_test(Config) ->
    rest_test_utils:set_config(Config),
    {ok, Provider1, _} = oz_test_utils:create_provider(Config, <<"pr1">>),
    {ok, Provider2, _} = oz_test_utils:create_provider(Config, <<"pr2">>),
    {ok, Provider3, _} = oz_test_utils:create_provider(Config, <<"pr3">>),
    ExpectedProviders = [
        {Provider1, <<"pr1">>},
        {Provider2, <<"pr2">>},
        {Provider3, <<"pr3">>}
    ],
    {ExpectedProviderIds, _} = lists:unzip(ExpectedProviders),
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, [set_privileges]),
    % User will be used to test the functionality
    {ok, TestUser} = oz_test_utils:create_user(Config, #od_user{}),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to list providers
    % as he does not yet have privs
    ?assert(check_list_providers(403, TestUser, undefined)),
    % As well as get data of those providers
    [
        ?assert(check_get_provider_data(
            403, TestUser, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],
    % Lets grant privileges to the user
    ?assert(check_set_privileges(204, Admin, TestUser, od_user,
        [<<"list_providers">>])),
    % Now he should be able to list providers
    ?assert(check_list_providers(200, TestUser, ExpectedProviderIds)),
    [
        ?assert(check_get_provider_data(
            200, TestUser, PrId, PrName)
        ) || {PrId, PrName} <- ExpectedProviders
    ],
    % Revoke the privileges again
    ?assert(check_set_privileges(204, Admin, TestUser, od_user, [])),
    % He should no longer be able to list providers
    ?assert(check_list_providers(403, TestUser, undefined)),
    [
        ?assert(check_get_provider_data(
            403, TestUser, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % list providers.
    {ok, TestGroup} = oz_test_utils:create_group(Config, TestUser, <<"gr">>),
    % The user should still not be able to list providers as the group
    % does not have privileges.
    ?assert(check_list_providers(403, TestUser, undefined)),
    [
        ?assert(check_get_provider_data(
            403, TestUser, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],
    % But when we grant privileges to TestGroup, he should be able to
    % list providers
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group,
        [<<"list_providers">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_providers(200, TestUser, ExpectedProviderIds)),
    [
        ?assert(check_get_provider_data(
            200, TestUser, PrId, PrName)
        ) || {PrId, PrName} <- ExpectedProviders
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_providers(403, TestUser, undefined)),
    [
        ?assert(check_get_provider_data(
            403, TestUser, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group, which belongs to a
    % group that has the privileges and check if he can list providers.
    {_, _, TopGroup} = create_3_nested_groups(Config, TestUser),
    % The user should still not be able to list providers as the group
    % does not have privileges.
    ?assert(check_list_providers(403, TestUser, undefined)),
    [
        ?assert(check_get_provider_data(
            403, TestUser, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],
    % But when we grant privileges to ParentGroup, he should be able to
    % list providers
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group,
        [<<"list_providers">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_providers(200, TestUser, ExpectedProviderIds)),
    [
        ?assert(check_get_provider_data(
            200, TestUser, PrId, PrName)
        ) || {PrId, PrName} <- ExpectedProviders
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_providers(403, TestUser, undefined)),
    [
        ?assert(check_get_provider_data(
            403, TestUser, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ].


list_users_of_provider_test(Config) ->
    rest_test_utils:set_config(Config),
    {ok, User1} = oz_test_utils:create_user(Config, #od_user{name = <<"u1">>}),
    {ok, User2} = oz_test_utils:create_user(Config, #od_user{name = <<"u2">>}),
    {ok, User3} = oz_test_utils:create_user(Config, #od_user{name = <<"u3">>}),
    {ok, Provider, _} = oz_test_utils:create_provider(Config, <<"pr">>),
    % Users 1 and 2 belong to the spaces directly
    {ok, Space1} = oz_test_utils:create_space(
        Config, {user, User1}, <<"sp">>
    ),
    {ok, Space2} = oz_test_utils:create_space(
        Config, {user, User2}, <<"sp">>
    ),
    {ok, Space3} = oz_test_utils:create_space(
        Config, {user, User2}, <<"sp">>
    ),
    % User 3 belongs to space 3 by nested groups
    {_, _, ParentGroup} = create_3_nested_groups(Config, User3),
    {ok, Space3} = oz_test_utils:add_user_to_space(
        Config, {group, ParentGroup}, Space3
    ),
    {ok, _} = oz_test_utils:support_space(Config, Provider, Space1, 100),
    {ok, _} = oz_test_utils:support_space(Config, Provider, Space2, 100),
    {ok, _} = oz_test_utils:support_space(Config, Provider, Space3, 100),
    ExpectedUsers = [
        {User1, <<"u1">>},
        {User2, <<"u2">>},
        {User3, <<"u3">>}
    ],
    {ExpectedUserIds, _} = lists:unzip(ExpectedUsers),
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, [set_privileges]),
    % User will be used to test the functionality
    {ok, TestUser} = oz_test_utils:create_user(Config, #od_user{}),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to list users of provider
    % as he does not yet have privs
    ?assert(check_list_users_of_provider(403, TestUser, Provider, undefined)),
    % As well as get data of those users
    [
        ?assert(check_get_user_of_provider_data(
            403, TestUser, Provider, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ],
    % Lets grant privileges to the user
    ?assert(check_set_privileges(204, Admin, TestUser, od_user,
        [<<"list_users_of_provider">>])),
    % Now he should be able to list users of provider
    ?assert(check_list_users_of_provider(200, TestUser, Provider,
        ExpectedUserIds)),
    [
        ?assert(check_get_user_of_provider_data(
            200, TestUser, Provider, UId, UName)
        ) || {UId, UName} <- ExpectedUsers
    ],
    % Revoke the privileges again
    ?assert(check_set_privileges(204, Admin, TestUser, od_user, [])),
    % He should no longer be able to list users of provider
    ?assert(check_list_users_of_provider(403, TestUser, Provider, undefined)),
    [
        ?assert(check_get_user_of_provider_data(
            403, TestUser, Provider, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ],

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % list users of provider.
    {ok, TestGroup} = oz_test_utils:create_group(Config, TestUser, <<"gr">>),
    % The user should still not be able to list users of provider as the group
    % does not have privileges.
    ?assert(check_list_users_of_provider(403, TestUser, Provider, undefined)),
    [
        ?assert(check_get_user_of_provider_data(
            403, TestUser, Provider, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ],
    % But when we grant privileges to TestGroup, he should be able to
    % list users of provider
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group,
        [<<"list_users_of_provider">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_users_of_provider(200, TestUser, Provider,
        ExpectedUserIds)),
    [
        ?assert(check_get_user_of_provider_data(
            200, TestUser, Provider, UId, UName)
        ) || {UId, UName} <- ExpectedUsers
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_users_of_provider(403, TestUser, Provider,
        undefined)),
    [
        ?assert(check_get_user_of_provider_data(
            403, TestUser, Provider, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ],

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group, which belongs to a
    % group that has the privileges and check if he can list users of provider.
    {_, _, TopGroup} = create_3_nested_groups(Config, TestUser),
    % The user should still not be able to list users of provider as the group
    % does not have privileges.
    ?assert(check_list_users_of_provider(403, TestUser, Provider, undefined)),
    [
        ?assert(check_get_user_of_provider_data(
            403, TestUser, Provider, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ],
    % But when we grant privileges to ParentGroup, he should be able to
    % list users of provider
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group,
        [<<"list_users_of_provider">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_users_of_provider(200, TestUser, Provider,
        ExpectedUserIds)),
    [
        ?assert(check_get_user_of_provider_data(
            200, TestUser, Provider, UId, UName)
        ) || {UId, UName} <- ExpectedUsers
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_users_of_provider(403, TestUser, Provider,
        undefined)),
    [
        ?assert(check_get_user_of_provider_data(
            403, TestUser, Provider, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ].


list_groups_of_provider_test(Config) ->
    rest_test_utils:set_config(Config),
    {ok, UserWithSpaces} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Provider, _} = oz_test_utils:create_provider(Config, <<"pr">>),
    % Users 1 and 2 belong to the spaces directly
    {ok, Space1} = oz_test_utils:create_space(
        Config, {user, UserWithSpaces}, <<"sp">>
    ),
    {ok, Space2} = oz_test_utils:create_space(
        Config, {user, UserWithSpaces}, <<"sp">>
    ),
    {ok, _} = oz_test_utils:support_space(Config, Provider, Space1, 100),
    {ok, _} = oz_test_utils:support_space(Config, Provider, Space2, 100),

    % Dummy users are users are used to create nested groups
    {ok, DummyUser1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, DummyUser2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, DummyUser3} = oz_test_utils:create_user(Config, #od_user{}),
    % Create some groups
    {ok, SomeGroup} = oz_test_utils:create_group(
        Config, DummyUser3, <<"sgr">>
    ),
    {BottomGroup1, MiddleGroup1, TopGroup1} = create_3_nested_groups(
        Config, DummyUser1, <<"bgr1">>, <<"mgr1">>, <<"tgr1">>
    ),
    {BottomGroup2, MiddleGroup2, TopGroup2} = create_3_nested_groups(
        Config, DummyUser2, <<"bgr2">>, <<"mgr2">>, <<"tgr2">>
    ),
    % Add TopGroup2 to MiddleGroup1, now TopGroup1 has the following
    % effective groups:
    %   BottomGroup1,
    %   MiddleGroup1,
    %   BottomGroup2,
    %   MiddleGroup2,
    %   TopGroup2
    {ok, MiddleGroup1} = oz_test_utils:add_user_to_group(
        Config, {group, TopGroup2}, MiddleGroup1
    ),
    % Add the groups to spaces
    {ok, Space1} = oz_test_utils:add_user_to_space(
        Config, {group, TopGroup1}, Space1
    ),
    {ok, Space2} = oz_test_utils:add_user_to_space(
        Config, {group, SomeGroup}, Space2
    ),
    ExpectedGroups = [
        {SomeGroup, <<"sgr">>},
        {BottomGroup1, <<"bgr1">>},
        {MiddleGroup1, <<"mgr1">>},
        {TopGroup1, <<"tgr1">>},
        {BottomGroup2, <<"bgr2">>},
        {MiddleGroup2, <<"mgr2">>},
        {TopGroup2, <<"tgr2">>}
    ],
    {ExpectedGroupIds, _} = lists:unzip(ExpectedGroups),
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, [set_privileges]),
    % User will be used to test the functionality
    {ok, TestUser} = oz_test_utils:create_user(Config, #od_user{}),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to list groups of provider
    % as he does not yet have privs
    ?assert(check_list_groups_of_provider(403, TestUser, Provider, undefined)),
    % As well as get data of those groups
    [
        ?assert(check_get_group_of_provider_data(
            403, TestUser, Provider, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ],
    % Lets grant privileges to the user
    ?assert(check_set_privileges(204, Admin, TestUser, od_user,
        [<<"list_groups_of_provider">>])),
    % Now he should be able to list groups of provider
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_groups_of_provider(200, TestUser, Provider,
        ExpectedGroupIds)),
    [
        ?assert(check_get_group_of_provider_data(
            200, TestUser, Provider, GrId, GrName)
        ) || {GrId, GrName} <- ExpectedGroups
    ],
    % Revoke the privileges again
    ?assert(check_set_privileges(204, Admin, TestUser, od_user, [])),
    % He should no longer be able to list groups of provider
    ?assert(check_list_groups_of_provider(403, TestUser, Provider, undefined)),
    [
        ?assert(check_get_group_of_provider_data(
            403, TestUser, Provider, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ],

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % list groups of provider.
    {ok, TestGroup} = oz_test_utils:create_group(Config, TestUser, <<"gr">>),
    % The user should still not be able to list groups of provider as the group
    % does not have privileges.
    ?assert(check_list_groups_of_provider(403, TestUser, Provider, undefined)),
    [
        ?assert(check_get_group_of_provider_data(
            403, TestUser, Provider, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ],
    % But when we grant privileges to TestGroup, he should be able to
    % list groups of provider
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group,
        [<<"list_groups_of_provider">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_groups_of_provider(200, TestUser, Provider,
        ExpectedGroupIds)),
    [
        ?assert(check_get_group_of_provider_data(
            200, TestUser, Provider, GrId, GrName)
        ) || {GrId, GrName} <- ExpectedGroups
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_groups_of_provider(403, TestUser, Provider,
        undefined)),
    [
        ?assert(check_get_group_of_provider_data(
            403, TestUser, Provider, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ],

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group, which belongs to a
    % group that has the privileges and check if he can list groups of provider.
    {_, _, TopGroup} = create_3_nested_groups(Config, TestUser),
    % The user should still not be able to list groups of provider as the group
    % does not have privileges.
    ?assert(check_list_groups_of_provider(403, TestUser, Provider, undefined)),
    [
        ?assert(check_get_group_of_provider_data(
            403, TestUser, Provider, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ],
    % But when we grant privileges to ParentGroup, he should be able to
    % list groups of provider
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group,
        [<<"list_groups_of_provider">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_groups_of_provider(200, TestUser, Provider,
        ExpectedGroupIds)),
    [
        ?assert(check_get_group_of_provider_data(
            200, TestUser, Provider, GrId, GrName)
        ) || {GrId, GrName} <- ExpectedGroups
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_groups_of_provider(403, TestUser, Provider,
        undefined)),
    [
        ?assert(check_get_group_of_provider_data(
            403, TestUser, Provider, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ].


list_spaces_of_provider_test(Config) ->
    rest_test_utils:set_config(Config),
    {ok, UserWithSpaces} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Provider, _} = oz_test_utils:create_provider(Config, <<"pr">>),
    {ok, Space1} = oz_test_utils:create_space(
        Config, {user, UserWithSpaces}, <<"sp1">>
    ),
    {ok, Space2} = oz_test_utils:create_space(
        Config, {user, UserWithSpaces}, <<"sp2">>
    ),
    {ok, Space3} = oz_test_utils:create_space(
        Config, {user, UserWithSpaces}, <<"sp3">>
    ),
    {ok, _} = oz_test_utils:support_space(Config, Provider, Space1, 100),
    {ok, _} = oz_test_utils:support_space(Config, Provider, Space2, 100),
    {ok, _} = oz_test_utils:support_space(Config, Provider, Space3, 100),
    ExpectedSpaces = [
        {Space1, <<"sp1">>},
        {Space2, <<"sp2">>},
        {Space3, <<"sp3">>}
    ],
    {ExpectedSpaceIds, _} = lists:unzip(ExpectedSpaces),
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, [set_privileges]),
    % User will be used to test the functionality
    {ok, TestUser} = oz_test_utils:create_user(Config, #od_user{}),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to list spaces of provider
    % as he does not yet have privs
    ?assert(check_list_spaces_of_provider(403, TestUser, Provider, undefined)),
    % As well as get data of those spaces
    [
        ?assert(check_get_space_of_provider_data(
            403, TestUser, Provider, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ],
    % Lets grant privileges to the user
    ?assert(check_set_privileges(204, Admin, TestUser, od_user,
        [<<"list_spaces_of_provider">>])),
    % Now he should be able to list spaces of provider
    ?assert(check_list_spaces_of_provider(
        200, TestUser, Provider, ExpectedSpaceIds
    )),
    [
        ?assert(check_get_space_of_provider_data(
            200, TestUser, Provider, SpId, SpName)
        ) || {SpId, SpName} <- ExpectedSpaces
    ],
    % Revoke the privileges again
    ?assert(check_set_privileges(204, Admin, TestUser, od_user, [])),
    % He should no longer be able to list spaces of provider
    ?assert(check_list_spaces_of_provider(403, TestUser, Provider, undefined)),
    [
        ?assert(check_get_space_of_provider_data(
            403, TestUser, Provider, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ],

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % list spaces of provider.
    {ok, TestGroup} = oz_test_utils:create_group(Config, TestUser, <<"gr">>),
    % The user should still not be able to list spaces of provider as the group
    % does not have privileges.
    ?assert(check_list_spaces_of_provider(403, TestUser, Provider, undefined)),
    [
        ?assert(check_get_space_of_provider_data(
            403, TestUser, Provider, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ],
    % But when we grant privileges to TestGroup, he should be able to
    % list spaces of provider
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group,
        [<<"list_spaces_of_provider">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_spaces_of_provider(200, TestUser, Provider,
        ExpectedSpaceIds)),
    [
        ?assert(check_get_space_of_provider_data(
            200, TestUser, Provider, SpId, SpName)
        ) || {SpId, SpName} <- ExpectedSpaces
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TestGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_spaces_of_provider(403, TestUser, Provider,
        undefined)),
    [
        ?assert(check_get_space_of_provider_data(
            403, TestUser, Provider, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ],

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group, which belongs to a
    % group that has the privileges and check if he can list spaces of provider.
    {_, _, TopGroup} = create_3_nested_groups(Config, TestUser),
    % The user should still not be able to list spaces of provider as the group
    % does not have privileges.
    ?assert(check_list_spaces_of_provider(403, TestUser, Provider, undefined)),
    [
        ?assert(check_get_space_of_provider_data(
            403, TestUser, Provider, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ],
    % But when we grant privileges to ParentGroup, he should be able to
    % list spaces of provider
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group,
        [<<"list_spaces_of_provider">>])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_spaces_of_provider(200, TestUser, Provider,
        ExpectedSpaceIds)),
    [
        ?assert(check_get_space_of_provider_data(
            200, TestUser, Provider, SpId, SpName)
        ) || {SpId, SpName} <- ExpectedSpaces
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(204, Admin, TopGroup, od_group, [])),
    % Try multiple times, because group graph takes a while to update
    ?assert_retry_10(check_list_spaces_of_provider(403, TestUser, Provider,
        undefined)),
    [
        ?assert(check_get_space_of_provider_data(
            403, TestUser, Provider, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ].

%%%===================================================================
%%% Helper functions
%%%===================================================================

% Create a group for user which belongs to a group,
% which belongs to another group. The structure looks as follows:
% User -> G1 -> G2 -> G3
create_3_nested_groups(Config, TestUser) ->
    create_3_nested_groups(Config, TestUser, <<"gr">>, <<"gr">>, <<"gr">>).
create_3_nested_groups(Config, TestUser, BotGrName, MidGrName, TopGrName) ->
    {ok, BottomGroup} = oz_test_utils:create_group(
        Config, TestUser, BotGrName
    ),
    % Dummy user will be used only to create groups
    {ok, DummyUser} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, MiddleGroup} = oz_test_utils:create_group(
        Config, DummyUser, MidGrName
    ),
    {ok, TopGroup} = oz_test_utils:create_group(
        Config, DummyUser, TopGrName
    ),
    {ok, MiddleGroup} = oz_test_utils:add_user_to_group(
        Config, {group, BottomGroup}, MiddleGroup
    ),
    {ok, TopGroup} = oz_test_utils:add_user_to_group(
        Config, {group, MiddleGroup}, TopGroup
    ),
    % Remove the dummy user, he is not needed anymore
    true = oz_test_utils:delete_user(Config, DummyUser),
    {BottomGroup, MiddleGroup, TopGroup}.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    application:start(etls),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(_Config) ->
    hackney:stop(),
    application:stop(etls).

end_per_testcase(_, Config) ->
    % Remove everything that was created during a testcase
    ok = oz_test_utils:delete_all_entities(Config).
