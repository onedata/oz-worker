%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
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
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").


-export([all/0, init_per_suite/1, end_per_suite/1, end_per_testcase/2]).
-export([
    modify_space_members_test/1,

    list_providers_of_space_test/1,

    list_providers_test/1,
    list_eff_users_of_provider_test/1,
    list_eff_groups_of_provider_test/1,
    list_spaces_of_provider_test/1
]).


%%%===================================================================
%%% API functions
%%%===================================================================

all() ->
    % Below tests check all OZ API privileges
    ?ALL([
        % test case                   % tested_privileges
        % ---------------------------------------------------
        modify_space_members_test,    % ?OZ_SPACES_(ADD|REMOVE)_MEMBERS,

        list_providers_of_space_test, % ?OZ_SPACES_LIST_PROVIDERS

        list_providers_test,          % ?OZ_USERS_LIST
        list_eff_users_of_provider_test,  % ?OZ_PROVIDERS_LIST_USERS
        list_eff_groups_of_provider_test, % ?OZ_PROVIDERS_LIST_GROUPS
        list_spaces_of_provider_test  % ?OZ_PROVIDERS_LIST_SPACES
    ]).

%%%===================================================================
%%% Functions used to validate REST calls
%%%===================================================================
% Below functions are used in the tests for concise test code. They check if
% given REST operation ended with expected results.

% Tries to set Privileges of SubjectType:SubjectId as Issuer
% and asserts if returned code matches expected Code.
check_set_privileges(Config, Code, Issuer, SubjectId, SubjectType, Privileges) ->
    ReqPath = case SubjectType of
        od_user -> [<<"/users/">>, SubjectId, <<"/privileges">>];
        od_group -> [<<"/groups/">>, SubjectId, <<"/privileges">>]
    end,
    PrivilegesBin = [atom_to_binary(P, utf8) || P <- Privileges],
    rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => patch,
            path => ReqPath,
            body => #{<<"privileges">> => PrivilegesBin},
            auth => Issuer
        },
        expect => #{
            code => Code
        }
    }).


% Tries to add a user or group to a space and asserts if returned code
% matches the expected.
check_add_member_to_space(Config, Code, Issuer, SubjectId, SubjectType, SpaceId) ->
    ReqPath = case SubjectType of
        od_user ->
            [<<"/spaces/">>, SpaceId, <<"/users/">>, SubjectId];
        od_group ->
            [<<"/spaces/">>, SpaceId, <<"/groups/">>, SubjectId]
    end,
    rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => put,
            path => ReqPath,
            auth => Issuer
        },
        expect => #{
            code => Code
        }
    }).


% Tries to remove a user or group from a space and asserts if returned code
% matches the expected.
check_remove_member_from_space(Config, Code, Issuer, SubjectId, SubjectType, SpaceId) ->
    ReqPath = case SubjectType of
        od_user ->
            [<<"/spaces/">>, SpaceId, <<"/users/">>, SubjectId];
        od_group ->
            [<<"/spaces/">>, SpaceId, <<"/groups/">>, SubjectId]

    end,
    rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => delete,
            path => ReqPath,
            auth => Issuer
        },
        expect => #{
            code => Code
        }
    }).


% Tries to list all spaces in the system and asserts if returned code and body
% match the expected.
check_list_providers_of_space(Config, Code, Issuer, SpaceId, Providers) ->
    ExpectedBody = case Providers of
        undefined -> undefined;
        _ -> #{<<"providers">> => Providers}
    end,
    rest_test_utils:check_rest_call(Config, #{
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
check_get_provider_of_space_data(Config, Code, Issuer, SpaceId, ProvId, ProvName) ->
    ExpectedBody = case ProvName of
        undefined ->
            undefined;
        _ ->
            {contains, #{
                <<"providerId">> => ProvId,
                <<"clientName">> => ProvName
            }}
    end,
    rest_test_utils:check_rest_call(Config, #{
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
check_list_providers(Config, Code, Issuer, Providers) ->
    ExpectedBody = case Providers of
        undefined -> undefined;
        _ -> #{<<"providers">> => Providers}
    end,
    rest_test_utils:check_rest_call(Config, #{
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
check_get_provider_data(Config, Code, Issuer, ProviderId, ProviderName) ->
    ExpectedBody = case ProviderName of
        undefined ->
            undefined;
        _ ->
            {contains, #{
                <<"providerId">> => ProviderId,
                <<"clientName">> => ProviderName
            }}
    end,
    rest_test_utils:check_rest_call(Config, #{
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
check_list_eff_users_of_provider(Config, Code, Issuer, ProviderId, Users) ->
    ExpectedBody = case Users of
        undefined -> undefined;
        _ -> #{<<"users">> => Users}
    end,
    rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => [<<"/providers/">>, ProviderId, <<"/effective_users">>],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to get details of a certain user of provider and asserts if
% returned code and body match the expected.
check_get_eff_user_of_provider_data(Config, Code, Issuer, ProviderId, UserId, UserName) ->
    ExpectedBody = case UserName of
        undefined ->
            undefined;
        _ ->
            {contains, #{
                <<"userId">> => UserId,
                <<"name">> => UserName
            }}
    end,
    rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => [<<"/providers/">>, ProviderId, <<"/effective_users/">>, UserId],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to list groups of a provider and asserts if returned code and body
% match the expected.
check_list_eff_groups_of_provider(Config, Code, Issuer, ProviderId, Groups) ->
    ExpectedBody = case Groups of
        undefined -> undefined;
        _ -> #{<<"groups">> => Groups}
    end,
    rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => [<<"/providers/">>, ProviderId, <<"/effective_groups">>],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to get details of a certain group of provider and asserts if
% returned code and body match the expected.
check_get_eff_group_of_provider_data(Config, Code, Issuer, ProviderId, GrId, GrName) ->
    ExpectedBody = case GrName of
        undefined ->
            undefined;
        _ ->
            {contains, #{
                <<"groupId">> => GrId,
                <<"name">> => GrName
            }}
    end,
    rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => [<<"/providers/">>, ProviderId, <<"/effective_groups/">>, GrId],
            auth => Issuer
        },
        expect => #{
            code => Code,
            body => ExpectedBody
        }
    }).


% Tries to list spaces of a provider and asserts if returned code and body
% match the expected.
check_list_spaces_of_provider(Config, Code, Issuer, ProviderId, Spaces) ->
    ExpectedBody = case Spaces of
        undefined -> undefined;
        _ -> #{<<"spaces">> => Spaces}
    end,
    rest_test_utils:check_rest_call(Config, #{
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
check_get_space_of_provider_data(Config, Code, Issuer, ProviderId, SpaceId, SpaceName) ->
    ExpectedBody = case SpaceName of
        undefined ->
            undefined;
        _ ->
            {contains, #{
                <<"spaceId">> => SpaceId,
                <<"name">> => SpaceName
            }}
    end,
    rest_test_utils:check_rest_call(Config, #{
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


modify_space_members_test(Config) ->
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, set, [?OZ_SET_PRIVILEGES]),
    % TestUser will be used to test the privileges
    {ok, TestUser} = oz_test_utils:create_user(Config, #od_user{}),
    % AddedUser will be added to spaces and removed by TestUser
    {ok, AddedUser} = oz_test_utils:create_user(Config, #od_user{}),
    % AddedGroup will be added to spaces and removed by TestUser
    {ok, GroupOwner} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, AddedGroup} = oz_test_utils:create_group(
        Config, ?USER(GroupOwner), <<"gr">>
    ),
    % Create a space
    {ok, SpaceOwner} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, TestSpace} = oz_test_utils:create_space(
        Config, ?USER(SpaceOwner), <<"sp">>
    ),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to add or remove members from a space
    % as he does not yet have privs
    ?assert(check_add_member_to_space(Config, 403, {user, TestUser}, AddedUser,
        od_user, TestSpace)),
    ?assert(check_add_member_to_space(Config, 403, {user, TestUser}, AddedGroup,
        od_group, TestSpace)),
    % Give him the privileges and check again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestUser, od_user,
        [?OZ_SPACES_ADD_MEMBERS])),
    ?assert(check_add_member_to_space(Config, 201, {user, TestUser}, AddedUser,
        od_user, TestSpace)),
    ?assert(check_add_member_to_space(Config, 201, {user, TestUser}, AddedGroup,
        od_group, TestSpace)),
    % Make sure inexistent users and groups cannot be added
    ?assert(check_add_member_to_space(Config, 400, {user, TestUser}, <<"wrong_user_id">>,
        od_user, TestSpace)),
    ?assert(check_add_member_to_space(Config, 400, {user, TestUser}, <<"wrong_group_id">>,
        od_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestUser, od_user, [])),
    ?assert(check_add_member_to_space(Config, 403, {user, TestUser}, AddedUser,
        od_user, TestSpace)),
    ?assert(check_add_member_to_space(Config, 403, {user, TestUser}, AddedGroup,
        od_group, TestSpace)),
    % The user should not be able to delete users/groups without privileges
    ?assert(check_remove_member_from_space(Config, 403, {user, TestUser}, AddedUser,
        od_user, TestSpace)),
    ?assert(check_remove_member_from_space(Config, 403, {user, TestUser}, AddedGroup,
        od_group, TestSpace)),
    % Give him the privileges and check again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestUser, od_user,
        [?OZ_SPACES_REMOVE_MEMBERS])),
    ?assert(check_remove_member_from_space(Config, 202, {user, TestUser}, AddedUser,
        od_user, TestSpace)),
    ?assert(check_remove_member_from_space(Config, 202, {user, TestUser}, AddedGroup,
        od_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestUser, od_user, [])),
    ?assert(check_remove_member_from_space(Config, 404, {user, TestUser}, AddedUser,
        od_user, TestSpace)),
    ?assert(check_remove_member_from_space(Config, 404, {user, TestUser}, AddedGroup,
        od_group, TestSpace)),

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % add or remove members from a space.
    {ok, TestGroup} = oz_test_utils:create_group(Config, ?USER(TestUser), <<"gr">>),
    % TestUser should not be able to perform add operations
    % as he does not yet have privs
    ?assert(check_add_member_to_space(Config, 403, {user, TestUser}, AddedUser,
        od_user, TestSpace)),
    ?assert(check_add_member_to_space(Config, 403, {user, TestUser}, AddedGroup,
        od_group, TestSpace)),
    % Give the privileges to his group and check again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestGroup, od_group,
        [?OZ_SPACES_ADD_MEMBERS])),
    ?assert(check_add_member_to_space(Config, 201, {user, TestUser}, AddedUser,
        od_user, TestSpace)),
    ?assert(check_add_member_to_space(Config, 201, {user, TestUser}, AddedGroup,
        od_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestGroup, od_group, [])),
    ?assert(check_add_member_to_space(Config, 403, {user, TestUser}, AddedUser,
        od_user, TestSpace)),
    ?assert(check_add_member_to_space(Config, 403, {user, TestUser}, AddedGroup,
        od_group, TestSpace)),
    % The user should not be able to delete users/groups without privileges
    ?assert(check_remove_member_from_space(Config, 403, {user, TestUser}, AddedUser,
        od_user, TestSpace)),
    ?assert(check_remove_member_from_space(Config, 403, {user, TestUser}, AddedGroup,
        od_group, TestSpace)),
    % Give him the privileges and check again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestGroup, od_group,
        [?OZ_SPACES_REMOVE_MEMBERS])),
    ?assert(check_remove_member_from_space(Config, 202, {user, TestUser},
        AddedUser, od_user, TestSpace)),
    ?assert(check_remove_member_from_space(Config, 202, {user, TestUser},
        AddedGroup, od_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestGroup, od_group, [])),
    ?assert(check_remove_member_from_space(Config, 404, {user, TestUser},
        AddedUser, od_user, TestSpace)),
    ?assert(check_remove_member_from_space(Config, 404, {user, TestUser},
        AddedGroup, od_group, TestSpace)),

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group,
    % which belongs to a group that has the privileges
    % and check if he can perform the operation.
    {_, _, TopGroup} = oz_test_utils:create_3_nested_groups(Config, TestUser),
    % TestUser should not be able to perform add operations
    % as he does not yet have privs
    ?assert(check_add_member_to_space(Config, 403, {user, TestUser}, AddedUser,
        od_user, TestSpace)),
    ?assert(check_add_member_to_space(Config, 403, {user, TestUser}, AddedGroup,
        od_group, TestSpace)),
    % Give the privileges to his group and check again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TopGroup, od_group,
        [?OZ_SPACES_ADD_MEMBERS])),
    ?assert(check_add_member_to_space(Config, 201, {user, TestUser}, AddedUser,
        od_user, TestSpace)),
    ?assert(check_add_member_to_space(Config, 201, {user, TestUser}, AddedGroup,
        od_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TopGroup, od_group, [])),
    ?assert(check_add_member_to_space(Config, 403, {user, TestUser}, AddedUser,
        od_user, TestSpace)),
    ?assert(check_add_member_to_space(Config, 403, {user, TestUser}, AddedGroup,
        od_group, TestSpace)),
    % The user should not be able to delete users/groups without privileges
    ?assert(check_remove_member_from_space(Config, 403, {user, TestUser}, AddedUser,
        od_user, TestSpace)),
    ?assert(check_remove_member_from_space(Config, 403, {user, TestUser}, AddedGroup,
        od_group, TestSpace)),
    % Give him the privileges and check again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TopGroup, od_group,
        [?OZ_SPACES_REMOVE_MEMBERS])),
    ?assert(check_remove_member_from_space(Config, 202, {user, TestUser},
        AddedUser, od_user, TestSpace)),
    ?assert(check_remove_member_from_space(Config, 202, {user, TestUser},
        AddedGroup, od_group, TestSpace)),
    % Revoke the privileges and make sure he cannot
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TopGroup, od_group, [])),
    ?assert(check_remove_member_from_space(Config, 404, {user, TestUser},
        AddedUser, od_user, TestSpace)),
    ?assert(check_remove_member_from_space(Config, 404, {user, TestUser},
        AddedGroup, od_group, TestSpace)).


list_providers_of_space_test(Config) ->
    {ok, UserWithSpaces} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, {Provider1, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"pr1">>),
    {ok, {Provider2, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"pr2">>),
    {ok, Space1} = oz_test_utils:create_space(
        Config, ?USER(UserWithSpaces), <<"sp">>
    ),
    {ok, _} = oz_test_utils:support_space(Config, Provider1, Space1, 10000000),
    {ok, _} = oz_test_utils:support_space(Config, Provider2, Space1, 10000000),
    ExpectedProviders = [
        {Provider1, <<"pr1">>},
        {Provider2, <<"pr2">>}
    ],
    {ExpectedProviderIds, _} = lists:unzip(ExpectedProviders),
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, set, [?OZ_SET_PRIVILEGES]),
    % User will be used to test the functionality
    {ok, TestUser} = oz_test_utils:create_user(Config, #od_user{}),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to list providers of space
    % as he does not yet have privs
    ?assert(check_list_providers_of_space(Config, 403, {user, TestUser}, Space1, undefined)),
    % As well as get data of those providers
    [
        ?assert(check_get_provider_of_space_data(
            Config, 403, {user, TestUser}, Space1, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],
    % Lets grant privileges to the user
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestUser, od_user,
        [?OZ_SPACES_LIST_PROVIDERS])),
    % Now he should be able to list providers of space
    ?assert(check_list_providers_of_space(Config, 200, {user, TestUser}, Space1,
        ExpectedProviderIds)),
    [
        ?assert(check_get_provider_of_space_data(
            Config, 200, {user, TestUser}, Space1, PrId, PrName)
        ) || {PrId, PrName} <- ExpectedProviders
    ],
    % Revoke the privileges again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestUser, od_user, [])),
    % He should no longer be able to list providers of space
    ?assert(check_list_providers_of_space(Config, 403, {user, TestUser}, Space1, undefined)),
    [
        ?assert(check_get_provider_of_space_data(
            Config, 403, {user, TestUser}, Space1, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % list providers of space.
    {ok, TestGroup} = oz_test_utils:create_group(Config, ?USER(TestUser), <<"gr">>),
    % The user should still not be able to list providers of space as the group
    % does not have privileges.
    ?assert(check_list_providers_of_space(Config, 403, {user, TestUser}, Space1, undefined)),
    [
        ?assert(check_get_provider_of_space_data(
            Config, 403, {user, TestUser}, Space1, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],
    % But when we grant privileges to TestGroup, he should be able to
    % list providers of space
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestGroup, od_group,
        [?OZ_SPACES_LIST_PROVIDERS])),
    ?assert(check_list_providers_of_space(Config, 200, {user, TestUser}, Space1,
        ExpectedProviderIds)),
    [
        ?assert(check_get_provider_of_space_data(
            Config, 200, {user, TestUser}, Space1, PrId, PrName)
        ) || {PrId, PrName} <- ExpectedProviders
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestGroup, od_group, [])),
    ?assert(check_list_providers_of_space(Config, 403, {user, TestUser}, Space1,
        undefined)),
    [
        ?assert(check_get_provider_of_space_data(
            Config, 403, {user, TestUser}, Space1, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group, which belongs to a
    % group that has the privileges and check if he can list providers of space.
    {_, _, TopGroup} = oz_test_utils:create_3_nested_groups(Config, TestUser),
    % The user should still not be able to list providers of space as the group
    % does not have privileges.
    ?assert(check_list_providers_of_space(Config, 403, {user, TestUser}, Space1, undefined)),
    [
        ?assert(check_get_provider_of_space_data(
            Config, 403, {user, TestUser}, Space1, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],
    % But when we grant privileges to ParentGroup, he should be able to
    % list providers of space
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TopGroup, od_group,
        [?OZ_SPACES_LIST_PROVIDERS])),
    ?assert(check_list_providers_of_space(Config, 200, {user, TestUser}, Space1,
        ExpectedProviderIds)),
    [
        ?assert(check_get_provider_of_space_data(
            Config, 200, {user, TestUser}, Space1, PrId, PrName)
        ) || {PrId, PrName} <- ExpectedProviders
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TopGroup, od_group, [])),
    ?assert(check_list_providers_of_space(Config, 403, {user, TestUser}, Space1,
        undefined)),
    [
        ?assert(check_get_provider_of_space_data(
            Config, 403, {user, TestUser}, Space1, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ].


list_providers_test(Config) ->
    {ok, {Provider1, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"pr1">>),
    {ok, {Provider2, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"pr2">>),
    {ok, {Provider3, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"pr3">>),
    ExpectedProviders = [
        {Provider1, <<"pr1">>},
        {Provider2, <<"pr2">>},
        {Provider3, <<"pr3">>}
    ],
    {ExpectedProviderIds, _} = lists:unzip(ExpectedProviders),
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, set, [?OZ_SET_PRIVILEGES]),
    % User will be used to test the functionality
    {ok, TestUser} = oz_test_utils:create_user(Config, #od_user{}),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to list providers
    % as he does not yet have privs
    ?assert(check_list_providers(Config, 403, {user, TestUser}, undefined)),
    % As well as get data of those providers
    [
        ?assert(check_get_provider_data(
            Config, 403, {user, TestUser}, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],
    % Lets grant privileges to the user
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestUser, od_user,
        [?OZ_PROVIDERS_LIST])),
    % Now he should be able to list providers
    ?assert(check_list_providers(Config, 200, {user, TestUser}, ExpectedProviderIds)),
    [
        ?assert(check_get_provider_data(
            Config, 200, {user, TestUser}, PrId, PrName)
        ) || {PrId, PrName} <- ExpectedProviders
    ],
    % Revoke the privileges again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestUser, od_user, [])),
    % He should no longer be able to list providers
    ?assert(check_list_providers(Config, 403, {user, TestUser}, undefined)),
    [
        ?assert(check_get_provider_data(
            Config, 403, {user, TestUser}, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % list providers.
    {ok, TestGroup} = oz_test_utils:create_group(Config, ?USER(TestUser), <<"gr">>),
    % The user should still not be able to list providers as the group
    % does not have privileges.
    ?assert(check_list_providers(Config, 403, {user, TestUser}, undefined)),
    [
        ?assert(check_get_provider_data(
            Config, 403, {user, TestUser}, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],
    % But when we grant privileges to TestGroup, he should be able to
    % list providers
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestGroup, od_group,
        [?OZ_PROVIDERS_LIST])),
    ?assert(check_list_providers(Config, 200, {user, TestUser}, ExpectedProviderIds)),
    [
        ?assert(check_get_provider_data(
            Config, 200, {user, TestUser}, PrId, PrName)
        ) || {PrId, PrName} <- ExpectedProviders
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestGroup, od_group, [])),
    ?assert(check_list_providers(Config, 403, {user, TestUser}, undefined)),
    [
        ?assert(check_get_provider_data(
            Config, 403, {user, TestUser}, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group, which belongs to a
    % group that has the privileges and check if he can list providers.
    {_, _, TopGroup} = oz_test_utils:create_3_nested_groups(Config, TestUser),
    % The user should still not be able to list providers as the group
    % does not have privileges.
    ?assert(check_list_providers(Config, 403, {user, TestUser}, undefined)),
    [
        ?assert(check_get_provider_data(
            Config, 403, {user, TestUser}, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ],
    % But when we grant privileges to ParentGroup, he should be able to
    % list providers
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TopGroup, od_group,
        [?OZ_PROVIDERS_LIST])),
    ?assert(check_list_providers(Config, 200, {user, TestUser}, ExpectedProviderIds)),
    [
        ?assert(check_get_provider_data(
            Config, 200, {user, TestUser}, PrId, PrName)
        ) || {PrId, PrName} <- ExpectedProviders
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TopGroup, od_group, [])),
    ?assert(check_list_providers(Config, 403, {user, TestUser}, undefined)),
    [
        ?assert(check_get_provider_data(
            Config, 403, {user, TestUser}, PrId, undefined)
        ) || {PrId, _PrName} <- ExpectedProviders
    ].


list_eff_users_of_provider_test(Config) ->
    {ok, User1} = oz_test_utils:create_user(Config, #od_user{name = <<"u1">>}),
    {ok, User2} = oz_test_utils:create_user(Config, #od_user{name = <<"u2">>}),
    {ok, User3} = oz_test_utils:create_user(Config, #od_user{name = <<"u3">>}),
    {ok, {Provider, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"pr">>),
    % Users 1 and 2 belong to the spaces directly
    {ok, Space1} = oz_test_utils:create_space(
        Config, ?USER(User1), <<"sp">>
    ),
    {ok, Space2} = oz_test_utils:create_space(
        Config, ?USER(User2), <<"sp">>
    ),
    {ok, Space3} = oz_test_utils:create_space(
        Config, ?USER(User2), <<"sp">>
    ),
    % User 3 belongs to space 3 by nested groups
    {_, _, ParentGroup} = oz_test_utils:create_3_nested_groups(Config, User3),
    {ok, ParentGroup} = oz_test_utils:space_add_group(
        Config, Space3, ParentGroup
    ),
    {ok, _} = oz_test_utils:support_space(Config, Provider, Space1, 10000000),
    {ok, _} = oz_test_utils:support_space(Config, Provider, Space2, 10000000),
    {ok, _} = oz_test_utils:support_space(Config, Provider, Space3, 10000000),
    ExpectedUsers = [
        {User1, <<"u1">>},
        {User2, <<"u2">>},
        {User3, <<"u3">>}
    ],
    {ExpectedUserIds, _} = lists:unzip(ExpectedUsers),
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, set, [?OZ_SET_PRIVILEGES]),
    % User will be used to test the functionality
    {ok, TestUser} = oz_test_utils:create_user(Config, #od_user{}),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to list users of provider
    % as he does not yet have privs
    ?assert(check_list_eff_users_of_provider(Config, 403, {user, TestUser}, Provider, undefined)),
    % As well as get data of those users
    [
        ?assert(check_get_eff_user_of_provider_data(
            Config, 403, {user, TestUser}, Provider, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ],
    % Lets grant privileges to the user
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestUser, od_user,
        [?OZ_PROVIDERS_LIST_USERS])),
    % Now he should be able to list users of provider
    ?assert(check_list_eff_users_of_provider(Config, 200, {user, TestUser}, Provider,
        ExpectedUserIds)),
    [
        ?assert(check_get_eff_user_of_provider_data(
            Config, 200, {user, TestUser}, Provider, UId, UName)
        ) || {UId, UName} <- ExpectedUsers
    ],
    % Revoke the privileges again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestUser, od_user, [])),
    % He should no longer be able to list users of provider
    ?assert(check_list_eff_users_of_provider(Config, 403, {user, TestUser}, Provider, undefined)),
    [
        ?assert(check_get_eff_user_of_provider_data(
            Config, 403, {user, TestUser}, Provider, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ],

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % list users of provider.
    {ok, TestGroup} = oz_test_utils:create_group(Config, ?USER(TestUser), <<"gr">>),
    % The user should still not be able to list users of provider as the group
    % does not have privileges.
    ?assert(check_list_eff_users_of_provider(Config, 403, {user, TestUser}, Provider, undefined)),
    [
        ?assert(check_get_eff_user_of_provider_data(
            Config, 403, {user, TestUser}, Provider, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ],
    % But when we grant privileges to TestGroup, he should be able to
    % list users of provider
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestGroup, od_group,
        [?OZ_PROVIDERS_LIST_USERS])),
    ?assert(check_list_eff_users_of_provider(Config, 200, {user, TestUser}, Provider,
        ExpectedUserIds)),
    [
        ?assert(check_get_eff_user_of_provider_data(
            Config, 200, {user, TestUser}, Provider, UId, UName)
        ) || {UId, UName} <- ExpectedUsers
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestGroup, od_group, [])),
    ?assert(check_list_eff_users_of_provider(Config, 403, {user, TestUser}, Provider,
        undefined)),
    [
        ?assert(check_get_eff_user_of_provider_data(
            Config, 403, {user, TestUser}, Provider, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ],

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group, which belongs to a
    % group that has the privileges and check if he can list users of provider.
    {_, _, TopGroup} = oz_test_utils:create_3_nested_groups(Config, TestUser),
    % The user should still not be able to list users of provider as the group
    % does not have privileges.
    ?assert(check_list_eff_users_of_provider(Config, 403, {user, TestUser}, Provider, undefined)),
    [
        ?assert(check_get_eff_user_of_provider_data(
            Config, 403, {user, TestUser}, Provider, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ],
    % But when we grant privileges to ParentGroup, he should be able to
    % list users of provider
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TopGroup, od_group,
        [?OZ_PROVIDERS_LIST_USERS])),
    ?assert(check_list_eff_users_of_provider(Config, 200, {user, TestUser}, Provider,
        ExpectedUserIds)),
    [
        ?assert(check_get_eff_user_of_provider_data(
            Config, 200, {user, TestUser}, Provider, UId, UName)
        ) || {UId, UName} <- ExpectedUsers
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TopGroup, od_group, [])),
    ?assert(check_list_eff_users_of_provider(Config, 403, {user, TestUser}, Provider,
        undefined)),
    [
        ?assert(check_get_eff_user_of_provider_data(
            Config, 403, {user, TestUser}, Provider, UId, undefined)
        ) || {UId, _UName} <- ExpectedUsers
    ].


list_eff_groups_of_provider_test(Config) ->
    {ok, UserWithSpaces} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, {Provider, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"pr">>),
    % Users 1 and 2 belong to the spaces directly
    {ok, Space1} = oz_test_utils:create_space(
        Config, ?USER(UserWithSpaces), <<"sp">>
    ),
    {ok, Space2} = oz_test_utils:create_space(
        Config, ?USER(UserWithSpaces), <<"sp">>
    ),
    {ok, _} = oz_test_utils:support_space(Config, Provider, Space1, 10000000),
    {ok, _} = oz_test_utils:support_space(Config, Provider, Space2, 10000000),

    % Dummy users are users are used to create nested groups
    {ok, DummyUser1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, DummyUser2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, DummyUser3} = oz_test_utils:create_user(Config, #od_user{}),
    % Create some groups
    {ok, SomeGroup} = oz_test_utils:create_group(
        Config, ?USER(DummyUser3), <<"sgr">>
    ),
    {BottomGroup1, MiddleGroup1, TopGroup1} = oz_test_utils:create_3_nested_groups(
        Config, DummyUser1, <<"bgr1">>, <<"mgr1">>, <<"tgr1">>
    ),
    {BottomGroup2, MiddleGroup2, TopGroup2} = oz_test_utils:create_3_nested_groups(
        Config, DummyUser2, <<"bgr2">>, <<"mgr2">>, <<"tgr2">>
    ),
    % Add TopGroup2 to MiddleGroup1, now TopGroup1 has the following
    % effective groups:
    %   BottomGroup1,
    %   MiddleGroup1,
    %   BottomGroup2,
    %   MiddleGroup2,
    %   TopGroup2
    {ok, TopGroup2} = oz_test_utils:group_add_group(
        Config, MiddleGroup1, TopGroup2
    ),
    % Add the groups to spaces
    {ok, TopGroup1} = oz_test_utils:space_add_group(
        Config, Space1, TopGroup1
    ),
    {ok, SomeGroup} = oz_test_utils:space_add_group(
        Config, Space2, SomeGroup
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
    oz_test_utils:user_set_oz_privileges(Config, Admin, set, [?OZ_SET_PRIVILEGES]),
    % User will be used to test the functionality
    {ok, TestUser} = oz_test_utils:create_user(Config, #od_user{}),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to list groups of provider
    % as he does not yet have privs
    ?assert(check_list_eff_groups_of_provider(Config, 403, {user, TestUser}, Provider, undefined)),
    % As well as get data of those groups
    [
        ?assert(check_get_eff_group_of_provider_data(
            Config, 403, {user, TestUser}, Provider, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ],
    % Lets grant privileges to the user
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestUser, od_user,
        [?OZ_PROVIDERS_LIST_GROUPS])),
    % Now he should be able to list groups of provider
    ?assert(check_list_eff_groups_of_provider(Config, 200, {user, TestUser}, Provider,
        ExpectedGroupIds)),
    [
        ?assert(check_get_eff_group_of_provider_data(
            Config, 200, {user, TestUser}, Provider, GrId, GrName)
        ) || {GrId, GrName} <- ExpectedGroups
    ],
    % Revoke the privileges again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestUser, od_user, [])),
    % He should no longer be able to list groups of provider
    ?assert(check_list_eff_groups_of_provider(Config, 403, {user, TestUser}, Provider, undefined)),
    [
        ?assert(check_get_eff_group_of_provider_data(
            Config, 403, {user, TestUser}, Provider, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ],

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % list groups of provider.
    {ok, TestGroup} = oz_test_utils:create_group(Config, ?USER(TestUser), <<"gr">>),
    % The user should still not be able to list groups of provider as the group
    % does not have privileges.
    ?assert(check_list_eff_groups_of_provider(Config, 403, {user, TestUser}, Provider, undefined)),
    [
        ?assert(check_get_eff_group_of_provider_data(
            Config, 403, {user, TestUser}, Provider, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ],
    % But when we grant privileges to TestGroup, he should be able to
    % list groups of provider
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestGroup, od_group,
        [?OZ_PROVIDERS_LIST_GROUPS])),
    ?assert(check_list_eff_groups_of_provider(Config, 200, {user, TestUser}, Provider,
        ExpectedGroupIds)),
    [
        ?assert(check_get_eff_group_of_provider_data(
            Config, 200, {user, TestUser}, Provider, GrId, GrName)
        ) || {GrId, GrName} <- ExpectedGroups
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestGroup, od_group, [])),
    ?assert(check_list_eff_groups_of_provider(Config, 403, {user, TestUser}, Provider,
        undefined)),
    [
        ?assert(check_get_eff_group_of_provider_data(
            Config, 403, {user, TestUser}, Provider, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ],

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group, which belongs to a
    % group that has the privileges and check if he can list groups of provider.
    {_, _, TopGroup} = oz_test_utils:create_3_nested_groups(Config, TestUser),
    % The user should still not be able to list groups of provider as the group
    % does not have privileges.
    ?assert(check_list_eff_groups_of_provider(Config, 403, {user, TestUser}, Provider, undefined)),
    [
        ?assert(check_get_eff_group_of_provider_data(
            Config, 403, {user, TestUser}, Provider, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ],
    % But when we grant privileges to ParentGroup, he should be able to
    % list groups of provider
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TopGroup, od_group,
        [?OZ_PROVIDERS_LIST_GROUPS])),
    ?assert(check_list_eff_groups_of_provider(Config, 200, {user, TestUser}, Provider,
        ExpectedGroupIds)),
    [
        ?assert(check_get_eff_group_of_provider_data(
            Config, 200, {user, TestUser}, Provider, GrId, GrName)
        ) || {GrId, GrName} <- ExpectedGroups
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TopGroup, od_group, [])),
    ?assert(check_list_eff_groups_of_provider(Config, 403, {user, TestUser}, Provider,
        undefined)),
    [
        ?assert(check_get_eff_group_of_provider_data(
            Config, 403, {user, TestUser}, Provider, GrId, undefined)
        ) || {GrId, _GrName} <- ExpectedGroups
    ].


list_spaces_of_provider_test(Config) ->
    {ok, UserWithSpaces} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, {Provider, _, _}} = oz_test_utils:create_provider_and_certs(Config, <<"pr">>),
    {ok, Space1} = oz_test_utils:create_space(
        Config, ?USER(UserWithSpaces), <<"sp1">>
    ),
    {ok, Space2} = oz_test_utils:create_space(
        Config, ?USER(UserWithSpaces), <<"sp2">>
    ),
    {ok, Space3} = oz_test_utils:create_space(
        Config, ?USER(UserWithSpaces), <<"sp3">>
    ),
    {ok, _} = oz_test_utils:support_space(Config, Provider, Space1, 10000000),
    {ok, _} = oz_test_utils:support_space(Config, Provider, Space2, 10000000),
    {ok, _} = oz_test_utils:support_space(Config, Provider, Space3, 10000000),
    ExpectedSpaces = [
        {Space1, <<"sp1">>},
        {Space2, <<"sp2">>},
        {Space3, <<"sp3">>}
    ],
    {ExpectedSpaceIds, _} = lists:unzip(ExpectedSpaces),
    % Admin will be used to grant or revoke privileges
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, set, [?OZ_SET_PRIVILEGES]),
    % User will be used to test the functionality
    {ok, TestUser} = oz_test_utils:create_user(Config, #od_user{}),

    %% PRIVILEGES AS A USER

    % TestUser should not be able to list spaces of provider
    % as he does not yet have privs
    ?assert(check_list_spaces_of_provider(Config, 403, {user, TestUser}, Provider, undefined)),
    % As well as get data of those spaces
    [
        ?assert(check_get_space_of_provider_data(Config,
            403, {user, TestUser}, Provider, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ],
    % Lets grant privileges to the user
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestUser, od_user,
        [?OZ_PROVIDERS_LIST_SPACES])),
    % Now he should be able to list spaces of provider
    ?assert(check_list_spaces_of_provider(Config,
        200, {user, TestUser}, Provider, ExpectedSpaceIds
    )),
    [
        ?assert(check_get_space_of_provider_data(Config,
            200, {user, TestUser}, Provider, SpId, SpName)
        ) || {SpId, SpName} <- ExpectedSpaces
    ],
    % Revoke the privileges again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestUser, od_user, [])),
    % He should no longer be able to list spaces of provider
    ?assert(check_list_spaces_of_provider(Config, 403, {user, TestUser}, Provider, undefined)),
    [
        ?assert(check_get_space_of_provider_data(Config,
            403, {user, TestUser}, Provider, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ],

    %% PRIVILEGES VIA GROUP

    % Add the user to a group and give it privileges, see if the user can
    % list spaces of provider.
    {ok, TestGroup} = oz_test_utils:create_group(Config, ?USER(TestUser), <<"gr">>),
    % The user should still not be able to list spaces of provider as the group
    % does not have privileges.
    ?assert(check_list_spaces_of_provider(Config, 403, {user, TestUser}, Provider, undefined)),
    [
        ?assert(check_get_space_of_provider_data(Config,
            403, {user, TestUser}, Provider, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ],
    % But when we grant privileges to TestGroup, he should be able to
    % list spaces of provider
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestGroup, od_group,
        [?OZ_PROVIDERS_LIST_SPACES])),
    ?assert(check_list_spaces_of_provider(Config, 200, {user, TestUser}, Provider,
        ExpectedSpaceIds)),
    [
        ?assert(check_get_space_of_provider_data(Config,
            200, {user, TestUser}, Provider, SpId, SpName)
        ) || {SpId, SpName} <- ExpectedSpaces
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TestGroup, od_group, [])),
    ?assert(check_list_spaces_of_provider(Config, 403, {user, TestUser}, Provider,
        undefined)),
    [
        ?assert(check_get_space_of_provider_data(Config,
            403, {user, TestUser}, Provider, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ],

    %% PRIVILEGES VIA NESTED GROUPS

    % Add him to a nested group, which belongs to a group, which belongs to a
    % group that has the privileges and check if he can list spaces of provider.
    {_, _, TopGroup} = oz_test_utils:create_3_nested_groups(Config, TestUser),
    % The user should still not be able to list spaces of provider as the group
    % does not have privileges.
    ?assert(check_list_spaces_of_provider(Config, 403, {user, TestUser}, Provider, undefined)),
    [
        ?assert(check_get_space_of_provider_data(Config,
            403, {user, TestUser}, Provider, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ],
    % But when we grant privileges to ParentGroup, he should be able to
    % list spaces of provider
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TopGroup, od_group,
        [?OZ_PROVIDERS_LIST_SPACES])),
    ?assert(check_list_spaces_of_provider(Config, 200, {user, TestUser}, Provider,
        ExpectedSpaceIds)),
    [
        ?assert(check_get_space_of_provider_data(Config,
            200, {user, TestUser}, Provider, SpId, SpName)
        ) || {SpId, SpName} <- ExpectedSpaces
    ],
    % Revoke the privileges and check again
    ?assert(check_set_privileges(Config, 204, {user, Admin}, TopGroup, od_group, [])),
    ?assert(check_list_spaces_of_provider(Config, 403, {user, TestUser}, Provider,
        undefined)),
    [
        ?assert(check_get_space_of_provider_data(Config,
            403, {user, TestUser}, Provider, SpId, undefined)
        ) || {SpId, _SpName} <- ExpectedSpaces
    ].


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

end_per_testcase(_, Config) ->
    % Remove everything that was created during a testcase
    ok = oz_test_utils:delete_all_entities(Config).
