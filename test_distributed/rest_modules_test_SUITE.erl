%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Integration tests of rest_modules in Global Registry
%%% @end
%%%-------------------------------------------------------------------
-module(rest_modules_test_SUITE).
-author("Jakub Kudzia").

-include("registered_names.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("annotations/include/annotations.hrl").
-include("dao/dao_users.hrl").

-define(CONTENT_TYPE_HEADER,[{"content-type","application/json"}]).

%%  set example test data
-define(URLS1, [<<"127.0.0.1">>]).
-define(URLS2, [<<"127.0.0.2">>]).
-define(REDIRECTION_POINT1, <<"https://127.0.0.1:443">>).
-define(REDIRECTION_POINT2, <<"https://127.0.0.2:443">>).
-define(CLIENT_NAME1, <<"provider1">>).
-define(CLIENT_NAME2, <<"provider2">>).
-define(USER_NAME1, <<"user1">>).
-define(USER_NAME2, <<"user2">>).
-define(USER_NAME3, <<"user3">>).
-define(SPACE_NAME1, <<"space1">>).
-define(SPACE_NAME2, <<"space2">>).
-define(GROUP_NAME1, <<"group1">>).
-define(GROUP_NAME2, <<"group2">>).
-define(SPACE_SIZE1, <<"1024">>).
-define(SPACE_SIZE2, <<"4096">>).
-define(GROUP_PRIVILEGES,
    [
        group_view_data, group_change_data, group_invite_user,
        group_remove_user, group_join_space, group_create_space,
        group_set_privileges, group_remove, group_leave_space,
        group_create_space_token
    ]
).
-define(SPACE_PRIVILEGES,
    [
        space_view_data, space_change_data, space_invite_user,
        space_remove_user, space_invite_group, space_remove_group,
        space_set_privileges,space_remove, space_add_provider,
        space_remove_provider
    ]
).

%% API
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([provider_create_test/1, provider_update_test/1, provider_get_info_test/1,
    provider_delete_test/1, provider_create_space_test/1, provider_get_supported_space_info_test/1,
    provider_delete_supported_space_test/1, provider_check_port_test/1, provider_check_ip_test/1,
    support_space_test/1, user_authenticate_test/1, user_update_test/1, user_delete_test/1,
    user_merge_test/1, user_create_space_test/1, user_default_space_test/1, user_delete_space_test/1,
    user_get_space_data_test/1, user_space_invite_test/1, user_get_group_data_test/1,
    user_leave_group_test/1, user_group_invite_test/1, group_create_test/1, group_update_test/1,
    group_delete_test/1, user_create_group_test/1, group_invite_user_test/1,
    group_get_user_data_test/1, group_delete_user_test/1, group_get_privileges_test/1,
    group_set_privileges_test/1, group_create_space_test/1, group_get_space_data_test/1,
    group_delete_space_test/1, group_space_invitation_test/1, space_create_by_user_test/1,
    space_create_by_provider_test/1, space_update_test/1, space_delete_test/1,
    space_get_users_test/1, space_get_user_data_test/1,
    space_delete_user_test/1, space_get_groups_test/1, space_get_group_data_test/1,
    space_delete_group_test/1, space_get_providers_test/1, space_get_provider_data_test/1,
    space_delete_provider_test/1, space_get_privileges_test/1, space_set_privileges_test/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-performance({test_cases, []}).
all() ->
    [
        {group, provider_rest_module_test_group},
        {group, user_rest_module_test_group},
        {group, group_rest_module_test_group},
        {group, spaces_rest_module_test_group}
    ].

groups() ->
    [
        {
            provider_rest_module_test_group,
            [],
            [
                provider_create_test,
                provider_update_test,
                provider_get_info_test,
                provider_delete_test,
                provider_create_space_test,
                provider_get_supported_space_info_test,
                provider_delete_supported_space_test,
                provider_check_ip_test,
                provider_check_port_test,
                support_space_test
            ]
        },
        {
            user_rest_module_test_group,
            [],
            [
                user_authenticate_test,
                user_update_test,
                user_delete_test,
                user_merge_test,
                user_create_space_test,
                user_default_space_test,
                user_delete_space_test,
                user_get_space_data_test,
                user_space_invite_test,
                user_get_group_data_test,
                user_leave_group_test,
                user_group_invite_test
            ]
        },
        {
            group_rest_module_test_group,
            [],
            [
                group_create_test,
                group_update_test,
                group_delete_test,
                user_create_group_test,
                group_invite_user_test,
                group_get_user_data_test,
                group_delete_user_test,
                group_get_privileges_test,
                group_set_privileges_test,
                group_create_space_test,
                group_get_space_data_test,
                group_delete_space_test,
                group_space_invitation_test
            ]
        },
        {
            spaces_rest_module_test_group,
            [],
            [
                space_create_by_user_test,
                space_create_by_provider_test,
                space_update_test,
                space_delete_test,
                space_get_users_test,
                space_get_user_data_test,
                space_delete_user_test,
                space_get_groups_test,
                space_get_group_data_test,
                space_delete_group_test,
                space_get_providers_test,
                space_get_provider_data_test,
                space_delete_provider_test,
                space_get_privileges_test,
                space_set_privileges_test
            ]
        }
    ].

%%%===================================================================
%%% Test functions
%%%===================================================================

%% provider_rest_module_test_group====================================

provider_create_test(Config) ->
    RestAddress = ?config(restAddress, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},

    {ProviderId, ProviderReqParams} =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, Config, ReqParams),

    ?assertMatch(
        [?CLIENT_NAME1, ?URLS1, ?REDIRECTION_POINT1, ProviderId],
        get_provider_info(ProviderReqParams)
    ).

provider_update_test(Config) ->
    ProviderId = ?config(providerId, Config),
    ProviderReqParams = ?config(providerReqParams, Config),

    update_provider(?URLS2, ?REDIRECTION_POINT2, ?CLIENT_NAME2, ProviderReqParams),

    ?assertMatch(
        [?CLIENT_NAME2, ?URLS2, ?REDIRECTION_POINT2, ProviderId],
        get_provider_info(ProviderReqParams)
    ).

provider_get_info_test(Config) ->
    ProviderId = ?config(providerId, Config),
    ProviderReqParams = ?config(providerReqParams, Config),

    ?assertMatch(
        [?CLIENT_NAME1, ?URLS1, ?REDIRECTION_POINT1, ProviderId],
        get_provider_info(ProviderId, ProviderReqParams)
    ).

provider_delete_test(Config) ->
    ProviderReqParams = ?config(providerReqParams, Config),

    ?assertMatch(ok, check_status(delete_provider(ProviderReqParams))),
    ?assertMatch(request_error,get_provider_info(ProviderReqParams)).

provider_create_space_test(Config) ->
    ProviderReqParams = ?config(providerReqParams, Config),
    UserReqParams = ?config(userReqParams, Config),

    %% get space creation token1
    SCRToken1 = get_space_creation_token_for_user(UserReqParams),
    SID1 = create_and_support_space(SCRToken1, ?SPACE_NAME1, ?SPACE_SIZE1, ProviderReqParams),

    ?assertMatch([SID1], get_supported_spaces(ProviderReqParams)).

provider_get_supported_space_info_test(Config) ->
    ProviderId = ?config(providerId, Config),
    ProviderReqParams = ?config(providerReqParams, Config),
    UserReqParams = ?config(userReqParams, Config),

    %% get space creation token1
    SCRToken1 = get_space_creation_token_for_user(UserReqParams),
    SID = create_and_support_space(SCRToken1, ?SPACE_NAME1, ?SPACE_SIZE1, ProviderReqParams),

    %% assertMatch has problem with nested brackets below
    [SID_test, SpaceName_test, {[{ProviderId_test, SpaceSize_test}]}] =
        get_space_info_by_provider(SID, ProviderReqParams),

    ?assertMatch(
        [SID_test, SpaceName_test, ProviderId_test, SpaceSize_test],
        [SID, ?SPACE_NAME1, ProviderId, binary_to_integer(?SPACE_SIZE1)]
    ).

provider_delete_supported_space_test(Config) ->
    ProviderReqParams = ?config(providerReqParams, Config),
    UserReqParams = ?config(userReqParams, Config),

    %% get space creation token1
    SCRToken1 = get_space_creation_token_for_user(UserReqParams),
    SID = create_and_support_space(SCRToken1, ?SPACE_NAME1, ?SPACE_SIZE1, ProviderReqParams),

    ?assertMatch(ok, check_status(delete_space(SID, ProviderReqParams))).

support_space_test(Config) ->
    ProviderReqParams = ?config(providerReqParams, Config),
    UserReqParams = ?config(userReqParams, Config),

    SID = create_space_for_user(?SPACE_NAME1, UserReqParams),
    Token = get_space_support_token(SID, UserReqParams),

    ?assertMatch(ok,
        check_status(support_space(Token, ?SPACE_SIZE1, ProviderReqParams))),
    ?assertMatch(true, is_included([SID], get_supported_spaces(ProviderReqParams))).

provider_check_ip_test(Config) ->
    ProviderReqParams = ?config(providerReqParams, Config),
    ?assertMatch(ok, check_status(check_provider_ip(ProviderReqParams))).

provider_check_port_test(Config) ->
    ProviderReqParams = ?config(providerReqParams, Config),
    ?assertMatch(ok, check_status(check_provider_ports(ProviderReqParams))).

%% user_rest_module_test_group========================================

user_authenticate_test(Config) ->
    UserId = ?config(userId, Config),
    UserReqParams = ?config(userReqParams, Config),

    ?assertMatch([UserId, ?USER_NAME1], get_user_info(UserReqParams)).

user_update_test(Config) ->
    UserId = ?config(userId, Config),
    UserReqParams = ?config(userReqParams, Config),

    ?assertMatch(ok, check_status(update_user(?USER_NAME2, UserReqParams))),
    ?assertMatch([UserId, ?USER_NAME2], get_user_info(UserReqParams)).

user_delete_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    ?assertMatch(ok, check_status(delete_user(UserReqParams))),
    ?assertMatch(request_error, get_user_info(UserReqParams)).

user_merge_test(Config) ->
    ProviderId = ?config(providerId, Config),
    ProviderReqParams = ?config(providerReqParams, Config),
    UserReqParams1 = ?config(userReqParams, Config),

    {_UserId2, UserReqParams2} =
        register_user(?USER_NAME2, ProviderId, Config, ProviderReqParams),

    MergeToken = get_user_merge_token(UserReqParams2),

    ?assertMatch(ok, check_status(merge_users(MergeToken, UserReqParams1))).

user_create_space_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    SID1 = create_space_for_user(?SPACE_NAME1, UserReqParams),
    SID2 = create_space_for_user(?SPACE_NAME1, UserReqParams),

    ?assertMatch([[SID1, SID2], <<"undefined">>], get_user_spaces(UserReqParams)).

user_default_space_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    SID1 = create_space_for_user(?SPACE_NAME1, UserReqParams),

    ?assertMatch([[SID1], <<"undefined">>], get_user_spaces(UserReqParams)),
    ?assertMatch(ok, check_status(set_default_space_for_user(SID1, UserReqParams))),
    ?assertMatch([[SID1], SID1], get_user_spaces(UserReqParams)),
    ?assertMatch(SID1, get_user_default_space(UserReqParams)).

user_get_space_data_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    SID = create_space_for_user(?SPACE_NAME1, UserReqParams),
    ?assertMatch([SID, ?SPACE_NAME1], get_space_info_by_user(SID, UserReqParams)).

user_delete_space_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    SID1 = create_space_for_user(?SPACE_NAME1, UserReqParams),
    ?assertMatch(ok, check_status(delete_space_by_user(SID1, UserReqParams))),
    ?assertMatch([[],<<"undefined">>], get_user_spaces(UserReqParams)).

user_space_invite_test(Config) ->
    ProviderId = ?config(providerId, Config),
    ProviderReqParams = ?config(providerReqParams, Config),
    UserReqParams1 = ?config(userReqParams, Config),

    {_UserId2, UserReqParams2} =
        register_user(?USER_NAME2, ProviderId, Config, ProviderReqParams),
    SID1 = create_space_for_user(?SPACE_NAME1, UserReqParams1),
    InvitationToken = get_space_invitation_token(users, SID1, UserReqParams1),
    
    ?assertMatch(SID1, join_user_to_space(InvitationToken, UserReqParams2)),

    %% check if space is in list of user2 space
    ?assertMatch([[SID1], <<"undefined">>], get_user_spaces(UserReqParams2)).

user_create_group_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    GID1 = create_group_for_user(?GROUP_NAME1, UserReqParams),
    GID2 = create_group_for_user(?GROUP_NAME2, UserReqParams),

    ?assertMatch(true, is_included([GID1, GID2], get_user_groups(UserReqParams))).

user_get_group_data_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    GID1 = create_group_for_user(?GROUP_NAME1, UserReqParams),
    ?assertMatch([GID1, ?GROUP_NAME1], get_group_info_by_user(GID1, UserReqParams)).

user_leave_group_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    GID1 = create_group_for_user(?GROUP_NAME1, UserReqParams),

    ?assertMatch(ok, check_status(user_leave_group(GID1,UserReqParams))),
    ?assertMatch(false, is_included([GID1], get_user_groups(UserReqParams))).

user_group_invite_test(Config) ->
    ProviderId = ?config(providerId, Config),
    ProviderReqParams = ?config(providerReqParams, Config),
    UserId1 = ?config(userId, Config),
    UserReqParams1 = ?config(userReqParams, Config),

    {UserId2, UserReqParams2} =
        register_user(?USER_NAME2, ProviderId, Config, ProviderReqParams),

    GID1 = create_group_for_user(?GROUP_NAME1, UserReqParams1),

    InvitationToken = get_group_invitation_token(GID1, UserReqParams1),

    %% check if GID returned for user2 is the same as GID1
    ?assertMatch(GID1, join_user_to_group(InvitationToken, UserReqParams2)),
    ?assertMatch([GID1, ?GROUP_NAME1], get_group_info_by_user(GID1, UserReqParams2)),
    ?assertMatch(true, is_included([UserId1, UserId2], get_group_users(GID1, UserReqParams1))).

%% group_rest_module_test_group =======================================

group_create_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    GID = create_group(?GROUP_NAME1, UserReqParams),

    ?assertMatch([GID, ?GROUP_NAME1], get_group_info(GID, UserReqParams)).

group_update_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    GID = create_group(?GROUP_NAME1, UserReqParams),

    ?assertMatch(ok, check_status(update_group(GID, ?GROUP_NAME2, UserReqParams))),
    ?assertMatch([GID, ?GROUP_NAME2], get_group_info(GID, UserReqParams)).

group_delete_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    GID = create_group(?GROUP_NAME1, UserReqParams),

    ?assertMatch(ok, check_status(delete_group(GID, UserReqParams))),
    ?assertMatch(request_error, get_group_info(GID, UserReqParams)).

group_invite_user_test(Config) ->
    ProviderId = ?config(providerId, Config),
    ProviderReqParams = ?config(providerReqParams, Config),
    UserId1 = ?config(userId, Config),
    UserReqParams1 = ?config(userReqParams, Config),

    {UserId2, UserReqParams2} =
        register_user(?USER_NAME2, ProviderId, Config, ProviderReqParams),

    GID = create_group(?GROUP_NAME1, UserReqParams1),

    Token = get_group_invitation_token(GID, UserReqParams1),
    ?assertMatch(GID, join_user_to_group(Token, UserReqParams2)),
    ?assertMatch(true, is_included([UserId1, UserId2], get_group_users(GID, UserReqParams1))).

group_get_user_data_test(Config) ->
    UserId1 = ?config(userId, Config),
    UserReqParams1 = ?config(userReqParams, Config),

    GID = create_group(?GROUP_NAME1, UserReqParams1),

    ?assertMatch([UserId1, ?USER_NAME1], get_user_info_by_group(GID, UserId1, UserReqParams1)).

group_delete_user_test(Config) ->
    UserId1 = ?config(userId, Config),
    UserReqParams1 = ?config(userReqParams, Config),

    GID = create_group(?GROUP_NAME1, UserReqParams1),

    ?assertMatch(ok, check_status(delete_user_from_group(GID, UserId1, UserReqParams1))).

group_get_privileges_test(Config) ->
    ProviderId = ?config(providerId, Config),
    ProviderReqParams = ?config(providerReqParams, Config),
    UserId1 = ?config(userId, Config),
    UserReqParams1 = ?config(userReqParams, Config),

    {UserId2, UserReqParams2} =
        register_user(?USER_NAME2, ProviderId, Config, ProviderReqParams),

    GID = create_group(?GROUP_NAME1, UserReqParams1),

    InvitationToken = get_group_invitation_token(GID, UserReqParams1),

    %% add user to group
    join_user_to_group(InvitationToken, UserReqParams2),

    %% check user creator privileges
    ?assertMatch(true,
        is_included(
            [atom_to_binary(Privilege, latin1) || Privilege <- ?GROUP_PRIVILEGES],
            get_group_privileges_of_user(GID, UserId1, UserReqParams1))
    ),

    %% check other user privileges
    ?assertMatch(true,
        is_included(
            [<<"group_view_data">>], get_group_privileges_of_user(GID, UserId2, UserReqParams1))).

group_set_privileges_test(Config) ->
    ProviderId = ?config(providerId, Config),
    ProviderReqParams = ?config(providerReqParams, Config),
    UserId1 = ?config(userId, Config),
    UserReqParams1 = ?config(userReqParams, Config),

    {UserId2, UserReqParams2} =
        register_user(?USER_NAME2, ProviderId, Config, ProviderReqParams),
    {UserId3, UserReqParams3} =
        register_user(?USER_NAME3, ProviderId, Config, ProviderReqParams),

    GID = create_group(?GROUP_NAME1, UserReqParams1),

    InvitationToken = get_group_invitation_token(GID, UserReqParams1),

    %% add user to group
    join_user_to_group(InvitationToken, UserReqParams2),

    SID = create_space_for_user(?SPACE_NAME1, UserReqParams2),

    Users = [{UserId1, UserReqParams1}, {UserId2, UserReqParams2}, {UserId3, UserReqParams3}],
    group_privileges_check(?GROUP_PRIVILEGES, Users, GID, SID).

group_create_space_test(Config) ->
    UserReqParams1 = ?config(userReqParams, Config),

    GID = create_group(?GROUP_NAME1, UserReqParams1),
    SID1 = create_space_for_group(?SPACE_NAME1, GID, UserReqParams1),

    ?assertMatch([SID1], get_group_spaces(GID,UserReqParams1)).

group_get_space_data_test(Config) ->
    UserReqParams1 = ?config(userReqParams, Config),

    GID = create_group(?GROUP_NAME1, UserReqParams1),
    SID1 = create_space_for_group(?SPACE_NAME1, GID, UserReqParams1),

    ?assertMatch([SID1, ?SPACE_NAME1], get_space_info_by_group(GID, SID1, UserReqParams1)).

group_delete_space_test(Config) ->
    UserReqParams1 = ?config(userReqParams, Config),

    GID = create_group(?GROUP_NAME1, UserReqParams1),
    SID1 = create_space_for_group(?SPACE_NAME1, GID, UserReqParams1),

    ?assertMatch(ok, check_status(group_leave_space(GID, SID1, UserReqParams1))),
    ?assertMatch(false, is_included([SID1], get_group_spaces(GID, UserReqParams1))).

group_space_invitation_test(Config) ->
    UserReqParams1 = ?config(userReqParams, Config),

    GID = create_group(?GROUP_NAME1, UserReqParams1),
    SID1 = create_space_for_group(?SPACE_NAME1, GID, UserReqParams1),
    SID2 = create_space_for_user(?SPACE_NAME2, UserReqParams1),

    InvitationToken = get_space_invitation_token(groups, SID2, UserReqParams1),

    ?assertMatch(SID2, join_group_to_space(InvitationToken, GID, UserReqParams1)),
    ?assertMatch(true, is_included([SID1, SID2], get_group_spaces(GID, UserReqParams1))).

%% spaces_rest_module_test_group =======================================

space_create_by_user_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    SID = create_space(?SPACE_NAME1, UserReqParams),

    ?assertMatch([SID, ?SPACE_NAME1],get_space_info(SID, UserReqParams)).

space_create_by_provider_test(Config) ->
    ProviderReqParams = ?config(providerReqParams, Config),
    UserReqParams = ?config(userReqParams, Config),

    Token = get_space_creation_token_for_user(UserReqParams),
    SID = create_space(Token, ?SPACE_NAME1, ?SPACE_SIZE1, ProviderReqParams),

    ?assertMatch([SID, ?SPACE_NAME1],get_space_info(SID, ProviderReqParams)).

space_update_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    SID = create_space(?SPACE_NAME1, UserReqParams),

    ?assertMatch(ok, check_status(update_space(?SPACE_NAME2, SID, UserReqParams))),
    ?assertMatch([SID, ?SPACE_NAME2], get_space_info(SID, UserReqParams)).

space_delete_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    SID = create_space(?SPACE_NAME1, UserReqParams),
    ?assertMatch(ok, check_status(delete_space(SID, UserReqParams))).

space_get_users_test(Config) ->
    ProviderId = ?config(providerId, Config),
    ProviderReqParams = ?config(providerReqParams, Config),
    UserId1 = ?config(userId, Config),
    UserReqParams1 = ?config(userReqParams, Config),

    {UserId2, UserReqParams2} =
        register_user(?USER_NAME2, ProviderId, Config, ProviderReqParams),

    SID = create_space(?SPACE_NAME1, UserReqParams1),

    InvitationToken = get_space_invitation_token(users, SID, UserReqParams1),

    join_user_to_space(InvitationToken, UserReqParams2),
    ?assertMatch(true, is_included([UserId1, UserId2], get_space_users(SID, UserReqParams1))).

space_get_user_data_test(Config) ->
    UserId1 = ?config(userId, Config),
    UserReqParams1 = ?config(userReqParams, Config),

    SID = create_space(?SPACE_NAME1, UserReqParams1),
    
    ?assertMatch([UserId1, ?USER_NAME1], get_user_from_space(SID, UserId1, UserReqParams1)).

space_delete_user_test(Config) ->
    ProviderId = ?config(providerId, Config),
    ProviderReqParams = ?config(providerReqParams, Config),
    UserReqParams1 = ?config(userReqParams, Config),

    {UserId2, UserReqParams2} =
        register_user(?USER_NAME2, ProviderId, Config, ProviderReqParams),

    SID = create_space(?SPACE_NAME1, UserReqParams1),
    Token = get_space_invitation_token(users, SID, UserReqParams1),
    join_user_to_space(Token, UserReqParams2),
    
    ?assertMatch(ok, check_status(delete_user_from_space(SID, UserId2, UserReqParams1))),
    ?assertMatch(false, is_included([UserId2], get_space_users(SID, UserReqParams1))).

space_get_groups_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    GID1 = create_group(?GROUP_NAME1, UserReqParams),
    SID = create_space_for_group(?SPACE_NAME1, GID1, UserReqParams),

    ?assertMatch(true, is_included([GID1], get_space_groups(SID, UserReqParams))).

space_get_group_data_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    GID1 = create_group(?GROUP_NAME1, UserReqParams),
    SID = create_space_for_group(?SPACE_NAME1, GID1, UserReqParams),

    ?assertMatch([GID1, ?GROUP_NAME1], get_group_from_space(SID, GID1, UserReqParams)).

space_delete_group_test(Config) ->
    UserReqParams = ?config(userReqParams, Config),

    GID1 = create_group(?GROUP_NAME1, UserReqParams),
    SID = create_space_for_group(?SPACE_NAME1, GID1, UserReqParams),

    ?assertMatch(ok, check_status(delete_group_from_space(SID, GID1, UserReqParams))).

space_get_providers_test(Config) ->
    ProviderId = ?config(providerId, Config),
    ProviderReqParams = ?config(providerReqParams, Config),
    UserReqParams = ?config(userReqParams, Config),

    Token = get_space_creation_token_for_user(UserReqParams),
    SID = create_and_support_space(Token, ?SPACE_NAME1, ?SPACE_SIZE1, ProviderReqParams),

    ?assertMatch([ProviderId], get_space_providers(SID, UserReqParams)).

space_get_provider_data_test(Config) ->
    ProviderId = ?config(providerId, Config),
    ProviderReqParams = ?config(providerReqParams, Config),
    UserReqParams = ?config(userReqParams, Config),

    Token = get_space_creation_token_for_user(UserReqParams),
    SID = create_and_support_space(Token, ?SPACE_NAME1, ?SPACE_SIZE1, ProviderReqParams),

    ?assertMatch(
        [?CLIENT_NAME1, ProviderId, ?URLS1, ?REDIRECTION_POINT1],
        get_supporting_provider_info(SID, ProviderId, UserReqParams)
    ).

space_delete_provider_test(Config) ->
    ProviderId = ?config(providerId, Config),
    ProviderReqParams = ?config(providerReqParams, Config),
    UserReqParams = ?config(userReqParams, Config),

    Token = get_space_creation_token_for_user(UserReqParams),
    SID = create_and_support_space(Token, ?SPACE_NAME1, ?SPACE_SIZE1, ProviderReqParams),

    ?assertMatch(ok, check_status(delete_supporting_provider(SID, ProviderId, UserReqParams))),
    ?assertMatch([], get_space_providers(SID, UserReqParams)).

space_get_privileges_test(Config) ->
    ProviderId = ?config(providerId, Config),
    ProviderReqParams = ?config(providerReqParams, Config),
    UserId1 = ?config(userId, Config),
    UserReqParams1 = ?config(userReqParams, Config),

    {UserId2, UserReqParams2} =
        register_user(?USER_NAME2, ProviderId, Config, ProviderReqParams),

    SID = create_space_for_user(?SPACE_NAME1, UserReqParams1),
    InvitationToken = get_space_invitation_token(users, SID, UserReqParams1),
    join_user_to_space(InvitationToken, UserReqParams2),

    ?assertMatch(true,
        is_included(
            [atom_to_binary(Privilege, latin1) || Privilege <- ?SPACE_PRIVILEGES],
            get_space_privileges(users, SID, UserId1, UserReqParams1)
        )
    ),

    ?assertMatch(true,
        is_included([<<"space_view_data">>], get_space_privileges(users, SID, UserId2, UserReqParams1))).

space_set_privileges_test(Config) ->
    ProviderId = ?config(providerId, Config),
    ProviderReqParams = ?config(providerReqParams, Config),
    UserId1 = ?config(userId, Config),
    UserReqParams1 = ?config(userReqParams, Config),

    {UserId2, UserReqParams2} =
        register_user(?USER_NAME2, ProviderId, Config, ProviderReqParams),
    {UserId3, UserReqParams3} =
        register_user(?USER_NAME3, ProviderId, Config, ProviderReqParams),

    GID = create_group(?GROUP_NAME1, UserReqParams1),
    SID = create_space_for_user(?SPACE_NAME1, UserReqParams1),
    InvitationToken = get_space_invitation_token(users, SID, UserReqParams1),
    join_user_to_space(InvitationToken, UserReqParams2),

    Users = [{UserId1, UserReqParams1}, {UserId2, UserReqParams2}, {UserId3, UserReqParams3}],

    space_privileges_check(?SPACE_PRIVILEGES, Users, GID, SID).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    [Node] = ?config(gr_nodes, NewConfig),
    GR_IP = get_node_ip(Node),
    RestPort = get_rest_port(Node),
    RestAddress = "https://" ++ GR_IP ++ ":" ++ integer_to_list(RestPort),
    timer:sleep(10000), % TODO add nagios to GR and delete sleep
    [{restAddress, RestAddress} | NewConfig ].

init_per_testcase(provider_create_test, Config) ->
    init_per_testcase(non_register, Config);
init_per_testcase(provider_update_test, Config) ->
    init_per_testcase(register_only_provider, Config);
init_per_testcase(provider_get_info_test, Config) ->
    init_per_testcase(register_only_provider, Config);
init_per_testcase(provider_delete_test, Config) ->
    init_per_testcase(register_only_provider, Config);
init_per_testcase(provider_check_ip_test, Config) ->
    init_per_testcase(register_only_provider, Config);
init_per_testcase(provider_check_port_test, Config) ->
    init_per_testcase(register_only_provider, Config);
init_per_testcase(non_register, Config) ->
    ibrowse:start(),
    ssl:start(),
    RestAddress = RestAddress = ?config(restAddress, Config),
    [{cert_files, generate_cert_files()} | Config];
init_per_testcase(register_only_provider, Config) ->
%%     this init function is for tests
%%     than need registered provider
    NewConfig = init_per_testcase(non_register, Config),
    RestAddress = ?config(restAddress, NewConfig),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},
    {ProviderId, ProviderReqParams} =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, NewConfig, ReqParams),
    [
        {providerId, ProviderId},
        {providerReqParams, ProviderReqParams}
        | NewConfig
    ];
init_per_testcase(_Default, Config) ->
%%     this default init function is for tests
%%     than need registered provider and user
    NewConfig = init_per_testcase(register_only_provider, Config),
    ProviderId = ?config(providerId, NewConfig),
    ProviderReqParams = ?config(providerReqParams, NewConfig),
    {UserId, UserReqParams} =
        register_user(?USER_NAME1, ProviderId, NewConfig, ProviderReqParams),
    [
        {userId, UserId},
        {userReqParams, UserReqParams}
        | NewConfig
    ].

end_per_testcase(_, Config) ->
    ssl:stop(),
    ibrowse:stop(),
    {KeyFile, CSRFile, CertFile} = ?config(cert_files, Config),
    file:delete(KeyFile),
    file:delete(CSRFile),
    file:delete(CertFile).

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_included(_, []) -> false;
is_included([], _MainList) -> true;
is_included([H|T], MainList) ->
    case lists:member(H, MainList) of
        true -> is_included(T, MainList);
        _ -> false
    end.

get_rest_port(Node)->
    {ok, RestPort} = rpc:call(Node, application, get_env, [?APP_Name, rest_port]),
    RestPort.

%% returns ip (as a string) of given node
get_node_ip(Node) ->
    CMD = "docker inspect --format '{{ .NetworkSettings.IPAddress }}'" ++ " " ++ utils:get_host(Node),
    re:replace(os:cmd(CMD), "\\s+", "", [global,{return,list}]).

generate_cert_files() ->
    {MegaSec, Sec, MiliSec} = erlang:now(),
    Prefix = lists:foldl(fun(Int, Acc) ->
        Acc ++ integer_to_list(Int) end, "provider", [MegaSec, Sec, MiliSec]),
    KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
    CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
    CertFile = filename:join(?TEMP_DIR, Prefix ++ "_cert.pem"),
    os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
    os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
    {KeyFile, CSRFile, CertFile}.

get_response_status(Response) ->
    {ok, Status, _ResponseHeaders, _ResponseBody} = Response,
    Status.

get_response_headers(Response) ->
    {ok, _Status, ResponseHeaders, _ResponseBody} = Response,
    ResponseHeaders.

get_response_body(Response) ->
    {ok, _Status, _ResponseHeaders, ResponseBody} = Response,
    ResponseBody.

%% returns list of values from Response's body,
%% returned list is ordered accordingly to keys in Keylist
%% Keylist is list of atoms
get_body_val(KeyList, Response) ->
    case check_status(Response) of
        bad -> request_error;
        _ -> {JSONOutput} = jiffy:decode(get_response_body(Response)),
            [ proplists:get_value(atom_to_binary(Key,latin1), JSONOutput) || Key <-KeyList ]
    end.

get_header_val(Parameter, Response) ->
    case check_status(Response) of
        bad -> request_error;
        _ -> case lists:keysearch("location", 1, get_response_headers(Response)) of
                {value, {_HeaderType, HeaderValue}} -> parse_http_param(Parameter, HeaderValue);
                false -> parameter_not_in_header
            end
    end.

parse_http_param(Parameter, HeaderValue)->
    [_, ParamVal] = re:split(HeaderValue, "/" ++ Parameter ++ "/"),
    ParamVal.

check_status(Response) ->
    Status = list_to_integer(get_response_status(Response)),
    case (Status >= 200) and (Status < 300) of
        true -> ok;
        _ -> bad
    end.

%% returns list of values from responsebody
do_request(Endpoint, Headers, Method) ->
    do_request(Endpoint, Headers, Method, [], []).
do_request(Endpoint, Headers, Method, Body) ->
    do_request(Endpoint, Headers, Method, Body, []).
do_request(Endpoint, Headers, Method, Body, Options) ->
    ibrowse:send_req(Endpoint, Headers, Method, Body, Options).

get_macaroon_id(Token) ->
    {ok, Macaroon} = macaroon:deserialize(Token),
    {ok, [{_, Identifier}]} = macaroon:third_party_caveats(Macaroon),
    Identifier.

prepare_macaroons_headers(SerializedMacaroon, SerializedDischarges)->
    {ok, Macaroon} = macaroon:deserialize(SerializedMacaroon),
    BoundMacaroons = lists:map(
        fun(SrlzdDischMacaroon) ->
            {ok, DM} = macaroon:deserialize(SrlzdDischMacaroon),
            {ok, BDM} = macaroon:prepare_for_request(Macaroon, DM),
            {ok, SBDM} = macaroon:serialize(BDM),
            binary_to_list(SBDM)
        end, [list_to_binary(SerializedDischarges)]),
    [
        {"macaroon", binary_to_list(SerializedMacaroon)},
        {"discharge-macaroons", BoundMacaroons}
    ].

update_req_params(ReqParams, NewParam, headers) ->
    {RestAddress, Headers, Options} = ReqParams,
    {RestAddress, Headers ++ NewParam, Options};
update_req_params(ReqParams, NewParam, options) ->
    {RestAddress, Headers, Options} = ReqParams,
    {RestAddress, Headers, Options++ NewParam}.

%% Provider functions =====================================================

register_provider(URLS, RedirectionPoint, ClientName, Config, ReqParams) ->
    {RestAddress, Headers, _Options} = ReqParams,
    {KeyFile, CSRFile, CertFile} = ?config(cert_files, Config),
    {ok, CSR} =file:read_file(CSRFile),
    Body = jiffy:encode({[
        {urls,URLS},
        {csr, CSR},
        {redirectionPoint, RedirectionPoint},
        {clientName, ClientName}
    ]}),
    Response = do_request(RestAddress ++ "/provider", Headers, post, Body),
    %% save cert
    [Cert, ProviderId] = get_body_val([certificate, providerId], Response),
    file:write_file(CertFile, Cert),
    %% set request options for provider
    Options = [{ssl_options, [{keyfile, KeyFile}, {certfile, CertFile }]}],
    %% set request parametres for provider
    ProviderReqParams = update_req_params(ReqParams, Options, options),
    {ProviderId, ProviderReqParams}.

get_provider_info(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(RestAddress ++ "/provider", Headers, get, [], Options),
    get_body_val([clientName, urls, redirectionPoint, providerId], Response).

get_provider_info(ProviderId, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(RestAddress ++ "/provider/" ++ binary_to_list(ProviderId), Headers, get, [], Options),
    get_body_val([clientName, urls, redirectionPoint, providerId], Response).

update_provider(URLS, RedirectionPoint, ClientName, ReqParams) ->
    Body = jiffy:encode({[
        {urls,URLS},
        {redirectionPoint, RedirectionPoint},
        {clientName, ClientName}
    ]}),
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/provider", Headers, patch, Body, Options).

delete_provider(ReqParams) ->
    {RestAddress, _Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/provider", [], delete,[], Options).

create_and_support_space(Token, SpaceName, Size, ReqParams) ->
  {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {name, SpaceName},
        {token, Token},
        {size, Size}
    ]}),
    Response = do_request(RestAddress ++ "/provider/spaces", Headers, post, Body, Options),
    get_header_val("spaces", Response).

get_supported_spaces(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(RestAddress ++ "/provider/spaces", Headers, get, [], Options),
    [Spaces] = get_body_val([spaces],Response),
    Spaces.

get_space_info_by_provider(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(RestAddress ++ "/provider/spaces/" ++ binary_to_list(SID), Headers, get, [], Options),
    get_body_val([spaceId, name, size],Response).

delete_space(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/provider/spaces/" ++ binary_to_list(SID), Headers, delete, [], Options).

check_provider_ip(ReqParams)->
    {RestAddress, _, _} = ReqParams,
    do_request(RestAddress ++ "/provider/test/check_my_ip", [], get).

check_provider_ports(ReqParams)->
    {RestAddress, _, _} = ReqParams,
    do_request(RestAddress ++ "/provider/test/check_my_ports", [], post).

support_space(Token, Size, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {token, Token}, 
        {size, Size}
    ]}),
    do_request(RestAddress ++ "/provider/spaces/support", Headers, post, Body, Options).

%% User functions =========================================================

create_user(UserName, Node) ->
    {ok, UserId} = rpc:call(Node, user_logic, create, [#user{name = UserName}]),
    UserId.

%% returns macaroons headers
auth_user(UserId, ProviderId, ReqParams, Node) ->
    SerializedMacaroon = rpc:call(Node, auth_logic, gen_token, [UserId, ProviderId]),
    {RestAddress, Headers, _Options} = ReqParams,
    Identifier = get_macaroon_id(SerializedMacaroon),
    Body = jiffy:encode({[{identifier, Identifier}]}),
    Resp = do_request(RestAddress ++ "/user/authorize", Headers, post, Body),
    {ok, _, _, SerializedDischarges} = Resp,
    prepare_macaroons_headers(SerializedMacaroon, SerializedDischarges).

register_user(UserName, ProviderId, Config, ProviderReqParams) ->
    [Node] = ?config(gr_nodes, Config),
    UserId = create_user(UserName, Node),
    NewHeaders = auth_user(UserId, ProviderId, ProviderReqParams, Node),
    UserReqParams = update_req_params(ProviderReqParams, NewHeaders, headers),
    {UserId, UserReqParams}.

get_user_info(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(RestAddress ++ "/user", Headers, get,[], Options),
    get_body_val([userId, name], Response).

update_user(NewUserName, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {name, NewUserName}
    ]}),
    do_request(RestAddress ++ "/user", Headers, patch, Body, Options).

delete_user(ReqParams) ->
     {RestAddress, Headers, Options} = ReqParams,
     do_request(RestAddress ++ "/user", Headers, delete, [], Options).

get_user_spaces(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(RestAddress ++ "/user/spaces", Headers, get, [], Options),
    get_body_val([spaces, default], Response).

get_space_info_by_user(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(RestAddress ++ "/user/spaces/" ++ binary_to_list(SID), Headers, get, [], Options),
    get_body_val([spaceId, name], Response).

get_user_default_space(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(RestAddress ++ "/user/spaces/default", Headers, get, [], Options),
    [DefSpace] = get_body_val([spaceId], Response),
    DefSpace.

create_space_for_user(SpaceName, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {name, SpaceName}
    ]}),
    Response = do_request(RestAddress ++ "/user/spaces", Headers, post, Body, Options),
    get_header_val("spaces", Response).

set_default_space_for_user(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {spaceId, SID}
    ]}),
    do_request(RestAddress ++ "/user/spaces/default", Headers, put, Body, Options).

get_user_merge_token(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(RestAddress ++ "/user/merge/token", Headers, get, [], Options),
    [Token] = get_body_val([token], Response),
    Token.

merge_users(Token, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
     Body = jiffy:encode({[
        {token, Token}
    ]}),
    do_request(RestAddress ++ "/user/merge", Headers, post, Body, Options).

delete_space_by_user(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/user/spaces/" ++ binary_to_list(SID), Headers, delete, [], Options).

join_user_to_space(Token, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {token, Token}
    ]}),
    Response = do_request(RestAddress ++ "/user/spaces/join", Headers, post, Body, Options),
    get_header_val("user/spaces",Response).
    
create_group_for_user(GroupName, ReqParams)->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {name, GroupName}
    ]}),
    Response = do_request(RestAddress ++ "/user/groups", Headers, post, Body, Options),
    get_header_val("groups", Response).

get_user_groups(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(RestAddress ++ "/user/groups", Headers, get, [], Options),
    [Groups] = get_body_val([groups], Response),
    Groups.

get_group_info_by_user(GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(RestAddress ++ "/user/groups/" ++ binary_to_list(GID) , Headers, get,[], Options),
    get_body_val([groupId, name], Response).

user_leave_group(GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/user/groups/" ++ binary_to_list(GID), Headers, delete, [], Options).

join_user_to_group(Token, ReqParams) ->
     {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {token, Token}
    ]}),
    Response = do_request(RestAddress ++ "/user/groups/join", Headers, post, Body, Options),
    get_header_val("user/groups",Response).

%% Group functions ==============================================================

create_group(GroupName, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {name, GroupName}
    ]}),
    Response = do_request(RestAddress ++ "/groups", Headers, post, Body, Options),
    get_header_val("groups", Response).

get_group_info(GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(RestAddress ++ "/groups/" ++ binary_to_list(GID) , Headers, get,[], Options),
    get_body_val([groupId, name], Response).

update_group(GID, NewGroupName, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
     Body = jiffy:encode({[
        {name, NewGroupName}
    ]}),
    do_request(
        RestAddress ++ "/groups/" ++ binary_to_list(GID) , Headers, patch, Body, Options
    ).

delete_group(GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/groups/" ++ binary_to_list(GID) , Headers, delete,[], Options).

get_group_invitation_token(GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/groups/" ++ binary_to_list(GID)++ "/users/token", Headers, get, [], Options
        ),
    [Token] = get_body_val([token], Response),
    Token.

get_group_users(GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/groups/" ++ binary_to_list(GID)++ "/users", Headers, get, [], Options
        ),
    [Users] = get_body_val([users], Response),
    Users.

get_user_info_by_group(GID, UID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/groups/" ++ binary_to_list(GID) ++ "/users/" ++ binary_to_list(UID),
            Headers, get, [], Options
        ),
    get_body_val([userId, name], Response).

delete_user_from_group(GID, UID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(
        RestAddress ++ "/groups/" ++ binary_to_list(GID) ++ "/users/" ++ binary_to_list(UID),
        Headers, delete, [], Options
    ).

get_group_privileges_of_user(GID, UID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/groups/" ++ binary_to_list(GID) ++ "/users/" ++
                binary_to_list(UID) ++ "/privileges",
            Headers, get, [], Options
        ),
    [Privileges] = get_body_val([privileges], Response),
    Privileges.

set_group_privileges_of_user(GID, UID, Privileges, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {privileges, Privileges}
    ]}),
    do_request(
        RestAddress ++ "/groups/" ++ binary_to_list(GID) ++ "/users/" ++
            binary_to_list(UID) ++ "/privileges",
        Headers, put, Body, Options
    ).

get_group_spaces(GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/groups/" ++ binary_to_list(GID)++ "/spaces", Headers, get, [], Options
        ),
    [Spaces] = get_body_val([spaces], Response),
    Spaces.

get_space_info_by_group(GID, SID, ReqParams) ->
   {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/groups/" ++ binary_to_list(GID) ++ "/spaces/" ++ binary_to_list(SID),
            Headers, get, [], Options),
    get_body_val([spaceId, name], Response).

create_space_for_group(Name, GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {name, Name}
    ]}),
    Response = do_request(
        RestAddress ++ "/groups/" ++ binary_to_list(GID)++ "/spaces", Headers, post, Body, Options
    ),
    get_header_val("spaces", Response).

group_leave_space(GID, SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(
        RestAddress ++ "/groups/" ++ binary_to_list(GID)++ "/spaces/" ++ binary_to_list(SID),
        Headers, delete, [], Options
    ).

join_group_to_space(Token, GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {token, Token}
    ]}),
    Response =
        do_request(
            RestAddress ++ "/groups/" ++ binary_to_list(GID)++ "/spaces/join",
            Headers, post, Body, Options
        ),
    get_header_val("groups/" ++ binary_to_list(GID) ++"/spaces", Response).

group_privileges_check([], _, _, _) -> ok;
group_privileges_check([FirstPrivilege | Privileges], Users, GID, _SID) ->
    group_privilege_check(FirstPrivilege, Users, GID, _SID),
    group_privileges_check(Privileges, Users, GID, _SID).

group_privilege_check(group_view_data, Users, GID, _SID) ->
    [{_UserId1, _UserReqParams1}, {_UserId2, UserReqParams2} | _] = Users,
    %% user who belongs to group should have group_view_data privilege by default
    ?assertMatch([GID, ?GROUP_NAME1], get_group_info(GID, UserReqParams2));
group_privilege_check(group_change_data, Users, GID, _SID) ->
    [{_UserId1, UserReqParams1}, {UserId2, UserReqParams2} | _] = Users,
    %% test if user2 lacks group_change_data privileges
    ?assertMatch(bad, check_status(update_group(GID, ?GROUP_NAME2, UserReqParams2))),
    set_group_privileges_of_user(GID, UserId2, [group_change_data], UserReqParams1),
    ?assertMatch(ok, check_status(update_group(GID, ?GROUP_NAME2, UserReqParams2))),
    clean_group_privileges(GID, UserId2, UserReqParams1);
group_privilege_check(group_invite_user, Users, GID, _SID) ->
    [{_UserId1, UserReqParams1}, {UserId2, UserReqParams2} | _] = Users,
    %% test if user2 lacks group_invite_user privileges
    ?assertMatch(bad, check_status(get_group_invitation_token(GID, UserReqParams2))),
    set_group_privileges_of_user(GID, UserId2, [group_invite_user], UserReqParams1),
    ?assertNotMatch(bad, check_status(get_group_invitation_token(GID, UserReqParams2))),
    clean_group_privileges(GID, UserId2, UserReqParams1);
group_privilege_check(group_set_privileges, Users, GID, _SID) ->
    [{UserId1, UserReqParams1}, {UserId2, UserReqParams2} | _] = Users,
     %% test if user2 lacks group_set_privileges privileges
    ?assertMatch(bad,
        check_status(set_group_privileges_of_user(GID, UserId1, ?GROUP_PRIVILEGES, UserReqParams2))
    ),
    set_group_privileges_of_user(GID, UserId2, [group_set_privileges], UserReqParams1),
    ?assertMatch(ok,
        check_status(set_group_privileges_of_user(GID, UserId1, ?GROUP_PRIVILEGES, UserReqParams2))
    ),
    clean_group_privileges(GID, UserId2, UserReqParams1);
group_privilege_check(group_join_space, Users, GID, SID) ->
    [{_UserId1, UserReqParams1}, {UserId2, UserReqParams2} | _] = Users,
    InvitationToken = get_space_invitation_token(groups, SID, UserReqParams2),
     %% test if user2 lacks group_join_space privileges
    ?assertMatch(bad, check_status(join_group_to_space(InvitationToken, GID, UserReqParams2))),
    set_group_privileges_of_user(GID, UserId2, [group_join_space], UserReqParams1),
    ?assertMatch(SID, check_status(join_group_to_space(InvitationToken, GID, UserReqParams2))),
    clean_group_privileges(GID, UserId2, UserReqParams1);
group_privilege_check(group_leave_space, Users, GID, SID) ->
    [{_UserId1, UserReqParams1}, {UserId2, UserReqParams2} | _] = Users,
    InvitationToken = get_space_invitation_token(groups, SID, UserReqParams1),
    join_group_to_space(InvitationToken, GID, UserReqParams1),
    %% test if user2 lacks group_leave_space privileges
    ?assertMatch(bad, check_status(group_leave_space(GID, SID, UserReqParams2))),
    set_group_privileges_of_user(GID, UserId2, [group_leave_space], UserReqParams1),
    ?assertMatch(ok, check_status(group_leave_space(GID, SID, UserReqParams2))),
    clean_group_privileges(GID, UserId2, UserReqParams1);
group_privilege_check(group_create_space_token, Users, GID, _SID) ->
    [{_UserId1, UserReqParams1}, {UserId2, UserReqParams2} | _] = Users,
    %% test if user2 lacks group_create_space_token privileges
    ?assertMatch(bad, check_status(get_space_creation_token_for_group(GID, UserReqParams2))),
    set_group_privileges_of_user(GID, UserId2, [group_create_space_token], UserReqParams1),
    ?assertNotMatch(bad, check_status(get_space_creation_token_for_group(GID, UserReqParams2))),
    clean_group_privileges(GID, UserId2, UserReqParams1);
group_privilege_check(group_create_space, Users, GID, _SID) ->
    [{_UserId1, UserReqParams1}, {UserId2, UserReqParams2} | _] = Users,
    %% test if user2 lacks group_create_space_token privileges
    ?assertMatch(bad, check_status(create_space_for_group(?SPACE_NAME2, GID, UserReqParams2))),
    set_group_privileges_of_user(GID, UserId2, [group_create_space], UserReqParams1),
    ?assertNotMatch(bad, check_status(create_space_for_group(?SPACE_NAME2, GID, UserReqParams2))),
    clean_group_privileges(GID, UserId2, UserReqParams1);
group_privilege_check(group_remove_user, Users, GID, _SID) ->
    [{_UserId1, UserReqParams1}, {UserId2, UserReqParams2} , {UserId3, UserReqParams3}| _] = Users,
    Token = get_group_invitation_token(GID, UserReqParams1),
    join_user_to_group(Token, UserReqParams3),
    %% test if user2 lacks group_remove_user privilege
    ?assertMatch(bad, check_status(delete_user_from_group(GID, UserId3, UserReqParams2))),
    set_group_privileges_of_user(GID, UserId2, [group_remove_user], UserReqParams1),
    ?assertMatch(ok, check_status(delete_user_from_group(GID, UserId3, UserReqParams2)));
group_privilege_check(group_remove, Users, GID, _SID) ->
    [{_UserId1, UserReqParams1}, {UserId2, UserReqParams2} | _] = Users,
    %% test if user2 lacks group_remove privileges
    ?assertMatch(bad, check_status(delete_group(GID, UserReqParams2))),
    set_group_privileges_of_user(GID, UserId2, [group_remove], UserReqParams1),
    ?assertMatch(ok, check_status(delete_group(GID, UserReqParams2))).

clean_group_privileges(GID, UserId, ReqParams) ->
    set_group_privileges_of_user(GID, UserId, [group_view_data], ReqParams).

%% Spaces functions ===========================================================

%% create space for user
create_space(Name, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {name, Name}
    ]}),
    Response = do_request(RestAddress ++ "/spaces", Headers, post, Body, Options),
    get_header_val("spaces", Response).

%% create space for user/group who delivers token
create_space(Token, Name, Size, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {name, Name},
        {token, Token},
        {size, Size}
    ]}),
    Response = do_request(RestAddress ++ "/spaces", Headers, post, Body, Options),
    get_header_val("spaces", Response).

get_space_info(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(RestAddress ++ "/spaces/" ++ binary_to_list(SID) , Headers, get,[], Options),
    get_body_val([spaceId, name], Response).

update_space(Name, SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
     Body = jiffy:encode({[
        {name, Name}
    ]}),
    do_request(RestAddress ++ "/spaces/" ++ binary_to_list(SID) , Headers, patch, Body, Options).

delete_space(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/spaces/" ++ binary_to_list(SID) , Headers, delete, [], Options).

get_space_users(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/spaces/" ++ binary_to_list(SID) ++ "/users" , Headers, get, [], Options
        ),
    [Users] = get_body_val([users], Response),
    Users.

get_user_from_space(SID, UID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/spaces/" ++ binary_to_list(SID) ++ "/users/" ++ binary_to_list(UID),
            Headers, get, [], Options
        ),
    get_body_val([userId, name], Response).

delete_user_from_space(SID, UID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(
        RestAddress ++ "/spaces/" ++ binary_to_list(SID) ++ "/users/" ++ binary_to_list(UID),
        Headers, delete, [], Options
    ).

get_space_privileges(UserType, SID, ID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/spaces/" ++ binary_to_list(SID) ++ "/" ++ atom_to_list(UserType) ++ "/"
                ++ binary_to_list(ID) ++ "/privileges",
            Headers, get, [], Options
        ),
    [Priveleges] = get_body_val([privileges], Response),
    Priveleges.

set_space_privileges(UserType, SID, ID, Privileges, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {privileges, Privileges}
    ]}),
    do_request(
        RestAddress ++ "/spaces/" ++ binary_to_list(SID) ++ "/" ++ atom_to_list(UserType) ++ "/" ++
            binary_to_list(ID) ++ "/privileges",
        Headers, put, Body, Options
    ).

get_space_groups(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/spaces/" ++ binary_to_list(SID) ++ "/groups" , Headers, get, [], Options
        ),
    [Groups] = get_body_val([groups], Response),
    Groups.

get_group_from_space(SID, GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/spaces/" ++ binary_to_list(SID) ++ "/groups/" ++ binary_to_list(GID),
            Headers, get, [], Options
        ),
    get_body_val([groupId, name],Response).

delete_group_from_space(SID, GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(
        RestAddress ++ "/spaces/" ++ binary_to_list(SID) ++ "/groups/" ++ binary_to_list(GID),
        Headers, delete, [], Options
    ).

get_space_providers(SID, ReqParams) ->
{RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/spaces/" ++ binary_to_list(SID) ++ "/providers",
            Headers, get, [], Options
        ),
    [Providers] = get_body_val([providers], Response),
    Providers.

get_supporting_provider_info(SID, PID, ReqParams) ->
{RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/spaces/" ++ binary_to_list(SID) ++ "/providers/" ++ binary_to_list(PID),
            Headers, get, [], Options
        ),
    get_body_val([clientName, providerId, urls, redirectionPoint], Response).

delete_supporting_provider(SID, PID, ReqParams) ->
{RestAddress, Headers, Options} = ReqParams,
    do_request(
        RestAddress ++ "/spaces/" ++ binary_to_list(SID) ++ "/providers/" ++ binary_to_list(PID),
        Headers, delete, [], Options
    ).

get_space_creation_token_for_user(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(RestAddress ++ "/user/spaces/token", Headers, get, [], Options),
    [Token] = get_body_val([token], Response),
    Token.

get_space_creation_token_for_group(GID, ReqParams)->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(
        RestAddress ++ "/groups/"++ binary_to_list(GID) ++ "/spaces/token", Headers, get, [], Options
    ),
    [Token] = get_body_val([token], Response),
    Token.

get_space_invitation_token(UserType, ID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/spaces/" ++ binary_to_list(ID)++ "/" ++ atom_to_list(UserType)++"/token",
            Headers, get, [], Options
        ),
    [Token] = get_body_val([token], Response),
    Token.

get_space_support_token(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(
        RestAddress ++ "/spaces/" ++ binary_to_list(SID) ++"/providers/token",
        Headers, get, [], Options
    ),
    [Token] = get_body_val([token], Response),
    Token.

space_privileges_check([], _, _, _) -> ok;
space_privileges_check([FirstPrivilege | Privileges], Users, GID, _SID) ->
    space_privilege_check(FirstPrivilege, Users, GID, _SID),
    space_privileges_check(Privileges, Users, GID, _SID).

space_privilege_check(space_view_data, Users, _GID, SID) ->
    [{_UserId1, _UserReqParams1}, {_UserId2, UserReqParams2} | _] = Users,
    %% user who belongs to group should have space_view_data privilege by default
    ?assertMatch([SID, ?SPACE_NAME1], get_space_info(SID, UserReqParams2));
space_privilege_check(space_change_data, Users, _GID, SID) ->
    [{_UserId1, UserReqParams1}, {UserId2, UserReqParams2} | _] = Users,
    %% test if user2 lacks space_change_data privileges
    ?assertMatch(bad, check_status(update_space(SID, ?SPACE_NAME2, UserReqParams2))),
    set_space_privileges(users, SID, UserId2, [space_change_data], UserReqParams1),
    ?assertMatch(ok, check_status(update_space(SID, ?SPACE_NAME2, UserReqParams2))),
    clean_space_privileges(SID, UserId2, UserReqParams1);
space_privilege_check(space_invite_user, Users, _GID, SID) ->
    [{_UserId1, UserReqParams1}, {UserId2, UserReqParams2} | _] = Users,
    %% test if user2 lacks space_invite_user privileges
    ?assertMatch(bad, check_status(get_space_invitation_token(users, SID, UserReqParams2))),
    set_space_privileges(users, SID, UserId2, [space_invite_user], UserReqParams1),
    ?assertNotMatch(bad, check_status(get_space_invitation_token(users, SID, UserReqParams2))),
    clean_space_privileges(SID, UserId2, UserReqParams1);
space_privilege_check(space_invite_group, Users, _GID, SID) ->
    [{_UserId1, UserReqParams1}, {UserId2, UserReqParams2} | _] = Users,
    %% test if user2 lacks space_invite_user privileges
    ?assertMatch(bad, check_status(get_space_invitation_token(group, SID, UserReqParams2))),
    set_space_privileges(users, SID, UserId2, [space_invite_group], UserReqParams1),
    ?assertNotMatch(bad, check_status(get_space_invitation_token(group, SID, UserReqParams2))),
    clean_space_privileges(SID, UserId2, UserReqParams1);
space_privilege_check(space_set_privileges, Users, _GID, SID) ->
    [{UserId1, UserReqParams1}, {UserId2, UserReqParams2} | _] = Users,
     %% test if user2 lacks space_set_privileges privileges
    ?assertMatch(bad,
        check_status(set_space_privileges(users, SID, UserId1, ?SPACE_PRIVILEGES, UserReqParams2))
    ),
    set_space_privileges(users, SID, UserId2, [space_set_privileges], UserReqParams1),
    ?assertMatch(ok,
        check_status(set_space_privileges(users, SID, UserId1, ?SPACE_PRIVILEGES, UserReqParams2))
    ),
    clean_space_privileges(SID, UserId2, UserReqParams1);
space_privilege_check(space_remove_user, Users, _GID, SID) ->
    [{_UserId1, UserReqParams1}, {UserId2, UserReqParams2}, {UserId3, UserReqParams3} | _] = Users,
    InvitationToken = get_space_invitation_token(users, SID, UserReqParams3),
    join_user_to_space(InvitationToken, UserReqParams3),
     %% test if user2 lacks space_remove_user privileges
    ?assertMatch(bad, check_status(delete_user_from_space(SID, UserId3, UserReqParams2))),
    set_space_privileges(users, SID, UserId2, [space_remove_user], UserReqParams1),
    ?assertMatch(ok,
        check_status(delete_user_from_space(SID, UserId3, UserReqParams2))
    ),
    clean_space_privileges(SID, UserId2, UserReqParams1);
space_privilege_check(space_remove_group, Users, GID, SID) ->
    [{_UserId1, UserReqParams1}, {UserId2, UserReqParams2} | _] = Users,
    InvitationToken = get_space_invitation_token(groups, SID, UserReqParams2),
    join_group_to_space(InvitationToken, GID, UserReqParams1),
     %% test if user2 lacks space_remove_group privileges
    ?assertMatch(bad,
        check_status(delete_group_from_space(SID, GID, UserReqParams2))
    ),
    set_space_privileges(users, SID, UserId2, [space_remove_group], UserReqParams1),
    ?assertMatch(ok,
        check_status(delete_group_from_space(SID, GID, UserReqParams2))
    ),
    clean_space_privileges(SID, UserId2, UserReqParams1);
space_privilege_check(space_add_provider, Users, _GID, SID) ->
    [{_UserId1, UserReqParams1}, {UserId2, UserReqParams2} | _] = Users,
     %% test if user2 lacks space_add_provider privileges
    ?assertMatch(bad, check_status(get_space_support_token(SID, UserReqParams2))),
    set_space_privileges(users, SID, UserId2, [space_add_provider], UserReqParams1),
    ?assertNotMatch(bad, check_status(get_space_support_token(SID, UserReqParams2))),
    clean_space_privileges(SID, UserId2, UserReqParams1);
space_privilege_check(space_remove_provider, Users, _GID, SID) ->
    [{_UserId1, UserReqParams1}, {UserId2, UserReqParams2} | _] = Users,
     %% test if user2 lacks space_remove_provider privileges
    [PID] = get_space_providers(SID, UserReqParams1),
    ?assertMatch(bad, check_status(delete_supporting_provider(SID, PID, UserReqParams2))),
    set_space_privileges(users, SID, UserId2, [space_remove_provider], UserReqParams1),
    ?assertNotMatch(bad, check_status(delete_supporting_provider(SID, PID, UserReqParams2))),
    clean_space_privileges(SID, UserId2, UserReqParams1);
space_privilege_check(space_remove, Users, _GID, SID) ->
    [{_UserId1, UserReqParams1}, {UserId2, UserReqParams2} | _] = Users,
    %% test if user2 lacks space_remove privileges
    ?assertMatch(bad, check_status(delete_group(SID, UserReqParams2))),
    set_space_privileges(users, SID, UserId2, [space_remove], UserReqParams1),
    ?assertMatch(ok, check_status(delete_group(SID, UserReqParams2))).

clean_space_privileges(SID, UserId, ReqParams) ->
    set_space_privileges(users, SID, UserId, [space_view_data], ReqParams).