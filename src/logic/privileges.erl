%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The module describing user privileges in group and Space.
%% ===================================================================
-module(privileges).
-author("Konrad Zemek").


-export([space_user/0, space_manager/0, space_admin/0]).
-export([group_user/0, group_manager/0, group_admin/0]).
-export([space_privileges/0, group_privileges/0]).
-export_type([space_privilege/0, group_privilege/0]).


%% User privileges with regards to Space management.
-type space_privilege() :: space_invite_user | space_remove_user |
    space_invite_group | space_remove_group | space_set_privileges |
    space_remove | space_add_provider | space_remove_provider |
    space_change_data | space_view_data.


%% User privileges with regards to group management.
-type group_privilege() :: group_change_data | group_invite_user |
    group_remove_user | group_join_space | group_create_space |
    group_set_privileges | group_remove | group_leave_space |
    group_view_data | group_create_space_token.


%% space_user/0
%% ====================================================================
%% @doc A privilege level of a Space user.
%% ====================================================================
-spec space_user() -> [space_privilege()].
%% ====================================================================
space_user() ->
    [space_view_data].


%% space_manager/0
%% ====================================================================
%% @doc A privilege level of a Space manager.
%% ====================================================================
-spec space_manager() -> [space_privilege()].
%% ====================================================================
space_manager() ->
    ordsets:union(
        space_user(),
        ordsets:from_list([
            space_invite_user,
            space_remove_user,
            space_invite_group,
            space_remove_group
        ])
    ).


%% space_admin/0
%% ====================================================================
%% @doc A privilege level of a Space administrator. This level contains all
%% atoms representing space privileges.
%% @end
%% ====================================================================
-spec space_admin() -> [space_privilege()].
%% ====================================================================
space_admin() ->
    ordsets:union(
        space_manager(),
        ordsets:from_list([
            space_add_provider,
            space_remove_provider,
            space_set_privileges,
            space_change_data,
            space_remove
        ])
    ).


%% group_user/0
%% ====================================================================
%% @doc A privilege level of a group user.
%% ====================================================================
-spec group_user() -> [group_privilege()].
%% ====================================================================
group_user() ->
    [group_view_data].


%% group_manager/0
%% ====================================================================
%% @doc A privilege level of a group manager.
%% ====================================================================
-spec group_manager() -> [group_privilege()].
%% ====================================================================
group_manager() ->
    ordsets:union(
        group_user(),
        ordsets:from_list([
            group_invite_user,
            group_remove_user
        ])
    ).


%% group_admin/0
%% ====================================================================
%% @doc A privilege level of a group administrator. This level contains all
%% atoms representing group privileges.
%% @end
%% ====================================================================
-spec group_admin() -> [group_privilege()].
%% ====================================================================
group_admin() ->
    ordsets:union(
        group_manager(),
        ordsets:from_list([
            group_create_space,
            group_create_space_token,
            group_join_space,
            group_leave_space,
            group_set_privileges,
            group_change_data,
            group_remove
        ])
    ).


%% space_privileges/0
%% ====================================================================
%% @doc All atoms representing space privileges.
%% @equiv space_admin()
%% ====================================================================
-spec space_privileges() -> [space_privilege()].
%% ====================================================================
space_privileges() ->
    space_admin().


%% group_privileges/0
%% ====================================================================
%% @doc All atoms representing group privileges.
%% @equiv group_admin()
%% ====================================================================
-spec group_privileges() -> [group_privilege()].
%% ====================================================================
group_privileges() ->
    group_admin().
