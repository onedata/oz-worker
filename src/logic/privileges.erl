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
-export([group_user/0, group_manager/0, group_space_manager/0, group_admin/0]).


%% User privileges with regards to Space management.
-type space_privilege() :: space_invite_user | space_remove_user |
    space_invite_group | space_remove_group | space_set_privileges |
    space_remove | space_add_provider | space_remove_provider |
    space_change_data | space_view_data.


%% User privileges with regards to group management.
-type group_privilege() :: group_change_data | group_invite_user |
    group_remove_user | group_join_space | group_create_space |
    group_set_privileges | group_remove | group_leave_space |
    group_view_data.


%% User privileges.
-type privilege() :: space_privilege() | group_privilege().


%% space_user/0
%% ====================================================================
%% @doc A privilege level of a Space user.
%% ====================================================================
-spec space_user() -> sets:set(space_privilege()).
%% ====================================================================
space_user() ->
    sets:from_list([
        space_view_data
    ]).


%% space_manager/0
%% ====================================================================
%% @doc A privilege level of a Space manager.
%% ====================================================================
-spec space_manager() -> sets:set(space_privilege()).
%% ====================================================================
space_manager() ->
    sets:union(
        space_user(),
        sets:from_list([
            space_invite_user,
            space_remove_user,
            space_invite_group,
            space_remove_group
        ])
    ).


%% space_admin/0
%% ====================================================================
%% @doc A privilege level of a Space administrator.
%% ====================================================================
-spec space_admin() -> sets:set(space_privilege()).
%% ====================================================================
space_admin() ->
    sets:union(
        space_manager(),
        sets:from_list([
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
-spec group_user() -> sets:set(privilege()).
%% ====================================================================
group_user() ->
    sets:union(
        space_user(),
        sets:from_list([
            group_view_data
        ])
    ).


%% group_manager/0
%% ====================================================================
%% @doc A privilege level of a group manager.
%% ====================================================================
-spec group_manager() -> sets:set(privilege()).
%% ====================================================================
group_manager() ->
    sets:union(
        group_user(),
        sets:from_list([
            group_invite_user,
            group_remove_user
        ])
    ).


%% group_space_manager/0
%% ====================================================================
%% @doc A privilege level of a group manager allowed to manage group's Spaces.
%% ====================================================================
-spec group_space_manager() -> sets:set(privilege()).
%% ====================================================================
group_space_manager() ->
    sets:union(
        group_manager(),
        space_manager()
    ).


%% group_admin/0
%% ====================================================================
%% @doc A privilege level of a group administrator.
%% ====================================================================
-spec group_admin() -> sets:set(privilege()).
%% ====================================================================
group_admin() ->
    sets:union(
        group_space_manager(),
        sets:from_list([
            group_create_space,
            group_join_space,
            group_leave_space,
            group_set_privileges,
            group_change_data,
            group_remove
        ])
    ).
