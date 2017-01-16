%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains definitions of all REST operations..
%%% @end
%%%-------------------------------------------------------------------
-module(rest_routes).
-author("Lukasz Opiola").

-include("rest.hrl").

-export([
    user_routes/0,
    group_routes/0,
    space_routes/0,
    share_routes/0,
    provider_routes/0,
    handle_service_routes/0,
    handle_routes/0
]).


%%--------------------------------------------------------------------
%% @doc
%% Definitions of user REST paths.
%% @end
%%--------------------------------------------------------------------
-spec user_routes() -> [{binary(), #rest_req{}}].
user_routes() ->
    R = #rest_req{
        el_plugin = n_user_logic_plugin,
        translator = user_rest_translator
    },
    [
        {<<"/users">>, R#rest_req{
            method = get, entity_id = undefined, resource = list
        }},
        {<<"/users/:id">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = data
        }},
        {<<"/users/:id">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = entity
        }},

        {<<"/users/:id/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = oz_privileges
        }},
        {<<"/users/:id/privileges">>, R#rest_req{
            method = patch, entity_id = ?BINDING(id), resource = oz_privileges
        }},
        {<<"/users/:id/privileges">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = oz_privileges
        }},
        {<<"/users/:id/effective_privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_oz_privileges
        }},

        {<<"/user">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = data
        }},
        {<<"/user">>, R#rest_req{
            method = patch, entity_id = ?CLIENT_ID, resource = entity
        }},
        {<<"/user">>, R#rest_req{
            method = delete, entity_id = ?CLIENT_ID, resource = entity
        }},

        {<<"/user/authorize">>, R#rest_req{
            method = post, entity_id = ?CLIENT_ID, resource = authorize
        }},

        {<<"/user/privileges">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = oz_privileges
        }},
        {<<"/user/privileges">>, R#rest_req{
            method = patch, entity_id = ?CLIENT_ID, resource = oz_privileges
        }},
        {<<"/user/privileges">>, R#rest_req{
            method = delete, entity_id = ?CLIENT_ID, resource = oz_privileges
        }},
        {<<"/user/effective_privileges">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = eff_oz_privileges
        }},

        {<<"/user/client_tokens">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = client_tokens
        }},
        {<<"/user/client_tokens">>, R#rest_req{
            method = post, entity_id = ?CLIENT_ID, resource = client_tokens
        }},
        {<<"/user/client_tokens/:tid">>, R#rest_req{
            method = delete, entity_id = ?CLIENT_ID, resource = {client_token, ?BINDING(tid)}
        }},

        {<<"/user/default_space">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = default_space
        }},
        {<<"/user/default_space">>, R#rest_req{
            method = put, entity_id = ?CLIENT_ID, resource = default_space
        }},
        {<<"/user/default_space">>, R#rest_req{
            method = delete, entity_id = ?CLIENT_ID, resource = default_space
        }},

        {<<"/user/default_provider">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = default_provider
        }},
        {<<"/user/default_provider">>, R#rest_req{
            method = put, entity_id = ?CLIENT_ID, resource = default_provider
        }},
        {<<"/user/default_provider">>, R#rest_req{
            method = delete, entity_id = ?CLIENT_ID, resource = default_provider
        }},

        {<<"/user/groups">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = groups
        }},
        {<<"/user/groups">>, R#rest_req{
            method = post, entity_id = ?BINDING(id), resource = create_group
        }},
        {<<"/user/groups/join">>, R#rest_req{
            method = post, entity_id = ?CLIENT_ID, resource = join_group
        }},
        {<<"/user/groups/:gid">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = {group, ?BINDING(gid)}
        }},
        {<<"/user/groups/:gid">>, R#rest_req{
            method = delete, entity_id = ?CLIENT_ID, resource = {group, ?BINDING(gid)}
        }},
        {<<"/user/effective_groups">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = eff_groups
        }},
        {<<"/user/effective_groups/:gid">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = {eff_group, ?BINDING(gid)}
        }},

        {<<"/user/spaces">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = spaces
        }},
        {<<"/user/spaces">>, R#rest_req{
            method = post, entity_id = ?BINDING(id), resource = create_space
        }},
        % TODO VFS-2918
        {<<"/user/spaces/default">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = deprecated_default_space
        }},
        % TODO VFS-2918
        {<<"/user/spaces/default">>, R#rest_req{
            method = put, entity_id = ?CLIENT_ID, resource = deprecated_default_space
        }},
        {<<"/user/spaces/join">>, R#rest_req{
            method = post, entity_id = ?CLIENT_ID, resource = join_space
        }},
        {<<"/user/spaces/:sid">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = {space, ?BINDING(sid)}
        }},
        {<<"/user/spaces/:sid">>, R#rest_req{
            method = delete, entity_id = ?CLIENT_ID, resource = {space, ?BINDING(sid)}
        }},
        {<<"/user/spaces/:sid/alias">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = {space_alias, ?BINDING(sid)}
        }},
        {<<"/user/spaces/:sid/alias">>, R#rest_req{
            method = put, entity_id = ?CLIENT_ID, resource = {space_alias, ?BINDING(sid)}
        }},
        {<<"/user/spaces/:sid/alias">>, R#rest_req{
            method = delete, entity_id = ?CLIENT_ID, resource = {space_alias, ?BINDING(sid)}
        }},
        {<<"/user/effective_spaces">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = eff_spaces
        }},
        {<<"/user/effective_spaces/:sid">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = {eff_space, ?BINDING(sid)}
        }},

        {<<"/user/effective_providers">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = eff_providers
        }},
        {<<"/user/effective_providers/:pid">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = {eff_provider, ?BINDING(pid)}
        }},

        {<<"/user/handle_services">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = handle_services
        }},
        {<<"/user/handle_services">>, R#rest_req{
            method = post, entity_id = ?BINDING(id), resource = create_handle_service
        }},
        {<<"/user/handle_services/:hsid">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = {handle_service, ?BINDING(hsid)}
        }},
        {<<"/user/handle_services/:hsid">>, R#rest_req{
            method = delete, entity_id = ?CLIENT_ID, resource = {handle_service, ?BINDING(hsid)}
        }},
        {<<"/user/effective_handle_services">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = eff_handle_services
        }},
        {<<"/user/effective_handle_services/:hsid">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = {eff_handle_service, ?BINDING(hsid)}
        }},

        {<<"/user/handles">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = handles
        }},
        {<<"/user/handles">>, R#rest_req{
            method = post, entity_id = ?BINDING(id), resource = create_handle
        }},
        {<<"/user/handles/:hid">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = {handle, ?BINDING(hid)}
        }},
        {<<"/user/handles/:hid">>, R#rest_req{
            method = delete, entity_id = ?CLIENT_ID, resource = {handle, ?BINDING(hid)}
        }},
        {<<"/user/effective_handles">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = eff_handles
        }},
        {<<"/user/effective_handles/:hid">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = {eff_handle, ?BINDING(hid)}
        }}
    ].


%%--------------------------------------------------------------------
%% @doc
%% Definitions of group REST paths.
%% @end
%%--------------------------------------------------------------------
-spec group_routes() -> [{binary(), #rest_req{}}].
group_routes() ->
    R = #rest_req{
        el_plugin = n_group_logic_plugin,
        translator = group_rest_translator
    },
    [
        {<<"/groups">>, R#rest_req{
            method = get, entity_id = undefined, resource = list
        }},
        {<<"/groups">>, R#rest_req{
            method = post, entity_id = undefined, resource = entity
        }},
        {<<"/groups/:id">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = data
        }},
        {<<"/groups/:id">>, R#rest_req{
            method = patch, entity_id = ?BINDING(id), resource = entity
        }},
        {<<"/groups/:id">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = entity
        }},

        {<<"/groups/:id/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = oz_privileges
        }},
        {<<"/groups/:id/privileges">>, R#rest_req{
            method = patch, entity_id = ?BINDING(id), resource = oz_privileges
        }},
        {<<"/groups/:id/privileges">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = oz_privileges
        }},
        {<<"/groups/:id/effective_privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_oz_privileges
        }},

        {<<"/groups/:id/users">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = users
        }},
        {<<"/groups/:id/users">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = users
        }},
        {<<"/groups/:id/users/token">>, R#rest_req{
            method = post, entity_id = ?BINDING(id), resource = invite_user_token
        }},
        % TODO VFS-2918
        {<<"/groups/:id/users/token">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = deprecated_invite_user_token
        }},
        {<<"/groups/:id/users/:uid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {user, ?BINDING(uid)}
        }},
        {<<"/groups/:id/users/:uid">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = {user, ?BINDING(uid)}
        }},
        {<<"/groups/:id/users/:uid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {user_privileges, ?BINDING(uid)}
        }},
        {<<"/groups/:id/users/:uid/privileges">>, R#rest_req{
            method = patch, entity_id = ?BINDING(id), resource = {user_privileges, ?BINDING(uid)}
        }},
        % TODO VFS-2918
        {<<"/groups/:id/users/:uid/privileges">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = {deprecated_user_privileges, ?BINDING(uid)}
        }},
        {<<"/groups/:id/effective_users">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_users
        }},
        {<<"/groups/:id/effective_users/:uid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_user, ?BINDING(uid)}
        }},
        {<<"/groups/:id/effective_users/:uid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_user_privileges, ?BINDING(uid)}
        }},

        % TODO VFS-2918
        {<<"/groups/:id/parent">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = parents
        }},
        % TODO VFS-2918
        {<<"/groups/:id/parent/:pid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {parent, ?BINDING(pid)}
        }},
        {<<"/groups/:id/parents">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = parents
        }},
        {<<"/groups/:id/parents/join">>, R#rest_req{
            method = post, entity_id = ?BINDING(id), resource = join_group
        }},
        {<<"/groups/:id/parents/:pid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {parent, ?BINDING(pid)}
        }},
        {<<"/groups/:id/effective_parents">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_parents
        }},
        {<<"/groups/:id/effective_parents/:pid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_parent, ?BINDING(pid)}
        }},

        % TODO VFS-2918
        {<<"/groups/:id/nested">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = children
        }},
        % TODO VFS-2918
        {<<"/groups/:id/nested/token">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = deprecated_invite_group_token
        }},
        % TODO VFS-2918
        {<<"/groups/:id/nested/join">>, R#rest_req{
            method = post, entity_id = ?BINDING(id), resource = join_group
        }},
        % TODO VFS-2918
        {<<"/groups/:id/nested/:nid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {child, ?BINDING(nid)}
        }},
        % TODO VFS-2918
        {<<"/groups/:id/nested/:nid">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = {child, ?BINDING(nid)}
        }},
        % TODO VFS-2918
        {<<"/groups/:id/nested/:nid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_child_privileges, ?BINDING(nid)}
        }},
        % TODO VFS-2918
        {<<"/groups/:id/nested/:nid/privileges">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = {deprecated_child_privileges, ?BINDING(nid)}
        }},


        {<<"/groups/:id/children">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = children
        }},
        {<<"/groups/:id/children">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = children
        }},
        {<<"/groups/:id/children/token">>, R#rest_req{
            method = post, entity_id = ?BINDING(id), resource = invite_group_token
        }},
        {<<"/groups/:id/children/:chid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {child, ?BINDING(chid)}
        }},
        {<<"/groups/:id/children/:chid">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = {child, ?BINDING(chid)}
        }},
        {<<"/groups/:id/children/:chid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {child_privileges, ?BINDING(chid)}
        }},
        {<<"/groups/:id/children/:chid/privileges">>, R#rest_req{
            method = patch, entity_id = ?BINDING(id), resource = {child_privileges, ?BINDING(chid)}
        }},
        {<<"/groups/:id/effective_children">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_children
        }},
        {<<"/groups/:id/effective_children/:chid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_child, ?BINDING(chid)}
        }},
        {<<"/groups/:id/effective_children/:chid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_child_privileges, ?BINDING(chid)}
        }},

        {<<"/groups/:id/spaces">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = spaces
        }},
        {<<"/groups/:id/spaces">>, R#rest_req{
            method = post, entity_id = ?BINDING(id), resource = create_space
        }},
        {<<"/groups/:id/spaces/join">>, R#rest_req{
            method = post, entity_id = ?BINDING(id), resource = join_space
        }},
        {<<"/groups/:id/spaces/:sid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {space, ?BINDING(sid)}
        }},
        {<<"/groups/:id/spaces/:sid">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = {space, ?BINDING(sid)}
        }},
        {<<"/groups/:id/spaces/:sid/alias">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {space_alias, ?BINDING(sid)}
        }},
        {<<"/groups/:id/spaces/:sid/alias">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = {space_alias, ?BINDING(sid)}
        }},
        {<<"/groups/:id/spaces/:sid/alias">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = {space_alias, ?BINDING(sid)}
        }},
        {<<"/groups/:id/effective_spaces">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_spaces
        }},
        {<<"/groups/:id/effective_spaces/:sid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_space, ?BINDING(sid)}
        }},

        {<<"/groups/:id/effective_providers">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_providers
        }},
        {<<"/groups/:id/effective_providers/:pid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_provider, ?BINDING(pid)}
        }},

        {<<"/groups/:id/handle_services">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = handle_services
        }},
        {<<"/groups/:id/handle_services">>, R#rest_req{
            method = post, entity_id = ?BINDING(id), resource = create_handle_service
        }},
        {<<"/groups/:id/handle_services/:hsid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {handle_service, ?BINDING(hsid)}
        }},
        {<<"/groups/:id/handle_services/:hsid">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = {handle_service, ?BINDING(hsid)}
        }},
        {<<"/groups/:id/effective_handle_services">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_handle_services
        }},
        {<<"/groups/:id/effective_handle_services/:hsid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_handle_service, ?BINDING(hsid)}
        }},

        {<<"/groups/:id/handles">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = handles
        }},
        {<<"/groups/:id/handles">>, R#rest_req{
            method = post, entity_id = ?BINDING(id), resource = create_handle
        }},
        {<<"/groups/:id/handles/:hid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {handle, ?BINDING(hid)}
        }},
        {<<"/groups/:id/handles/:hid">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = {handle, ?BINDING(hid)}
        }},
        {<<"/groups/:id/effective_handles">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_handles
        }},
        {<<"/groups/:id/effective_handles/:hid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_handle, ?BINDING(hid)}
        }}
    ].


%%--------------------------------------------------------------------
%% @doc
%% Definitions of space REST paths.
%% @end
%%--------------------------------------------------------------------
-spec space_routes() -> [{binary(), #rest_req{}}].
space_routes() ->
    R = #rest_req{
        el_plugin = n_space_logic_plugin,
        translator = space_rest_translator
    },
    [
        {<<"/spaces">>, R#rest_req{
            method = get, entity_id = undefined, resource = list
        }},
        {<<"/spaces">>, R#rest_req{
            method = post, entity_id = undefined, resource = entity
        }},
        {<<"/spaces/:id">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = data
        }},
        {<<"/spaces/:id">>, R#rest_req{
            method = patch, entity_id = ?BINDING(id), resource = entity
        }},
        {<<"/spaces/:id">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = entity
        }},

        {<<"/spaces/:id/users">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = users
        }},
        {<<"/spaces/:id/users">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = users
        }},
        {<<"/spaces/:id/users/token">>, R#rest_req{
            method = post, entity_id = ?BINDING(id), resource = invite_user_token
        }},
        % TODO VFS-2918
        {<<"/spaces/:id/users/token">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = deprecated_invite_user_token
        }},
        {<<"/spaces/:id/users/:uid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {user, ?BINDING(uid)}
        }},
        {<<"/spaces/:id/users/:uid">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = {user, ?BINDING(uid)}
        }},
        {<<"/spaces/:id/users/:uid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {user_privileges, ?BINDING(uid)}
        }},
        {<<"/spaces/:id/users/:uid/privileges">>, R#rest_req{
            method = patch, entity_id = ?BINDING(id), resource = {user_privileges, ?BINDING(uid)}
        }},
        % TODO VFS-2918
        {<<"/spaces/:id/users/:uid/privileges">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = {deprecated_user_privileges, ?BINDING(uid)}
        }},
        {<<"/spaces/:id/effective_users">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_users
        }},
        {<<"/spaces/:id/effective_users/:uid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_user, ?BINDING(uid)}
        }},
        {<<"/spaces/:id/effective_users/:uid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_user_privileges, ?BINDING(uid)}
        }},

        {<<"/spaces/:id/groups">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = groups
        }},
        {<<"/spaces/:id/groups">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = groups
        }},
        {<<"/spaces/:id/groups/token">>, R#rest_req{
            method = post, entity_id = ?BINDING(id), resource = invite_group_token
        }},
        % TODO VFS-2918
        {<<"/spaces/:id/groups/token">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = deprecated_invite_group_token
        }},
        {<<"/spaces/:id/groups/:gid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {group, ?BINDING(gid)}
        }},
        {<<"/spaces/:id/groups/:gid">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = {group, ?BINDING(gid)}
        }},
        {<<"/spaces/:id/groups/:gid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {group_privileges, ?BINDING(gid)}
        }},
        {<<"/spaces/:id/groups/:gid/privileges">>, R#rest_req{
            method = patch, entity_id = ?BINDING(id), resource = {group_privileges, ?BINDING(gid)}
        }},
        % TODO VFS-2918
        {<<"/spaces/:id/groups/:gid/privileges">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = {deprecated_group_privileges, ?BINDING(gid)}
        }},
        {<<"/spaces/:id/effective_groups">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_groups
        }},
        {<<"/spaces/:id/effective_groups/:gid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_group, ?BINDING(gid)}
        }},
        {<<"/spaces/:id/effective_groups/:gid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_group_privileges, ?BINDING(gid)}
        }},

        {<<"/spaces/:id/shares">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = shares
        }},
        {<<"/spaces/:id/shares/:shid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {shares, ?BINDING(shid)}
        }},
        % TODO VFS-2918
        {<<"/spaces/:id/shares/:pid">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = {create_share, ?BINDING(shid)}
        }},

        {<<"/spaces/:id/providers">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = providers
        }},
        {<<"/spaces/:id/providers/token">>, R#rest_req{
            method = post, entity_id = ?BINDING(id), resource = invite_user_token
        }},
        % TODO VFS-2918
        {<<"/spaces/:id/providers/token">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = deprecated_invite_provider_token
        }},
        {<<"/spaces/:id/providers/:pid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {provider, ?BINDING(pid)}
        }},
        {<<"/spaces/:id/providers/:pid">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = {provider, ?BINDING(pid)}
        }}
    ].


%%--------------------------------------------------------------------
%% @doc
%% Definitions of share REST paths.
%% @end
%%--------------------------------------------------------------------
-spec share_routes() -> [{binary(), #rest_req{}}].
share_routes() ->
    R = #rest_req{
        el_plugin = n_share_logic_plugin,
        translator = share_rest_translator
    },
    [
        {<<"/shares">>, R#rest_req{
            method = get, entity_id = undefined, resource = list
        }},
        {<<"/shares">>, R#rest_req{
            method = post, entity_id = undefined, resource = entity
        }},
        {<<"/shares/:id">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = data
        }},
        {<<"/shares/:id">>, R#rest_req{
            method = patch, entity_id = ?BINDING(id), resource = entity
        }},
        {<<"/shares/:id">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = entity
        }}
    ].


%%--------------------------------------------------------------------
%% @doc
%% Definitions of provider REST paths.
%% @end
%%--------------------------------------------------------------------
-spec provider_routes() -> [{binary(), #rest_req{}}].
provider_routes() ->
    R = #rest_req{
        el_plugin = n_provider_logic_plugin,
        translator = provider_rest_translator
    },
    [
        {<<"/providers">>, R#rest_req{
            method = get, entity_id = undefined, resource = list
        }},

        {<<"/providers/:id">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = data
        }},
        {<<"/providers/:id">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = entity
        }},

        {<<"/providers/:id/effective_users">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_users
        }},
        {<<"/providers/:id/effective_users/:uid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_user, ?BINDING(uid)}
        }},

        {<<"/providers/:id/effective_groups">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_groups
        }},
        {<<"/providers/:id/effective_groups/:gid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_group, ?BINDING(gid)}
        }},

        {<<"/providers/:id/spaces">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = spaces
        }},
        {<<"/providers/:id/spaces/:sid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {space, ?BINDING(sid)}
        }},

        {<<"/provider">>, R#rest_req{
            method = post, entity_id = undefined, resource = entity
        }},
        {<<"/provider">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = data
        }},
        {<<"/provider">>, R#rest_req{
            method = patch, entity_id = ?CLIENT_ID, resource = entity
        }},
        {<<"/provider">>, R#rest_req{
            method = delete, entity_id = ?CLIENT_ID, resource = entity
        }},

        {<<"/provider_dev">>, R#rest_req{
            method = post, entity_id = undefined, resource = entity_dev
        }},

        {<<"/provider/spaces">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = spaces
        }},
        {<<"/provider/spaces/support">>, R#rest_req{
            method = post, entity_id = ?CLIENT_ID, resource = support
        }},
        {<<"/provider/spaces/:sid">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = {space, ?BINDING(sid)}
        }},
        {<<"/provider/spaces/:sid">>, R#rest_req{
            method = patch, entity_id = ?CLIENT_ID, resource = {space, ?BINDING(sid)}
        }},
        {<<"/provider/spaces/:sid">>, R#rest_req{
            method = delete, entity_id = ?CLIENT_ID, resource = {space, ?BINDING(sid)}
        }},

        {<<"/provider/test/check_my_ip">>, R#rest_req{
            method = get, entity_id = undefined, resource = {check_my_ip, ?COWBOY_REQ}
        }},
        {<<"/provider/test/check_my_ports">>, R#rest_req{
            method = post, entity_id = undefined, resource = check_my_ports
        }}
    ].


%%--------------------------------------------------------------------
%% @doc
%% Definitions of handle_service REST paths.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_routes() -> [{binary(), #rest_req{}}].
handle_service_routes() ->
    R = #rest_req{
        el_plugin = n_handle_service_logic_plugin,
        translator = handle_service_rest_translator
    },
    [
        {<<"/handle_services">>, R#rest_req{
            method = get, entity_id = undefined, resource = list
        }},
        {<<"/handle_services">>, R#rest_req{
            method = post, entity_id = undefined, resource = entity
        }},
        {<<"/handle_services/:id">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = data
        }},
        {<<"/handle_services/:id">>, R#rest_req{
            method = patch, entity_id = ?BINDING(id), resource = entity
        }},
        {<<"/handle_services/:id">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = entity
        }},

        {<<"/handle_services/:id/users">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = users
        }},
        {<<"/handle_services/:id/users">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = users
        }},
        {<<"/handle_services/:id/users/:uid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {user, ?BINDING(uid)}
        }},
        {<<"/handle_services/:id/users/:uid">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = {user, ?BINDING(uid)}
        }},
        {<<"/handle_services/:id/users/:uid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {user_privileges, ?BINDING(uid)}
        }},
        {<<"/handle_services/:id/users/:uid/privileges">>, R#rest_req{
            method = patch, entity_id = ?BINDING(id), resource = {user_privileges, ?BINDING(uid)}
        }},
        % TODO VFS-2918
        {<<"/handle_services/:id/users/:uid/privileges">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = {deprecated_user_privileges, ?BINDING(uid)}
        }},
        {<<"/handle_services/:id/effective_users">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_users
        }},
        {<<"/handle_services/:id/effective_users/:uid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_user, ?BINDING(uid)}
        }},
        {<<"/handle_services/:id/effective_users/:uid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_user_privileges, ?BINDING(uid)}
        }},

        {<<"/handle_services/:id/groups">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = groups
        }},
        {<<"/handle_services/:id/groups">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = groups
        }},
        {<<"/handle_services/:id/groups/:gid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {group, ?BINDING(gid)}
        }},
        {<<"/handle_services/:id/groups/:gid">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = {group, ?BINDING(gid)}
        }},
        {<<"/handle_services/:id/groups/:gid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {group_privileges, ?BINDING(gid)}
        }},
        {<<"/handle_services/:id/groups/:gid/privileges">>, R#rest_req{
            method = patch, entity_id = ?BINDING(id), resource = {group_privileges, ?BINDING(gid)}
        }},
        % TODO VFS-2918
        {<<"/handle_services/:id/groups/:gid/privileges">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = {deprecated_group_privileges, ?BINDING(gid)}
        }},
        {<<"/handle_services/:id/effective_groups">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_groups
        }},
        {<<"/handle_services/:id/effective_groups/:gid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_group, ?BINDING(gid)}
        }},
        {<<"/handle_services/:id/effective_groups/:gid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_group_privileges, ?BINDING(gid)}
        }},

        {<<"/handle_services/:id/handles">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = handles
        }},
        {<<"/handle_services/:id/handles/:hid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {handle, ?BINDING(hid)}
        }}
    ].


%%--------------------------------------------------------------------
%% @doc
%% Definitions of handle REST paths.
%% @end
%%--------------------------------------------------------------------
-spec handle_routes() -> [{binary(), #rest_req{}}].
handle_routes() ->
    R = #rest_req{
        el_plugin = n_handle_logic_plugin,
        translator = handle_rest_translator
    },
    [
        {<<"/handles">>, R#rest_req{
            method = get, entity_id = undefined, resource = list
        }},
        {<<"/handles">>, R#rest_req{
            method = post, entity_id = undefined, resource = entity
        }},
        {<<"/handles/:id">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = data
        }},
        {<<"/handles/:id">>, R#rest_req{
            method = patch, entity_id = ?BINDING(id), resource = entity
        }},
        {<<"/handles/:id">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = entity
        }},

        {<<"/handles/:id/users">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = users
        }},
        {<<"/handles/:id/users">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = users
        }},
        {<<"/handles/:id/users/:uid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {user, ?BINDING(uid)}
        }},
        {<<"/handles/:id/users/:uid">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = {user, ?BINDING(uid)}
        }},
        {<<"/handles/:id/users/:uid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {user_privileges, ?BINDING(uid)}
        }},
        {<<"/handles/:id/users/:uid/privileges">>, R#rest_req{
            method = patch, entity_id = ?BINDING(id), resource = {user_privileges, ?BINDING(uid)}
        }},
        % TODO VFS-2918
        {<<"/handles/:id/users/:uid/privileges">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = {deprecated_user_privileges, ?BINDING(uid)}
        }},
        {<<"/handles/:id/effective_users">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_users
        }},
        {<<"/handles/:id/effective_users/:uid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_user, ?BINDING(uid)}
        }},
        {<<"/handles/:id/effective_users/:uid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_user_privileges, ?BINDING(uid)}
        }},

        {<<"/handles/:id/groups">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = groups
        }},
        {<<"/handles/:id/groups">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = groups
        }},
        {<<"/handles/:id/groups/:gid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {group, ?BINDING(gid)}
        }},
        {<<"/handles/:id/groups/:gid">>, R#rest_req{
            method = delete, entity_id = ?BINDING(id), resource = {group, ?BINDING(gid)}
        }},
        {<<"/handles/:id/groups/:gid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {group_privileges, ?BINDING(gid)}
        }},
        {<<"/handles/:id/groups/:gid/privileges">>, R#rest_req{
            method = patch, entity_id = ?BINDING(id), resource = {group_privileges, ?BINDING(gid)}
        }},
        % TODO VFS-2918
        {<<"/handles/:id/groups/:gid/privileges">>, R#rest_req{
            method = put, entity_id = ?BINDING(id), resource = {deprecated_group_privileges, ?BINDING(gid)}
        }},
        {<<"/handles/:id/effective_groups">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = eff_groups
        }},
        {<<"/handles/:id/effective_groups/:gid">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_group, ?BINDING(gid)}
        }},
        {<<"/handles/:id/effective_groups/:gid/privileges">>, R#rest_req{
            method = get, entity_id = ?BINDING(id), resource = {eff_group_privileges, ?BINDING(gid)}
        }}
    ].


















