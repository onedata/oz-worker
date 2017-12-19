%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains definitions of all REST methods.
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
    handle_routes/0,
    identity_routes/0
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Definitions of user REST paths.
%% @end
%%--------------------------------------------------------------------
-spec user_routes() -> [{binary(), #rest_req{}}].
user_routes() -> [
    {<<"/users">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, aspect = list}
    }},
    {<<"/users/:id">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    {<<"/users/:id">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = instance}
    }},

    {<<"/users/:id/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = oz_privileges}
    }},
    {<<"/users/:id/privileges">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = oz_privileges}
    }},
    {<<"/users/:id/privileges">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = oz_privileges}
    }},
    {<<"/users/:id/effective_privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?BINDING(id), aspect = eff_oz_privileges}
    }},

    {<<"/user">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = instance, scope = protected}
    }},
    {<<"/user">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = instance}
    }},
    {<<"/user">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = instance}
    }},

    {<<"/user/authorize">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_user, aspect = authorize}
    }},

    {<<"/user/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = oz_privileges}
    }},
    {<<"/user/privileges">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = oz_privileges}
    }},
    {<<"/user/privileges">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = oz_privileges}
    }},
    {<<"/user/effective_privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_oz_privileges}
    }},

    {<<"/user/client_tokens">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = client_tokens}
    }},
    {<<"/user/client_tokens">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = client_tokens}
    }},
    {<<"/user/client_tokens/:tid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {client_token, ?BINDING(tid)}}
    }},

    {<<"/user/default_space">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_space}
    }},
    {<<"/user/default_space">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_space}
    }},
    {<<"/user/default_space">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_space}
    }},

    {<<"/user/default_provider">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_provider}
    }},
    {<<"/user/default_provider">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_provider}
    }},
    {<<"/user/default_provider">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = default_provider}
    }},

    {<<"/user/groups">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = groups}
    }},
    {<<"/user/groups">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_group, id = undefined, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/user/groups/join">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_group, id = undefined, aspect = join},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/user/groups/:gid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    {<<"/user/groups/:gid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {group, ?BINDING(gid)}}
    }},
    {<<"/user/effective_groups">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_groups}
    }},
    {<<"/user/effective_groups/:gid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},

    {<<"/user/spaces">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = spaces}
    }},
    {<<"/user/spaces">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_space, id = undefined, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    % TODO VFS-2918
    {<<"/user/spaces/default">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = deprecated_default_space}
    }},
    % TODO VFS-2918
    {<<"/user/spaces/default">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = deprecated_default_space}
    }},
    {<<"/user/spaces/join">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_space, id = undefined, aspect = join},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/user/spaces/:sid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    {<<"/user/spaces/:sid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {space, ?BINDING(sid)}}
    }},
    {<<"/user/spaces/:sid/alias">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {space_alias, ?BINDING(sid)}}
    }},
    {<<"/user/spaces/:sid/alias">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {space_alias, ?BINDING(sid)}}
    }},
    {<<"/user/spaces/:sid/alias">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {space_alias, ?BINDING(sid)}}
    }},
    {<<"/user/effective_spaces">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_spaces}
    }},
    {<<"/user/effective_spaces/:sid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},

    {<<"/user/effective_providers">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_providers}
    }},
    {<<"/user/effective_providers/:pid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},

    {<<"/user/handle_services">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = handle_services}
    }},
    {<<"/user/handle_services">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_handle_service, id = undefined, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/user/handle_services/:hsid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(hsid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    {<<"/user/handle_services/:hsid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {handle_service, ?BINDING(hsid)}}
    }},
    {<<"/user/effective_handle_services">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_handle_services}
    }},
    {<<"/user/effective_handle_services/:hsid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(hsid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},

    {<<"/user/handles">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = handles}
    }},
    {<<"/user/handles">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_handle, id = undefined, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/user/handles/:hid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }},
    {<<"/user/handles/:hid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = {handle, ?BINDING(hid)}}
    }},
    {<<"/user/effective_handles">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?CLIENT_ID, aspect = eff_handles}
    }},
    {<<"/user/effective_handles/:hid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_USER(?CLIENT_ID)
    }}
].


%%--------------------------------------------------------------------
%% @doc
%% Definitions of group REST paths.
%% @end
%%--------------------------------------------------------------------
-spec group_routes() -> [{binary(), #rest_req{}}].
group_routes() -> [
    {<<"/groups">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, aspect = list}
    }},
    {<<"/groups">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_group, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/groups/:id">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    {<<"/groups/:id">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = instance}
    }},
    {<<"/groups/:id">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = instance}
    }},

    {<<"/groups/:id/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = oz_privileges}
    }},
    {<<"/groups/:id/privileges">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = oz_privileges}
    }},
    {<<"/groups/:id/privileges">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = oz_privileges}
    }},
    {<<"/groups/:id/effective_privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_oz_privileges}
    }},

    {<<"/groups/:id/users">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = users}
    }},
    % TODO VFS-2918
    {<<"/groups/:id/users/token">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = deprecated_invite_user_token}
    }},
    {<<"/groups/:id/users/token">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = invite_user_token}
    }},
    {<<"/groups/:id/users/:uid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/users/:uid">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    {<<"/groups/:id/users/:uid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    {<<"/groups/:id/users/:uid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    {<<"/groups/:id/users/:uid/privileges">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    % TODO VFS-2918
    {<<"/groups/:id/users/:uid/privileges">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {deprecated_user_privileges, ?BINDING(uid)}}
    }},
    {<<"/groups/:id/effective_users">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_users}
    }},
    {<<"/groups/:id/effective_users/:uid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/effective_users/:uid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {eff_user_privileges, ?BINDING(uid)}}
    }},

    % TODO VFS-2918
    {<<"/groups/:id/parent">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = parents}
    }},
    % TODO VFS-2918
    {<<"/groups/:id/parent/:pid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/parents">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = parents}
    }},
    {<<"/groups/:id/parents/join">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_group, id = undefined, aspect = join},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/parents/:pid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/parents/:pid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {parent, ?BINDING(pid)}}
    }},
    {<<"/groups/:id/effective_parents">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_parents}
    }},
    {<<"/groups/:id/effective_parents/:pid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},

    % TODO VFS-2918
    {<<"/groups/:id/nested">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = children}
    }},
    % TODO VFS-2918
    {<<"/groups/:id/nested/token">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = deprecated_invite_group_token}
    }},
    % TODO VFS-2918
    {<<"/groups/:id/nested/join">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_group, id = undefined, aspect = join},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    % TODO VFS-2918
    {<<"/groups/:id/nested/:nid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(nid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    % TODO VFS-2918
    {<<"/groups/:id/nested/:nid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {child, ?BINDING(nid)}}
    }},
    % TODO VFS-2918
    {<<"/groups/:id/nested/:nid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {child_privileges, ?BINDING(nid)}}
    }},
    % TODO VFS-2918
    {<<"/groups/:id/nested/:nid/privileges">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {deprecated_child_privileges, ?BINDING(nid)}}
    }},


    {<<"/groups/:id/children">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = children}
    }},
    {<<"/groups/:id/children/token">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = invite_group_token}
    }},
    {<<"/groups/:id/children/:chid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(chid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/children/:chid">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {child, ?BINDING(chid)}}
    }},
    {<<"/groups/:id/children/:chid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {child, ?BINDING(chid)}}
    }},
    {<<"/groups/:id/children/:chid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {child_privileges, ?BINDING(chid)}}
    }},
    {<<"/groups/:id/children/:chid/privileges">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {child_privileges, ?BINDING(chid)}}
    }},
    {<<"/groups/:id/effective_children">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_children}
    }},
    {<<"/groups/:id/effective_children/:chid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(chid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/effective_children/:chid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {eff_child_privileges, ?BINDING(chid)}}
    }},

    {<<"/groups/:id/spaces">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = spaces}
    }},
    {<<"/groups/:id/spaces">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_space, id = undefined, aspect = instance},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/spaces/join">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_space, id = undefined, aspect = join},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/spaces/:sid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/spaces/:sid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {space, ?BINDING(sid)}}
    }},
    {<<"/groups/:id/effective_spaces">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_spaces}
    }},
    {<<"/groups/:id/effective_spaces/:sid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},

    {<<"/groups/:id/effective_providers">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_providers}
    }},
    {<<"/groups/:id/effective_providers/:pid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},

    {<<"/groups/:id/handle_services">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = handle_services}
    }},
    {<<"/groups/:id/handle_services">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_handle_service, id = undefined, aspect = instance},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/handle_services/:hsid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(hsid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/handle_services/:hsid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {handle_service, ?BINDING(hsid)}}
    }},
    {<<"/groups/:id/effective_handle_services">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_handle_services}
    }},
    {<<"/groups/:id/effective_handle_services/:hsid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(hsid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},

    {<<"/groups/:id/handles">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = handles}
    }},
    {<<"/groups/:id/handles">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_handle, id = undefined, aspect = instance},
        b_auth_hint = ?AS_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/handles/:hid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }},
    {<<"/groups/:id/handles/:hid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = {handle, ?BINDING(hid)}}
    }},
    {<<"/groups/:id/effective_handles">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(id), aspect = eff_handles}
    }},
    {<<"/groups/:id/effective_handles/:hid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_GROUP(?BINDING(id))
    }}
].


%%--------------------------------------------------------------------
%% @doc
%% Definitions of space REST paths.
%% @end
%%--------------------------------------------------------------------
-spec space_routes() -> [{binary(), #rest_req{}}].
space_routes() -> [
    {<<"/spaces">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, aspect = list}
    }},
    {<<"/spaces">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_space, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/spaces/:id">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    {<<"/spaces/:id">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = instance}
    }},
    {<<"/spaces/:id">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = instance}
    }},

    {<<"/spaces/:id/users">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = users}
    }},
    % TODO VFS-2918
    {<<"/spaces/:id/users/token">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = deprecated_invite_user_token}
    }},
    {<<"/spaces/:id/users/token">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = invite_user_token}
    }},
    {<<"/spaces/:id/users/:uid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    {<<"/spaces/:id/users/:uid">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    {<<"/spaces/:id/users/:uid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    {<<"/spaces/:id/users/:uid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    {<<"/spaces/:id/users/:uid/privileges">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    % TODO VFS-2918
    {<<"/spaces/:id/users/:uid/privileges">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {deprecated_user_privileges, ?BINDING(uid)}}
    }},
    {<<"/spaces/:id/effective_users">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = eff_users}
    }},
    {<<"/spaces/:id/effective_users/:uid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    {<<"/spaces/:id/effective_users/:uid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {eff_user_privileges, ?BINDING(uid)}}
    }},

    {<<"/spaces/:id/groups">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = groups}
    }},
    {<<"/spaces/:id/groups/token">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = invite_group_token}
    }},
    % TODO VFS-2918
    {<<"/spaces/:id/groups/token">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = deprecated_invite_group_token}
    }},
    {<<"/spaces/:id/groups/:gid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    {<<"/spaces/:id/groups/:gid">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    {<<"/spaces/:id/groups/:gid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    {<<"/spaces/:id/groups/:gid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    {<<"/spaces/:id/groups/:gid/privileges">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    % TODO VFS-2918
    {<<"/spaces/:id/groups/:gid/privileges">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {deprecated_group_privileges, ?BINDING(gid)}}
    }},
    {<<"/spaces/:id/effective_groups">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = eff_groups}
    }},
    {<<"/spaces/:id/effective_groups/:gid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    {<<"/spaces/:id/effective_groups/:gid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {eff_group_privileges, ?BINDING(gid)}}
    }},

    {<<"/spaces/:id/shares">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = shares}
    }},
    {<<"/spaces/:id/shares/:shid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_share, id = ?BINDING(shid), aspect = instance, scope = private},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    % TODO VFS-2918
    {<<"/spaces/:id/shares/:shid">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {deprecated_create_share, ?BINDING(shid)}}
    }},

    {<<"/spaces/:id/providers">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = providers}
    }},
    {<<"/spaces/:id/providers/token">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = invite_provider_token}
    }},
    % TODO VFS-2918
    {<<"/spaces/:id/providers/token">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = deprecated_invite_provider_token}
    }},
    {<<"/spaces/:id/providers/:pid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_provider, id = ?BINDING(pid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_SPACE(?BINDING(id))
    }},
    {<<"/spaces/:id/providers/:pid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_space, id = ?BINDING(id), aspect = {provider, ?BINDING(pid)}}
    }}
].


%%--------------------------------------------------------------------
%% @doc
%% Definitions of share REST paths.
%% @end
%%--------------------------------------------------------------------
-spec share_routes() -> [{binary(), #rest_req{}}].
share_routes() -> [
    {<<"/shares">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_share, aspect = list}
    }},
    {<<"/shares">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_share, aspect = instance}
    }},
    {<<"/shares/:id">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_share, id = ?BINDING(id), aspect = instance, scope = private}
    }},
    {<<"/shares/:id">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_share, id = ?BINDING(id), aspect = instance}
    }},
    {<<"/shares/:id">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_share, id = ?BINDING(id), aspect = instance}
    }}
].


%%--------------------------------------------------------------------
%% @doc
%% Definitions of provider REST paths.
%% @end
%%--------------------------------------------------------------------
-spec provider_routes() -> [{binary(), #rest_req{}}].
provider_routes() -> [
    {<<"/providers">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_provider, aspect = list}
    }},

    {<<"/providers/:id">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_provider, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    {<<"/providers/:id">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_provider, id = ?BINDING(id), aspect = instance}
    }},

    {<<"/providers/:id/effective_users">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_provider, id = ?BINDING(id), aspect = eff_users}
    }},
    {<<"/providers/:id/effective_users/:uid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_PROVIDER(?BINDING(id))
    }},

    {<<"/providers/:id/effective_groups">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_provider, id = ?BINDING(id), aspect = eff_groups}
    }},
    {<<"/providers/:id/effective_groups/:gid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_PROVIDER(?BINDING(id))
    }},

    {<<"/providers/:id/spaces">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_provider, id = ?BINDING(id), aspect = spaces}
    }},
    {<<"/providers/:id/spaces/:sid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_PROVIDER(?BINDING(id))
    }},

    {<<"/provider">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_provider, aspect = instance}
    }},
    {<<"/provider">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = instance, scope = protected}
    }},
    {<<"/provider">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = instance}
    }},
    {<<"/provider">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = instance}
    }},

    {<<"/provider_dev">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_provider, aspect = instance_dev}
    }},

    {<<"/provider/spaces">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = spaces}
    }},
    {<<"/provider/spaces/support">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = support}
    }},
    {<<"/provider/spaces/:sid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_space, id = ?BINDING(sid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_PROVIDER(?CLIENT_ID)
    }},
    {<<"/provider/spaces/:sid">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = {space, ?BINDING(sid)}}
    }},
    {<<"/provider/spaces/:sid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_provider, id = ?CLIENT_ID, aspect = {space, ?BINDING(sid)}}
    }},

    {<<"/provider/test/check_my_ip">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_provider, aspect = {check_my_ip, ?CLIENT_IP}}
    }},
    {<<"/provider/test/get_current_time">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_provider, aspect = current_time}
    }},
    {<<"/provider/test/check_my_ports">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_provider, aspect = check_my_ports}
    }},
    {<<"/provider/test/map_idp_group">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_provider, aspect = map_idp_group}
    }}
].


%%--------------------------------------------------------------------
%% @doc
%% Definitions of handle_service REST paths.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_routes() -> [{binary(), #rest_req{}}].
handle_service_routes() -> [
    {<<"/handle_services">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle_service, aspect = list}
    }},
    {<<"/handle_services">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_handle_service, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/handle_services/:id">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    {<<"/handle_services/:id">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = instance}
    }},
    {<<"/handle_services/:id">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = instance}
    }},

    {<<"/handle_services/:id/users">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = users}
    }},
    {<<"/handle_services/:id/users/:uid">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    {<<"/handle_services/:id/users/:uid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE_SERVICE(?BINDING(id))
    }},
    {<<"/handle_services/:id/users/:uid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    {<<"/handle_services/:id/users/:uid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    {<<"/handle_services/:id/users/:uid/privileges">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    % TODO VFS-2918
    {<<"/handle_services/:id/users/:uid/privileges">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {deprecated_user_privileges, ?BINDING(uid)}}
    }},
    {<<"/handle_services/:id/effective_users">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = eff_users}
    }},
    {<<"/handle_services/:id/effective_users/:uid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE_SERVICE(?BINDING(id))
    }},
    {<<"/handle_services/:id/effective_users/:uid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {eff_user_privileges, ?BINDING(uid)}}
    }},

    {<<"/handle_services/:id/groups">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = groups}
    }},
    {<<"/handle_services/:id/groups/:gid">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    {<<"/handle_services/:id/groups/:gid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE_SERVICE(?BINDING(id))
    }},
    {<<"/handle_services/:id/groups/:gid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    {<<"/handle_services/:id/groups/:gid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    {<<"/handle_services/:id/groups/:gid/privileges">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    % TODO VFS-2918
    {<<"/handle_services/:id/groups/:gid/privileges">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {deprecated_group_privileges, ?BINDING(gid)}}
    }},
    {<<"/handle_services/:id/effective_groups">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = eff_groups}
    }},
    {<<"/handle_services/:id/effective_groups/:gid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE_SERVICE(?BINDING(id))
    }},
    {<<"/handle_services/:id/effective_groups/:gid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = {eff_group_privileges, ?BINDING(gid)}}
    }},

    {<<"/handle_services/:id/handles">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle_service, id = ?BINDING(id), aspect = handles}
    }},
    {<<"/handle_services/:id/handles/:hid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(hid), aspect = instance, scope = protected},
        b_auth_hint = ?THROUGH_HANDLE_SERVICE(?BINDING(id))
    }}
].


%%--------------------------------------------------------------------
%% @doc
%% Definitions of handle REST paths.
%% @end
%%--------------------------------------------------------------------
-spec handle_routes() -> [{binary(), #rest_req{}}].
handle_routes() -> [
    {<<"/handles">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle, aspect = list}
    }},
    {<<"/handles">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = od_handle, aspect = instance},
        b_auth_hint = ?AS_USER(?CLIENT_ID)
    }},
    {<<"/handles/:id">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = instance, scope = protected}
    }},
    {<<"/handles/:id">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = instance}
    }},
    {<<"/handles/:id">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = instance}
    }},

    {<<"/handles/:id/users">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = users}
    }},
    {<<"/handles/:id/users/:uid">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    {<<"/handles/:id/users/:uid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE(?BINDING(id))
    }},
    {<<"/handles/:id/users/:uid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {user, ?BINDING(uid)}}
    }},
    {<<"/handles/:id/users/:uid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    {<<"/handles/:id/users/:uid/privileges">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {user_privileges, ?BINDING(uid)}}
    }},
    % TODO VFS-2918
    {<<"/handles/:id/users/:uid/privileges">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {deprecated_user_privileges, ?BINDING(uid)}}
    }},
    {<<"/handles/:id/effective_users">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = eff_users}
    }},
    {<<"/handles/:id/effective_users/:uid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_user, id = ?BINDING(uid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE(?BINDING(id))
    }},
    {<<"/handles/:id/effective_users/:uid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {eff_user_privileges, ?BINDING(uid)}}
    }},

    {<<"/handles/:id/groups">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = groups}
    }},
    {<<"/handles/:id/groups/:gid">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    {<<"/handles/:id/groups/:gid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE(?BINDING(id))
    }},
    {<<"/handles/:id/groups/:gid">>, #rest_req{
        method = delete,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {group, ?BINDING(gid)}}
    }},
    {<<"/handles/:id/groups/:gid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    {<<"/handles/:id/groups/:gid/privileges">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {group_privileges, ?BINDING(gid)}}
    }},
    % TODO VFS-2918
    {<<"/handles/:id/groups/:gid/privileges">>, #rest_req{
        method = put,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {deprecated_group_privileges, ?BINDING(gid)}}
    }},
    {<<"/handles/:id/effective_groups">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = eff_groups}
    }},
    {<<"/handles/:id/effective_groups/:gid">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_group, id = ?BINDING(gid), aspect = instance, scope = shared},
        b_auth_hint = ?THROUGH_HANDLE(?BINDING(id))
    }},
    {<<"/handles/:id/effective_groups/:gid/privileges">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = od_handle, id = ?BINDING(id), aspect = {eff_group_privileges, ?BINDING(gid)}}
    }}
].


%%--------------------------------------------------------------------
%% @doc
%% Definitions of identity REST paths.
%% @end
%%--------------------------------------------------------------------
-spec identity_routes() -> [{binary(), #rest_req{}}].
identity_routes() -> [
    {<<"/publickey/:id">>, #rest_req{
        method = get,
        b_gri = #b_gri{type = owned_identity, aspect = {publickey, ?BINDING(id)}}
    }},
    {<<"/publickey/:id">>, #rest_req{
        method = patch,
        b_gri = #b_gri{type = owned_identity, aspect = {publickey, ?BINDING(id)}}
    }},
    {<<"/provider_data/:id">>, #rest_req{
        method = post,
        b_gri = #b_gri{type = owned_identity, aspect = {provider, ?BINDING(id)}}
    }}
].















