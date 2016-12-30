%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains definitions of all REST operations, in a
%%% format of cowboy router.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_routes).
-author("Lukasz Opiola").

-include("rest.hrl").

-export([all/0]).

all() ->
    AllRoutes = lists:flatten([
        provider_routes(),
        user_routes()
    ]),
    % Convert all routes to cowboy-compliant routes
    % (rest handler module must be added as second element to the tuples)
    lists:map(fun({Path, State}) ->
        {Path, ?REST_HANDLER_MODULE, State}
    end, AllRoutes).


provider_routes() ->
    P = n_provider_logic_plugin,
    [
        {<<"/providers">>, #rest_req{methods = #{
            get => {P, undefined, list}
        }}},
        {<<"/providers/:id">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), entity},
            delete => {P, ?BINDING(id), entity}
        }}},
        {<<"/providers/:id/effective_users">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), eff_users}
        }}},
        {<<"/providers/:id/effective_users/:uid">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), {eff_user, ?BINDING(uid)}}
        }}},
        {<<"/providers/:id/effective_groups">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), eff_groups}
        }}},
        {<<"/providers/:id/effective_groups/:gid">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), {eff_group, ?BINDING(gid)}}
        }}},
        {<<"/providers/:id/spaces">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), spaces}
        }}},
        {<<"/providers/:id/spaces/:sid">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), {space, ?BINDING(sid)}}
        }}},

        {<<"/provider">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, entity},
            post => {P, undefined, entity},
            patch => {P, ?CLIENT_ID, entity},
            delete => {P, ?CLIENT_ID, entity}
        }}},
        {<<"/provider_dev">>, #rest_req{methods = #{
            post => {P, undefined, entity}
        }}},
        {<<"/provider/spaces">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, spaces}
        }}},
        {<<"/provider/spaces/support">>, #rest_req{methods = #{
            post => {P, ?CLIENT_ID, support}
        }}},
        {<<"/provider/spaces/:sid">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, {space, ?BINDING(sid)}},
            patch => {P, ?CLIENT_ID, {space, ?BINDING(sid)}},
            delete => {P, ?CLIENT_ID, {space, ?BINDING(sid)}}
        }}},
        {<<"/provider/test/check_my_ip">>, #rest_req{methods = #{
            get => {P, undefined, {check_my_ip, ?COWBOY_REQ}}
        }}},
        {<<"/provider/test/check_my_ports">>, #rest_req{methods = #{
            post => {P, undefined, check_my_ports}
        }}}
    ].


user_routes() ->
    P = n_user_logic_plugin,
    [
        {<<"/users">>, #rest_req{methods = #{
            get => {P, undefined, list}
        }}},
        {<<"/users/:id">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), entity},
            delete => {P, ?BINDING(id), entity}
        }}},
        {<<"/users/:id/privileges">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), oz_privileges},
            patch => {P, ?BINDING(id), oz_privileges},
            delete => {P, ?BINDING(id), oz_privileges}
        }}},
        {<<"/users/:id/effective_privileges">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), eff_oz_privileges}
        }}},

        {<<"/user">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, entity},
            patch => {P, ?CLIENT_ID, entity},
            delete => {P, ?CLIENT_ID, entity}
        }}},
        {<<"/user/authorize">>, #rest_req{methods = #{
            post => {P, ?CLIENT_ID, authorize}
        }}},
        {<<"/user/privileges">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), oz_privileges},
            patch => {P, ?BINDING(id), oz_privileges},
            delete => {P, ?BINDING(id), oz_privileges}
        }}},
        {<<"/user/effective_privileges">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), eff_oz_privileges}
        }}},

        {<<"/user/client_tokens">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, client_tokens},
            post => {P, ?CLIENT_ID, client_tokens},
            delete => {P, ?CLIENT_ID, client_tokens}
        }}},
        {<<"/user/client_tokens/:tid">>, #rest_req{methods = #{
            delete => {P, ?CLIENT_ID, {client_token, ?BINDING(tid)}}
        }}},

        {<<"/user/groups">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, groups},
            post => {n_group_logic_plugin, undefined, entity}
        }}},
        {<<"/user/groups/:gid">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, {group, ?BINDING(gid)}},
            delete => {P, ?CLIENT_ID, {group, ?BINDING(gid)}}
        }}},
        {<<"/user/groups/join">>, #rest_req{methods = #{
            post => {P, ?CLIENT_ID, join_group}
        }}},
        {<<"/user/effective_groups">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, eff_groups}
        }}},
        {<<"/user/effective_groups/:gid">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, {eff_group, ?BINDING(gid)}}
        }}},

        {<<"/user/spaces">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, spaces},
            post => {n_space_logic_plugin, undefined, entity}
        }}},
        {<<"/user/spaces/default">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, default_space},
            put => {P, ?CLIENT_ID, default_space},
            delete => {P, ?CLIENT_ID, default_space}
        }}},
        {<<"/user/spaces/join">>, #rest_req{methods = #{
            post => {P, ?CLIENT_ID, join_space}
        }}},
        {<<"/user/spaces/:sid">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, {space, ?BINDING(sid)}},
            delete => {P, ?CLIENT_ID, {space, ?BINDING(sid)}}
        }}},
        {<<"/user/spaces/:sid/alias">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, {space_alias, ?BINDING(sid)}},
            put => {P, ?CLIENT_ID, {space_alias, ?BINDING(sid)}},
            delete => {P, ?CLIENT_ID, {space_alias, ?BINDING(sid)}}
        }}},
        {<<"/user/effective_spaces">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, eff_spaces}
        }}},
        {<<"/user/effective_spaces/:sid">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, {eff_space, ?BINDING(sid)}}
        }}},

        {<<"/user/effective_providers">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, eff_providers}
        }}},
        {<<"/user/effective_providers/:pid">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, {eff_provider, ?BINDING(pid)}}
        }}},

        {<<"/user/handle_services">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, handle_services},
            post => {n_handle_service_logic_plugin, undefined, entity}
        }}},
        {<<"/user/handle_services/:gid">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, {handle_service, ?BINDING(hsid)}},
            delete => {P, ?CLIENT_ID, {handle_service, ?BINDING(hsid)}}
        }}},
        {<<"/user/effective_handle_services">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, eff_handle_services}
        }}},
        {<<"/user/effective_handle_services/:hsid">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, {eff_handle_service, ?BINDING(hsid)}}
        }}},

        {<<"/user/handles">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, handles},
            post => {n_handle_logic_plugin, undefined, entity}
        }}},
        {<<"/user/handles/:gid">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, {handle, ?BINDING(hid)}},
            delete => {P, ?CLIENT_ID, {handle, ?BINDING(hid)}}
        }}},
        {<<"/user/effective_handles">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, eff_handles}
        }}},
        {<<"/user/effective_handles/:hid">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, {eff_handle, ?BINDING(hid)}}
        }}}


    ].


















