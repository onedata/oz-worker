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
-include_lib("ctool/include/logging.hrl").

-export([all/0]).

all() ->
    AllRoutes = lists:flatten([
        provider_routes(),
        user_routes()
    ]),
    % Aggregate routes that share the same path
    AggregatedRoutes = lists:foldl(
        fun({Path, RestReq}, AccProps) ->
            #rest_req{method = Method} = RestReq,
            RoutesForPath = proplists:get_value(Path, AccProps, #{}),
            lists:keystore(
                Path, 1, AccProps,
                {Path, RoutesForPath#{Method => RestReq}}
            )
    end, [], AllRoutes),
    % Convert all routes to cowboy-compliant routes
    % - rest handler module must be added as second element to the tuples
    % - RoutesForPath will serve as Opts to rest handler init.
    A = lists:map(fun({Path, RoutesForPath}) ->
        {Path, ?REST_HANDLER_MODULE, RoutesForPath}
    end, AggregatedRoutes),
    ?dump(A),
    lists:map(fun({Path, RoutesForPath}) ->
        {Path, ?REST_HANDLER_MODULE, RoutesForPath}
    end, AggregatedRoutes).


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

        {<<"/user/client_tokens">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = client_tokens
        }},
        {<<"/user/client_tokens">>, R#rest_req{
            method = post, entity_id = ?CLIENT_ID, resource = client_tokens
        }},
        {<<"/user/client_tokens/:tid">>, R#rest_req{
            method = delete, entity_id = ?CLIENT_ID, resource = {client_token, ?BINDING(tid)}
        }},

        {<<"/user/groups">>, R#rest_req{
            method = get, entity_id = ?CLIENT_ID, resource = groups
        }},
        {<<"/user/groups">>, R#rest_req{
            method = post, entity_id = undefined, resource = entity,
            % Alias for group_logic, but use user_rest_translator for reply
            el_plugin = n_group_logic_plugin
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
            method = post, entity_id = undefined, resource = entity,
            % Alias for space_logic, but use user_rest_translator for reply
            el_plugin = n_space_logic_plugin
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
            method = post, entity_id = undefined, resource = entity,
            % Alias for handle_service_logic, but use user_rest_translator for reply
            el_plugin = n_handle_service_logic_plugin
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
            method = post, entity_id = undefined, resource = entity,
            % Alias for handle_logic, but use user_rest_translator for reply
            el_plugin = n_handle_logic_plugin
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


















