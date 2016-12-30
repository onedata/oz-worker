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
        provider_routes()
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
            get => {P, undefined, list}  % DONE
        }}},
        {<<"/providers/:id">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), entity},  % DONE
            delete => {P, ?BINDING(id), entity} % DONE
        }}},
        {<<"/providers/:id/effective_users">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), eff_users}  % DONE
        }}},
        {<<"/providers/:id/effective_users/:uid">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), {eff_user, ?BINDING(uid)}}  % DONE
        }}},
        {<<"/providers/:id/effective_groups">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), eff_groups}  % DONE
        }}},
        {<<"/providers/:id/effective_groups/:gid">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), {eff_group, ?BINDING(gid)}}  % DONE
        }}},
        {<<"/providers/:id/spaces">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), spaces}  % DONE
        }}},
        {<<"/providers/:id/spaces/:sid">>, #rest_req{methods = #{
            get => {P, ?BINDING(id), {space, ?BINDING(sid)}}  % DONE
        }}},
        {<<"/provider">>, #rest_req{methods = #{
            get => {P, ?CLIENT_ID, entity},  % DONE
            post => {P, undefined, entity},  % DONE
            patch => {P, ?CLIENT_ID, entity},  % DONE
            delete => {P, ?CLIENT_ID, entity}  % DONE
        }}},
        {<<"/provider_dev">>, #rest_req{methods = #{
            post => {P, undefined, entity}  % DONE
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
