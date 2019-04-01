%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common definitions for REST.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(REST_HRL).
-define(REST_HRL, 1).

-include("entity_logic.hrl").
-include("http/codes.hrl").

-define(REST_HANDLER_MODULE, rest_handler).

% (bound GRI) - GRI with bindings that is converted to proper GRI when bindings
% are resolved.
-record(b_gri, {
    type :: undefined | gs_protocol:entity_type(),
    id :: undefined | rest_handler:binding(),
    aspect :: undefined | gs_protocol:aspect(),
    scope = private :: gs_protocol:scope()
}).

%% Record containing the state of REST request.
-record(rest_req, {
    method = get :: rest_handler:method(),
    b_gri :: rest_handler:bound_gri(),
    b_auth_hint :: rest_handler:bound_auth_hint()
}).
%% Record representing REST response.
-record(rest_resp, {
    code = ?HTTP_200_OK :: integer(),
    headers = #{} :: #{binary() => binary()},
    body = {binary, <<"">>} :: json_utils:json_term() | {binary, binary()}
}).

% Convenience macros user in rest_req, they will be processed before passed
% further to entity logic.
% Injects the value of binding into this placeholder
-define(BINDING(Key), {binding, Key}).
% Injects the id of authenticated client into this placeholder
-define(CLIENT_ID, client_id).
% Injects the IP of requesting client into this placeholder (as binary)
-define(CLIENT_IP, client_ip).

-endif.
