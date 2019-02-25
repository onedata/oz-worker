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

-define(REST_LISTENER, rest).
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
    code = 200 :: integer(),
    headers = #{} :: #{binary() => binary()},
    body = {binary, <<"">>} :: json_utils:json_term() | {binary, binary()}
}).

% Convenience macros user in rest_req, they will be processed before passed
% further to entity logic.
% Injects the value of binding into this placeholder
-define(BINDING(__KEY), {binding, __KEY}).
% Injects the id of authenticated client into this placeholder
-define(CLIENT_ID, client_id).
% Injects the IP of requesting client into this placeholder (as binary)
-define(CLIENT_IP, client_ip).

% Defines with HTTP codes
-define(HTTP_200_OK, 200).
-define(HTTP_201_CREATED, 201).
-define(HTTP_202_ACCEPTED, 202).
-define(HTTP_204_NO_CONTENT, 204).

-define(HTTP_400_BAD_REQUEST, 400).
-define(HTTP_401_UNAUTHORIZED, 401).
-define(HTTP_403_FORBIDDEN, 403).
-define(HTTP_404_NOT_FOUND, 404).

-define(HTTP_500_INTERNAL_SERVER_ERROR, 500).
-define(HTTP_501_NOT_IMPLEMENTED, 501).
-define(HTTP_503_SERVICE_UNAVAILABLE, 503).

-endif.
