%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
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
-define(REST_HANDLER_MODULE, n_rest_handler).

%% Record containing the state of REST request.
-record(rest_req, {
    method = get :: n_rest_handler:method(),
    % Entity logic plugin to handle the request
    el_plugin = undefined :: undefined | n_entity_logic:el_plugin(),
    entity_id = undefined :: undefined | n_entity_logic:entity_id(),
    resource = undefined :: undefined | n_entity_logic:resource(),
    % Rest translator plugin to translate the response
    translator = undefined :: module()
}).
%% Record representing REST response.
-record(rest_resp, {
    code = 200 :: integer(),
    headers = #{} :: #{binary() => binary()},
    body = {binary, <<"">>} :: jiffy:json_value() | {binary, binary()}
}).

% Convenience macros user in rest_req
-define(BINDING(__KEY), {binding, __KEY}).
-define(CLIENT_ID, client_id).
-define(COWBOY_REQ, cowboy_req).

% Defines with HTTP codes
-define(HTTP_200_OK, 200).
-define(HTTP_201_CREATED, 201).
-define(HTTP_202_ACCEPTED, 202).
-define(HTTP_204_NO_CONTENT, 204).

-define(HTTP_400_BAD_REQUEST, 401).
-define(HTTP_401_UNAUTHORIZED, 401).
-define(HTTP_403_FORBIDDEN, 403).
-define(HTTP_404_NOT_FOUND, 404).

-define(HTTP_500_INTERNAL_SERVER_ERROR, 500).
-define(HTTP_501_NOT_IMPLEMENTED, 501).

-endif.
