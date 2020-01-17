%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for interacting with oz-worker's GraphSync in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_gs).
-author("Lukasz Opiola").

% Types of available GraphSync endpoints
-type endpoint() :: gui | oneprovider.

-include("ozt.hrl").

%% API
-export([connect_and_request/3]).
-export([normalize_client_auth/1]).
-export([endpoint_url/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec connect_and_request(endpoint(), gs_protocol:client_auth(),
    gs_protocol:req_wrapper() | gs_protocol:rpc_req() | gs_protocol:graph_req() | gs_protocol:unsub_req()) ->
    {ok, gs_protocol:rpc_resp() | gs_protocol:graph_resp() | gs_protocol:unsub_resp()} | errors:error().
connect_and_request(Endpoint, ClientAuth, GsReq) ->
    ConnectResult = gs_client:start_link(
        endpoint_url(Endpoint),
        normalize_client_auth(ClientAuth),
        ?SUPPORTED_PROTO_VERSIONS,
        fun(_) -> ok end,
        ozt_http:ssl_opts()
    ),
    case ConnectResult of
        {error, _} = Error ->
            Error;
        {ok, GsClient, _} ->
            Result = gs_client:sync_request(GsClient, GsReq),
            gs_client:kill(GsClient),
            Result
    end.


-spec normalize_client_auth(gs_protocol:client_auth() | {token, tokens:token()}) ->
    gs_protocol:client_auth().
normalize_client_auth(undefined) -> undefined;
normalize_client_auth(nobody) -> nobody;
normalize_client_auth({token, Token}) -> {token, ozt_tokens:ensure_serialized(Token)}.


-spec endpoint_url(endpoint()) -> http_client:url().
endpoint_url(oneprovider) ->
    ozt_http:build_url(wss, <<?PROVIDER_GRAPH_SYNC_WS_PATH>>);
endpoint_url(gui) ->
    ozt_http:build_url(wss, <<?GUI_GRAPH_SYNC_WS_PATH>>).
