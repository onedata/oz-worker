%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Http client for handle proxy service
%%% @end
%%%--------------------------------------------------------------------
-module(handle_proxy_client).
-author("Tomasz Lichon").

-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").

%% API
-export([put/4, patch/4, delete/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% PUT operation on handle proxy
%% @end
%%--------------------------------------------------------------------
-spec put(ProxyEndpoint :: handle_service:proxy_endpoint(), URL :: http_client:url(),
    Headers :: http_client:headers(), Body :: http_client:body()) ->
    {ok, http_client:code(), http_client:headers(), http_client:body()} | {error, term()}.
put(ProxyEndpoint, Url, Headers, Body) ->
    http_client:put(<<ProxyEndpoint/binary, Url/binary>>, Headers, Body).

%%--------------------------------------------------------------------
%% @doc
%% PATCH operation on handle proxy
%% @end
%%--------------------------------------------------------------------
-spec patch(ProxyEndpoint :: handle_service:proxy_endpoint(), URL :: http_client:url(),
    Headers :: http_client:headers(), Body :: http_client:body()) ->
    {ok, http_client:code(), http_client:headers(), http_client:body()} | {error, term()}.
patch(ProxyEndpoint, Url, Headers, Body) ->
    http_client:request(patch, <<ProxyEndpoint/binary, Url/binary>>, Headers, Body).

%%--------------------------------------------------------------------
%% @doc
%% DELETE operation on handle proxy
%% @end
%%--------------------------------------------------------------------
-spec delete(ProxyEndpoint :: handle_service:proxy_endpoint(), URL :: http_client:url(),
    Headers :: http_client:headers(), Body :: http_client:body()) ->
    {ok, http_client:code(), http_client:headers(), http_client:body()} | {error, term()}.
delete(ProxyEndpoint, Url, Headers, Body) ->
    http_client:delete(<<ProxyEndpoint/binary, Url/binary>>, Headers, Body).