%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements callback_backend_behaviour.
%%% It is used to handle RPC calls from clients with active session.
%%% @end
%%%-------------------------------------------------------------------
-module(private_rpc_backend).
-author("Lukasz Opiola").
-behaviour(rpc_backend_behaviour).

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([handle/2]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link rpc_backend_behaviour} callback handle/2.
%% @end
%%--------------------------------------------------------------------
handle(<<"sessionDetails">>, _) ->
    {ok, #document{value = #onedata_user{name = Name}}} =
        onedata_user:get(g_session:get_user_id()),
    FirstLogin = g_session:get_value(firstLogin, false),
    Res = [
        {<<"userName">>, Name},
        {<<"firstLogin">>, FirstLogin}
    ],
    ?alert("~p", [Res]),
    {ok, Res};

handle(<<"getConnectAccountEndpoint">>, [{<<"provider">>, ProviderBin}]) ->
    Provider = binary_to_atom(ProviderBin, utf8),
    HandlerModule = auth_config:get_provider_module(Provider),
    {ok, URL} = HandlerModule:get_redirect_url(true),
    {ok, URL};

handle(<<"getSupportToken">>, [{<<"spaceId">>, SpaceId}]) ->
    Client = #client{type = user, id = g_session:get_user_id()},
    {ok, Token} = token_logic:create(
        Client, space_support_token, {space, SpaceId}),
    {ok, Token};

handle(<<"getRedirectURL">>, [{<<"providerId">>, ProviderId}]) ->
    UserId = g_session:get_user_id(),
    % @todo check if provider is online, if not push update of model
    auth_logic:get_redirection_uri(UserId, ProviderId).
