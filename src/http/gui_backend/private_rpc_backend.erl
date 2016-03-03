%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements callback_backend_behaviour.
%%% It is used to handle callbacks from the client - such requests that do not
%%% correspond to underlying models. For example -
%%% 'give me the name of current user'.
%%% @end
%%%-------------------------------------------------------------------
-module(private_rpc_backend).
-author("Lukasz Opiola").
-behaviour(rpc_backend_behaviour).

-compile([export_all]).

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([handle/2]).

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
    Provider = provider_to_provider_id(ProviderBin),
    HandlerModule = auth_config:get_provider_module(Provider),
    {ok, URL} = HandlerModule:get_redirect_url(true),
    {ok, URL};

handle(<<"getSupportToken">>, [{<<"spaceId">>, SpaceId}]) ->
    ?dump({getSupportToken, SpaceId}),
    Client = #client{type = user, id = g_session:get_user_id()},
    {ok, Token} = token_logic:create(
        Client, space_support_token, {space, SpaceId}),
    ?dump(Token),
    {ok, Token};

handle(<<"getRedirectURL">>, [{<<"providerId">>, ProviderId}]) ->
    UserId = g_session:get_user_id(),
    {ok, ProviderData} = provider_logic:get_data(ProviderId),
    RedirectionPoint = proplists:get_value(redirectionPoint, ProviderData),
    {ok, {_Scheme, _UserInfo, _HostStr, Port, _Path, _Query}} =
        http_uri:parse(str_utils:to_list(RedirectionPoint)),
    % @todo check if provider is online, if not push update of model
    {ok, _RedURI} = auth_logic:get_redirection_uri(UserId, ProviderId, Port).


provider_to_provider_id(<<"github">>) -> github;
provider_to_provider_id(<<"plgrid">>) -> plgrid;
provider_to_provider_id(<<"google">>) -> google;
provider_to_provider_id(<<"dropbox">>) -> dropbox;
provider_to_provider_id(<<"facebook">>) -> facebook.

