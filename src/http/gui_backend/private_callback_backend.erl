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
-module(private_callback_backend).
-author("Lukasz Opiola").

-compile([export_all]).

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([callback/2]).

callback(<<"sessionDetails">>, _) ->
    {ok, #document{value = #onedata_user{name = Name}}} =
        onedata_user:get(g_session:get_user_id()),
    FirstLogin = g_session:get_value(firstLogin, false),
    Res = [
        {<<"userName">>, Name},
        {<<"firstLogin">>, FirstLogin}
    ],
    ?alert("~p", [Res]),
    {ok, Res};

callback(<<"getConnectAccountEndpoint">>, [{<<"provider">>, ProviderBin}]) ->
    Provider = provider_to_provider_id(ProviderBin),
    HandlerModule = auth_config:get_provider_module(Provider),
    {ok, URL} = HandlerModule:get_redirect_url(true),
    {ok, URL};

callback(<<"getSupportToken">>, Data) ->
    ?dump(Data),
    {ok, <<"ads789f6ads789r623487g523guy45aegsyf87adstyf">>};

callback(<<"getRedirectURL">>, Data) ->
    ?dump(Data),
    {ok, <<"https://google.com">>}.


provider_to_provider_id(<<"github">>) -> github;
provider_to_provider_id(<<"plgrid">>) -> plgrid;
provider_to_provider_id(<<"google">>) -> google;
provider_to_provider_id(<<"dropbox">>) -> dropbox;
provider_to_provider_id(<<"facebook">>) -> facebook.
