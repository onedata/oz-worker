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
-module(public_callback_backend).
-author("Lukasz Opiola").

-compile([export_all]).

-include("gui/common.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([callback/2]).

callback(<<"getLoginEndpoint">>, {<<"provider">>, ProviderBin}) ->
    case application:get_env(?APP_Name, dev_mode) of
        {ok, true} ->
            {ok, <<"/dev_login">>};
        _ ->
            Provider = provider_to_provider_id(ProviderBin),
            HandlerModule = auth_config:get_provider_module(Provider),
            {ok, URL} = HandlerModule:get_redirect_url(false),
            {ok, URL}
    end.


provider_to_provider_id(<<"github">>) -> github;
provider_to_provider_id(<<"plgrid">>) -> plgrid;
provider_to_provider_id(<<"google">>) -> google;
provider_to_provider_id(<<"dropbox">>) -> dropbox;
provider_to_provider_id(<<"facebook">>) -> facebook.
