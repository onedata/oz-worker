%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements callback_backend_behaviour.
%%% It is used to handle RPC calls from clients with no session.
%%% @end
%%%-------------------------------------------------------------------
-module(public_rpc_backend).
-author("Lukasz Opiola").
-behaviour(rpc_backend_behaviour).

-include("gui/common.hrl").
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
handle(<<"getSupportedAuthorizers">>, _) ->
    Providers = auth_config:get_auth_providers(),
    % Providers is a list of atoms
    {ok, [str_utils:to_binary(Provider) || Provider <- Providers]};

handle(<<"getLoginEndpoint">>, [{<<"provider">>, ProviderBin}]) ->
    case application:get_env(?APP_Name, dev_mode) of
        {ok, true} ->
            {ok, <<"/dev_login">>};
        _ ->
            Provider = binary_to_atom(ProviderBin, utf8),
            HandlerModule = auth_config:get_provider_module(Provider),
            {ok, URL} = HandlerModule:get_redirect_url(false),
            {ok, URL}
    end.
