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
-spec handle(FunctionId :: binary(), RequestData :: term()) ->
    ok | {ok, ResponseData :: term()} | gui_error:error_result().
handle(<<"getZoneName">>, _) ->
    {ok, ZoneName} = application:get_env(?APP_NAME, oz_name),
    {_AppId, _AppName, AppVersion} = lists:keyfind(
        ?APP_NAME, 1, application:loaded_applications()
    ),
    {ok, [
        {<<"zoneName">>, str_utils:to_binary(ZoneName)},
        {<<"serviceVersion">>, str_utils:to_binary(AppVersion)}
    ]};

handle(<<"getSupportedAuthorizers">>, _) ->
    case application:get_env(?APP_NAME, dev_mode) of
        {ok, true} ->
            % If dev mode is enabled, always return basic auth and just one
            % dummy provider which will redirect to /dev_login page.
            {ok, [
                {<<"authorizers">>, [<<"basicAuth">>, <<"plgrid">>]}
            ]};
        _ ->
            % Production mode, return providers from config
            % get_auth_providers() returns list of atoms
            ProvidersAtoms = auth_config:get_auth_providers(),
            Providers =
                [str_utils:to_binary(Provider) || Provider <- ProvidersAtoms],
            {ok, [
                {<<"authorizers">>, Providers}
            ]}
    end;

handle(<<"getLoginEndpoint">>, [{<<"provider">>, ProviderBin}]) ->
    case application:get_env(?APP_NAME, dev_mode) of
        {ok, true} ->
            {ok, [
                {<<"url">>, <<"/dev_login">>}
            ]};
        _ ->
            Provider = binary_to_atom(ProviderBin, utf8),
            HandlerModule = auth_config:get_provider_module(Provider),
            {ok, URL} = HandlerModule:get_redirect_url(false),
            {ok, [
                {<<"url">>, URL}
            ]}
    end.
