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

-include("registered_names.hrl").
-include_lib("esaml/include/esaml.hrl").
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
    {ok, ZoneName} = oz_worker:get_env(oz_name),
    BrandSubtitle = oz_worker:get_env(brand_subtitle, ""),
    LoginNotification = oz_worker:get_env(login_notification, ""),
    {_AppId, _AppName, AppVersion} = lists:keyfind(
        ?APP_NAME, 1, application:loaded_applications()
    ),
    {ok, [
        {<<"zoneName">>, str_utils:to_binary(ZoneName)},
        {<<"brandSubtitle">>, str_utils:to_binary(BrandSubtitle)},
        {<<"loginNotification">>, str_utils:to_binary(LoginNotification)},
        {<<"serviceVersion">>, str_utils:to_binary(AppVersion)}
    ]};

handle(<<"getSupportedAuthorizers">>, _) ->
    case oz_worker:get_env(dev_mode) of
        {ok, true} ->
            % If dev mode is enabled, always return basic auth and just one
            % dummy provider which will redirect to /dev_login page.
            {ok, [
                {<<"authorizers">>, [
                    #{
                        <<"id">> => <<"onepanel">>,
                        <<"displayName">> => <<"Onepanel account">>,
                        <<"iconPath">> => <<"/assets/images/auth-providers/onepanel.svg">>,
                        <<"iconBackgroundColor">> => <<"#4BD187">>
                    },
                    #{
                        <<"id">> => <<"devLogin">>,
                        <<"displayName">> => <<"Developer Login">>,
                        <<"iconPath">> => <<"/assets/images/auth-providers/default.svg">>
                    }
                ]}
            ]};
        _ ->
            {ok, [
                {<<"authorizers">>, auth_config:get_supported_idps()}
            ]}
    end;

handle(<<"getLoginEndpoint">>, [{<<"provider">>, IdPBin}]) ->
    case oz_worker:get_env(dev_mode) of
        {ok, true} ->
            {ok, [
                {<<"method">>, <<"get">>},
                {<<"url">>, <<"/dev_login">>},
                {<<"formData">>, null}
            ]};
        _ ->
            IdP = binary_to_atom(IdPBin, utf8),
            {ok, Map} = auth_logic:get_login_endpoint(IdP, false),
            {ok, maps:to_list(Map)}
    end.
