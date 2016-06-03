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
-spec handle(FunctionId :: binary(), RequestData :: term()) ->
    ok | {ok, ResponseData :: term()} | gui_error:error_result().
handle(<<"getUserAlias">>, _) ->
    UserId = g_session:get_user_id(),
    {ok, #onedata_user{
        alias = Alias
    }} = user_logic:get_user(UserId),
    UserAlias = case str_utils:to_binary(Alias) of
        <<"">> -> null;
        Bin -> Bin
    end,
    {ok, [
        {<<"userAlias">>, UserAlias}
    ]};

handle(<<"changePassword">>, Props) ->
    random:seed(now()),
    case random:uniform(2) of
        1 ->
            ok;
        2 ->
            gui_error:report_error(<<"random error">>)
    end;
%%    OldPassword = proplists:get_value(<<"oldPassword">>, Props),
%%    NewPassword = proplists:get_value(<<"newPassword">>, Props),
%%    {ok, UserData} = user_logic:get_data()
%%
%%    UserId = g_session:get_user_id(),
%%    {ok, #onedata_user{
%%        alias = Alias
%%    }} = user_logic:get_user(UserId),
%%    UserAlias = case str_utils:to_binary(Alias) of
%%        <<"">> -> null;
%%        Bin -> Bin
%%    end,
%%    {ok, [
%%        {<<"userAlias">>, UserAlias}
%%    ]};

handle(<<"setUserAlias">>, [{<<"userAlias">>, NewAlias}]) ->
    UserId = g_session:get_user_id(),
    case user_logic:modify(UserId, [{alias, NewAlias}]) of
        ok ->
            {ok, [
                {<<"userAlias">>, NewAlias}
            ]};
        {error, disallowed_prefix} ->
            gui_error:report_warning(
                <<"Alias cannot start with \"", ?NO_ALIAS_UUID_PREFIX, "\".">>);
        {error, invalid_alias} ->
            gui_error:report_warning(
                <<"Alias can contain only lowercase letters and digits, and "
                "must be at least 5 characters long.">>);
        {error, alias_occupied} ->
            gui_error:report_warning(
                <<"This alias is occupied by someone else. "
                "Please choose other alias.">>);
        {error, alias_conflict} ->
            gui_error:report_warning(
                <<"This alias is occupied by someone else. "
                "Please choose other alias.">>);
        _ ->
            gui_error:report_warning(
                <<"Cannot change alias due to unknown error.">>)
    end;

handle(<<"getConnectAccountEndpoint">>, [{<<"provider">>, ProviderBin}]) ->
    Provider = binary_to_atom(ProviderBin, utf8),
    HandlerModule = auth_config:get_provider_module(Provider),
    {ok, URL} = HandlerModule:get_redirect_url(true),
    {ok, [
        {<<"url">>, URL}
    ]};

handle(<<"getTokenProviderSupportSpace">>, [{<<"spaceId">>, SpaceId}]) ->
    Client = #client{type = user, id = g_session:get_user_id()},
    {ok, Token} = token_logic:create(
        Client, space_support_token, {space, SpaceId}),
    {ok, [
        {<<"token">>, Token}
    ]};

handle(<<"getProviderRedirectURL">>, [{<<"providerId">>, ProviderId}]) ->
    UserId = g_session:get_user_id(),
    % @todo check if provider is online, if not push update of model
    {ok, URL} = auth_logic:get_redirection_uri(UserId, ProviderId),
    {ok, [
        {<<"url">>, URL}
    ]}.
