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
handle(<<"changePassword">>, Props) ->
    UserId = gui_session:get_user_id(),
    {ok, #od_user{
        login = Login
    }} = user_logic:get_user(UserId),
    OldPassword = proplists:get_value(<<"oldPassword">>, Props),
    NewPassword = proplists:get_value(<<"newPassword">>, Props),
    case user_logic:change_user_password(Login, OldPassword, NewPassword) of
        ok ->
            ok;
        {error, Binary} when is_binary(Binary) ->
            gui_error:report_warning(Binary);
        _ ->
            gui_error:report_warning(
                <<"Cannot change user password - old password incorrect.">>)
    end;

handle(<<"getConnectAccountEndpoint">>, [{<<"provider">>, ProviderBin}]) ->
    Provider = binary_to_atom(ProviderBin, utf8),
    HandlerModule = auth_config:get_provider_module(Provider),
    {ok, URL} = HandlerModule:get_redirect_url(true),
    {ok, [
        {<<"url">>, URL}
    ]};

handle(<<"getTokenProviderSupportSpace">>, [{<<"spaceId">>, SpaceId}]) ->
    Client = #client{type = user, id = gui_session:get_user_id()},
    {ok, Token} = token_logic:create(
        Client, space_support_token, {space, SpaceId}),
    {ok, [
        {<<"token">>, Token}
    ]};

handle(<<"getProviderRedirectURL">>, [{<<"providerId">>, ProviderId}]) ->
    UserId = gui_session:get_user_id(),
    % @todo check if provider is online, if not push update of model
    {ok, URL} = auth_logic:get_redirection_uri(UserId, ProviderId),
    {ok, [
        {<<"url">>, URL}
    ]};

handle(<<"unsupportSpace">>, Props) ->
    SpaceId = proplists:get_value(<<"spaceId">>, Props),
    ProviderId = proplists:get_value(<<"providerId">>, Props),
    UserId = gui_session:get_user_id(),
    Authorized = space_logic:has_effective_privilege(
        SpaceId, UserId, space_remove_provider
    ),
    case Authorized of
        true ->
            true = space_logic:remove_provider(SpaceId, ProviderId),
            % Push the modified space and provider to the client
            gui_async:push_created(
                <<"space">>, space_data_backend:space_record(SpaceId, UserId)
            ),
            ProviderRecord = provider_data_backend:provider_record(
                ProviderId, UserId
            ),
            % If the provider no longer supports any of user's spaces, delete
            % the record in ember cache.
            case proplists:get_value(<<"spaces">>, ProviderRecord) of
                [] ->
                    gui_async:push_deleted(<<"provider">>, ProviderId);
                _ ->
                    gui_async:push_updated(<<"provider">>, ProviderRecord)
            end,
            ok;
        false ->
            gui_error:report_warning(
                <<"You do not have permissions to unsupport this space. "
                "Those persmissions can be modified in file browser, "
                "'Spaces' tab.">>
            )
    end;


handle(<<"userJoinSpace">>, [{<<"token">>, Token}]) ->
    UserId = gui_session:get_user_id(),
    case token_logic:validate(Token, space_invite_user_token) of
        false ->
            gui_error:report_warning(<<"Invalid token value.">>);
        {true, Macaroon} ->
            {ok, SpaceId} = space_logic:join({user, UserId}, Macaroon),
            % Push the newly joined space to the client's model
            gui_async:push_created(
                <<"space">>, space_data_backend:space_record(SpaceId, UserId)
            ),
            {ok, [{<<"spaceId">>, SpaceId}]}
    end.
