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

-include("rest.hrl").
-include("errors.hrl").
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
    }} = n_user_logic:get(?USER(UserId), UserId),
    OldPassword = proplists:get_value(<<"oldPassword">>, Props),
    NewPassword = proplists:get_value(<<"newPassword">>, Props),
    case n_user_logic:change_user_password(Login, OldPassword, NewPassword) of
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
    Client = ?USER(gui_session:get_user_id()),
    case n_space_logic:create_provider_invite_token(Client, SpaceId) of
        {ok, Token} ->
            {ok, [{<<"token">>, Token}]};
        ?ERROR_UNAUTHORIZED ->
            gui_error:report_warning(
                <<"You do not have permissions to issue support tokens.">>
            )
    end;

handle(<<"getProviderRedirectURL">>, [{<<"providerId">>, ProviderId}]) ->
    UserId = gui_session:get_user_id(),
    % @todo check if provider is online, if not push update of model
    {ok, URL} = auth_logic:get_redirection_uri(UserId, ProviderId),
    {ok, [
        {<<"url">>, URL}
    ]};

handle(<<"unsupportSpace">>, Props) ->
    Client = ?USER(gui_session:get_user_id()),
    SpaceId = proplists:get_value(<<"spaceId">>, Props),
    ProviderId = proplists:get_value(<<"providerId">>, Props),
    UserId = gui_session:get_user_id(),
    case n_space_logic:leave_provider(Client, SpaceId, ProviderId) of
        ok ->
            gui_async:push_updated(
                <<"user">>, user_data_backend:user_record(Client, UserId)
            ),
            ok;
        ?ERROR_UNAUTHORIZED ->
            gui_error:report_warning(
                <<"You do not have permissions to unsupport this space. "
                "Those persmissions can be modified in file browser, "
                "'Spaces' tab.">>
            )
    end;

handle(<<"userJoinSpace">>, [{<<"token">>, Token}]) ->
    UserId = gui_session:get_user_id(),
    case n_user_logic:join_space(?USER(UserId), UserId, Token) of
        ?ERROR_BAD_VALUE_TOKEN(_) ->
            gui_error:report_warning(<<"Invalid token value.">>);
        ?ERROR_BAD_VALUE_BAD_TOKEN_TYPE(_) ->
            gui_error:report_warning(<<"Invalid token type.">>);
        {ok, SpaceId} ->
            % Push user record with a new space list.
            gui_async:push_updated(
                <<"user">>, user_data_backend:user_record(?USER(UserId), UserId)
            ),
            {ok, [{<<"spaceId">>, SpaceId}]}
    end;

handle(<<"userLeaveSpace">>, [{<<"spaceId">>, SpaceId}]) ->
    UserId = gui_session:get_user_id(),
    n_user_logic:leave_space(?USER(UserId), UserId, SpaceId),
    % Push user record with a new space list.
    gui_async:push_updated(
        <<"user">>, user_data_backend:user_record(?USER(UserId), UserId)
    ),
    ok;

handle(<<"getTokenUserJoinGroup">>, [{<<"groupId">>, GroupId}]) ->
    UserId = gui_session:get_user_id(),
    case n_group_logic:create_user_invite_token(?USER(UserId), GroupId) of
        {ok, Token} ->
            {ok, [{<<"token">>, Token}]};
        ?ERROR_UNAUTHORIZED ->
            gui_error:report_warning(
                <<"You do not have permissions to issue invite tokens for users.">>
            )
    end;



handle(<<"userJoinGroup">>, [{<<"token">>, Token}]) ->
    UserId = gui_session:get_user_id(),
    case n_user_logic:join_group(?USER(UserId), UserId, Token) of
        ?ERROR_BAD_VALUE_TOKEN(_) ->
            gui_error:report_warning(<<"Invalid token value.">>);
        ?ERROR_BAD_VALUE_BAD_TOKEN_TYPE(_) ->
            gui_error:report_warning(<<"Invalid token type.">>);
        {ok, GroupId} ->
            % Push user record with a new group list.
            gui_async:push_updated(
                <<"user">>, user_data_backend:user_record(?USER(UserId), UserId)
            ),
            {ok, [{<<"groupId">>, GroupId}]}
    end.
