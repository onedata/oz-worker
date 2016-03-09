%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements data_backend_behaviour and is used to synchronize
%%% the `clienttoken` model used in Ember application.
%%% @end
%%%-------------------------------------------------------------------
-module(client_token_data_backend).
-author("Lukasz Opiola").
-behaviour(data_backend_behaviour).

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([init/0]).
-export([find/2, find_all/1, find_query/2]).
-export([create_record/2, update_record/3, delete_record/2]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback init/0.
%% @end
%%--------------------------------------------------------------------
init() ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback find/2.
%% @end
%%--------------------------------------------------------------------
find(<<"clienttoken">>, _Ids) ->
    {error, not_iplemented}.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback find_query/2.
%% @end
%%--------------------------------------------------------------------
find_query(<<"clienttoken">>, _Data) ->
    {error, not_iplemented}.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback find_all/1.
%% @end
%%--------------------------------------------------------------------
find_all(<<"clienttoken">>) ->
    UserId = g_session:get_user_id(),
    {ok, ClientTokens} = user_logic:get_client_tokens(UserId),
    Res = lists:map(
        fun(Id) ->
            [{<<"id">>, Id}]
        end, ClientTokens),
    {ok, Res}.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback create_record/2.
%% @end
%%--------------------------------------------------------------------
create_record(<<"clienttoken">>, _Data) ->
    UserId = g_session:get_user_id(),
    Token = auth_logic:gen_token(UserId),
    user_logic:add_client_token(UserId, Token),
    {ok, [
        {<<"id">>, Token}
    ]}.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback update_record/3.
%% @end
%%--------------------------------------------------------------------
update_record(<<"clienttoken">>, _TokenId, _Data) ->
    {error, not_iplemented}.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback delete_record/2.
%% @end
%%--------------------------------------------------------------------
delete_record(<<"clienttoken">>, Token) ->
    UserId = g_session:get_user_id(),
    {ok, Macaroon} = macaroon:deserialize(Token),
    Identifier = macaroon:identifier(Macaroon),
    onedata_auth:delete(Identifier),
    user_logic:delete_client_token(UserId, Token).
