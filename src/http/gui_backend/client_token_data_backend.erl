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
%% {@link data_backend_behaviour} callback init/0.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback find/2.
%% @end
%%--------------------------------------------------------------------
-spec find(ResourceType :: binary(), Id :: binary()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
find(<<"clienttoken">>, _Id) ->
    gui_error:report_error(<<"Not iplemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback find_all/1.
%% @end
%%--------------------------------------------------------------------
-spec find_all(ResourceType :: binary()) ->
    {ok, [proplists:proplist()]} | gui_error:error_result().
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
%% {@link data_backend_behaviour} callback find_query/2.
%% @end
%%--------------------------------------------------------------------
-spec find_query(ResourceType :: binary(), Data :: proplists:proplist()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
find_query(<<"clienttoken">>, _Data) ->
    gui_error:report_error(<<"Not iplemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback create_record/2.
%% @end
%%--------------------------------------------------------------------
-spec create_record(RsrcType :: binary(), Data :: proplists:proplist()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
create_record(<<"clienttoken">>, _Data) ->
    UserId = g_session:get_user_id(),
    Token = auth_logic:gen_token(UserId),
    user_logic:add_client_token(UserId, Token),
    {ok, [
        {<<"id">>, Token}
    ]}.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback update_record/3.
%% @end
%%--------------------------------------------------------------------
-spec update_record(RsrcType :: binary(), Id :: binary(),
    Data :: proplists:proplist()) ->
    ok | gui_error:error_result().
update_record(<<"clienttoken">>, _TokenId, _Data) ->
    gui_error:report_error(<<"Not iplemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback delete_record/2.
%% @end
%%--------------------------------------------------------------------
-spec delete_record(RsrcType :: binary(), Id :: binary()) ->
    ok | gui_error:error_result().
delete_record(<<"clienttoken">>, Token) ->
    UserId = g_session:get_user_id(),
    {ok, Macaroon} = token_utils:deserialize(Token),
    Identifier = macaroon:identifier(Macaroon),
    onedata_auth:delete(Identifier),
    user_logic:delete_client_token(UserId, Token).
