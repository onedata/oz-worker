%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module provides high level DB API for handling tokens documents.
%%% @end
%%%-------------------------------------------------------------------
-module(dao_tokens).
-author("Tomasz Lichon").

-include("dao/dao_tokens.hrl").
-include("dao/dao_types.hrl").
-include("dao/dao_external.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("dao/include/dao_helper.hrl").

%% API
-export([save_token/1, remove_token/1, exist_token/1, get_token/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Saves token to DB. Argument should be either #token{} record
%% (if you want to save it as new document) <br/>
%% or #db_document{} that wraps #token{} if you want to update descriptor in DB. <br/>
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #db_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
%%--------------------------------------------------------------------
-spec save_token(Token :: token_info() | token_doc()) -> {ok, token_id()} | {error, any()} | no_return().
save_token(#token{} = Token) ->
    save_token(#db_document{record = Token});
save_token(#db_document{record = #token{}, uuid = UUID} = TokenDoc) when is_list(UUID) ->
    dao_external:set_db(?TOKENS_DB_NAME),
    dao_records:save_record(TokenDoc).

%%--------------------------------------------------------------------
%% @doc Removes token from DB
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
%%--------------------------------------------------------------------
-spec remove_token(TokenId :: uuid() | binary()) ->
    ok | {error, any()} | no_return().
remove_token(<<_>> = TokenBin) -> remove_token(binary_to_list(TokenBin));
remove_token(TokenId) ->
    dao_external:set_db(?TOKENS_DB_NAME),
    dao_records:remove_record(TokenId).

%%--------------------------------------------------------------------
%% @doc Checks whether token exists in DB.
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
%%--------------------------------------------------------------------
-spec exist_token(TokenId :: uuid()) -> {ok, true | false} | {error, any()}.
exist_token(TokenId) ->
    dao_external:set_db(?TOKENS_DB_NAME),
    dao_records:exist_record(TokenId).

%%--------------------------------------------------------------------
%% @doc Gets token from DB
%% Non-error return value is always {ok, #db_document{record = #token}.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #db_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
%%--------------------------------------------------------------------
-spec get_token(TokenId :: uuid() | binary()) -> {ok, token_doc()} | {error, any()} | no_return().
get_token(<<_>> = TokenBin) -> get_token(binary_to_list(TokenBin));
get_token(TokenId) ->
    dao_external:set_db(?TOKENS_DB_NAME),
    {ok, #db_document{record = #token{}}} = dao_records:get_record(TokenId).
