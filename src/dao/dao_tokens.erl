%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module provides high level DB API for handling tokens documents.
%% @end
%% ===================================================================
-module(dao_tokens).
-author("Tomasz Lichon").

%% Includes
-include("dao/dao_tokens.hrl").
-include("dao/dao.hrl").
-include("dao/dao_types.hrl").

%% API
-export([save_token/1, remove_token/1, exist_token/1, get_token/1]).

%% save_token/1
%% ====================================================================
%% @doc Saves token to DB. Argument should be either #token{} record
%% (if you want to save it as new document) <br/>
%% or #veil_document{} that wraps #token{} if you want to update descriptor in DB. <br/>
%% See {@link dao:save_record/1} and {@link dao:get_record/1} for more details about #veil_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao:handle/2} instead (See {@link dao:handle/2} for more details).
%% @end
-spec save_token(Token :: token_info() | token_doc()) -> {ok, token_id()} | {error, any()} | no_return().
%% ====================================================================
save_token(#token{} = Token) ->
	save_token(#veil_document{record = Token});
save_token(#veil_document{record = #token{}, uuid = UUID} = TokenDoc) when is_list(UUID) ->
	dao:save_record(TokenDoc).


%% remove_token/1
%% ====================================================================
%% @doc Removes token from DB
%% Should not be used directly, use {@link dao:handle/2} instead (See {@link dao:handle/2} for more details).
%% @end
-spec remove_token(TokenId:: uuid()) ->
	ok | {error, any()} | no_return().
%% ====================================================================
remove_token(TokenId) ->
	dao:remove_record(TokenId).

%% exist_token/1
%% ====================================================================
%% @doc Checks whether token exists in DB.
%% Should not be used directly, use {@link dao:handle/2} instead (See {@link dao:handle/2} for more details).
%% @end
-spec exist_token(TokenId :: uuid()) -> {ok, true | false} | {error, any()}.
%% ====================================================================
exist_token(TokenId) ->
	dao:exist_record(TokenId).

%% get_token/1
%% ====================================================================
%% @doc Gets token from DB
%% Non-error return value is always {ok, #veil_document{record = #token}.
%% See {@link dao:save_record/1} and {@link dao:get_record/1} for more details about #veil_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao:handle/2} instead (See {@link dao:handle/2} for more details).
%% @end
-spec get_token(TokenId :: uuid()) -> {ok, token_doc()} | {error, any()} | no_return().
%% ====================================================================
get_token(TokenId) ->
	dao:get_record(TokenId).