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
-include("dao/dao_types.hrl").
-include("dao/dao_external.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("dao/include/dao_helper.hrl").

%% API
-export([save_token/1, remove_token/1, exist_token/1, get_token/1,
    get_token_by_value/1]).


%% save_token/1
%% ====================================================================
%% @doc Saves token to DB. Argument should be either #token{} record
%% (if you want to save it as new document) <br/>
%% or #veil_document{} that wraps #token{} if you want to update descriptor in DB. <br/>
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #veil_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec save_token(Token :: token_info() | token_doc()) -> {ok, token_id()} | {error, any()} | no_return().
%% ====================================================================
save_token(#token{} = Token) ->
	save_token(#veil_document{record = Token});
save_token(#veil_document{record = #token{}, uuid = UUID} = TokenDoc) when is_list(UUID) ->
    dao_external:set_db(?TOKENS_DB_NAME),
    dao_records:save_record(TokenDoc).


%% remove_token/1
%% ====================================================================
%% @doc Removes token from DB
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec remove_token(TokenId:: uuid()) ->
	ok | {error, any()} | no_return().
%% ====================================================================
remove_token(TokenId) ->
    dao_external:set_db(?TOKENS_DB_NAME),
    dao_records:remove_record(TokenId).


%% exist_token/1
%% ====================================================================
%% @doc Checks whether token exists in DB.
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec exist_token(TokenId :: uuid()) -> {ok, true | false} | {error, any()}.
%% ====================================================================
exist_token(TokenId) ->
    dao_external:set_db(?TOKENS_DB_NAME),
    dao_records:exist_record(TokenId).


%% get_token/1
%% ====================================================================
%% @doc Gets token from DB
%% Non-error return value is always {ok, #veil_document{record = #token}.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #veil_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec get_token(TokenId :: uuid()) -> {ok, token_doc()} | {error, any()} | no_return().
%% ====================================================================
get_token(TokenId) ->
    dao_external:set_db(?TOKENS_DB_NAME),
    {ok, #veil_document{record = #token{}}} = dao_records:get_record(TokenId).


%% get_token_by_value/1
%% ====================================================================
%% @doc Gets a token from DB by a given value. The function doesn't throw when
%% such record doesn't exist, instead returning {error, not_found}.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1}
%% for more details about #veil_document{} wrapper.
%% @end
%% ====================================================================
-spec get_token_by_value(Value :: binary()) ->
    {ok, token_doc()} | {error, not_found}.
%% ====================================================================
get_token_by_value(Value) ->
    View = ?TOKEN_BY_VALUE,
    QueryArgs = #view_query_args{keys = [<<?RECORD_FIELD_BINARY_PREFIX, Value/binary>>],
                                 include_docs = true},

    case dao_records:list_records(View, QueryArgs) of
        {ok, #view_result{rows = [#view_row{doc = Doc}]}} ->
            #veil_document{record = #access{}} = Doc,
            {ok, Doc};
        {ok, #view_result{rows = []}} ->
            ?warning("Couldn't find token by value ~p", [Value]),
            {error, not_found}
    end.