%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module provides high level DB API for handling authorizations documents.
%% @end
%% ===================================================================
-module(dao_auth).
-author("Konrad Zemek").

%% Includes
-include("dao/dao_auth.hrl").
-include("dao/dao_types.hrl").
-include("dao/dao_external.hrl").
-include_lib("dao/include/dao_helper.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([save_authorization/1, remove_authorization/1, exist_authorization/1,
    get_authorization/1]).
-export([save_access/1, remove_access/1, exist_access/1, get_access/1]).


%% save_authorization/1
%% ====================================================================
%% @doc Saves authorization to DB. Argument should be either #authorization{} record
%% (if you want to save it as new document) <br/>
%% or #veil_document{} that wraps #authorization{} if you want to update descriptor in DB. <br/>
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #veil_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec save_authorization(Authorization :: authorization_info() | authorization_doc()) -> {ok, authorization_id()} | {error, any()} | no_return().
%% ====================================================================
save_authorization(#authorization{} = Authorization) ->
    save_authorization(#veil_document{record = Authorization});
save_authorization(#veil_document{record = #authorization{}, uuid = UUID} = AuthorizationDoc) when is_list(UUID) ->
    dao_external:set_db(?AUTHORIZATION_DB_NAME),
    dao_records:save_record(AuthorizationDoc).


%% remove_authorization/1
%% ====================================================================
%% @doc Removes authorization from DB
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec remove_authorization(AuthorizationId:: uuid()) ->
    ok | {error, any()} | no_return().
%% ====================================================================
remove_authorization(AuthorizationId) ->
    dao_external:set_db(?AUTHORIZATION_DB_NAME),
    dao_records:remove_record(AuthorizationId).


%% exist_authorization/1
%% ====================================================================
%% @doc Checks whether authorization exists in DB.
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec exist_authorization(AuthorizationId :: uuid()) -> {ok, true | false} | {error, any()}.
%% ====================================================================
exist_authorization(AuthorizationId) ->
    dao_external:set_db(?AUTHORIZATION_DB_NAME),
    dao_records:exist_record(AuthorizationId).


%% get_authorization/1
%% ====================================================================
%% @doc Gets authorization from DB
%% Non-error return value is always {ok, #veil_document{record = #authorization}.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #veil_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec get_authorization(AuthorizationKey :: uuid() | {atom(), binary() | non_neg_integer()}) ->
    {ok, authorization_doc()} | {error, any()} | no_return().
%% ====================================================================
get_authorization(AuthorizationId) when is_list(AuthorizationId) ->
    dao_external:set_db(?AUTHORIZATION_DB_NAME),
    {ok, #veil_document{record = #authorization{}}} = dao_records:get_record(AuthorizationId);
get_authorization({Key, Value}) ->
    {View, QueryArgs} = case Key of
        code ->
            {?AUTHORIZATION_BY_CODE, #view_query_args{keys =
                [<<?RECORD_FIELD_BINARY_PREFIX, (dao_helper:name(Value))/binary>>],
                include_docs = true
            }};
        expiration_up_to when is_integer(Value) ->
            {?AUTHORIZATION_BY_EXPIRATION, #view_query_args{start_key = 0, end_key = Value}}
    end,

    case dao_records:list_records(View, QueryArgs) of
        {ok, #view_result{rows = Rows}} when is_list(Rows) ->
            {ok, lists:map(fun(Row) -> Row#view_row.doc end, Rows)};
        Other ->
            ?error("Invalid view response: ~p", [Other]),
            throw(invalid_data)
    end.


%% save_access/1
%% ====================================================================
%% @doc Saves access to DB. Argument should be either #access{} record
%% (if you want to save it as new document) <br/>
%% or #veil_document{} that wraps #access{} if you want to update descriptor in DB. <br/>
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #veil_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec save_access(Access :: access_info() | access_doc()) -> {ok, access_id()} | {error, any()} | no_return().
%% ====================================================================
save_access(#access{} = Access) ->
    save_access(#veil_document{record = Access});
save_access(#veil_document{record = #access{}, uuid = UUID} = AccessDoc) when is_list(UUID) ->
    dao_external:set_db(?AUTHORIZATION_DB_NAME),
    dao_records:save_record(AccessDoc).


%% remove_access/1
%% ====================================================================
%% @doc Removes access from DB
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec remove_access(AccessId:: uuid()) ->
    ok | {error, any()} | no_return().
%% ====================================================================
remove_access(AccessId) ->
    dao_external:set_db(?AUTHORIZATION_DB_NAME),
    dao_records:remove_record(AccessId).


%% exist_access/1
%% ====================================================================
%% @doc Checks whether access exists in DB.
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec exist_access(AccessId :: uuid()) -> {ok, true | false} | {error, any()}.
%% ====================================================================
exist_access(AccessId) ->
    dao_external:set_db(?AUTHORIZATION_DB_NAME),
    dao_records:exist_record(AccessId).


%% get_access/1
%% ====================================================================
%% @doc Gets access from DB
%% Non-error return value is always {ok, #veil_document{record = #access}.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #veil_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec get_access(AccessKey :: uuid() | {atom(), binary()}) ->
    {ok, access_doc()} | {error, any()} | no_return().
%% ====================================================================
get_access(AccessId) when is_list(AccessId) ->
    dao_external:set_db(?AUTHORIZATION_DB_NAME),
    {ok, #veil_document{record = #access{}}} = dao_records:get_record(AccessId);
get_access({Key, Value}) ->
    View = case Key of
        refresh_token -> ?ACCESS_BY_REFRESH_TOKEN_VIEW;
        token_hash -> ?ACCESS_BY_TOKEN_HASH;
        token -> ?ACCESS_BY_TOKEN;
        user_id -> ?ACCESS_BY_USER_ID
    end,

    QueryArgs = #view_query_args{keys =
        [<<?RECORD_FIELD_BINARY_PREFIX, (dao_helper:name(Value))/binary>>],
        include_docs = true
    },

    case dao_records:list_records(View, QueryArgs) of
        {ok, #view_result{rows = Rows}} when is_list(Rows) ->
            {ok, lists:map(fun(Row) -> Row#view_row.doc end, Rows)};
        Other ->
            ?error("Invalid view response: ~p", [Other]),
            throw(invalid_data)
    end.
