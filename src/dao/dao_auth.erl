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
    get_authorization/1, get_expired_authorizations_ids/1,
    get_authorization_by_code/1]).
-export([save_access/1, remove_access/1, exist_access/1, get_access/1,
    get_access_by_key/2, get_accesses_by_user/1, get_access_by_user_and_provider/2]).


%% save_authorization/1
%% ====================================================================
%% @doc Saves authorization to DB. Argument should be either #authorization{}
%% record (if you want to save it as new document) or #db_document{} that
%% wraps #authorization{} if you want to update descriptor in DB.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1}
%% for more details about #db_document{} wrapper.
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead
%% (See {@link dao_worker:handle_call/3} for more details).
%% @end
%% ====================================================================
-spec save_authorization(Authorization :: authorization_info()
                                        | authorization_doc()) ->
    {ok, authorization_id()} | {error, any()} | no_return().
%% ====================================================================
save_authorization(#authorization{} = Authorization) ->
    save_authorization(#db_document{record = Authorization});
save_authorization(#db_document{record = #authorization{}, uuid = UUID} = AuthorizationDoc) when is_list(UUID) ->
    dao_external:set_db(?AUTHORIZATION_DB_NAME),
    dao_records:save_record(AuthorizationDoc).


%% remove_authorization/1
%% ====================================================================
%% @doc Removes authorization from DB.
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead
%% (See {@link dao_worker:handle_call/3} for more details).
%% @end
%% ====================================================================
-spec remove_authorization(AuthorizationId:: authorization_id()) ->
    ok | {error, any()} | no_return().
%% ====================================================================
remove_authorization(AuthorizationId) ->
    dao_external:set_db(?AUTHORIZATION_DB_NAME),
    dao_records:remove_record(vcn_utils:ensure_list(AuthorizationId)).


%% exist_authorization/1
%% ====================================================================
%% @doc Checks whether authorization exists in DB.
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead
%% (See {@link dao_worker:handle_call/3} for more details).
%% @end
%% ====================================================================
-spec exist_authorization(AuthorizationId :: authorization_id()) ->
    {ok, boolean()} | {error, any()}.
%% ====================================================================
exist_authorization(AuthorizationId) ->
    dao_external:set_db(?AUTHORIZATION_DB_NAME),
    dao_records:exist_record(vcn_utils:ensure_list(AuthorizationId)).


%% get_authorization/1
%% ====================================================================
%% @doc Gets authorization from DB.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1}
%% for more details about #db_document{} wrapper.
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead
%% (See {@link dao_worker:handle_call/3} for more details).
%% @end
%% ====================================================================
-spec get_authorization(AuthorizationId :: authorization_id()) ->
    {ok, authorization_doc()} | {error, any()} | no_return().
%% ====================================================================
get_authorization(AuthorizationId) ->
    dao_external:set_db(?AUTHORIZATION_DB_NAME),
    {ok, #db_document{record = #authorization{}}} =
        dao_records:get_record(vcn_utils:ensure_list(AuthorizationId)).


%% get_authorization_by_code/1
%% ====================================================================
%% @doc Gets authorization from DB by an authorization code. The function
%% doesn't throw when such record doesn't exist, instead returning
%% {error, not_found}.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1}
%% for more details about #db_document{} wrapper.
%% @end
%% ====================================================================
-spec get_authorization_by_code(Code :: binary()) ->
    {ok, authorization_doc()} | {error, not_found}.
%% ====================================================================
get_authorization_by_code(Code) ->
    View = ?AUTHORIZATION_BY_CODE,
    QueryArgs = #view_query_args{keys = [<<?RECORD_FIELD_BINARY_PREFIX, Code/binary>>],
                                 include_docs = true},

    case dao_records:list_records(View, QueryArgs) of
        {ok, #view_result{rows = [#view_row{doc = Doc}]}} ->
            #db_document{record = #authorization{}} = Doc,
            {ok, Doc};
        {ok, #view_result{rows = []}} ->
            ?debug("Couldn't find authorization with code ~p", [Code]),
            {error, not_found}
    end.


%% get_expired_authorizations_ids/1
%% ====================================================================
%% @doc Gets ids of expired authorization documents from DB.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1}
%% for more details about #db_document{} wrapper.
%% @end
%% ====================================================================
-spec get_expired_authorizations_ids(Limit :: pos_integer()) ->
    {ok, [access_id()]}.
%% ====================================================================
get_expired_authorizations_ids(Limit) ->
    Now = vcn_utils:time(),
    View = ?AUTHORIZATION_BY_EXPIRATION,
    QueryArgs = #view_query_args{start_key = 0, end_key = Now, limit = Limit},

    {ok, #view_result{rows = Rows}} = dao_records:list_records(View, QueryArgs),
    {ok, [Row#view_row.id || Row <- Rows]}.


%% save_access/1
%% ====================================================================
%% @doc Saves access to DB. Argument should be either #access{} record
%% (if you want to save it as new document) or #db_document{} that wraps
%% #access{} if you want to update descriptor in DB.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1}
%% for more details about #db_document{} wrapper.
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead
%% (See {@link dao_worker:handle_call/3} for more details).
%% @end
%% ====================================================================
-spec save_access(Access :: access_info() | access_doc()) ->
    {ok, access_id()} | {error, any()} | no_return().
%% ====================================================================
save_access(#access{} = Access) ->
    save_access(#db_document{record = Access});
save_access(#db_document{record = #access{}, uuid = UUID} = AccessDoc) when is_list(UUID) ->
    dao_external:set_db(?AUTHORIZATION_DB_NAME),
    dao_records:save_record(AccessDoc).


%% remove_access/1
%% ====================================================================
%% @doc Removes access from DB
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead
%% (See {@link dao_worker:handle_call/3} for more details).
%% @end
%% ====================================================================
-spec remove_access(AccessId:: access_id()) ->
    ok | {error, any()} | no_return().
%% ====================================================================
remove_access(AccessId) ->
    dao_external:set_db(?AUTHORIZATION_DB_NAME),
    dao_records:remove_record(AccessId).


%% exist_access/1
%% ====================================================================
%% @doc Checks whether access exists in DB.
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead
%% (See {@link dao_worker:handle_call/3} for more details).
%% @end
%% ====================================================================
-spec exist_access(AccessId :: uuid()) ->
    {ok, boolean()} | {error, any()}.
%% ====================================================================
exist_access(AccessId) ->
    dao_external:set_db(?AUTHORIZATION_DB_NAME),
    dao_records:exist_record(AccessId).


%% get_access/1
%% ====================================================================
%% @doc Gets access from DB
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1}
%% for more details about #db_document{} wrapper.
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead
%% (See {@link dao_worker:handle_call/3} for more details).
%% @end
%% ====================================================================
-spec get_access(AccessId :: access_id() | binary()) ->
    {ok, access_doc()} | {error, any()} | no_return().
%% ====================================================================
get_access(AccessId) ->
    dao_external:set_db(?AUTHORIZATION_DB_NAME),
    LAccessId = vcn_utils:ensure_list(AccessId),
    {ok, #db_document{record = #access{}}} = dao_records:get_record(LAccessId).


%% get_access_by_key/2
%% ====================================================================
%% @doc Gets access from DB by a given key and value. The function
%% doesn't throw when such record doesn't exist, instead returning
%% {error, not_found}.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1}
%% for more details about #db_document{} wrapper.
%% @end
%% ====================================================================
-spec get_access_by_key(Key :: refresh_token | token | token_hash, Value :: binary()) ->
    {ok, access_doc()} | {error, not_found}.
%% ====================================================================
get_access_by_key(Key, Value) ->
    View = case Key of
        refresh_token -> ?ACCESS_BY_REFRESH_TOKEN_VIEW;
        token_hash -> ?ACCESS_BY_TOKEN_HASH;
        token -> ?ACCESS_BY_TOKEN
    end,

    QueryArgs = #view_query_args{keys = [<<?RECORD_FIELD_BINARY_PREFIX, Value/binary>>],
                                 include_docs = true},

    case dao_records:list_records(View, QueryArgs) of
        {ok, #view_result{rows = [#view_row{doc = Doc}]}} ->
            #db_document{record = #access{}} = Doc,
            {ok, Doc};
        {ok, #view_result{rows = []}} ->
            ?debug("Couldn't find access by ~p with value ~p", [Key, Value]),
            {error, not_found}
    end.


%% get_access_by_user_and_provider/2
%% ====================================================================
%% @doc Gets access from the DB by a given userId and providerId. The function
%% doesn't throw when such record doesn't exist, instead returning
%% {error, not_found}.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1}
%% for more details about #db_document{} wrapper.
%% @end
%% ====================================================================
-spec get_access_by_user_and_provider(UserId :: binary(), ProviderId :: binary()) ->
    {ok, access_doc()} | {error, not_found}.
%% ====================================================================
get_access_by_user_and_provider(UserId, ProviderId) ->
    View = ?ACCESS_BY_USER_AND_PROVIDER,
    QueryArgs = #view_query_args{keys = [
        [<<?RECORD_FIELD_BINARY_PREFIX, UserId/binary>>,
         <<?RECORD_FIELD_BINARY_PREFIX, ProviderId/binary>>]],
        include_docs = true},

    case dao_records:list_records(View, QueryArgs) of
        {ok, #view_result{rows = [#view_row{doc = Doc}]}} ->
            #db_document{record = #access{}} = Doc,
            {ok, Doc};
        {ok, #view_result{rows = []}} ->
            ?debug("Couldn't find access by User ~p and Provider ~p", [UserId, ProviderId]),
            {error, not_found}
    end.


%% get_accesses_by_user/1
%% ====================================================================
%% @doc Gets a list of authorizations for a given user.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1}
%% for more details about #db_document{} wrapper.
%% @end
%% ====================================================================
-spec get_accesses_by_user(UserId :: binary()) ->
    {ok, [authorization_doc()]}.
%% ====================================================================
get_accesses_by_user(UserId) ->
    View = ?ACCESS_BY_USER_ID,
    QueryArgs = #view_query_args{keys = [<<?RECORD_FIELD_BINARY_PREFIX, UserId/binary>>],
                                 include_docs = true},

    {ok, #view_result{rows = Rows}} = dao_records:list_records(View, QueryArgs),
    Accesses = [#db_document{record = #access{}} = Row#view_row.doc || Row <- Rows],
    {ok, Accesses}.
