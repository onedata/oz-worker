%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module provides high level DB API for handling users documents.
%% @end
%% ===================================================================
-module(dao_users).
-author("Tomasz Lichon").

%% Includes
-include_lib("ctool/include/logging.hrl").
-include_lib("dao/include/dao_helper.hrl").
-include("dao/dao_users.hrl").
-include("dao/dao_types.hrl").
-include("dao/dao_external.hrl").


%% API
-export([save_user/1, remove_user/1, exist_user/1, get_user/1]).

%% save_user/1
%% ====================================================================
%% @doc Saves user to DB. Argument should be either #user{} record
%% (if you want to save it as new document) <br/>
%% or #veil_document{} that wraps #user{} if you want to update descriptor in DB. <br/>
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #veil_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec save_user(User :: user_info() | user_doc()) -> {ok, user_id()} | {error, any()} | no_return().
%% ====================================================================
save_user(#user{} = User) ->
    save_user(#veil_document{record = User});
save_user(#veil_document{record = #user{}, uuid = UUID} = UserDoc) when is_list(UUID) ->
    dao_external:set_db(?USERS_DB_NAME),
    dao_records:save_record(UserDoc).


%% remove_user/1
%% ====================================================================
%% @doc Removes user from DB
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec remove_user(UserId :: uuid()) ->
    ok | {error, any()} | no_return().
%% ====================================================================
remove_user(UserId) ->
    dao_external:set_db(?USERS_DB_NAME),
    dao_records:remove_record(UserId).

%% exist_user/1
%% ====================================================================
%% @doc Checks whether user exists in DB.
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec exist_user(UserId :: uuid()) -> {ok, true | false} | {error, any()}.
%% ====================================================================
exist_user(UserId) ->
    dao_external:set_db(?USERS_DB_NAME),
    dao_records:exist_record(UserId).

%% get_user/1
%% ====================================================================
%% @doc Gets user from DB
%% Non-error return value is always {ok, #veil_document{record = #user}.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #veil_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec get_user(Key) -> {ok, user_doc()} | {error, any()} | no_return() when
    Key :: UserId :: binary() | {connected_account_user_id, {ProviderID :: binary(), UserID :: binary()}} | {email, binary()}.
%% ====================================================================
get_user(UUID) when is_list(UUID) ->
    dao_external:set_db(?USERS_DB_NAME),
    dao_records:get_record(UUID);

get_user({Key, Value}) ->
    dao_external:set_db(?USERS_DB_NAME),

    {View, QueryArgs} = case Key of
                            connected_account_user_id ->
                                {ProviderID, UserID} = Value,
                                {?USER_BY_CONNECTED_ACCOUNT_USER_ID_VIEW, #view_query_args{keys =
                                [
                                    <<?RECORD_FIELD_ATOM_PREFIX,
                                    (gui_str:to_binary(ProviderID))/binary,
                                    "_",
                                    ?RECORD_FIELD_BINARY_PREFIX,
                                    UserID/binary>>
                                ], include_docs = true}};
                            email ->
                                {?USER_BY_EMAIL_VIEW, #view_query_args{keys =
                                [<<?RECORD_FIELD_BINARY_PREFIX, (dao_helper:name(Value))/binary>>], include_docs = true}}
                        end,

    case dao_records:list_records(View, QueryArgs) of
        {ok, #view_result{rows = [#view_row{doc = FDoc}]}} ->
            {ok, FDoc};
        {ok, #view_result{rows = []}} ->
            ?warning("User by ~p: ~p not found", [Key, Value]),
            throw(user_not_found);
        {ok, #view_result{rows = [#view_row{doc = FDoc} | Tail] = AllRows}} ->
            case length(lists:usort(AllRows)) of
                Count when Count > 1 ->
                    ?warning("User ~p is duplicated. Returning first copy. Others: ~p", [FDoc#veil_document.uuid, Tail]);
                _ -> ok
            end,
            {ok, FDoc};
        Other ->
            ?error("Invalid view response: ~p", [Other]),
            throw(invalid_data)
    end.
