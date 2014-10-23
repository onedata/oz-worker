%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module provides high level DB API for handling groups documents.
%% @end
%% ===================================================================
-module(dao_groups).
-author("Tomasz Lichon").

%% Includes
-include("dao/dao_groups.hrl").
-include("dao/dao_types.hrl").
-include("dao/dao_external.hrl").

%% API
-export([save_group/1, remove_group/1, exist_group/1, get_group/1]).

%% save_group/1
%% ====================================================================
%% @doc Saves group to DB. Argument should be either #group{} record
%% (if you want to save it as new document) <br/>
%% or #db_document{} that wraps #group{} if you want to update descriptor in DB. <br/>
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #db_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec save_group(Group :: group_info() | group_doc()) -> {ok, group_id()} | {error, any()} | no_return().
%% ====================================================================
save_group(#user_group{} = Group) ->
	save_group(#db_document{record = Group});
save_group(#db_document{record = #user_group{}, uuid = UUID} = GroupDoc) when is_list(UUID) ->
    dao_external:set_db(?SYSTEM_DB_NAME),
	dao_records:save_record(GroupDoc).


%% remove_group/1
%% ====================================================================
%% @doc Removes group from DB
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec remove_group(GroupId:: uuid()) ->
	ok | {error, any()} | no_return().
%% ====================================================================
remove_group(GroupId) ->
    dao_external:set_db(?SYSTEM_DB_NAME),
    dao_records:remove_record(GroupId).

%% exist_group/1
%% ====================================================================
%% @doc Checks whether group exists in DB.
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec exist_group(GroupId :: uuid()) -> {ok, true | false} | {error, any()}.
%% ====================================================================
exist_group(GroupId) ->
    dao_external:set_db(?SYSTEM_DB_NAME),
    dao_records:exist_record(GroupId).

%% get_group/1
%% ====================================================================
%% @doc Gets group from DB
%% Non-error return value is always {ok, #db_document{record = #group}.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #db_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec get_group(GroupId :: uuid()) -> {ok, group_doc()} | {error, any()} | no_return().
%% ====================================================================
get_group(GroupId) ->
    dao_external:set_db(?SYSTEM_DB_NAME),
    {ok, #db_document{record = #user_group{}}} = dao_records:get_record(GroupId).
