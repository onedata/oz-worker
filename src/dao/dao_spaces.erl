%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module provides high level DB API for handling spaces documents.
%%% @end
%%%-------------------------------------------------------------------
-module(dao_spaces).
-author("Tomasz Lichon").

-include("dao/dao_spaces.hrl").
-include("dao/dao_types.hrl").
-include("dao/dao_external.hrl").

%% API
-export([save_space/1, remove_space/1, exist_space/1, get_space/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Saves space to DB. Argument should be either #space{} record
%% (if you want to save it as new document) <br/>
%% or #db_document{} that wraps #space{} if you want to update descriptor in DB. <br/>
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #db_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
%%--------------------------------------------------------------------
-spec save_space(Space :: space_info() | space_doc()) -> {ok, space_id()} | {error, any()} | no_return().
save_space(#space{} = Space) ->
    save_space(#db_document{record = Space});
save_space(#db_document{record = #space{}, uuid = UUID} = SpaceDoc) when is_list(UUID) ->
    dao_external:set_db(?SYSTEM_DB_NAME),
    dao_records:save_record(SpaceDoc).

%%--------------------------------------------------------------------
%% @doc Removes space from DB
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
%%--------------------------------------------------------------------
-spec remove_space(SpaceId :: uuid()) ->
    ok | {error, any()} | no_return().
remove_space(SpaceId) ->
    dao_external:set_db(?SYSTEM_DB_NAME),
    dao_records:remove_record(SpaceId).

%%--------------------------------------------------------------------
%% @doc Checks whether space exists in DB.
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
%%--------------------------------------------------------------------
-spec exist_space(SpaceId :: uuid()) -> {ok, true | false} | {error, any()}.
exist_space(SpaceId) ->
    dao_external:set_db(?SYSTEM_DB_NAME),
    dao_records:exist_record(SpaceId).

%%--------------------------------------------------------------------
%% @doc Gets space from DB
%% Non-error return value is always {ok, #db_document{record = #space}.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #db_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
%%--------------------------------------------------------------------
-spec get_space(SpaceId :: uuid()) -> {ok, space_doc()} | {error, any()} | no_return().
get_space(SpaceId) ->
    dao_external:set_db(?SYSTEM_DB_NAME),
    {ok, #db_document{record = #space{}}} = dao_records:get_record(SpaceId).
