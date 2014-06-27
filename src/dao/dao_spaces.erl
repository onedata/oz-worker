%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module provides high level DB API for handling spaces documents.
%% @end
%% ===================================================================
-module(dao_spaces).
-author("Tomasz Lichon").

%% Includes
-include("dao/dao_spaces.hrl").
-include("dao/dao_types.hrl").

%% API
-export([save_space/1, remove_space/1, exist_space/1, get_space/1]).

%% save_space/1
%% ====================================================================
%% @doc Saves space to DB. Argument should be either #space{} record
%% (if you want to save it as new document) <br/>
%% or #veil_document{} that wraps #space{} if you want to update descriptor in DB. <br/>
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #veil_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec save_space(Space :: space_info() | space_doc()) -> {ok, space_id()} | {error, any()} | no_return().
%% ====================================================================
save_space(#space{} = Space) ->
	save_space(#veil_document{record = Space});
save_space(#veil_document{record = #space{}, uuid = UUID} = SpaceDoc) when is_list(UUID) ->
    dao_records:save_record(SpaceDoc).


%% remove_space/1
%% ====================================================================
%% @doc Removes space from DB
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec remove_space(SpaceId:: uuid()) ->
	ok | {error, any()} | no_return().
%% ====================================================================
remove_space(SpaceId) ->
    dao_records:remove_record(SpaceId).

%% exist_space/1
%% ====================================================================
%% @doc Checks whether space exists in DB.
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec exist_space(SpaceId :: uuid()) -> {ok, true | false} | {error, any()}.
%% ====================================================================
exist_space(SpaceId) ->
    dao_records:exist_record(SpaceId).

%% get_space/1
%% ====================================================================
%% @doc Gets space from DB
%% Non-error return value is always {ok, #veil_document{record = #space}.
%% See {@link dao_records:save_record/1} and {@link dao_records:get_record/1} for more details about #veil_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao_worker:handle_call/3} instead (See {@link dao_worker:handle_call/3} for more details).
%% @end
-spec get_space(SpaceId :: uuid()) -> {ok, space_doc()} | {error, any()} | no_return().
%% ====================================================================
get_space(SpaceId) ->
    {ok, #veil_document{record = #space{}}} = dao_records:get_record(SpaceId).
