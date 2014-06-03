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
-include("dao/dao_users.hrl").
-include("dao/dao.hrl").
-include("dao/dao_types.hrl").

%% API
-export([save_user/1, remove_user/1, exist_user/1, get_user/1]).

%% save_user/1
%% ====================================================================
%% @doc Saves user to DB. Argument should be either #user{} record
%% (if you want to save it as new document) <br/>
%% or #veil_document{} that wraps #user{} if you want to update descriptor in DB. <br/>
%% See {@link dao:save_record/1} and {@link dao:get_record/1} for more details about #veil_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao:handle/2} instead (See {@link dao:handle/2} for more details).
%% @end
-spec save_user(User :: user_info() | user_doc()) -> {ok, user_id()} | {error, any()} | no_return().
%% ====================================================================
save_user(#user{} = User) ->
	save_user(#veil_document{record = User});
save_user(#veil_document{record = #user{}, uuid = UUID} = UserDoc) when is_list(UUID) ->
	dao:save_record(UserDoc).


%% remove_user/1
%% ====================================================================
%% @doc Removes user from DB
%% Should not be used directly, use {@link dao:handle/2} instead (See {@link dao:handle/2} for more details).
%% @end
-spec remove_user(UserId:: uuid()) ->
	ok | {error, any()} | no_return().
%% ====================================================================
remove_user(UserId) ->
	dao:remove_record(UserId).

%% exist_user/1
%% ====================================================================
%% @doc Checks whether user exists in DB.
%% Should not be used directly, use {@link dao:handle/2} instead (See {@link dao:handle/2} for more details).
%% @end
-spec exist_user(UserId :: uuid()) -> {ok, true | false} | {error, any()}.
%% ====================================================================
exist_user(UserId) ->
	dao:exist_record(UserId).

%% get_user/1
%% ====================================================================
%% @doc Gets user from DB
%% Non-error return value is always {ok, #veil_document{record = #user}.
%% See {@link dao:save_record/1} and {@link dao:get_record/1} for more details about #veil_document{} wrapper.<br/>
%% Should not be used directly, use {@link dao:handle/2} instead (See {@link dao:handle/2} for more details).
%% @end
-spec get_user(UserId :: uuid()) -> {ok, user_doc()} | {error, any()} | no_return().
%% ====================================================================
get_user(UserId) ->
	dao:get_record(UserId).