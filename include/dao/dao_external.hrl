%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Application specific definitions
%% @end
%% ===================================================================
-author("Tomasz Lichon").

-ifndef(DAO_DRIVER).
-define(DAO_DRIVER, 1).

-include("dao/dao_groups.hrl").
-include("dao/dao_providers.hrl").
-include("dao/dao_spaces.hrl").
-include("dao/dao_tokens.hrl").
-include("dao/dao_users.hrl").
-include_lib("dao/include/common.hrl").

%% Every record that will be saved to DB have to be "registered" with this define.
%% Each registered record should be listed in defined below 'case' block as fallow:
%% record_name -> ?record_info_gen(record_name);
%% where 'record_name' is the name of the record. 'some_record' is an example.
-define(dao_record_info(R),
    case R of
        some_record         -> ?record_info_gen(some_record);   %example record from dao common.hrl
        user                -> ?record_info_gen(user);
        oauth_account       -> ?record_info_gen(oauth_account);
        provider            -> ?record_info_gen(provider);
        space               -> ?record_info_gen(space);
        token               -> ?record_info_gen(token);
        user_group          -> ?record_info_gen(user_group);
        _ -> {error, unsupported_record}
    end).

%% ====================================================================
%% DB definitions
%% ====================================================================
%% DB Names
-define(SYSTEM_DB_NAME, "system_data").
-define(USERS_DB_NAME, "user_data").

%% List of all used databases :: [string()]
-define(DB_LIST, [?SYSTEM_DB_NAME, ?USERS_DB_NAME]).

%% Views
-define(USER_BY_EMAIL_VIEW, #view_info{name = "user_by_email", db_name = ?USERS_DB_NAME, version = 1}).
-define(USER_BY_CONNECTED_ACCOUNT_USER_ID_VIEW, #view_info{name = "user_by_connected_account_user_id", db_name = ?USERS_DB_NAME, version = 1}).

%% List of all used views :: [#view_info]
-define(VIEW_LIST, [?USER_BY_EMAIL_VIEW, ?USER_BY_CONNECTED_ACCOUNT_USER_ID_VIEW]).

%% Default database name
-define(DEFAULT_DB, lists:nth(1, ?DB_LIST)).

%% Do not try to read this macro (3 nested list comprehensions). All it does is:
%% Create an list containing #db_info structures base on ?DB_LIST
%% Inside every #db_info, list of #design_info is created based on views list (?VIEW_LIST)
%% Inside every #design_info, list of #view_info is created based on views list (?VIEW_LIST)
%% Such structural representation of views, makes it easier to initialize views in DBMS
%% WARNING: Do not evaluate this macro anywhere but dao_worker:init/cleanup because it's
%% potentially slow - O(db_count * view_count^2)
-define(DATABASE_DESIGN_STRUCTURE, [#db_info{name = DbName,
    designs = [#design_info{name = dao_utils:get_versioned_view_name(ViewName, Vsn),
        views = [ViewInfo || #view_info{name = ViewName1, db_name = DbName2} = ViewInfo <- ?VIEW_LIST,
            DbName2 == DbName, ViewName1 == ViewName]
    } || #view_info{db_name = DbName1, name = ViewName, version = Vsn} <- ?VIEW_LIST, DbName1 == DbName]
} || DbName <- ?DB_LIST]).

-endif.