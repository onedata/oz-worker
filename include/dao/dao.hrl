%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2013 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: dao header
%% @end
%% ===================================================================

-ifndef(DAO_HRL).
-define(DAO_HRL, 1).

-include_lib("dao/common.hrl").

%% record definition used in record registration example
-record(some_record, {field1 = "", field2 = "", field3 = ""}).

%% Helper macro. See macro ?dao_record_info/1 for more details.
-define(record_info_gen(X), {record_info(size, X), record_info(fields, X), #X{}}).

%% Every record that will be saved to DB have to be "registered" with this define.
%% Each registered record should be listed in defined below 'case' block as fallow:
%% record_name -> ?record_info_gen(record_name);
%% where 'record_name' is the name of the record. 'some_record' is an example.
-define(dao_record_info(R),
    case R of
        some_record         -> ?record_info_gen(some_record);
		user                -> ?record_info_gen(user);
		provider            -> ?record_info_gen(provider);
		space               -> ?record_info_gen(space);
		token               -> ?record_info_gen(token);
		user_group          -> ?record_info_gen(user_group);
        _ -> {error, unsupported_record}
    end).

%% Record-wrapper for regular records that needs to be saved in DB. Adds UUID and Revision info to each record.
%% `uuid` is document UUID, `rev_info` is documents' current revision number
%% `record` is an record representing this document (its data), `force_update` is a flag
%% that forces dao:save_record/1 to update this document even if rev_info isn't valid or up to date.
-record(veil_document, {uuid = "", rev_info = 0, record = none, force_update = false}).

%% Those records represent view result Each #view_resault contains list of #view_row.
%% If the view has been queried with `include_docs` option, #view_row.doc will contain #veil_document, therefore
%% #view_row.id == #view_row.doc#veil_document.uuid. Unfortunately wrapping demanded record in #veil_document is
%% necessary because we may need revision info for that document.
-record(view_row, {id = "", key = "", value = 0, doc = none}).
-record(view_result, {total = 0, offset = 0, rows = []}).

%% These records allows representing databases, design documents and their views.
%% Used in DAO initial configuration in order to easily setup/update views in database.
-record(db_info, {name = "", designs = []}).
-record(design_info, {name = "", version = 0, views = []}).
-record(view_info, {name = "", design = "", db_name = ""}).

%% ====================================================================
%% DB definitions
%% ====================================================================
%% DB Names
-define(SYSTEM_DB_NAME, "system_data").

%% Design Names

%% Views

%% Others
-define(RECORD_INSTANCES_DOC_PREFIX, "record_instances_").
-define(RECORD_FIELD_BINARY_PREFIX, "__bin__: ").
-define(RECORD_FIELD_ATOM_PREFIX, "__atom__: ").
-define(RECORD_FIELD_PID_PREFIX, "__pid__: ").
-define(RECORD_TUPLE_FIELD_NAME_PREFIX, "tuple_field_").
-define(RECORD_META_FIELD_NAME, "record__").

%% List of all used databases :: [string()]
-define(DB_LIST, [?SYSTEM_DB_NAME]).
%% List of all used views :: [#view_info]
-define(VIEW_LIST, []).
%% Default database name
-define(DEFAULT_DB, lists:nth(1, ?DB_LIST)).

%% Do not try to read this macro (3 nested list comprehensions). All it does is:
%% Create an list containing #db_info structures base on ?DB_LIST
%% Inside every #db_info, list of #design_info is created based on views list (?VIEW_LIST)
%% Inside every #design_info, list of #view_info is created based on views list (?VIEW_LIST)
%% Such structural representation of views, makes it easier to initialize views in DBMS
%% WARNING: Do not evaluate this macro anywhere but dao:init/cleanup because it's
%% potentially slow - O(db_count * view_count^2)
-define(DATABASE_DESIGN_STRUCTURE, [#db_info{name = DbName,
                                        designs = [#design_info{name = DesignName,
                                                views = [ViewInfo || #view_info{design = Design, db_name = DbName2} = ViewInfo <- ?VIEW_LIST,
                                                    Design == DesignName, DbName2 == DbName]
                                            } || #view_info{db_name = DbName1, design = DesignName} <- ?VIEW_LIST, DbName1 == DbName]
                                        } || DbName <- ?DB_LIST]).
-endif.
