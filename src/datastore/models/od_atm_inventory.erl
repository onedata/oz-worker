%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Database model representing an automation inventory - a set of workflow
%%% schemas and their building blocks - lambda functions. Members of an
%%% automation inventory share all these definitions and can execute the
%%% workflows on their data.
%%% @end
%%%-------------------------------------------------------------------
-module(od_atm_inventory).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([create/1, get/1, exists/1, update/2, force_delete/1, list/0]).
-export([to_string/1]).
-export([entity_logic_plugin/0]).
-export([critical_section/2]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1]).

-type id() :: binary().
-type record() :: #od_atm_inventory{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-type name() :: binary().

-export_type([id/0, record/0]).
-export_type([name/0]).

-define(CTX, #{
    model => ?MODULE,
    secure_fold_enabled => true,
    sync_enabled => true,
    memory_copies => all
}).

-compile({no_auto_import, [get/1]}).

%%%===================================================================
%%% API
%%%===================================================================

-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).


-spec get(id()) -> {ok, doc()} | {error, term()}.
get(AtmInventoryId) ->
    datastore_model:get(?CTX, AtmInventoryId).


-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(AtmInventoryId) ->
    datastore_model:exists(?CTX, AtmInventoryId).


-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(AtmInventoryId, Diff) ->
    datastore_model:update(?CTX, AtmInventoryId, Diff).


%%--------------------------------------------------------------------
%% @doc
%% Deletes an automation inventory by Id.
%% WARNING: Must not be used directly, as deleting an inventory that still has
%% relations to other entities will cause serious inconsistencies in database.
%% To safely delete a inventory, use atm_inventory_logic.
%% @end
%%--------------------------------------------------------------------
-spec force_delete(id()) -> ok | {error, term()}.
force_delete(AtmInventoryId) ->
    datastore_model:delete(?CTX, AtmInventoryId).


-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).


-spec to_string(AtmInventoryId :: id()) -> binary().
to_string(AtmInventoryId) ->
    <<"atmInventory:", AtmInventoryId/binary>>.


-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    atm_inventory_logic_plugin.


%%--------------------------------------------------------------------
%% @doc
%% Every operation that modifies relations of an inventory should be wrapped
%% in this critical section to avoid race conditions caused by concurrent
%% modifications.
%% @end
%%--------------------------------------------------------------------
-spec critical_section(id(), fun(() -> Result)) -> Result.
critical_section(AtmInventoryId, Fun) ->
    critical_section:run({?MODULE, ?FUNCTION_NAME, AtmInventoryId}, Fun).

%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================

-spec get_record_version() -> datastore_model:record_version().
get_record_version() ->
    1.

-spec get_record_struct(datastore_model:record_version()) ->
    datastore_model:record_struct().
get_record_struct(1) ->
    {record, [
        {name, string},

        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},
        {atm_lambdas, [string]},
        {atm_workflow_schemas, [string]},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},

        {creation_time, integer},
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}},

        {bottom_up_dirty, boolean}
    ]}.