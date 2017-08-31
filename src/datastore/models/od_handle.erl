%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Database model representing file handles.
%%% @end
%%%-------------------------------------------------------------------
-module(od_handle).
-author("Tomasz Lichon").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([create/1, save/1, get/1, exists/1, update/2, delete/1, list/0]).
-export([to_string/1, actual_timestamp/0]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1, upgrade_record/2]).

-type id() :: binary().
-type record() :: #od_handle{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-type resource_type() :: binary().
-type resource_id() :: binary().
-type public_handle() :: binary().
-type metadata() :: binary().
-type timestamp() :: calendar:datetime().

-export_type([id/0, record/0]).
-export_type([resource_type/0, resource_id/0, public_handle/0, metadata/0,
    timestamp/0]).

-define(CTX, #{
    model => od_handle,
    fold_enabled => true,
    sync_enabled => true
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates handle.
%% @end
%%--------------------------------------------------------------------
-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Saves handle.
%% @end
%%--------------------------------------------------------------------
-spec save(doc()) -> {ok, doc()} | {error, term()}.
save(Doc) ->
    datastore_model:save(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Returns handle by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(HandleId) ->
    datastore_model:get(?CTX, HandleId).

%%--------------------------------------------------------------------
%% @doc
%% Checks whether handle given by ID exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(HandleId) ->
    datastore_model:exists(?CTX, HandleId).

%%--------------------------------------------------------------------
%% @doc
%% Updates handle by ID.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(HandleId, Diff) ->
    datastore_model:update(?CTX, HandleId, Diff).

%%--------------------------------------------------------------------
%% @doc
%% Deletes handle by ID.
%% @end
%%--------------------------------------------------------------------
-spec delete(id()) -> ok | {error, term()}.
delete(HandleId) ->
    datastore_model:delete(?CTX, HandleId).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all handles.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).

%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the handle with given id.
%% @end
%%--------------------------------------------------------------------
-spec to_string(HandleId :: id()) -> binary().
to_string(HandleId) ->
    <<"handle:", HandleId/binary>>.

%%--------------------------------------------------------------------
%% @equiv erlang:universaltime().
%% @end
%%--------------------------------------------------------------------
-spec actual_timestamp() -> timestamp().
actual_timestamp() ->
    erlang:universaltime().

%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns model's record version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_version() -> datastore_model:record_version().
get_record_version() ->
    2.

%%--------------------------------------------------------------------
%% @doc
%% Returns model's record structure in provided version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_struct(datastore_model:record_version()) ->
    datastore_model:record_struct().
get_record_struct(1) ->
    {record, [
        {public_handle, string},
        {resource_type, string},
        {resource_id, string},
        {metadata, string},
        {timestamp, {{integer, integer, integer}, {integer, integer, integer}}},
        {handle_service, string},
        {users, [{string, [atom]}]},
        {groups, [{string, [atom]}]},
        {eff_users, [{string, [atom]}]},
        {eff_groups, [{string, [atom]}]},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(2) ->
    {record, [
        {public_handle, string},
        {resource_type, string},
        {metadata, string},
        {timestamp, {{integer, integer, integer}, {integer, integer, integer}}},
        {resource_id, string},
        {handle_service, string},
        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},
        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},
        {bottom_up_dirty, boolean}
    ]}.

%%--------------------------------------------------------------------
%% @doc
%% Upgrades model's record from provided version to the next one.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_record(datastore_model:record_version(), datastore_model:record()) ->
    {datastore_model:record_version(), datastore_model:record()}.
upgrade_record(1, Handle) ->
    {
        od_handle,
        PublicHandle,
        ResourceType,
        ResourceId,
        Metadata,
        Timestamp,

        HandleService,
        Users,
        Groups,

        _EffUsers,
        _EffGroups,

        _BottomUpDirty
    } = Handle,
    {2, #od_handle{
        public_handle = PublicHandle,
        resource_type = ResourceType,
        metadata = Metadata,
        timestamp = Timestamp,

        resource_id = ResourceId,
        handle_service = HandleService,
        users = maps:from_list(Users),
        groups = maps:from_list(Groups),

        eff_users = #{},
        eff_groups = #{},

        bottom_up_dirty = true
    }}.
