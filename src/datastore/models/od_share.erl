%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for share record - representing a file share in the system.
%%% @end
%%%-------------------------------------------------------------------
-module(od_share).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([create/1, save/1, get/1, exists/1, update/2, force_delete/1, list/0]).
-export([to_string/1]).
-export([entity_logic_plugin/0]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1, upgrade_record/2]).

-type id() :: binary().
-type record() :: #od_share{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-export_type([id/0, record/0]).

-type name() :: binary().
-export_type([name/0]).

-define(CTX, #{
    model => ?MODULE,
    fold_enabled => true,
    sync_enabled => true
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates share.
%% @end
%%--------------------------------------------------------------------
-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Saves share.
%% @end
%%--------------------------------------------------------------------
-spec save(doc()) -> {ok, doc()} | {error, term()}.
save(Doc) ->
    datastore_model:save(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Returns share by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(ShareId) ->
    datastore_model:get(?CTX, ShareId).

%%--------------------------------------------------------------------
%% @doc
%% Checks whether share given by ID exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(ShareId) ->
    datastore_model:exists(?CTX, ShareId).

%%--------------------------------------------------------------------
%% @doc
%% Updates share by ID.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(ShareId, Diff) ->
    datastore_model:update(?CTX, ShareId, Diff).

%%--------------------------------------------------------------------
%% @doc
%% Deletes share by ID.
%% WARNING: Must not be used directly, as deleting a share that still has
%% relations to other entities will cause serious inconsistencies in database.
%% To safely delete a share use share_logic.
%% @end
%%--------------------------------------------------------------------
-spec force_delete(id()) -> ok | {error, term()}.
force_delete(ShareId) ->
    datastore_model:delete(?CTX, ShareId).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all shares.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).

%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the share with given id.
%% @end
%%--------------------------------------------------------------------
-spec to_string(ShareId :: id()) -> binary().
to_string(ShareId) ->
    <<"share:", ShareId/binary>>.

%%--------------------------------------------------------------------
%% @doc
%% Returns the entity logic plugin module that handles model logic.
%% @end
%%--------------------------------------------------------------------
-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    share_logic_plugin.

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
    3.

%%--------------------------------------------------------------------
%% @doc
%% Returns model's record structure in provided version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_struct(datastore_model:record_version()) ->
    datastore_model:record_struct().
get_record_struct(1) ->
    {record, [
        {name, string},
        {public_url, string},
        {space, string},
        {handle, string},
        {root_file, string},
        {eff_users, [string]},
        {eff_groups, [string]},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(2) ->
    {record, [
        {name, string},
        {public_url, string},
        {space, string},
        {handle, string},
        {root_file, string}
    ]};
get_record_struct(3) ->
    % * new field - created_at
    % * new field - creator
    {record, [
        {name, string},
        {public_url, string},
        {space, string},
        {handle, string},
        {root_file, string},

        {created_at, integer}, % New field
        {creator, {record, [ % New field
            {type, atom},
            {id, string}
        ]}}
    ]}.

%%--------------------------------------------------------------------
%% @doc
%% Upgrades model's record from provided version to the next one.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_record(datastore_model:record_version(), datastore_model:record()) ->
    {datastore_model:record_version(), datastore_model:record()}.
upgrade_record(1, Share) ->
    {
        od_share,
        Name,
        PublicUrl,
        SpaceId,
        HandleId,
        RootFileId,

        _EffUsers,
        _EffGroups,

        _BottomUpDirty
    } = Share,
    {2, {od_share,
        Name,
        PublicUrl,
        SpaceId,
        HandleId,
        RootFileId
    }};
upgrade_record(2, Share) ->
    {
        od_share,
        Name,
        PublicUrl,
        SpaceId,
        HandleId,
        RootFileId
    } = Share,
    {3, #od_share{
        name = Name,
        public_url = PublicUrl,
        space = SpaceId,
        handle = HandleId,
        root_file = RootFileId,

        created_at = time_utils:system_time_seconds(),
        creator = undefined
    }}.