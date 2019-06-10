%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Database model representing service allowing for registration of file
%%% handles.
%%% @end
%%%-------------------------------------------------------------------
-module(od_handle_service).
-author("Tomasz Lichon").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([create/1, save/1, get/1, exists/1, update/2, force_delete/1, list/0]).
-export([to_string/1]).
-export([entity_logic_plugin/0]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1, upgrade_record/2]).

-type id() :: binary().
-type record() :: #od_handle_service{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-type name() :: binary().
-type proxy_endpoint() :: binary().
-type service_properties() :: map().

-export_type([id/0, record/0]).
-export_type([name/0, proxy_endpoint/0, service_properties/0]).

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
%% Creates handle service.
%% @end
%%--------------------------------------------------------------------
-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Saves handle service.
%% @end
%%--------------------------------------------------------------------
-spec save(doc()) -> {ok, doc()} | {error, term()}.
save(Doc) ->
    datastore_model:save(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Returns handle service by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(HServiceId) ->
    datastore_model:get(?CTX, HServiceId).

%%--------------------------------------------------------------------
%% @doc
%% Checks whether handle service given by ID exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(HServiceId) ->
    datastore_model:exists(?CTX, HServiceId).

%%--------------------------------------------------------------------
%% @doc
%% Updates handle service by ID.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(HServiceId, Diff) ->
    datastore_model:update(?CTX, HServiceId, Diff).

%%--------------------------------------------------------------------
%% @doc
%% Deletes handle service by ID.
%% WARNING: Must not be used directly, as deleting a handle_service that still has
%% relations to other entities will cause serious inconsistencies in database.
%% To safely delete a handle_service use handle_service_logic.
%% @end
%%--------------------------------------------------------------------
-spec force_delete(id()) -> ok | {error, term()}.
force_delete(HServiceId) ->
    datastore_model:delete(?CTX, HServiceId).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all handle services.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).

%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the handle service with given id.
%% @end
%%--------------------------------------------------------------------
-spec to_string(HServiceId :: id()) -> binary().
to_string(HServiceId) ->
    <<"handle_service:", HServiceId/binary>>.

%%--------------------------------------------------------------------
%% @doc
%% Returns the entity logic plugin module that handles model logic.
%% @end
%%--------------------------------------------------------------------
-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    handle_service_logic_plugin.

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
    4.

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
        {proxy_endpoint, string},
        {service_properties, [term]},
        {users, [{string, [atom]}]},
        {groups, [{string, [atom]}]},
        {eff_users, [{string, [atom]}]},
        {eff_groups, [{string, [atom]}]},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(2) ->
    {record, [
        {name, string},
        {proxy_endpoint, string},
        {service_properties, #{term => term}},
        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},
        {handles, [string]},
        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(3) ->
    % There are no changes, but all records must be marked dirty to recalculate
    % effective relations (as intermediaries computing logic has changed).
    get_record_struct(2);
get_record_struct(4) ->
    % * new field - creation_time
    % * new field - creator
    {record, [
        {name, string},
        {proxy_endpoint, string},
        {service_properties, #{term => term}},

        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},
        {handles, [string]},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},

        {creation_time, integer}, % New field
        {creator, {record, [ % New field
            {type, atom},
            {id, string}
        ]}},

        {bottom_up_dirty, boolean}
    ]}.


%%--------------------------------------------------------------------
%% @doc
%% Upgrades model's record from provided version to the next one.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_record(datastore_model:record_version(), datastore_model:record()) ->
    {datastore_model:record_version(), datastore_model:record()}.
upgrade_record(1, HandleService) ->
    {
        od_handle_service,
        Name,
        ProxyEndpoint,
        ServiceProperties,

        Users,
        Groups,

        _EffUsers,
        _EffGroups,

        _BottomUpDirty
    } = HandleService,
    {2, {od_handle_service,
        Name,
        ProxyEndpoint,
        maps:from_list(ServiceProperties),

        maps:from_list(Users),
        maps:from_list(Groups),
        [],

        #{},
        #{},

        true
    }};
upgrade_record(2, HandleService) ->
    {od_handle_service,
        Name,
        ProxyEndpoint,
        ServiceProperties,

        Users,
        Groups,
        Handles,

        _EffUsers,
        _EffGroups,

        _BottomUpDirty
    } = HandleService,
    {3, {od_handle_service,
        Name,
        ProxyEndpoint,
        ServiceProperties,

        Users,
        Groups,
        Handles,

        #{},
        #{},

        true
    }};
upgrade_record(3, HandleService) ->
    {od_handle_service,
        Name,
        ProxyEndpoint,
        ServiceProperties,

        Users,
        Groups,
        Handles,

        EffUsers,
        EffGroups,

        BottomUpDirty
    } = HandleService,
    {4, #od_handle_service{
        name = Name,
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties,

        users = Users,
        groups = Groups,
        handles = Handles,

        eff_users = EffUsers,
        eff_groups = EffGroups,

        creation_time = time_utils:system_time_seconds(),
        creator = undefined,

        bottom_up_dirty = BottomUpDirty
    }}.