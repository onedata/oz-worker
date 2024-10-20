%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for storage record - representing a storage in the system.
%%% @end
%%%-------------------------------------------------------------------
-module(od_storage).
-author("Michal Stanisz").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/privileges.hrl").

%% API
-export([create/1, get/1, get_record/1, exists/1, update/2, force_delete/1, list/0]).
-export([to_string/1, print_summary/0, print_summary/1]).
-export([entity_logic_plugin/0]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1, upgrade_record/2]).

-type id() :: binary().
-type record() :: #od_storage{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
% Map with key-value pairs used for describing storage QoS parameters
-type qos_parameters() :: #{binary() => binary() | integer()}.
-export_type([id/0, record/0, qos_parameters/0]).

-type name() :: binary().
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
get(StorageId) ->
    datastore_model:get(?CTX, StorageId).


-spec get_record(id()) -> {ok, record()} | {error, term()}.
get_record(StorageId) ->
    case get(StorageId) of
        {ok, #document{value = Record}} -> {ok, Record};
        {error, _} = Error -> Error
    end.


-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(StorageId) ->
    datastore_model:exists(?CTX, StorageId).


-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(StorageId, Diff) ->
    datastore_model:update(?CTX, StorageId, Diff).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a storage by ID.
%% WARNING: Must not be used directly, as deleting a storage that still has
%% relations to other entities will cause serious inconsistencies in database.
%% To safely delete a storage use storage_logic.
%% @end
%%--------------------------------------------------------------------
-spec force_delete(id()) -> ok | {error, term()}.
force_delete(StorageId) ->
    datastore_model:delete(?CTX, StorageId).


-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).


-spec to_string(StorageId :: id()) -> binary().
to_string(StorageId) ->
    <<"storage:", StorageId/binary>>.

%%--------------------------------------------------------------------
%% @doc
%% Prints all storage records to the console in a nicely-formatted manner.
%% Sorts the records in a default manner.
%% @end
%%--------------------------------------------------------------------
-spec print_summary() -> ok.
print_summary() ->
    print_summary(name).

%%--------------------------------------------------------------------
%% @doc
%% Prints all storage records to the console in a nicely-formatted manner.
%% Sorts the records by given attribute (specified by name or position).
%% @end
%%--------------------------------------------------------------------
-spec print_summary(id | name | users | groups | shares | providers | support | pos_integer()) -> ok.
print_summary(id) -> print_summary(1);
print_summary(name) -> print_summary(2);
print_summary(users) -> print_summary(3);
print_summary(groups) -> print_summary(4);
print_summary(spaces) -> print_summary(5);
print_summary(provider) -> print_summary(6);
print_summary(support) -> print_summary(7);
print_summary(SortPos) when is_integer(SortPos) ->
    {ok, Storages} = list(),
    StorageAttrs = lists:map(fun(#document{key = Id, value = St}) ->
        {
            Id,
            St#od_storage.name,
            maps:size(St#od_storage.eff_users),
            maps:size(St#od_storage.eff_groups),
            maps:size(St#od_storage.spaces),
            St#od_storage.provider,
            lists:sum(maps:values(St#od_storage.spaces))
        }
    end, Storages),
    Sorted = lists:keysort(SortPos, StorageAttrs),
    io:format("---------------------------------------------------------------------------------------------------------------------------~n"),
    io:format("Id                                Name               Users   Groups  Spaces  Provider                          Tot. support~n"),
    io:format("---------------------------------------------------------------------------------------------------------------------------~n"),
    lists:foreach(fun({Id, Name, EffUsers, EffGroups, Spaces, Provider, Support}) ->
        io:format("~-33s ~-18ts ~-7B ~-7B ~-7B ~-33s ~ts~n", [
            Id, Name, EffUsers, EffGroups, Spaces, Provider, str_utils:format_byte_size(Support)
        ])
    end, Sorted),
    io:format("---------------------------------------------------------------------------------------------------------------------------~n"),
    io:format("~B storages in total~n", [length(Sorted)]).

%%--------------------------------------------------------------------
%% @doc
%% Returns the entity logic plugin module that handles model logic.
%% @end
%%--------------------------------------------------------------------
-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    storage_logic_plugin.

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

        {qos_parameters, #{string => string}},

        {provider, string},
        {spaces, #{string => integer}},

        {eff_users, #{string => [{atom, string}]}},
        {eff_groups, #{string => [{atom, string}]}},
        {eff_harvesters, #{string => [{atom, string}]}},

        {eff_providers, #{string => [{atom, string}]}},
        {eff_spaces, #{string => {integer, [{atom, string}]}}},

        {creation_time, integer},
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}},

        {bottom_up_dirty, boolean},
        {top_down_dirty, boolean}
    ]};
get_record_struct(2) ->
    % * new field - imported
    {record, [
        {name, string},
        {qos_parameters, #{string => string}},
        {imported, atom},
        
        {provider, string},
        {spaces, #{string => integer}},
        
        {eff_users, #{string => [{atom, string}]}},
        {eff_groups, #{string => [{atom, string}]}},
        {eff_harvesters, #{string => [{atom, string}]}},
        
        {eff_providers, #{string => [{atom, string}]}},
        {eff_spaces, #{string => {integer, [{atom, string}]}}},
        
        {creation_time, integer},
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}},
        
        {bottom_up_dirty, boolean},
        {top_down_dirty, boolean}
    ]};
get_record_struct(3) ->
    % * new field - readonly
    % * changed qos_parameters type
    {record, [
        {name, string},
        {qos_parameters, {custom, json, {json_utils, encode, decode}}},
        {imported, atom},
        {readonly, boolean},

        {provider, string},
        {spaces, #{string => integer}},

        {eff_users, #{string => [{atom, string}]}},
        {eff_groups, #{string => [{atom, string}]}},
        {eff_harvesters, #{string => [{atom, string}]}},

        {eff_providers, #{string => [{atom, string}]}},
        {eff_spaces, #{string => {integer, [{atom, string}]}}},

        {creation_time, integer},
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}},

        {bottom_up_dirty, boolean},
        {top_down_dirty, boolean}
    ]}.


%%--------------------------------------------------------------------
%% @doc
%% Upgrades model's record from provided version to the next one.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_record(datastore_model:record_version(), datastore_model:record()) ->
    {datastore_model:record_version(), datastore_model:record()}.
upgrade_record(1, Storage) ->
    {
        od_storage,
        Name,
        QosParameters,
        
        Provider,
        Spaces,
        
        EffUsers,
        EffGroups,
        EffHarvesters,
    
        EffProviders,
        EffSpaces,
    
        CreationTime,
        Creator,

        TopDownDirty,
        BottomUpDirty
    } = Storage,
    {2, {
        od_storage,
        Name,
        QosParameters,
        unknown, % will be set by provider during its cluster upgrade procedure
       
        Provider,
        Spaces,
        
        EffUsers,
        EffGroups,
        EffHarvesters,
        
        EffProviders,
        EffSpaces,
        
        CreationTime,
        Creator,
        
        TopDownDirty,
        BottomUpDirty
    }};
upgrade_record(2, Storage) ->
    {
        od_storage,
        Name,
        QosParameters,
        Imported,

        Provider,
        Spaces,

        EffUsers,
        EffGroups,
        EffHarvesters,

        EffProviders,
        EffSpaces,

        CreationTime,
        Creator,

        TopDownDirty,
        BottomUpDirty
    } = Storage,
    {3, #od_storage{
        name = Name,
        qos_parameters = QosParameters,
        imported = Imported,
        readonly = false,

        provider = Provider,
        spaces = Spaces,

        eff_users = EffUsers,
        eff_groups = EffGroups,
        eff_harvesters = EffHarvesters,

        eff_providers = EffProviders,
        eff_spaces = EffSpaces,

        creation_time = CreationTime,
        creator = Creator,

        top_down_dirty = TopDownDirty,
        bottom_up_dirty = BottomUpDirty
    }}.
