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
-export([create/1, save/1, get/1, exists/1, update/2, force_delete/1, list/0]).
-export([to_string/1, print_summary/0, print_summary/1]).
-export([entity_logic_plugin/0]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1]).

-type id() :: binary().
-type record() :: #od_storage{}.
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
%% Creates space.
%% @end
%%--------------------------------------------------------------------
-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Save space.
%% @end
%%--------------------------------------------------------------------
-spec save(doc()) -> {ok, doc()} | {error, term()}.
save(Doc) ->
    datastore_model:save(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Returns space by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(SpaceId) ->
    datastore_model:get(?CTX, SpaceId).

%%--------------------------------------------------------------------
%% @doc
%% Checks whether space given by ID exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(SpaceId) ->
    datastore_model:exists(?CTX, SpaceId).

%%--------------------------------------------------------------------
%% @doc
%% Updates space by ID.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(SpaceId, Diff) ->
    datastore_model:update(?CTX, SpaceId, Diff).

%%--------------------------------------------------------------------
%% @doc
%% Deletes space by ID.
%% WARNING: Must not be used directly, as deleting a space that still has
%% relations to other entities will cause serious inconsistencies in database.
%% To safely delete a space use space_logic.
%% @end
%%--------------------------------------------------------------------
-spec force_delete(id()) -> ok | {error, term()}.
force_delete(SpaceId) ->
    datastore_model:delete(?CTX, SpaceId).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all providers.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).

%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the space with given id.
%% @end
%%--------------------------------------------------------------------
-spec to_string(SpaceId :: id()) -> binary().
to_string(StorageId) ->
    <<"storage:", StorageId/binary>>.

%%--------------------------------------------------------------------
%% @doc
%% Prints all space records to the console in a nicely-formatted manner.
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
        io:format("~-33s ~-18ts ~-7B ~-7B ~-7B ~-33s ~s~n", [
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
    1.

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

        {qos_parameters, #{binary => binary}},

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

