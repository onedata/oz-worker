%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for space record - representing a space in the system.
%%% @end
%%%-------------------------------------------------------------------
-module(od_space).
-author("Michal Zmuda").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/privileges.hrl").

%% API
-export([create/1, save/1, get/1, exists/1, update/2, force_delete/1, list/0]).
-export([to_string/1]).
-export([entity_logic_plugin/0]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1, upgrade_record/2]).

-type id() :: binary().
-type record() :: #od_space{}.
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
to_string(SpaceId) ->
    <<"space:", SpaceId/binary>>.

%%--------------------------------------------------------------------
%% @doc
%% Returns the entity logic plugin module that handles model logic.
%% @end
%%--------------------------------------------------------------------
-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    space_logic_plugin.

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
        {providers_supports, [{string, integer}]},
        {users, [{string, [atom]}]},
        {groups, [{string, [atom]}]},
        {shares, [string]},
        {eff_users, [{string, [atom]}]},
        {eff_groups, [{string, [atom]}]},
        {top_down_dirty, boolean},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(2) ->
    {record, [
        {name, string},
        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},
        {providers, #{string => integer}},
        {shares, [string]},
        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},
        {eff_providers, #{string => [{atom, string}]}},
        {top_down_dirty, boolean},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(3) ->
    % The structure does not change, but all records must be marked dirty to
    % recalculate effective relations (as intermediaries computing logic has changed).
    get_record_struct(2);
get_record_struct(4) ->
    % * new field - creation_time
    % * new field - creator
    % * privileges are translated
    {record, [
        {name, string},

        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},
        {providers, #{string => integer}},
        {shares, [string]},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},
        {eff_providers, #{string => [{atom, string}]}},

        {creation_time, integer}, % New field
        {creator, {record, [ % New field
            {type, atom},
            {id, string}
        ]}},

        {top_down_dirty, boolean},
        {bottom_up_dirty, boolean}
    ]}.

%%--------------------------------------------------------------------
%% @doc
%% Upgrades model's record from provided version to the next one.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_record(datastore_model:record_version(), datastore_model:record()) ->
    {datastore_model:record_version(), datastore_model:record()}.
upgrade_record(1, Space) ->
    {
        od_space,
        Name,

        ProviderSupports,
        Users,
        Groups,
        Shares,

        _EffUsers,
        _EffGroups,

        _TopDownDirty,
        _BottomUpDirty
    } = Space,
    {2, {od_space,
        Name,

        maps:from_list(Users),
        maps:from_list(Groups),
        maps:from_list(ProviderSupports),
        Shares,

        #{},
        #{},
        #{},

        true,
        true
    }};
upgrade_record(2, Space) ->
    {od_space,
        Name,

        Users,
        Groups,
        ProviderSupports,
        Shares,

        _EffUsers,
        _EffGroups,
        _EffProviders,

        _TopDownDirty,
        _BottomUpDirty
    } = Space,
    {3, {od_space,
        Name,

        Users,
        Groups,
        ProviderSupports,
        Shares,

        #{},
        #{},
        #{},

        true,
        true
    }};
upgrade_record(3, Space) ->
    {
        od_space,
        Name,

        Users,
        Groups,
        Providers,
        Shares,

        EffUsers,
        EffGroups,
        EffProviders,

        _TopDownDirty,
        _BottomUpDirty

    } = Space,

    % These new privileges are given to all users and groups, because before
    % introduction they all were allowed to perform related operations
    NewPrivileges = [
        ?SPACE_READ_DATA,
        ?SPACE_MANAGE_INDEXES, ?SPACE_QUERY_INDEXES,
        ?SPACE_VIEW_STATISTICS
    ],

    TranslatePrivileges = fun(Privileges) ->
        privileges:union(NewPrivileges, lists:flatten(lists:map(fun
            (space_view) -> [?SPACE_VIEW, ?SPACE_VIEW_PRIVILEGES];
            (space_invite_group) -> [?SPACE_ADD_GROUP];
            (Other) -> Other
        end, Privileges)))
    end,

    TranslateField = fun(Field) ->
        maps:map(fun
            (_, {Privs, Relation}) -> {TranslatePrivileges(Privs), Relation};
            (_, Privs) -> TranslatePrivileges(Privs)
        end, Field)
    end,

    {4, #od_space{
        name = Name,

        users = TranslateField(Users),
        groups = TranslateField(Groups),
        providers = Providers,
        shares = Shares,

        eff_users = TranslateField(EffUsers),
        eff_groups = TranslateField(EffGroups),
        eff_providers = EffProviders,

        creation_time = time_utils:system_time_seconds(),
        creator = undefined,

        top_down_dirty = true,
        bottom_up_dirty = true
    }}.
