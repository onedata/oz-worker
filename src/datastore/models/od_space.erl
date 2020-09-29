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
-export([create/1, get/1, exists/1, update/2, force_delete/1, list/0]).
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
-type support_size() :: pos_integer().
-export_type([name/0, support_size/0]).

-define(CTX, #{
    model => ?MODULE,
    secure_fold_enabled => true,
    sync_enabled => true,
    memory_copies => all
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
    9.

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
    % * new field - harvesters       
    % * new field - eff harvesters
    % * privileges are translated
    {record, [
        {name, string},

        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},
        {providers, #{string => integer}},
        {shares, [string]},
        {harvesters, [string]}, % New field

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},
        {eff_providers, #{string => [{atom, string}]}},
        {eff_harvesters, #{string => [{atom, string}]}}, % New field

        {creation_time, integer}, % New field
        {creator, {record, [ % New field
            {type, atom},
            {id, string}
        ]}},

        {top_down_dirty, boolean},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(5) ->
    % The structure does not change, but some privileges change names and some are added
    % so all records must be marked dirty to recalculate effective relations.
    get_record_struct(4);
get_record_struct(6) ->
    % creator field - nested record changed from #client{} to #subject{}
    {record, [
        {name, string},

        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},
        {providers, #{string => integer}},
        {shares, [string]},
        {harvesters, [string]},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},
        {eff_providers, #{string => [{atom, string}]}},
        {eff_harvesters, #{string => [{atom, string}]}},

        {creation_time, integer},
        {creator, {record, [ % nested record changed from #client{} to #subject{}
            {type, atom},
            {id, string}
        ]}},

        {top_down_dirty, boolean},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(7) ->
    % creator field - nested #subject{} record and encoding changed
    % * removed field - providers
    % * new field - storages
    {record, [
        {name, string},

        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},
        {storages, #{string => integer}}, % New field
        {shares, [string]},
        {harvesters, [string]},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},
        {eff_providers, #{string => {integer, [{atom, string}]}}}, % Modified field
        {eff_harvesters, #{string => [{atom, string}]}},

        {creation_time, integer},
        % nested #subject{} record was extended and is now encoded as string
        % rather than record tuple
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}},

        {top_down_dirty, boolean},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(8) ->
    % the structure does not change, but privileges are updated
    % (new privilege was added space_register_files)
    {record, [
        {name, string},

        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},
        {storages, #{string => integer}},
        {shares, [string]},
        {harvesters, [string]},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},
        {eff_providers, #{string => {integer, [{atom, string}]}}},
        {eff_harvesters, #{string => [{atom, string}]}},

        {creation_time, integer},
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}},

        {top_down_dirty, boolean},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(9) ->
    % new field - owners
    {record, [
        {name, string},

        {owners, [string]},

        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},
        {storages, #{string => integer}},
        {shares, [string]},
        {harvesters, [string]},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},
        {eff_providers, #{string => {integer, [{atom, string}]}}},
        {eff_harvesters, #{string => [{atom, string}]}},

        {creation_time, integer},
        {creator, {custom, string, {aai, serialize_subject, deserialize_subject}}},

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
        space_manage_indexes, space_query_indexes,
        ?SPACE_VIEW_STATISTICS
    ],

    TranslatePrivileges = fun(Privileges) ->
        privileges:union(NewPrivileges, lists:flatmap(fun
            (space_view) -> [?SPACE_VIEW, ?SPACE_VIEW_PRIVILEGES];
            (space_invite_user) -> [?SPACE_ADD_USER];
            (space_invite_group) -> [?SPACE_ADD_GROUP];
            (space_invite_provider) -> [space_add_provider];
            (Other) -> [Other]
        end, Privileges))
    end,

    TranslateField = fun(Field) ->
        maps:map(fun
            (_, {Privs, Relation}) -> {TranslatePrivileges(Privs), Relation};
            (_, Privs) -> TranslatePrivileges(Privs)
        end, Field)
    end,

    {4, {od_space,
        Name,

        TranslateField(Users),
        TranslateField(Groups),
        Providers,
        Shares,
        [],

        TranslateField(EffUsers),
        TranslateField(EffGroups),
        EffProviders,
        #{},

        time_utils:timestamp_seconds(),
        undefined,

        true,
        true
    }};
upgrade_record(4, Space) ->
    {
        od_space,
        Name,

        Users,
        Groups,
        Providers,
        Shares,
        Harvesters,

        EffUsers,
        EffGroups,
        EffProviders,
        EffHarvesters,

        CreationTime,
        Creator,

        _TopDownDirty,
        _BottomUpDirty

    } = Space,

    TranslatePrivileges = fun(Privileges) ->
        privileges:from_list(lists:flatmap(fun
            (?SPACE_VIEW) -> [?SPACE_VIEW, ?SPACE_VIEW_CHANGES_STREAM];
            (space_manage_indexes) -> [?SPACE_VIEW_VIEWS, ?SPACE_MANAGE_VIEWS];
            (space_query_indexes) -> [?SPACE_VIEW_VIEWS, ?SPACE_QUERY_VIEWS];
            (Other) -> [Other]
        end, Privileges))
    end,

    TranslateField = fun(Field) ->
        maps:map(fun
            (_, {Privs, Relation}) -> {TranslatePrivileges(Privs), Relation};
            (_, Privs) -> TranslatePrivileges(Privs)
        end, Field)
    end,

    {5, {
        od_space,
        Name,

        TranslateField(Users),
        TranslateField(Groups),
        Providers,
        Shares,
        Harvesters,

        TranslateField(EffUsers),
        TranslateField(EffGroups),
        EffProviders,
        EffHarvesters,

        CreationTime,
        Creator,

        true,
        true
    }};
upgrade_record(5, Space) ->
    {
        od_space,
        Name,

        Users,
        Groups,
        Providers,
        Shares,
        Harvesters,

        EffUsers,
        EffGroups,
        EffProviders,
        EffHarvesters,

        CreationTime,
        Creator,

        TopDownDirty,
        BottomUpDirty

    } = Space,

    {6, {
        od_space,
        Name,

        Users,
        Groups,
        Providers,
        Shares,
        Harvesters,

        EffUsers,
        EffGroups,
        EffProviders,
        EffHarvesters,

        CreationTime,
        upgrade_common:client_to_subject(Creator),

        TopDownDirty,
        BottomUpDirty

    }};
upgrade_record(6, Space) ->
    {
        od_space,
        Name,

        Users,
        Groups,
        _Providers,
        Shares,
        Harvesters,

        EffUsers,
        EffGroups,
        _EffProviders,
        EffHarvesters,

        CreationTime,
        Creator,

        _TopDownDirty,
        _BottomUpDirty

    } = Space,

    UpgradePrivileges = fun(Privileges) ->
        privileges:from_list(lists:flatmap(fun
            (space_add_provider) -> [?SPACE_ADD_SUPPORT];
            (space_remove_provider) -> [?SPACE_REMOVE_SUPPORT];
            (Other) -> [Other]
        end, Privileges))
    end,

    UpgradeRelation = fun(Field) ->
        maps:map(fun
            (_, {Privs, Relation}) -> {UpgradePrivileges(Privs), Relation};
            (_, Privs) -> UpgradePrivileges(Privs)
        end, Field)
    end,

    {7, {od_space,
        Name,

        UpgradeRelation(Users),
        UpgradeRelation(Groups),
        #{}, % storages - recalculated during cluster upgrade procedure
        Shares,
        Harvesters,

        UpgradeRelation(EffUsers),
        UpgradeRelation(EffGroups),
        #{}, %% eff_providers - recalculated during cluster upgrade procedure
        EffHarvesters,

        CreationTime,
        upgrade_common:upgrade_subject_record(Creator),

        true,
        true
    }};
upgrade_record(7, Space) ->
    {
        od_space,
        Name,

        Users,
        Groups,
        Storages,
        Shares,
        Harvesters,

        EffUsers,
        EffGroups,
        EffProviders,
        EffHarvesters,

        CreationTime,
        Creator,

        _TopDownDirty,
        _BottomUpDirty
    } = Space,

    PreviousManagerPrivs = privileges:space_manager() -- [?SPACE_REGISTER_FILES],
    UpgradePrivileges = fun(Privileges) ->
        % the ?SPACE_REGISTER_FILES is granted to all members that had at least
        % manager privileges before the upgrade
        case lists_utils:intersect(PreviousManagerPrivs, Privileges) of
            PreviousManagerPrivs -> privileges:from_list([?SPACE_REGISTER_FILES | Privileges]);
            _ -> Privileges
        end
    end,

    UpgradeRelation = fun(Field) ->
        maps:map(fun
            (_, {Privs, Relation}) -> {UpgradePrivileges(Privs), Relation};
            (_, Privs) -> UpgradePrivileges(Privs)
        end, Field)
    end,

    {8, {od_space,
        Name,

        UpgradeRelation(Users),
        UpgradeRelation(Groups),
        Storages,
        Shares,
        Harvesters,

        UpgradeRelation(EffUsers),
        UpgradeRelation(EffGroups),
        EffProviders,
        EffHarvesters,

        CreationTime,
        Creator,

        true,
        true
    }};
upgrade_record(8, Space) ->
    {
        od_space,
        Name,

        Users,
        Groups,
        Storages,
        Shares,
        Harvesters,

        EffUsers,
        EffGroups,
        EffProviders,
        EffHarvesters,

        CreationTime,
        Creator,

        TopDownDirty,
        BottomUpDirty
    } = Space,

    % Space ownership is automatically granted to all direct users that had the
    % most effective privileges in the space before the upgrade
    UserPrivilegeCounts = lists:map(fun(DirectUserId) ->
        % during the check, eff privileges might not be up to date - sum with direct privileges
        {EffPrivileges, _} = maps:get(DirectUserId, EffUsers, {[], []}),
        DirectPrivileges = maps:get(DirectUserId, Users),
        PrivilegeCount = length(privileges:from_list(EffPrivileges ++ DirectPrivileges)),
        {PrivilegeCount, DirectUserId}
    end, maps:keys(Users)),

    SortedByPrivilegeCount = lists:reverse(lists:sort(UserPrivilegeCounts)),
    MostPrivileges = case SortedByPrivilegeCount of
        [] -> 0;
        [{Count, _} | _] -> Count
    end,

    Owners = lists:filtermap(fun({PrivilegeCount, DirectUserId}) ->
        case PrivilegeCount of
            MostPrivileges -> {true, DirectUserId};
            _ -> false
        end
    end, SortedByPrivilegeCount),

    {9, #od_space{
        name = Name,

        owners = Owners,

        users = Users,
        groups = Groups,
        storages = Storages,
        shares = Shares,
        harvesters = Harvesters,

        eff_users = EffUsers,
        eff_groups = EffGroups,
        eff_providers = EffProviders,
        eff_harvesters = EffHarvesters,

        creation_time = CreationTime,
        creator = Creator,

        top_down_dirty = TopDownDirty,
        bottom_up_dirty = BottomUpDirty
    }}.