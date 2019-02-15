%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for od_group record - representing a group in the system.
%%% @end
%%%-------------------------------------------------------------------
-module(od_group).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/privileges.hrl").

%% API
-export([create/1, save/1, get/1, exists/1, update/2, update/3, force_delete/1]).
-export([list/0]).
-export([to_string/1]).
-export([entity_logic_plugin/0]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1, upgrade_record/2]).

-type id() :: binary().
-type record() :: #od_group{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-export_type([id/0, record/0]).

-type name() :: binary().
-type type() :: organization | unit | team | role_holders.
-export_type([name/0, type/0]).

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
%% Creates group.
%% @end
%%--------------------------------------------------------------------
-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Saves group.
%% @end
%%--------------------------------------------------------------------
-spec save(doc()) -> {ok, doc()} | {error, term()}.
save(Doc) ->
    datastore_model:save(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Returns group by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(GroupId) ->
    datastore_model:get(?CTX, GroupId).

%%--------------------------------------------------------------------
%% @doc
%% Checks whether group given by ID exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(GroupId) ->
    datastore_model:exists(?CTX, GroupId).

%%--------------------------------------------------------------------
%% @doc
%% Updates group by ID.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(GroupId, Diff) ->
    datastore_model:update(?CTX, GroupId, Diff).

%%--------------------------------------------------------------------
%% @doc
%% Updates group by ID or creates default one.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff(), record()) -> {ok, doc()} | {error, term()}.
update(GroupId, Diff, Default) ->
    datastore_model:update(?CTX, GroupId, Diff, Default).

%%--------------------------------------------------------------------
%% @doc
%% Deletes group by ID.
%% WARNING: Must not be used directly, as deleting a group that still has
%% relations to other entities will cause serious inconsistencies in database.
%% To safely delete a group use group_logic.
%% @end
%%--------------------------------------------------------------------
-spec force_delete(id()) -> ok | {error, term()}.
force_delete(GroupId) ->
    datastore_model:delete(?CTX, GroupId).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all groups.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).

%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the group with given id.
%% @end
%%--------------------------------------------------------------------
-spec to_string(GroupId :: id()) -> binary().
to_string(GroupId) ->
    <<"group:", GroupId/binary>>.

%%--------------------------------------------------------------------
%% @doc
%% Returns the entity logic plugin module that handles model logic.
%% @end
%%--------------------------------------------------------------------
-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    group_logic_plugin.

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
    6.

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
        {type, atom},
        {oz_privileges, [atom]},
        {eff_oz_privileges, [atom]},
        {parents, [string]},
        {children, [{string, [atom]}]},
        {eff_parents, [string]},
        {eff_children, [{string, [atom]}]},
        {users, [{string, [atom]}]},
        {spaces, [string]},
        {handle_services, [string]},
        {handles, [string]},
        {eff_users, [{string, [atom]}]},
        {eff_spaces, [string]},
        {eff_shares, [string]},
        {eff_providers, [string]},
        {eff_handle_services, [string]},
        {eff_handles, [string]},
        {top_down_dirty, boolean},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(2) ->
    {record, [
        {name, string},
        {type, atom},
        {oz_privileges, [atom]},
        {eff_oz_privileges, [atom]},

        {parents, [string]},
        {children, #{string => [atom]}},
        {eff_parents, #{string => [{atom, string}]}},
        {eff_children, #{string => {[atom], [{atom, string}]}}},

        {users, #{string => [atom]}},
        {spaces, [string]},
        {handle_services, [string]},
        {handles, [string]},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_spaces, #{string => [{atom, string}]}},
        {eff_providers, #{string => [{atom, string}]}},
        {eff_handle_services, #{string => [{atom, string}]}},
        {eff_handles, #{string => [{atom, string}]}},

        {top_down_dirty, boolean},
        {bottom_up_dirty, boolean}
    ]};
get_record_struct(3) ->
    % The structure does not change, only group names are normalized.
    get_record_struct(2);
get_record_struct(4) ->
    % There are no changes, but all records must be marked dirty to recalculate
    % effective relations (as intermediaries computing logic has changed).
    get_record_struct(3);
get_record_struct(5) ->
    % The 'role' type is changed to 'role_holders', the structure does not change
    get_record_struct(4);
get_record_struct(6) ->
    % * new field - protected group flag
    % * new field - creation_time
    % * new field - creator
    % * new field - harvesters
    % * new field - eff_harvesters
    % * privileges are translated
    {record, [
        {name, string},
        {type, atom},
        {protected, boolean}, % New field
        {oz_privileges, [atom]},
        {eff_oz_privileges, [atom]},

        {parents, [string]},
        {children, #{string => [atom]}},
        {eff_parents, #{string => [{atom, string}]}},
        {eff_children, #{string => {[atom], [{atom, string}]}}},

        {users, #{string => [atom]}},
        {spaces, [string]},
        {handle_services, [string]},
        {handles, [string]},
        {harvesters, [string]},

        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_spaces, #{string => [{atom, string}]}},
        {eff_providers, #{string => [{atom, string}]}},
        {eff_handle_services, #{string => [{atom, string}]}},
        {eff_handles, #{string => [{atom, string}]}},
        {eff_harvesters, #{string => [{atom, string}]}},

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
upgrade_record(1, Group) ->
    {
        od_group,
        Name,
        Type,
        OzPrivileges,
        _EffOzPrivileges,

        Parents,
        Children,
        _EffParents,
        _EffChildren,

        Users,
        Spaces,
        HandleServices,
        Handles,

        _EffUsers,
        _EffSpaces,
        _EffShares,
        _EffProviders,
        _EffHandleServices,
        _EffHandles,

        _TopDownDirty,
        _BottomUpDirty
    } = Group,
    {2, {
        od_group,
        Name,
        Type,
        OzPrivileges,
        [],

        Parents,
        maps:from_list(Children),
        #{},
        #{},

        maps:from_list(Users),
        Spaces,
        HandleServices,
        Handles,

        #{},
        #{},
        #{},
        #{},
        #{},

        true,
        true
    }};
upgrade_record(2, Group) ->
    {
        od_group,
        Name,
        Type,
        OzPrivileges,
        EffOzPrivileges,

        Parents,
        Children,
        EffParents,
        EffChildren,

        Users,
        Spaces,
        HandleServices,
        Handles,

        EffUsers,
        EffSpaces,
        EffProviders,
        EffHandleServices,
        EffHandles,

        _TopDownDirty,
        _BottomUpDirty
    } = Group,

    {3, {od_group,
        entity_logic:normalize_name(Name),
        Type,

        OzPrivileges,
        EffOzPrivileges,

        Parents,
        Children,
        EffParents,
        EffChildren,

        Users,
        Spaces,
        HandleServices,
        Handles,

        EffUsers,
        EffSpaces,
        EffProviders,
        EffHandleServices,
        EffHandles,

        true,
        true
    }};
upgrade_record(3, Group) ->
    {
        od_group,
        Name,
        Type,
        OzPrivileges,
        EffOzPrivileges,

        Parents,
        Children,
        _EffParents,
        _EffChildren,

        Users,
        Spaces,
        HandleServices,
        Handles,

        _EffUsers,
        _EffSpaces,
        _EffProviders,
        _EffHandleServices,
        _EffHandles,

        _TopDownDirty,
        _BottomUpDirty
    } = Group,

    {4, {od_group,
        entity_logic:normalize_name(Name),
        Type,

        OzPrivileges,
        EffOzPrivileges,

        Parents,
        Children,
        #{},
        #{},

        Users,
        Spaces,
        HandleServices,
        Handles,

        #{},
        #{},
        #{},
        #{},
        #{},

        true,
        true
    }};
upgrade_record(4, Group) ->
    {
        od_group,
        Name,
        Type,

        OzPrivileges,
        EffOzPrivileges,

        Parents,
        Children,
        EffParents,
        EffChildren,

        Users,
        Spaces,
        HandleServices,
        Handles,

        EffUsers,
        EffSpaces,
        EffProviders,
        EffHandleServices,
        EffHandles,

        TopDownDirty,
        BottomUpDirty
    } = Group,

    {5, {od_group,
        Name,
        case Type of
            role -> role_holders;
            Other -> Other
        end,

        OzPrivileges,
        EffOzPrivileges,

        Parents,
        Children,
        EffParents,
        EffChildren,

        Users,
        Spaces,
        HandleServices,
        Handles,

        EffUsers,
        EffSpaces,
        EffProviders,
        EffHandleServices,
        EffHandles,

        TopDownDirty,
        BottomUpDirty
    }};
upgrade_record(5, Group) ->
    {
        od_group,
        Name,
        Type,
        OzPrivileges,
        EffOzPrivileges,

        Parents,
        Children,
        EffParents,
        EffChildren,

        Users,
        Spaces,
        HandleServices,
        Handles,

        EffUsers,
        EffSpaces,
        EffProviders,
        EffHandleServices,
        EffHandles,

        TopDownDirty,
        BottomUpDirty
    } = Group,

    TranslatePrivileges = fun(Privileges) ->
        privileges:from_list(lists:flatten(lists:map(fun
            (group_invite_group) -> ?GROUP_ADD_CHILD;
            (group_remove_group) -> ?GROUP_REMOVE_CHILD;
            (group_join_group) -> ?GROUP_ADD_PARENT;
            (group_leave_group) -> ?GROUP_LEAVE_PARENT;
            (group_create_space) -> ?GROUP_ADD_SPACE;
            (group_join_space) -> ?GROUP_ADD_SPACE;
            (group_view) -> [?GROUP_VIEW, ?GROUP_VIEW_PRIVILEGES];

            (oz_users_list) -> [?OZ_USERS_LIST, ?OZ_USERS_VIEW];

            (oz_groups_list) -> [?OZ_GROUPS_LIST, ?OZ_GROUPS_VIEW];
            (oz_groups_list_users) -> ?OZ_GROUPS_LIST_RELATIONSHIPS;
            (oz_groups_list_groups) -> ?OZ_GROUPS_LIST_RELATIONSHIPS;
            (oz_groups_add_members) -> ?OZ_GROUPS_ADD_RELATIONSHIPS;
            (oz_groups_remove_members) -> ?OZ_GROUPS_REMOVE_RELATIONSHIPS;

            (oz_spaces_list) -> [?OZ_SPACES_LIST, ?OZ_SPACES_VIEW];
            (oz_spaces_list_users) -> ?OZ_SPACES_LIST_RELATIONSHIPS;
            (oz_spaces_list_groups) -> ?OZ_SPACES_LIST_RELATIONSHIPS;
            (oz_spaces_list_providers) -> ?OZ_SPACES_LIST_RELATIONSHIPS;
            (oz_spaces_add_members) -> ?OZ_SPACES_ADD_RELATIONSHIPS;
            (oz_spaces_remove_members) -> ?OZ_SPACES_REMOVE_RELATIONSHIPS;

            (oz_providers_list) -> [?OZ_PROVIDERS_LIST, ?OZ_PROVIDERS_VIEW];
            (oz_providers_list_users) -> ?OZ_PROVIDERS_LIST_RELATIONSHIPS;
            (oz_providers_list_groups) -> ?OZ_PROVIDERS_LIST_RELATIONSHIPS;
            (oz_providers_list_spaces) -> ?OZ_PROVIDERS_LIST_RELATIONSHIPS;

            (Other) -> Other
        end, Privileges)))
    end,

    TranslateField = fun(Field) ->
        maps:map(fun
            (_, {Privs, Intermediaries}) -> {TranslatePrivileges(Privs), Intermediaries};
            (_, Privs) -> TranslatePrivileges(Privs)
        end, Field)
    end,

    {6, #od_group{
        name = Name,
        type = Type,
        protected = false,
        oz_privileges = TranslatePrivileges(OzPrivileges),
        eff_oz_privileges = TranslatePrivileges(EffOzPrivileges),

        parents = Parents,
        children = TranslateField(Children),
        eff_parents = EffParents,
        eff_children = TranslateField(EffChildren),

        users = TranslateField(Users),
        spaces = Spaces,
        handle_services = HandleServices,
        handles = Handles,
        harvesters = [],

        eff_users = TranslateField(EffUsers),
        eff_spaces = EffSpaces,
        eff_providers = EffProviders,
        eff_handle_services = EffHandleServices,
        eff_handles = EffHandles,
        eff_harvesters = #{},

        creation_time = time_utils:system_time_seconds(),
        creator = undefined,

        top_down_dirty = TopDownDirty,
        bottom_up_dirty = BottomUpDirty
    }}.

