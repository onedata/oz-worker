%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for od_group record - representing a group in the system.
%%% @end
%%%-------------------------------------------------------------------
-module(od_group).
-author("Michal Zmuda").
-behaviour(model_behaviour).

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("cluster_worker/include/modules/datastore/datastore_model.hrl").

-type doc() :: datastore:document().
-type info() :: #od_group{}.
-type id() :: binary().
-export_type([doc/0, info/0, id/0]).

-type name() :: binary().
-type type() :: organization | unit | team | role.
-export_type([name/0, type/0]).

%% model_behaviour callbacks
-export([save/1, get/1, list/0, exists/1, delete/1, update/2, create/1,
    model_init/0, 'after'/5, before/4]).
-export([record_struct/1, record_upgrade/2]).
-export([to_string/1]).

-define(USER_MODULE, od_user).

%%--------------------------------------------------------------------
%% @doc
%% Returns structure of the record in specified version.
%% @end
%%--------------------------------------------------------------------
-spec record_struct(datastore_json:record_version()) -> datastore_json:record_struct().
record_struct(1) ->
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
record_struct(2) ->
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
    ]}.

%%%===================================================================
%%% model_behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback save/1.
%% @end
%%--------------------------------------------------------------------
-spec save(datastore:document()) ->
    {ok, datastore:ext_key()} | datastore:generic_error().
save(Document) ->
    model:execute_with_default_context(?MODULE, save, [Document]).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback update/2.
%% @end
%%--------------------------------------------------------------------
-spec update(datastore:ext_key(), Diff :: datastore:document_diff()) ->
    {ok, datastore:ext_key()} | datastore:update_error().
update(Key, Diff) ->
    model:execute_with_default_context(?MODULE, update, [Key, Diff]).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback create/1.
%% @end
%%--------------------------------------------------------------------
-spec create(datastore:document()) ->
    {ok, datastore:ext_key()} | datastore:create_error().
create(Document) ->
    model:execute_with_default_context(?MODULE, create, [Document]).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback get/1.
%% @end
%%--------------------------------------------------------------------
-spec get(datastore:ext_key()) -> {ok, datastore:document()} | datastore:get_error().
get(Key) ->
    model:execute_with_default_context(?MODULE, get, [Key]).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all records.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [datastore:document()]} | datastore:generic_error() | no_return().
list() ->
    model:execute_with_default_context(?MODULE, list, [?GET_ALL, []]).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback delete/1.
%% @end
%%--------------------------------------------------------------------
-spec delete(datastore:ext_key()) -> ok | datastore:generic_error().
delete(Key) ->
    model:execute_with_default_context(?MODULE, delete, [Key]).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback exists/1.
%% @end
%%--------------------------------------------------------------------
-spec exists(datastore:ext_key()) -> datastore:exists_return().
exists(Key) ->
    ?RESPONSE(model:execute_with_default_context(?MODULE, exists, [Key])).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback model_init/0.
%% @end
%%--------------------------------------------------------------------
-spec model_init() -> model_behaviour:model_config().
model_init() ->
    % TODO migrate to GLOBALLY_CACHED_LEVEL
    StoreLevel = application:get_env(?APP_NAME, group_store_level, ?DISK_ONLY_LEVEL),
    UserHooks = [{?USER_MODULE, save}, {?USER_MODULE, update}, {?USER_MODULE, create},
        {?USER_MODULE, create_or_opdate}],
    Hooks = [{?MODULE, save}, {?MODULE, update}, {?MODULE, create}, {?MODULE, create_or_opdate}],
    Config = ?MODEL_CONFIG(od_group_bucket, Hooks ++ UserHooks, StoreLevel),
    Config#model_config{version = 2}.

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback 'after'/5.
%% @end
%%--------------------------------------------------------------------
-spec 'after'(ModelName :: model_behaviour:model_type(), Method :: model_behaviour:model_action(),
    Level :: datastore:store_level(), Context :: term(),
    ReturnValue :: term()) -> ok.
'after'(_ModelName, _Method, _Level, _Context, _ReturnValue) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback before/4.
%% @end
%%--------------------------------------------------------------------
-spec before(ModelName :: model_behaviour:model_type(), Method :: model_behaviour:model_action(),
    Level :: datastore:store_level(), Context :: term()) -> ok | datastore:generic_error().
before(_ModelName, _Method, _Level, _Context) ->
    ok.


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
%% Upgrades record from specified version.
%% @end
%%--------------------------------------------------------------------
-spec record_upgrade(datastore_json:record_version(), tuple()) ->
    {datastore_json:record_version(), tuple()}.
record_upgrade(1, Group) ->
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
    {2, #od_group{
        name = Name,
        type = Type,
        oz_privileges = OzPrivileges,
        eff_oz_privileges = [],

        parents = Parents,
        children = maps:from_list(Children),
        eff_parents = #{},
        eff_children = #{},

        users = maps:from_list(Users),
        spaces = Spaces,
        handle_services = HandleServices,
        handles = Handles,

        eff_users = #{},
        eff_spaces = #{},
        eff_providers = #{},
        eff_handle_services = #{},
        eff_handles = #{},

        top_down_dirty = true,
        bottom_up_dirty = true
    }}.
