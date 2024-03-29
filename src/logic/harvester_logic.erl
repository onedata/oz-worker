%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all harvester logic functionality.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(harvester_logic).
-author("Michal Stanisz").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

-export([
    create/2, create/5
]).
-export([
    get/2,
    get_protected_data/2,
    get_public_data/2,
    get_name/2,
    list/1,
    list_privileges/0,
    get_all_backend_types/0,
    get_gui_plugin_config/2
]).
-export([
    update/3, update/5,
    update_gui_plugin_config/3
]).
-export([
    delete/2, delete_harvested_metadata/2
]).
-export([
    create_index/3, create_index/4, create_index/5, 
    get_index/3, get_public_index/3, 
    get_index_stats/3, 
    update_index/4,
    delete_index/3, delete_index_metadata/3,
    query_index/4,
    gen_curl_query/5,
    list_indices/2
]).
-export([
    submit_batch/4, submit_batch/7
]).
-export([
    create_user_invite_token/2,
    create_group_invite_token/2,
    create_space_invite_token/2,

    add_user/3, add_user/4,
    add_group/3, add_group/4,
    add_space/3,
    create_group/3, create_group/4,

    get_users/2, get_eff_users/2,
    get_user/3, get_eff_user/3,
    get_user_privileges/3, get_eff_user_privileges/3,
    get_eff_user_membership_intermediaries/3,

    get_groups/2, get_eff_groups/2,
    get_group/3, get_eff_group/3,
    get_group_privileges/3, get_eff_group_privileges/3,
    get_eff_group_membership_intermediaries/3,

    get_spaces/2, get_space/3,
    join_space/3,

    get_eff_providers/2, get_eff_provider/3,

    update_user_privileges/5, update_user_privileges/4,
    update_group_privileges/5, update_group_privileges/4,

    remove_space/3,
    remove_user/3,
    remove_group/3
]).
-export([
    exists/1,
    has_eff_privilege/3,
    has_direct_user/2,
    has_eff_user/2,
    has_eff_group/2,
    has_space/2,
    has_eff_provider/2
]).
-export([
    deploy_default_gui_package/0
]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new harvester document in database. 
%% Harvester name, endpoint plugin and config are given explicitly.
%% @end
%%--------------------------------------------------------------------
-spec create(Auth :: aai:auth(), Name :: binary(), Endpoint :: binary(),
    Plugin :: binary(), Config :: #{}) -> {ok, od_harvester:id()} | errors:error().
create(Auth, Name, Endpoint, BackendType, Config) ->
    create(Auth, #{
        <<"name">> => Name,
        <<"harvestingBackendType">> => BackendType,
        <<"harvestingBackendEndpoint">> => Endpoint,
        <<"guiPLuginConfig">> => Config
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new harvester document in database. 
%% Harvester name, endpoint plugin and config are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create(Auth :: aai:auth(), Data :: #{}) ->
    {ok, od_harvester:id()} | errors:error().
create(Auth, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = undefined, aspect = instance},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a harvester record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(Auth :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    {ok, #od_harvester{}} | errors:error().
get(Auth, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves protected harvester data from database.
%% @end
%%--------------------------------------------------------------------
-spec get_protected_data(Auth :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    {ok, map()} | errors:error().
get_protected_data(Auth, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = instance, scope = protected}
    }).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves public harvester data from database.
%% @end
%%--------------------------------------------------------------------
-spec get_public_data(Auth :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    {ok, map()} | errors:error().
get_public_data(Auth, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = instance, scope = public}
    }).


-spec get_name(aai:auth(), od_harvester:id()) ->
    {ok, od_harvester:name()} | {error, term()}.
get_name(Auth, HarvesterId) ->
    case get(Auth, HarvesterId) of
        {ok, #od_harvester{name = Name}} -> {ok, Name};
        {error, _} = Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a harvester config from database.
%% @end
%%--------------------------------------------------------------------
-spec get_gui_plugin_config(Auth :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    {ok, #od_harvester{}} | errors:error().
get_gui_plugin_config(Auth, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = gui_plugin_config}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Lists all harvesters (their ids) in database.
%% @end
%%--------------------------------------------------------------------
-spec list(Auth :: aai:auth()) ->
    {ok, [od_harvester:id()]} | errors:error().
list(Auth) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = undefined, aspect = list}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Get all possible harvester privileges.
%% @end
%%--------------------------------------------------------------------
-spec list_privileges() -> {ok, map()} | errors:error().
list_privileges() ->
    entity_logic:handle(#el_req{
        operation = get,
        gri = #gri{type = od_harvester, id = undefined, aspect = privileges}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given harvester.
%% Harvester name, endpoint and plugin are given explicitly.
%% @end
%%--------------------------------------------------------------------
-spec update(Auth :: aai:auth(), Name :: binary(), HarvesterId :: od_harvester:id(),
    Endpoint :: binary(), Plugin :: binary()) -> {ok, od_harvester:id()} | errors:error().
update(Auth, HarvesterId, Name, Endpoint, Plugin) ->
    update(Auth, HarvesterId, #{
        <<"name">> => Name,
        <<"endpoint">> => Endpoint,
        <<"plugin">> => Plugin
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given harvester.
%% Harvester name, endpoint and plugin are provided in proper data object.
%% @end
%%--------------------------------------------------------------------
-spec update(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    Data :: #{}) -> ok | errors:error().
update(Auth, HarvesterId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = instance},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates gui plugin configuration of given harvester.
%% Config is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_gui_plugin_config(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    Data :: #{}) -> ok | errors:error().
update_gui_plugin_config(Auth, HarvesterId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = gui_plugin_config},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given harvester from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(Auth :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    ok | errors:error().
delete(Auth, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Deletes harvested metadata in all indices in given harvester.
%% @end
%%--------------------------------------------------------------------
-spec delete_harvested_metadata(Auth :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    ok | errors:error().
delete_harvested_metadata(Auth, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = metadata}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates index in given harvester.
%% @end
%%--------------------------------------------------------------------
-spec create_index(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    Name :: binary(), GuiPluginName :: binary()) -> ok | errors:error().
create_index(Auth, HarvesterId, Name, GuiPluginName) ->
    create_index(Auth, HarvesterId, #{
        <<"name">> => Name,
        <<"guiPluginName">> => GuiPluginName
    }).

%%--------------------------------------------------------------------
%% @doc
%% Creates index in given harvester.
%% @end
%%--------------------------------------------------------------------
-spec create_index(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    Name :: binary(), Schema :: od_harvester:schema(), GuiPluginName :: binary()) -> ok | errors:error().
create_index(Auth, HarvesterId, Name, Schema, GuiPluginName) ->
    create_index(Auth, HarvesterId, #{
        <<"name">> => Name,
        <<"schema">> => Schema,
        <<"guiPluginName">> => GuiPluginName
    }).

%%--------------------------------------------------------------------
%% @doc
%% Creates index in given harvester.
%% @end
%%--------------------------------------------------------------------
-spec create_index(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    Data :: map()) -> {ok, od_harvester:index_id()} | errors:error().
create_index(Auth, HarvesterId, Data) ->
    Res = entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = index},
        data = Data
    }),
    case Res of
        {ok, resource, {#gri{aspect = {index, IndexId}}, _}}  ->
            {ok, IndexId};
        {error, _} = Error -> 
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Deletes given index in given harvester.
%% @end
%%--------------------------------------------------------------------
-spec delete_index(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    IndexId :: od_harvester:index_id()) -> ok | errors:error().
delete_index(Auth, HarvesterId, IndexId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {index, IndexId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Deletes harvested metadata in given index.
%% @end
%%--------------------------------------------------------------------
-spec delete_index_metadata(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    IndexId :: od_harvester:index_id()) -> ok | errors:error().
delete_index_metadata(Auth, HarvesterId, IndexId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {index_metadata, IndexId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Lists all indices in given harvester.
%% @end
%%--------------------------------------------------------------------
-spec list_indices(Auth :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    ok | errors:error().
list_indices(Auth, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = indices}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a harvester index from database.
%% @end
%%--------------------------------------------------------------------
-spec get_index(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    IndexId :: od_harvester:index_id()) -> {ok, #od_harvester{}} | errors:error().
get_index(Auth, HarvesterId, IndexId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {index, IndexId}, scope = private}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a public harvester index data from database.
%% @end
%%--------------------------------------------------------------------
-spec get_public_index(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    IndexId :: od_harvester:index_id()) -> {ok, #od_harvester{}} | errors:error().
get_public_index(Auth, HarvesterId, IndexId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {index, IndexId}, scope = public}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a harvester index stats from database.
%% @end
%%--------------------------------------------------------------------
-spec get_index_stats(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    IndexId :: od_harvester:index_id()) -> {ok, od_harvester:indices_stats()} | errors:error().
get_index_stats(Auth, HarvesterId, IndexId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {index_stats, IndexId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates name and guiPluginName of given index.
%% Name and/or guiPluginName are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_index(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    IndexId :: od_harvester:index_id(), Data :: map()) -> ok | errors:error().
update_index(Auth, HarvesterId, IndexId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {index, IndexId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Submits given batch to harvesters backend.
%% Indices, MaxSeq and Batch are given explicitly.
%% @end
%%--------------------------------------------------------------------
-spec submit_batch(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    Indices :: od_harvester:indices(), SpaceId :: od_space:id(), Batch :: od_harvester:batch(), 
    MaxStreamSeq :: integer(), MaxSeq :: integer()) -> {ok, map()} | errors:error().
submit_batch(Auth, HarvesterId, Indices, SpaceId, Batch, MaxStreamSeq, MaxSeq) ->
    submit_batch(Auth, HarvesterId, SpaceId, #{
            <<"indices">> => Indices,
            <<"maxStreamSeq">> => MaxStreamSeq,
            <<"maxSeq">> => MaxSeq,
            <<"batch">> => Batch
    }).


%%--------------------------------------------------------------------
%% @doc
%% Submits given batch to harvesters backend.
%% Indices, MaxSeq and Batch are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec submit_batch(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    SpaceId :: od_space:id(), Data :: map()) -> {ok, map()} | errors:error().
submit_batch(Auth, HarvesterId, SpaceId, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {submit_batch, SpaceId}},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Query harvester backend using given data.
%% @end
%%--------------------------------------------------------------------
-spec query_index(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    IndexId :: od_harvester:index_id(), Data :: map()) -> ok | errors:error().
query_index(Auth, HarvesterId, IndexId, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {query, IndexId}},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Generate curl request to query harvester backend using given data.
%% @end
%%--------------------------------------------------------------------
-spec gen_curl_query(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    IndexId :: od_harvester:index_id(), public | private, Data :: map()) -> ok | errors:error().
gen_curl_query(Auth, HarvesterId, IndexId, Scope, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {gen_curl_query, IndexId}, scope = Scope},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Get all harvesting backend types.
%% @end
%%--------------------------------------------------------------------
-spec get_all_backend_types() -> ok | errors:error().
get_all_backend_types() ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = ?ROOT,
        gri = #gri{type = od_harvester, aspect = all_backend_types}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a user invite token, which can be used by any user to join
%% given harvester.
%% @end
%%--------------------------------------------------------------------
-spec create_user_invite_token(Auth :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    {ok, tokens:token()} | errors:error().
create_user_invite_token(Auth, HarvesterId) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = invite_user_token},
        data = #{}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a group invite token, which can be used by any group to join
%% given harvester.
%% @end
%%--------------------------------------------------------------------
-spec create_group_invite_token(Auth :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    {ok, tokens:token()} | errors:error().
create_group_invite_token(Auth, HarvesterId) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = invite_group_token},
        data = #{}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a space invite token, which can be used by any space to join
%% given harvester.
%% @end
%%--------------------------------------------------------------------
-spec create_space_invite_token(Auth :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    {ok, tokens:token()} | errors:error().
create_space_invite_token(Auth, HarvesterId) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = invite_space_token},
        data = #{}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given harvester.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Auth :: aai:auth(),
    HarvesterId :: od_harvester:id(), UserId :: od_user:id()) ->
    {ok, od_user:id()} | errors:error().
add_user(Auth, HarvesterId, UserId) ->
    add_user(Auth, HarvesterId, UserId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given harvester.
%% Allows to specify the privileges of the newly added user. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Auth :: aai:auth(),
    HarvesterId :: od_harvester:id(), UserId :: od_user:id(),
    PrivilegesPrivilegesOrData :: [privileges:harvester_privilege()] | #{}) ->
    {ok, od_user:id()} | errors:error().
add_user(Auth, HarvesterId, UserId, Privileges) when is_list(Privileges) ->
    add_user(Auth, HarvesterId, UserId, #{
        <<"privileges">> => Privileges
    });
add_user(Auth, HarvesterId, UserId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {user, UserId}},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified group to given harvester.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Auth :: aai:auth(),
    HarvesterId :: od_harvester:id(), GroupId :: od_group:id()) ->
    {ok, od_group:id()} | errors:error().
add_group(Auth, HarvesterId, GroupId) ->
    add_group(Auth, HarvesterId, GroupId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified group to given harvester.
%% Allows to specify the privileges of the newly added group. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Auth :: aai:auth(),
    HarvesterId :: od_harvester:id(), GroupId :: od_group:id(),
    PrivilegesOrData :: [privileges:harvester_privilege()] | #{}) ->
    {ok, od_group:id()} | errors:error().
add_group(Auth, HarvesterId, GroupId, Privileges) when is_list(Privileges) ->
    add_group(Auth, HarvesterId, GroupId, #{
        <<"privileges">> => Privileges
    });
add_group(Auth, HarvesterId, GroupId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {group, GroupId}},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new group in the harvester based on group name and type.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Auth :: aai:auth(), od_harvester:id(), od_group:name(),
    od_group:type()) -> {ok, od_group:id()} | errors:error().
create_group(Auth, HarvesterId, Name, Type) ->
    create_group(Auth, HarvesterId, #{<<"name">> => Name, <<"type">> => Type}).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new group in the harvester. Has two variants:
%% 1) Group Name is given explicitly (the new group will be of default type)
%% 2) Group name is provided in a proper Data object, group type is optional.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Auth :: aai:auth(), od_harvester:id(),
    NameOrData :: od_group:name() | #{}) -> {ok, od_group:id()} | errors:error().
create_group(Auth, HarvesterId, Name) when is_binary(Name) ->
    create_group(Auth, HarvesterId, #{<<"name">> => Name});
create_group(Auth, HarvesterId, Data) ->
    AuthHint = case Auth of
        ?USER(UserId) -> ?AS_USER(UserId);
        _ -> undefined
    end,
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = group},
        data = Data,
        auth_hint = AuthHint
    })).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified space to given harvester.
%% @end
%%--------------------------------------------------------------------
add_space(Auth, HarvesterId, SpaceId) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {space, SpaceId}}
    })).



%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of users of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_users(Auth :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    {ok, [od_user:id()]} | errors:error().
get_users(Auth, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = users}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective users of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_users(Auth :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    {ok, [od_user:id()]} | errors:error().
get_eff_users(Auth, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = eff_users}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific user among users of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_user(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id()) -> {ok, #{}} | errors:error().
get_user(Auth, HarvesterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HARVESTER(HarvesterId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective user among
%% effective users of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id()) -> {ok, #{}} | errors:error().
get_eff_user(Auth, HarvesterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HARVESTER(HarvesterId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific user among users of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id()) -> {ok, [privileges:harvester_privilege()]} | errors:error().
get_user_privileges(Auth, HarvesterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {user_privileges, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective user
%% among effective users of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user_privileges(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id()) -> {ok, [privileges:harvester_privilege()]} | errors:error().
get_eff_user_privileges(Auth, HarvesterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {eff_user_privileges, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the membership intermediaries of specific effective user
%% among effective users of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user_membership_intermediaries(Auth :: aai:auth(),
    HarvesterId :: od_harvester:id(), UserId :: od_user:id()) ->
    {ok, entity_graph:intermediaries()} | errors:error().
get_eff_user_membership_intermediaries(Auth, HarvesterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {eff_user_membership, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of groups of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(Auth :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    {ok, [od_group:id()]} | errors:error().
get_groups(Auth, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = groups}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective groups of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_groups(Auth :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    {ok, [od_group:id()]} | errors:error().
get_eff_groups(Auth, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = eff_groups}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific group among groups of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_group(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | errors:error().
get_group(Auth, HarvesterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HARVESTER(HarvesterId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective group among
%% effective groups of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | errors:error().
get_eff_group(Auth, HarvesterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HARVESTER(HarvesterId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific group among groups of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_group_privileges(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:harvester_privilege()]} | errors:error().
get_group_privileges(Auth, HarvesterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {group_privileges, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective group
%% among effective groups of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group_privileges(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:harvester_privilege()]} | errors:error().
get_eff_group_privileges(Auth, HarvesterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {eff_group_privileges, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the membership intermediaries of specific effective group
%% among effective groups of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group_membership_intermediaries(Auth :: aai:auth(),
    HarvesterId :: od_harvester:id(), GroupId :: od_group:id()) ->
    {ok, entity_graph:intermediaries()} | errors:error().
get_eff_group_membership_intermediaries(Auth, HarvesterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {eff_group_membership, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of spaces of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(Auth :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    {ok, [od_space:id()]} | errors:error().
get_spaces(Auth, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = spaces}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific space among spaces of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_space(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    SpaceId :: od_space:id()) -> {ok, #{}} | errors:error().
get_space(Auth, HarvesterId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_HARVESTER(HarvesterId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Joins a space on behalf of given harvester based on harvester_invite_space token.
%% Has two variants:
%% 1) Token is given explicitly
%% 2) Token is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec join_space(Auth :: aai:auth(), HarvesterId :: od_group:id(),
    TokenOrData :: tokens:serialized() | tokens:token() | map()) ->
    {ok, od_space:id()} | errors:error().
join_space(Auth, HarvesterId, Data) when is_map(Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_space, id = undefined, aspect = join},
        auth_hint = ?AS_HARVESTER(HarvesterId),
        data = Data
    }));
join_space(Auth, HarvesterId, Token) ->
    join_space(Auth, HarvesterId, #{<<"token">> => Token}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective providers of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_providers(Auth :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    {ok, [od_provider:id()]} | errors:error().
get_eff_providers(Auth, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = eff_providers}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective provider among
%% effective providers of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_provider(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    ProviderId :: od_provider:id()) -> {ok, #{}} | errors:error().
get_eff_provider(Auth, HarvesterId, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_provider, id = ProviderId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HARVESTER(HarvesterId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given harvester.
%% Allows to specify privileges to grant and to revoke.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id(), PrivsToGrant :: [privileges:harvester_privilege()],
    PrivsToRevoke :: [privileges:harvester_privilege()]) -> ok | errors:error().
update_user_privileges(Auth, HarvesterId, UserId, PrivsToGrant, PrivsToRevoke) ->
    update_user_privileges(Auth, HarvesterId, UserId, #{
        <<"grant">> => PrivsToGrant,
        <<"revoke">> => PrivsToRevoke
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given harvester.
%% Privileges to grant and revoke must be included in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id(), Data :: #{}) -> ok | errors:error().
update_user_privileges(Auth, HarvesterId, UserId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {user_privileges, UserId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified group of given harvester.
%% Allows to specify privileges to grant and to revoke.
%% @end
%%--------------------------------------------------------------------
-spec update_group_privileges(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    GroupId :: od_group:id(), PrivsToGrant :: [privileges:harvester_privilege()],
    PrivsToRevoke :: [privileges:harvester_privilege()]) -> ok | errors:error().
update_group_privileges(Auth, HarvesterId, GroupId, PrivsToGrant, PrivsToRevoke) ->
    update_group_privileges(Auth, HarvesterId, GroupId, #{
        <<"grant">> => PrivsToGrant,
        <<"revoke">> => PrivsToRevoke
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified group of given harvester.
%% Privileges to grant and revoke must be included in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_group_privileges(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    GroupId :: od_user:id(), Data :: #{}) -> ok | errors:error().
update_group_privileges(Auth, HarvesterId, GroupId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {group_privileges, GroupId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified space from given harvester.
%% @end
%%--------------------------------------------------------------------
-spec remove_space(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    SpaceId :: od_space:id()) -> ok | errors:error().
remove_space(Auth, HarvesterId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {space, SpaceId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified user from given harvester.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id()) -> ok | errors:error().
remove_user(Auth, HarvesterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {user, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified group from given harvester.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(Auth :: aai:auth(), HarvesterId :: od_harvester:id(),
    GroupId :: od_group:id()) -> ok | errors:error().
remove_group(Auth, HarvesterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {group, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a harvester exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(HarvesterId :: od_harvester:id()) -> boolean().
exists(HarvesterId) ->
    {ok, Exists} = od_harvester:exists(HarvesterId),
    Exists.

%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified effective user has specified
%% effective privilege in given harvester.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_privilege(HarvesterOrId :: od_harvester:id() | #od_harvester{},
    UserId :: od_user:id(), Privilege :: privileges:harvester_privilege()) ->
    boolean().
has_eff_privilege(HarvesterId, UserId, Privilege) when is_binary(HarvesterId) ->
    entity_graph:has_privilege(effective, bottom_up, od_user, UserId, Privilege, od_harvester, HarvesterId);
has_eff_privilege(Harvester, UserId, Privilege) ->
    entity_graph:has_privilege(effective, bottom_up, od_user, UserId, Privilege, Harvester).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified user is an direct user of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec has_direct_user(HarvesterOrId :: od_harvester:id() | #od_harvester{},
    UserId :: od_user:id()) -> boolean().
has_direct_user(HarvesterId, UserId) when is_binary(HarvesterId) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, od_harvester, HarvesterId);
has_direct_user(Harvester, UserId) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, Harvester).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified user is an effective user of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_user(HarvesterOrId :: od_harvester:id() | #od_harvester{},
    UserId :: od_user:id()) -> boolean().
has_eff_user(HarvesterId, UserId) when is_binary(HarvesterId) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, od_harvester, HarvesterId);
has_eff_user(Harvester, UserId) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Harvester).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified group is an effective group of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_group(HarvesterOrId :: od_harvester:id() | #od_harvester{},
    GroupId :: od_group:id()) -> boolean().
has_eff_group(HarvesterId, GroupId) when is_binary(HarvesterId) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, od_harvester, HarvesterId);
has_eff_group(Harvester, GroupId) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, Harvester).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified space is a member of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec has_space(HarvesterOrId :: od_harvester:id() | #od_harvester{},
    SpaceId :: od_space:id()) -> boolean().
has_space(HarvesterId, SpaceId) when is_binary(HarvesterId) ->
    entity_graph:has_relation(direct, top_down, od_space, SpaceId, od_harvester, HarvesterId);
has_space(Harvester, SpaceId) ->
    entity_graph:has_relation(direct, top_down, od_space, SpaceId, Harvester).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified provider is an effective provider of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_provider(HarvesterOrId :: od_harvester:id() | #od_harvester{},
    ProviderId :: od_provider:id()) -> boolean().
has_eff_provider(HarvesterId, ProviderId) when is_binary(HarvesterId) ->
    entity_graph:has_relation(effective, top_down, od_provider, ProviderId, od_harvester, HarvesterId);
has_eff_provider(Harvester, ProviderId) ->
    entity_graph:has_relation(effective, top_down, od_provider, ProviderId, Harvester).


-spec deploy_default_gui_package() -> ok.
deploy_default_gui_package() ->    
    PackagePath = oz_worker:get_env(default_hrv_gui_package_path),
    ok = gui_static:deploy_default_harvester_package(PackagePath).
