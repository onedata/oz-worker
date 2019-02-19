%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all harvester logic functionalities.
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
    list/1,
    get_all_plugins/0,
    get_config/2
]).
-export([
    update/3, update/5,
    update_config/3
]).
-export([
    delete/2
]).
-export([
    submit_entry/4,
    delete_entry/3,
    query/3
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
    has_space/2
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new group document in database based on harvester name, endpoint, plugin and config.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: entity_logic:client(), Name :: binary(), Endpoint :: binary(), 
    Plugin :: binary(), Config :: #{}) ->
    {ok, od_harvester:id()} | {error, term()}.
create(Client, Name, Endpoint, Plugin, Config) ->
    create(Client, #{
        <<"name">> => Name,
        <<"endpoint">> => Endpoint,
        <<"plugin">> => Plugin,
        <<"config">> => Config
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new group document in database. Harvester name, endpoint 
%% and plugin is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: entity_logic:client(), Data :: #{}) -> 
    {ok, od_harvester:id()} | {error, term()}.
create(Client, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_harvester, id = undefined, aspect = instance},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a harvester record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(Client :: entity_logic:client(), HarvesterId :: od_harvester:id()) ->
    {ok, #od_harvester{}} | {error, term()}.
get(Client, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves protected harvester data from database.
%% @end
%%--------------------------------------------------------------------
-spec get_protected_data(Client :: entity_logic:client(), HarvesterId :: od_harvester:id()) ->
    {ok, maps:map()} | {error, term()}.
get_protected_data(Client, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = instance, scope = protected}
    }).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a harvester config from database.
%% @end
%%--------------------------------------------------------------------
-spec get_config(Client :: entity_logic:client(), HarvesterId :: od_harvester:id()) ->
    {ok, #od_harvester{}} | {error, term()}.
get_config(Client, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = config}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Lists all harvesters (their ids) in database.
%% @end
%%--------------------------------------------------------------------
-spec list(Client :: entity_logic:client()) ->
    {ok, [od_harvester:id()]} | {error, term()}.
list(Client) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_harvester, id = undefined, aspect = list}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given harvester.
%% Harvester Name, Endpoint and Plugin are given explicitly
%% @end
%%--------------------------------------------------------------------
-spec update(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    NewName :: binary(), NewEndpoint :: binary(), NewPlugin :: binary()) ->
    ok | {error, term()}.
update(Client, HarvesterId, NewName, NewEndpoint, NewPlugin) ->
    update(Client, HarvesterId, #{
        <<"name">> => NewName,
        <<"endpoint">> => NewEndpoint,
        <<"plugin">> => NewPlugin
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given harvester.
%% Harvester Name, Endpoint and Plugin are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    Data :: #{}) -> ok | {error, term()}.
update(Client, HarvesterId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = instance},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates configuration of given harvester.
%% Config is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_config(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(), 
    Data :: #{}) -> ok | {error, term()}.
update_config(Client, HarvesterId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = config},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given harvester from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(Client :: entity_logic:client(), HarvesterId :: od_harvester:id()) ->
    ok | {error, term()}.
delete(Client, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Submits given data to harvesters search engine.
%% @end
%%--------------------------------------------------------------------
-spec submit_entry(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(), 
    Data :: maps:map(), FileId :: binary()) -> ok | {error, term()}.
submit_entry(Client, HarvesterId, FileId, Data) ->
    entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {entry, FileId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes given FileId from harvesters search engine.
%% @end
%%--------------------------------------------------------------------
-spec delete_entry(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    FileId :: binary()) -> ok | {error, term()}.
delete_entry(Client, HarvesterId, FileId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {entry, FileId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Query harvester engine using given data.
%% @end
%%--------------------------------------------------------------------
-spec query(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    Data :: maps:map()) -> ok | {error, term()}.
query(Client, HarvesterId, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = query},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Get all harvester plugins.
%% @end
%%--------------------------------------------------------------------
-spec get_all_plugins() -> ok | {error, term()}.
get_all_plugins() ->
    entity_logic:handle(#el_req{
        operation = get,
        client = ?ROOT,
        gri = #gri{type = od_harvester, aspect = all_plugins}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a user invite token, which can be used by any user to join
%% given harvester.
%% @end
%%--------------------------------------------------------------------
-spec create_user_invite_token(Client :: entity_logic:client(), HarvesterId :: od_harvester:id()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
create_user_invite_token(Client, HarvesterId) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = invite_user_token},
        data = #{}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a group invite token, which can be used by any group to join
%% given harvester.
%% @end
%%--------------------------------------------------------------------
-spec create_group_invite_token(Client :: entity_logic:client(), HarvesterId :: od_harvester:id()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
create_group_invite_token(Client, HarvesterId) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = invite_group_token},
        data = #{}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a space invite token, which can be used by any space to join
%% given harvester.
%% @end
%%--------------------------------------------------------------------
-spec create_space_invite_token(Client :: entity_logic:client(), HarvesterId :: od_harvester:id()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
create_space_invite_token(Client, HarvesterId) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = invite_space_token},
        data = #{}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given harvester.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Client :: entity_logic:client(),
    HarvesterId :: od_harvester:id(), UserId :: od_user:id()) ->
    {ok, od_user:id()} | {error, term()}.
add_user(Client, HarvesterId, UserId) ->
    add_user(Client, HarvesterId, UserId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given harvester.
%% Allows to specify the privileges of the newly added user. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Client :: entity_logic:client(),
    HarvesterId :: od_harvester:id(), UserId :: od_user:id(),
    PrivilegesPrivilegesOrData :: [privileges:harvester_privileges()] | #{}) ->
    {ok, od_user:id()} | {error, term()}.
add_user(Client, HarvesterId, UserId, Privileges) when is_list(Privileges) ->
    add_user(Client, HarvesterId, UserId, #{
        <<"privileges">> => Privileges
    });
add_user(Client, HarvesterId, UserId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {user, UserId}},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified group to given harvester.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Client :: entity_logic:client(),
    HarvesterId :: od_harvester:id(), GroupId :: od_group:id()) ->
    {ok, od_group:id()} | {error, term()}.
add_group(Client, HarvesterId, GroupId) ->
    add_group(Client, HarvesterId, GroupId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified group to given harvester.
%% Allows to specify the privileges of the newly added group. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Client :: entity_logic:client(),
    HarvesterId :: od_harvester:id(), GroupId :: od_group:id(),
    PrivilegesOrData :: [privileges:harvester_privileges()] | #{}) ->
    {ok, od_group:id()} | {error, term()}.
add_group(Client, HarvesterId, GroupId, Privileges) when is_list(Privileges) ->
    add_group(Client, HarvesterId, GroupId, #{
        <<"privileges">> => Privileges
    });
add_group(Client, HarvesterId, GroupId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {group, GroupId}},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new group in the harvester based on group name and type.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Client :: entity_logic:client(), od_harvester:id(), od_group:name(),
    od_group:type()) -> {ok, od_group:id()} | {error, term()}.
create_group(Client, HarvesterId, Name, Type) ->
    create_group(Client, HarvesterId, #{<<"name">> => Name, <<"type">> => Type}).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new group in the harvester. Has two variants:
%% 1) Group Name is given explicitly (the new group will be of default type)
%% 2) Group name is provided in a proper Data object, group type is optional.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Client :: entity_logic:client(), od_harvester:id(),
    NameOrData :: od_group:name() | #{}) -> {ok, od_group:id()} | {error, term()}.
create_group(Client, HarvesterId, Name) when is_binary(Name) ->
    create_group(Client, HarvesterId, #{<<"name">> => Name});
create_group(Client, HarvesterId, Data) ->
    AuthHint = case Client of
                   ?USER(UserId) -> ?AS_USER(UserId);
                   _ -> undefined
               end,
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = group},
        data = Data,
        auth_hint = AuthHint
    })).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified space to given harvester.
%% @end
%%--------------------------------------------------------------------
add_space(Client, HarvesterId, SpaceId) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {space, SpaceId}}
    })).



%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of users of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_users(Client :: entity_logic:client(), HarvesterId :: od_harvester:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_users(Client, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = users}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective users of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_users(Client :: entity_logic:client(), HarvesterId :: od_harvester:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_eff_users(Client, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = eff_users}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific user among users of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_user(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_user(Client, HarvesterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HARVESTER(HarvesterId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective user among
%% effective users of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_eff_user(Client, HarvesterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HARVESTER(HarvesterId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific user among users of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id()) -> {ok, [privileges:harvester_privileges()]} | {error, term()}.
get_user_privileges(Client, HarvesterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {user_privileges, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective user
%% among effective users of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user_privileges(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id()) -> {ok, [privileges:harvester_privileges()]} | {error, term()}.
get_eff_user_privileges(Client, HarvesterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {eff_user_privileges, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the membership intermediaries of specific effective user
%% among effective users of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user_membership_intermediaries(Client :: entity_logic:client(),
    HarvesterId :: od_harvester:id(), UserId :: od_user:id()) ->
    {ok, entity_graph:intermediaries()} | {error, term()}.
get_eff_user_membership_intermediaries(Client, HarvesterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {eff_user_membership, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of groups of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(Client :: entity_logic:client(), HarvesterId :: od_harvester:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_groups(Client, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = groups}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective groups of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_groups(Client :: entity_logic:client(), HarvesterId :: od_harvester:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_eff_groups(Client, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = eff_groups}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific group among groups of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_group(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_group(Client, HarvesterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HARVESTER(HarvesterId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective group among
%% effective groups of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_eff_group(Client, HarvesterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_HARVESTER(HarvesterId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific group among groups of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_group_privileges(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:harvester_privileges()]} | {error, term()}.
get_group_privileges(Client, HarvesterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {group_privileges, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective group
%% among effective groups of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group_privileges(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:harvester_privileges()]} | {error, term()}.
get_eff_group_privileges(Client, HarvesterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {eff_group_privileges, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the membership intermediaries of specific effective group
%% among effective groups of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group_membership_intermediaries(Client :: entity_logic:client(),
    HarvesterId :: od_harvester:id(), GroupId :: od_group:id()) ->
    {ok, entity_graph:intermediaries()} | {error, term()}.
get_eff_group_membership_intermediaries(Client, HarvesterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {eff_group_membership, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of spaces of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(Client :: entity_logic:client(), HarvesterId :: od_harvester:id()) ->
    {ok, [od_space:id()]} | {error, term()}.
get_spaces(Client, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = spaces}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific space among spaces of given harvester.
%% @end
%%--------------------------------------------------------------------
-spec get_space(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    SpaceId :: od_space:id()) -> {ok, #{}} | {error, term()}.
get_space(Client, HarvesterId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_space, id = SpaceId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_HARVESTER(HarvesterId)
    }).

%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given harvester.
%% Allows to specify privileges to grant and to revoke.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id(), PrivsToGrant :: [privileges:harvester_privilege()],
    PrivsToRevoke :: [privileges:harvester_privilege()]) -> ok | {error, term()}.
update_user_privileges(Client, HarvesterId, UserId, PrivsToGrant, PrivsToRevoke) ->
    update_user_privileges(Client, HarvesterId, UserId, #{
        <<"grant">> => PrivsToGrant,
        <<"revoke">> => PrivsToRevoke
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given harvester.
%% Privileges to grant and revoke must be included in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id(), Data :: #{}) -> ok | {error, term()}.
update_user_privileges(Client, HarvesterId, UserId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {user_privileges, UserId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified group of given harvester.
%% Allows to specify privileges to grant and to revoke.
%% @end
%%--------------------------------------------------------------------
-spec update_group_privileges(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    GroupId :: od_group:id(), PrivsToGrant :: [privileges:harvester_privilege()],
    PrivsToRevoke :: [privileges:harvester_privilege()]) -> ok | {error, term()}.
update_group_privileges(Client, HarvesterId, GroupId, PrivsToGrant, PrivsToRevoke) ->
    update_group_privileges(Client, HarvesterId, GroupId, #{
        <<"grant">> => PrivsToGrant,
        <<"revoke">> => PrivsToRevoke
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified group of given harvester.
%% Privileges to grant and revoke must be included in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_group_privileges(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    GroupId :: od_user:id(), Data :: #{}) -> ok | {error, term()}.
update_group_privileges(Client, HarvesterId, GroupId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {group_privileges, GroupId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified space from given harvester.
%% @end
%%--------------------------------------------------------------------
-spec remove_space(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    SpaceId :: od_space:id()) -> ok | {error, term()}.
remove_space(Client, HarvesterId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {space, SpaceId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified user from given harvester.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id()) -> ok | {error, term()}.
remove_user(Client, HarvesterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        client = Client,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = {user, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified group from given harvester.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(Client :: entity_logic:client(), HarvesterId :: od_harvester:id(),
    GroupId :: od_group:id()) -> ok | {error, term()}.
remove_group(Client, HarvesterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        client = Client,
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
    UserId :: od_user:id(), Privilege :: privileges:harvester_privileges()) ->
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
    entity_graph:has_relation(direct, bottom_up, od_space, SpaceId, od_harvester, HarvesterId);
has_space(Harvester, SpaceId) ->
    entity_graph:has_relation(direct, bottom_up, od_space, SpaceId, Harvester).


