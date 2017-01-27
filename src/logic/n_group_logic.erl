%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all group logic functionalities.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(n_group_logic).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-define(PLUGIN, n_group_logic_plugin).

-export([
    create/2, create/3
]).
-export([
    get/2,
    get_data/2,
    list/1,
    get_oz_privileges/2, get_eff_oz_privileges/2
]).
-export([
    update/3,
    update_oz_privileges/4, update_oz_privileges/3
]).
-export([
    delete/2,
    delete_oz_privileges/2
]).
-export([
    create_space/3,
    create_handle_service/5, create_handle_service/3,
    create_handle/6, create_handle/3,

    create_user_invite_token/2,
    create_group_invite_token/2,

    join_group/3,
    join_space/3,

    add_user/3, add_user/4,
    add_group/3, add_group/4,

    get_users/2, get_eff_users/2,
    get_user/3, get_eff_user/3,
    get_user_privileges/3, get_eff_user_privileges/3,

    get_parents/2, get_eff_parents/2,
    get_parent/3, get_eff_parent/3,

    get_children/2, get_eff_children/2,
    get_child/3, get_eff_child/3,
    get_child_privileges/3, get_eff_child_privileges/3,

    get_spaces/2, get_eff_spaces/2,
    get_space/3, get_eff_space/3,

    get_eff_providers/2, get_eff_provider/3,

    get_handle_services/2, get_eff_handle_services/2,
    get_handle_service/3, get_eff_handle_service/3,

    get_handles/2, get_eff_handles/2,
    get_handle/3, get_eff_handle/3,

    update_user_privileges/5, update_user_privileges/4,
    update_child_privileges/5, update_child_privileges/4,

    leave_group/3,
    leave_space/3,
    leave_handle_service/3,
    leave_handle/3,

    remove_user/3,
    remove_group/3
]).
-export([
    exists/1,
    has_eff_privilege/3
]).
-export([
    create_predefined_groups/0
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new group document in database based on group name and type.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: n_entity_logic:client(), Name :: binary(),
    Type :: od_group:type()) -> {ok, od_group:id()} | {error, term()}.
create(Client, Name, Type) ->
    create(Client, #{<<"name">> => Name, <<"type">> => Type}).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new group document in database. Has two variants:
%% 1) Group Name is given explicitly (the new group will be of default type)
%% 2) Group name is provided in a proper Data object, group type is optional.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: n_entity_logic:client(), NameOrData :: binary() | #{}) ->
    {ok, od_group:id()} | {error, term()}.
create(Client, Name) when is_binary(Name) ->
    create(Client, #{<<"name">> => Name});
create(Client, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, undefined, entity, Data).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a group record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, #od_group{}} | {error, term()}.
get(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, entity, GroupId).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves information about a group record from database.
%% @end
%%--------------------------------------------------------------------
-spec get_data(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, #{}} | {error, term()}.
get_data(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, data, GroupId).


%%--------------------------------------------------------------------
%% @doc
%% Lists all groups (their ids) in database.
%% @end
%%--------------------------------------------------------------------
-spec list(Client :: n_entity_logic:client()) ->
    {ok, [od_group:id()]} | {error, term()}.
list(Client) ->
    n_entity_logic:get(Client, ?PLUGIN, undefined, list).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves oz privileges of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_oz_privileges(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, [privileges:oz_privilege()]} | {error, term()}.
get_oz_privileges(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, oz_privileges).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves effective oz privileges of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_oz_privileges(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, [privileges:oz_privilege()]} | {error, term()}.
get_eff_oz_privileges(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, eff_oz_privileges).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given group (name and alias).
%% @end
%%--------------------------------------------------------------------
-spec update(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    Data :: #{}) -> ok | {error, term()}.
update(Client, GroupId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, GroupId, entity, Data).


%%--------------------------------------------------------------------
%% @doc
%% Updates oz privileges of given group.
%% Allows to specify operation (set | grant | revoke) and the privileges.
%% @end
%%--------------------------------------------------------------------
-spec update_oz_privileges(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    Operation :: entity_graph:privileges_operation(),
    Privs :: [privileges:oz_privilege()]) -> ok | {error, term()}.
update_oz_privileges(Client, GroupId, Operation, Privs) when is_list(Privs) ->
    update_oz_privileges(Client, GroupId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates oz privileges of given group.
%% Privileges must be included in proper Data object, operation is optional.
%% @end
%%--------------------------------------------------------------------
-spec update_oz_privileges(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    Data :: #{}) -> ok | {error, term()}.
update_oz_privileges(Client, GroupId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, GroupId, oz_privileges, Data).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given group from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    ok | {error, term()}.
delete(Client, GroupId) ->
    n_entity_logic:delete(Client, ?PLUGIN, GroupId, entity).


%%--------------------------------------------------------------------
%% @doc
%% Deletes (sets to empty list) oz privileges of given group.
%% @end
%%--------------------------------------------------------------------
-spec delete_oz_privileges(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    ok | {error, term()}.
delete_oz_privileges(Client, GroupId) ->
    n_entity_logic:delete(Client, ?PLUGIN, GroupId, oz_privileges).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new space for given group. Has two variants:
%% 1) Space name is given explicitly
%% 2) Space name is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create_space(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    NameOrData :: binary() | #{}) -> {ok, od_space:id()} | {error, term()}.
create_space(Client, GroupId, Name) ->
    create_space(Client, GroupId, #{<<"name">> => Name});
create_space(Client, GroupId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, create_space, Data).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new handle service for given group.
%% Allows to specify the Name, ProxyEndpoint and ServiceProperties.
%% @end
%%--------------------------------------------------------------------
-spec create_handle_service(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    Name :: binary(), ProxyEndpoint :: od_handle_service:proxy_endpoint(),
    ServiceProperties :: od_handle_service:service_properties()) ->
    {ok, od_handle_service:id()} | {error, term()}.
create_handle_service(Client, GroupId, Name, ProxyEndpoint, ServiceProperties) ->
    create_handle_service(Client, GroupId, #{
        <<"name">> => Name,
        <<"proxyEndpoint">> => ProxyEndpoint,
        <<"serviceProperties">> => ServiceProperties
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new handle service for given group.
%% Name, ProxyEndpoint and ServiceProperties must be given in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create_handle_service(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    Data :: #{}) -> {ok, od_handle_service:id()} | {error, term()}.
create_handle_service(Client, GroupId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, create_handle_service, Data).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new handle for given group.
%% Allows to specify the HServiceId, ResourceType, ResourceId and Metadata.
%% @end
%%--------------------------------------------------------------------
-spec create_handle(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    HServiceId :: od_handle_service:id(), ResourceType :: od_handle:resource_type(),
    ResourceId :: od_handle:resource_id(), Metadata :: od_handle:metadata()) ->
    {ok, od_handle:id()} | {error, term()}.
create_handle(Client, GroupId, HServiceId, ResourceType, ResourceId, Metadata) ->
    create_handle(Client, GroupId, #{
        <<"handleServiceId">> => HServiceId,
        <<"resourceType">> => ResourceType,
        <<"resourceId">> => ResourceId,
        <<"metadata">> => Metadata
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new handle for given group.
%% HServiceId, ResourceType, ResourceId and Metadata must be given in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create_handle(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    Data :: #{}) -> {ok, od_handle:id()} | {error, term()}.
create_handle(Client, GroupId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, create_handle, Data).


%%--------------------------------------------------------------------
%% @doc
%% Creates a user invite token, which can be used by any user to join
%% given group.
%% @end
%%--------------------------------------------------------------------
-spec create_user_invite_token(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
create_user_invite_token(Client, GroupId) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, invite_user_token, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Creates a group invite token, which can be used by any child group to join
%% given group.
%% @end
%%--------------------------------------------------------------------
-spec create_group_invite_token(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
create_group_invite_token(Client, GroupId) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, invite_group_token, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Joins a group on behalf of given user based on group_invite_user token.
%% Has two variants:
%% 1) Token is given explicitly (as binary() or macaroon())
%% 2) Token is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec join_group(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    TokenOrData :: token:id() | macaroon:macaroon() | #{}) ->
    {ok, od_group:id()} | {error, term()}.
join_group(Client, GroupId, Data) when is_map(Data) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, join_group, Data);
join_group(Client, GroupId, Token) ->
    join_group(Client, GroupId, #{<<"token">> => Token}).


%%--------------------------------------------------------------------
%% @doc
%% Joins a space on behalf of given user based on space_invite_user token.
%% Has two variants:
%% 1) Token is given explicitly (as binary() or macaroon())
%% 2) Token is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec join_space(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    TokenOrData :: token:id() | macaroon:macaroon() | #{}) ->
    {ok, od_space:id()} | {error, term()}.
join_space(Client, GroupId, Data) when is_map(Data) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, join_space, Data);
join_space(Client, GroupId, Token) ->
    join_space(Client, GroupId, #{<<"token">> => Token}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given group.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Client :: n_entity_logic:client(),
    GroupId :: od_group:id(), UserId :: od_user:id()) ->
    {ok, od_user:id()} | {error, term()}.
add_user(Client, GroupId, UserId)  ->
    add_user(Client, GroupId, UserId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given group.
%% Allows to specify the privileges of the newly added user. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Client :: n_entity_logic:client(),
    GroupId :: od_group:id(), UserId :: od_user:id(),
    PrivilegesPrivilegesOrData :: [privileges:group_privileges()] | #{}) ->
    {ok, od_user:id()} | {error, term()}.
add_user(Client, GroupId, UserId, Privileges) when is_list(Privileges) ->
    add_user(Client, GroupId, UserId, #{
        <<"privileges">> => Privileges
    });
add_user(Client, GroupId, UserId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, {user, UserId}, Data).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified child group to given parent group.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Client :: n_entity_logic:client(),
    GroupId :: od_group:id(), ChildGroupId :: od_group:id()) ->
    {ok, od_group:id()} | {error, term()}.
add_group(Client, GroupId, ChildGroupId)  ->
    add_group(Client, GroupId, ChildGroupId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified child group to given parent group.
%% Allows to specify the privileges of the newly added group. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Client :: n_entity_logic:client(),
    GroupId :: od_group:id(), ChildGroupId :: od_group:id(),
    PrivilegesOrData :: [privileges:group_privileges()] | #{}) ->
    {ok, od_group:id()} | {error, term()}.
add_group(Client, GroupId, ChildGroupId, Privileges) when is_binary(ChildGroupId) ->
    add_group(Client, GroupId, ChildGroupId, #{
        <<"privileges">> => Privileges
    });
add_group(Client, GroupId, ChildGroupId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, {child, ChildGroupId}, Data).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of users of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_users(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_users(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, users).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective users of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_users(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_eff_users(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, eff_users).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific user among users of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_user(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_user(Client, GroupId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {user, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective user among
%% effective users of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_eff_user(Client, GroupId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_user, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific user among users of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_user_privileges(Client, GroupId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {user_privileges, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective user
%% among effective users of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user_privileges(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_eff_user_privileges(Client, GroupId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_user_privileges, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of parents of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_parents(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_parents(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, parents).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective parents of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_parents(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_eff_parents(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, eff_parents).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific parent among parents of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_parent(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    ParentId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_parent(Client, GroupId, ParentId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {parent, ParentId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective parent among
%% effective parents of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_parent(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    ParentId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_eff_parent(Client, GroupId, ParentId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_parent, ParentId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of children of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_children(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_children(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, children).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective children of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_children(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_eff_children(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, eff_children).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific child among children of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_child(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    ChildId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_child(Client, GroupId, ChildId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {child, ChildId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective child among
%% effective children of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_child(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    ChildId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_eff_child(Client, GroupId, ChildId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_child, ChildId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific child among children of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_child_privileges(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    ChildId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_child_privileges(Client, GroupId, ChildId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {child_privileges, ChildId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective child
%% among effective children of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_child_privileges(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    ChildId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_eff_child_privileges(Client, GroupId, ChildId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_child_privileges, ChildId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of spaces of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, [od_space:id()]} | {error, term()}.
get_spaces(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, spaces).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective spaces of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_spaces(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, [od_space:id()]} | {error, term()}.
get_eff_spaces(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, eff_spaces).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific space among spaces of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_space(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    SpaceId :: od_space:id()) -> {ok, #{}} | {error, term()}.
get_space(Client, GroupId, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {space, SpaceId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective space among
%% effective spaces of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_space(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    SpaceId :: od_space:id()) -> {ok, #{}} | {error, term()}.
get_eff_space(Client, GroupId, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_space, SpaceId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective providers of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_providers(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, [od_provider:id()]} | {error, term()}.
get_eff_providers(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, eff_providers).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective provider among
%% effective providers of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_provider(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    ProviderId :: od_provider:id()) -> {ok, #{}} | {error, term()}.
get_eff_provider(Client, GroupId, ProviderId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_provider, ProviderId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of handle_services of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_handle_services(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, [od_handle_service:id()]} | {error, term()}.
get_handle_services(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, handle_services).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective handle_services of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_handle_services(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, [od_handle_service:id()]} | {error, term()}.
get_eff_handle_services(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, eff_handle_services).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific handle_service among
%% handle_services of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_handle_service(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    HServiceId :: od_handle_service:id()) -> {ok, #{}} | {error, term()}.
get_handle_service(Client, GroupId, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {handle_service, HServiceId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective handle_service among
%% effective handle_services of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_handle_service(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    HServiceId :: od_handle_service:id()) -> {ok, #{}} | {error, term()}.
get_eff_handle_service(Client, GroupId, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_handle_service, HServiceId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of handles of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_handles(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, [od_handle:id()]} | {error, term()}.
get_handles(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, handles).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective handles of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_handles(Client :: n_entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, [od_handle:id()]} | {error, term()}.
get_eff_handles(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, eff_handles).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific handle among
%% handles of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_handle(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    HandleId :: od_handle:id()) -> {ok, #{}} | {error, term()}.
get_handle(Client, GroupId, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {handle, HandleId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective handle among
%% effective handles of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_handle(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    HandleId :: od_handle:id()) -> {ok, #{}} | {error, term()}.
get_eff_handle(Client, GroupId, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_handle, HandleId}).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given group.
%% Allows to specify operation (set | grant | revoke) and the privileges.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    UserId :: od_user:id(), Operation :: entity_graph:privileges_operation(),
    Privs :: [privileges:group_privilege()]) -> ok | {error, term()}.
update_user_privileges(Client, GroupId, UserId, Operation, Privs) when is_list(Privs) ->
    update_user_privileges(Client, GroupId, UserId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given group.
%% Privileges must be included in proper Data object, operation is optional.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    UserId :: od_user:id(), Data :: #{}) -> ok | {error, term()}.
update_user_privileges(Client, GroupId, UserId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, GroupId, {user_privileges, UserId}, Data).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified child of given group.
%% Allows to specify operation (set | grant | revoke) and the privileges.
%% @end
%%--------------------------------------------------------------------
-spec update_child_privileges(Client :: n_entity_logic:client(), ParentId :: od_group:id(),
    ChildId :: od_group:id(), Operation :: entity_graph:privileges_operation(),
    Privs :: [privileges:group_privilege()]) -> ok | {error, term()}.
update_child_privileges(Client, ParentId, ChildId, Operation, Privs) when is_list(Privs) ->
    update_child_privileges(Client, ParentId, ChildId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified child of given group.
%% Privileges must be included in proper Data object, operation is optional.
%% @end
%%--------------------------------------------------------------------
-spec update_child_privileges(Client :: n_entity_logic:client(), ParentId :: od_group:id(),
    ChildId :: od_user:id(), Data :: #{}) -> ok | {error, term()}.
update_child_privileges(Client, ParentId, ChildId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, ParentId, {child_privileges, ChildId}, Data).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified parent group on behalf of given child group.
%% @end
%%--------------------------------------------------------------------
-spec leave_group(Client :: n_entity_logic:client(), ChildId :: od_user:id(),
    ParentId :: od_group:id()) -> ok | {error, term()}.
leave_group(Client, ChildId, ParentId) ->
    n_entity_logic:delete(Client, ?PLUGIN, ChildId, {parent, ParentId}).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified space on behalf of given group.
%% @end
%%--------------------------------------------------------------------
-spec leave_space(Client :: n_entity_logic:client(), GroupId :: od_user:id(),
    SpaceId :: od_space:id()) -> ok | {error, term()}.
leave_space(Client, GroupId, SpaceId) ->
    n_entity_logic:delete(Client, ?PLUGIN, GroupId, {space, SpaceId}).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified od_handle_service on behalf of given group.
%% @end
%%--------------------------------------------------------------------
-spec leave_handle_service(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    HServiceId :: od_handle_service:id()) -> ok | {error, term()}.
leave_handle_service(Client, GroupId, HServiceId) ->
    n_entity_logic:delete(Client, ?PLUGIN, GroupId, {handle_service, HServiceId}).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified handle on behalf of given group.
%% @end
%%--------------------------------------------------------------------
-spec leave_handle(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    HandleId :: od_handle:id()) -> ok | {error, term()}.
leave_handle(Client, GroupId, HandleId) ->
    n_entity_logic:delete(Client, ?PLUGIN, GroupId, {handle, HandleId}).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified user from given group.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(Client :: n_entity_logic:client(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> ok | {error, term()}.
remove_user(Client, GroupId, UserId) ->
    n_entity_logic:delete(Client, ?PLUGIN, GroupId, {user, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified child group from given parent group.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(Client :: n_entity_logic:client(), ParentId :: od_group:id(),
    ChildId :: od_group:id()) -> ok | {error, term()}.
remove_group(Client, ParentId, ChildId) ->
    n_entity_logic:delete(Client, ?PLUGIN, ParentId, {child, ChildId}).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a group exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(GroupId :: od_group:id()) -> boolean().
exists(GroupId) ->
    od_group:exists(GroupId).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified effective user has specified
%% effective privilege in given group.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_privilege(GroupOrId :: od_group:id() | #od_group{},
    UserId :: od_user:id(), Privilege :: privileges:group_privileges()) ->
    boolean().
has_eff_privilege(GroupId, UserId, Privilege) when is_binary(GroupId) ->
    case od_group:get(GroupId) of
        {ok, #document{value = Group}} ->
            has_eff_privilege(Group, UserId, Privilege);
        _ ->
            false
    end;
has_eff_privilege(#od_group{eff_users = UsersPrivileges}, UserId, Privilege) ->
    {UserPrivileges, _} = maps:get(UserId, UsersPrivileges, {[], []}),
    lists:member(Privilege, UserPrivileges).


%%--------------------------------------------------------------------
%% @doc
%% Creates predefined groups in the system based on settings in app.config.
%% @end
%%--------------------------------------------------------------------
-spec create_predefined_groups() -> ok.
create_predefined_groups() ->
    {ok, PredefinedGroups} =
        application:get_env(?APP_NAME, predefined_groups),
    lists:foreach(
        fun(GroupMap) ->
            Id = maps:get(id, GroupMap),
            Name = maps:get(name, GroupMap),
            % Privileges can be either a list of privileges or a module and
            % function to call that will return such list.
            Privs = case maps:get(oz_privileges, GroupMap) of
                List when is_list(List) ->
                    List;
                {Module, Function} ->
                    Module:Function()
            end,
            create_predefined_group(Id, Name, Privs)
        end, PredefinedGroups).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a predefined group in the system, if it does not exist, and grants
%% given privileges to it.
%% @end
%%--------------------------------------------------------------------
-spec create_predefined_group(Id :: binary(), Name :: binary(),
    Privileges :: [privileges:oz_privilege()]) -> ok | error.
create_predefined_group(GroupId, Name, Privileges) ->
    case od_group:exists(GroupId) of
        true ->
            ?info("Predefined group '~s' already exists, "
            "skipping.", [Name]),
            ok;
        false ->
            NewGroup = #document{
                key = GroupId,
                value = #od_group{
                    name = Name,
                    type = role
                }},
            case od_group:create(NewGroup) of
                {ok, GroupId} ->
                    ok = update_oz_privileges(?ROOT, GroupId, set, Privileges),
                    ?info("Created predefined group '~s'", [Name]),
                    ok;
                Other ->
                    ?error("Cannot create predefined group '~s' - ~p",
                        [GroupId, Other]),
                    error
            end
    end.
