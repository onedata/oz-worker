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
-module(group_logic).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("entity_logic.hrl").
-include_lib("ctool/include/logging.hrl").

-export([
    create/2, create/3
]).
-export([
    get/2,
    get_protected_data/2,
    get_shared_data/2,
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
    create_parent_group/4, create_parent_group/3,
    create_space/3,
    create_handle_service/5, create_handle_service/3,
    create_handle/6, create_handle/3,
    create_harvester/3, create_harvester/6,

    create_user_invite_token/2,
    create_group_invite_token/2,

    create_child_group/3, create_child_group/4,
    join_group/3,
    join_space/3,
    join_harvester/3,
    join_cluster/3,

    add_user/3, add_user/4,
    add_group/3, add_group/4,

    get_name/2,

    get_parents/2, get_eff_parents/2,
    get_parent/3, get_eff_parent/3,

    get_children/2, get_eff_children/2,
    get_child/3, get_eff_child/3,
    get_child_privileges/3, get_eff_child_privileges/3,
    get_eff_child_membership_intermediaries/3,

    get_users/2, get_eff_users/2,
    get_user/3, get_eff_user/3,
    get_user_privileges/3, get_eff_user_privileges/3,
    get_eff_user_membership_intermediaries/3,

    get_spaces/2, get_eff_spaces/2,
    get_space/3, get_eff_space/3,

    get_eff_providers/2, get_eff_provider/3,
    get_spaces_in_eff_provider/3,

    get_handle_services/2, get_eff_handle_services/2,
    get_handle_service/3, get_eff_handle_service/3,

    get_handles/2, get_eff_handles/2,
    get_handle/3, get_eff_handle/3,

    get_harvesters/2, get_eff_harvesters/2,
    get_harvester/3, get_eff_harvester/3,

    get_clusters/2, get_eff_clusters/2,
    get_cluster/3, get_eff_cluster/3,

    update_user_privileges/5, update_user_privileges/4,
    update_child_privileges/5, update_child_privileges/4,

    leave_group/3,
    leave_space/3,
    leave_handle_service/3,
    leave_handle/3,
    leave_harvester/3,
    leave_cluster/3,

    remove_user/3,
    remove_group/3
]).
-export([
    exists/1,
    has_eff_parent/2,
    has_eff_child/2,
    has_direct_user/2,
    has_eff_user/2,
    has_eff_space/2,
    has_eff_provider/2,
    has_eff_handle_service/2,
    has_eff_handle/2,
    has_eff_harvester/2,
    has_eff_cluster/2,
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
-spec create(Auth :: aai:auth(), Name :: binary(),
    Type :: od_group:type()) -> {ok, od_group:id()} | {error, term()}.
create(Auth, Name, Type) ->
    create(Auth, #{<<"name">> => Name, <<"type">> => Type}).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new group document in database. Has two variants:
%% 1) Group Name is given explicitly (the new group will be of default type)
%% 2) Group name is provided in a proper Data object, group type is optional.
%% @end
%%--------------------------------------------------------------------
-spec create(Auth :: aai:auth(), NameOrData :: binary() | #{}) ->
    {ok, od_group:id()} | {error, term()}.
create(Auth, Name) when is_binary(Name) ->
    create(Auth, #{<<"name">> => Name});
create(Auth, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_group, id = undefined, aspect = instance},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a group record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, #od_group{}} | {error, term()}.
get(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves protected group data from database.
%% @end
%%--------------------------------------------------------------------
-spec get_protected_data(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, map()} | {error, term()}.
get_protected_data(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = protected}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves shared group data from database.
%% @end
%%--------------------------------------------------------------------
-spec get_shared_data(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, #od_group{}} | {error, term()}.
get_shared_data(Auth, GroupId) ->
    % Currently, these two return the same data
    get_protected_data(Auth, GroupId).


%%--------------------------------------------------------------------
%% @doc
%% Lists all groups (their ids) in database.
%% @end
%%--------------------------------------------------------------------
-spec list(Auth :: aai:auth()) ->
    {ok, [od_group:id()]} | {error, term()}.
list(Auth) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = undefined, aspect = list}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves oz privileges of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_oz_privileges(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [privileges:oz_privilege()]} | {error, term()}.
get_oz_privileges(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = oz_privileges}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves effective oz privileges of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_oz_privileges(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [privileges:oz_privilege()]} | {error, term()}.
get_eff_oz_privileges(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = eff_oz_privileges}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given group (name and type).
%% @end
%%--------------------------------------------------------------------
-spec update(Auth :: aai:auth(), GroupId :: od_group:id(),
    Data :: #{}) -> ok | {error, term()}.
update(Auth, GroupId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = instance},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates oz privileges of given group.
%% Allows to specify privileges to grant and to revoke.
%% @end
%%--------------------------------------------------------------------
-spec update_oz_privileges(Auth :: aai:auth(), GroupId :: od_group:id(),
    PrivsToGrant :: [privileges:oz_privilege()],
    PrivsToRevoke :: [privileges:oz_privilege()]) -> ok | {error, term()}.
update_oz_privileges(Auth, GroupId, PrivsToGrant, PrivsToRevoke) ->
    update_oz_privileges(Auth, GroupId, #{
        <<"grant">> => PrivsToGrant,
        <<"revoke">> => PrivsToRevoke
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates oz privileges of given group.
%% Privileges to grant and revoke must be included in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_oz_privileges(Auth :: aai:auth(), GroupId :: od_group:id(),
    Data :: #{}) -> ok | {error, term()}.
update_oz_privileges(Auth, GroupId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = oz_privileges},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given group from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    ok | {error, term()}.
delete(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Deletes (sets to empty list) oz privileges of given group.
%% @end
%%--------------------------------------------------------------------
-spec delete_oz_privileges(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    ok | {error, term()}.
delete_oz_privileges(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = oz_privileges}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new parent group document in database based on group name and type.
%% @end
%%--------------------------------------------------------------------
-spec create_parent_group(Auth :: aai:auth(), GroupId :: od_group:id(),
    Name :: binary(), Type :: od_group:type()) -> {ok, od_group:id()} | {error, term()}.
create_parent_group(Auth, GroupId, Name, Type) ->
    create_parent_group(Auth, GroupId, #{<<"name">> => Name, <<"type">> => Type}).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new parent group document in database. Has two variants:
%% 1) Group Name is given explicitly (the new group will be of default type)
%% 2) Group name is provided in a proper Data object, group type is optional.
%% @end
%%--------------------------------------------------------------------
-spec create_parent_group(Auth :: aai:auth(), GroupId :: od_group:id(),
    NameOrData :: binary() | #{}) -> {ok, od_space:id()} | {error, term()}.
create_parent_group(Auth, GroupId, Name) when is_binary(Name) ->
    create_parent_group(Auth, GroupId, #{<<"name">> => Name});
create_parent_group(Auth, GroupId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_group, id = undefined, aspect = instance},
        auth_hint = ?AS_GROUP(GroupId),
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new space for given group. Has two variants:
%% 1) Space name is given explicitly
%% 2) Space name is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create_space(Auth :: aai:auth(), GroupId :: od_group:id(),
    NameOrData :: binary() | #{}) -> {ok, od_space:id()} | {error, term()}.
create_space(Auth, GroupId, Name) when is_binary(Name) ->
    create_space(Auth, GroupId, #{<<"name">> => Name});
create_space(Auth, GroupId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_space, id = undefined, aspect = instance},
        auth_hint = ?AS_GROUP(GroupId),
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new handle service for given group.
%% Allows to specify the Name, ProxyEndpoint and ServiceProperties.
%% @end
%%--------------------------------------------------------------------
-spec create_handle_service(Auth :: aai:auth(), GroupId :: od_group:id(),
    Name :: binary(), ProxyEndpoint :: od_handle_service:proxy_endpoint(),
    ServiceProperties :: od_handle_service:service_properties()) ->
    {ok, od_handle_service:id()} | {error, term()}.
create_handle_service(Auth, GroupId, Name, ProxyEndpoint, ServiceProperties) ->
    create_handle_service(Auth, GroupId, #{
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
-spec create_handle_service(Auth :: aai:auth(), GroupId :: od_group:id(),
    Data :: #{}) -> {ok, od_handle_service:id()} | {error, term()}.
create_handle_service(Auth, GroupId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_handle_service, id = undefined, aspect = instance},
        auth_hint = ?AS_GROUP(GroupId),
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new handle for given group.
%% Allows to specify the HServiceId, ResourceType, ResourceId and Metadata.
%% @end
%%--------------------------------------------------------------------
-spec create_handle(Auth :: aai:auth(), GroupId :: od_group:id(),
    HServiceId :: od_handle_service:id(), ResourceType :: od_handle:resource_type(),
    ResourceId :: od_handle:resource_id(), Metadata :: od_handle:metadata()) ->
    {ok, od_handle:id()} | {error, term()}.
create_handle(Auth, GroupId, HServiceId, ResourceType, ResourceId, Metadata) ->
    create_handle(Auth, GroupId, #{
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
-spec create_handle(Auth :: aai:auth(), GroupId :: od_group:id(),
    Data :: #{}) -> {ok, od_handle:id()} | {error, term()}.
create_handle(Auth, GroupId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_handle, id = undefined, aspect = instance},
        auth_hint = ?AS_GROUP(GroupId),
        data = Data
    })).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new harvester for given group. 
%% Harvester name, endpoint plugin and config are given explicitly.
%% @end
%%--------------------------------------------------------------------
-spec create_harvester(Auth :: aai:auth(), GroupId :: od_group:id(), Name :: binary(),
    Endpoint :: binary(), Plugin :: binary(), Config :: map()) -> {ok, od_harvester:id()} | {error, term()}.
create_harvester(Auth, GroupId, Name, Endpoint, Plugin, Config) ->
    create_harvester(Auth, GroupId, #{
        <<"name">> => Name,
        <<"endpoint">> => Endpoint,
        <<"plugin">> => Plugin,
        <<"guiPluginConfig">> => Config
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new harvester for given group. 
%% Harvester name, endpoint plugin and config are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create_harvester(Auth :: aai:auth(), GroupId :: od_group:id(),
    Data :: #{}) -> {ok, od_harvester:id()} | {error, term()}.
create_harvester(Auth, GroupId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = undefined, aspect = instance},
        auth_hint = ?AS_GROUP(GroupId),
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a user invite token, which can be used by any user to join
%% given group.
%% @end
%%--------------------------------------------------------------------
-spec create_user_invite_token(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
create_user_invite_token(Auth, GroupId) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = invite_user_token},
        data = #{}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a group invite token, which can be used by any child group to join
%% given group.
%% @end
%%--------------------------------------------------------------------
-spec create_group_invite_token(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
create_group_invite_token(Auth, GroupId) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = invite_group_token},
        data = #{}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new child group belonging to given group.
%% @end
%%--------------------------------------------------------------------
-spec create_child_group(Auth :: aai:auth(),
    ParentGroupId :: od_group:id(), Name :: binary(),
    Type :: od_group:type()) -> {ok, od_group:id()} | {error, term()}.
create_child_group(Auth, ParentGroupId, Name, Type) ->
    create_child_group(Auth, ParentGroupId, #{
        <<"name">> => Name, <<"type">> => Type
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new child group belonging to given group. Has two variants:
%% 1) Group Name is given explicitly (the new group will be of default type)
%% 2) Group name is provided in a proper Data object, group type is optional.
%% @end
%%--------------------------------------------------------------------
-spec create_child_group(Auth :: aai:auth(),
    ParentGroupId :: od_group:id(), NameOrData :: binary() | #{}) ->
    {ok, od_group:id()} | {error, term()}.
create_child_group(Auth, ParentGroupId, Name) when is_binary(Name) ->
    create_child_group(Auth, ParentGroupId, #{<<"name">> => Name});
create_child_group(Auth, ParentGroupId, Data) ->
    AuthHint = case Auth of
        ?USER(UserId) -> ?AS_USER(UserId);
        _ -> undefined
    end,
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_group, id = ParentGroupId, aspect = child},
        data = Data,
        auth_hint = AuthHint
    })).


%%--------------------------------------------------------------------
%% @doc
%% Joins a group on behalf of given user based on group_invite_user token.
%% Has two variants:
%% 1) Token is given explicitly (as binary() or macaroon())
%% 2) Token is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec join_group(Auth :: aai:auth(), GroupId :: od_group:id(),
    TokenOrData :: token:id() | macaroon:macaroon() | #{}) ->
    {ok, od_group:id()} | {error, term()}.
join_group(Auth, GroupId, Data) when is_map(Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_group, id = undefined, aspect = join},
        auth_hint = ?AS_GROUP(GroupId),
        data = Data
    }));
join_group(Auth, GroupId, Token) ->
    join_group(Auth, GroupId, #{<<"token">> => Token}).


%%--------------------------------------------------------------------
%% @doc
%% Joins a space on behalf of given user based on space_invite_user token.
%% Has two variants:
%% 1) Token is given explicitly (as binary() or macaroon())
%% 2) Token is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec join_space(Auth :: aai:auth(), GroupId :: od_group:id(),
    TokenOrData :: token:id() | macaroon:macaroon() | #{}) ->
    {ok, od_space:id()} | {error, term()}.
join_space(Auth, GroupId, Data) when is_map(Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_space, id = undefined, aspect = join},
        auth_hint = ?AS_GROUP(GroupId),
        data = Data
    }));
join_space(Auth, GroupId, Token) ->
    join_space(Auth, GroupId, #{<<"token">> => Token}).


%%--------------------------------------------------------------------
%% @doc
%% Joins a harvester on behalf of given group based on harvester_invite_group token.
%% Has two variants:
%% 1) Token is given explicitly (as binary() or macaroon())
%% 2) Token is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec join_harvester(Auth :: aai:auth(), GroupId :: od_group:id(),
    TokenOrData :: token:id() | macaroon:macaroon() | #{}) ->
    {ok, od_harvester:id()} | {error, term()}.
join_harvester(Auth, GroupId, Data) when is_map(Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = undefined, aspect = join},
        auth_hint = ?AS_GROUP(GroupId),
        data = Data
    }));
join_harvester(Auth, GroupId, Token) ->
    join_harvester(Auth, GroupId, #{<<"token">> => Token}).


%%--------------------------------------------------------------------
%% @doc
%% Joins a cluster on behalf of given group based on cluster_invite_group token.
%% Has two variants:
%% 1) Token is given explicitly (as binary() or macaroon())
%% 2) Token is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec join_cluster(Auth :: aai:auth(), GroupId :: od_group:id(),
    TokenOrData :: token:id() | macaroon:macaroon() | #{}) ->
    {ok, od_cluster:id()} | {error, term()}.
join_cluster(Auth, GroupId, Data) when is_map(Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_cluster, id = undefined, aspect = join},
        auth_hint = ?AS_GROUP(GroupId),
        data = Data
    }));
join_cluster(Auth, GroupId, Token) ->
    join_cluster(Auth, GroupId, #{<<"token">> => Token}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given group.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Auth :: aai:auth(),
    GroupId :: od_group:id(), UserId :: od_user:id()) ->
    {ok, od_user:id()} | {error, term()}.
add_user(Auth, GroupId, UserId) ->
    add_user(Auth, GroupId, UserId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given group.
%% Allows to specify the privileges of the newly added user. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Auth :: aai:auth(),
    GroupId :: od_group:id(), UserId :: od_user:id(),
    PrivilegesPrivilegesOrData :: [privileges:group_privileges()] | #{}) ->
    {ok, od_user:id()} | {error, term()}.
add_user(Auth, GroupId, UserId, Privileges) when is_list(Privileges) ->
    add_user(Auth, GroupId, UserId, #{
        <<"privileges">> => Privileges
    });
add_user(Auth, GroupId, UserId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {user, UserId}},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified child group to given group.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Auth :: aai:auth(),
    GroupId :: od_group:id(), ChildGroupId :: od_group:id()) ->
    {ok, od_group:id()} | {error, term()}.
add_group(Auth, GroupId, ChildGroupId) ->
    add_group(Auth, GroupId, ChildGroupId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified child group to given group.
%% Allows to specify the privileges of the newly added group. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Auth :: aai:auth(),
    GroupId :: od_group:id(), ChildGroupId :: od_group:id(),
    PrivilegesOrData :: [privileges:group_privileges()] | #{}) ->
    {ok, od_group:id()} | {error, term()}.
add_group(Auth, GroupId, ChildGroupId, Privileges) when is_list(Privileges) ->
    add_group(Auth, GroupId, ChildGroupId, #{
        <<"privileges">> => Privileges
    });
add_group(Auth, GroupId, ChildGroupId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {child, ChildGroupId}},
        data = Data
    })).


-spec get_name(aai:auth(), od_group:id()) ->
    {ok, od_group:name()} | {error, term()}.
get_name(Auth, GroupId) ->
    case get(Auth, GroupId) of
        {ok, #od_group{name = Name}} -> {ok, Name};
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of parents of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_parents(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_parents(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = parents}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective parents of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_parents(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_eff_parents(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = eff_parents}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific parent among parents of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_parent(Auth :: aai:auth(), GroupId :: od_group:id(),
    ParentGroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_parent(Auth, GroupId, ParentGroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = ParentGroupId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective parent among
%% effective parents of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_parent(Auth :: aai:auth(), GroupId :: od_group:id(),
    ParentGroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_eff_parent(Auth, GroupId, ParentGroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = ParentGroupId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of children of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_children(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_children(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = children}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective children of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_children(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_eff_children(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = eff_children}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific child among children of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_child(Auth :: aai:auth(), GroupId :: od_group:id(),
    ChildGroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_child(Auth, GroupId, ChildGroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = ChildGroupId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective child among
%% effective children of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_child(Auth :: aai:auth(), GroupId :: od_group:id(),
    ChildGroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_eff_child(Auth, GroupId, ChildGroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = ChildGroupId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific child among children of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_child_privileges(Auth :: aai:auth(), GroupId :: od_group:id(),
    ChildGroupId :: od_group:id()) -> {ok, [privileges:group_privileges()]} | {error, term()}.
get_child_privileges(Auth, GroupId, ChildGroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {child_privileges, ChildGroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective child
%% among effective children of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_child_privileges(Auth :: aai:auth(), GroupId :: od_group:id(),
    ChildGroupId :: od_group:id()) -> {ok, [privileges:group_privileges()]} | {error, term()}.
get_eff_child_privileges(Auth, GroupId, ChildGroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {eff_child_privileges, ChildGroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the membership intermediaries of specific effective child
%% among effective children of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_child_membership_intermediaries(Auth :: aai:auth(),
    GroupId :: od_group:id(), ChildGroupId :: od_group:id()) ->
    {ok, entity_graph:intermediaries()} | {error, term()}.
get_eff_child_membership_intermediaries(Auth, GroupId, ChildGroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {eff_child_membership, ChildGroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of users of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_users(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_users(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = users}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective users of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_users(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_eff_users(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = eff_users}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific user among users of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_user(Auth :: aai:auth(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_user(Auth, GroupId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective user among
%% effective users of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user(Auth :: aai:auth(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_eff_user(Auth, GroupId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific user among users of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(Auth :: aai:auth(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> {ok, [privileges:group_privileges()]} | {error, term()}.
get_user_privileges(Auth, GroupId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {user_privileges, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective user
%% among effective users of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user_privileges(Auth :: aai:auth(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> {ok, [privileges:group_privileges()]} | {error, term()}.
get_eff_user_privileges(Auth, GroupId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {eff_user_privileges, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the membership intermediaries of specific effective user
%% among effective users of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user_membership_intermediaries(Auth :: aai:auth(),
    GroupId :: od_group:id(), UserId :: od_user:id()) ->
    {ok, entity_graph:intermediaries()} | {error, term()}.
get_eff_user_membership_intermediaries(Auth, GroupId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {eff_user_membership, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of spaces of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_space:id()]} | {error, term()}.
get_spaces(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = spaces}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective spaces of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_spaces(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_space:id()]} | {error, term()}.
get_eff_spaces(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = eff_spaces}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific space among spaces of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_space(Auth :: aai:auth(), GroupId :: od_group:id(),
    SpaceId :: od_space:id()) -> {ok, #{}} | {error, term()}.
get_space(Auth, GroupId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective space among
%% effective spaces of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_space(Auth :: aai:auth(), GroupId :: od_group:id(),
    SpaceId :: od_space:id()) -> {ok, #{}} | {error, term()}.
get_eff_space(Auth, GroupId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective providers of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_providers(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_provider:id()]} | {error, term()}.
get_eff_providers(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = eff_providers}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective provider among
%% effective providers of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_provider(Auth :: aai:auth(), GroupId :: od_group:id(),
    ProviderId :: od_provider:id()) -> {ok, #{}} | {error, term()}.
get_eff_provider(Auth, GroupId, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_provider, id = ProviderId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of spaces supported by specific effective provider among
%% effective providers of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces_in_eff_provider(Auth :: aai:auth(), GroupId :: od_group:id(),
    ProviderId :: od_provider:id()) -> {ok, #{}} | {error, term()}.
get_spaces_in_eff_provider(Auth, GroupId, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_provider, id = ProviderId, aspect = {group_spaces, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of handle_services of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_handle_services(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_handle_service:id()]} | {error, term()}.
get_handle_services(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = handle_services}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective handle_services of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_handle_services(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_handle_service:id()]} | {error, term()}.
get_eff_handle_services(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = eff_handle_services}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific handle_service among
%% handle_services of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_handle_service(Auth :: aai:auth(), GroupId :: od_group:id(),
    HServiceId :: od_handle_service:id()) -> {ok, #{}} | {error, term()}.
get_handle_service(Auth, GroupId, HServiceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle_service, id = HServiceId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective handle_service among
%% effective handle_services of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_handle_service(Auth :: aai:auth(), GroupId :: od_group:id(),
    HServiceId :: od_handle_service:id()) -> {ok, #{}} | {error, term()}.
get_eff_handle_service(Auth, GroupId, HServiceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle_service, id = HServiceId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of handles of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_handles(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_handle:id()]} | {error, term()}.
get_handles(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = handles}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective handles of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_handles(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_handle:id()]} | {error, term()}.
get_eff_handles(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = eff_handles}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific handle among
%% handles of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_handle(Auth :: aai:auth(), GroupId :: od_group:id(),
    HandleId :: od_handle:id()) -> {ok, #{}} | {error, term()}.
get_handle(Auth, GroupId, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective handle among
%% effective handles of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_handle(Auth :: aai:auth(), GroupId :: od_group:id(),
    HandleId :: od_handle:id()) -> {ok, #{}} | {error, term()}.
get_eff_handle(Auth, GroupId, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of clusters of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_clusters(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_cluster:id()]} | {error, term()}.
get_clusters(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = clusters}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective clusters of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_clusters(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_cluster:id()]} | {error, term()}.
get_eff_clusters(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = eff_clusters}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific cluster among clusters of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_cluster(Auth :: aai:auth(), GroupId :: od_group:id(),
    ClusterId :: od_cluster:id()) -> {ok, #{}} | {error, term()}.
get_cluster(Auth, GroupId, ClusterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective cluster among
%% effective clusters of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_cluster(Auth :: aai:auth(), GroupId :: od_group:id(),
    ClusterId :: od_cluster:id()) -> {ok, #{}} | {error, term()}.
get_eff_cluster(Auth, GroupId, ClusterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of harvesters of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_harvesters(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_harvester:id()]} | {error, term()}.
get_harvesters(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = harvesters}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective harvesters of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_harvesters(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_harvester:id()]} | {error, term()}.
get_eff_harvesters(Auth, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = eff_harvesters}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific harvester among harvesters of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_harvester(Auth :: aai:auth(), GroupId :: od_group:id(),
    HarvesterId :: od_harvester:id()) -> {ok, #{}} | {error, term()}.
get_harvester(Auth, GroupId, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective harvester among
%% effective harvesters of given group.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_harvester(Auth :: aai:auth(), GroupId :: od_group:id(),
    HarvesterId :: od_harvester:id()) -> {ok, #{}} | {error, term()}.
get_eff_harvester(Auth, GroupId, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_GROUP(GroupId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given group.
%% Allows to specify privileges to grant and to revoke.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Auth :: aai:auth(), GroupId :: od_group:id(),
    UserId :: od_user:id(), PrivsToGrant :: [privileges:group_privilege()],
    PrivsToRevoke :: [privileges:group_privilege()]) -> ok | {error, term()}.
update_user_privileges(Auth, GroupId, UserId, PrivsToGrant, PrivsToRevoke) ->
    update_user_privileges(Auth, GroupId, UserId, #{
        <<"grant">> => PrivsToGrant,
        <<"revoke">> => PrivsToRevoke
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given group.
%% Privileges to grant and revoke must be included in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Auth :: aai:auth(), GroupId :: od_group:id(),
    UserId :: od_user:id(), Data :: #{}) -> ok | {error, term()}.
update_user_privileges(Auth, GroupId, UserId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {user_privileges, UserId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified child of given group.
%% Allows to specify privileges to grant and to revoke.
%% @end
%%--------------------------------------------------------------------
-spec update_child_privileges(Auth :: aai:auth(), GroupId :: od_group:id(),
    ChildGroupId :: od_group:id(), PrivsToGrant :: [privileges:group_privilege()],
    PrivsToRevoke :: [privileges:group_privilege()]) -> ok | {error, term()}.
update_child_privileges(Auth, GroupId, ChildGroupId, PrivsToGrant, PrivsToRevoke) ->
    update_child_privileges(Auth, GroupId, ChildGroupId, #{
        <<"grant">> => PrivsToGrant,
        <<"revoke">> => PrivsToRevoke
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified child of given group.
%% Privileges to grant and revoke must be included in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_child_privileges(Auth :: aai:auth(), GroupId :: od_group:id(),
    ChildGroupId :: od_user:id(), Data :: #{}) -> ok | {error, term()}.
update_child_privileges(Auth, GroupId, ChildGroupId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {child_privileges, ChildGroupId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified parent group on behalf of given group.
%% @end
%%--------------------------------------------------------------------
-spec leave_group(Auth :: aai:auth(), GroupId :: od_group:id(),
    ParentGroupId :: od_group:id()) -> ok | {error, term()}.
leave_group(Auth, GroupId, ParentGroupId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {parent, ParentGroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified space on behalf of given group.
%% @end
%%--------------------------------------------------------------------
-spec leave_space(Auth :: aai:auth(), GroupId :: od_user:id(),
    SpaceId :: od_space:id()) -> ok | {error, term()}.
leave_space(Auth, GroupId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {space, SpaceId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified od_handle_service on behalf of given group.
%% @end
%%--------------------------------------------------------------------
-spec leave_handle_service(Auth :: aai:auth(), GroupId :: od_group:id(),
    HServiceId :: od_handle_service:id()) -> ok | {error, term()}.
leave_handle_service(Auth, GroupId, HServiceId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {handle_service, HServiceId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified handle on behalf of given group.
%% @end
%%--------------------------------------------------------------------
-spec leave_handle(Auth :: aai:auth(), GroupId :: od_group:id(),
    HandleId :: od_handle:id()) -> ok | {error, term()}.
leave_handle(Auth, GroupId, HandleId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {handle, HandleId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified harvester on behalf of given group.
%% @end
%%--------------------------------------------------------------------
-spec leave_harvester(Auth :: aai:auth(), GroupId :: od_user:id(),
    HarvesterId :: od_harvester:id()) -> ok | {error, term()}.
leave_harvester(Auth, GroupId, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {harvester, HarvesterId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified cluster on behalf of given group.
%% @end
%%--------------------------------------------------------------------
-spec leave_cluster(Auth :: aai:auth(), GroupId :: od_user:id(),
    ClusterId :: od_cluster:id()) -> ok | {error, term()}.
leave_cluster(Auth, GroupId, ClusterId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {cluster, ClusterId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified user from given group.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(Auth :: aai:auth(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> ok | {error, term()}.
remove_user(Auth, GroupId, UserId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {user, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified child group from given group.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(Auth :: aai:auth(), GroupId :: od_group:id(),
    ChildGroupId :: od_group:id()) -> ok | {error, term()}.
remove_group(Auth, GroupId, ChildGroupId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = {child, ChildGroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a group exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(GroupId :: od_group:id()) -> boolean().
exists(GroupId) ->
    {ok, Exists} = od_group:exists(GroupId),
    Exists.


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified group is an effective parent of given group.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_parent(GroupOrId :: od_group:id() | #od_group{},
    ParentId :: od_group:id()) -> boolean().
has_eff_parent(GroupId, ParentId) when is_binary(GroupId) ->
    entity_graph:has_relation(effective, top_down, od_group, ParentId, od_group, GroupId);
has_eff_parent(Group, ParentId) ->
    entity_graph:has_relation(effective, top_down, od_group, ParentId, Group).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified group is an effective child of given group.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_child(GroupOrId :: od_group:id() | #od_group{},
    ChildId :: od_group:id()) -> boolean().
has_eff_child(GroupId, ChildId) when is_binary(GroupId) ->
    entity_graph:has_relation(effective, bottom_up, od_group, ChildId, od_group, GroupId);
has_eff_child(Group, ChildId) ->
    entity_graph:has_relation(effective, bottom_up, od_group, ChildId, Group).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified user is a direct member of given group.
%% @end
%%--------------------------------------------------------------------
-spec has_direct_user(GroupOrId :: od_group:id() | #od_group{},
    UserId :: od_group:id()) -> boolean().
has_direct_user(GroupId, UserId) when is_binary(GroupId) ->
    case od_group:get(GroupId) of
        {ok, #document{value = Group}} ->
            has_direct_user(Group, UserId);
        _ ->
            false
    end;
has_direct_user(#od_group{users = Users}, UserId) ->
    maps:is_key(UserId, Users).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified user is an effective user of given group.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_user(GroupOrId :: od_group:id() | #od_group{},
    UserId :: od_user:id()) -> boolean().
has_eff_user(GroupId, UserId) when is_binary(GroupId) ->
    case od_group:get(GroupId) of
        {ok, #document{value = Group}} ->
            has_eff_user(Group, UserId);
        _ ->
            false
    end;
has_eff_user(#od_group{eff_users = EffUsers}, UserId) ->
    maps:is_key(UserId, EffUsers).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified group is an effective group in given space.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_space(GroupOrId :: od_group:id() | #od_group{},
    SpaceId :: od_space:id()) -> boolean().
has_eff_space(GroupId, SpaceId) when is_binary(GroupId) ->
    entity_graph:has_relation(effective, top_down, od_space, SpaceId, od_group, GroupId);
has_eff_space(Group, SpaceId) ->
    entity_graph:has_relation(effective, top_down, od_space, SpaceId, Group).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified group is an effective group in given
%% provider.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_provider(GroupOrId :: od_group:id() | #od_group{},
    ProviderId :: od_provider:id()) -> boolean().
has_eff_provider(GroupId, ProviderId) when is_binary(GroupId) ->
    entity_graph:has_relation(effective, top_down, od_provider, ProviderId, od_group, GroupId);
has_eff_provider(Group, ProviderId) ->
    entity_graph:has_relation(effective, top_down, od_provider, ProviderId, Group).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified group is an effective group in given
%% handle_service.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_handle_service(GroupOrId :: od_group:id() | #od_group{},
    HServiceId :: od_handle_service:id()) -> boolean().
has_eff_handle_service(GroupId, HServiceId) when is_binary(GroupId) ->
    entity_graph:has_relation(effective, top_down, od_handle_service, HServiceId, od_group, GroupId);
has_eff_handle_service(Group, HServiceId) ->
    entity_graph:has_relation(effective, top_down, od_handle_service, HServiceId, Group).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified group is an effective group in given
%% handle.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_handle(GroupOrId :: od_group:id() | #od_group{},
    HandleId :: od_handle:id()) -> boolean().
has_eff_handle(GroupId, HandleId) when is_binary(GroupId) ->
    entity_graph:has_relation(effective, top_down, od_handle, HandleId, od_group, GroupId);
has_eff_handle(Group, HandleId) ->
    entity_graph:has_relation(effective, top_down, od_handle, HandleId, Group).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified group is an effective group in given
%% harvester.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_harvester(GroupOrId :: od_group:id() | #od_group{},
    HarvesterId :: od_harvester:id()) -> boolean().
has_eff_harvester(GroupId, HarvesterId) when is_binary(GroupId) ->
    entity_graph:has_relation(effective, top_down, od_harvester, HarvesterId, od_group, GroupId);
has_eff_harvester(Group, HarvesterId) ->
    entity_graph:has_relation(effective, top_down, od_harvester, HarvesterId, Group).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified group is an effective group in given cluster.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_cluster(GroupOrId :: od_group:id() | #od_group{},
    ClusterId :: od_cluster:id()) -> boolean().
has_eff_cluster(GroupId, ClusterId) when is_binary(GroupId) ->
    entity_graph:has_relation(effective, top_down, od_cluster, ClusterId, od_group, GroupId);
has_eff_cluster(Group, ClusterId) ->
    entity_graph:has_relation(effective, top_down, od_cluster, ClusterId, Group).


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
    entity_graph:has_privilege(effective, bottom_up, od_user, UserId, Privilege, od_group, GroupId);
has_eff_privilege(Group, UserId, Privilege) ->
    entity_graph:has_privilege(effective, bottom_up, od_user, UserId, Privilege, Group).


%%--------------------------------------------------------------------
%% @doc
%% Creates predefined groups in the system based on settings in app.config.
%% @end
%%--------------------------------------------------------------------
-spec create_predefined_groups() -> ok.
create_predefined_groups() ->
    PredefinedGroups = oz_worker:get_env(predefined_groups),
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
            ok = create_predefined_group(Id, Name, Privs)
        end, PredefinedGroups).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a predefined group in the system, if it does not exist, and grants
%% given OZ privileges to it.
%% @end
%%--------------------------------------------------------------------
-spec create_predefined_group(Id :: binary(), Name :: binary(),
    Privileges :: [privileges:oz_privilege()]) -> ok | error.
create_predefined_group(GroupId, Name, Privileges) ->
    NormalizedName = entity_logic:normalize_name(Name),
    Result = case od_group:exists(GroupId) of
        {ok, true} ->
            ?info("Predefined group '~ts' already exists, refreshing name and privileges.", [
                NormalizedName
            ]),
            ok;
        {ok, false} ->
            NewGroup = #document{
                key = GroupId,
                value = #od_group{
                    name = NormalizedName,
                    type = role_holders,
                    protected = true,
                    creator = ?SUB(root)
                }},
            case od_group:create(NewGroup) of
                {ok, _} ->
                    ?info("Created predefined group '~ts'", [NormalizedName]),
                    ok;
                Other ->
                    ?error("Cannot create predefined group '~ts' - ~p", [
                        NormalizedName, Other
                    ]),
                    error
            end
    end,
    case Result of
        ok ->
            ok = update(?ROOT, GroupId, #{<<"name">> => NormalizedName}),
            ToRevoke = privileges:oz_admin() -- Privileges,
            ok = update_oz_privileges(?ROOT, GroupId, Privileges, ToRevoke);
        error ->
            error
    end.
