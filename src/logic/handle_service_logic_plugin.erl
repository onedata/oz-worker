%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_handle_service model.
%%% @end
%%%-------------------------------------------------------------------
-module(handle_service_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("entity_logic.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").

-type resource() :: {deprecated_user_privileges, od_user:id()} | % TODO VFS-2918
{deprecated_group_privileges, od_group:id()} | % TODO VFS-2918
entity | data | public_data | list |
users | eff_users | {user, od_user:id()} | {eff_user, od_user:id()} |
{user_privileges, od_user:id()} | {eff_user_privileges, od_user:id()} |
groups | eff_groups | {group, od_group:id()} | {eff_group, od_group:id()} |
{group_privileges, od_user:id()} | {eff_group_privileges, od_user:id()} |
handles | {handle, od_handle:id()}.


-export_type([resource/0]).

-export([get_entity/1, create/4, get/4, update/3, delete/2]).
-export([exists/1, authorize/4, validate/2]).
-export([entity_to_string/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity from datastore based on its EntityId.
%% Should return ?ERROR_NOT_FOUND if the entity does not exist.
%% @end
%%--------------------------------------------------------------------
-spec get_entity(EntityId :: entity_logic:entity_id()) ->
    {ok, entity_logic:entity()} | {error, Reason :: term()}.
get_entity(HServiceId) ->
    case od_handle_service:get(HServiceId) of
        {ok, #document{value = HandleService}} ->
            {ok, HandleService};
        _ ->
            ?ERROR_NOT_FOUND
    end.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource based on EntityId, Resource identifier and Data.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: entity_logic:client(),
    EntityId :: entity_logic:entity_id(), Resource :: resource(),
    entity_logic:data()) -> entity_logic:result().
% TODO VFS-2918
create(_Client, HServiceId, {deprecated_user_privileges, UserId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_user, UserId,
        od_handle_service, HServiceId,
        {Operation, Privileges}
    );
% TODO VFS-2918
create(_Client, HServiceId, {deprecated_group_privileges, GroupId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_group, GroupId,
        od_handle_service, HServiceId,
        {Operation, Privileges}
    );

create(Client, _, entity, Data) ->
    Name = maps:get(<<"name">>, Data),
    ProxyEndpoint = maps:get(<<"proxyEndpoint">>, Data),
    ServiceProperties = maps:get(<<"serviceProperties">>, Data),
    HandleService = #document{value = #od_handle_service{
        name = Name,
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties
    }},
    {ok, HServiceId} = od_handle_service:create(HandleService),
    case Client of
        ?USER(UserId) ->
            entity_graph:add_relation(
                od_user, UserId,
                od_handle_service, HServiceId,
                privileges:handle_service_admin()
            );
        _ ->
            ok
    end,
    {ok, HServiceId};
create(?USER, HServiceId, {user, UserId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:handle_service_user()),
    entity_graph:add_relation(
        od_user, UserId,
        od_handle_service, HServiceId,
        Privileges
    ),
    {ok, UserId};
create(?USER, HServiceId, {group, GroupId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:handle_service_user()),
    entity_graph:add_relation(
        od_group, GroupId,
        od_handle_service, HServiceId,
        Privileges
    ),
    {ok, GroupId}.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource based on EntityId and Resource identifier.
%% @end
%%--------------------------------------------------------------------
-spec get(Client :: entity_logic:client(), EntityId :: entity_logic:entity_id(),
    Entity :: entity_logic:entity(), Resource :: resource()) ->
    entity_logic:result().
get(_, _HServiceId, #od_handle_service{} = HService, data) ->
    #od_handle_service{
        name = Name, proxy_endpoint = Proxy, service_properties = ServiceProps
    } = HService,
    {ok, #{
        <<"name">> => Name,
        <<"proxyEndpoint">> => Proxy,
        <<"serviceProperties">> => ServiceProps
    }};

get(_, undefined, undefined, list) ->
    {ok, HServiceDocs} = od_handle_service:list(),
    {ok, [HServiceId || #document{key = HServiceId} <- HServiceDocs]};

get(_, _HServiceId, #od_handle_service{users = Users}, users) ->
    {ok, maps:keys(Users)};
get(_, _HServiceId, #od_handle_service{eff_users = Users}, eff_users) ->
    {ok, maps:keys(Users)};
get(_, _HServiceId, #od_handle_service{}, {user, UserId}) ->
    {ok, User} = ?throw_on_failure(user_logic_plugin:get_entity(UserId)),
    user_logic_plugin:get(?ROOT, UserId, User, data);
get(_, _HServiceId, #od_handle_service{}, {eff_user, UserId}) ->
    {ok, User} = ?throw_on_failure(user_logic_plugin:get_entity(UserId)),
    user_logic_plugin:get(?ROOT, UserId, User, data);
get(_, _HServiceId, #od_handle_service{users = Users}, {user_privileges, UserId}) ->
    {ok, maps:get(UserId, Users)};
get(_, _HServiceId, #od_handle_service{eff_users = Users}, {eff_user_privileges, UserId}) ->
    {Privileges, _} = maps:get(UserId, Users),
    {ok, Privileges};

get(_, _HServiceId, #od_handle_service{groups = Groups}, groups) ->
    {ok, maps:keys(Groups)};
get(_, _HServiceId, #od_handle_service{eff_groups = Groups}, eff_groups) ->
    {ok, maps:keys(Groups)};
get(_, _HServiceId, #od_handle_service{}, {group, GroupId}) ->
    {ok, Group} = ?throw_on_failure(group_logic_plugin:get_entity(GroupId)),
    group_logic_plugin:get(?ROOT, GroupId, Group, data);
get(_, _HServiceId, #od_handle_service{}, {eff_group, GroupId}) ->
    {ok, Group} = ?throw_on_failure(group_logic_plugin:get_entity(GroupId)),
    group_logic_plugin:get(?ROOT, GroupId, Group, data);
get(_, _HServiceId, #od_handle_service{groups = Groups}, {group_privileges, GroupId}) ->
    {ok, maps:get(GroupId, Groups)};
get(_, _HServiceId, #od_handle_service{eff_groups = Groups}, {eff_group_privileges, GroupId}) ->
    {Privileges, _} = maps:get(GroupId, Groups),
    {ok, Privileges};

get(_, _HServiceId, #od_handle_service{handles = Handles}, handles) ->
    {ok, Handles};
get(_, _HServiceId, #od_handle_service{}, {handle, HandleId}) ->
    {ok, Handle} = ?throw_on_failure(handle_logic_plugin:get_entity(HandleId)),
    handle_logic_plugin:get(?ROOT, HandleId, Handle, data).


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource based on EntityId, Resource identifier and Data.
%% @end
%%--------------------------------------------------------------------
-spec update(EntityId :: entity_logic:entity_id(), Resource :: resource(),
    entity_logic:data()) -> entity_logic:result().
update(HServiceId, entity, Data) ->
    {ok, _} = od_handle_service:update(HServiceId, fun(HService) ->
        #od_handle_service{
            name = OldName, proxy_endpoint = OldProxy,
            service_properties = OldServiceProps
        } = HService,
        NewName = maps:get(<<"name">>, Data, OldName),
        NewProxy = maps:get(<<"proxyEndpoint">>, Data, OldProxy),
        NewServiceProps = maps:get(<<"serviceProperties">>, Data, OldServiceProps),
        {ok, HService#od_handle_service{
            name = NewName, proxy_endpoint = NewProxy,
            service_properties = NewServiceProps
        }}
    end),
    ok;

update(HServiceId, {user_privileges, UserId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_user, UserId,
        od_handle_service, HServiceId,
        {Operation, Privileges}
    );

update(HServiceId, {group_privileges, GroupId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_group, GroupId,
        od_handle_service, HServiceId,
        {Operation, Privileges}
    ).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource based on EntityId and Resource identifier.
%% @end
%%--------------------------------------------------------------------
-spec delete(EntityId :: entity_logic:entity_id(), Resource :: resource()) ->
    entity_logic:result().
delete(HServiceId, entity) ->
    entity_graph:delete_with_relations(od_handle_service, HServiceId);

delete(HServiceId, {user, UserId}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_handle_service, HServiceId
    );

delete(HServiceId, {group, GroupId}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_handle_service, HServiceId
    ).


%%--------------------------------------------------------------------
%% @doc
%% Returns existence verificators for given Resource identifier.
%% Existence verificators can be internal, which means they operate on the
%% entity to which the resource corresponds, or external - independent of
%% the entity. If there are multiple verificators, they will be checked in
%% sequence until one of them returns true.
%% Implicit verificators 'true' | 'false' immediately stop the verification
%% process with given result.
%% @end
%%--------------------------------------------------------------------
-spec exists(Resource :: resource()) ->
    entity_logic:existence_verificator()|
    [entity_logic:existence_verificator()].
exists({user, UserId}) ->
    {internal, fun(#od_handle_service{users = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists({eff_user, UserId}) ->
    {internal, fun(#od_handle_service{eff_users = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists({user_privileges, UserId}) ->
    {internal, fun(#od_handle_service{users = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists({eff_user_privileges, UserId}) ->
    {internal, fun(#od_handle_service{eff_users = Users}) ->
        maps:is_key(UserId, Users)
    end};

exists({group, UserId}) ->
    {internal, fun(#od_handle_service{groups = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists({eff_group, UserId}) ->
    {internal, fun(#od_handle_service{eff_groups = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists({group_privileges, UserId}) ->
    {internal, fun(#od_handle_service{groups = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists({eff_group_privileges, UserId}) ->
    {internal, fun(#od_handle_service{eff_groups = Users}) ->
        maps:is_key(UserId, Users)
    end};

exists({handle, HandleId}) ->
    {internal, fun(#od_handle_service{handles = Handles}) ->
        lists:member(HandleId, Handles)
    end};

exists(_) ->
    % No matter the resource, return true if it belongs to a handle
    {internal, fun(#od_handle_service{}) ->
        % If the handle with HandleId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


%%--------------------------------------------------------------------
%% @doc
%% Returns existence verificators for given Resource identifier.
%% Existence verificators can be internal, which means they operate on the
%% entity to which the resource corresponds, or external - independent of
%% the entity. If there are multiple verificators, they will be checked in
%% sequence until one of them returns true.
%% Implicit verificators 'true' | 'false' immediately stop the verification
%% process with given result.
%% @end
%%--------------------------------------------------------------------
-spec authorize(Operation :: entity_logic:operation(),
    EntityId :: entity_logic:entity_id(), Resource :: resource(),
    Client :: entity_logic:client()) ->
    entity_logic:authorization_verificator() |
    [authorization_verificator:existence_verificator()].
% TODO VFS-2918
authorize(create, _HServiceId, {deprecated_user_privileges, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_SERVICE_UPDATE);
% TODO VFS-2918
authorize(create, _HServiceId, {deprecated_group_privileges, _GroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_SERVICE_UPDATE);

authorize(create, undefined, entity, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_HANDLE_SERVICES_CREATE);

authorize(create, _HServiceId, {user, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_SERVICE_UPDATE);

authorize(create, _HServiceId, {group, _GroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_SERVICE_UPDATE);


authorize(get, _HServiceId, entity, ?USER(UserId)) -> [
    auth_by_privilege(UserId, ?HANDLE_SERVICE_VIEW),
    auth_by_oz_privilege(UserId, ?OZ_HANDLE_SERVICES_LIST)
];

authorize(get, _HServiceId, data, ?USER(UserId)) -> [
    auth_by_privilege(UserId, ?HANDLE_SERVICE_VIEW),
    auth_by_oz_privilege(UserId, ?OZ_HANDLE_SERVICES_LIST)
];

authorize(get, undefined, list, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_HANDLE_SERVICES_LIST);

authorize(get, _HServiceId, _, ?USER(UserId)) ->
    % All other resources can be accessed with view privileges
    auth_by_privilege(UserId, ?HANDLE_SERVICE_VIEW);


authorize(update, _HandleId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_SERVICE_UPDATE);

authorize(update, _HandleId, {user_privileges, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_SERVICE_UPDATE);

authorize(update, _HandleId, {group_privileges, _GroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_SERVICE_UPDATE);


authorize(delete, _HandleId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_SERVICE_DELETE);

authorize(delete, _HandleId, {user, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_SERVICE_UPDATE);

authorize(delete, _HandleId, {group, _GroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_SERVICE_UPDATE);

authorize(_, _, _, _) ->
    false.


%%--------------------------------------------------------------------
%% @doc
%% Returns validity verificators for given Operation and Resource identifier.
%% Returns a map with 'required', 'optional' and 'at_least_one' keys.
%% Under each of them, there is a map:
%%      Key => {type_verificator, value_verificator}
%% Which means how value of given Key should be validated.
%% @end
%%--------------------------------------------------------------------
-spec validate(Operation :: entity_logic:operation(),
    Resource :: resource()) ->
    entity_logic:validity_verificator().
% TODO VFS-2918
validate(create, {deprecated_user_privileges, UserId}) ->
    validate(update, {user_privileges, UserId});
% TODO VFS-2918
validate(create, {deprecated_group_privileges, GroupId}) ->
    validate(update, {user_privileges, GroupId});
validate(create, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty},
        <<"proxyEndpoint">> => {binary, non_empty},
        <<"serviceProperties">> => {json, non_empty}
    }
};
validate(create, {user, _UserId}) -> #{
    required => #{
        resource => {any, {resource_exists, <<"User Id">>, fun({user, UserId}) ->
            user_logic:exists(UserId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:handle_service_privileges()}
    }
};
validate(create, {group, _GroupId}) -> #{
    required => #{
        resource => {any, {resource_exists, <<"Group Id">>, fun({group, GroupId}) ->
            group_logic:exists(GroupId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:handle_service_privileges()}
    }
};
validate(update, entity) -> #{
    at_least_one => #{
        <<"name">> => {binary, non_empty},
        <<"proxyEndpoint">> => {binary, non_empty},
        <<"serviceProperties">> => {json, non_empty}
    }
};
validate(update, {user_privileges, _UserId}) -> #{
    required => #{
        <<"privileges">> => {list_of_atoms, privileges:handle_service_privileges()}
    },
    optional => #{
        <<"operation">> => {atom, [set, grant, revoke]}
    }
};
validate(update, {group_privileges, GroupId}) ->
    validate(update, {user_privileges, GroupId}).


%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the entity with given id.
%% @end
%%--------------------------------------------------------------------
-spec entity_to_string(EntityId :: entity_logic:entity_id()) -> binary().
entity_to_string(HServiceId) ->
    od_handle_service:to_string(HServiceId).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns authorization verificator that checks if given user has specific
%% effective privilege in the handle service represented by entity.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_privilege(UserId :: od_user:id(),
    Privilege :: privileges:handle_service_privilege()) ->
    entity_logic:authorization_verificator().
auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_handle_service{} = HService) ->
        handle_service_logic:user_has_eff_privilege(HService, UserId, Privilege)
    end}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns authorization verificator that checks if given user has specified
%% effective oz privilege.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_oz_privilege(UserId :: od_user:id(),
    Privilege :: privileges:oz_privilege()) ->
    entity_logic:authorization_verificator().
auth_by_oz_privilege(UserId, Privilege) ->
    {external, fun() ->
        user_logic:has_eff_oz_privilege(UserId, Privilege)
    end}.