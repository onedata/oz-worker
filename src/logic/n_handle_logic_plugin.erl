%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_handle model.
%%% @end
%%%-------------------------------------------------------------------
-module(n_handle_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("errors.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").

-type resource() :: {deprecated_user_privileges, od_user:id()} | % TODO VFS-2918
{deprecated_group_privileges, od_group:id()} | % TODO VFS-2918
entity | data | public_data | list |
users | eff_users | {user, od_user:id()} | {eff_user, od_user:id()} |
{user_privileges, od_user:id()} | {eff_user_privileges, od_user:id()} |
groups | eff_groups | {group, od_group:id()} | {eff_group, od_group:id()} |
{group_privileges, od_user:id()} | {eff_group_privileges, od_user:id()}.

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
-spec get_entity(EntityId :: n_entity_logic:entity_id()) ->
    {ok, n_entity_logic:entity()} | {error, Reason :: term()}.
get_entity(HandleId) ->
    case od_handle:get(HandleId) of
        {ok, #document{value = Handle}} ->
            {ok, Handle};
        _ ->
            ?ERROR_NOT_FOUND
    end.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource based on EntityId, Resource identifier and Data.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: n_entity_logic:client(),
    EntityId :: n_entity_logic:entity_id(), Resource :: resource(),
    n_entity_logic:data()) -> n_entity_logic:result().
% TODO VFS-2918
create(_Client, HandleId, {deprecated_user_privileges, UserId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_user, UserId,
        od_handle, HandleId,
        {Operation, Privileges}
    );
% TODO VFS-2918
create(_Client, HandleId, {deprecated_group_privileges, GroupId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_group, GroupId,
        od_handle, HandleId,
        {Operation, Privileges}
    );


create(Client, _, entity, Data) ->
    HandleServiceId = maps:get(<<"handleServiceId">>, Data),
    ResourceType = maps:get(<<"resourceType">>, Data),
    ResourceId = maps:get(<<"resourceId">>, Data),
    Metadata = maps:get(<<"metadata">>, Data),
    {ok, PublicHandle} = handle_proxy:register_handle(
        HandleServiceId, ResourceType, ResourceId, Metadata
    ),
    Handle = #document{value = #od_handle{
        handle_service = HandleServiceId,
        resource_type = ResourceType,
        resource_id = ResourceId,
        public_handle = PublicHandle,
        metadata = Metadata
    }},
    {ok, HandleId} = od_handle:create(Handle),
    entity_graph:add_relation(
        od_handle, HandleId,
        od_handle_service, HandleServiceId,
        privileges:handle_admin()
    ),
    case Client of
        ?USER(UserId) ->
            entity_graph:add_relation(
                od_user, UserId,
                od_handle, HandleId,
                privileges:handle_admin()
            );
        _ ->
            ok
    end,
    case ResourceType of
        <<"Share">> ->
            entity_graph:add_relation(
                od_handle, HandleId,
                od_share, ResourceId
            );
        _ ->
            ok
    end,
    {ok, HandleId};
create(?USER, HandleId, {user, UserId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:handle_user()),
    entity_graph:add_relation(
        od_user, UserId,
        od_handle, HandleId,
        Privileges
    ),
    {ok, HandleId};
create(?USER, HandleId, {group, GroupId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:handle_user()),
    entity_graph:add_relation(
        od_group, GroupId,
        od_handle, HandleId,
        Privileges
    ),
    {ok, HandleId}.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource based on EntityId and Resource identifier.
%% @end
%%--------------------------------------------------------------------
-spec get(Client :: n_entity_logic:client(), EntityId :: n_entity_logic:entity_id(),
    Entity :: n_entity_logic:entity(), Resource :: resource()) ->
    n_entity_logic:result().
get(_, undefined, undefined, list) ->
    {ok, HandleDocs} = od_handle:list(),
    {ok, [HandleId || #document{key = HandleId} <- HandleDocs]};

get(_, _HandleId, #od_handle{} = Handle, data) ->
    #od_handle{handle_service = HandleService, public_handle = Handle,
        resource_type = ResourceType, resource_id = ResourceId,
        metadata = Metadata, timestamp = Timestamp
    } = Handle,
    {ok, #{
        <<"handleServiceId">> => HandleService,
        <<"handle">> => Handle,
        <<"resourceType">> => ResourceType,
        <<"resourceId">> => ResourceId,
        <<"metadata">> => Metadata,
        <<"timestamp">> => Timestamp
    }};

get(_, _HandleId, #od_handle{} = Handle, public_data) ->
    #od_handle{
        public_handle = Handle, metadata = Metadata, timestamp = Timestamp
    } = Handle,
    {ok, #{
        <<"handle">> => Handle,
        <<"metadata">> => Metadata,
        <<"timestamp">> => Timestamp
    }};

get(_, _HandleId, #od_handle{users = Users}, users) ->
    {ok, maps:keys(Users)};
get(_, _HandleId, #od_handle{eff_users = Users}, eff_users) ->
    {ok, maps:keys(Users)};
get(_, _HandleId, #od_handle{}, {user, UserId}) ->
    {ok, User} = ?throw_on_failure(n_user_logic_plugin:get_entity(UserId)),
    n_user_logic_plugin:get(?ROOT, UserId, User, data);
get(_, _HandleId, #od_handle{}, {eff_user, UserId}) ->
    {ok, User} = ?throw_on_failure(n_user_logic_plugin:get_entity(UserId)),
    n_user_logic_plugin:get(?ROOT, UserId, User, data);
get(_, _HandleId, #od_handle{users = Users}, {user_privileges, UserId}) ->
    {ok, maps:get(UserId, Users)};
get(_, _HandleId, #od_handle{eff_users = Users}, {eff_user_privileges, UserId}) ->
    {Privileges, _} = maps:get(UserId, Users),
    {ok, Privileges};

get(_, _HandleId, #od_handle{groups = Groups}, groups) ->
    {ok, maps:keys(Groups)};
get(_, _HandleId, #od_handle{eff_groups = Groups}, eff_groups) ->
    {ok, maps:keys(Groups)};
get(_, _HandleId, #od_handle{}, {group, GroupId}) ->
    {ok, Group} = ?throw_on_failure(n_group_logic_plugin:get_entity(GroupId)),
    n_group_logic_plugin:get(?ROOT, GroupId, Group, data);
get(_, _HandleId, #od_handle{}, {eff_group, GroupId}) ->
    {ok, Group} = ?throw_on_failure(n_group_logic_plugin:get_entity(GroupId)),
    n_group_logic_plugin:get(?ROOT, GroupId, Group, data);
get(_, _HandleId, #od_handle{groups = Groups}, {group_privileges, GroupId}) ->
    {ok, maps:get(GroupId, Groups)};
get(_, _HandleId, #od_handle{eff_groups = Groups}, {eff_group_privileges, GroupId}) ->
    {Privileges, _} = maps:get(GroupId, Groups),
    {ok, Privileges}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource based on EntityId, Resource identifier and Data.
%% @end
%%--------------------------------------------------------------------
-spec update(EntityId :: n_entity_logic:entity_id(), Resource :: resource(),
    n_entity_logic:data()) -> n_entity_logic:result().
update(HandleId, entity, #{<<"metadata">> := NewMetadata}) ->
    {ok, _} = od_handle:update(HandleId, #{metadata => NewMetadata}),
    ok;

update(HandleId, {user_privileges, UserId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_user, UserId,
        od_handle, HandleId,
        {Operation, Privileges}
    );

update(HandleId, {group_privileges, GroupId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_group, GroupId,
        od_handle, HandleId,
        {Operation, Privileges}
    ).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource based on EntityId and Resource identifier.
%% @end
%%--------------------------------------------------------------------
-spec delete(EntityId :: n_entity_logic:entity_id(), Resource :: resource()) ->
    n_entity_logic:result().
delete(HandleId, entity) ->
    entity_graph:delete_with_relations(od_handle, HandleId);

delete(HandleId, {user, UserId}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_handle, HandleId
    );

delete(HandleId, {group, GroupId}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_handle, HandleId
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
    n_entity_logic:existence_verificator()|
    [n_entity_logic:existence_verificator()].
exists({user, UserId}) ->
    {internal, fun(#od_handle{users = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists({eff_user, UserId}) ->
    {internal, fun(#od_handle{eff_users = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists({user_privileges, UserId}) ->
    {internal, fun(#od_handle{users = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists({eff_user_privileges, UserId}) ->
    {internal, fun(#od_handle{eff_users = Users}) ->
        maps:is_key(UserId, Users)
    end};

exists({group, UserId}) ->
    {internal, fun(#od_handle{groups = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists({eff_group, UserId}) ->
    {internal, fun(#od_handle{eff_groups = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists({group_privileges, UserId}) ->
    {internal, fun(#od_handle{groups = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists({eff_group_privileges, UserId}) ->
    {internal, fun(#od_handle{eff_groups = Users}) ->
        maps:is_key(UserId, Users)
    end};

exists(_) ->
    % No matter the resource, return true if it belongs to a handle
    {internal, fun(#od_handle{}) ->
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
-spec authorize(Operation :: n_entity_logic:operation(),
    EntityId :: n_entity_logic:entity_id(), Resource :: resource(),
    Client :: n_entity_logic:client()) ->
    n_entity_logic:authorization_verificator() |
    [authorization_verificator:existence_verificator()].
% TODO VFS-2918
authorize(create, _GroupId, {deprecated_user_privileges, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE);
% TODO VFS-2918
authorize(create, _GroupId, {deprecated_group_privileges, _ChildGroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE);

authorize(create, undefined, entity, ?USER(UserId)) ->
    {data_dependent, fun(Data) ->
        HServiceId = maps:get(<<"handleServiceId">>, Data, <<"">>),
        ShareId = maps:get(<<"resourceId">>, Data, <<"">>),
        {ok, #od_share{space = SpaceId}} = od_share:get(ShareId),
        CanManageShares = n_space_logic:has_eff_privilege(
            SpaceId, UserId, ?SPACE_MANAGE_SHARES
        ),
        CanRegisterHandles = n_handle_service_logic:user_has_eff_privilege(
            HServiceId, UserId, ?HANDLE_SERVICE_REGISTER_HANDLE
        ),
        CanManageShares andalso CanRegisterHandles
    end};

authorize(create, _HandleId, {user, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE);

authorize(create, _HandleId, {group, _GroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE);


authorize(get, _HandleId, entity, ?USER(UserId)) ->[
    auth_by_privilege(UserId, ?HANDLE_VIEW),
    auth_by_oz_privilege(UserId, ?OZ_HANDLES_LIST)
];

authorize(get, _HandleId, data, ?USER(UserId)) ->[
    auth_by_privilege(UserId, ?HANDLE_VIEW),
    auth_by_oz_privilege(UserId, ?OZ_HANDLES_LIST)
];

authorize(get, _HandleId, public_data, _Client) ->
    % Public data is available to whomever has handle id.
    true;

authorize(get, undefined, list, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_HANDLES_LIST);

authorize(get, _HandleId, _, ?USER(UserId)) ->
    % All other resources can be accessed with view privileges
    auth_by_privilege(UserId, ?HANDLE_VIEW);


authorize(update, _HandleId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE);

authorize(update, _HandleId, {user_privileges, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE);

authorize(update, _HandleId, {group_privileges, _GroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE);


authorize(delete, _HandleId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_DELETE);

authorize(delete, _HandleId, {user, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE);

authorize(delete, _HandleId, {group, _GroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE).


%%--------------------------------------------------------------------
%% @doc
%% Returns validity verificators for given Operation and Resource identifier.
%% Returns a map with 'required', 'optional' and 'at_least_one' keys.
%% Under each of them, there is a map:
%%      Key => {type_verificator, value_verificator}
%% Which means how value of given Key should be validated.
%% @end
%%--------------------------------------------------------------------
-spec validate(Operation :: n_entity_logic:operation(),
    Resource :: resource()) ->
    n_entity_logic:validity_verificator().
validate(create, entity) -> #{
    required => #{
        <<"handleServiceId">> => {binary, {exists, fun(Value) ->
            n_handle_service_logic:exists(Value)
        end}},
        <<"resourceType">> => {binary, [<<"Share">>]},
        <<"resourceId">> => {binary, {exists, fun(Value) ->
            n_share_logic:exists(Value) end
        }},
        <<"metadata">> => {binary, non_empty}
    }
};
validate(create, {user, _UserId}) -> #{
    required => #{
        resource => {any, {resource_exists, <<"User Id">>, fun({user, UserId}) ->
            n_user_logic:exists(UserId) end}
        },
        <<"privileges">> => {list_of_atoms, privileges:handle_service_privileges()}
    }
};
validate(create, {group, _GroupId}) -> #{
    required => #{
        resource => {any, {resource_exists, <<"Group Id">>, fun({child, GroupId}) ->
            n_group_logic:exists(GroupId) end}
        },
        <<"privileges">> => {list_of_atoms, privileges:handle_service_privileges()}
    }
};
validate(update, entity) -> #{
    required => #{
        <<"metadata">> => {binary, non_empty}
    }
}.


%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the entity with given id.
%% @end
%%--------------------------------------------------------------------
-spec entity_to_string(EntityId :: n_entity_logic:entity_id()) -> binary().
entity_to_string(HandleId) ->
    od_handle:to_string(HandleId).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns authorization verificator that checks if given user has specific
%% effective privilege in the handle represented by entity.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_privilege(UserId :: od_user:id(),
    Privilege :: privileges:handle_privilege()) ->
    n_entity_logic:authorization_verificator().
auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_handle{} = Handle) ->
        n_handle_logic:user_has_eff_privilege(Handle, UserId, Privilege)
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
    n_entity_logic:authorization_verificator().
auth_by_oz_privilege(UserId, Privilege) ->
    {external, fun() ->
        n_user_logic:has_eff_oz_privilege(UserId, Privilege)
    end}.
