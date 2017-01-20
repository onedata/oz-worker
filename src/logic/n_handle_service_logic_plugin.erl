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
-module(n_handle_service_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("entity_logic.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").


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
get_entity(HServiceId) ->
    case od_handle_service:get(HServiceId) of
        {ok, #document{value = HandleService}} ->
            {ok, HandleService};
        _ ->
            ?ERROR_NOT_FOUND
    end.


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
create(_Client, HServiceId, {deprecated_child_privileges, GroupId}, Data) ->
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
create(?USER, HServiceId, users, #{<<"userId">> := UserId}) ->
    entity_graph:add_relation(
        od_user, UserId,
        od_handle_service, HServiceId,
        privileges:handle_service_user()
    ),
    {ok, HServiceId};
create(?USER, HServiceId, groups, #{<<"groupId">> := GroupId}) ->
    entity_graph:add_relation(
        od_group, GroupId,
        od_handle_service, HServiceId,
        privileges:handle_service_user()
    ),
    {ok, HServiceId}.



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
    {ok, User} = ?throw_on_failure(n_user_logic_plugin:get_entity(UserId)),
    n_user_logic_plugin:get(?ROOT, UserId, User, data);
get(_, _HServiceId, #od_handle_service{}, {eff_user, UserId}) ->
    {ok, User} = ?throw_on_failure(n_user_logic_plugin:get_entity(UserId)),
    n_user_logic_plugin:get(?ROOT, UserId, User, data);
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
    {ok, Group} = ?throw_on_failure(n_group_logic_plugin:get_entity(GroupId)),
    n_group_logic_plugin:get(?ROOT, GroupId, Group, data);
get(_, _HServiceId, #od_handle_service{}, {eff_group, GroupId}) ->
    {ok, Group} = ?throw_on_failure(n_group_logic_plugin:get_entity(GroupId)),
    n_group_logic_plugin:get(?ROOT, GroupId, Group, data);
get(_, _HServiceId, #od_handle_service{groups = Groups}, {group_privileges, GroupId}) ->
    {ok, maps:get(GroupId, Groups)};
get(_, _HServiceId, #od_handle_service{eff_groups = Groups}, {eff_group_privileges, GroupId}) ->
    {Privileges, _} = maps:get(GroupId, Groups),
    {ok, Privileges};

get(_, _HServiceId, #od_handle_service{handles = Handles}, handles) ->
    {ok, Handles};
get(_, _HServiceId, #od_handle_service{}, {handle, HandleId}) ->
    {ok, Handle} = ?throw_on_failure(n_handle_logic_plugin:get_entity(HandleId)),
    n_handle_logic_plugin:get(?ROOT, HandleId, Handle, data).



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
        maps:is_key(HandleId, Handles)
    end};

exists(_) ->
    % No matter the resource, return true if it belongs to a handle
    {internal, fun(#od_handle_service{}) ->
        % If the handle with HandleId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


% TODO VFS-2918
authorize(create, _HServiceId, {deprecated_user_privileges, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_SERVICE_UPDATE);
% TODO VFS-2918
authorize(create, _HServiceId, {deprecated_child_privileges, _GroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_SERVICE_UPDATE);

authorize(create, undefined, entity, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_HANDLE_SERVICES_CREATE);

authorize(create, _HServiceId, users, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_SERVICE_UPDATE);

authorize(create, _HServiceId, groups, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_SERVICE_UPDATE);


authorize(get, _HServiceId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_SERVICE_VIEW);

authorize(get, _HServiceId, data, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_SERVICE_VIEW);

authorize(get, undefined, list, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_HANDLE_SERVICES_LIST);

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
    auth_by_privilege(UserId, ?HANDLE_SERVICE_UPDATE).




validate(create, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty},
        <<"proxyEndpoint">> => {binary, non_empty},
        <<"serviceProperties">> => {json, non_empty}
    }
};
validate(create, users) -> #{
    required => #{
        <<"userId">> => {binary, {exists, fun(Value) ->
            n_user_logic:exists(Value) end
        }}
    }
};
validate(create, groups) -> #{
    required => #{
        <<"groupId">> => {binary, {exists, fun(Value) ->
            n_group_logic:exists(Value) end
        }}
    }
};
validate(update, entity) -> #{
    at_least_one => #{
        <<"name">> => {binary, non_empty},
        <<"proxyEndpoint">> => {binary, non_empty},
        <<"serviceProperties">> => {json, non_empty}
    }
}.


entity_to_string(HServiceId) ->
    od_handle_service:to_string(HServiceId).



auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_handle_service{} = HService) ->
        n_handle_service_logic:user_has_eff_privilege(HService, UserId, Privilege)
    end}.
