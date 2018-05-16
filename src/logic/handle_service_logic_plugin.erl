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
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/api_errors.hrl").

-export([fetch_entity/1, operation_supported/3]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, validate/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity from datastore based on its EntityId.
%% Should return ?ERROR_NOT_FOUND if the entity does not exist.
%% @end
%%--------------------------------------------------------------------
-spec fetch_entity(entity_logic:entity_id()) ->
    {ok, entity_logic:entity()} | entity_logic:error().
fetch_entity(HServiceId) ->
    case od_handle_service:get(HServiceId) of
        {ok, #document{value = HandleService}} ->
            {ok, HandleService};
        _ ->
            ?ERROR_NOT_FOUND
    end.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given operation is supported based on operation, aspect and
%% scope (entity type is known based on the plugin itself).
%% @end
%%--------------------------------------------------------------------
-spec operation_supported(entity_logic:operation(), entity_logic:aspect(),
    entity_logic:scope()) -> boolean().
operation_supported(create, instance, private) -> true;
operation_supported(create, {user, _}, private) -> true;
operation_supported(create, {group, _}, private) -> true;

operation_supported(get, list, private) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, instance, protected) -> true;

operation_supported(get, users, private) -> true;
operation_supported(get, eff_users, private) -> true;
operation_supported(get, {user_privileges, _}, private) -> true;
operation_supported(get, {eff_user_privileges, _}, private) -> true;

operation_supported(get, groups, private) -> true;
operation_supported(get, eff_groups, private) -> true;
operation_supported(get, {group_privileges, _}, private) -> true;
operation_supported(get, {eff_group_privileges, _}, private) -> true;

operation_supported(get, handles, private) -> true;

operation_supported(update, instance, private) -> true;
operation_supported(update, {user_privileges, _}, private) -> true;
operation_supported(update, {group_privileges, _}, private) -> true;

operation_supported(delete, instance, private) -> true;
operation_supported(delete, {user, _}, private) -> true;
operation_supported(delete, {group, _}, private) -> true;

operation_supported(_, _, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(Req = #el_req{gri = #gri{id = undefined, aspect = instance} = GRI}) ->
    Name = maps:get(<<"name">>, Req#el_req.data),
    ProxyEndpoint = maps:get(<<"proxyEndpoint">>, Req#el_req.data),
    ServiceProperties = maps:get(<<"serviceProperties">>, Req#el_req.data),
    HandleService = #document{value = #od_handle_service{
        name = Name,
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties
    }},
    {ok, #document{key = HServiceId}} = od_handle_service:create(HandleService),
    case Req#el_req.auth_hint of
        ?AS_USER(UserId) ->
            entity_graph:add_relation(
                od_user, UserId,
                od_handle_service, HServiceId,
                privileges:handle_service_admin()
            );
        ?AS_GROUP(GroupId) ->
            entity_graph:add_relation(
                od_group, GroupId,
                od_handle_service, HServiceId,
                privileges:handle_service_admin()
            );
        _ ->
            ok
    end,
    % Handle Service has been modified by adding relation, so it will need to be
    % fetched again.
    {ok, {not_fetched, GRI#gri{id = HServiceId}}};

create(#el_req{gri = #gri{id = HServiceId, aspect = {user, UserId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:handle_service_user()),
    entity_graph:add_relation(
        od_user, UserId,
        od_handle_service, HServiceId,
        Privileges
    ),
    NewGRI = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
    {ok, {not_fetched, NewGRI, ?THROUGH_HANDLE_SERVICE(HServiceId)}};

create(#el_req{gri = #gri{id = HServiceId, aspect = {group, GroupId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:handle_service_user()),
    entity_graph:add_relation(
        od_group, GroupId,
        od_handle_service, HServiceId,
        Privileges
    ),
    NewGRI = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
    {ok, {not_fetched, NewGRI, ?THROUGH_HANDLE_SERVICE(HServiceId)}}.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = list}}, _) ->
    {ok, HServiceDocs} = od_handle_service:list(),
    {ok, [HServiceId || #document{key = HServiceId} <- HServiceDocs]};

get(#el_req{gri = #gri{aspect = instance, scope = private}}, HService) ->
    {ok, HService};
get(#el_req{gri = #gri{aspect = instance, scope = protected}}, HService) ->
    #od_handle_service{
        name = Name, proxy_endpoint = Proxy, service_properties = ServiceProps
    } = HService,
    {ok, #{
        <<"name">> => Name,
        <<"proxyEndpoint">> => Proxy,
        <<"serviceProperties">> => ServiceProps
    }};

get(#el_req{gri = #gri{aspect = users}}, HService) ->
    {ok, maps:keys(HService#od_handle_service.users)};
get(#el_req{gri = #gri{aspect = eff_users}}, HService) ->
    {ok, maps:keys(HService#od_handle_service.eff_users)};
get(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, HService) ->
    {ok, maps:get(UserId, HService#od_handle_service.users)};
get(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, HService) ->
    {Privileges, _} = maps:get(UserId, HService#od_handle_service.eff_users),
    {ok, Privileges};

get(#el_req{gri = #gri{aspect = groups}}, HService) ->
    {ok, maps:keys(HService#od_handle_service.groups)};
get(#el_req{gri = #gri{aspect = eff_groups}}, HService) ->
    {ok, maps:keys(HService#od_handle_service.eff_groups)};
get(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, HService) ->
    {ok, maps:get(GroupId, HService#od_handle_service.groups)};
get(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, HService) ->
    {Privileges, _} = maps:get(GroupId, HService#od_handle_service.eff_groups),
    {ok, Privileges};

get(#el_req{gri = #gri{aspect = handles}}, HService) ->
    {ok, HService#od_handle_service.handles}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = HServiceId, aspect = instance}, data = Data}) ->
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

update(Req = #el_req{gri = #gri{id = HServiceId, aspect = {user_privileges, UserId}}}) ->
    Privileges = maps:get(<<"privileges">>, Req#el_req.data),
    Operation = maps:get(<<"operation">>, Req#el_req.data, set),
    entity_graph:update_relation(
        od_user, UserId,
        od_handle_service, HServiceId,
        {Operation, Privileges}
    );

update(Req = #el_req{gri = #gri{id = HServiceId, aspect = {group_privileges, GroupId}}}) ->
    Privileges = maps:get(<<"privileges">>, Req#el_req.data),
    Operation = maps:get(<<"operation">>, Req#el_req.data, set),
    entity_graph:update_relation(
        od_group, GroupId,
        od_handle_service, HServiceId,
        {Operation, Privileges}
    ).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = HServiceId, aspect = instance}}) ->
    entity_graph:delete_with_relations(od_handle_service, HServiceId);

delete(#el_req{gri = #gri{id = HServiceId, aspect = {user, UserId}}}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_handle_service, HServiceId
    );

delete(#el_req{gri = #gri{id = HServiceId, aspect = {group, GroupId}}}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_handle_service, HServiceId
    ).


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(Req = #el_req{gri = #gri{aspect = instance, scope = protected}}, HService) ->
    case Req#el_req.auth_hint of
        ?THROUGH_USER(UserId) ->
            handle_service_logic:has_eff_user(HService, UserId);
        ?THROUGH_GROUP(GroupId) ->
            handle_service_logic:has_eff_group(HService, GroupId);
        undefined ->
            true
    end;

exists(#el_req{gri = #gri{aspect = {user, UserId}}}, HService) ->
    maps:is_key(UserId, HService#od_handle_service.users);

exists(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, HService) ->
    maps:is_key(UserId, HService#od_handle_service.users);

exists(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, HService) ->
    maps:is_key(UserId, HService#od_handle_service.eff_users);

exists(#el_req{gri = #gri{aspect = {group, GroupId}}}, HService) ->
    maps:is_key(GroupId, HService#od_handle_service.groups);

exists(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, HService) ->
    maps:is_key(GroupId, HService#od_handle_service.groups);

exists(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, HService) ->
    maps:is_key(GroupId, HService#od_handle_service.eff_groups);

% All other aspects exist if handle_service record exists.
exists(#el_req{gri = #gri{id = Id}}, #od_handle_service{}) ->
    Id =/= undefined.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(Req = #el_req{operation = create, gri = #gri{aspect = instance}}, _) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            user_logic_plugin:auth_by_oz_privilege(UserId, ?OZ_HANDLE_SERVICES_CREATE);

        {?USER(UserId), ?AS_GROUP(GroupId)} ->
            % TODO VFS-3351 ?GROUP_CREATE_HANDLE_SERVICE
            user_logic:has_eff_group(UserId, GroupId) andalso
                user_logic_plugin:auth_by_oz_privilege(UserId, ?OZ_HANDLE_SERVICES_CREATE);

        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, _}}}, HService) ->
    auth_by_privilege(Req, HService, ?HANDLE_SERVICE_UPDATE);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {group, _}}}, HService) ->
    auth_by_privilege(Req, HService, ?HANDLE_SERVICE_UPDATE);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = list}}, _) ->
    user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_HANDLE_SERVICES_LIST);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = instance, scope = private}}, HService) ->
    auth_by_privilege(Req, HService, ?HANDLE_SERVICE_VIEW);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = instance, scope = protected}}, HService) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?THROUGH_USER(UserId)} ->
            % User's membership in this handle_service is checked in 'exists'
            true;

        {?USER(_UserId), ?THROUGH_USER(_OtherUserId)} ->
            false;

        {?USER(ClientUserId), ?THROUGH_GROUP(GroupId)} ->
            % Groups's membership in this handle_service is checked in 'exists'
            group_logic:has_eff_user(GroupId, ClientUserId);

        {?USER(ClientUserId), _} ->
            handle_service_logic:has_eff_user(HService, ClientUserId) orelse
                user_logic_plugin:auth_by_oz_privilege(ClientUserId, ?OZ_HANDLE_SERVICES_LIST);

        _ ->
            % Access to private data also allows access to protected data
            authorize(Req#el_req{gri = #gri{scope = private}}, HService)
    end;

authorize(Req = #el_req{operation = get, client = ?USER}, HService) ->
    % All other resources can be accessed with view privileges
    auth_by_privilege(Req, HService, ?HANDLE_SERVICE_VIEW);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = instance}}, HService) ->
    auth_by_privilege(Req, HService, ?HANDLE_SERVICE_UPDATE);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}, HService) ->
    auth_by_privilege(Req, HService, ?HANDLE_SERVICE_UPDATE);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {group_privileges, _}}}, HService) ->
    auth_by_privilege(Req, HService, ?HANDLE_SERVICE_UPDATE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = instance}}, HService) ->
    auth_by_privilege(Req, HService, ?HANDLE_SERVICE_DELETE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {user, _}}}, HService) ->
    auth_by_privilege(Req, HService, ?HANDLE_SERVICE_UPDATE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {group, _}}}, HService) ->
    auth_by_privilege(Req, HService, ?HANDLE_SERVICE_UPDATE);

authorize(_, _) ->
    false.


%%--------------------------------------------------------------------
%% @doc
%% Returns validity verificators for given request.
%% Returns a map with 'required', 'optional' and 'at_least_one' keys.
%% Under each of them, there is a map:
%%      Key => {type_verificator, value_verificator}
%% Which means how value of given Key should be validated.
%% @end
%%--------------------------------------------------------------------
-spec validate(entity_logic:req()) -> entity_logic:validity_verificator().
validate(#el_req{operation = create, gri = #gri{aspect = instance}}) -> #{
    required => #{
        <<"name">> => {binary, name},
        <<"proxyEndpoint">> => {binary, non_empty},
        <<"serviceProperties">> => {json, non_empty}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {user, _}}}) -> #{
    required => #{
        {aspect, <<"userId">>} => {any, {exists, fun(UserId) ->
            user_logic:exists(UserId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:handle_service_privileges()}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {group, _}}}) -> #{
    required => #{
        {aspect, <<"groupId">>} => {any, {exists, fun(GroupId) ->
            group_logic:exists(GroupId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:handle_service_privileges()}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    at_least_one => #{
        <<"name">> => {binary, name},
        <<"proxyEndpoint">> => {binary, non_empty},
        <<"serviceProperties">> => {json, non_empty}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) ->
    #{
        required => #{
            <<"privileges">> => {list_of_atoms, privileges:handle_service_privileges()}
        },
        optional => #{
            <<"operation">> => {atom, [set, grant, revoke]}
        }
    };

validate(#el_req{operation = update, gri = #gri{aspect = {group_privileges, Id}}}) ->
    validate(#el_req{operation = update, gri = #gri{aspect = {user_privileges, Id}}}).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns if given user has specific effective privilege in the handle_service.
%% UserId is either given explicitly or derived from entity logic request.
%% Clients of type other than user are discarded.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_privilege(entity_logic:req() | od_user:id(),
    od_handle_service:id() | od_handle_service:info(),
    privileges:handle_service_privilege()) -> boolean().
auth_by_privilege(#el_req{client = ?USER(UserId)}, HServiceOrId, Privilege) ->
    auth_by_privilege(UserId, HServiceOrId, Privilege);
auth_by_privilege(#el_req{client = _OtherClient}, _HServiceOrId, _Privilege) ->
    false;
auth_by_privilege(UserId, HServiceOrId, Privilege) ->
    handle_service_logic:has_eff_privilege(HServiceOrId, UserId, Privilege).
