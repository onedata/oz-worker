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

-export([fetch_entity/1, operation_supported/3, is_subscribable/2]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, required_admin_privileges/1, validate/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity and its revision from datastore based on EntityId.
%% Should return ?ERROR_NOT_FOUND if the entity does not exist.
%% @end
%%--------------------------------------------------------------------
-spec fetch_entity(entity_logic:entity_id()) ->
    {ok, entity_logic:versioned_entity()} | entity_logic:error().
fetch_entity(HServiceId) ->
    case od_handle_service:get(HServiceId) of
        {ok, #document{value = HandleService, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_utils:parse_rev(DbRev),
            {ok, {HandleService, Revision}};
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
%% Determines if given {Aspect, Scope} pair is subscribable, i.e. clients can
%% subscribe to receive updates concerning the aspect of entity.
%% @end
%%--------------------------------------------------------------------
-spec is_subscribable(entity_logic:aspect(), entity_logic:scope()) ->
    boolean().
is_subscribable(instance, _) -> true;
is_subscribable(_, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(Req = #el_req{gri = #gri{id = undefined, aspect = instance} = GRI, auth = Auth}) ->
    Name = maps:get(<<"name">>, Req#el_req.data),
    ProxyEndpoint = maps:get(<<"proxyEndpoint">>, Req#el_req.data),
    ServiceProperties = maps:get(<<"serviceProperties">>, Req#el_req.data),
    HandleService = #document{value = #od_handle_service{
        name = Name,
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties,
        creator = Auth#auth.subject
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
    {ok, {FetchedHandleService, Rev}} = fetch_entity(HServiceId),
    {ok, resource, {GRI#gri{id = HServiceId}, {FetchedHandleService, Rev}}};

create(#el_req{gri = #gri{id = HServiceId, aspect = {user, UserId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:handle_service_user()),
    entity_graph:add_relation(
        od_user, UserId,
        od_handle_service, HServiceId,
        Privileges
    ),
    NewGRI = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
    {ok, {User, Rev}} = user_logic_plugin:fetch_entity(UserId),
    {ok, UserData} = user_logic_plugin:get(#el_req{gri = NewGRI}, User),
    {ok, resource, {NewGRI, ?THROUGH_HANDLE_SERVICE(HServiceId), {UserData, Rev}}};

create(#el_req{gri = #gri{id = HServiceId, aspect = {group, GroupId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:handle_service_user()),
    entity_graph:add_relation(
        od_group, GroupId,
        od_handle_service, HServiceId,
        Privileges
    ),
    NewGRI = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
    {ok, {Group, Rev}} = group_logic_plugin:fetch_entity(GroupId),
    {ok, GroupData} = group_logic_plugin:get(#el_req{gri = NewGRI}, Group),
    {ok, resource, {NewGRI, ?THROUGH_HANDLE_SERVICE(HServiceId), {GroupData, Rev}}}.


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
        name = Name, proxy_endpoint = Proxy, service_properties = ServiceProps,
        creation_time = CreationTime, creator = Creator
    } = HService,
    {ok, #{
        <<"name">> => Name,
        <<"proxyEndpoint">> => Proxy,
        <<"serviceProperties">> => ServiceProps,
        <<"creationTime">> => CreationTime,
        <<"creator">> => Creator
    }};

get(#el_req{gri = #gri{aspect = users}}, HService) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_user, HService)};
get(#el_req{gri = #gri{aspect = eff_users}}, HService) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_user, HService)};
get(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, HService) ->
    {ok, entity_graph:get_privileges(direct, bottom_up, od_user, UserId, HService)};
get(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, HService) ->
    {ok, entity_graph:get_privileges(effective, bottom_up, od_user, UserId, HService)};

get(#el_req{gri = #gri{aspect = groups}}, HService) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_group, HService)};
get(#el_req{gri = #gri{aspect = eff_groups}}, HService) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_group, HService)};
get(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, HService) ->
    {ok, entity_graph:get_privileges(direct, bottom_up, od_group, GroupId, HService)};
get(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, HService) ->
    {ok, entity_graph:get_privileges(effective, bottom_up, od_group, GroupId, HService)};

get(#el_req{gri = #gri{aspect = handles}}, HService) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_handle, HService)}.


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
    PrivsToGrant = maps:get(<<"grant">>, Req#el_req.data, []),
    PrivsToRevoke = maps:get(<<"revoke">>, Req#el_req.data, []),
    entity_graph:update_relation(
        od_user, UserId,
        od_handle_service, HServiceId,
        {PrivsToGrant, PrivsToRevoke}
    );

update(Req = #el_req{gri = #gri{id = HServiceId, aspect = {group_privileges, GroupId}}}) ->
    PrivsToGrant = maps:get(<<"grant">>, Req#el_req.data, []),
    PrivsToRevoke = maps:get(<<"revoke">>, Req#el_req.data, []),
    entity_graph:update_relation(
        od_group, GroupId,
        od_handle_service, HServiceId,
        {PrivsToGrant, PrivsToRevoke}
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
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, HService);

exists(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, HService) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, HService);

exists(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, HService) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, HService);

exists(#el_req{gri = #gri{aspect = {group, GroupId}}}, HService) ->
    entity_graph:has_relation(direct, bottom_up, od_group, GroupId, HService);

exists(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, HService) ->
    entity_graph:has_relation(direct, bottom_up, od_group, GroupId, HService);

exists(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, HService) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, HService);

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
authorize(Req = #el_req{operation = create, gri = #gri{id = undefined, aspect = instance}}, _) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            user_logic_plugin:auth_by_oz_privilege(UserId, ?OZ_HANDLE_SERVICES_CREATE);

        {?USER(UserId), ?AS_GROUP(GroupId)} ->
            group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_CREATE_HANDLE_SERVICE) andalso
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

authorize(Req = #el_req{operation = get, gri = GRI = #gri{aspect = instance, scope = protected}}, HService) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
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
            authorize(Req#el_req{gri = GRI#gri{scope = private}}, HService)
    end;

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {user_privileges, UserId}}}, _) ->
    true;

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {eff_user_privileges, UserId}}}, _) ->
    true;

authorize(Req = #el_req{operation = get, auth = ?USER}, HService) ->
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
%% Returns list of admin privileges needed to perform given operation.
%% @end
%%--------------------------------------------------------------------
-spec required_admin_privileges(entity_logic:req()) -> [privileges:oz_privilege()] | forbidden.
required_admin_privileges(Req = #el_req{operation = create, gri = #gri{aspect = instance}}) ->
    case Req#el_req.auth_hint of
        ?AS_USER(_) -> [?OZ_HANDLE_SERVICES_CREATE, ?OZ_USERS_ADD_RELATIONSHIPS];
        ?AS_GROUP(_) -> [?OZ_HANDLE_SERVICES_CREATE, ?OZ_GROUPS_ADD_RELATIONSHIPS];
        _ -> [?OZ_HANDLE_SERVICES_CREATE]
    end;

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {user, _}}}) ->
    [?OZ_HANDLE_SERVICES_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {group, _}}}) ->
    [?OZ_HANDLE_SERVICES_ADD_RELATIONSHIPS, ?OZ_GROUPS_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = list}}) ->
    [?OZ_HANDLE_SERVICES_LIST];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance, scope = protected}}) ->
    [?OZ_HANDLE_SERVICES_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = users}}) ->
    [?OZ_HANDLE_SERVICES_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_users}}) ->
    [?OZ_HANDLE_SERVICES_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {user_privileges, _}}}) ->
    [?OZ_HANDLE_SERVICES_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_user_privileges, _}}}) ->
    [?OZ_HANDLE_SERVICES_VIEW_PRIVILEGES];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = groups}}) ->
    [?OZ_HANDLE_SERVICES_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_groups}}) ->
    [?OZ_HANDLE_SERVICES_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {group_privileges, _}}}) ->
    [?OZ_HANDLE_SERVICES_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_group_privileges, _}}}) ->
    [?OZ_HANDLE_SERVICES_VIEW_PRIVILEGES];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = handles}}) ->
    [?OZ_HANDLE_SERVICES_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = instance}}) ->
    [?OZ_HANDLE_SERVICES_UPDATE];
required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) ->
    [?OZ_HANDLE_SERVICES_SET_PRIVILEGES];
required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {group_privileges, _}}}) ->
    [?OZ_HANDLE_SERVICES_SET_PRIVILEGES];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = instance}}) ->
    [?OZ_HANDLE_SERVICES_DELETE];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {user, _}}}) ->
    [?OZ_HANDLE_SERVICES_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {group, _}}}) ->
    [?OZ_HANDLE_SERVICES_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS];

required_admin_privileges(_) ->
    forbidden.

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
        at_least_one => #{
            <<"grant">> => {list_of_atoms, privileges:handle_service_privileges()},
            <<"revoke">> => {list_of_atoms, privileges:handle_service_privileges()}
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
%% Auths of type other than user are discarded.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_privilege(entity_logic:req() | od_user:id(),
    od_handle_service:id() | od_handle_service:info(),
    privileges:handle_service_privilege()) -> boolean().
auth_by_privilege(#el_req{auth = ?USER(UserId)}, HServiceOrId, Privilege) ->
    auth_by_privilege(UserId, HServiceOrId, Privilege);
auth_by_privilege(#el_req{auth = _OtherAuth}, _HServiceOrId, _Privilege) ->
    false;
auth_by_privilege(UserId, HServiceOrId, Privilege) ->
    handle_service_logic:has_eff_privilege(HServiceOrId, UserId, Privilege).
