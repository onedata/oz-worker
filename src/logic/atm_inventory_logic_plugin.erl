%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_atm_inventory model.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_inventory_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/errors.hrl").

-export([fetch_entity/1, operation_supported/3, is_subscribable/2]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, required_admin_privileges/1, validate/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity and its revision from datastore, if applicable.
%% Should return:
%%  * {true, entity_logic:versioned_entity()}
%%      if the fetch was successful
%%  * false
%%      if fetch is not applicable for this operation
%%  * {error, _}
%%      if there was an error, such as ?ERROR_NOT_FOUND
%% @end
%%--------------------------------------------------------------------
-spec fetch_entity(gri:gri()) ->
    {true, entity_logic:versioned_entity()} | false | errors:error().
fetch_entity(#gri{id = AtmInventoryId}) ->
    case od_atm_inventory:get(AtmInventoryId) of
        {ok, #document{value = AtmInventory, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_rev:parse(DbRev),
            {true, {AtmInventory, Revision}};
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
operation_supported(create, join, private) -> true;

operation_supported(create, {user, _}, private) -> true;
operation_supported(create, {group, _}, private) -> true;
operation_supported(create, group, private) -> true;

operation_supported(get, list, private) -> true;
operation_supported(get, privileges, _) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, instance, protected) -> true;

operation_supported(get, users, private) -> true;
operation_supported(get, eff_users, private) -> true;
operation_supported(get, {user_privileges, _}, private) -> true;
operation_supported(get, {eff_user_privileges, _}, private) -> true;
operation_supported(get, {eff_user_membership, _}, private) -> true;

operation_supported(get, groups, private) -> true;
operation_supported(get, eff_groups, private) -> true;
operation_supported(get, {group_privileges, _}, private) -> true;
operation_supported(get, {eff_group_privileges, _}, private) -> true;
operation_supported(get, {eff_group_membership, _}, private) -> true;

operation_supported(get, atm_lambdas, private) -> true;
operation_supported(get, atm_workflow_schemas, private) -> true;

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
is_subscribable(users, private) -> true;
is_subscribable(eff_users, private) -> true;
is_subscribable(groups, private) -> true;
is_subscribable(eff_groups, private) -> true;
is_subscribable(group, private) -> true;
is_subscribable({group, _}, private) -> true;
is_subscribable({user_privileges, _}, private) -> true;
is_subscribable({eff_user_privileges, _}, private) -> true;
is_subscribable({eff_user_membership, _}, private) -> true;
is_subscribable({group_privileges, _}, private) -> true;
is_subscribable({eff_group_privileges, _}, private) -> true;
is_subscribable({eff_group_membership, _}, private) -> true;
is_subscribable(atm_lambdas, private) -> true;
is_subscribable(atm_workflow_schemas, private) -> true;
is_subscribable(_, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(Req = #el_req{gri = #gri{id = undefined, aspect = instance} = GRI, auth = Auth}) ->
    #{<<"name">> := Name} = Req#el_req.data,
    {ok, #document{key = AtmInventoryId}} = od_atm_inventory:create(#document{
        value = #od_atm_inventory{
            name = Name,
            creator = aai:normalize_subject(Auth#auth.subject),
            creation_time = global_clock:timestamp_seconds()
        }
    }),
    case Req#el_req.auth_hint of
        ?AS_USER(UserId) ->
            entity_graph:add_relation(
                od_user, UserId,
                od_atm_inventory, AtmInventoryId,
                privileges:atm_inventory_admin()
            );
        ?AS_GROUP(GroupId) ->
            entity_graph:add_relation(
                od_group, GroupId,
                od_atm_inventory, AtmInventoryId,
                privileges:atm_inventory_admin()
            );
        _ ->
            ok
    end,

    {true, {AtmInventory, Rev}} = fetch_entity(#gri{aspect = instance, id = AtmInventoryId}),
    {ok, resource, {GRI#gri{id = AtmInventoryId}, {AtmInventory, Rev}}};

create(Req = #el_req{auth = Auth, gri = #gri{id = undefined, aspect = join}}) ->
    Token = maps:get(<<"token">>, Req#el_req.data),
    ExpType = case Req#el_req.auth_hint of
        ?AS_USER(_) -> ?USER_JOIN_ATM_INVENTORY;
        ?AS_GROUP(_) -> ?GROUP_JOIN_ATM_INVENTORY
    end,

    invite_tokens:consume(Auth, Token, ExpType, fun(AtmInventoryId, _, Privileges) ->
        case Req#el_req.auth_hint of
            ?AS_USER(UserId) ->
                entity_graph:add_relation(
                    od_user, UserId,
                    od_atm_inventory, AtmInventoryId,
                    Privileges
                );
            ?AS_GROUP(GroupId) ->
                entity_graph:add_relation(
                    od_group, GroupId,
                    od_atm_inventory, AtmInventoryId,
                    Privileges
                )
        end,
        NewGRI = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = instance,
            scope = case lists:member(?ATM_INVENTORY_VIEW, Privileges) of
                true -> private;
                false -> protected
            end
        },
        {true, {AtmInventory, Rev}} = fetch_entity(#gri{aspect = instance, id = AtmInventoryId}),
        {ok, AtmInventoryData} = get(#el_req{gri = NewGRI}, AtmInventory),
        {ok, resource, {NewGRI, {AtmInventoryData, Rev}}}
    end);

create(#el_req{gri = #gri{id = AtmInventoryId, aspect = {user, UserId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:atm_inventory_member()),
    entity_graph:add_relation(
        od_user, UserId,
        od_atm_inventory, AtmInventoryId,
        Privileges
    ),
    NewGRI = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
    {true, {User, Rev}} = user_logic_plugin:fetch_entity(#gri{id = UserId}),
    {ok, UserData} = user_logic_plugin:get(#el_req{gri = NewGRI}, User),
    {ok, resource, {NewGRI, ?THROUGH_ATM_INVENTORY(AtmInventoryId), {UserData, Rev}}};

create(Req = #el_req{gri = GRI = #gri{id = AtmInventoryId, aspect = group}}) ->
    % Create a new group for the requesting client and add the group as a member
    % of this inventory.
    {ok, resource, {NewGRI = #gri{id = GroupId}, _}} = group_logic_plugin:create(
        Req#el_req{gri = GRI#gri{type = od_group, id = undefined, aspect = instance}}
    ),
    Privileges = privileges:atm_inventory_member(),
    entity_graph:add_relation(
        od_group, GroupId,
        od_atm_inventory, AtmInventoryId,
        Privileges
    ),
    {true, {Group, Rev}} = group_logic_plugin:fetch_entity(#gri{id = GroupId}),
    {ok, resource, {NewGRI, {Group, Rev}}};

create(#el_req{gri = #gri{id = AtmInventoryId, aspect = {group, GroupId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:atm_inventory_member()),
    entity_graph:add_relation(
        od_group, GroupId,
        od_atm_inventory, AtmInventoryId,
        Privileges
    ),
    NewGRI = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
    {true, {Group, Rev}} = group_logic_plugin:fetch_entity(#gri{id = GroupId}),
    {ok, GroupData} = group_logic_plugin:get(#el_req{gri = NewGRI}, Group),
    {ok, resource, {NewGRI, ?THROUGH_GROUP(AtmInventoryId), {GroupData, Rev}}}.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = list}}, _) ->
    {ok, AtmInventoryDocs} = od_atm_inventory:list(),
    {ok, [AtmInventoryId || #document{key = AtmInventoryId} <- AtmInventoryDocs]};

get(#el_req{gri = #gri{aspect = privileges}}, _) ->
    {ok, #{
        <<"member">> => privileges:atm_inventory_member(),
        <<"manager">> => privileges:atm_inventory_manager(),
        <<"admin">> => privileges:atm_inventory_admin()
    }};

get(#el_req{gri = #gri{aspect = instance, scope = private}}, AtmInventory) ->
    {ok, AtmInventory};
get(#el_req{gri = #gri{aspect = instance, scope = protected}}, AtmInventory) ->
    #od_atm_inventory{
        name = Name,
        creation_time = CreationTime,
        creator = Creator
    } = AtmInventory,

    {ok, #{
        <<"name">> => Name,
        <<"creationTime">> => CreationTime,
        <<"creator">> => Creator
    }};

get(#el_req{gri = #gri{aspect = users}}, AtmInventory) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_user, AtmInventory)};
get(#el_req{gri = #gri{aspect = eff_users}}, AtmInventory) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_user, AtmInventory)};
get(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, AtmInventory) ->
    {ok, entity_graph:get_relation_attrs(direct, bottom_up, od_user, UserId, AtmInventory)};
get(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, AtmInventory) ->
    {ok, entity_graph:get_relation_attrs(effective, bottom_up, od_user, UserId, AtmInventory)};
get(#el_req{gri = #gri{aspect = {eff_user_membership, UserId}}}, AtmInventory) ->
    {ok, entity_graph:get_intermediaries(bottom_up, od_user, UserId, AtmInventory)};

get(#el_req{gri = #gri{aspect = groups}}, AtmInventory) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_group, AtmInventory)};
get(#el_req{gri = #gri{aspect = eff_groups}}, AtmInventory) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_group, AtmInventory)};
get(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, AtmInventory) ->
    {ok, entity_graph:get_relation_attrs(direct, bottom_up, od_group, GroupId, AtmInventory)};
get(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, AtmInventory) ->
    {ok, entity_graph:get_relation_attrs(effective, bottom_up, od_group, GroupId, AtmInventory)};
get(#el_req{gri = #gri{aspect = {eff_group_membership, GroupId}}}, AtmInventory) ->
    {ok, entity_graph:get_intermediaries(bottom_up, od_group, GroupId, AtmInventory)};

get(#el_req{gri = #gri{aspect = atm_lambdas}}, AtmInventory) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_atm_lambda, AtmInventory)};

get(#el_req{gri = #gri{aspect = atm_workflow_schemas}}, AtmInventory) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_atm_workflow_schema, AtmInventory)}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = AtmInventoryId, aspect = instance}, data = Data}) ->
    ?extract_ok(od_atm_inventory:update(AtmInventoryId, fun(AtmInventory) ->
        {ok, AtmInventory#od_atm_inventory{
            name = maps:get(<<"name">>, Data, AtmInventory#od_atm_inventory.name)
        }}
    end));

update(Req = #el_req{gri = #gri{id = AtmInventoryId, aspect = {user_privileges, UserId}}}) ->
    PrivsToGrant = maps:get(<<"grant">>, Req#el_req.data, []),
    PrivsToRevoke = maps:get(<<"revoke">>, Req#el_req.data, []),
    entity_graph:update_relation(
        od_user, UserId,
        od_atm_inventory, AtmInventoryId,
        {PrivsToGrant, PrivsToRevoke}
    );

update(Req = #el_req{gri = #gri{id = AtmInventoryId, aspect = {group_privileges, GroupId}}}) ->
    PrivsToGrant = maps:get(<<"grant">>, Req#el_req.data, []),
    PrivsToRevoke = maps:get(<<"revoke">>, Req#el_req.data, []),
    entity_graph:update_relation(
        od_group, GroupId,
        od_atm_inventory, AtmInventoryId,
        {PrivsToGrant, PrivsToRevoke}
    ).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = AtmInventoryId, aspect = instance}}) ->
    entity_graph:delete_with_relations(od_atm_inventory, AtmInventoryId);

delete(#el_req{gri = #gri{id = AtmInventoryId, aspect = {user, UserId}}}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_atm_inventory, AtmInventoryId
    );

delete(#el_req{gri = #gri{id = AtmInventoryId, aspect = {group, GroupId}}}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_atm_inventory, AtmInventoryId
    ).


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(Req = #el_req{gri = #gri{aspect = instance, scope = protected}}, AtmInventory) ->
    case Req#el_req.auth_hint of
        ?THROUGH_USER(UserId) ->
            atm_inventory_logic:has_eff_user(AtmInventory, UserId);
        ?THROUGH_GROUP(GroupId) ->
            atm_inventory_logic:has_eff_group(AtmInventory, GroupId);
        undefined ->
            true
    end;

exists(#el_req{gri = #gri{aspect = {user, UserId}}}, AtmInventory) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, AtmInventory);

exists(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, AtmInventory) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, AtmInventory);

exists(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, AtmInventory) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, AtmInventory);

exists(#el_req{gri = #gri{aspect = {eff_user_membership, UserId}}}, AtmInventory) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, AtmInventory);

exists(#el_req{gri = #gri{aspect = {group, GroupId}}}, AtmInventory) ->
    entity_graph:has_relation(direct, bottom_up, od_group, GroupId, AtmInventory);

exists(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, AtmInventory) ->
    entity_graph:has_relation(direct, bottom_up, od_group, GroupId, AtmInventory);

exists(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, AtmInventory) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, AtmInventory);

exists(#el_req{gri = #gri{aspect = {eff_group_membership, GroupId}}}, AtmInventory) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, AtmInventory);

% All other aspects exist if inventory record exists.
exists(#el_req{gri = #gri{id = Id}}, #od_atm_inventory{}) ->
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
            true;
        {?USER(UserId), ?AS_GROUP(GroupId)} ->
            group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_ATM_INVENTORY);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = join}}, _) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            true;
        {?USER(UserId), ?AS_GROUP(GroupId)} ->
            group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_ATM_INVENTORY);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, UserId}}, auth = ?USER(UserId), data = #{<<"privileges">> := _}}, AtmInventory) ->
    auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_ADD_USER) andalso auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_SET_PRIVILEGES);
authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, UserId}}, auth = ?USER(UserId), data = _}, AtmInventory) ->
    auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_ADD_USER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {group, GroupId}}, auth = ?USER(UserId), data = #{<<"privileges">> := _}}, AtmInventory) ->
    auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_ADD_GROUP) andalso
        auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_SET_PRIVILEGES) andalso
        group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_ATM_INVENTORY);
authorize(Req = #el_req{operation = create, gri = #gri{aspect = {group, GroupId}}, auth = ?USER(UserId), data = _}, AtmInventory) ->
    auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_ADD_GROUP) andalso
        group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_ATM_INVENTORY);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = group}}, AtmInventory) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_ADD_GROUP);
        _ ->
            false
    end;

authorize(#el_req{operation = get, gri = #gri{aspect = privileges}}, _) ->
    true;

authorize(#el_req{auth = ?USER(UserId), operation = get, gri = #gri{aspect = instance, scope = private}}, AtmInventory) ->
    auth_by_privilege(UserId, AtmInventory, ?ATM_INVENTORY_VIEW);

authorize(Req = #el_req{operation = get, gri = GRI = #gri{aspect = instance, scope = protected}}, AtmInventory) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?THROUGH_USER(UserId)} ->
            % User's membership in this inventory is checked in 'exists'
            true;

        {?USER(_UserId), ?THROUGH_USER(_OtherUserId)} ->
            false;

        {?USER(ClientUserId), ?THROUGH_GROUP(GroupId)} ->
            % Groups's membership in this inventory is checked in 'exists'
            group_logic:has_eff_privilege(GroupId, ClientUserId, ?GROUP_VIEW);

        {?USER(ClientUserId), _} ->
            atm_inventory_logic:has_eff_user(AtmInventory, ClientUserId);

        _ ->
            % Access to private data also allows access to protected data
            authorize(Req#el_req{gri = GRI#gri{scope = private}}, AtmInventory)
    end;

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {user_privileges, UserId}}}, _) ->
    true;
authorize(Req = #el_req{operation = get, gri = #gri{aspect = {user_privileges, _}}}, AtmInventory) ->
    auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_VIEW_PRIVILEGES);

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {eff_user_privileges, UserId}}}, _) ->
    true;
authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_user_privileges, _}}}, AtmInventory) ->
    auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_VIEW_PRIVILEGES);

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {eff_user_membership, UserId}}}, _) ->
    true;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_user_membership, _}}}, AtmInventory) ->
    auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_VIEW);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {group_privileges, _}}}, AtmInventory) ->
    auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_VIEW_PRIVILEGES);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_group_privileges, _}}}, AtmInventory) ->
    auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_VIEW_PRIVILEGES);

authorize(Req = #el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {eff_group_membership, GroupId}}}, AtmInventory) ->
    group_logic:has_eff_user(GroupId, UserId) orelse auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_VIEW);

authorize(#el_req{operation = get, auth = ?USER(ClientUserId), gri = #gri{aspect = atm_lambdas}}, AtmInventory) ->
    atm_inventory_logic:has_eff_user(AtmInventory, ClientUserId);

authorize(#el_req{operation = get, auth = ?USER(ClientUserId), gri = #gri{aspect = atm_workflow_schemas}}, AtmInventory) ->
    atm_inventory_logic:has_eff_user(AtmInventory, ClientUserId);

authorize(Req = #el_req{operation = get, auth = ?USER}, AtmInventory) ->
    % All other resources can be accessed with view privileges
    auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_VIEW);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = instance}}, AtmInventory) ->
    auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_UPDATE);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}, AtmInventory) ->
    auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_SET_PRIVILEGES);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {group_privileges, _}}}, AtmInventory) ->
    auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_SET_PRIVILEGES);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = instance}}, AtmInventory) ->
    auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_DELETE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {user, _}}}, AtmInventory) ->
    auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_REMOVE_USER);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {group, _}}}, AtmInventory) ->
    auth_by_privilege(Req, AtmInventory, ?ATM_INVENTORY_REMOVE_GROUP);

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
        ?AS_USER(_) -> [?OZ_ATM_INVENTORIES_CREATE, ?OZ_USERS_ADD_RELATIONSHIPS];
        ?AS_GROUP(_) -> [?OZ_ATM_INVENTORIES_CREATE, ?OZ_GROUPS_ADD_RELATIONSHIPS];
        _ -> [?OZ_ATM_INVENTORIES_CREATE]
    end;

required_admin_privileges(Req = #el_req{operation = create, gri = #gri{aspect = join}}) ->
    case Req#el_req.auth_hint of
        ?AS_USER(_) -> [?OZ_USERS_ADD_RELATIONSHIPS];
        ?AS_GROUP(_) -> [?OZ_GROUPS_ADD_RELATIONSHIPS]
    end;
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {user, _}}, data = #{<<"privileges">> := _}}) ->
    [?OZ_ATM_INVENTORIES_ADD_RELATIONSHIPS, ?OZ_ATM_INVENTORIES_SET_PRIVILEGES, ?OZ_USERS_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {user, _}}, data = _}) ->
    [?OZ_ATM_INVENTORIES_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {group, _}}, data = #{<<"privileges">> := _}}) ->
    [?OZ_ATM_INVENTORIES_ADD_RELATIONSHIPS, ?OZ_ATM_INVENTORIES_SET_PRIVILEGES, ?OZ_GROUPS_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {group, _}}, data = _}) ->
    [?OZ_ATM_INVENTORIES_ADD_RELATIONSHIPS, ?OZ_GROUPS_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = group}}) ->
    [?OZ_GROUPS_CREATE, ?OZ_ATM_INVENTORIES_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = list}}) ->
    [?OZ_ATM_INVENTORIES_LIST];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance, scope = protected}}) ->
    [?OZ_ATM_INVENTORIES_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {user_privileges, _}}}) ->
    [?OZ_ATM_INVENTORIES_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_user_privileges, _}}}) ->
    [?OZ_ATM_INVENTORIES_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_user_membership, _}}}) ->
    [?OZ_ATM_INVENTORIES_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {group_privileges, _}}}) ->
    [?OZ_ATM_INVENTORIES_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_group_privileges, _}}}) ->
    [?OZ_ATM_INVENTORIES_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_group_membership, _}}}) ->
    [?OZ_ATM_INVENTORIES_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = atm_lambdas}}) ->
    [?OZ_ATM_INVENTORIES_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = atm_workflow_schemas}}) ->
    [?OZ_ATM_INVENTORIES_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = users}}) ->
    [?OZ_ATM_INVENTORIES_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_users}}) ->
    [?OZ_ATM_INVENTORIES_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = groups}}) ->
    [?OZ_ATM_INVENTORIES_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_groups}}) ->
    [?OZ_ATM_INVENTORIES_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = instance}}) ->
    [?OZ_ATM_INVENTORIES_UPDATE];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) ->
    [?OZ_ATM_INVENTORIES_SET_PRIVILEGES];
required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {group_privileges, _}}}) ->
    [?OZ_ATM_INVENTORIES_SET_PRIVILEGES];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = instance}}) ->
    [?OZ_ATM_INVENTORIES_DELETE];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {user, _}}}) ->
    [?OZ_ATM_INVENTORIES_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {group, _}}}) ->
    [?OZ_ATM_INVENTORIES_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS];

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
validate(#el_req{operation = create, gri = #gri{aspect = instance}}) ->
    #{
        required => #{
            <<"name">> => {binary, name}
        }
    };

validate(Req = #el_req{operation = create, gri = #gri{aspect = join}}) ->
    #{
        required => #{
            <<"token">> => {invite_token, case Req#el_req.auth_hint of
                ?AS_USER(_) -> ?USER_JOIN_ATM_INVENTORY;
                ?AS_GROUP(_) -> ?GROUP_JOIN_ATM_INVENTORY
            end}
        }
    };

validate(#el_req{operation = create, gri = #gri{aspect = {user, _}}}) -> #{
    required => #{
        {aspect, <<"userId">>} => {any, {exists, fun(UserId) ->
            user_logic:exists(UserId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:atm_inventory_privileges()}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {group, _}}}) -> #{
    required => #{
        {aspect, <<"groupId">>} => {any, {exists, fun(GroupId) ->
            group_logic:exists(GroupId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:atm_inventory_privileges()}
    }
};

validate(Req = #el_req{operation = create, gri = #gri{aspect = group}}) ->
    group_logic_plugin:validate(Req#el_req{gri = #gri{
        type = od_group, id = undefined, aspect = instance
    }});

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) ->
    #{
        required => #{
            <<"name">> => {binary, name}
        }
    };

validate(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) ->
    #{
        at_least_one => #{
            <<"grant">> => {list_of_atoms, privileges:atm_inventory_privileges()},
            <<"revoke">> => {list_of_atoms, privileges:atm_inventory_privileges()}
        }
    };

validate(#el_req{operation = update, gri = #gri{aspect = {group_privileges, Id}}}) ->
    validate(#el_req{operation = update, gri = #gri{aspect = {user_privileges, Id}}}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec auth_by_privilege(entity_logic:req() | od_user:id() | aai:auth(),
    od_atm_inventory:id() | od_atm_inventory:record(), privileges:atm_inventory_privilege()) -> boolean().
auth_by_privilege(#el_req{auth = ?USER(UserId)}, AtmInventoryOrId, Privilege) ->
    auth_by_privilege(UserId, AtmInventoryOrId, Privilege);
auth_by_privilege(#el_req{auth = _OtherAuth}, _AtmInventoryOrId, _Privilege) ->
    false;
auth_by_privilege(UserId, AtmInventoryOrId, Privilege) ->
    atm_inventory_logic:has_eff_privilege(AtmInventoryOrId, UserId, Privilege).
