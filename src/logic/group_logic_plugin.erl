%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_group model.
%%% @end
%%%-------------------------------------------------------------------
-module(group_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("tokens.hrl").
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
fetch_entity(GroupId) ->
    case od_group:get(GroupId) of
        {ok, #document{value = Group}} ->
            {ok, Group};
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
operation_supported(create, invite_user_token, private) -> true;
operation_supported(create, invite_group_token, private) -> true;

operation_supported(create, instance, private) -> true;
operation_supported(create, join, private) -> true;

operation_supported(create, {user, _}, private) -> true;
operation_supported(create, {child, _}, private) -> true;

operation_supported(get, list, private) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, instance, protected) -> true;
operation_supported(get, instance, shared) -> true;

operation_supported(get, oz_privileges, private) -> true;
operation_supported(get, eff_oz_privileges, private) -> true;

operation_supported(get, users, private) -> true;
operation_supported(get, eff_users, private) -> true;
operation_supported(get, {user_privileges, _}, private) -> true;
operation_supported(get, {eff_user_privileges, _}, private) -> true;

operation_supported(get, parents, private) -> true;
operation_supported(get, eff_parents, private) -> true;

operation_supported(get, children, private) -> true;
operation_supported(get, eff_children, private) -> true;
operation_supported(get, {child_privileges, _}, private) -> true;
operation_supported(get, {eff_child_privileges, _}, private) -> true;

operation_supported(get, spaces, private) -> true;
operation_supported(get, eff_spaces, private) -> true;

operation_supported(get, eff_providers, private) -> true;

operation_supported(get, handle_services, private) -> true;
operation_supported(get, eff_handle_services, private) -> true;

operation_supported(get, handles, private) -> true;
operation_supported(get, eff_handles, private) -> true;

operation_supported(update, instance, private) -> true;
operation_supported(update, oz_privileges, private) -> true;
operation_supported(update, {user_privileges, _}, private) -> true;
operation_supported(update, {child_privileges, _}, private) -> true;

operation_supported(delete, instance, private) -> true;
operation_supported(delete, oz_privileges, private) -> true;
operation_supported(delete, {user, _}, private) -> true;
operation_supported(delete, {parent, _}, private) -> true;
operation_supported(delete, {child, _}, private) -> true;
operation_supported(delete, {space, _}, private) -> true;
operation_supported(delete, {handle_service, _}, private) -> true;
operation_supported(delete, {handle, _}, private) -> true.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(Req = #el_req{gri = #gri{id = undefined, aspect = instance} = GRI}) ->
    Name = maps:get(<<"name">>, Req#el_req.data),
    Type = maps:get(<<"type">>, Req#el_req.data, role),
    {ok, #document{key = GroupId}} = od_group:create(
        #document{value = #od_group{name = Name, type = Type}}
    ),
    case Req#el_req.auth_hint of
        ?AS_USER(UserId) ->
            entity_graph:add_relation(
                od_user, UserId,
                od_group, GroupId,
                privileges:group_admin()
            );
        ?AS_GROUP(ChildGroupId) ->
            entity_graph:add_relation(
                od_group, ChildGroupId,
                od_group, GroupId,
                privileges:group_admin()
            );
        _ ->
            ok
    end,
    % Group has been modified by adding relation, so it will need to be
    % fetched again.
    {ok, {not_fetched, GRI#gri{id = GroupId, aspect = instance, scope = private}}};

create(Req = #el_req{gri = #gri{id = undefined, aspect = join}}) ->
    Macaroon = maps:get(<<"token">>, Req#el_req.data),
    {ok, {od_group, GroupId}} = token_logic:consume(Macaroon),
    % In the future, privileges can be included in token
    Privileges = privileges:group_user(),
    case Req#el_req.auth_hint of
        ?AS_USER(UserId) ->
            entity_graph:add_relation(
                od_user, UserId,
                od_group, GroupId,
                Privileges
            );
        ?AS_GROUP(GroupId) ->
            throw(?ERROR_CANNOT_JOIN_GROUP_TO_ITSELF);
        ?AS_GROUP(ChildGroupId) ->
            entity_graph:add_relation(
                od_group, ChildGroupId,
                od_group, GroupId,
                Privileges
            );
        _ ->
            ok
    end,
    NewGRI = case lists:member(?GROUP_VIEW, Privileges) of
        true ->
            #gri{type = od_group, id = GroupId, aspect = instance, scope = private};
        false ->
            #gri{type = od_group, id = GroupId, aspect = instance, scope = protected}
    end,
    {ok, {not_fetched, NewGRI}};

create(Req = #el_req{gri = #gri{id = GrId, aspect = invite_user_token}}) ->
    {ok, Macaroon} = token_logic:create(
        Req#el_req.client,
        ?GROUP_INVITE_USER_TOKEN,
        {od_group, GrId}
    ),
    {ok, {data, Macaroon}};

create(Req = #el_req{gri = #gri{id = GrId, aspect = invite_group_token}}) ->
    {ok, Macaroon} = token_logic:create(
        Req#el_req.client,
        ?GROUP_INVITE_GROUP_TOKEN,
        {od_group, GrId}
    ),
    {ok, {data, Macaroon}};

create(#el_req{gri = #gri{id = GrId, aspect = {user, UserId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:group_user()),
    entity_graph:add_relation(
        od_user, UserId,
        od_group, GrId,
        Privileges
    ),
    NewGRI = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
    {ok, {not_fetched, NewGRI, ?THROUGH_GROUP(GrId)}};

create(#el_req{gri = #gri{id = GrId, aspect = {child, ChGrId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:group_user()),
    entity_graph:add_relation(
        od_group, ChGrId,
        od_group, GrId,
        Privileges
    ),
    NewGRI = #gri{type = od_group, id = ChGrId, aspect = instance, scope = shared},
    {ok, {not_fetched, NewGRI, ?THROUGH_GROUP(GrId)}}.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = list}}, _) ->
    {ok, GroupDocs} = od_group:list(),
    {ok, [GroupId || #document{key = GroupId} <- GroupDocs]};

get(#el_req{gri = #gri{aspect = instance, scope = private}}, Group) ->
    {ok, Group};
% Shared and protected data is (currently) the same
get(#el_req{gri = #gri{aspect = instance, scope = shared}}, Group) ->
    get(#el_req{gri = #gri{aspect = instance, scope = protected}}, Group);
get(#el_req{gri = #gri{aspect = instance, scope = protected}}, Group) ->
    #od_group{name = Name, type = Type} = Group,
    {ok, #{
        <<"name">> => Name,
        <<"type">> => Type
    }};

get(#el_req{gri = #gri{aspect = oz_privileges}}, Group) ->
    {ok, Group#od_group.oz_privileges};

get(#el_req{gri = #gri{aspect = eff_oz_privileges}}, Group) ->
    {ok, Group#od_group.eff_oz_privileges};

get(#el_req{gri = #gri{aspect = users}}, Group) ->
    {ok, maps:keys(Group#od_group.users)};
get(#el_req{gri = #gri{aspect = eff_users}}, Group) ->
    {ok, maps:keys(Group#od_group.eff_users)};
get(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, Group) ->
    {ok, maps:get(UserId, Group#od_group.users)};
get(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, Group) ->
    {Privileges, _} = maps:get(UserId, Group#od_group.eff_users),
    {ok, Privileges};

get(#el_req{gri = #gri{aspect = parents}}, Group) ->
    {ok, Group#od_group.parents};
get(#el_req{gri = #gri{aspect = eff_parents}}, Group) ->
    {ok, maps:keys(Group#od_group.eff_parents)};

get(#el_req{gri = #gri{aspect = children}}, Group) ->
    {ok, maps:keys(Group#od_group.children)};
get(#el_req{gri = #gri{aspect = eff_children}}, Group) ->
    {ok, maps:keys(Group#od_group.eff_children)};
get(#el_req{gri = #gri{aspect = {child_privileges, ChildId}}}, Group) ->
    {ok, maps:get(ChildId, Group#od_group.children)};
get(#el_req{gri = #gri{aspect = {eff_child_privileges, ChildId}}}, Group) ->
    {Privileges, _} = maps:get(ChildId, Group#od_group.eff_children),
    {ok, Privileges};

get(#el_req{gri = #gri{aspect = spaces}}, Group) ->
    {ok, Group#od_group.spaces};
get(#el_req{gri = #gri{aspect = eff_spaces}}, Group) ->
    {ok, maps:keys(Group#od_group.eff_spaces)};

get(#el_req{gri = #gri{aspect = eff_providers}}, Group) ->
    {ok, maps:keys(Group#od_group.eff_providers)};

get(#el_req{gri = #gri{aspect = handle_services}}, Group) ->
    {ok, Group#od_group.handle_services};
get(#el_req{gri = #gri{aspect = eff_handle_services}}, Group) ->
    {ok, maps:keys(Group#od_group.eff_handle_services)};

get(#el_req{gri = #gri{aspect = handles}}, Group) ->
    {ok, Group#od_group.handles};
get(#el_req{gri = #gri{aspect = eff_handles}}, Group) ->
    {ok, maps:keys(Group#od_group.eff_handles)}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = GroupId, aspect = instance}, data = Data}) ->
    {ok, _} = od_group:update(GroupId, fun(Group) ->
        #od_group{name = OldName, type = OldType} = Group,
        NewName = maps:get(<<"name">>, Data, OldName),
        NewType = maps:get(<<"type">>, Data, OldType),
        {ok, Group#od_group{name = NewName, type = NewType}}
    end),
    ok;

update(#el_req{gri = #gri{id = GroupId, aspect = oz_privileges}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_oz_privileges(od_group, GroupId, Operation, Privileges);

update(Req = #el_req{gri = #gri{id = GroupId, aspect = {user_privileges, UserId}}}) ->
    Privileges = maps:get(<<"privileges">>, Req#el_req.data),
    Operation = maps:get(<<"operation">>, Req#el_req.data, set),
    entity_graph:update_relation(
        od_user, UserId,
        od_group, GroupId,
        {Operation, Privileges}
    );

update(Req = #el_req{gri = #gri{id = ParGrId, aspect = {child_privileges, ChGrId}}}) ->
    Privileges = maps:get(<<"privileges">>, Req#el_req.data),
    Operation = maps:get(<<"operation">>, Req#el_req.data, set),
    entity_graph:update_relation(
        od_group, ChGrId,
        od_group, ParGrId,
        {Operation, Privileges}
    ).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = GroupId, aspect = instance}}) ->
    entity_graph:delete_with_relations(od_group, GroupId);

delete(#el_req{gri = #gri{id = GroupId, aspect = oz_privileges}}) ->
    update(#el_req{gri = #gri{id = GroupId, aspect = oz_privileges}, data = #{
        <<"operation">> => set, <<"privileges">> => []
    }});

delete(#el_req{gri = #gri{id = GroupId, aspect = {user, UserId}}}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_group, GroupId
    );

delete(#el_req{gri = #gri{id = ChildGroupId, aspect = {parent, ParentGroupId}}}) ->
    entity_graph:remove_relation(
        od_group, ChildGroupId,
        od_group, ParentGroupId
    );

delete(#el_req{gri = #gri{id = ParentGroupId, aspect = {child, ChildGroupId}}}) ->
    entity_graph:remove_relation(
        od_group, ChildGroupId,
        od_group, ParentGroupId
    );

delete(#el_req{gri = #gri{id = GroupId, aspect = {space, SpaceId}}}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_space, SpaceId
    );

delete(#el_req{gri = #gri{id = GroupId, aspect = {handle_service, HServiceId}}}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_handle_service, HServiceId
    );

delete(#el_req{gri = #gri{id = GroupId, aspect = {handle, HandleId}}}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_handle, HandleId
    ).


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(Req = #el_req{gri = #gri{aspect = instance, scope = protected}}, Group) ->
    case Req#el_req.auth_hint of
        ?THROUGH_USER(UserId) ->
            group_logic:has_eff_user(Group, UserId);
        ?THROUGH_GROUP(ChildGroupId) ->
            group_logic:has_eff_child(Group, ChildGroupId);
        ?THROUGH_PROVIDER(ProviderId) ->
            group_logic:has_eff_provider(Group, ProviderId);
        undefined ->
            true
    end;

exists(Req = #el_req{gri = #gri{aspect = instance, scope = shared}}, Group) ->
    case Req#el_req.auth_hint of
        ?THROUGH_GROUP(ParentGroupId) ->
            group_logic:has_eff_parent(Group, ParentGroupId);
        ?THROUGH_SPACE(SpaceId) ->
            group_logic:has_eff_space(Group, SpaceId);
        ?THROUGH_HANDLE_SERVICE(HServiceId) ->
            group_logic:has_eff_handle_service(Group, HServiceId);
        ?THROUGH_HANDLE(HandleId) ->
            group_logic:has_eff_handle(Group, HandleId);
        undefined ->
            true
    end;

exists(#el_req{gri = #gri{aspect = {user, UserId}}}, Group) ->
    maps:is_key(UserId, Group#od_group.users);

exists(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, Group) ->
    maps:is_key(UserId, Group#od_group.users);

exists(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, Group) ->
    maps:is_key(UserId, Group#od_group.eff_users);

exists(#el_req{gri = #gri{aspect = {child, ChildId}}}, Group) ->
    maps:is_key(ChildId, Group#od_group.children);

exists(#el_req{gri = #gri{aspect = {child_privileges, ChildId}}}, Group) ->
    maps:is_key(ChildId, Group#od_group.children);

exists(#el_req{gri = #gri{aspect = {eff_child_privileges, ChildId}}}, Group) ->
    maps:is_key(ChildId, Group#od_group.eff_children);

exists(#el_req{gri = #gri{aspect = {parent, ParentId}}}, Group) ->
    lists:member(ParentId, Group#od_group.parents);

exists(#el_req{gri = #gri{aspect = {space, SpaceId}}}, Group) ->
    lists:member(SpaceId, Group#od_group.spaces);

exists(#el_req{gri = #gri{aspect = {handle_service, HServiceId}}}, Group) ->
    lists:member(HServiceId, Group#od_group.handle_services);

exists(#el_req{gri = #gri{aspect = {handle, HandleId}}}, Group) ->
    lists:member(HandleId, Group#od_group.handles);

% All other aspects exist if group record exists.
exists(#el_req{gri = #gri{id = Id}}, #od_group{}) ->
    Id =/= undefined.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(Req = #el_req{operation = get, gri = #gri{aspect = oz_privileges}}, _) ->
    user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_VIEW_PRIVILEGES);
authorize(Req = #el_req{operation = get, gri = #gri{aspect = eff_oz_privileges}}, _) ->
    user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_VIEW_PRIVILEGES);
authorize(Req = #el_req{operation = update, gri = #gri{aspect = oz_privileges}}, _) ->
    user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_SET_PRIVILEGES);
authorize(Req = #el_req{operation = delete, gri = #gri{aspect = oz_privileges}}, _) ->
    user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_SET_PRIVILEGES);


authorize(Req = #el_req{operation = create, gri = #gri{aspect = instance}}, _) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            true;
        {?USER(UserId), ?AS_GROUP(ChildGroupId)} ->
            % TODO VFS-3351 ?GROUP_CREATE_GROUP
            auth_by_membership(UserId, ChildGroupId);
        _ -> false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = join}}, _) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            true;
        {?USER(UserId), ?AS_GROUP(ChildGroupId)} ->
%%            auth_by_privilege(UserId, ChildGroupId, ?GROUP_JOIN_PARENT); % TODO VFS-3351
            auth_by_privilege(UserId, ChildGroupId, ?GROUP_JOIN_GROUP);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_user_token}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_INVITE_USER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_group_token}}, Group) ->
%%    auth_by_privilege(Req, Group, ?GROUP_INVITE_CHILD); % TODO VFS-3351
    auth_by_privilege(Req, Group, ?GROUP_INVITE_GROUP);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, _}}}, _) ->
    user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_GROUPS_ADD_MEMBERS);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {child, _}}}, _) ->
    user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_GROUPS_ADD_MEMBERS);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = list}}, _) ->
    user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_GROUPS_LIST);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = instance, scope = private}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_VIEW);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = instance, scope = protected}}, Group) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?THROUGH_USER(UserId)} ->
            % User's membership in this group is checked in 'exists'
            group_logic:has_eff_privilege(Group, UserId, ?GROUP_VIEW);

        {?USER(_UserId), ?THROUGH_USER(_OtherUserId)} ->
            false;

        {?USER(UserId), ?THROUGH_GROUP(ChildGroupId)} ->
            % Child group's membership in this group is checked in 'exists'
            group_logic:has_eff_user(ChildGroupId, UserId);

        {?PROVIDER(ProviderId), ?THROUGH_PROVIDER(ProviderId)} ->
            % Group's membership in provider is checked in 'exists'
            group_logic:has_eff_provider(Group, ProviderId);

        {?PROVIDER(_ProviderId), ?THROUGH_PROVIDER(_OtherProviderId)} ->
            false;

        {?USER(ClientUserId), ?THROUGH_PROVIDER(_ProviderId)} ->
            % Group's membership in provider is checked in 'exists'
            user_logic:has_eff_oz_privilege(ClientUserId, ?OZ_PROVIDERS_LIST_GROUPS);

        {?USER(ClientUserId), _} ->
            auth_by_membership(ClientUserId, Group) orelse
                user_logic_plugin:auth_by_oz_privilege(ClientUserId, ?OZ_GROUPS_LIST);

        _ ->
            % Access to private data also allows access to protected data
            authorize(Req#el_req{gri = #gri{scope = private}}, Group)
    end;

authorize(Req = #el_req{operation = get, gri = GRI = #gri{aspect = instance, scope = shared}}, Group) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(ClientUserId), ?THROUGH_GROUP(ParentGroupId)} ->
            % Group's membership in parent group is checked in 'exists'
            group_logic:has_eff_privilege(ParentGroupId, ClientUserId, ?GROUP_VIEW) orelse
                user_logic:has_eff_oz_privilege(ClientUserId, ?OZ_GROUPS_LIST_GROUPS);

        {?USER(ClientUserId), ?THROUGH_SPACE(SpaceId)} ->
            % Group's membership in space is checked in 'exists'
            space_logic:has_eff_privilege(SpaceId, ClientUserId, ?SPACE_VIEW) orelse
                user_logic:has_eff_oz_privilege(ClientUserId, ?OZ_SPACES_LIST_GROUPS);

        {?USER(ClientUserId), ?THROUGH_HANDLE_SERVICE(HServiceId)} ->
            % Group's membership in handle_service is checked in 'exists'
            handle_service_logic:has_eff_privilege(HServiceId, ClientUserId, ?HANDLE_SERVICE_VIEW);

        {?USER(ClientUserId), ?THROUGH_HANDLE(HandleId)} ->
            % Group's membership in handle is checked in 'exists'
            handle_logic:has_eff_privilege(HandleId, ClientUserId, ?HANDLE_VIEW);

        {?USER(ClientUserId), undefined} ->
            auth_by_membership(ClientUserId, Group) orelse
                user_logic_plugin:auth_by_oz_privilege(ClientUserId, ?OZ_GROUPS_LIST);

        _ ->
            % Access to protected data also allows access to shared data
            authorize(Req#el_req{gri = GRI#gri{scope = protected}}, Group)
    end;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = users}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_VIEW) orelse
        user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_GROUPS_LIST_USERS);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = eff_users}}, Group) ->
    authorize(Req#el_req{operation = get, gri = #gri{aspect = users}}, Group);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = children}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_VIEW) orelse
        user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_GROUPS_LIST_GROUPS);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = eff_children}}, Group) ->
    authorize(Req#el_req{operation = get, gri = #gri{aspect = children}}, Group);

authorize(Req = #el_req{operation = get}, Group) ->
    % All other resources can be accessed with view privileges
    auth_by_privilege(Req, Group, ?GROUP_VIEW);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = instance}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_UPDATE);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_SET_PRIVILEGES);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {child_privileges, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_SET_PRIVILEGES);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = instance}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_DELETE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {parent, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_UPDATE);
% TODO VFS-3351 ?GROUP_LEAVE_GROUP

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {space, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_LEAVE_SPACE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {handle_service, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_UPDATE);
% TODO VFS-3351 ?GROUP_LEAVE_HANDLE_SERVICE

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {handle, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_UPDATE);
% TODO VFS-3351 ?GROUP_LEAVE_HANDLE

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {user, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_REMOVE_USER) orelse
        user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_GROUPS_REMOVE_MEMBERS);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {child, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_REMOVE_GROUP) orelse
%%    auth_by_privilege(Req, Group, ?GROUP_REMOVE_CHILD) orelse % TODO VFS-3351
        user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_GROUPS_REMOVE_MEMBERS);

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
        <<"name">> => {binary, name}
    },
    optional => #{
        <<"type">> => {atom, [organization, unit, team, role]}
    }
};

validate(Req = #el_req{operation = create, gri = #gri{aspect = join}}) ->
    TokenType = case Req#el_req.auth_hint of
        ?AS_USER(_) -> ?GROUP_INVITE_USER_TOKEN;
        ?AS_GROUP(_) -> ?GROUP_INVITE_GROUP_TOKEN
    end,
    #{
        required => #{
            <<"token">> => {token, TokenType}
        }
    };

validate(#el_req{operation = create, gri = #gri{aspect = invite_user_token}}) ->
    #{
    };

validate(#el_req{operation = create, gri = #gri{aspect = invite_group_token}}) ->
    #{
    };

validate(#el_req{operation = create, gri = #gri{aspect = {user, _}}}) -> #{
    required => #{
        {aspect, <<"userId">>} => {any, {exists, fun(UserId) ->
            user_logic:exists(UserId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:group_privileges()}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {child, _}}}) -> #{
    required => #{
        {aspect, <<"groupId">>} => {any, {exists, fun(ChildId) ->
            group_logic:exists(ChildId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:group_privileges()}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    at_least_one => #{
        <<"name">> => {binary, name},
        <<"type">> => {atom, [organization, unit, team, role]}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) ->
    #{
        required => #{
            <<"privileges">> => {list_of_atoms, privileges:group_privileges()}
        },
        optional => #{
            <<"operation">> => {atom, [set, grant, revoke]}
        }
    };

validate(#el_req{operation = update, gri = #gri{aspect = {child_privileges, Id}}}) ->
    validate(#el_req{operation = update, gri = #gri{aspect = {user_privileges, Id}}});


validate(#el_req{operation = update, gri = #gri{aspect = oz_privileges}}) -> #{
    required => #{
        <<"privileges">> => {list_of_atoms, privileges:oz_privileges()}
    },
    optional => #{
        <<"operation">> => {atom, [set, grant, revoke]}
    }
}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns if given user belongs to the group represented by entity.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_membership(od_user:id(), od_group:id() | od_group:info()) -> boolean().
auth_by_membership(UserId, GroupOrId) ->
    group_logic:has_eff_user(GroupOrId, UserId).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns if given user has specific effective privilege in the group.
%% UserId is either given explicitly or derived from entity logic request.
%% Clients of type other than user are discarded.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_privilege(entity_logic:req() | od_user:id(),
    od_group:id() | od_group:info(), privileges:group_privilege()) -> boolean().
auth_by_privilege(#el_req{client = ?USER(UserId)}, GroupOrId, Privilege) ->
    auth_by_privilege(UserId, GroupOrId, Privilege);
auth_by_privilege(#el_req{client = _OtherClient}, _GroupOrId, _Privilege) ->
    false;
auth_by_privilege(UserId, GroupOrId, Privilege) ->
    group_logic:has_eff_privilege(GroupOrId, UserId, Privilege).
