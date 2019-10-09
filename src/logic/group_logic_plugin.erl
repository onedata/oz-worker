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

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/errors.hrl").

-export([fetch_entity/1, operation_supported/3, is_subscribable/2]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, required_admin_privileges/1, validate/1]).
-export([is_authorized_to_invite/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity and its revision from datastore, if applicable.
%% Should return:
%%  * {true, entity_logic:versioned_entity()}
%%      if the fetch was successful
%%  * {true, gri:gri(), entity_logic:versioned_entity()}
%%      if the fetch was successful and new GRI was resolved
%%  * false
%%      if fetch is not applicable for this operation
%%  * {error, _}
%%      if there was an error, such as ?ERROR_NOT_FOUND
%% @end
%%--------------------------------------------------------------------
-spec fetch_entity(gri:gri()) ->
    {true, entity_logic:versioned_entity()} | false | errors:error().
fetch_entity(#gri{id = GroupId}) ->
    case od_group:get(GroupId) of
        {ok, #document{value = Group, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_utils:parse_rev(DbRev),
            {true, {Group, Revision}};
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
operation_supported(create, child, private) -> true;

operation_supported(get, list, private) -> true;
operation_supported(get, privileges, _) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, instance, protected) -> true;
operation_supported(get, instance, shared) -> true;

operation_supported(get, oz_privileges, private) -> true;
operation_supported(get, eff_oz_privileges, private) -> true;

operation_supported(get, parents, private) -> true;
operation_supported(get, eff_parents, private) -> true;

operation_supported(get, children, private) -> true;
operation_supported(get, eff_children, private) -> true;
operation_supported(get, {child_privileges, _}, private) -> true;
operation_supported(get, {eff_child_privileges, _}, private) -> true;
operation_supported(get, {eff_child_membership, _}, private) -> true;

operation_supported(get, users, private) -> true;
operation_supported(get, eff_users, private) -> true;
operation_supported(get, {user_privileges, _}, private) -> true;
operation_supported(get, {eff_user_privileges, _}, private) -> true;
operation_supported(get, {eff_user_membership, _}, private) -> true;

operation_supported(get, spaces, private) -> true;
operation_supported(get, eff_spaces, private) -> true;

operation_supported(get, eff_providers, private) -> true;

operation_supported(get, handle_services, private) -> true;
operation_supported(get, eff_handle_services, private) -> true;

operation_supported(get, handles, private) -> true;
operation_supported(get, eff_handles, private) -> true;

operation_supported(get, harvesters, private) -> true;
operation_supported(get, eff_harvesters, private) -> true;

operation_supported(get, clusters, private) -> true;
operation_supported(get, eff_clusters, private) -> true;

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
operation_supported(delete, {handle, _}, private) -> true;
operation_supported(delete, {harvester, _}, private) -> true;
operation_supported(delete, {cluster, _}, private) -> true.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given {Aspect, Scope} pair is subscribable, i.e. clients can
%% subscribe to receive updates concerning the aspect of entity.
%% @end
%%--------------------------------------------------------------------
-spec is_subscribable(entity_logic:aspect(), entity_logic:scope()) ->
    boolean().
is_subscribable(instance, _) -> true;
is_subscribable(parents, private) -> true;
is_subscribable(children, private) -> true;
is_subscribable(eff_children, private) -> true;
is_subscribable(child, private) -> true;
is_subscribable({child, _}, private) -> true;
is_subscribable({child_privileges, _}, private) -> true;
is_subscribable({eff_child_privileges, _}, private) -> true;
is_subscribable({eff_child_membership, _}, private) -> true;
is_subscribable(users, private) -> true;
is_subscribable(eff_users, private) -> true;
is_subscribable({user_privileges, _}, private) -> true;
is_subscribable({eff_user_privileges, _}, private) -> true;
is_subscribable({eff_user_membership, _}, private) -> true;
is_subscribable(spaces, private) -> true;
is_subscribable(harvesters, private) -> true;
is_subscribable(_, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(Req = #el_req{gri = #gri{id = undefined, aspect = instance} = GRI, auth = Auth}) ->
    Name = maps:get(<<"name">>, Req#el_req.data),
    Type = maps:get(<<"type">>, Req#el_req.data, ?DEFAULT_GROUP_TYPE),
    {ok, #document{key = GroupId}} = od_group:create(
        #document{value = #od_group{
            name = Name, type = Type, creator = Auth#auth.subject
        }}
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
    {true, {Group, Rev}} = fetch_entity(#gri{aspect = instance, id = GroupId}),
    {ok, resource, {GRI#gri{id = GroupId}, {Group, Rev}}};

create(Req = #el_req{auth = Auth, gri = #gri{id = undefined, aspect = join}}) ->
    Token = maps:get(<<"token">>, Req#el_req.data),
    % In the future, privileges can be included in token
    Privileges = privileges:group_member(),

    ExpType = case Req#el_req.auth_hint of
        ?AS_USER(_) -> ?GROUP_INVITE_USER_TOKEN;
        ?AS_GROUP(_) -> ?GROUP_INVITE_GROUP_TOKEN
    end,

    token_logic_plugin:consume_invite_token(Auth, Token, ExpType, fun is_authorized_to_invite/3, fun(GroupId) ->
        case Req#el_req.auth_hint of
            ?AS_USER(UserId) ->
                entity_graph:add_relation(
                    od_user, UserId,
                    od_group, GroupId,
                    Privileges
                );
            ?AS_GROUP(ChildGroupId) ->
                entity_graph:add_relation(
                    od_group, ChildGroupId,
                    od_group, GroupId,
                    Privileges
                )
        end,
        NewGRI = #gri{type = od_group, id = GroupId, aspect = instance,
            scope = case lists:member(?GROUP_VIEW, Privileges) of
                true -> private;
                false -> protected
            end
        },
        {true, {Group, Rev}} = fetch_entity(#gri{aspect = instance, id = GroupId}),
        {ok, GroupData} = get(#el_req{gri = NewGRI}, Group),
        {ok, resource, {NewGRI, {GroupData, Rev}}}
    end);

create(Req = #el_req{gri = GRI = #gri{id = ParentGroupId, aspect = child}}) ->
    % Create a new group for user/group (authHint is checked in authorize) and
    % add the group as child of the parent group.
    {ok, resource, {NewGRI = #gri{id = ChildGroupId}, _}} = create(
        Req#el_req{gri = GRI#gri{id = undefined, aspect = instance}}
    ),
    Privileges = privileges:group_member(),
    entity_graph:add_relation(
        od_group, ChildGroupId,
        od_group, ParentGroupId,
        Privileges
    ),
    {true, {Group, Rev}} = fetch_entity(#gri{aspect = instance, id = ChildGroupId}),
    {ok, resource, {NewGRI, {Group, Rev}}};

create(#el_req{auth = ?USER(UserId) = Auth, gri = #gri{id = GrId, aspect = invite_user_token}}) ->
    %% @TODO VFS-5727 move entirely to token_logic
    Result = token_logic:create_user_named_token(
        Auth, UserId, ?INVITE_TOKEN_NAME(?GROUP_INVITE_USER_TOKEN),
        ?INVITE_TOKEN(?GROUP_INVITE_USER_TOKEN, GrId), [], #{}
    ),
    case Result of
        {ok, Token} -> {ok, value, Token};
        {error, _} = Error -> Error
    end;

create(#el_req{auth = ?USER(UserId) = Auth, gri = #gri{id = GrId, aspect = invite_group_token}}) ->
    %% @TODO VFS-5727 move entirely to token_logic
    Result = token_logic:create_user_named_token(
        Auth, UserId, ?INVITE_TOKEN_NAME(?GROUP_INVITE_GROUP_TOKEN),
        ?INVITE_TOKEN(?GROUP_INVITE_GROUP_TOKEN, GrId), [], #{}
    ),
    case Result of
        {ok, Token} -> {ok, value, Token};
        {error, _} = Error -> Error
    end;

create(#el_req{gri = #gri{id = GrId, aspect = {user, UserId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:group_member()),
    entity_graph:add_relation(
        od_user, UserId,
        od_group, GrId,
        Privileges
    ),
    NewGRI = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
    {true, {User, Rev}} = user_logic_plugin:fetch_entity(#gri{id = UserId}),
    {ok, UserData} = user_logic_plugin:get(#el_req{gri = NewGRI}, User),
    {ok, resource, {NewGRI, ?THROUGH_GROUP(GrId), {UserData, Rev}}};

create(#el_req{gri = #gri{id = GrId, aspect = {child, ChGrId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:group_member()),
    entity_graph:add_relation(
        od_group, ChGrId,
        od_group, GrId,
        Privileges
    ),
    NewGRI = #gri{type = od_group, id = ChGrId, aspect = instance, scope = shared},
    {true, {ChildGroup, Rev}} = fetch_entity(#gri{aspect = instance, id = ChGrId}),
    {ok, ChildGroupData} = get(#el_req{gri = NewGRI}, ChildGroup),
    {ok, resource, {NewGRI, ?THROUGH_GROUP(GrId), {ChildGroupData, Rev}}}.


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

get(#el_req{gri = #gri{aspect = privileges}}, _) ->
    {ok, #{
        <<"member">> => privileges:group_member(),
        <<"manager">> => privileges:group_manager(),
        <<"admin">> => privileges:group_admin()
    }};

get(#el_req{gri = #gri{aspect = instance, scope = private}}, Group) ->
    {ok, Group};
get(#el_req{gri = #gri{aspect = instance, scope = protected}}, Group) ->
    #od_group{
        name = Name, type = Type, creation_time = CreationTime, creator = Creator
    } = Group,
    {ok, #{
        <<"name">> => Name,
        <<"type">> => Type,
        <<"creationTime">> => CreationTime,
        <<"creator">> => Creator
    }};
get(#el_req{gri = #gri{aspect = instance, scope = shared}}, Group) ->
    #od_group{name = Name, type = Type, creation_time = CreationTime} = Group,
    {ok, #{
        <<"name">> => Name,
        <<"type">> => Type,
        <<"creationTime">> => CreationTime
    }};

get(#el_req{gri = #gri{aspect = oz_privileges}}, Group) ->
    {ok, entity_graph:get_oz_privileges(direct, Group)};

get(#el_req{gri = #gri{aspect = eff_oz_privileges}}, Group) ->
    {ok, entity_graph:get_oz_privileges(effective, Group)};

get(#el_req{gri = #gri{aspect = parents}}, Group) ->
    {ok, entity_graph:get_relations(direct, top_down, od_group, Group)};
get(#el_req{gri = #gri{aspect = eff_parents}}, Group) ->
    {ok, entity_graph:get_relations(effective, top_down, od_group, Group)};

get(#el_req{gri = #gri{aspect = children}}, Group) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_group, Group)};
get(#el_req{gri = #gri{aspect = eff_children}}, Group) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_group, Group)};
get(#el_req{gri = #gri{aspect = {child_privileges, ChildId}}}, Group) ->
    {ok, entity_graph:get_privileges(direct, bottom_up, od_group, ChildId, Group)};
get(#el_req{gri = #gri{aspect = {eff_child_privileges, ChildId}}}, Group) ->
    {ok, entity_graph:get_privileges(effective, bottom_up, od_group, ChildId, Group)};
get(#el_req{gri = #gri{aspect = {eff_child_membership, ChildId}}}, Group) ->
    {ok, entity_graph:get_intermediaries(bottom_up, od_group, ChildId, Group)};

get(#el_req{gri = #gri{aspect = users}}, Group) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_user, Group)};
get(#el_req{gri = #gri{aspect = eff_users}}, Group) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_user, Group)};
get(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, Group) ->
    {ok, entity_graph:get_privileges(direct, bottom_up, od_user, UserId, Group)};
get(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, Group) ->
    {ok, entity_graph:get_privileges(effective, bottom_up, od_user, UserId, Group)};
get(#el_req{gri = #gri{aspect = {eff_user_membership, UserId}}}, Group) ->
    {ok, entity_graph:get_intermediaries(bottom_up, od_user, UserId, Group)};

get(#el_req{gri = #gri{aspect = spaces}}, Group) ->
    {ok, entity_graph:get_relations(direct, top_down, od_space, Group)};
get(#el_req{gri = #gri{aspect = eff_spaces}}, Group) ->
    {ok, entity_graph:get_relations(effective, top_down, od_space, Group)};

get(#el_req{gri = #gri{aspect = eff_providers}}, Group) ->
    {ok, entity_graph:get_relations(effective, top_down, od_provider, Group)};

get(#el_req{gri = #gri{aspect = handle_services}}, Group) ->
    {ok, entity_graph:get_relations(direct, top_down, od_handle_service, Group)};
get(#el_req{gri = #gri{aspect = eff_handle_services}}, Group) ->
    {ok, entity_graph:get_relations(effective, top_down, od_handle_service, Group)};

get(#el_req{gri = #gri{aspect = handles}}, Group) ->
    {ok, entity_graph:get_relations(direct, top_down, od_handle, Group)};
get(#el_req{gri = #gri{aspect = eff_handles}}, Group) ->
    {ok, entity_graph:get_relations(effective, top_down, od_handle, Group)};

get(#el_req{gri = #gri{aspect = harvesters}}, Group) ->
    {ok, entity_graph:get_relations(direct, top_down, od_harvester, Group)};
get(#el_req{gri = #gri{aspect = eff_harvesters}}, Group) ->
    {ok, entity_graph:get_relations(effective, top_down, od_harvester, Group)};

get(#el_req{gri = #gri{aspect = clusters}}, Group) ->
    {ok, entity_graph:get_relations(direct, top_down, od_cluster, Group)};
get(#el_req{gri = #gri{aspect = eff_clusters}}, Group) ->
    {ok, entity_graph:get_relations(effective, top_down, od_cluster, Group)}.


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
    PrivsToGrant = maps:get(<<"grant">>, Data, []),
    PrivsToRevoke = maps:get(<<"revoke">>, Data, []),
    entity_graph:update_oz_privileges(od_group, GroupId, PrivsToGrant, PrivsToRevoke);

update(Req = #el_req{gri = #gri{id = GroupId, aspect = {user_privileges, UserId}}}) ->
    PrivsToGrant = maps:get(<<"grant">>, Req#el_req.data, []),
    PrivsToRevoke = maps:get(<<"revoke">>, Req#el_req.data, []),
    entity_graph:update_relation(
        od_user, UserId,
        od_group, GroupId,
        {PrivsToGrant, PrivsToRevoke}
    );

update(Req = #el_req{gri = #gri{id = ParGrId, aspect = {child_privileges, ChGrId}}}) ->
    PrivsToGrant = maps:get(<<"grant">>, Req#el_req.data, []),
    PrivsToRevoke = maps:get(<<"revoke">>, Req#el_req.data, []),
    entity_graph:update_relation(
        od_group, ChGrId,
        od_group, ParGrId,
        {PrivsToGrant, PrivsToRevoke}
    ).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = GroupId, aspect = instance}}) ->
    {true, {Group, _}} = fetch_entity(#gri{aspect = instance, id = GroupId}),
    case Group#od_group.protected of
        true ->
            throw(?ERROR_PROTECTED_GROUP);
        false ->
            entity_graph:delete_with_relations(od_group, GroupId, Group)
    end;

delete(#el_req{gri = #gri{id = GroupId, aspect = oz_privileges}}) ->
    update(#el_req{gri = #gri{id = GroupId, aspect = oz_privileges}, data = #{
        <<"grant">> => [], <<"revoke">> => privileges:oz_privileges()
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
    );

delete(#el_req{gri = #gri{id = GroupId, aspect = {harvester, HarvesterId}}}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_harvester, HarvesterId
    );

delete(#el_req{gri = #gri{id = GroupId, aspect = {cluster, ClusterId}}}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_cluster, ClusterId
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
        ?THROUGH_HARVESTER(HarvesterId) ->
            group_logic:has_eff_harvester(Group, HarvesterId);
        ?THROUGH_CLUSTER(ClusterId) ->
            group_logic:has_eff_cluster(Group, ClusterId);
        undefined ->
            true
    end;

exists(#el_req{gri = #gri{aspect = {parent, ParentId}}}, Group) ->
    entity_graph:has_relation(direct, top_down, od_group, ParentId, Group);

exists(#el_req{gri = #gri{aspect = {child, ChildId}}}, Group) ->
    entity_graph:has_relation(direct, bottom_up, od_group, ChildId, Group);

exists(#el_req{gri = #gri{aspect = {child_privileges, ChildId}}}, Group) ->
    entity_graph:has_relation(direct, bottom_up, od_group, ChildId, Group);

exists(#el_req{gri = #gri{aspect = {eff_child_privileges, ChildId}}}, Group) ->
    entity_graph:has_relation(effective, bottom_up, od_group, ChildId, Group);

exists(#el_req{gri = #gri{aspect = {eff_child_membership, ChildId}}}, Group) ->
    entity_graph:has_relation(effective, bottom_up, od_group, ChildId, Group);

exists(#el_req{gri = #gri{aspect = {user, UserId}}}, Group) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, Group);

exists(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, Group) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, Group);

exists(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, Group) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Group);

exists(#el_req{gri = #gri{aspect = {eff_user_membership, UserId}}}, Group) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Group);

exists(#el_req{gri = #gri{aspect = {space, SpaceId}}}, Group) ->
    entity_graph:has_relation(direct, top_down, od_space, SpaceId, Group);

exists(#el_req{gri = #gri{aspect = {handle_service, HServiceId}}}, Group) ->
    entity_graph:has_relation(direct, top_down, od_handle_service, HServiceId, Group);

exists(#el_req{gri = #gri{aspect = {handle, HandleId}}}, Group) ->
    entity_graph:has_relation(direct, top_down, od_handle, HandleId, Group);

exists(#el_req{gri = #gri{aspect = {harvester, HarvesterId}}}, Group) ->
    entity_graph:has_relation(direct, top_down, od_harvester, HarvesterId, Group);

exists(#el_req{gri = #gri{aspect = {cluster, ClusterId}}}, Group) ->
    entity_graph:has_relation(direct, top_down, od_cluster, ClusterId, Group);

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


authorize(Req = #el_req{operation = create, gri = #gri{id = undefined, aspect = instance}}, _) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            true;
        {?USER(UserId), ?AS_GROUP(ChildGroupId)} ->
            auth_by_privilege(UserId, ChildGroupId, ?GROUP_ADD_PARENT);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = join}}, _) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            true;
        {?USER(UserId), ?AS_GROUP(ChildGroupId)} ->
            auth_by_privilege(UserId, ChildGroupId, ?GROUP_ADD_PARENT);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, UserId}}, auth = ?USER(UserId), data = #{<<"privileges">> := _}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_ADD_USER) andalso auth_by_privilege(Req, Group, ?GROUP_SET_PRIVILEGES);
authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, UserId}}, auth = ?USER(UserId), data = _}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_ADD_USER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {child, ChildId}}, auth = ?USER(UserId), data = #{<<"privileges">> := _}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_ADD_CHILD) andalso
        auth_by_privilege(Req, Group, ?GROUP_SET_PRIVILEGES) andalso
        group_logic:has_eff_privilege(ChildId, UserId, ?GROUP_ADD_PARENT);
authorize(Req = #el_req{operation = create, gri = #gri{aspect = {child, ChildId}}, auth = ?USER(UserId), data = _}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_ADD_CHILD) andalso
        group_logic:has_eff_privilege(ChildId, UserId, ?GROUP_ADD_PARENT);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = child}}, Group) ->
    % A child group can only be created as a user or another group
    auth_by_privilege(Req, Group, ?GROUP_ADD_CHILD) andalso Req#el_req.auth_hint /= undefined;


authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_user_token}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_ADD_USER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_group_token}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_ADD_CHILD);

authorize(#el_req{operation = get, gri = #gri{aspect = privileges}}, _) ->
    true;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = instance, scope = private}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_VIEW);

authorize(Req = #el_req{operation = get, gri = GRI = #gri{aspect = instance, scope = protected}}, Group) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
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

        {?USER(ClientUserId), ?THROUGH_PROVIDER(ProviderId)} ->
            % Group's membership in provider is checked in 'exists'
            ClusterId = ProviderId,
            cluster_logic:has_eff_privilege(ClusterId, ClientUserId, ?CLUSTER_VIEW);

        {?USER(ClientUserId), _} ->
            auth_by_membership(ClientUserId, Group);

        _ ->
            % Access to private data also allows access to protected data
            authorize(Req#el_req{gri = GRI#gri{scope = private}}, Group)
    end;

authorize(Req = #el_req{operation = get, gri = GRI = #gri{aspect = instance, scope = shared}}, Group) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(ClientUserId), ?THROUGH_GROUP(ParentGroupId)} ->
            % Group's membership in parent group is checked in 'exists'
            group_logic:has_eff_privilege(ParentGroupId, ClientUserId, ?GROUP_VIEW);

        {?USER(ClientUserId), ?THROUGH_SPACE(SpaceId)} ->
            % Group's membership in space is checked in 'exists'
            space_logic:has_eff_privilege(SpaceId, ClientUserId, ?SPACE_VIEW);

        {?USER(ClientUserId), ?THROUGH_HANDLE_SERVICE(HServiceId)} ->
            % Group's membership in handle_service is checked in 'exists'
            handle_service_logic:has_eff_privilege(HServiceId, ClientUserId, ?HANDLE_SERVICE_VIEW);

        {?USER(ClientUserId), ?THROUGH_HANDLE(HandleId)} ->
            % Group's membership in handle is checked in 'exists'
            handle_logic:has_eff_privilege(HandleId, ClientUserId, ?HANDLE_VIEW);

        {?USER(ClientUserId), ?THROUGH_HARVESTER(HarvesterId)} ->
            % Group's membership in harvester is checked in 'exists'
            harvester_logic:has_eff_privilege(HarvesterId, ClientUserId, ?HARVESTER_VIEW);

        {?USER(ClientUserId), ?THROUGH_CLUSTER(ClusterId)} ->
            % Group's membership in cluster is checked in 'exists'
            cluster_logic:has_eff_privilege(ClusterId, ClientUserId, ?CLUSTER_VIEW);

        {?USER(ClientUserId), undefined} ->
            auth_by_membership(ClientUserId, Group);

        {?PROVIDER(ProviderId), ?THROUGH_CLUSTER(ClusterId)} ->
            cluster_logic:is_provider_cluster(ClusterId, ProviderId);

        _ ->
            % Access to protected data also allows access to shared data
            authorize(Req#el_req{gri = GRI#gri{scope = protected}}, Group)
    end;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = children}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_VIEW);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = eff_children}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_VIEW);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {child_privileges, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_VIEW_PRIVILEGES);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_child_privileges, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_VIEW_PRIVILEGES);

authorize(Req = #el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {eff_child_membership, GroupId}}}, Group) ->
    group_logic:has_eff_user(GroupId, UserId) orelse auth_by_privilege(Req, Group, ?GROUP_VIEW);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = users}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_VIEW);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = eff_users}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_VIEW);

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {user_privileges, UserId}}}, _) ->
    true;
authorize(Req = #el_req{operation = get, gri = #gri{aspect = {user_privileges, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_VIEW_PRIVILEGES);

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {eff_user_privileges, UserId}}}, _) ->
    true;
authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_user_privileges, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_VIEW_PRIVILEGES);

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {eff_user_membership, UserId}}}, _) ->
    true;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_user_membership, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_VIEW);

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
    auth_by_privilege(Req, Group, ?GROUP_LEAVE_PARENT);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {space, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_LEAVE_SPACE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {handle_service, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_LEAVE_HANDLE_SERVICE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {handle, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_LEAVE_HANDLE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {harvester, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_REMOVE_HARVESTER);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {cluster, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_LEAVE_CLUSTER);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {user, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_REMOVE_USER);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {child, _}}}, Group) ->
    auth_by_privilege(Req, Group, ?GROUP_REMOVE_CHILD);

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
        ?AS_USER(_) -> [?OZ_GROUPS_CREATE, ?OZ_USERS_ADD_RELATIONSHIPS];
        ?AS_GROUP(_) -> [?OZ_GROUPS_CREATE, ?OZ_GROUPS_ADD_RELATIONSHIPS];
        _ -> [?OZ_GROUPS_CREATE]
    end;

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = invite_user_token}}) ->
    [?OZ_GROUPS_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = invite_group_token}}) ->
    [?OZ_GROUPS_ADD_RELATIONSHIPS];

required_admin_privileges(Req = #el_req{operation = create, gri = #gri{aspect = join}}) ->
    case Req#el_req.auth_hint of
        ?AS_USER(_) -> [?OZ_USERS_ADD_RELATIONSHIPS];
        ?AS_GROUP(_) -> [?OZ_GROUPS_ADD_RELATIONSHIPS]
    end;

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {user, _}}, data = #{<<"privileges">> := _}}) ->
    [?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_GROUPS_SET_PRIVILEGES, ?OZ_USERS_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {user, _}}, data = _}) ->
    [?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {child, _}}, data = #{<<"privileges">> := _}}) ->
    [?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_GROUPS_SET_PRIVILEGES];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {child, _}}, data = _}) ->
    [?OZ_GROUPS_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = child}}) ->
    [?OZ_GROUPS_CREATE, ?OZ_GROUPS_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = list}}) ->
    [?OZ_GROUPS_LIST];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance, scope = protected}}) ->
    [?OZ_GROUPS_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance, scope = shared}}) ->
    [?OZ_GROUPS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = parents}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_parents}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = children}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_children}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {child_privileges, _}}}) ->
    [?OZ_GROUPS_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_child_privileges, _}}}) ->
    [?OZ_GROUPS_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_child_membership, _}}}) ->
    [?OZ_GROUPS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = users}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_users}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {user_privileges, _}}}) ->
    [?OZ_GROUPS_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_user_privileges, _}}}) ->
    [?OZ_GROUPS_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_user_membership, _}}}) ->
    [?OZ_GROUPS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = spaces}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_spaces}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_providers}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = handle_services}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_handle_services}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = handles}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_handles}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = harvesters}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_harvesters}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = clusters}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_clusters}}) ->
    [?OZ_GROUPS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = instance}}) ->
    [?OZ_GROUPS_UPDATE];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) ->
    [?OZ_GROUPS_SET_PRIVILEGES];
required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {child_privileges, _}}}) ->
    [?OZ_GROUPS_SET_PRIVILEGES];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = instance}}) ->
    [?OZ_GROUPS_DELETE];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {user, _}}}) ->
    [?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {child, _}}}) ->
    [?OZ_GROUPS_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {parent, _}}}) ->
    [?OZ_GROUPS_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {space, _}}}) ->
    [?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_SPACES_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {handle_service, _}}}) ->
    [?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_HANDLE_SERVICES_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {handle, _}}}) ->
    [?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_HANDLES_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {harvester, _}}}) ->
    [?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_HARVESTERS_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {cluster, _}}}) ->
    [?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_CLUSTERS_REMOVE_RELATIONSHIPS];

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
        <<"name">> => {binary, name}
    },
    optional => #{
        <<"type">> => {atom, [organization, unit, team, role_holders]}
    }
};

validate(Req = #el_req{operation = create, gri = #gri{aspect = join}}) ->
    #{
        required => #{
            <<"token">> => {invite_token, case Req#el_req.auth_hint of
                ?AS_USER(_) -> ?GROUP_INVITE_USER_TOKEN;
                ?AS_GROUP(_) -> ?GROUP_INVITE_GROUP_TOKEN
            end}
        }
    };

validate(Req = #el_req{operation = create, gri = GRI = #gri{aspect = child}}) ->
    validate(Req#el_req{gri = GRI#gri{aspect = instance}});

validate(#el_req{operation = create, gri = #gri{aspect = invite_user_token}}) -> #{
};

validate(#el_req{operation = create, gri = #gri{aspect = invite_group_token}}) -> #{
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
        <<"type">> => {atom, [organization, unit, team, role_holders]}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) -> #{
    at_least_one => #{
        <<"grant">> => {list_of_atoms, privileges:group_privileges()},
        <<"revoke">> => {list_of_atoms, privileges:group_privileges()}
    }
};

validate(Req = #el_req{operation = update, gri = GRI = #gri{aspect = {child_privileges, Id}}}) ->
    validate(Req#el_req{operation = update, gri = GRI#gri{aspect = {user_privileges, Id}}});


validate(#el_req{operation = update, gri = #gri{aspect = oz_privileges}}) -> #{
    at_least_one => #{
        <<"grant">> => {list_of_atoms, privileges:oz_privileges()},
        <<"revoke">> => {list_of_atoms, privileges:oz_privileges()}
    }
}.


-spec is_authorized_to_invite(aai:auth(), tokens:invite_token_type(), od_group:id()) ->
    boolean().
is_authorized_to_invite(?USER(UserId), ?GROUP_INVITE_USER_TOKEN, GroupId) ->
    auth_by_privilege(UserId, GroupId, ?GROUP_ADD_USER) orelse
        user_logic:has_eff_oz_privilege(UserId, ?OZ_GROUPS_ADD_RELATIONSHIPS);
is_authorized_to_invite(?USER(UserId), ?GROUP_INVITE_GROUP_TOKEN, GroupId) ->
    auth_by_privilege(UserId, GroupId, ?GROUP_ADD_CHILD) orelse
        user_logic:has_eff_oz_privilege(UserId, ?OZ_GROUPS_ADD_RELATIONSHIPS);
is_authorized_to_invite(_, _, _) ->
    false.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns if given user belongs to the group represented by entity.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_membership(od_user:id(), od_group:id() | od_group:record()) -> boolean().
auth_by_membership(UserId, GroupOrId) ->
    group_logic:has_eff_user(GroupOrId, UserId).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns if given user has specific effective privilege in the group.
%% UserId is either given explicitly or derived from entity logic request.
%% Auths of type other than user are discarded.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_privilege(entity_logic:req() | od_user:id(),
    od_group:id() | od_group:record(), privileges:group_privilege()) -> boolean().
auth_by_privilege(#el_req{auth = ?USER(UserId)}, GroupOrId, Privilege) ->
    auth_by_privilege(UserId, GroupOrId, Privilege);
auth_by_privilege(#el_req{auth = _OtherAuth}, _GroupOrId, _Privilege) ->
    false;
auth_by_privilege(UserId, GroupOrId, Privilege) ->
    group_logic:has_eff_privilege(GroupOrId, UserId, Privilege).
