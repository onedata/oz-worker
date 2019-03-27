%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_cluster model.
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("tokens.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
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
%% Retrieves an entity from datastore based on its EntityId.
%% Should return ?ERROR_NOT_FOUND if the entity does not exist.
%% @end
%%--------------------------------------------------------------------
-spec fetch_entity(entity_logic:entity_id()) ->
    {ok, entity_logic:entity()} | entity_logic:error().
fetch_entity(ClusterId) ->
    case od_cluster:get(ClusterId) of
        {ok, #document{value = Cluster}} ->
            {ok, Cluster};
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

operation_supported(create, join, private) -> true;

operation_supported(create, {user, _}, private) -> true;
operation_supported(create, {group, _}, private) -> true;
operation_supported(create, group, private) -> true;

operation_supported(get, list, private) -> true;

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

operation_supported(update, instance, private) -> true;
operation_supported(update, {user_privileges, _}, private) -> true;
operation_supported(update, {group_privileges, _}, private) -> true;

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
is_subscribable(_, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(Req = #el_req{gri = #gri{id = undefined, aspect = join}}) ->
    Macaroon = maps:get(<<"token">>, Req#el_req.data),
    Privileges = privileges:cluster_user(),
    JoinClusterFun = fun(od_cluster, ClusterId) ->
        case Req#el_req.auth_hint of
            ?AS_USER(UserId) ->
                entity_graph:add_relation(
                    od_user, UserId,
                    od_cluster, ClusterId,
                    Privileges
                );
            ?AS_GROUP(GroupId) ->
                entity_graph:add_relation(
                    od_group, GroupId,
                    od_cluster, ClusterId,
                    Privileges
                );
            _ ->
                ok
        end,
        ClusterId
    end,
    ClusterId = token_logic:consume(Macaroon, JoinClusterFun),

    NewGRI = #gri{type = od_cluster, id = ClusterId, aspect = instance,
        scope = case lists:member(?CLUSTER_VIEW, Privileges) of
            true -> private;
            false -> protected
        end
    },
    {ok, Cluster} = fetch_entity(ClusterId),
    {ok, ClusterData} = get(#el_req{gri = NewGRI}, Cluster),
    {ok, resource, {NewGRI, ClusterData}};

create(Req = #el_req{gri = #gri{id = ClusterId, aspect = invite_user_token}}) ->
    {ok, Macaroon} = token_logic:create(
        Req#el_req.client,
        ?CLUSTER_INVITE_USER_TOKEN,
        {od_cluster, ClusterId}
    ),
    {ok, value, Macaroon};

create(Req = #el_req{gri = #gri{id = ClusterId, aspect = invite_group_token}}) ->
    {ok, Macaroon} = token_logic:create(
        Req#el_req.client,
        ?CLUSTER_INVITE_GROUP_TOKEN,
        {od_cluster, ClusterId}
    ),
    {ok, value, Macaroon};

create(#el_req{gri = #gri{id = ClusterId, aspect = {user, UserId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:cluster_user()),
    entity_graph:add_relation(
        od_user, UserId,
        od_cluster, ClusterId,
        Privileges
    ),
    NewGRI = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
    {ok, User} = user_logic_plugin:fetch_entity(UserId),
    {ok, UserData} = user_logic_plugin:get(#el_req{gri = NewGRI}, User),
    {ok, resource, {NewGRI, ?THROUGH_CLUSTER(ClusterId), UserData}};

create(Req = #el_req{gri = GRI = #gri{id = ClusterId, aspect = group}}) ->
    % Create a new group for the requesting client and add the group as a member
    % of this cluster.
    {ok, resource, {NewGRI = #gri{id = GroupId}, _}} = group_logic_plugin:create(
        Req#el_req{gri = GRI#gri{type = od_group, id = undefined, aspect = instance}}
    ),
    Privileges = privileges:cluster_user(),
    entity_graph:add_relation(
        od_group, GroupId,
        od_cluster, ClusterId,
        Privileges
    ),
    {ok, Group} = group_logic_plugin:fetch_entity(GroupId),
    {ok, resource, {NewGRI, Group}};

create(#el_req{gri = #gri{id = ClusterId, aspect = {group, GroupId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:cluster_user()),
    entity_graph:add_relation(
        od_group, GroupId,
        od_cluster, ClusterId,
        Privileges
    ),
    NewGRI = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
    {ok, Group} = group_logic_plugin:fetch_entity(GroupId),
    {ok, GroupData} = group_logic_plugin:get(#el_req{gri = NewGRI}, Group),
    {ok, resource, {NewGRI, ?THROUGH_GROUP(ClusterId), GroupData}}.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = list}}, _) ->
    {ok, ClusterDocs} = od_cluster:list(),
    {ok, [ClusterId || #document{key = ClusterId} <- ClusterDocs]};

get(#el_req{gri = #gri{aspect = instance, scope = private}}, Cluster) ->
    {ok, Cluster};
get(#el_req{gri = #gri{aspect = instance, scope = protected}}, Cluster) ->
    #od_cluster{
        type = Type,
        service_id = ServiceId,
        worker_version = WorkerVersion,
        onepanel_version = OnepanelVersion,
        onepanel_proxy = OnepanelProxy,
        creation_time = CreationTime,
        creator = Creator
    } = Cluster,

    {ok, #{
        <<"type">> => Type,
        <<"serviceId">> => ServiceId,
        <<"workerVersion">> => cluster_logic:version_info_to_json(WorkerVersion),
        <<"onepanelVersion">> => cluster_logic:version_info_to_json(OnepanelVersion),
        <<"onepanelProxy">> => OnepanelProxy,
        <<"creationTime">> => CreationTime,
        <<"creator">> => Creator
    }};

get(#el_req{gri = #gri{aspect = users}}, Cluster) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_user, Cluster)};
get(#el_req{gri = #gri{aspect = eff_users}}, Cluster) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_user, Cluster)};
get(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, Cluster) ->
    {ok, entity_graph:get_privileges(direct, bottom_up, od_user, UserId, Cluster)};
get(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, Cluster) ->
    {ok, entity_graph:get_privileges(effective, bottom_up, od_user, UserId, Cluster)};
get(#el_req{gri = #gri{aspect = {eff_user_membership, UserId}}}, Cluster) ->
    {ok, entity_graph:get_intermediaries(bottom_up, od_user, UserId, Cluster)};

get(#el_req{gri = #gri{aspect = groups}}, Cluster) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_group, Cluster)};
get(#el_req{gri = #gri{aspect = eff_groups}}, Cluster) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_group, Cluster)};
get(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, Cluster) ->
    {ok, entity_graph:get_privileges(direct, bottom_up, od_group, GroupId, Cluster)};
get(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, Cluster) ->
    {ok, entity_graph:get_privileges(effective, bottom_up, od_group, GroupId, Cluster)};
get(#el_req{gri = #gri{aspect = {eff_group_membership, GroupId}}}, Cluster) ->
    {ok, entity_graph:get_intermediaries(bottom_up, od_group, GroupId, Cluster)}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = ClusterId, aspect = instance}, data = Data}) ->
    update_cluster(ClusterId, Data);

update(Req = #el_req{gri = #gri{id = ClusterId, aspect = {user_privileges, UserId}}}) ->
    PrivsToGrant = maps:get(<<"grant">>, Req#el_req.data, []),
    PrivsToRevoke = maps:get(<<"revoke">>, Req#el_req.data, []),
    entity_graph:update_relation(
        od_user, UserId,
        od_cluster, ClusterId,
        {PrivsToGrant, PrivsToRevoke}
    );

update(Req = #el_req{gri = #gri{id = ClusterId, aspect = {group_privileges, GroupId}}}) ->
    PrivsToGrant = maps:get(<<"grant">>, Req#el_req.data, []),
    PrivsToRevoke = maps:get(<<"revoke">>, Req#el_req.data, []),
    entity_graph:update_relation(
        od_group, GroupId,
        od_cluster, ClusterId,
        {PrivsToGrant, PrivsToRevoke}
    ).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = ClusterId, aspect = {user, UserId}}}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_cluster, ClusterId
    );

delete(#el_req{gri = #gri{id = ClusterId, aspect = {group, GroupId}}}) ->
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
exists(Req = #el_req{gri = #gri{aspect = instance, scope = protected}}, Cluster) ->
    case Req#el_req.auth_hint of
        ?THROUGH_USER(UserId) ->
            cluster_logic:has_eff_user(Cluster, UserId);
        ?THROUGH_GROUP(GroupId) ->
            cluster_logic:has_eff_group(Cluster, GroupId);
        undefined ->
            true
    end;

exists(#el_req{gri = #gri{aspect = {user, UserId}}}, Cluster) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, Cluster);

exists(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, Cluster) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, Cluster);

exists(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, Cluster) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Cluster);

exists(#el_req{gri = #gri{aspect = {eff_user_membership, UserId}}}, Cluster) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Cluster);

exists(#el_req{gri = #gri{aspect = {group, GroupId}}}, Cluster) ->
    entity_graph:has_relation(direct, bottom_up, od_group, GroupId, Cluster);

exists(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, Cluster) ->
    entity_graph:has_relation(direct, bottom_up, od_group, GroupId, Cluster);

exists(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, Cluster) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, Cluster);

exists(#el_req{gri = #gri{aspect = {eff_group_membership, GroupId}}}, Cluster) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, Cluster);

% All other aspects exist if cluster record exists.
exists(#el_req{gri = #gri{id = Id}}, #od_cluster{}) ->
    Id =/= undefined.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(Req = #el_req{operation = create, gri = #gri{aspect = join}}, _) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            true;
        {?USER(UserId), ?AS_GROUP(GroupId)} ->
            group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_CLUSTER);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, UserId}}, client = ?USER(UserId), data = #{<<"privileges">> := _}}, Cluster) ->
    auth_by_privilege(Req, Cluster, ?CLUSTER_ADD_USER) andalso auth_by_privilege(Req, Cluster, ?CLUSTER_SET_PRIVILEGES);
authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, UserId}}, client = ?USER(UserId), data = _}, Cluster) ->
    auth_by_privilege(Req, Cluster, ?CLUSTER_ADD_USER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {group, GroupId}}, client = ?USER(UserId), data = #{<<"privileges">> := _}}, Cluster) ->
    auth_by_privilege(Req, Cluster, ?CLUSTER_ADD_GROUP) andalso
        auth_by_privilege(Req, Cluster, ?CLUSTER_SET_PRIVILEGES) andalso
        group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_CLUSTER);
authorize(Req = #el_req{operation = create, gri = #gri{aspect = {group, GroupId}}, client = ?USER(UserId), data = _}, Cluster) ->
    auth_by_privilege(Req, Cluster, ?CLUSTER_ADD_GROUP) andalso
        group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_CLUSTER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = group}}, Cluster) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            auth_by_privilege(Req, Cluster, ?CLUSTER_ADD_GROUP);
        _ ->
            false
    end;

% A provider can perform all operations within its cluster
authorize(#el_req{client = ?PROVIDER(ProviderId)}, Cluster) ->
    cluster_logic:is_linked_to_provider(Cluster, ProviderId);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_user_token}}, Cluster) ->
    auth_by_privilege(Req, Cluster, ?CLUSTER_ADD_USER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_group_token}}, Cluster) ->
    auth_by_privilege(Req, Cluster, ?CLUSTER_ADD_GROUP);

authorize(#el_req{client = ?USER(UserId), operation = get, gri = #gri{aspect = instance, scope = private}}, Cluster) ->
    auth_by_privilege(UserId, Cluster, ?CLUSTER_VIEW);

authorize(Req = #el_req{operation = get, gri = GRI = #gri{aspect = instance, scope = protected}}, Cluster) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?THROUGH_USER(UserId)} ->
            % User's membership in this cluster is checked in 'exists'
            true;

        {?USER(_UserId), ?THROUGH_USER(_OtherUserId)} ->
            false;

        {?USER(ClientUserId), ?THROUGH_GROUP(GroupId)} ->
            % Groups's membership in this cluster is checked in 'exists'
            group_logic:has_eff_privilege(GroupId, ClientUserId, ?GROUP_VIEW);

        {?USER(ClientUserId), _} ->
            cluster_logic:has_eff_user(Cluster, ClientUserId) orelse begin
                case Cluster#od_cluster.type of
                    ?ONEPROVIDER -> provider_logic:has_eff_user(Cluster#od_cluster.service_id, ClientUserId);
                    ?ONEZONE -> false
                end
            end;

        _ ->
            % Access to private data also allows access to protected data
            authorize(Req#el_req{gri = GRI#gri{scope = private}}, Cluster)
    end;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {user_privileges, _}}}, Cluster) ->
    auth_by_privilege(Req, Cluster, ?CLUSTER_VIEW_PRIVILEGES);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_user_privileges, _}}}, Cluster) ->
    auth_by_privilege(Req, Cluster, ?CLUSTER_VIEW_PRIVILEGES);

authorize(#el_req{operation = get, client = ?USER(UserId), gri = #gri{aspect = {eff_user_membership, UserId}}}, _) ->
    true;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_user_membership, _}}}, Cluster) ->
    auth_by_privilege(Req, Cluster, ?CLUSTER_VIEW);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {group_privileges, _}}}, Cluster) ->
    auth_by_privilege(Req, Cluster, ?CLUSTER_VIEW_PRIVILEGES);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_group_privileges, _}}}, Cluster) ->
    auth_by_privilege(Req, Cluster, ?CLUSTER_VIEW_PRIVILEGES);

authorize(Req = #el_req{operation = get, client = ?USER(UserId), gri = #gri{aspect = {eff_group_membership, GroupId}}}, Cluster) ->
    group_logic:has_eff_user(GroupId, UserId) orelse auth_by_privilege(Req, Cluster, ?CLUSTER_VIEW);

authorize(Req = #el_req{operation = get, client = ?USER}, Cluster) ->
    % All other resources can be accessed with view privileges
    auth_by_privilege(Req, Cluster, ?CLUSTER_VIEW);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = instance}}, Cluster) ->
    case Req#el_req.client of
        ?USER(UserId) ->
            auth_by_privilege(UserId, Cluster, ?CLUSTER_UPDATE);
        ?PROVIDER(ProviderId) ->
            cluster_logic:is_linked_to_provider(Cluster, ProviderId)
    end;

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}, Cluster) ->
    auth_by_privilege(Req, Cluster, ?CLUSTER_SET_PRIVILEGES);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {group_privileges, _}}}, Cluster) ->
    auth_by_privilege(Req, Cluster, ?CLUSTER_SET_PRIVILEGES);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {user, _}}}, Cluster) ->
    auth_by_privilege(Req, Cluster, ?CLUSTER_REMOVE_USER);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {group, _}}}, Cluster) ->
    auth_by_privilege(Req, Cluster, ?CLUSTER_REMOVE_GROUP);

authorize(_, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc
%% Returns list of admin privileges needed to perform given operation.
%% @end
%%--------------------------------------------------------------------
-spec required_admin_privileges(entity_logic:req()) -> [privileges:oz_privilege()] | forbidden.
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = invite_user_token}}) ->
    [?OZ_CLUSTERS_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = invite_group_token}}) ->
    [?OZ_CLUSTERS_ADD_RELATIONSHIPS];

required_admin_privileges(Req = #el_req{operation = create, gri = #gri{aspect = join}}) ->
    case Req#el_req.auth_hint of
        ?AS_USER(_) -> [?OZ_USERS_ADD_RELATIONSHIPS];
        ?AS_GROUP(_) -> [?OZ_GROUPS_ADD_RELATIONSHIPS]
    end;

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {user, _}}, data = #{<<"privileges">> := _}}) ->
    [?OZ_CLUSTERS_ADD_RELATIONSHIPS, ?OZ_CLUSTERS_SET_PRIVILEGES, ?OZ_USERS_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {user, _}}, data = _}) ->
    [?OZ_CLUSTERS_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {group, _}}, data = #{<<"privileges">> := _}}) ->
    [?OZ_CLUSTERS_ADD_RELATIONSHIPS, ?OZ_CLUSTERS_SET_PRIVILEGES, ?OZ_GROUPS_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {group, _}}, data = _}) ->
    [?OZ_CLUSTERS_ADD_RELATIONSHIPS, ?OZ_GROUPS_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = group}}) ->
    [?OZ_GROUPS_CREATE, ?OZ_CLUSTERS_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = list}}) ->
    [?OZ_CLUSTERS_LIST];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance, scope = protected}}) ->
    [?OZ_CLUSTERS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {user_privileges, _}}}) ->
    [?OZ_CLUSTERS_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_user_privileges, _}}}) ->
    [?OZ_CLUSTERS_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_user_membership, _}}}) ->
    [?OZ_CLUSTERS_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {group_privileges, _}}}) ->
    [?OZ_CLUSTERS_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_group_privileges, _}}}) ->
    [?OZ_CLUSTERS_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_group_membership, _}}}) ->
    [?OZ_CLUSTERS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = users}}) ->
    [?OZ_CLUSTERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_users}}) ->
    [?OZ_CLUSTERS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = groups}}) ->
    [?OZ_CLUSTERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_groups}}) ->
    [?OZ_CLUSTERS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = instance}}) ->
    [?OZ_CLUSTERS_UPDATE];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) ->
    [?OZ_CLUSTERS_SET_PRIVILEGES];
required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {group_privileges, _}}}) ->
    [?OZ_CLUSTERS_SET_PRIVILEGES];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {user, _}}}) ->
    [?OZ_CLUSTERS_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {group, _}}}) ->
    [?OZ_CLUSTERS_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS];

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
validate(Req = #el_req{operation = create, gri = #gri{aspect = join}}) ->
    TokenType = case Req#el_req.auth_hint of
        ?AS_USER(_) -> ?CLUSTER_INVITE_USER_TOKEN;
        ?AS_GROUP(_) -> ?CLUSTER_INVITE_GROUP_TOKEN
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
        <<"privileges">> => {list_of_atoms, privileges:cluster_privileges()}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {group, _}}}) -> #{
    required => #{
        {aspect, <<"groupId">>} => {any, {exists, fun(GroupId) ->
            group_logic:exists(GroupId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:cluster_privileges()}
    }
};

validate(Req = #el_req{operation = create, gri = #gri{aspect = group}}) ->
    group_logic_plugin:validate(Req#el_req{gri = #gri{
        type = od_group, id = undefined, aspect = instance
    }});

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) ->
    VersionValidator = {json, #{
        <<"release">> => {binary, non_empty},
        <<"build">> => {binary, non_empty},
        <<"gui">> => {binary, non_empty}
    }},
    #{
        at_least_one => #{
            <<"workerVersion">> => VersionValidator,
            <<"onepanelVersion">> => VersionValidator,
            <<"onepanelProxy">> => {boolean, any}
        }
    };

validate(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) ->
    #{
        at_least_one => #{
            <<"grant">> => {list_of_atoms, privileges:cluster_privileges()},
            <<"revoke">> => {list_of_atoms, privileges:cluster_privileges()}
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
%% Returns if given user has specific effective privilege in the cluster.
%% UserId is either given explicitly or derived from entity logic request.
%% Clients of type other than user are discarded.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_privilege(entity_logic:req() | od_user:id(),
    od_cluster:id() | od_cluster:info(), privileges:cluster_privilege()) -> boolean().
auth_by_privilege(#el_req{client = ?USER(UserId)}, ClusterOrId, Privilege) ->
    auth_by_privilege(UserId, ClusterOrId, Privilege);
auth_by_privilege(#el_req{client = _OtherClient}, _ClusterOrId, _Privilege) ->
    false;
auth_by_privilege(UserId, ClusterOrId, Privilege) ->
    cluster_logic:has_eff_privilege(ClusterOrId, UserId, Privilege).


%% @private
-spec update_cluster(od_cluster:id(), entity_logic:data()) -> ok | {error, term()}.
update_cluster(ClusterId, Data) ->
    Result = od_cluster:update(ClusterId, fun(Cluster) -> try
        Cluster2 = update_version_info(Cluster, ClusterId, ?WORKER, <<"workerVersion">>, Data),
        Cluster3 = update_version_info(Cluster2, ClusterId, ?ONEPANEL, <<"onepanelVersion">>, Data),
        Cluster4 = update_onepanel_proxy(Cluster3, Data),
        {ok, Cluster4}
    catch throw:Error ->
        Error
    end end),

    UpgradedCluster = case Result of
        {error, _} = Error -> throw(Error);
        {ok, #document{value = Cluster}} -> Cluster
    end,

    % If update of a version info was requested and the GUI has was reset to empty,
    % it indicates that the version was invalid. Release and build versions are still
    % updated and error is returned to the client. Applies to Oneprovider cluster only.
    case {Data, UpgradedCluster} of
        {#{<<"workerVersion">> := _}, #od_cluster{worker_version = {_, _, ?EMPTY_GUI_HASH}}} ->
            throw(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"workerVersion.gui">>));
        {#{<<"onepanelVersion">> := _}, #od_cluster{onepanel_version = {_, _, ?EMPTY_GUI_HASH}}} ->
            throw(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"onepanelVersion.gui">>));
        _ ->
            ok
    end.


%% @private
-spec update_version_info(od_cluster:record(), od_cluster:id(), onedata:service_type(),
    VersionKey :: binary(), entity_logic:data()) -> od_cluster:record().
update_version_info(Cluster, ClusterId, ServiceType, VersionKey, Data) ->
    case maps:find(VersionKey, Data) of
        error ->
            Cluster;
        {ok, VersionData} ->
            ClusterType = Cluster#od_cluster.type,
            Service = onedata:service_by_type(ClusterType, ServiceType),
            {Release, Build, RequestedGuiHash} = cluster_logic:json_to_version_info(VersionData),
            GuiExists = gui_static:gui_exists(Service, RequestedGuiHash),
            GuiHash = case {GuiExists, ClusterType} of
                {true, _} ->
                    RequestedGuiHash;
                {false, ?ONEPROVIDER} ->
                    % Providing a bad GUI hash still causes an update in Oneprovider cluster
                    % and sets the GUI to empty
                    ?EMPTY_GUI_HASH;
                {false, ?ONEZONE} ->
                    % Though in case of Onezone cluster, the whole operation fails.
                    throw(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<VersionKey/binary, ".gui">>))
            end,
            {_, _, OldGuiHash} = get_version_info(Cluster, ServiceType),
            GuiHash /= OldGuiHash andalso gui_static:link_service_gui(Service, ClusterId, GuiHash),
            set_version_info(Cluster, ServiceType, {Release, Build, GuiHash})
    end.


%% @private
-spec set_version_info(od_cluster:record(), onedata:service_type(), od_cluster:version_info()) ->
    od_cluster:record().
set_version_info(Cluster, ?WORKER, {Release, Build, GuiHash}) ->
    Cluster#od_cluster{worker_version = {Release, Build, GuiHash}};
set_version_info(Cluster, ?ONEPANEL, {Release, Build, GuiHash}) ->
    Cluster#od_cluster{onepanel_version = {Release, Build, GuiHash}}.


%% @private
-spec get_version_info(od_cluster:record(), onedata:service_type()) ->
    od_cluster:version_info().
get_version_info(Cluster, ?WORKER) ->
    Cluster#od_cluster.worker_version;
get_version_info(Cluster, ?ONEPANEL) ->
    Cluster#od_cluster.onepanel_version.


%% @private
-spec update_onepanel_proxy(od_cluster:record(), entity_logic:data()) -> od_cluster:record().
update_onepanel_proxy(Cluster, Data) ->
    case maps:find(<<"onepanelProxy">>, Data) of
        error -> Cluster;
        {ok, Flag} -> Cluster#od_cluster{onepanel_proxy = Flag}
    end.
