%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_space model.
%%% @end
%%%-------------------------------------------------------------------
-module(space_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("tokens.hrl").
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
%% Retrieves an entity from datastore based on its EntityId.
%% Should return ?ERROR_NOT_FOUND if the entity does not exist.
%% @end
%%--------------------------------------------------------------------
-spec fetch_entity(entity_logic:entity_id()) ->
    {ok, entity_logic:entity()} | entity_logic:error().
fetch_entity(SpaceId) ->
    case od_space:get(SpaceId) of
        {ok, #document{value = Space}} ->
            {ok, Space};
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
operation_supported(create, invite_provider_token, private) -> true;

operation_supported(create, instance, private) -> true;
operation_supported(create, join, private) -> true;

operation_supported(create, {user, _}, private) -> true;
operation_supported(create, {group, _}, private) -> true;
operation_supported(create, group, private) -> true;
operation_supported(create, harvest_metadata, private) -> true;

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

operation_supported(get, shares, private) -> true;

operation_supported(get, providers, private) -> true;
operation_supported(get, harvesters, private) -> true;

operation_supported(update, instance, private) -> true;
operation_supported(update, {user_privileges, _}, private) -> true;
operation_supported(update, {group_privileges, _}, private) -> true;

operation_supported(delete, instance, private) -> true;
operation_supported(delete, {user, _}, private) -> true;
operation_supported(delete, {group, _}, private) -> true;
operation_supported(delete, {provider, _}, private) -> true;
operation_supported(delete, {harvester, _}, private) -> true;

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
is_subscribable(providers, private) -> true;
is_subscribable(harvesters, private) -> true;
is_subscribable(_, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(Req = #el_req{gri = #gri{id = undefined, aspect = instance} = GRI, client = Client}) ->
    #{<<"name">> := Name} = Req#el_req.data,
    {ok, #document{key = SpaceId}} = od_space:create(#document{
        value = #od_space{name = Name, creator = Client}
    }),
    case Req#el_req.auth_hint of
        ?AS_USER(UserId) ->
            entity_graph:add_relation(
                od_user, UserId,
                od_space, SpaceId,
                privileges:space_admin()
            );
        ?AS_GROUP(GroupId) ->
            entity_graph:add_relation(
                od_group, GroupId,
                od_space, SpaceId,
                privileges:space_admin()
            );
        _ ->
            ok
    end,
    {ok, Space} = fetch_entity(SpaceId),
    {ok, resource, {GRI#gri{id = SpaceId}, Space}};

create(Req = #el_req{gri = #gri{id = undefined, aspect = join}}) ->
    Macaroon = maps:get(<<"token">>, Req#el_req.data),
    % In the future, privileges can be included in token
    Privileges = privileges:space_user(),
    JoinSpaceFun = fun(od_space, SpaceId) ->
        case Req#el_req.auth_hint of
            ?AS_USER(UserId) ->
                entity_graph:add_relation(
                    od_user, UserId,
                    od_space, SpaceId,
                    Privileges
                );
            ?AS_GROUP(GroupId) ->
                entity_graph:add_relation(
                    od_group, GroupId,
                    od_space, SpaceId,
                    Privileges
                );
            _ ->
                ok
        end,
        SpaceId
    end,
    SpaceId = token_logic:consume(Macaroon, JoinSpaceFun),

    NewGRI = #gri{type = od_space, id = SpaceId, aspect = instance,
        scope = case lists:member(?SPACE_VIEW, Privileges) of
            true -> private;
            false -> protected
        end
    },
    {ok, Space} = fetch_entity(SpaceId),
    {ok, SpaceData} = get(#el_req{gri = NewGRI}, Space),
    {ok, resource, {NewGRI, SpaceData}};

create(Req = #el_req{gri = #gri{id = SpaceId, aspect = invite_user_token}}) ->
    {ok, Macaroon} = token_logic:create(
        Req#el_req.client,
        ?SPACE_INVITE_USER_TOKEN,
        {od_space, SpaceId}
    ),
    {ok, value, Macaroon};

create(Req = #el_req{gri = #gri{id = SpaceId, aspect = invite_group_token}}) ->
    {ok, Macaroon} = token_logic:create(
        Req#el_req.client,
        ?SPACE_INVITE_GROUP_TOKEN,
        {od_space, SpaceId}
    ),
    {ok, value, Macaroon};

create(Req = #el_req{gri = #gri{id = SpaceId, aspect = invite_provider_token}}) ->
    {ok, Macaroon} = token_logic:create(
        Req#el_req.client,
        ?SPACE_SUPPORT_TOKEN,
        {od_space, SpaceId}
    ),
    {ok, value, Macaroon};

create(#el_req{gri = #gri{id = SpaceId, aspect = {user, UserId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:space_user()),
    entity_graph:add_relation(
        od_user, UserId,
        od_space, SpaceId,
        Privileges
    ),
    NewGRI = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
    {ok, User} = user_logic_plugin:fetch_entity(UserId),
    {ok, UserData} = user_logic_plugin:get(#el_req{gri = NewGRI}, User),
    {ok, resource, {NewGRI, ?THROUGH_SPACE(SpaceId), UserData}};

create(Req = #el_req{gri = GRI = #gri{id = SpaceId, aspect = group}}) ->
    % Create a new group for a user and add the group as a member of this space.
    {ok, resource, {NewGRI = #gri{id = GroupId}, _}} = group_logic_plugin:create(
        Req#el_req{gri = GRI#gri{type = od_group, id = undefined, aspect = instance}}
    ),
    Privileges = privileges:space_user(),
    entity_graph:add_relation(
        od_group, GroupId,
        od_space, SpaceId,
        Privileges
    ),
    {ok, Group} = group_logic_plugin:fetch_entity(GroupId),
    {ok, resource, {NewGRI, Group}};

create(#el_req{gri = #gri{id = SpaceId, aspect = {group, GroupId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:space_user()),
    entity_graph:add_relation(
        od_group, GroupId,
        od_space, SpaceId,
        Privileges
    ),
    NewGRI = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
    {ok, Group} = group_logic_plugin:fetch_entity(GroupId),
    {ok, GroupData} = group_logic_plugin:get(#el_req{gri = NewGRI}, Group),
    {ok, resource, {NewGRI, ?THROUGH_GROUP(SpaceId), GroupData}};

create(#el_req{client = Client, gri = #gri{id = SpaceId, aspect = harvest_metadata}, data = Data}) ->
    #{
        <<"destination">> := Destination,
        <<"maxSeq">> := MaxSeq,
        <<"maxStreamSeq">> := MaxStreamSeq,
        <<"batch">> := Batch
    } = Data,
    
    Res = utils:pmap(fun(HarvesterId) ->
        Indices = maps:get(HarvesterId, Destination),
        case harvester_logic:submit_batch(Client, HarvesterId, Indices, SpaceId, Batch, MaxStreamSeq, MaxSeq) of
            {ok, FailedIndices} -> {HarvesterId, FailedIndices};
            Error -> {HarvesterId, Error}
        end
    end, maps:keys(Destination)),
    
    {ok, value, lists:foldl(
        fun({HarvesterId, {error, _} = Error}, Acc) -> Acc#{HarvesterId => #{<<"error">> => Error}};
           ({HarvesterId, FailedIndices}, Acc) when map_size(FailedIndices) =/= 0 -> Acc#{HarvesterId => FailedIndices};
           (_, Acc) -> Acc
        end, #{}, Res)}.
    

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = list}}, _) ->
    {ok, SpaceDocs} = od_space:list(),
    {ok, [SpaceId || #document{key = SpaceId} <- SpaceDocs]};

get(#el_req{gri = #gri{aspect = instance, scope = private}}, Space) ->
    {ok, Space};
get(#el_req{gri = #gri{aspect = instance, scope = protected}}, Space) ->
    #od_space{
        name = Name, providers = Providers, creation_time = CreationTime,
        shares = Shares, creator = Creator
    } = Space,
    {ok, #{
        <<"name">> => Name,
        <<"providers">> => Providers,
        <<"creationTime">> => CreationTime,
        <<"creator">> => Creator,
        <<"sharedDirectories">> => length(Shares)
    }};

get(#el_req{gri = #gri{aspect = users}}, Space) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_user, Space)};
get(#el_req{gri = #gri{aspect = eff_users}}, Space) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_user, Space)};
get(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, Space) ->
    {ok, entity_graph:get_privileges(direct, bottom_up, od_user, UserId, Space)};
get(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, Space) ->
    {ok, entity_graph:get_privileges(effective, bottom_up, od_user, UserId, Space)};
get(#el_req{gri = #gri{aspect = {eff_user_membership, UserId}}}, Space) ->
    {ok, entity_graph:get_intermediaries(bottom_up, od_user, UserId, Space)};

get(#el_req{gri = #gri{aspect = groups}}, Space) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_group, Space)};
get(#el_req{gri = #gri{aspect = eff_groups}}, Space) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_group, Space)};
get(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, Space) ->
    {ok, entity_graph:get_privileges(direct, bottom_up, od_group, GroupId, Space)};
get(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, Space) ->
    {ok, entity_graph:get_privileges(effective, bottom_up, od_group, GroupId, Space)};
get(#el_req{gri = #gri{aspect = {eff_group_membership, GroupId}}}, Space) ->
    {ok, entity_graph:get_intermediaries(bottom_up, od_group, GroupId, Space)};

get(#el_req{gri = #gri{aspect = shares}}, Space) ->
    {ok, Space#od_space.shares};

get(#el_req{gri = #gri{aspect = providers}}, Space) ->
    {ok, entity_graph:get_relations(direct, top_down, od_provider, Space)};

get(#el_req{gri = #gri{aspect = harvesters}}, Space) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_harvester, Space)}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = SpaceId, aspect = instance}, data = Data}) ->
    NewName = maps:get(<<"name">>, Data),
    {ok, _} = od_space:update(SpaceId, fun(Space = #od_space{}) ->
        {ok, Space#od_space{name = NewName}}
    end),
    ok;

update(Req = #el_req{gri = #gri{id = SpaceId, aspect = {user_privileges, UserId}}}) ->
    PrivsToGrant = maps:get(<<"grant">>, Req#el_req.data, []),
    PrivsToRevoke = maps:get(<<"revoke">>, Req#el_req.data, []),
    entity_graph:update_relation(
        od_user, UserId,
        od_space, SpaceId,
        {PrivsToGrant, PrivsToRevoke}
    );

update(Req = #el_req{gri = #gri{id = SpaceId, aspect = {group_privileges, GroupId}}}) ->
    PrivsToGrant = maps:get(<<"grant">>, Req#el_req.data, []),
    PrivsToRevoke = maps:get(<<"revoke">>, Req#el_req.data, []),
    entity_graph:update_relation(
        od_group, GroupId,
        od_space, SpaceId,
        {PrivsToGrant, PrivsToRevoke}
    ).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = SpaceId, aspect = instance}}) ->
    fun(#od_space{harvesters = Harvesters}) ->
        lists:foreach(fun(HarvesterId) ->
            harvester_logic:update_indices_stats(HarvesterId, all,
                fun(ExistingStats) -> maps:without([SpaceId], ExistingStats) end)
            end, Harvesters),
        entity_graph:delete_with_relations(od_space, SpaceId)
    end;

delete(#el_req{gri = #gri{id = SpaceId, aspect = {user, UserId}}}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_space, SpaceId
    );

delete(#el_req{gri = #gri{id = SpaceId, aspect = {group, GroupId}}}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_space, SpaceId
    );

delete(#el_req{gri = #gri{id = SpaceId, aspect = {provider, ProviderId}}}) ->
    fun(#od_space{harvesters = Harvesters}) ->
        lists:foreach(fun(HarvesterId) ->
            harvester_logic:update_indices_stats(HarvesterId, all,
                fun(ExistingStats) -> 
                    harvester_logic:prepare_index_stats(ExistingStats, SpaceId, ProviderId, true)
                end)
        end, Harvesters),
        
        entity_graph:remove_relation(
            od_space, SpaceId,
            od_provider, ProviderId
        )
    end;

delete(#el_req{gri = #gri{id = SpaceId, aspect = {harvester, HarvesterId}}}) ->
    fun(#od_space{providers = Providers}) ->
        harvester_logic:update_indices_stats(HarvesterId, all, fun(ExistingStats) ->
            harvester_logic:prepare_index_stats(ExistingStats, SpaceId, maps:keys(Providers), true)
        end),
        entity_graph:remove_relation(
            od_harvester, HarvesterId,
            od_space, SpaceId
        )
    end.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(Req = #el_req{gri = #gri{aspect = instance, scope = protected}}, Space) ->
    case Req#el_req.auth_hint of
        ?THROUGH_USER(UserId) ->
            space_logic:has_eff_user(Space, UserId);
        ?THROUGH_GROUP(GroupId) ->
            space_logic:has_eff_group(Space, GroupId);
        ?THROUGH_PROVIDER(ProviderId) ->
            space_logic:has_provider(Space, ProviderId);
        ?THROUGH_HARVESTER(HarvesterId) ->
            space_logic:has_harvester(Space, HarvesterId);
        undefined ->
            true
    end;

exists(#el_req{gri = #gri{aspect = {user, UserId}}}, Space) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, Space);

exists(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, Space) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, Space);

exists(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, Space) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Space);

exists(#el_req{gri = #gri{aspect = {eff_user_membership, UserId}}}, Space) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Space);

exists(#el_req{gri = #gri{aspect = {group, GroupId}}}, Space) ->
    entity_graph:has_relation(direct, bottom_up, od_group, GroupId, Space);

exists(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, Space) ->
    entity_graph:has_relation(direct, bottom_up, od_group, GroupId, Space);

exists(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, Space) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, Space);

exists(#el_req{gri = #gri{aspect = {eff_group_membership, GroupId}}}, Space) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, Space);

exists(#el_req{gri = #gri{aspect = {provider, ProviderId}}}, Space) ->
    entity_graph:has_relation(direct, top_down, od_provider, ProviderId, Space);

exists(#el_req{gri = #gri{aspect = {harvester, HarvesterId}}}, Space) ->
    entity_graph:has_relation(direct, bottom_up, od_harvester, HarvesterId, Space);

% All other aspects exist if space record exists.
exists(#el_req{gri = #gri{id = Id}}, #od_space{}) ->
    Id =/= undefined.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(Req = #el_req{operation = create, gri = #gri{id = undefined, aspect = instance}}, _) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            true;
        {?USER(UserId), ?AS_GROUP(GroupId)} ->
            group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_SPACE);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = join}}, _) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            true;
        {?USER(UserId), ?AS_GROUP(GroupId)} ->
            group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_SPACE);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, UserId}}, client = ?USER(UserId), data = #{<<"privileges">> := _}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_ADD_USER) andalso auth_by_privilege(Req, Space, ?SPACE_SET_PRIVILEGES);
authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, UserId}}, client = ?USER(UserId), data = _}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_ADD_USER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {group, GroupId}}, client = ?USER(UserId), data = #{<<"privileges">> := _}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_ADD_GROUP) andalso
        auth_by_privilege(Req, Space, ?SPACE_SET_PRIVILEGES) andalso
        group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_SPACE);
authorize(Req = #el_req{operation = create, gri = #gri{aspect = {group, GroupId}}, client = ?USER(UserId), data = _}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_ADD_GROUP) andalso
        group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_SPACE);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = group}}, Space) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            auth_by_privilege(Req, Space, ?SPACE_ADD_GROUP);
        _ ->
            false
    end;

authorize(#el_req{operation = create, gri = #gri{id = SpaceId, aspect = harvest_metadata}, 
    client = ?PROVIDER(ProviderId), data = #{<<"batch">> := Batch}}, Space) ->
    
    space_logic:has_provider(Space, ProviderId) andalso 
            lists:all(fun(#{<<"fileId">> := FileId}) -> 
                {ok, Guid} = file_id:objectid_to_guid(FileId),
                file_id:guid_to_space_id(Guid) == SpaceId
            end, Batch);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_user_token}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_ADD_USER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_group_token}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_ADD_GROUP);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_provider_token}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_ADD_PROVIDER);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = instance, scope = private}}, Space) ->
    case Req#el_req.client of
        ?USER(UserId) ->
            auth_by_privilege(UserId, Space, ?SPACE_VIEW);
        ?PROVIDER(ProviderId) ->
            space_logic:has_provider(Space, ProviderId)
    end;

authorize(Req = #el_req{operation = get, gri = GRI = #gri{aspect = instance, scope = protected}}, Space) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?THROUGH_USER(UserId)} ->
            % User's membership in this space is checked in 'exists'
            true;

        {?USER(_UserId), ?THROUGH_USER(_OtherUserId)} ->
            false;

        {?USER(ClientUserId), ?THROUGH_GROUP(GroupId)} ->
            % Groups's membership in this space is checked in 'exists'
            group_logic:has_eff_privilege(GroupId, ClientUserId, ?GROUP_VIEW);

        {?PROVIDER(ProviderId), ?THROUGH_PROVIDER(ProviderId)} ->
            % Provider's support in this space is checked in 'exists'
            true;

        {?PROVIDER(_ProviderId), ?THROUGH_PROVIDER(_OtherProviderId)} ->
            false;

        {?USER(ClientUserId), ?THROUGH_HARVESTER(HarvesterId)} ->
            % Harvester's membership in this space is checked in 'exists'
            harvester_logic:has_eff_privilege(HarvesterId, ClientUserId, ?HARVESTER_VIEW);

        {?USER(ClientUserId), ?THROUGH_PROVIDER(ProviderId)} ->
            % Provider's support in this space is checked in 'exists'
            ClusterId = ProviderId,
            cluster_logic:has_eff_privilege(ClusterId, ClientUserId, ?CLUSTER_VIEW);

        {?USER(ClientUserId), _} ->
            space_logic:has_eff_user(Space, ClientUserId);

        _ ->
            % Access to private data also allows access to protected data
            authorize(Req#el_req{gri = GRI#gri{scope = private}}, Space)
    end;

authorize(#el_req{operation = get, client = ?USER(UserId), gri = #gri{aspect = {user_privileges, UserId}}}, _) ->
    true;
authorize(Req = #el_req{operation = get, gri = #gri{aspect = {user_privileges, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_VIEW_PRIVILEGES);

authorize(#el_req{operation = get, client = ?USER(UserId), gri = #gri{aspect = {eff_user_privileges, UserId}}}, _) ->
    true;
authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_user_privileges, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_VIEW_PRIVILEGES);

authorize(#el_req{operation = get, client = ?USER(UserId), gri = #gri{aspect = {eff_user_membership, UserId}}}, _) ->
    true;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_user_membership, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_VIEW);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {group_privileges, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_VIEW_PRIVILEGES);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_group_privileges, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_VIEW_PRIVILEGES);

authorize(Req = #el_req{operation = get, client = ?USER(UserId), gri = #gri{aspect = {eff_group_membership, GroupId}}}, Space) ->
    group_logic:has_eff_user(GroupId, UserId) orelse auth_by_privilege(Req, Space, ?SPACE_VIEW);

authorize(Req = #el_req{operation = get, client = ?USER}, Space) ->
    % All other resources can be accessed with view privileges
    auth_by_privilege(Req, Space, ?SPACE_VIEW);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = instance}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_UPDATE);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_SET_PRIVILEGES);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {group_privileges, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_SET_PRIVILEGES);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = instance}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_DELETE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {user, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_REMOVE_USER);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {group, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_REMOVE_GROUP);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {provider, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_REMOVE_PROVIDER);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {harvester, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_REMOVE_HARVESTER);

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
        ?AS_USER(_) -> [?OZ_SPACES_CREATE, ?OZ_USERS_ADD_RELATIONSHIPS];
        ?AS_GROUP(_) -> [?OZ_SPACES_CREATE, ?OZ_GROUPS_ADD_RELATIONSHIPS];
        _ -> [?OZ_SPACES_CREATE]
    end;

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = invite_user_token}}) ->
    [?OZ_SPACES_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = invite_group_token}}) ->
    [?OZ_SPACES_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = invite_provider_token}}) ->
    [?OZ_SPACES_ADD_RELATIONSHIPS];

required_admin_privileges(Req = #el_req{operation = create, gri = #gri{aspect = join}}) ->
    case Req#el_req.auth_hint of
        ?AS_USER(_) -> [?OZ_USERS_ADD_RELATIONSHIPS];
        ?AS_GROUP(_) -> [?OZ_GROUPS_ADD_RELATIONSHIPS]
    end;

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {user, _}}, data = #{<<"privileges">> := _}}) ->
    [?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_SPACES_SET_PRIVILEGES, ?OZ_USERS_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {user, _}}, data = _}) ->
    [?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {group, _}}, data = #{<<"privileges">> := _}}) ->
    [?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_SPACES_SET_PRIVILEGES, ?OZ_GROUPS_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {group, _}}, data = _}) ->
    [?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_GROUPS_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = group}}) ->
    [?OZ_GROUPS_CREATE, ?OZ_SPACES_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = list}}) ->
    [?OZ_SPACES_LIST];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance, scope = protected}}) ->
    [?OZ_SPACES_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {user_privileges, _}}}) ->
    [?OZ_SPACES_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_user_privileges, _}}}) ->
    [?OZ_SPACES_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_user_membership, _}}}) ->
    [?OZ_SPACES_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {group_privileges, _}}}) ->
    [?OZ_SPACES_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_group_privileges, _}}}) ->
    [?OZ_SPACES_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_group_membership, _}}}) ->
    [?OZ_SPACES_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = users}}) ->
    [?OZ_SPACES_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_users}}) ->
    [?OZ_SPACES_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = groups}}) ->
    [?OZ_SPACES_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_groups}}) ->
    [?OZ_SPACES_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = shares}}) ->
    [?OZ_SPACES_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = providers}}) ->
    [?OZ_SPACES_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = harvesters}}) ->
    [?OZ_SPACES_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = instance}}) ->
    [?OZ_SPACES_UPDATE];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) ->
    [?OZ_SPACES_SET_PRIVILEGES];
required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {group_privileges, _}}}) ->
    [?OZ_SPACES_SET_PRIVILEGES];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = instance}}) ->
    [?OZ_SPACES_DELETE];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {user, _}}}) ->
    [?OZ_SPACES_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {group, _}}}) ->
    [?OZ_SPACES_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {provider, _}}}) ->
    [?OZ_SPACES_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {harvester, _}}}) ->
    [?OZ_SPACES_REMOVE_RELATIONSHIPS, ?OZ_HARVESTERS_REMOVE_RELATIONSHIPS];

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
    }
};

validate(Req = #el_req{operation = create, gri = #gri{aspect = join}}) ->
    TokenType = case Req#el_req.auth_hint of
        ?AS_USER(_) -> ?SPACE_INVITE_USER_TOKEN;
        ?AS_GROUP(_) -> ?SPACE_INVITE_GROUP_TOKEN
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

validate(#el_req{operation = create, gri = #gri{aspect = invite_provider_token}}) ->
    #{
    };

validate(#el_req{operation = create, gri = #gri{aspect = {user, _}}}) -> #{
    required => #{
        {aspect, <<"userId">>} => {any, {exists, fun(UserId) ->
            user_logic:exists(UserId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:space_privileges()}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {group, _}}}) -> #{
    required => #{
        {aspect, <<"groupId">>} => {any, {exists, fun(GroupId) ->
            group_logic:exists(GroupId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:space_privileges()}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = harvest_metadata}}) -> #{
    required => #{
        <<"destination">> => {any, any},
        <<"maxSeq">> => {integer, {not_lower_than, 0}},
        <<"maxStreamSeq">> => {integer, {not_lower_than, 0}},
        <<"batch">> => {any, any}
    }   
};

validate(Req = #el_req{operation = create, gri = #gri{aspect = group}}) ->
    group_logic_plugin:validate(Req#el_req{gri = #gri{
        type = od_group, id = undefined, aspect = instance
    }});

validate(Req = #el_req{operation = create, gri = #gri{aspect = harvester}}) ->
    harvester_logic_plugin:validate(Req#el_req{gri = #gri{
        type = od_harvester, id = undefined, aspect = instance
    }});

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    required => #{
        <<"name">> => {binary, name}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) ->
    #{
        at_least_one => #{
            <<"grant">> => {list_of_atoms, privileges:space_privileges()},
            <<"revoke">> => {list_of_atoms, privileges:space_privileges()}
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
%% Returns if given user has specific effective privilege in the space.
%% UserId is either given explicitly or derived from entity logic request.
%% Clients of type other than user are discarded.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_privilege(entity_logic:req() | od_user:id(),
    od_space:id() | od_space:info(), privileges:space_privilege()) -> boolean().
auth_by_privilege(#el_req{client = ?USER(UserId)}, SpaceOrId, Privilege) ->
    auth_by_privilege(UserId, SpaceOrId, Privilege);
auth_by_privilege(#el_req{client = _OtherClient}, _SpaceOrId, _Privilege) ->
    false;
auth_by_privilege(UserId, SpaceOrId, Privilege) ->
    space_logic:has_eff_privilege(SpaceOrId, UserId, Privilege).
