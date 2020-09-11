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

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("datastore/oz_datastore_models.hrl").
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
fetch_entity(#gri{id = SpaceId}) ->
    case od_space:get(SpaceId) of
        {ok, #document{value = Space, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_rev:parse(DbRev),
            {true, {Space, Revision}};
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
operation_supported(create, space_support_token, private) -> true;

operation_supported(create, instance, private) -> true;
operation_supported(create, join, private) -> true;
operation_supported(create, {owner, _}, private) -> true;

operation_supported(create, {user, _}, private) -> true;
operation_supported(create, {group, _}, private) -> true;
operation_supported(create, group, private) -> true;
operation_supported(create, harvest_metadata, private) -> true;

operation_supported(get, list, private) -> true;
operation_supported(get, privileges, _) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, instance, protected) -> true;
operation_supported(get, owners, private) -> true;

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

operation_supported(get, eff_providers, private) -> true;
operation_supported(get, harvesters, private) -> true;

operation_supported(get, storages, private) -> true;

operation_supported(update, instance, private) -> true;
operation_supported(update, {user_privileges, _}, private) -> true;
operation_supported(update, {group_privileges, _}, private) -> true;

operation_supported(delete, instance, private) -> true;
operation_supported(delete, {owner, _}, private) -> true;

operation_supported(delete, {user, _}, private) -> true;
operation_supported(delete, {group, _}, private) -> true;
operation_supported(delete, {storage, _}, private) -> true;
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
is_subscribable(owners, private) -> true;
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
is_subscribable(eff_providers, private) -> true;
is_subscribable(shares, private) -> true;
is_subscribable(harvesters, private) -> true;
is_subscribable(_, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(Req = #el_req{gri = #gri{id = undefined, aspect = instance} = GRI, auth = Auth}) ->
    #{<<"name">> := Name} = Req#el_req.data,
    {ok, #document{key = SpaceId}} = od_space:create(#document{
        value = #od_space{
            name = Name,
            creator = aai:normalize_subject(Auth#auth.subject)
        }
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
            ),
            case Auth of
                ?USER(CreatorUserId) ->
                    % if the space is created by one of the group members, add
                    % the creator as direct user (which will make him an owner)
                    group_logic:has_eff_user(GroupId, CreatorUserId) andalso entity_graph:add_relation(
                        od_user, CreatorUserId,
                        od_space, SpaceId,
                        privileges:space_admin()
                    );
                _ ->
                    ok
            end;
        _ ->
            ok
    end,

    {true, {Space, Rev}} = fetch_entity(#gri{aspect = instance, id = SpaceId}),
    {ok, resource, {GRI#gri{id = SpaceId}, {Space, Rev}}};

create(Req = #el_req{auth = Auth, gri = #gri{id = undefined, aspect = join}}) ->
    Token = maps:get(<<"token">>, Req#el_req.data),
    ExpType = case Req#el_req.auth_hint of
        ?AS_USER(_) -> ?USER_JOIN_SPACE;
        ?AS_GROUP(_) -> ?GROUP_JOIN_SPACE;
        ?AS_HARVESTER(_) -> ?HARVESTER_JOIN_SPACE
    end,

    invite_tokens:consume(Auth, Token, ExpType, fun(SpaceId, _, Privileges) ->
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
            ?AS_HARVESTER(HarvesterId) ->
                entity_graph:add_relation(
                    od_harvester, HarvesterId,
                    od_space, SpaceId
                ),
                harvester_indices:update_stats(HarvesterId, all, fun(ExistingStats) ->
                    harvester_indices:coalesce_index_stats(ExistingStats, SpaceId, false)
                end)
        end,
        NewGRI = #gri{type = od_space, id = SpaceId, aspect = instance,
            % Privileges are defined only for USER_JOIN_SPACE and GROUP_JOIN_SPACE
            scope = case is_list(Privileges) andalso lists:member(?SPACE_VIEW, Privileges) of
                true -> private;
                false -> protected
            end
        },
        {true, {Space, Rev}} = fetch_entity(#gri{id = SpaceId}),
        {ok, SpaceData} = get(#el_req{gri = NewGRI}, Space),
        {ok, resource, {NewGRI, {SpaceData, Rev}}}
    end);

create(#el_req{auth = Auth, gri = #gri{id = SpaceId, aspect = invite_user_token}}) ->
    %% @TODO VFS-5815 deprecated, should be removed in the next major version AFTER 20.02.*
    token_logic:create_legacy_invite_token(Auth, ?INVITE_TOKEN(?USER_JOIN_SPACE, SpaceId));

create(#el_req{auth = Auth, gri = #gri{id = SpaceId, aspect = invite_group_token}}) ->
    %% @TODO VFS-5815 deprecated, should be removed in the next major version AFTER 20.02.*
    token_logic:create_legacy_invite_token(Auth, ?INVITE_TOKEN(?GROUP_JOIN_SPACE, SpaceId));

create(#el_req{auth = Auth, gri = #gri{id = SpaceId, aspect = space_support_token}}) ->
    %% @TODO VFS-5815 deprecated, should be removed in the next major version AFTER 20.02.*
    token_logic:create_legacy_invite_token(Auth, ?INVITE_TOKEN(
        ?SUPPORT_SPACE, SpaceId, support_parameters:build(global, eager)
    ));

create(#el_req{gri = #gri{id = SpaceId, aspect = {owner, UserId}}}) ->
    % this is run for the entity that was fetched at the beginning of the request processing
    fun(PreviousSpace) ->
        case space_logic:has_eff_user(PreviousSpace, UserId) of
            false ->
                % only effective members can be assigned as owners
                ?ERROR_RELATION_DOES_NOT_EXIST(od_space, SpaceId, od_user, UserId);
            true ->
                % add the user as direct member if he wasn't one
                UserWasDirectMember = space_logic:has_direct_user(PreviousSpace, UserId),
                UserWasDirectMember orelse entity_graph:add_relation(
                    od_user, UserId,
                    od_space, SpaceId,
                    privileges:space_admin()
                ),

                ?extract_ok(od_space:update(SpaceId, fun(CurrentSpace = #od_space{owners = Owners}) ->
                    case space_logic:has_direct_user(CurrentSpace, UserId) of
                        false ->
                            % as the adding of direct member and this update are not in one transaction,
                            % it is possible that the direct membership has been removed in the
                            % meantime - in such case deny the operation
                            ?ERROR_RELATION_DOES_NOT_EXIST(od_space, SpaceId, od_user, UserId);
                        true ->
                            case lists:member(UserId, Owners) of
                                true when not UserWasDirectMember ->
                                    % calling entity_graph:add_relation before might have already
                                    % assigned the user as owner - given that the user was added
                                    % as the first direct member of the space
                                    {ok, CurrentSpace};
                                true ->
                                    ?ERROR_ALREADY_EXISTS;
                                false ->
                                    {ok, CurrentSpace#od_space{owners = [UserId | Owners]}}
                            end
                    end
                end))
        end
    end;

create(#el_req{gri = #gri{id = SpaceId, aspect = {user, UserId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:space_member()),
    entity_graph:add_relation(
        od_user, UserId,
        od_space, SpaceId,
        Privileges
    ),
    NewGRI = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
    {true, {User, Rev}} = user_logic_plugin:fetch_entity(#gri{id = UserId}),
    {ok, UserData} = user_logic_plugin:get(#el_req{gri = NewGRI}, User),
    {ok, resource, {NewGRI, ?THROUGH_SPACE(SpaceId), {UserData, Rev}}};

create(Req = #el_req{gri = GRI = #gri{id = SpaceId, aspect = group}}) ->
    % Create a new group for a user and add the group as a member of this space.
    {ok, resource, {NewGRI = #gri{id = GroupId}, _}} = group_logic_plugin:create(
        Req#el_req{gri = GRI#gri{type = od_group, id = undefined, aspect = instance}}
    ),
    Privileges = privileges:space_member(),
    entity_graph:add_relation(
        od_group, GroupId,
        od_space, SpaceId,
        Privileges
    ),
    {true, {Group, Rev}} = group_logic_plugin:fetch_entity(#gri{id = GroupId}),
    {ok, resource, {NewGRI, {Group, Rev}}};

create(#el_req{gri = #gri{id = SpaceId, aspect = {group, GroupId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:space_member()),
    entity_graph:add_relation(
        od_group, GroupId,
        od_space, SpaceId,
        Privileges
    ),
    NewGRI = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
    {true, {Group, Rev}} = group_logic_plugin:fetch_entity(#gri{id = GroupId}),
    {ok, GroupData} = group_logic_plugin:get(#el_req{gri = NewGRI}, Group),
    {ok, resource, {NewGRI, ?THROUGH_GROUP(SpaceId), {GroupData, Rev}}};

create(#el_req{auth = Auth, gri = #gri{id = SpaceId, aspect = harvest_metadata}, data = Data}) ->
    #{
        <<"destination">> := Destination,
        <<"maxSeq">> := MaxSeq,
        <<"maxStreamSeq">> := MaxStreamSeq,
        <<"batch">> := Batch
    } = Data,

    Res = try
        lists_utils:pmap(fun(HarvesterId) ->
            Indices = maps:get(HarvesterId, Destination),
            case harvester_logic:submit_batch(Auth, HarvesterId, Indices, SpaceId, Batch, MaxStreamSeq, MaxSeq) of
                {ok, FailedIndices} -> {HarvesterId, FailedIndices};
                Error -> {HarvesterId, Error}
            end
        end, maps:keys(Destination))
    catch error:{parallel_call_failed, {failed_processes, Errors}} ->
        ?error("Harvesting metadata in space ~p failed due to: ~p",
            [SpaceId, Errors]),
        throw(?ERROR_TEMPORARY_FAILURE)
    end,

    {ok, value, lists:foldl(
        fun({HarvesterId, {error, _} = Error}, Acc) -> Acc#{HarvesterId => #{<<"error">> => Error}};
            ({HarvesterId, FailedIndices}, Acc) when map_size(FailedIndices) =/= 0 ->
                Acc#{HarvesterId => FailedIndices};
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

get(#el_req{gri = #gri{aspect = privileges}}, _) ->
    {ok, #{
        <<"member">> => privileges:space_member(),
        <<"manager">> => privileges:space_manager(),
        <<"admin">> => privileges:space_admin()
    }};

get(#el_req{gri = #gri{aspect = owners}}, #od_space{owners = Owners}) ->
    {ok, Owners};

get(#el_req{gri = #gri{aspect = instance, scope = private}}, Space) ->
    {ok, Space};
get(#el_req{gri = #gri{aspect = instance, scope = protected}}, Space) ->
    #od_space{
        name = Name,
        shares = Shares,
        creation_time = CreationTime,
        creator = Creator
    } = Space,
    {ok, #{
        <<"name">> => Name,
        <<"providers">> => entity_graph:get_relations_with_attrs(effective, top_down, od_provider, Space),
        <<"creationTime">> => CreationTime,
        <<"creator">> => Creator,
        <<"sharesCount">> => length(Shares)
    }};

get(#el_req{gri = #gri{aspect = users}}, Space) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_user, Space)};
get(#el_req{gri = #gri{aspect = eff_users}}, Space) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_user, Space)};
get(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, Space) ->
    {ok, entity_graph:get_relation_attrs(direct, bottom_up, od_user, UserId, Space)};
get(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, Space) ->
    {ok, entity_graph:get_relation_attrs(effective, bottom_up, od_user, UserId, Space)};
get(#el_req{gri = #gri{aspect = {eff_user_membership, UserId}}}, Space) ->
    {ok, entity_graph:get_intermediaries(bottom_up, od_user, UserId, Space)};

get(#el_req{gri = #gri{aspect = groups}}, Space) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_group, Space)};
get(#el_req{gri = #gri{aspect = eff_groups}}, Space) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_group, Space)};
get(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, Space) ->
    {ok, entity_graph:get_relation_attrs(direct, bottom_up, od_group, GroupId, Space)};
get(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, Space) ->
    {ok, entity_graph:get_relation_attrs(effective, bottom_up, od_group, GroupId, Space)};
get(#el_req{gri = #gri{aspect = {eff_group_membership, GroupId}}}, Space) ->
    {ok, entity_graph:get_intermediaries(bottom_up, od_group, GroupId, Space)};

get(#el_req{gri = #gri{aspect = shares}}, Space) ->
    {ok, Space#od_space.shares};

get(#el_req{gri = #gri{aspect = eff_providers}}, Space) ->
    {ok, entity_graph:get_relations(effective, top_down, od_provider, Space)};

get(#el_req{gri = #gri{aspect = harvesters}}, Space) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_harvester, Space)};

get(#el_req{gri = #gri{aspect = storages}}, Space) ->
    {ok, entity_graph:get_relations(direct, top_down, od_storage, Space)}.


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
            harvester_indices:update_stats(HarvesterId, all,
                fun(ExistingStats) ->
                    harvester_indices:coalesce_index_stats(ExistingStats, SpaceId, true)
                end)
        end, Harvesters),
        % remove all owners from the space to be deleted in order to avoid
        % potential errors when cleaning up user relations
        {ok, _} = od_space:update(SpaceId, fun(Space) ->
            {ok, Space#od_space{owners = []}}
        end),
        entity_graph:delete_with_relations(od_space, SpaceId)
    end;

delete(#el_req{gri = #gri{id = SpaceId, aspect = {owner, UserId}}}) ->
    ?extract_ok(od_space:update(SpaceId, fun(Space = #od_space{owners = Owners}) ->
        case lists:member(UserId, Owners) of
            false ->
                ?ERROR_NOT_FOUND;
            true ->
                case Owners of
                    [UserId] -> ?ERROR_CANNOT_REMOVE_LAST_OWNER(od_space, SpaceId);
                    _ -> {ok, Space#od_space{owners = lists:delete(UserId, Owners)}}
                end
        end
    end));

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

delete(#el_req{gri = #gri{id = SpaceId, aspect = {storage, StorageId}}}) ->
    fun(#od_space{harvesters = Harvesters}) ->

        {true, {
            #od_storage{provider = ProviderId}, _Rev
        }} = storage_logic_plugin:fetch_entity(#gri{id = StorageId}),

        lists:foreach(fun(HarvesterId) ->
            harvester_indices:update_stats(HarvesterId, all,
                fun(ExistingStats) ->
                    harvester_indices:coalesce_index_stats(ExistingStats, SpaceId, ProviderId, true)
                end)
        end, Harvesters),

        entity_graph:remove_relation(
            od_space, SpaceId,
            od_storage, StorageId
        )
    end;

delete(#el_req{gri = #gri{id = SpaceId, aspect = {provider, ProviderId}}}) ->
    fun(#od_space{harvesters = Harvesters, storages = Storages}) ->
        lists:foreach(fun(HarvesterId) ->
            harvester_indices:update_stats(HarvesterId, all,
                fun(ExistingStats) ->
                    harvester_indices:coalesce_index_stats(ExistingStats, SpaceId, ProviderId, true)
                end)
        end, Harvesters),

        lists:foreach(fun(StorageId) ->
            case storage_logic_plugin:fetch_entity(#gri{id = StorageId}) of
                {true, {#od_storage{provider = ProviderId}, _Rev}} ->
                    entity_graph:remove_relation(
                        od_space, SpaceId,
                        od_storage, StorageId
                    );
                _ ->
                    ok
            end
        end, maps:keys(Storages))
    end;

delete(#el_req{gri = #gri{id = SpaceId, aspect = {harvester, HarvesterId}}}) ->
    fun(#od_space{eff_providers = Providers}) ->
        harvester_indices:update_stats(HarvesterId, all, fun(ExistingStats) ->
            harvester_indices:coalesce_index_stats(ExistingStats, SpaceId, maps:keys(Providers), true)
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
            space_logic:is_supported_by_provider(Space, ProviderId);
        ?THROUGH_HARVESTER(HarvesterId) ->
            space_logic:has_harvester(Space, HarvesterId);
        undefined ->
            true
    end;

exists(#el_req{gri = #gri{aspect = {owner, UserId}}}, Space) ->
    lists:member(UserId, Space#od_space.owners);

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

exists(#el_req{gri = #gri{aspect = {storage, StorageId}}}, Space) ->
    entity_graph:has_relation(direct, top_down, od_storage, StorageId, Space);

exists(#el_req{gri = #gri{aspect = {provider, ProviderId}}}, Space) ->
    entity_graph:has_relation(effective, top_down, od_provider, ProviderId, Space);

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
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            true;
        {?USER(UserId), ?AS_GROUP(GroupId)} ->
            group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_SPACE);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = join}}, _) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            true;
        {?USER(UserId), ?AS_GROUP(GroupId)} ->
            group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_SPACE);
        {?USER(UserId), ?AS_HARVESTER(HarvesterId)} ->
            harvester_logic:has_eff_privilege(HarvesterId, UserId, ?HARVESTER_ADD_SPACE);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {owner, _UserId}}}, Space) ->
    auth_by_ownership(Req, Space);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, UserId}}, auth = ?USER(UserId), data = #{<<"privileges">> := _}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_ADD_USER) andalso auth_by_privilege(Req, Space, ?SPACE_SET_PRIVILEGES);
authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, UserId}}, auth = ?USER(UserId), data = _}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_ADD_USER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {group, GroupId}}, auth = ?USER(UserId), data = #{<<"privileges">> := _}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_ADD_GROUP) andalso
        auth_by_privilege(Req, Space, ?SPACE_SET_PRIVILEGES) andalso
        group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_SPACE);
authorize(Req = #el_req{operation = create, gri = #gri{aspect = {group, GroupId}}, auth = ?USER(UserId), data = _}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_ADD_GROUP) andalso
        group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_SPACE);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = group}}, Space) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            auth_by_privilege(Req, Space, ?SPACE_ADD_GROUP);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{id = SpaceId, aspect = harvest_metadata}, data = Data}, Space) ->
    auth_by_support(Req, Space) andalso
        lists:all(fun(#{<<"fileId">> := FileId}) ->
            {ok, Guid} = file_id:objectid_to_guid(FileId),
            file_id:guid_to_space_id(Guid) == SpaceId
        end, maps:get(<<"batch">>, Data));

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_user_token}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_ADD_USER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_group_token}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_ADD_GROUP);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = space_support_token}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_ADD_SUPPORT);

authorize(#el_req{operation = get, gri = #gri{aspect = privileges}}, _) ->
    true;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = instance, scope = private}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_VIEW) orelse auth_by_support(Req, Space);

authorize(Req = #el_req{operation = get, gri = GRI = #gri{aspect = instance, scope = protected}}, Space) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
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

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {user_privileges, UserId}}}, _) ->
    true;
authorize(Req = #el_req{operation = get, gri = #gri{aspect = {user_privileges, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_VIEW_PRIVILEGES) orelse auth_by_support(Req, Space);

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {eff_user_privileges, UserId}}}, _) ->
    true;
authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_user_privileges, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_VIEW_PRIVILEGES) orelse auth_by_support(Req, Space);

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {eff_user_membership, UserId}}}, _) ->
    true;
authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_user_membership, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_VIEW) orelse auth_by_support(Req, Space);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {group_privileges, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_VIEW_PRIVILEGES) orelse auth_by_support(Req, Space);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_group_privileges, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_VIEW_PRIVILEGES) orelse auth_by_support(Req, Space);

authorize(Req = #el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {eff_group_membership, GroupId}}}, Space) ->
    group_logic:has_eff_user(GroupId, UserId) orelse
        auth_by_privilege(Req, Space, ?SPACE_VIEW) orelse
        auth_by_support(Req, Space);

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = eff_providers}}, Space) ->
    % any space member can learn the list of providers
    provider_logic:has_eff_user(Space, UserId);

authorize(Req = #el_req{operation = get}, Space) ->
    % All other resources can be accessed with view privileges or by the supporting provider
    auth_by_privilege(Req, Space, ?SPACE_VIEW) orelse auth_by_support(Req, Space);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = instance}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_UPDATE);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_SET_PRIVILEGES);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {group_privileges, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_SET_PRIVILEGES);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = instance}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_DELETE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {owner, _}}}, Space) ->
    auth_by_ownership(Req, Space);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {user, UserId}}}, Space) ->
    % only space owners can remove other owners from the space
    case space_logic:is_owner(Space, UserId) of
        true -> auth_by_ownership(Req, Space);
        false -> auth_by_privilege(Req, Space, ?SPACE_REMOVE_USER)
    end;

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {group, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_REMOVE_GROUP);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {storage, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_REMOVE_SUPPORT);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {provider, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_REMOVE_SUPPORT);

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
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = space_support_token}}) ->
    [?OZ_SPACES_ADD_RELATIONSHIPS];

required_admin_privileges(Req = #el_req{operation = create, gri = #gri{aspect = join}}) ->
    case Req#el_req.auth_hint of
        ?AS_USER(_) -> [?OZ_USERS_ADD_RELATIONSHIPS];
        ?AS_GROUP(_) -> [?OZ_GROUPS_ADD_RELATIONSHIPS];
        ?AS_HARVESTER(_) -> [?OZ_HARVESTERS_ADD_RELATIONSHIPS]
    end;

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {owner, _}}}) ->
    [?OZ_SPACES_SET_PRIVILEGES];

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

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = owners}}) ->
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

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_providers}}) ->
    [?OZ_SPACES_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = harvesters}}) ->
    [?OZ_SPACES_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = storages}}) ->
    [?OZ_SPACES_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = instance}}) ->
    [?OZ_SPACES_UPDATE];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) ->
    [?OZ_SPACES_SET_PRIVILEGES];
required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {group_privileges, _}}}) ->
    [?OZ_SPACES_SET_PRIVILEGES];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = instance}}) ->
    [?OZ_SPACES_DELETE];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {owner, _}}}) ->
    [?OZ_SPACES_SET_PRIVILEGES];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {user, _}}}) ->
    [?OZ_SPACES_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {group, _}}}) ->
    [?OZ_SPACES_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {storage, _}}}) ->
    [?OZ_SPACES_REMOVE_RELATIONSHIPS];
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
    #{
        required => #{
            <<"token">> => {invite_token, case Req#el_req.auth_hint of
                ?AS_USER(_) -> ?USER_JOIN_SPACE;
                ?AS_GROUP(_) -> ?GROUP_JOIN_SPACE;
                ?AS_HARVESTER(_) -> ?HARVESTER_JOIN_SPACE
            end}
        }
    };

validate(#el_req{operation = create, gri = #gri{aspect = invite_user_token}}) ->
    #{
    };

validate(#el_req{operation = create, gri = #gri{aspect = invite_group_token}}) ->
    #{
    };

validate(#el_req{operation = create, gri = #gri{aspect = space_support_token}}) ->
    #{
    };

validate(#el_req{operation = create, gri = #gri{aspect = {owner, _}}}) -> #{
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

%% @private
-spec auth_by_ownership(entity_logic:req(), od_space:id() | od_space:record()) ->
    boolean().
auth_by_ownership(#el_req{auth = ?USER(UserId)}, SpaceOrId) ->
    space_logic:is_owner(SpaceOrId, UserId);
auth_by_ownership(_, _) ->
    false.


%% @private
-spec auth_by_privilege(entity_logic:req(), od_space:id() | od_space:record(),
    privileges:space_privilege()) -> boolean().
auth_by_privilege(#el_req{auth = ?USER(UserId)}, SpaceOrId, Privilege) ->
    space_logic:has_eff_privilege(SpaceOrId, UserId, Privilege);
auth_by_privilege(_, _, _) ->
    false.


%% @private
-spec auth_by_support(entity_logic:req(), od_space:record()) -> boolean().
auth_by_support(#el_req{auth = ?PROVIDER(ProviderId)}, Space) ->
    space_logic:is_supported_by_provider(Space, ProviderId);
auth_by_support(_, _) ->
    false.