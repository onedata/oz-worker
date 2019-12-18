%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_storage model.
%%% @end
%%%-------------------------------------------------------------------
-module(storage_logic_plugin).
-author("Michal Stanisz").
-behaviour(entity_logic_plugin_behaviour).

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/errors.hrl").

-define(MINIMUM_SUPPORT_SIZE, oz_worker:get_env(minimum_space_support_size, 1000000)).

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
fetch_entity(#gri{id = StorageId}) ->
    case od_storage:get(StorageId) of
        {ok, #document{value = Storage, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_rev:parse(DbRev),
            {true, {Storage, Revision}};
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
operation_supported(create, support, private) -> true;
operation_supported(create, {upgrade_legacy_support, _}, private) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, instance, shared) -> true;

operation_supported(get, spaces, private) -> true;

operation_supported(update, instance, private) -> true;
operation_supported(update, {space, _}, private) -> true;

operation_supported(delete, instance, private) -> true;
operation_supported(delete, {space, _}, private) -> true;

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
create(#el_req{gri = #gri{id = ProposedId, aspect = instance} = GRI, auth = ?PROVIDER(ProviderId) = Auth, data = Data}) ->
    Name = maps:get(<<"name">>, Data),
    StorageDoc = #document{
        key = ProposedId,
        value = #od_storage{
            name = Name,
            creator = Auth#auth.subject,
            provider = ProviderId
        }
    },
    case od_storage:create(StorageDoc) of
        {error, already_exists} ->
            throw(?ERROR_ALREADY_EXISTS);
        {ok, #document{key = StorageId}} ->
            entity_graph:add_relation(
                od_storage, StorageId,
                od_provider, ProviderId
            ),
            {true, {Storage, Rev}} = fetch_entity(#gri{id = StorageId}),
            {ok, resource, {GRI#gri{id = StorageId}, {Storage, Rev}}}
    end;

create(#el_req{auth = Auth, gri = #gri{id = StorageId, aspect = support}, data = Data}) ->
    fun(#od_storage{provider = ProviderId}) ->
        SupportSize = maps:get(<<"size">>, Data),
        Token = maps:get(<<"token">>, Data),

        invite_tokens:consume(Auth, Token, ?SUPPORT_SPACE, fun(SpaceId, _) ->
            entity_graph:add_relation(
                od_space, SpaceId,
                od_storage, StorageId,
                SupportSize
            ),
            NewGRI = #gri{type = od_space, id = SpaceId, aspect = instance, scope = protected},
            {true, {Space, Rev}} = space_logic_plugin:fetch_entity(NewGRI),

            lists:foreach(fun(HarvesterId) ->
                harvester_indices:update_stats(HarvesterId, all, fun(ExistingStats) ->
                    harvester_indices:coalesce_index_stats(ExistingStats, SpaceId, ProviderId, false)
                end)
            end, Space#od_space.harvesters),

            {ok, SpaceData} = space_logic_plugin:get(#el_req{gri = NewGRI}, Space),
            {ok, resource, {NewGRI, {SpaceData, Rev}}}
        end)
    end;

% This endpoint is dedicated to providers upgrading from version 19.02.* to the next major release.
create(#el_req{gri = #gri{id = StorageId, aspect = {upgrade_legacy_support, SpaceId}}}) ->
    fun(#od_storage{provider = ProviderId}) ->
        {true, {VirtualStorage, _}} = fetch_entity(#gri{id = ProviderId}),
        SupportSize = entity_graph:get_relation_attrs(direct, bottom_up, od_space, SpaceId, VirtualStorage),
        try
            entity_graph:add_relation(od_space, SpaceId, od_storage, StorageId, SupportSize)
        catch
            _:(?ERROR_RELATION_ALREADY_EXISTS(_, _, _, _)) -> ok
        end,
        ok = entity_graph:remove_relation(od_space, SpaceId, od_storage, ProviderId),
        ?notice("Support of space: ~p successfully upgraded", [SpaceId])
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = instance, scope = private}}, Storage) ->
    {ok, Storage};
get(#el_req{gri = #gri{aspect = instance, scope = shared}}, Storage) ->
    #od_storage{
        qos_parameters = QosParameters,
        creation_time = CreationTime, creator = Creator
    } = Storage,
    {ok, #{
        <<"qos_parameters">> => QosParameters,
        <<"creationTime">> => CreationTime,
        <<"creator">> => Creator
    }};

get(#el_req{gri = #gri{aspect = spaces}}, Storage) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_space, Storage)}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = StorageId, aspect = instance}, data = Data}) ->
    {ok, _} = od_storage:update(StorageId, fun(Storage) ->
        #od_storage{
            name = Name,
            qos_parameters = QosParameters
        } = Storage,

        NewName = maps:get(<<"name">>, Data, Name),
        NewQosParameters = maps:get(<<"qos_parameters">>, Data, QosParameters),

        {ok, Storage#od_storage{name = NewName, qos_parameters = NewQosParameters}}
    end),
    ok;

update(Req = #el_req{gri = #gri{id = StorageId, aspect = {space, SpaceId}}}) ->
    NewSupportSize = maps:get(<<"size">>, Req#el_req.data),
    entity_graph:update_relation(
        od_space, SpaceId, od_storage, StorageId, NewSupportSize
    ).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = StorageId, aspect = instance}}) ->
    entity_graph:delete_with_relations(od_storage, StorageId);

delete(#el_req{gri = #gri{id = StorageId, aspect = {space, SpaceId}}}) ->
    fun(#od_storage{provider = ProviderId}) ->
        {true, {#od_space{harvesters = Harvesters}, _}} = space_logic_plugin:fetch_entity(#gri{id = SpaceId}),
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
    end.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(Req = #el_req{gri = #gri{aspect = instance, scope = protected}}, Storage) ->
    case Req#el_req.auth_hint of
        ?THROUGH_PROVIDER(ProviderId) ->
            storage_logic:belongs_to_provider(Storage, ProviderId);
        undefined ->
            true
    end;

exists(#el_req{gri = #gri{aspect = {space, SpaceId}}}, Storage) ->
    entity_graph:has_relation(direct, bottom_up, od_space, SpaceId, Storage);

% All other aspects exist if storage record exists.
exists(#el_req{gri = #gri{id = Id}}, #od_storage{}) ->
    Id =/= undefined.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(Req = #el_req{operation = create, gri = #gri{aspect = instance}}, _) ->
    case Req#el_req.auth of
        ?PROVIDER(_) -> true;
        _ -> false
    end;

authorize(#el_req{operation = create, auth = ?PROVIDER(ProviderId), gri = #gri{aspect = support}}, Storage) ->
    storage_logic:belongs_to_provider(Storage, ProviderId);

authorize(#el_req{operation = create, auth = ?PROVIDER(ProviderId), gri = #gri{aspect = {upgrade_legacy_support, SpaceId}}}, Storage) ->
    storage_logic:belongs_to_provider(Storage, ProviderId)
    %% Check whether given space is supported by provider virtual storage (with id equal to providers)
        andalso storage_logic:supports_space(ProviderId, SpaceId);

authorize(#el_req{operation = get, auth = ?PROVIDER(ProviderId), gri = #gri{aspect = instance, scope = private}}, Storage) ->
    storage_logic:belongs_to_provider(Storage, ProviderId);

authorize(Req = #el_req{operation = get, gri = GRI = #gri{aspect = instance, id = StorageId, scope = shared}}, Storage) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?PROVIDER(ProviderId), ?THROUGH_SPACE(SpaceId)} ->
            storage_logic:supports_space(StorageId, SpaceId)
                andalso provider_logic:supports_space(ProviderId, SpaceId);
        _ ->
            % Access to private data also allows access to protected data
            authorize(Req#el_req{gri = GRI#gri{scope = private}}, Storage)
    end;

authorize(#el_req{operation = get, auth = ?PROVIDER(ProviderId), gri = #gri{aspect = spaces}}, Storage) ->
    storage_logic:belongs_to_provider(Storage, ProviderId);

authorize(#el_req{operation = update, auth = ?PROVIDER(ProviderId), gri = #gri{aspect = instance}}, Storage) ->
    storage_logic:belongs_to_provider(Storage, ProviderId);

authorize(#el_req{operation = update, auth = ?PROVIDER(ProviderId), gri = #gri{aspect = {space, _}}}, Storage) ->
    storage_logic:belongs_to_provider(Storage, ProviderId);

authorize(#el_req{operation = delete, auth = ?PROVIDER(ProviderId), gri = #gri{aspect = instance}}, Storage) ->
    storage_logic:belongs_to_provider(Storage, ProviderId);

authorize(#el_req{operation = delete, auth = ?PROVIDER(ProviderId), gri = #gri{aspect = {space, _}}}, Storage) ->
    storage_logic:belongs_to_provider(Storage, ProviderId);

authorize(_, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc
%% Returns list of admin privileges needed to perform given operation.
%% @end
%%--------------------------------------------------------------------
-spec required_admin_privileges(entity_logic:req()) -> [privileges:oz_privilege()] | forbidden.
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

validate(#el_req{operation = create, gri = #gri{aspect = support}}) -> #{
    required => #{
        <<"token">> => {invite_token, ?SUPPORT_SPACE},
        <<"size">> => {integer, {not_lower_than, ?MINIMUM_SUPPORT_SIZE}}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {upgrade_legacy_support, _}}}) ->
    #{};

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    at_least_one => #{
        <<"name">> => {binary, name},
        <<"qos_parameters">> => {json, qos_parameters}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = {space, _}}}) -> #{
    required => #{
        <<"size">> => {integer, {not_lower_than, ?MINIMUM_SUPPORT_SIZE}}
    }
}.
