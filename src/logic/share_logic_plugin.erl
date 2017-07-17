%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_share model.
%%% @end
%%%-------------------------------------------------------------------
-module(share_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("errors.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").

-type resource() :: entity | data | list.

-export_type([resource/0]).

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
-spec get_entity(EntityId :: entity_logic:entity_id()) ->
    {ok, entity_logic:entity()} | {error, Reason :: term()}.
get_entity(ShareId) ->
    case od_share:get(ShareId) of
        {ok, #document{value = Share}} ->
            {ok, Share};
        _ ->
            ?ERROR_NOT_FOUND
    end.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource based on EntityId, Resource identifier and Data.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: entity_logic:client(),
    EntityId :: entity_logic:entity_id(), Resource :: resource(),
    entity_logic:data()) -> entity_logic:result().
create(_Client, _, entity, Data) ->
    ShareId = maps:get(<<"shareId">>, Data),
    Name = maps:get(<<"name">>, Data),
    SpaceId = maps:get(<<"spaceId">>, Data),
    RootFileId = maps:get(<<"rootFileId">>, Data),
    Share = #document{key = ShareId, value = #od_share{
        name = Name,
        root_file = RootFileId,
        public_url = share_logic:share_id_to_public_url(ShareId)
    }},
    case od_share:create(Share) of
        {ok, ShareId} ->
            entity_graph:add_relation(
                od_share, ShareId,
                od_space, SpaceId
            ),
            {ok, ShareId};
        _ ->
            % This can potentially happen if a share with given share id
            % has been created between data verification and create
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource based on EntityId and Resource identifier.
%% @end
%%--------------------------------------------------------------------
-spec get(Client :: entity_logic:client(), EntityId :: entity_logic:entity_id(),
    Entity :: entity_logic:entity(), Resource :: resource()) ->
    entity_logic:result().
get(_, undefined, undefined, list) ->
    {ok, ShareDocs} = od_share:list(),
    {ok, [ShareId || #document{key = ShareId} <- ShareDocs]};

get(_, _ShareId, #od_share{} = Share, data) ->
    #od_share{
        name = Name, public_url = PublicUrl, space = SpaceId,
        root_file = RootFileId, handle = HandleId
    } = Share,
    {ok, #{
        <<"name">> => Name, <<"publicUrl">> => PublicUrl,
        <<"spaceId">> => SpaceId, <<"rootFileId">> => RootFileId,
        <<"handleId">> => HandleId
    }}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource based on EntityId, Resource identifier and Data.
%% @end
%%--------------------------------------------------------------------
-spec update(EntityId :: entity_logic:entity_id(), Resource :: resource(),
    entity_logic:data()) -> entity_logic:result().
update(ShareId, entity, #{<<"name">> := NewName}) ->
    {ok, _} = od_share:update(ShareId, #{name => NewName}),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource based on EntityId and Resource identifier.
%% @end
%%--------------------------------------------------------------------
-spec delete(EntityId :: entity_logic:entity_id(), Resource :: resource()) ->
    entity_logic:result().
delete(ShareId, entity) ->
    entity_graph:delete_with_relations(od_share, ShareId).


%%--------------------------------------------------------------------
%% @doc
%% Returns existence verificators for given Resource identifier.
%% Existence verificators can be internal, which means they operate on the
%% entity to which the resource corresponds, or external - independent of
%% the entity. If there are multiple verificators, they will be checked in
%% sequence until one of them returns true.
%% Implicit verificators 'true' | 'false' immediately stop the verification
%% process with given result.
%% @end
%%--------------------------------------------------------------------
-spec exists(Resource :: resource()) ->
    entity_logic:existence_verificator()|
    [entity_logic:existence_verificator()].
exists(_) ->
    % No matter the resource, return true if it belongs to a share
    {internal, fun(#od_share{}) ->
        % If the share with ShareId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


%%--------------------------------------------------------------------
%% @doc
%% Returns existence verificators for given Resource identifier.
%% Existence verificators can be internal, which means they operate on the
%% entity to which the resource corresponds, or external - independent of
%% the entity. If there are multiple verificators, they will be checked in
%% sequence until one of them returns true.
%% Implicit verificators 'true' | 'false' immediately stop the verification
%% process with given result.
%% @end
%%--------------------------------------------------------------------
-spec authorize(Operation :: entity_logic:operation(),
    EntityId :: entity_logic:entity_id(), Resource :: resource(),
    Client :: entity_logic:client()) ->
    entity_logic:authorization_verificator() |
    [authorization_verificator:existence_verificator()].
authorize(create, undefined, entity, ?USER(UserId)) ->
    {data_dependent, fun(Data) ->
        SpaceId = maps:get(<<"spaceId">>, Data, <<"">>),
        space_logic:has_eff_privilege(
            SpaceId, UserId, ?SPACE_MANAGE_SHARES
        )
    end};

authorize(get, _ShareId, list, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_SHARES_LIST);

authorize(get, _ShareId, entity, ?USER(UserId)) -> [
    auth_by_space_membership(UserId),
    auth_by_oz_privilege(UserId, ?OZ_SHARES_LIST)
];

authorize(get, _ShareId, data, ?USER(UserId)) -> [
    auth_by_space_membership(UserId),
    auth_by_oz_privilege(UserId, ?OZ_SHARES_LIST)
];

authorize(get, _ShareId, data, ?PROVIDER(ProviderId)) -> [
    auth_by_space_support(ProviderId)
];

authorize(update, _ShareId, entity, ?USER(UserId)) ->
    auth_by_space_privilege(UserId, ?SPACE_MANAGE_SHARES);

authorize(delete, _ShareId, entity, ?USER(UserId)) ->
    auth_by_space_privilege(UserId, ?SPACE_MANAGE_SHARES);

authorize(_, _, _, _) ->
    false.


%%--------------------------------------------------------------------
%% @doc
%% Returns validity verificators for given Operation and Resource identifier.
%% Returns a map with 'required', 'optional' and 'at_least_one' keys.
%% Under each of them, there is a map:
%%      Key => {type_verificator, value_verificator}
%% Which means how value of given Key should be validated.
%% @end
%%--------------------------------------------------------------------
-spec validate(Operation :: entity_logic:operation(),
    Resource :: resource()) ->
    entity_logic:validity_verificator().
validate(create, entity) -> #{
    required => #{
        <<"shareId">> => {binary, {not_exists, fun(Value) ->
            not share_logic:exists(Value)
        end}},
        <<"name">> => {binary, non_empty},
        <<"rootFileId">> => {binary, non_empty},
        <<"spaceId">> => {binary, {exists, fun(Value) ->
            space_logic:exists(Value)
        end}}
    }
};
validate(update, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty}
    }
}.


%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the entity with given id.
%% @end
%%--------------------------------------------------------------------
-spec entity_to_string(EntityId :: entity_logic:entity_id()) -> binary().
entity_to_string(ShareId) ->
    od_share:to_string(ShareId).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns authorization verificator that checks if given user belongs
%% to the space represented by entity.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_space_membership(UserId :: od_user:id()) ->
    entity_logic:authorization_verificator().
auth_by_space_membership(UserId) ->
    {internal, fun(#od_share{space = SpaceId}) ->
        space_logic:has_eff_user(SpaceId, UserId)
    end}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns authorization verificator that checks if given provider supports
%% the space to which share represented by entity belongs.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_space_support(ProviderId :: od_provider:id()) ->
    entity_logic:authorization_verificator().
auth_by_space_support(ProviderId) ->
    {internal, fun(#od_share{space = SpaceId}) ->
        space_logic:has_provider(SpaceId, ProviderId)
    end}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns authorization verificator that checks if given user has specific
%% effective privilege in the space to which the share represented by the entity
%% belongs.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_space_privilege(UserId :: od_user:id(),
    Privilege :: privileges:space_privilege()) ->
    entity_logic:authorization_verificator().
auth_by_space_privilege(UserId, Privilege) ->
    {internal, fun(#od_share{space = SpaceId}) ->
        space_logic:has_eff_privilege(SpaceId, UserId, Privilege)
    end}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns authorization verificator that checks if given user has specified
%% effective oz privilege.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_oz_privilege(UserId :: od_user:id(),
    Privilege :: privileges:oz_privilege()) ->
    entity_logic:authorization_verificator().
auth_by_oz_privilege(UserId, Privilege) ->
    {external, fun() ->
        user_logic:has_eff_oz_privilege(UserId, Privilege)
    end}.


