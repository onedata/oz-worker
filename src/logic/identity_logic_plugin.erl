%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc

%%% @end
%%%-------------------------------------------------------------------
-module(identity_logic_plugin).
-author("Lukasz Opiola").

-include("errors.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").

-type resource() :: {provider, od_provider:id()} | {publickey, Id :: binary()}.

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
get_entity(_) ->
    ?ERROR_NOT_FOUND.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource based on EntityId, Resource identifier and Data.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: entity_logic:client(),
    EntityId :: entity_logic:entity_id(), Resource :: resource(),
    entity_logic:data()) -> entity_logic:result().
create(_Client, undefined, {provider, Id}, Data) ->
    EncodedPublicKey = maps:get(<<"publicKey">>, Data),
    URLs = maps:get(<<"urls">>, Data),
    RedirectionPoint = maps:get(<<"redirectionPoint">>, Data),
    case plugins:apply(identity_repository, publish, [Id, EncodedPublicKey]) of
        ok ->
            Provider = #od_provider{name = Id, urls = URLs, redirection_point = RedirectionPoint},
            {ok, _} = od_provider:save(#document{key = Id, value = Provider}),
            ok;
        {error, Reason} ->
            ?warning("Unable to create new provider with ID ~p due to ~p", [Id, Reason]),
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
get(_, undefined, undefined, {publickey, Id}) ->
    plugins:apply(identity_repository, get, [Id]).


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource based on EntityId, Resource identifier and Data.
%% @end
%%--------------------------------------------------------------------
-spec update(EntityId :: entity_logic:entity_id(), Resource :: resource(),
    entity_logic:data()) -> entity_logic:result().
update(undefined, {publickey, Id}, #{<<"publicKey">> := EncodedPublicKey}) ->
    case plugins:apply(identity_repository, publish, [Id, EncodedPublicKey]) of
        ok ->
            ok;
        {error, _Reason} ->
            ?warning("Unsucessful trial to override key of ~p", [Id]),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource based on EntityId and Resource identifier.
%% @end
%%--------------------------------------------------------------------
-spec delete(EntityId :: entity_logic:entity_id(), Resource :: resource()) ->
    entity_logic:result().
delete(_, _) ->
    ?ERROR_NOT_IMPLEMENTED.


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
exists({publickey, Id}) ->
    {external, fun() ->
        case plugins:apply(identity_repository, get, [Id]) of
            {error, _} -> false;
            {ok, _} -> true
        end
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
authorize(create, undefined, {provider, _Id}, _Client) ->
    true;

authorize(get, undefined, {publickey, _Id}, _Client) ->
    true;

authorize(update, undefined, {publickey, Id}, ?PROVIDER(ProviderId)) ->
    Id =:= ProviderId;

authorize(delete, undefined, _, _Client) ->
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
validate(create, {provider, _Id}) -> #{
    required => #{
        <<"publicKey">> => {binary, non_empty},
        <<"urls">> => {list_of_binaries, non_empty},
        <<"redirectionPoint">> => {binary, non_empty}
    }
};
validate(update, {publickey, _Id}) -> #{
    required => #{
        <<"publicKey">> => {binary, non_empty}
    }
}.


%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the entity with given id.
%% @end
%%--------------------------------------------------------------------
-spec entity_to_string(EntityId :: entity_logic:entity_id()) -> binary().
entity_to_string(Id) ->
    <<"identity:", Id/binary>>.


