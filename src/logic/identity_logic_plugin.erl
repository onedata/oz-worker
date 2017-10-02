%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to owned_identity model.
%%% @end
%%%-------------------------------------------------------------------
-module(identity_logic_plugin).
-author("Lukasz Opiola").

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("cluster_worker/include/api_errors.hrl").

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
fetch_entity(_) ->
    ?ERROR_NOT_FOUND.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given operation is supported based on operation, aspect and
%% scope (entity type is known based on the plugin itself).
%% @end
%%--------------------------------------------------------------------
-spec operation_supported(entity_logic:operation(), entity_logic:aspect(),
    entity_logic:scope()) -> boolean().
operation_supported(create, {provider, _}, private) -> true;
operation_supported(get, {publickey, _}, private) -> true;
operation_supported(update, {publickey, _}, private) -> true;

operation_supported(_, _, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(#el_req{gri = #gri{aspect = {provider, Id}}, data = Data}) ->
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
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = {publickey, Id}}}, _) ->
    plugins:apply(identity_repository, get, [Id]).


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{aspect = {publickey, Id}}, data = #{<<"publicKey">> := EncodedPublicKey}}) ->
    case plugins:apply(identity_repository, publish, [Id, EncodedPublicKey]) of
        ok ->
            ok;
        {error, _Reason} ->
            ?warning("Unsucessful trial to override key of ~p", [Id]),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(_) ->
    ?ERROR_NOT_IMPLEMENTED.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(#el_req{gri = #gri{aspect = {publickey, Id}}}, _) ->
    case plugins:apply(identity_repository, get, [Id]) of
        {error, _} -> false;
        {ok, _} -> true
    end.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(#el_req{operation = create, gri = #gri{aspect = {provider, _}}}, _) ->
    true;

authorize(#el_req{operation = get, gri = #gri{aspect = {publickey, _}}}, _) ->
    true;

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {publickey, Id}}}, _) ->
    case Req#el_req.client of
        ?PROVIDER(ProviderId) ->
            Id =:= ProviderId;
        _ ->
            false
    end;

authorize(#el_req{operation = delete}, _) ->
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
validate(#el_req{operation = create, gri = #gri{aspect = {provider, _}}}) -> #{
    required => #{
        <<"publicKey">> => {binary, non_empty},
        <<"urls">> => {list_of_binaries, non_empty},
        <<"redirectionPoint">> => {binary, non_empty}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = {publickey, _}}}) -> #{
    required => #{
        <<"publicKey">> => {binary, non_empty}
    }
}.
