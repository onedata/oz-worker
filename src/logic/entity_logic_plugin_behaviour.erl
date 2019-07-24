%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour should be implemented by modules that implement entity logic
%%% operations. Every entity logic plugin serves as a middleware between
%%% API and datastore in the context of specific entity type (od_xxx records).
%%% @end
%%%-------------------------------------------------------------------
-module(entity_logic_plugin_behaviour).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity and its revision from datastore based on EntityId.
%% Should return ?ERROR_NOT_FOUND if the entity does not exist.
%% @end
%%--------------------------------------------------------------------
-callback fetch_entity(entity_logic:entity_id()) ->
    {ok, entity_logic:versioned_entity()} | entity_logic:error().


%%--------------------------------------------------------------------
%% @doc
%% Determines if given operation is supported based on operation, aspect and
%% scope (entity type is known based on the plugin itself).
%% @end
%%--------------------------------------------------------------------
-callback operation_supported(entity_logic:operation(), entity_logic:aspect(),
    entity_logic:scope()) -> boolean().


%%--------------------------------------------------------------------
%% @doc
%% Determines if given {Aspect, Scope} pair is subscribable, i.e. clients can
%% subscribe to receive updates concerning the aspect of entity.
%% @end
%%--------------------------------------------------------------------
-callback is_subscribable(entity_logic:aspect(), entity_logic:scope()) ->
    boolean().


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-callback create(entity_logic:req()) -> entity_logic:create_result().


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-callback get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-callback update(entity_logic:req()) -> entity_logic:update_result().


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-callback delete(entity_logic:req()) -> entity_logic:delete_result().


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-callback exists(entity_logic:req(), entity_logic:entity()) -> boolean().


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-callback authorize(entity_logic:req(), entity_logic:entity()) -> boolean().


%%--------------------------------------------------------------------
%% @doc
%% Returns list of admin privileges needed to perform given operation.
%% @end
%%--------------------------------------------------------------------
-callback required_admin_privileges(entity_logic:req()) -> [privileges:oz_privilege()] | forbidden.


%%--------------------------------------------------------------------
%% @doc
%% Returns validity verificators for given request.
%% Returns a map with 'required', 'optional' and 'at_least_one' keys.
%% Under each of them, there is a map:
%%      Key => {type_verificator, value_verificator}
%% Which means how value of given Key should be validated.
%% @end
%%--------------------------------------------------------------------
-callback validate(entity_logic:req()) -> entity_logic:validity_verificator().
