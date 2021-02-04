%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations regarding the Onezone service information.
%%% @end
%%%-------------------------------------------------------------------
-module(zone_logic_plugin).
-author("Wojciech Geisler").
-behaviour(entity_logic_plugin_behaviour).

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
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
fetch_entity(_) ->
    false.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given operation is supported based on operation, aspect and
%% scope (entity type is known based on the plugin itself).
%% @end
%%--------------------------------------------------------------------
-spec operation_supported(entity_logic:operation(), entity_logic:aspect(),
    entity_logic:scope()) -> boolean().
operation_supported(get, configuration, _) -> true;
operation_supported(get, test_image, _) -> true;
operation_supported(get, privileges, _) -> true;
operation_supported(get, {gui_message, _}, _) -> true;
operation_supported(update, {gui_message, _}, _) -> true;

operation_supported(_, _, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given {Aspect, Scope} pair is subscribable, i.e. clients can
%% subscribe to receive updates concerning the aspect of entity.
%% @end
%%--------------------------------------------------------------------
-spec is_subscribable(entity_logic:aspect(), entity_logic:scope()) ->
    boolean().
is_subscribable(_, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(_Req) ->
    ?ERROR_NOT_SUPPORTED.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = configuration}}, _) ->
    Version = oz_worker:get_release_version(),
    Resolver = compatibility:build_resolver(consistent_hashing:get_all_nodes(), []),
    SubdomainDelegationSupported = oz_worker:get_env(subdomain_delegation_supported, true),
    CompatibilityRegistryRevision = query_compatibility_registry(peek_current_registry_revision, [Resolver]),
    CompatibleOpVersions = query_compatibility_registry(get_compatible_versions, [Resolver, ?ONEZONE, Version, ?ONEPROVIDER]),
    {ok, #{
        <<"name">> => utils:undefined_to_null(oz_worker:get_name()),
        <<"version">> => Version,
        <<"build">> => oz_worker:get_build_version(),
        <<"domain">> => oz_worker:get_domain(),
        <<"subdomainDelegationSupported">> => SubdomainDelegationSupported,
        <<"supportedIdPs">> => auth_config:get_supported_idps_in_configuration_format(),
        <<"compatibilityRegistryRevision">> => CompatibilityRegistryRevision,
        <<"compatibleOneproviderVersions">> => CompatibleOpVersions
    }};

get(#el_req{gri = #gri{aspect = test_image}}, _) ->
    % Dummy image in png format. Used by gui to check connectivity.
    {ok, {binary, <<
        137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0,
        0, 1, 0, 0, 0, 1, 1, 3, 0, 0, 0, 37, 219, 86, 202, 0, 0, 0, 6, 80,
        76, 84, 69, 0, 0, 0, 255, 255, 255, 165, 217, 159, 221, 0, 0, 0, 9,
        112, 72, 89, 115, 0, 0, 14, 196, 0, 0, 14, 196, 1, 149, 43, 14, 27,
        0, 0, 0, 10, 73, 68, 65, 84, 8, 153, 99, 96, 0, 0, 0, 2, 0, 1, 244,
        113, 100, 166, 0, 0, 0, 0, 73, 69, 78, 68, 174, 66, 96, 130
    >>}};

get(#el_req{gri = #gri{aspect = privileges}}, _) ->
    {ok, #{
        <<"viewer">> => privileges:oz_viewer(),
        <<"admin">> => privileges:oz_admin()
    }};

get(#el_req{gri = #gri{aspect = {gui_message, MessageId}}}, _) ->
    case gui_message:get(MessageId) of
        {ok, #document{value = Message}} -> {ok, Message};
        {error, not_found} -> throw(?ERROR_NOT_FOUND)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{aspect = {gui_message, MessageId}}, data = Data}) ->
    UpdateFun = fun(Message) ->
        {ok, Message#gui_message{
            enabled = maps:get(<<"enabled">>, Data, Message#gui_message.enabled),
            body = maps:get(<<"body">>, Data, Message#gui_message.body)
        }}
    end,
    case gui_message:update(MessageId, UpdateFun) of
        {ok, _} -> ok;
        {error, not_found} -> throw(?ERROR_NOT_FOUND)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(_GRI) ->
    ?ERROR_NOT_SUPPORTED.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(#el_req{gri = #gri{aspect = _}}, _) ->
    % this function is never called when gri.id = undefined
    false.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(#el_req{operation = get, gri = #gri{aspect = configuration}}, _) ->
    true;

authorize(#el_req{operation = get, gri = #gri{aspect = test_image}}, _) ->
    true;

authorize(#el_req{operation = get, gri = #gri{aspect = privileges}}, _) ->
    true;

authorize(#el_req{operation = get, gri = #gri{aspect = {gui_message, _}}}, _) ->
    true;

authorize(#el_req{operation = update, gri = #gri{aspect = {gui_message, _}}}, _) ->
    % only root (onepanel) can perform this operation
    false;

authorize(_Req = #el_req{}, _) ->
    false.


%%--------------------------------------------------------------------
%% @doc
%% Returns list of admin privileges needed to perform given operation.
%% @end
%%--------------------------------------------------------------------
-spec required_admin_privileges(entity_logic:req()) -> [privileges:oz_privilege()] | forbidden.
required_admin_privileges(_) -> forbidden.


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
validate(#el_req{operation = update, gri = #gri{aspect = {gui_message, _}}}) ->
    #{
        optional => #{
            <<"enabled">> => {boolean, any},
            <<"body">> => {binary, any}
        }
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec query_compatibility_registry(Fun :: atom(), Args :: [term()]) -> term().
query_compatibility_registry(Fun, Args) ->
    Module = compatibility,
    case apply(Module, Fun, Args) of
        {ok, SuccessfulResult} ->
            SuccessfulResult;
        {error, _} = Error ->
            ?debug("Error querying registry - ~w:~w(~w)~nError was: ~p", [Module, Fun, Args, Error]),
            <<"unknown">>
    end.