%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all common logic concerning entities in onezone.
%%% It is used to process entity requests is a standardized way, i.e.:
%%%     # checks existence of given entity
%%%     # checks authorization of client to perform certain action
%%%     # checks validity of data provided in the request
%%%     # handles all errors in a uniform way
%%% It uses a callback system, accepting modules that implement
%%% entity_logic_plugin_behaviour.
%%% @end
%%%-------------------------------------------------------------------
-module(entity_logic).
-author("Lukasz Opiola").

-include("entity_logic.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([create/5, get/4, update/5, delete/4]).
-export([client_to_string/1]).

-type client() :: #client{}.
-type el_plugin() :: module().
-type operation() :: create | get | update | delete.
-type entity_id() :: undefined | od_user:id() | od_group:id() | od_space:id() |
od_share:id() | od_provider:id() | od_handle_service:id() | od_handle:id().
-type entity_type() :: od_user | od_group | od_space |
od_share | od_provider | od_handle_service | od_handle | oz_privileges.
-type entity() :: undefined | #od_user{} | #od_group{} | #od_space{} |
#od_share{} | #od_provider{} | #od_handle_service{} | #od_handle{}.
-type resource() :: atom() | {atom(), term()}.
-type data() :: maps:map() | binary().
-type result() :: ok | {ok, term()} | {error, Reason :: term()}.

-type existence_verificator() :: true | false |
{internal, fun((entity()) -> boolean())} |
{external, fun(() -> boolean())}.

-type authorization_verificator() :: true | false |
{data_dependent, fun((data()) -> boolean())} |
{internal, fun((entity()) -> boolean())} |
{external, fun(() -> boolean())}.

-type type_validator() :: any | atom | list_of_atoms | binary |
list_of_binaries | integer | float | json | token.

-type value_validator() :: any | non_empty |
{not_lower_than, integer()} | {not_greater_than, integer()} |
{between, integer(), integer()} |
[term()] | % A list of accepted values
{exists, fun((entity_id()) -> boolean())} |
{not_exists, fun((entity_id()) -> boolean())} |
token_logic:token_type() | % Compatible only with 'token' type validator
alias |  % Compatible only with 'binary' type validator
{resource_exists, ReadableIdentifier :: binary(), fun((entity_id()) -> boolean())}.

% The 'resource' key word allows to validate the data provided in resource
% identifier.
-type validity_verificator() :: #{
required => #{Key :: binary() | resource => {type_validator(), value_validator()}},
at_least_one => #{Key :: binary() | resource => {type_validator(), value_validator()}},
optional => #{Key :: binary() | resource => {type_validator(), value_validator()}}
}.

-export_type([
    client/0,
    el_plugin/0,
    operation/0,
    entity_id/0,
    entity_type/0,
    entity/0,
    resource/0,
    data/0,
    result/0,
    existence_verificator/0,
    authorization_verificator/0,
    type_validator/0,
    value_validator/0,
    validity_verificator/0
]).

% Internal record containing the request data.
-record(request, {
    client = #client{} :: client(),
    el_plugin = undefined :: el_plugin(),
    entity_id = undefined :: entity_id(),
    entity = undefined :: entity(),
    operation = create :: operation(),
    resource = undefined :: resource(),
    data = #{} :: data()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a resource using provided entity logic module.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: client(), ELPlugin :: el_plugin(),
    EntityId :: entity_id(), Resource :: resource(), Data :: data()) ->
    result().
create(Client, ELPlugin, EntityId, Resource, Data) ->
    handle_errors(fun create_internal/5, [
        Client, ELPlugin, EntityId, Resource, Data
    ]).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource using provided entity logic module.
%% @end
%%--------------------------------------------------------------------
-spec get(Client :: client(), ELPlugin :: el_plugin(),
    EntityId :: entity_id(), Resource :: resource()) -> result().
get(Client, ELPlugin, EntityId, Resource) ->
    handle_errors(fun get_internal/4, [
        Client, ELPlugin, EntityId, Resource
    ]).


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource using provided entity logic module.
%% @end
%%--------------------------------------------------------------------
-spec update(Client :: client(), ELPlugin :: el_plugin(),
    EntityId :: entity_id(), Resource :: resource(), Data :: data()) ->
    result().
update(Client, ELPlugin, EntityId, Resource, Data) ->
    handle_errors(fun update_internal/5, [
        Client, ELPlugin, EntityId, Resource, Data
    ]).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource using provided entity logic module.
%% @end
%%--------------------------------------------------------------------
-spec delete(Client :: client(), ELPlugin :: el_plugin(),
    EntityId :: entity_id(), Resource :: resource()) -> result().
delete(Client, ELPlugin, EntityId, Resource) ->
    handle_errors(fun delete_internal/4, [
        Client, ELPlugin, EntityId, Resource
    ]).


%%--------------------------------------------------------------------
%% @doc
%% Returns a readable string representing provided client.
%% @end
%%--------------------------------------------------------------------
-spec client_to_string(Client :: client()) -> string().
client_to_string(?NOBODY) -> "nobody (unauthenticated user)";
client_to_string(?ROOT) -> "root";
client_to_string(?USER(UId)) -> str_utils:format("user:~s", [UId]);
client_to_string(?PROVIDER(PId)) -> str_utils:format("provider:~s", [PId]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wraps implementations of create/get/update/delete and handles all
%% errors uniformly.
%% @end
%%--------------------------------------------------------------------
-spec handle_errors(Function :: fun(), Args :: [term()]) -> result().
handle_errors(Function, Args) ->
    try
        erlang:apply(Function, Args)
    catch
        throw:Error ->
            Error;
        Type:Message ->
            ?error_stacktrace("Unexpected error in entity_logic - ~p:~p", [
                Type, Message
            ]),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a resource using provided entity logic module.
%% Must be evaluated inside handle_errors function.
%% @end
%%--------------------------------------------------------------------
-spec create_internal(Client :: client(), ELPlugin :: el_plugin(),
    EntityId :: entity_id(), Resource :: resource(), Data :: data()) ->
    result().
create_internal(Client, ELPlugin, EntityId, Resource, Data) ->
    Request = #request{
        client = Client,
        el_plugin = ELPlugin,
        entity_id = EntityId,
        operation = create,
        data = Data,
        resource = Resource
    },
    call_create(
        check_validity(
            check_authorization(
                check_existence_of_entity(Request)))).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves a resource using provided entity logic module.
%% Must be evaluated inside handle_errors function.
%% @end
%%--------------------------------------------------------------------
-spec get_internal(Client :: client(), ELPlugin :: el_plugin(),
    EntityId :: entity_id(), Resource :: resource()) -> result().
get_internal(Client, ELPlugin, EntityId, Resource) ->
    Request = #request{
        client = Client,
        el_plugin = ELPlugin,
        entity_id = EntityId,
        operation = get,
        resource = Resource
    },
    call_get_resource(
        check_authorization(
            check_existence(Request))).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates a resource using provided entity logic module.
%% Must be evaluated inside handle_errors function.
%% @end
%%--------------------------------------------------------------------
-spec update_internal(Client :: client(), ELPlugin :: el_plugin(),
    EntityId :: entity_id(), Resource :: resource(), Data :: data()) ->
    result().
update_internal(Client, ELPlugin, EntityId, Resource, Data) ->
    Request = #request{
        client = Client,
        el_plugin = ELPlugin,
        entity_id = EntityId,
        operation = update,
        resource = Resource,
        data = Data
    },
    call_update(
        check_validity(
            check_authorization(
                check_existence(Request)))).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deletes a resource using provided entity logic module.
%% Must be evaluated inside handle_errors function.
%% @end
%%--------------------------------------------------------------------
-spec delete_internal(Client :: client(), ELPlugin :: el_plugin(),
    EntityId :: entity_id(), Resource :: resource()) -> result().
delete_internal(Client, ELPlugin, EntityId, Resource) ->
    Request = #request{
        client = Client,
        el_plugin = ELPlugin,
        entity_id = EntityId,
        operation = delete,
        resource = Resource
    },
    Result = call_delete(
        check_authorization(
            check_existence(Request))),
    case {Result, Resource} of
        {ok, entity} ->
            % If an entity is deleted, log an information about it
            % (it's a serious operation and this information might be useful).
            ?info("~s has been deleted by client: ~s", [
                ELPlugin:entity_to_string(EntityId),
                client_to_string(Client)
            ]),
            ok;
        _ ->
            Result
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves the entity specified in request by calling back
%% proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_get_entity(Request :: #request{}) -> entity().
call_get_entity(Request) ->
    #request{el_plugin = ELPlugin, entity_id = EntityId} = Request,
    case ELPlugin:get_entity(EntityId) of
        {ok, Entity} ->
            Entity;
        ?ERROR_NOT_FOUND ->
            throw(?ERROR_NOT_FOUND)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves a resource specified in request by calling back
%% proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_get_resource(Request :: #request{}) -> {ok, term()}.
call_get_resource(Request) ->
    #request{
        client = Client,
        el_plugin = ELPlugin,
        entity_id = EntityId,
        entity = Entity,
        resource = Resource
    } = Request,
    % Entity might be already prefetched, reuse it if possible.
    Result = case {EntityId, Entity, Resource} of
        {undefined, _, _} ->
            % EntityId is not defined, do not fetch entity.
            ELPlugin:get(Client, undefined, undefined, Resource);
        {_EntityId, undefined, entity} ->
            % EntityId is defined and asking for entity -> entity resource.
            % The Entity was not fetched yet, fetch and return it.
            {ok, call_get_entity(Request)};
        {_EntityId, Entity, entity} ->
            % EntityId is defined and asking for entity -> entity resource.
            % The Entity is already fetched, return it.
            {ok, Entity};
        {_EntityId, undefined, _} ->
            % EntityId is defined and some resource -> internal resource.
            % The Entity is already fetched, reuse it.
            FetchedEntity = call_get_entity(Request),
            ELPlugin:get(Client, EntityId, FetchedEntity, Resource);
        {_EntityId, Entity, _} ->
            % EntityId is defined and some resource -> internal resource.
            % The Entity was not fetched yet, fetch and use it.
            ELPlugin:get(Client, EntityId, Entity, Resource)
    end,
    case Result of
        {ok, _} ->
            Result;
        ?ERROR_NOT_FOUND ->
            throw(?ERROR_NOT_FOUND)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a resource specified in request by calling back
%% proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_create(Request :: #request{}) -> result().
call_create(Request) ->
    #request{
        client = Client,
        el_plugin = ELPlugin,
        entity_id = EntityId,
        resource = Resource,
        data = Data
    } = Request,
    ELPlugin:create(Client, EntityId, Resource, Data).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates a resource specified in request by calling back
%% proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_update(Request :: #request{}) -> result().
call_update(Request) ->
    #request{
        el_plugin = ELPlugin,
        entity_id = EntityId,
        resource = Resource,
        data = Data
    } = Request,
    ELPlugin:update(EntityId, Resource, Data).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deletes a resource specified in request by calling back
%% proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_delete(Request :: #request{}) -> result().
call_delete(Request) ->
    #request{
        el_plugin = ELPlugin,
        entity_id = EntityId,
        resource = Resource
    } = Request,
    ELPlugin:delete(EntityId, Resource).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calls back entity logic plugin to retrieve the list of
%% existence verificators for given resource.
%% @end
%%--------------------------------------------------------------------
-spec call_exists(Request :: #request{}) -> [existence_verificator()].
call_exists(Request) ->
    #request{
        resource = Resource,
        el_plugin = ELPlugin
    } = Request,
    % Call the plugin to obtain auth verification procedures
    case ELPlugin:exists(Resource) of
        List when is_list(List) ->
            List;
        Item ->
            [Item]
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calls back entity logic plugin to retrieve the list of
%% authorization verificators for given resource.
%% @end
%%--------------------------------------------------------------------
-spec call_authorize(Request :: #request{}) -> [authorization_verificator()].
call_authorize(#request{client = ?ROOT}) ->
    % Root client type is allowed to do everything
    [true];
call_authorize(Request) ->
    #request{
        client = Client,
        entity_id = EntityId,
        operation = Operation,
        resource = Resource,
        el_plugin = ELPlugin
    } = Request,
    % Call the plugin to obtain auth verification procedures
    try ELPlugin:authorize(Operation, EntityId, Resource, Client) of
        List when is_list(List) ->
            List;
        Item ->
            [Item]
    catch
        throw:Error ->
            throw(Error);
        Type:Message ->
            ?error_stacktrace("Cannot get authorization rules - ~p:~p~n"
            "Plugin: ~p~n"
            "Client: ~p~n"
            "Operation: ~p~n"
            "EntityId: ~p~n"
            "Resource: ~p~n", [
                Type, Message, ELPlugin, Client, Operation, EntityId, Resource
            ]),
            [false]
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calls back entity logic plugin to retrieve the list of
%% validity verificators for given resource.
%% @end
%%--------------------------------------------------------------------
-spec call_validate(Request :: #request{}) -> validity_verificator().
call_validate(Request) ->
    #request{
        operation = Operation,
        resource = Resource,
        el_plugin = ELPlugin
    } = Request,
    ELPlugin:validate(Operation, Resource).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures entity specified in request exists, throws on error.
%% @end
%%--------------------------------------------------------------------
-spec check_existence_of_entity(Request :: #request{}) -> #request{}.
check_existence_of_entity(#request{entity_id = undefined} = Request) ->
    % Undefined entity always exists (resource is not correlated with any entity).
    Request;
check_existence_of_entity(#request{entity = undefined} = Request) ->
    % This will throw NOT_FOUND if the entity cannot be retrieved.
    Request#request{entity = call_get_entity(Request)}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures resource specified in request exists, throws on error.
%% @end
%%--------------------------------------------------------------------
-spec check_existence(Request :: #request{}) -> #request{}.
check_existence(#request{entity_id = undefined} = Request) ->
    % Resources where entity id is undefined always exist.
    Request;
check_existence(Request) ->
    Verificators = call_exists(Request),
    check_existence(Verificators, Request).
check_existence([], _) ->
    throw(?ERROR_NOT_FOUND);
check_existence([true | _], Request) ->
    Request;
check_existence([false | _], _) ->
    throw(?ERROR_NOT_FOUND);
check_existence([{external, Fun} | Tail], Request) ->
    case Fun() of
        true ->
            Request;
        false ->
            check_existence(Tail, Request)
    end;
check_existence([{internal, _} | _] = List, #request{entity = undefined} = Req) ->
    Entity = call_get_entity(Req),
    check_existence(List, Req#request{entity = Entity});
check_existence([{internal, Fun} | Tail], #request{entity = Entity} = Req) ->
    case Fun(Entity) of
        true ->
            Req;
        false ->
            check_existence(Tail, Req)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures client specified in request is authorized to perform the request,
%% throws on error.
%% @end
%%--------------------------------------------------------------------
-spec check_authorization(Request :: #request{}) -> #request{}.
check_authorization(#request{client = Client} = Request) ->
    Result = try
        Verificators = call_authorize(Request),
        check_authorization(Verificators, Request)
    catch
        throw:Error ->
            throw(Error);
        Type:Message ->
            ?error_stacktrace("Error in entity_logic:check_authorization - ~p:~p", [
                Type, Message
            ]),
            false
    end,
    case Result of
        false ->
            case Client of
                ?NOBODY ->
                    % The client was not authenticated -> unauthorized
                    throw(?ERROR_UNAUTHORIZED);
                _ ->
                    % The client was authenticated but cannot access the
                    % resource -> forbidden
                    throw(?ERROR_FORBIDDEN)
            end;
        NewRequest ->
            NewRequest
    end.
check_authorization([], _) ->
    false;
check_authorization([true | _], Request) ->
    Request;
check_authorization([false | _], _) ->
    false;
check_authorization([{data_dependent, Fun} | Tail], #request{data = Data} = Req) ->
    case Fun(Data) of
        true ->
            Req;
        false ->
            check_authorization(Tail, Req)
    end;
check_authorization([{external, Fun} | Tail], Request) ->
    case Fun() of
        true ->
            Request;
        false ->
            check_authorization(Tail, Request)
    end;
check_authorization([{internal, _} | _] = List, #request{entity = undefined} = Req) ->
    Entity = call_get_entity(Req),
    check_authorization(List, Req#request{entity = Entity});
check_authorization([{internal, Fun} | Tail], #request{entity = Entity} = Req) ->
    case Fun(Entity) of
        true ->
            Req;
        false ->
            check_authorization(Tail, Req)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures data specified in request is valid, throws on error.
%% @end
%%--------------------------------------------------------------------
-spec check_validity(Request :: #request{}) -> #request{}.
check_validity(#request{data = Data, resource = Resource} = Request) ->
    ValidatorsMap = call_validate(Request),
    % Get all types of validators validators
    Required = maps:get(required, ValidatorsMap, #{}),
    Optional = maps:get(optional, ValidatorsMap, #{}),
    AtLeastOne = maps:get(at_least_one, ValidatorsMap, #{}),
    % Artificially add 'resource' key to Data to simplify validation code.
    % This key word allows to verify if data provided in resource identifier
    % is valid.
    DataWithResource = Data#{resource => Resource},
    % Start with required parameters. Transform the data if needed, fail when
    % any key is missing or cannot be validated.
    Data2 = lists:foldl(
        fun(Key, DataAcc) ->
            case transform_and_check_value(Key, DataAcc, Required) of
                false ->
                    throw(?ERROR_MISSING_REQUIRED_VALUE(Key));
                {true, NewData} ->
                    NewData
            end
        end, DataWithResource, maps:keys(Required)),
    % Now, optional parameters. Transform the data if needed, fail when
    % any of the keys exists in the data but cannot be validated.
    Data3 = lists:foldl(
        fun(Key, DataAcc) ->
            case transform_and_check_value(Key, DataAcc, Optional) of
                false ->
                    DataAcc;
                {true, NewData} ->
                    NewData
            end
        end, Data2, maps:keys(Optional)),
    % Finally, "at least one" parameters. Transform the data if needed, fail
    % when less than one key exists in the data or any of the keys cannot
    % be validated.
    {Data4, HasAtLeastOne} = lists:foldl(
        fun(Key, {DataAcc, HasAtLeastOneAcc}) ->
            case transform_and_check_value(Key, DataAcc, AtLeastOne) of
                false ->
                    {DataAcc, HasAtLeastOneAcc orelse false};
                {true, NewData} ->
                    {NewData, true}
            end
        end, {Data3, false}, maps:keys(AtLeastOne)),
    case {length(maps:keys(AtLeastOne)), HasAtLeastOne} of
        {_, true} ->
            ok;
        {0, false} ->
            ok;
        {_, false} ->
            throw(?ERROR_MISSING_AT_LEAST_ONE_VALUE(maps:keys(AtLeastOne)))
    end,
    % Remove 'resource' key from data as it is no longer needed
    Request#request{data = maps:remove(resource, Data4)}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs simple value conversion (if possible) and checks the type and value
%% of value for Key in Data.
%% @end
%%--------------------------------------------------------------------
-spec transform_and_check_value(Key :: binary(), Data :: data(),
    Validator :: {type_validator(), value_validator()}) ->
    {true, NewData :: data()} | false.
transform_and_check_value(Key, Data, Validator) ->
    case maps:get(Key, Data, undefined) of
        undefined ->
            false;
        Value ->
            {TypeRule, ValueRule} = maps:get(Key, Validator),
            try
                NewValue = check_type(TypeRule, Key, Value),
                check_value(TypeRule, ValueRule, Key, NewValue),
                {true, Data#{Key => NewValue}}
            catch
                throw:Error ->
                    throw(Error);
                Type:Message ->
                    ?error_stacktrace(
                        "Error in entity_logic:transform_and_check_value - ~p:~p",
                        [Type, Message]
                    ),
                    throw(?ERROR_BAD_DATA(Key))
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs simple value conversion (if possible) and checks the type
%% of value for Key in Data.
%% @end
%%--------------------------------------------------------------------
-spec check_type(type_validator(), Key :: binary(), Value :: term()) -> term().
check_type(any, _Key, Term) ->
    Term;
check_type(atom, _Key, Atom) when is_atom(Atom) ->
    Atom;
check_type(atom, Key, Binary) when is_binary(Binary) ->
    try
        binary_to_existing_atom(Binary, utf8)
    catch
        _:_ ->
            throw(?ERROR_BAD_VALUE_ATOM(Key))
    end;
check_type(atom, Key, _) ->
    throw(?ERROR_BAD_VALUE_ATOM(Key));
check_type(list_of_atoms, Key, Values) ->
    try
        lists:map(
            fun(Value) ->
                case Value of
                    Atom when is_atom(Atom) ->
                        Atom;
                    Bin when is_binary(Bin) ->
                        binary_to_existing_atom(Bin, utf8)
                end
            end, Values)
    catch
        _:_ ->
            throw(?ERROR_BAD_VALUE_LIST_OF_ATOMS(Key))
    end;
check_type(binary, _Key, Binary) when is_binary(Binary) ->
    Binary;
check_type(binary, _Key, Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
check_type(binary, Key, _) ->
    throw(?ERROR_BAD_VALUE_BINARY(Key));
check_type(list_of_binaries, Key, Values)  ->
    try
        lists:map(
            fun(Value) ->
                case Value of
                    Atom when is_atom(Atom) ->
                        atom_to_binary(Atom, utf8);
                    Bin when is_binary(Bin) ->
                        Bin
                end
            end, Values)
    catch
        _:_ ->
            throw(?ERROR_BAD_VALUE_LIST_OF_BINARIES(Key))
    end;
check_type(integer, Key, Bin) when is_binary(Bin) ->
    try
        binary_to_integer(Bin)
    catch _:_ ->
        throw(?ERROR_BAD_VALUE_INTEGER(Key))
    end;
check_type(integer, _Key, Int) when is_integer(Int) ->
    Int;
check_type(integer, Key, _) ->
    throw(?ERROR_BAD_VALUE_INTEGER(Key));
check_type(float, Key, Bin) when is_binary(Bin) ->
    try
        binary_to_float(Bin)
    catch _:_ ->
        try
            % Erlang will crash if the binary does not have a
            % floating point dot, but we still want to accept integers as floats.
            float(binary_to_integer(Bin))
        catch _:_ ->
            throw(?ERROR_BAD_VALUE_FLOAT(Key))
        end
    end;
check_type(float, _Key, Int) when is_integer(Int) ->
    float(Int);
check_type(float, _Key, Float) when is_float(Float) ->
    Float;
check_type(float, Key, _) ->
    throw(?ERROR_BAD_VALUE_FLOAT(Key));
check_type(json, _Key, JSON) when is_map(JSON) ->
    JSON;
check_type(json, Key, _) ->
    throw(?ERROR_BAD_VALUE_JSON(Key));
check_type(token, Key, Token) when is_binary(Token) ->
    case token_logic:deserialize(Token) of
        {ok, Macaroon} ->
            Macaroon;
        {error, macaroon_invalid} ->
            throw(?ERROR_BAD_VALUE_TOKEN(Key))
    end;
check_type(token, _Key, Macaroon) ->
    % Accept everything, it will be validated in check_value
    Macaroon;
check_type(Rule, Key, _) ->
    ?error("Unknown type rule: ~p for key: ~p", [Rule, Key]),
    throw(?ERROR_INTERNAL_SERVER_ERROR).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs simple value conversion (if possible) and checks the type
%% of value for Key in Data.
%% @end
%%--------------------------------------------------------------------
-spec check_value(type_validator(), value_validator(), Key :: binary(),
    Value :: term()) -> ok.
check_value(_, any, _Key, _) ->
    ok;
check_value(atom, non_empty, Key, '') ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_value(list_of_atoms, non_empty, Key, []) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_value(binary, non_empty, Key, <<"">>) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_value(list_of_binaries, non_empty, Key, []) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_value(json, non_empty, Key, Map) when map_size(Map) == 0 ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_value(_, non_empty, _Key, _) ->
    ok;
check_value(_, {not_lower_than, Threshold}, Key, Value) ->
    case Value >= Threshold of
        true ->
            ok;
        false ->
            throw(?ERROR_BAD_VALUE_TOO_LOW(Key, Threshold))
    end;
check_value(_, {not_greater_than, Threshold}, Key, Value) ->
    case Value =< Threshold of
        true ->
            ok;
        false ->
            throw(?ERROR_BAD_VALUE_TOO_HIGH(Key, Threshold))
    end;
check_value(_, {between, Low, High}, Key, Value) ->
    case Value >= Low andalso Value =< High of
        true ->
            ok;
        false ->
            throw(?ERROR_BAD_VALUE_NOT_BETWEEN(Key, Low, High))
    end;
check_value(_, AllowedVals, Key, Vals) when is_list(AllowedVals) andalso is_list(Vals) ->
    case ordsets:subtract(ordsets:from_list(Vals), ordsets:from_list(AllowedVals)) of
        [] ->
            ok;
        _ ->
            throw(?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(Key, AllowedVals))
    end;
check_value(_, AllowedVals, Key, Val) when is_list(AllowedVals) ->
    case lists:member(Val, AllowedVals) of
        true ->
            ok;
        _ ->
            throw(?ERROR_BAD_VALUE_NOT_ALLOWED(Key, AllowedVals))
    end;
check_value(_, VerifyFun, Key, Vals) when is_function(VerifyFun, 1) andalso is_list(Vals) ->
    case lists:all(VerifyFun, Vals) of
        true ->
            ok;
        false ->
            throw(?ERROR_BAD_DATA(Key))
    end;
check_value(_, VerifyFun, Key, Val) when is_function(VerifyFun, 1) ->
    case VerifyFun(Val) of
        true ->
            ok;
        false ->
            throw(?ERROR_BAD_DATA(Key))
    end;
check_value(_, {exists, VerifyFun}, Key, Val) when is_function(VerifyFun, 1) ->
    case VerifyFun(Val) of
        true ->
            ok;
        false ->
            throw(?ERROR_BAD_VALUE_ID_NOT_FOUND(Key))
    end;
check_value(_, {not_exists, VerifyFun}, Key, Val) when is_function(VerifyFun, 1) ->
    case VerifyFun(Val) of
        true ->
            ok;
        false ->
            throw(?ERROR_BAD_VALUE_ID_OCCUPIED(Key))
    end;
check_value(token, TokenType, Key, Macaroon) ->
    case token_logic:validate(Macaroon, TokenType) of
        ok ->
            ok;
        inexistent ->
            throw(?ERROR_BAD_VALUE_TOKEN(Key));
        bad_macaroon ->
            throw(?ERROR_BAD_VALUE_TOKEN(Key));
        bad_type ->
            throw(?ERROR_BAD_VALUE_BAD_TOKEN_TYPE(Key))
    end;
check_value(_, alias, Key, Value) ->
    RegExpValidation = (match =:= re:run(
        Value, ?ALIAS_VALIDATION_REGEXP, [{capture, none}]
    )),
    case {Value, RegExpValidation} of
        {?EMPTY_ALIAS, _} ->
            throw(?ERROR_BAD_VALUE_EMPTY(Key));
        {<<?NO_ALIAS_UUID_PREFIX, _/binary>>, _} ->
            throw(?ERROR_BAD_VALUE_ALIAS_WRONG_PREFIX(Key));
        {_, false} ->
            throw(?ERROR_BAD_VALUE_ALIAS(Key));
        {_, true} -> ok
    end;
check_value(_, {resource_exists, ReadableIdentifier, VerifyFun}, _Key, Val) when is_function(VerifyFun, 1) ->
    case VerifyFun(Val) of
        true ->
            ok;
        false ->
            throw(?ERROR_RESOURCE_DOES_NOT_EXIST(ReadableIdentifier))
    end;
check_value(TypeRule, ValueRule, Key, _) ->
    ?error("Unknown {type, value} rule: {~p, ~p} for key: ~p", [
        TypeRule, ValueRule, Key
    ]),
    throw(?ERROR_INTERNAL_SERVER_ERROR).

