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
-include("datastore/oz_datastore_models.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/api_errors.hrl").

% Some of the types are just aliases for types from gs_protocol, this is
% for better readability of logic modules.
-type req() :: #el_req{}.
-type client() :: #client{}.
-type el_plugin() :: module().
-type operation() :: gs_protocol:operation().
-type entity_id() :: undefined | od_user:id() | od_group:id() | od_space:id() |
od_share:id() | od_provider:id() | od_handle_service:id() | od_handle:id().
-type entity_type() :: od_user | od_group | od_space | od_share | od_provider |
od_handle_service | od_handle | oz_privileges.
-type entity() :: undefined | #od_user{} | #od_group{} | #od_space{} |
#od_share{} | #od_provider{} | #od_handle_service{} | #od_handle{}.
-type aspect() :: gs_protocol:aspect().
-type scope() :: gs_protocol:scope().
-type data() :: gs_protocol:data().
-type gri() :: gs_protocol:gri().
-type auth_hint() :: gs_protocol:auth_hint().
-type error() :: {error, Reason :: term()}.
-type create_result() :: ok | {ok, {data, term()} | {fetched, gri(), term()} |
{not_fetched, gri()} | {not_fetched, gri(), auth_hint()}} | error().
-type get_result() :: {ok, term()} | error().
-type delete_result() :: ok | error().
-type update_result() :: ok | error().
-type result() :: create_result() | get_result() | update_result() | delete_result().

-type type_validator() :: any | atom | list_of_atoms | binary | alias |
list_of_binaries | integer | float | json | token | boolean | list_of_ipv4_addresses.

-type value_validator() :: any | non_empty |
fun((term()) -> boolean()) |
{not_lower_than, integer()} | {not_greater_than, integer()} |
{between, integer(), integer()} |
[term()] | % A list of accepted values
{exists, fun((entity_id()) -> boolean())} |
{not_exists, fun((entity_id()) -> boolean())} |
{relation_exists, atom(), binary(), atom(), binary(), fun((entity_id()) -> boolean())} |
token_logic:token_type() | % Compatible only with 'token' type validator
subdomain | domain |
email |
alias |
name | user_name.

% The 'aspect' key word allows to validate the data provided in aspect
% identifier.
-type validity_verificator() :: #{
required => #{Key :: binary() | {aspect, binary()} => {type_validator(), value_validator()}},
at_least_one => #{Key :: binary() | {aspect, binary()} => {type_validator(), value_validator()}},
optional => #{Key :: binary() | {aspect, binary()} => {type_validator(), value_validator()}}
}.

-export_type([
    client/0,
    el_plugin/0,
    operation/0,
    entity_id/0,
    entity_type/0,
    entity/0,
    aspect/0,
    scope/0,
    gri/0,
    data/0,
    auth_hint/0,
    create_result/0,
    get_result/0,
    update_result/0,
    delete_result/0,
    error/0,
    result/0,
    type_validator/0,
    value_validator/0,
    validity_verificator/0
]).

% Internal record containing the request data and state.
-record(state, {
    req = #el_req{} :: req(),
    plugin = undefined :: el_plugin(),
    entity = undefined :: entity()
}).

-export([handle/1, handle/2]).
-export([is_authorized/2]).
-export([client_to_string/1]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Handles an entity logic request expressed by a #el_req{} record.
%% @end
%%--------------------------------------------------------------------
-spec handle(req()) -> result().
handle(ElReq) ->
    handle(ElReq, undefined).


%%--------------------------------------------------------------------
%% @doc
%% Handles an entity logic request expressed by a #el_req{} record. Entity can
%% be provided if it was prefetched.
%% @end
%%--------------------------------------------------------------------
-spec handle(req(), Entity :: entity()) -> result().
handle(#el_req{gri = #gri{type = EntityType}} = ElReq, Entity) ->
    try
        ElPlugin = EntityType:entity_logic_plugin(),
        handle_unsafe(#state{
            req = ElReq, plugin = ElPlugin, entity = Entity
        })
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
%% @doc
%% Return if given client is authorized to perform given request, as specified
%% in the #el_req{} record. Entity can be provided if it was prefetched.
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(req(), entity()) -> boolean().
is_authorized(#el_req{gri = #gri{type = EntityType}} = ElReq, Entity) ->
    try
        ElPlugin = EntityType:entity_logic_plugin(),
        % Existence must be checked too, as sometimes authorization depends
        % on that.
        ensure_authorized(
            ensure_exists(#state{
                req = ElReq, plugin = ElPlugin, entity = Entity
            })),
        true
    catch
        _:_ ->
            false
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns a readable string representing provided client.
%% @end
%%--------------------------------------------------------------------
-spec client_to_string(Client :: client()) -> string().
client_to_string(?NOBODY) -> "nobody (unauthenticated client)";
client_to_string(?ROOT) -> "root";
client_to_string(?USER(UId)) -> str_utils:format("user:~s", [UId]);
client_to_string(?PROVIDER(PId)) -> str_utils:format("provider:~s", [PId]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Handles an entity logic request based on operation,
%% should be wrapped in a try-catch.
%% @end
%%--------------------------------------------------------------------
-spec handle_unsafe(#state{}) -> result().
handle_unsafe(State = #state{req = Req = #el_req{operation = create}}) ->
    Result = call_create(
        ensure_valid(
            ensure_authorized(
                fetch_entity(
                    ensure_operation_supported(
                        State))))),
    case {Result, Req} of
        {{ok, _}, #el_req{gri = #gri{aspect = instance}, client = Cl}} ->
            % If an entity instance is created, log an information about it
            % (it's a significant operation and this information might be useful).
            {EntType, EntId} = case Result of
                {ok, {fetched, #gri{type = Type, id = Id}, _}} -> {Type, Id};
                {ok, {not_fetched, #gri{type = Type, id = Id}}} -> {Type, Id};
                {ok, {not_fetched, #gri{type = Type, id = Id}, _}} -> {Type, Id}
            end,
            ?info("~s has been created by client: ~s", [
                EntType:to_string(EntId),
                client_to_string(Cl)
            ]),
            Result;
        _ ->
            Result
    end;

handle_unsafe(State = #state{req = #el_req{operation = get}}) ->
    call_get(
        ensure_authorized(
            ensure_exists(
                fetch_entity(
                    ensure_operation_supported(
                        State)))));

handle_unsafe(State = #state{req = #el_req{operation = update}}) ->
    call_update(
        ensure_valid(
            ensure_authorized(
                ensure_exists(
                    fetch_entity(
                        ensure_operation_supported(
                            State))))));

handle_unsafe(State = #state{req = Req = #el_req{operation = delete}}) ->
    Result = call_delete(
        ensure_authorized(
            ensure_exists(
                fetch_entity(
                    ensure_operation_supported(
                        State))))),
    case {Result, Req} of
        {ok, #el_req{gri = #gri{type = Type, id = Id, aspect = instance}, client = Cl}} ->
            % If an entity instance is deleted, log an information about it
            % (it's a significant operation and this information might be useful).
            ?info("~s has been deleted by client: ~s", [
                Type:to_string(Id),
                client_to_string(Cl)
            ]),
            ok;
        _ ->
            Result
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves the entity specified in request by calling back proper entity
%% logic plugin. Does nothing if the entity is prefetched, or GRI of the
%% request is not related to any entity.
%% @end
%%--------------------------------------------------------------------
-spec fetch_entity(State :: #state{}) -> #state{}.
fetch_entity(State = #state{entity = Entity}) when Entity /= undefined ->
    State;
fetch_entity(State = #state{req = #el_req{gri = #gri{id = undefined}}}) ->
    State;
fetch_entity(St = #state{plugin = Plugin, req = #el_req{gri = #gri{id = Id}}}) ->
    case Plugin:fetch_entity(Id) of
        {ok, Entity} ->
            St#state{entity = Entity};
        ?ERROR_NOT_FOUND ->
            throw(?ERROR_NOT_FOUND)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates an aspect of entity specified in request by calling back
%% proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_create(State :: #state{}) -> create_result().
call_create(#state{req = ElReq, plugin = Plugin}) ->
    Plugin:create(ElReq).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves an aspect specified in request by calling back
%% proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_get(State :: #state{}) -> get_result().
call_get(#state{req = ElReq, plugin = Plugin, entity = Entity}) ->
    Plugin:get(ElReq, Entity).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates an aspect of entity specified in request by calling back
%% proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_update(State :: #state{}) -> update_result().
call_update(#state{req = ElReq, plugin = Plugin}) ->
    Plugin:update(ElReq).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deletes an aspect of entity specified in request by calling back
%% proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_delete(State :: #state{}) -> delete_result().
call_delete(#state{req = ElReq, plugin = Plugin}) ->
    Plugin:delete(ElReq).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures requested operation is supported by calling back
%% proper entity logic plugin, throws a proper error of not.
%% @end
%%--------------------------------------------------------------------
-spec ensure_operation_supported(#state{}) -> #state{}.
ensure_operation_supported(State = #state{req = Req, plugin = Plugin}) ->
    Result = try
        #el_req{operation = Op, gri = #gri{aspect = Asp, scope = Scp}} = Req,
        Plugin:operation_supported(Op, Asp, Scp)
    catch _:_ ->
        % No need for log here, 'operation_supported' may crash depending on
        % what the request contains and this is expected.
        false
    end,
    case Result of
        true -> State;
        false -> throw(?ERROR_NOT_SUPPORTED)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures aspect of entity specified in request exists, throws on error.
%% @end
%%--------------------------------------------------------------------
-spec ensure_exists(State :: #state{}) -> #state{}.
ensure_exists(#state{req = #el_req{gri = #gri{id = undefined}}} = State) ->
    % Aspects where entity id is undefined always exist.
    State;
ensure_exists(State = #state{req = ElReq, plugin = Plugin, entity = Entity}) ->
    Result = try
        Plugin:exists(ElReq, Entity)
    catch _:_ ->
        % No need for log here, 'exists' may crash depending on what the
        % request contains and this is expected.
        false
    end,
    case Result of
        true -> State;
        false -> throw(?ERROR_NOT_FOUND)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures client specified in request is authorized to perform the request,
%% throws on error.
%% @end
%%--------------------------------------------------------------------
-spec ensure_authorized(State :: #state{}) -> #state{}.
ensure_authorized(State = #state{req = #el_req{client = ?ROOT}}) ->
    % Root client is authorized to do everything (that client is only available
    % internally).
    State;
ensure_authorized(State = #state{req = ElReq, plugin = Plugin, entity = Entity}) ->
    Result = try
        Plugin:authorize(ElReq, Entity)
    catch _:_ ->
        % No need for log here, 'authorize' may crash depending on what the
        % request contains and this is expected.
        false
    end,
    case Result of
        true ->
            State;
        false ->
            case ElReq#el_req.client of
                ?NOBODY ->
                    % The client was not authenticated -> unauthorized
                    throw(?ERROR_UNAUTHORIZED);
                _ ->
                    % The client was authenticated but cannot access the
                    % aspect -> forbidden
                    throw(?ERROR_FORBIDDEN)
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures data specified in request is valid, throws on error.
%% @end
%%--------------------------------------------------------------------
-spec ensure_valid(State :: #state{}) -> #state{}.
ensure_valid(State) ->
    #state{
        req = #el_req{gri = #gri{aspect = Aspect}, data = Data} = Req,
        plugin = Plugin
    } = State,
    ValidatorsMap = Plugin:validate(Req),
    % Get all types of validators
    Required = maps:get(required, ValidatorsMap, #{}),
    Optional = maps:get(optional, ValidatorsMap, #{}),
    AtLeastOne = maps:get(at_least_one, ValidatorsMap, #{}),
    % Artificially add 'aspect' key to Data to simplify validation code.
    % This key word allows to verify if data provided in aspect identifier
    % is valid.
    DataWithAspect = case Data of
        undefined -> #{aspect => Aspect};
        _ -> Data#{aspect => Aspect}
    end,
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
        end, DataWithAspect, maps:keys(Required)),
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
    % Remove 'aspect' key from data as it is no longer needed
    State#state{req = Req#el_req{data = maps:remove(aspect, Data4)}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs simple value conversion (if possible) and checks the type and value
%% of value for Key in Data. Takes into consideration special keys which are
%% in form {aspect, binary()}, that allows to validate data in aspect.
%% Data map must include 'aspect' key, that holds the aspect.
%% @end
%%--------------------------------------------------------------------
-spec transform_and_check_value(Key :: binary(), Data :: data(),
    Validator :: #{type_validator() => value_validator()}) ->
    {true, NewData :: data()} | false.
transform_and_check_value({aspect, Key}, Data, Validator) ->
    {TypeRule, ValueRule} = maps:get({aspect, Key}, Validator),
    %% Aspect validator supports only aspects that are tuples
    {_, Value} = maps:get(aspect, Data),
    % Ignore the returned value - the check will throw in case the value is
    % not valid
    transform_and_check_value(TypeRule, ValueRule, Key, Value),
    {true, Data};
transform_and_check_value(Key, Data, Validator) ->
    case maps:get(Key, Data, undefined) of
        undefined ->
            false;
        Value ->
            {TypeRule, ValueRule} = maps:get(Key, Validator),
            NewValue = transform_and_check_value(TypeRule, ValueRule, Key, Value),
            {true, Data#{Key => NewValue}}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs simple value conversion (if possible) and checks the type and value
%% of value.
%% @end
%%--------------------------------------------------------------------
-spec transform_and_check_value(TypeRule :: type_validator(),
    ValueRule :: value_validator(), Key :: binary(), Value :: term()) ->
    {true, NewData :: data()} | false.
transform_and_check_value(TypeRule, ValueRule, Key, Value) ->
    try
        NewValue = check_type(TypeRule, Key, Value),
        check_value(TypeRule, ValueRule, Key, NewValue),
        NewValue
    catch
        throw:Error ->
            throw(Error);
        Type:Message ->
            ?error_stacktrace(
                "Error in entity_logic:transform_and_check_value - ~p:~p",
                [Type, Message]
            ),
            throw(?ERROR_BAD_DATA(Key))
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
check_type(boolean, _Key, true) ->
    true;
check_type(boolean, _Key, false) ->
    false;
check_type(boolean, Key, _) ->
    throw(?ERROR_BAD_VALUE_BOOLEAN(Key));
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
check_type(list_of_binaries, Key, Values) ->
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
check_type(token, Key, <<>>) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_type(token, Key, Token) when is_binary(Token) ->
    case token_logic:deserialize(Token) of
        {ok, Macaroon} -> Macaroon;
        ?ERROR_BAD_MACAROON -> throw(?ERROR_BAD_VALUE_TOKEN(Key))
    end;
check_type(token, Key, Macaroon) ->
    case macaroon:is_macaroon(Macaroon) of
        true -> Macaroon;
        false -> throw(?ERROR_BAD_VALUE_TOKEN(Key))
    end;
check_type(alias, _Key, null) ->
    undefined;
check_type(alias, _Key, undefined) ->
    undefined;
check_type(alias, _Key, Binary) when is_binary(Binary) ->
    Binary;
check_type(alias, _Key, _) ->
    throw(?ERROR_BAD_VALUE_ALIAS);
check_type(list_of_ipv4_addresses, Key, ListOfIPs) ->
    try
        lists:map(fun(IP) ->
            case IP of
                _ when is_binary(IP) ->
                    {ok, IPTuple} = inet:parse_ipv4strict_address(
                        binary_to_list(IP)),
                    IPTuple;
                _ when is_tuple(IP) ->
                    {ok, IPTuple} = inet:getaddr(IP, inet),
                    IPTuple
            end
        end, ListOfIPs)
    catch _:_ ->
        throw(?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(Key))
    end;
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
            throw(?ERROR_BAD_VALUE_NOT_IN_RANGE(Key, Low, High))
    end;
check_value(binary, domain, Key, <<"">>) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_value(binary, domain, Key, Value) ->
    case size(Value) =< ?MAX_DOMAIN_LENGTH of
        true ->
            case re:run(Value, ?DOMAIN_VALIDATION_REGEXP, [{capture, none}]) of
                match -> ok;
                _ -> throw(?ERROR_BAD_VALUE_DOMAIN(Key))
            end;
        _ -> throw(?ERROR_BAD_VALUE_DOMAIN(Key))
    end;

check_value(binary, subdomain, Key, <<"">>) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_value(binary, subdomain, _Key, Value) ->
    case re:run(Value, ?SUBDOMAIN_VALIDATION_REGEXP, [{capture, none}]) of
        match -> % Check length
            % + 1 for the dot between subdomain and domain
            DomainLength = size(Value) + byte_size(oz_worker:get_domain()) + 1,
            case DomainLength =< ?MAX_DOMAIN_LENGTH of
                true -> ok;
                _ -> throw(?ERROR_BAD_VALUE_SUBDOMAIN)
            end;
        _ -> throw(?ERROR_BAD_VALUE_SUBDOMAIN)
    end;

check_value(binary, email, Key, <<"">>) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_value(binary, email, _Key, Value) ->
    case re:run(Value, ?EMAIL_VALIDATION_REGEXP, [{capture, none}]) of
        match ->
            case byte_size(Value) > ?EMAIL_MAX_LENGTH of
                true -> throw(?ERROR_BAD_VALUE_EMAIL);
                false -> ok
            end;
        _ -> throw(?ERROR_BAD_VALUE_EMAIL)
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
check_value(Type, {exists, VerifyFun}, Key, Val) when is_function(VerifyFun, 1) ->
    check_value(Type, non_empty, Key, Val),
    case VerifyFun(Val) of
        true ->
            ok;
        false ->
            throw(?ERROR_BAD_VALUE_ID_NOT_FOUND(Key))
    end;
check_value(Type, {not_exists, VerifyFun}, Key, Val) when is_function(VerifyFun, 1) ->
    check_value(Type, non_empty, Key, Val),
    case VerifyFun(Val) of
        true ->
            ok;
        false ->
            throw(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(Key))
    end;
check_value(_, {relation_exists, ChType, ChId, ParType, ParId, VerifyFun}, _Key, Val) when is_function(VerifyFun, 1) ->
    case VerifyFun(Val) of
        true ->
            ok;
        false ->
            throw(?ERROR_RELATION_DOES_NOT_EXIST(ChType, ChId, ParType, ParId))
    end;
check_value(token, any, _Key, _Macaroon) ->
    ok;
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
check_value(alias, alias, _Key, undefined) ->
    ok;
check_value(alias, alias, _Key, Value) ->
    case re:run(Value, ?ALIAS_VALIDATION_REGEXP, [{capture, none}]) of
        match -> ok;
        _ -> throw(?ERROR_BAD_VALUE_ALIAS)
    end;
check_value(binary, name, _Key, Value) ->
    case re:run(Value, ?NAME_VALIDATION_REGEXP, [{capture, none}, unicode, ucp]) of
        match -> ok;
        _ -> throw(?ERROR_BAD_VALUE_NAME)
    end;
check_value(binary, user_name,_Key, Value) ->
    case re:run(Value, ?USER_NAME_VALIDATION_REGEXP, [{capture, none}, unicode, ucp]) of
        match -> ok;
        _ -> throw(?ERROR_BAD_VALUE_USER_NAME)
    end;
check_value(TypeRule, ValueRule, Key, _) ->
    ?error("Unknown {type, value} rule: {~p, ~p} for key: ~p", [
        TypeRule, ValueRule, Key
    ]),
    throw(?ERROR_INTERNAL_SERVER_ERROR).
