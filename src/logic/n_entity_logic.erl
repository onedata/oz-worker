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
-module(n_entity_logic).
-author("Lukasz Opiola").
-behaviour(data_logic_behaviour).

-include("entity_logic.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([create/5, get/4, update/5, delete/4]).
-export([client_to_string/1]).

-type client() :: #client{}.
-type el_plugin() :: module().
-type operation() :: create | get | update | delete.
-type entity_id() :: od_user:id() | od_group:id() | od_space:id() |
od_share:id() | od_provider:id() | od_handle_service:id() | od_handle:id().
-type entity_type() :: od_user | od_group | od_space |
od_share | od_provider | od_handle_service | od_handle | oz_privileges.
-type entity() :: #od_user{} | #od_group{} | #od_space{} |
#od_share{} | #od_provider{} | #od_handle_service{} | #od_handle{}.
-type resource() :: atom() | {atom(), term()}.
-type data() :: maps:map() | binary().
-type result() :: ok | {ok, term()} | {error, Reason :: term()}.
-export_type([
    client/0,
    el_plugin/0,
    operation/0,
    entity_id/0,
    entity_type/0,
    entity/0,
    resource/0,
    data/0,
    result/0
]).

-record(request, {
    client = #client{} :: client(),
    el_plugin = undefined :: el_plugin(),
    entity_id = undefined :: undefined | entity_id(),
    entity = undefined :: undefined | entity(),
    operation = create :: operation(),
    resource = undefined :: resource(),
    data = #{} :: data()
}).


% TODO do typu
type_rule() -> [
    atom,
    list_of_atoms,
    binary,
    list_of_binaries,
    integer,
    float,
    json,
    token
].

value_rule() -> [
    any,
    non_empty,
    {not_lower_than, threshold},
    {not_greater_than, threshold},
    {between, low, high},
    [possible_values],
    fun() -> true end,  % TODO czy to potrzebne?
    {exists, fun(Id) -> true end},
    {not_exists, fun(Id) -> true end},
    token_type, % compatible only with token
    alias
].


create(Client, ELPlugin, EntityId, Resource, Data) ->
    try
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
                    check_existence_of_entity(Request))))
    catch
        throw:Error ->
            Error;
        Type:Message ->
            ?error_stacktrace("Error in entity_logic:create - ~p:~p", [
                Type, Message
            ]),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


get(Client, ELPlugin, EntityId, Resource) ->
    try
        Request = #request{
            client = Client,
            el_plugin = ELPlugin,
            entity_id = EntityId,
            operation = get,
            resource = Resource
        },
        call_get_resource(
            check_authorization(
                check_existence(Request)))
    catch
        throw:Error ->
            Error;
        Type:Message ->
            ?error_stacktrace("Error in entity_logic:get - ~p:~p", [
                Type, Message
            ]),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


update(Client, ELPlugin, EntityId, Resource, Data) ->
    try
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
                    check_existence(Request))))
    catch
        throw:Error ->
            Error;
        Type:Message ->
            ?error_stacktrace("Error in entity_logic:update - ~p:~p", [
                Type, Message
            ]),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


delete(Client, ELPlugin, EntityId, Resource) ->
    try
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
        end
    catch
        throw:Error ->
            Error;
        Type:Message ->
            ?error_stacktrace("Error in entity_logic:delete - ~p:~p", [
                Type, Message
            ]),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


client_to_string(?NOBODY) -> "nobody (unauthenticated user)";
client_to_string(?ROOT) -> "root";
client_to_string(?USER(UId)) -> str_utils:format("user:~s", [UId]);
client_to_string(?PROVIDER(PId)) -> str_utils:format("provider:~s", [PId]).


call_get_entity(Request) ->
    #request{el_plugin = ELPlugin, entity_id = EntityId} = Request,
    case ELPlugin:get_entity(EntityId) of
        {ok, Entity} ->
            Entity;
        ?ERROR_NOT_FOUND ->
            throw(?ERROR_NOT_FOUND)
    end.


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


call_create(Request) ->
    #request{
        client = Client,
        el_plugin = ELPlugin,
        entity_id = EntityId,
        resource = Resource,
        data = Data
    } = Request,
    ELPlugin:create(Client, EntityId, Resource, Data).


call_update(Request) ->
    #request{
        el_plugin = ELPlugin,
        entity_id = EntityId,
        resource = Resource,
        data = Data
    } = Request,
    ELPlugin:update(EntityId, Resource, Data).


call_delete(Request) ->
    #request{
        el_plugin = ELPlugin,
        entity_id = EntityId,
        resource = Resource
    } = Request,
    ELPlugin:delete(EntityId, Resource).


call_exists(Request) ->
    #request{
        entity_id = EntityId,
        % TODO potrzeba operation??
        resource = Resource,
        el_plugin = ELPlugin
    } = Request,
    % Call the plugin to obtain auth verification procedures
    case ELPlugin:exists(EntityId, Resource) of
        List when is_list(List) ->
            List;
        Item ->
            [Item]
    end.


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
        _:_ ->
            [false]
    end.


call_validate(Request) ->
    #request{
        operation = Operation,
        resource = Resource,
        el_plugin = ELPlugin
    } = Request,
    ELPlugin:validate(Operation, Resource).



check_existence_of_entity(#request{entity_id = undefined} = Request) ->
    % Undefined entity always exists (resource is not correlated with any entity).
    Request;
check_existence_of_entity(#request{entity = undefined} = Request) ->
    % This will throw NOT_FOUND if the entity cannot be retrieved.
    Request#request{entity = call_get_entity(Request)};
check_existence_of_entity(Request) ->
    % Entity is defined and fetched
    Request.


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


% TODO jesli juz jest atom to ne trzeba konwersji
check_validity(#request{data = Data} = Request) ->
    ValidatorsMap = call_validate(Request),
    % Get all types of validators validators
    Required = maps:get(required, ValidatorsMap, #{}),
    Optional = maps:get(optional, ValidatorsMap, #{}),
    AtLeastOne = maps:get(at_least_one, ValidatorsMap, #{}),
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
        end, Data, maps:keys(Required)),
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
    Request#request{data = Data4}.


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
check_type(list_of_atoms, _Key, []) ->
    [];
check_type(list_of_atoms, _Key, [Atom | _] = Atoms) when is_atom(Atom) ->
    Atoms;
check_type(list_of_atoms, Key, [Binary | _] = Binaries) when is_binary(Binary) ->
    try
        [binary_to_existing_atom(Bin, utf8) || Bin <- Binaries]
    catch
        _:_ ->
            throw(?ERROR_BAD_VALUE_LIST_OF_ATOMS(Key))
    end;
check_type(list_of_atoms, Key, _) ->
    throw(?ERROR_BAD_VALUE_LIST_OF_ATOMS(Key));
check_type(binary, _Key, Binary) when is_binary(Binary) ->
    Binary;
check_type(binary, _Key, Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
check_type(binary, Key, _) ->
    throw(?ERROR_BAD_VALUE_BINARY(Key));
check_type(list_of_binaries, _Key, []) ->
    [];
check_type(list_of_binaries, _Key, [Binary | _] = Binaries) when is_binary(Binary) ->
    Binaries;
check_type(list_of_binaries, _Key, [Atom | _] = Atoms) when is_atom(Atom) ->
    [atom_to_binary(A, utf8) || A <- Atoms];
check_type(list_of_binaries, Key, _) ->
    throw(?ERROR_BAD_VALUE_LIST_OF_BINARIES(Key));
check_type(integer, _Key, Int) when is_integer(Int) ->
    Int;
check_type(integer, Key, _) ->
    throw(?ERROR_BAD_VALUE_INTEGER(Key));
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
check_value(TypeRule, ValueRule, Key, _) ->
    ?error("Unknown {type, value} rule: {~p, ~p} for key: ~p", [
        TypeRule, ValueRule, Key
    ]),
    throw(?ERROR_INTERNAL_SERVER_ERROR).

