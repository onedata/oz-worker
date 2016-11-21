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

-include("entity_logic_errors.hrl").
-include_lib("ctool/include/logging.hrl").

-export([create/5, get/4, update/5, delete/4, consume_token/4]).


-record(request, {
    issuer = undefined :: term(), % TODO
    el_plugin = undefined :: undefined | atom(),
    entity_id = undefined :: undefined | binary(),
    entity = undefined :: undefined | term(),
    operation = create :: atom(), % TODO
    resource = undefined :: undefined | term(),
    data = #{} :: maps:map()
}).


create(Issuer, ELPlugin, EntityId, Resource, Data) ->
    try
        Request = #request{
            issuer = Issuer,
            el_plugin = ELPlugin,
            entity_id = EntityId,
            operation = create,
            data = Data,
            resource = Resource
        },
        call_create(
            check_validity(
                check_authorization(
                    check_existence(Request))))
    catch
        throw:ElError ->
            ElError;
        Error:Message ->
            ?error_stacktrace("Error in data_logic:create - ~p:~p", [
                Error, Message
            ]),
            ?EL_INTERNAL_SERVER_ERROR
    end.


get(Issuer, ELPlugin, EntityId, Resource) ->
    try
        Request = #request{
            issuer = Issuer,
            el_plugin = ELPlugin,
            entity_id = EntityId,
            operation = get,
            resource = Resource
        },
        call_get_resource(
            check_authorization(
                check_existence(Request)))
    catch
        throw:ElError ->
            ElError;
        Error:Message ->
            ?error_stacktrace("Error in data_logic:get - ~p:~p", [
                Error, Message
            ]),
            ?EL_INTERNAL_SERVER_ERROR
    end.


update(Issuer, ELPlugin, EntityId, Resource, Data) ->
    try
        Request = #request{
            issuer = Issuer,
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
        throw:ElError ->
            ElError;
        Error:Message ->
            ?error_stacktrace("Error in data_logic:update - ~p:~p", [
                Error, Message
            ]),
            ?EL_INTERNAL_SERVER_ERROR
    end.


delete(Issuer, ELPlugin, EntityId, Resource) ->
    try
        Request = #request{
            issuer = Issuer,
            el_plugin = ELPlugin,
            entity_id = EntityId,
            operation = delete,
            resource = Resource
        },
        call_delete(
            check_authorization(
                check_existence(Request)))
    catch
        throw:ElError ->
            ElError;
        Error:Message ->
            ?error_stacktrace("Error in data_logic:update - ~p:~p", [
                Error, Message
            ]),
            ?EL_INTERNAL_SERVER_ERROR
    end.


%%add_relation(Issuer, ELPlugin, Resource, ChildModel, ChildId) ->
%%    try
%%%%        check_authorization(Issuer, ELPlugin, add_relation, Resource),
%%%%        call_add_relation(ELPlugin, Resource, ChildModel, ChildId)
%%        ok
%%    catch
%%        throw:ElError ->
%%            ElError;
%%        Error:Message ->
%%            ?error_stacktrace("Error in data_logic:add_relation - ~p:~p", [
%%                Error, Message
%%            ]),
%%            ?EL_INTERNAL_SERVER_ERROR
%%    end.


consume_token(Issuer, ELPlugin, Resource, Token) ->
    try
%%        check_authorization(Issuer, ELPlugin, consume_token, Resource),
%%        call_add_relation(ELPlugin, Resource, ChildModel, ChildId)
        ok
    catch
        throw:ElError ->
            ElError;
        Error:Message ->
            ?error_stacktrace("Error in data_logic:consume_token - ~p:~p", [
                Error, Message
            ]),
            ?EL_INTERNAL_SERVER_ERROR
    end.


call_get_entity(Request) ->
    #request{el_plugin = ELPlugin, entity_id = EntityId} = Request,
    io:format("> call_get_entity: ~p~n", [EntityId]),
    case ELPlugin:get_entity(EntityId) of
        {ok, Entity} ->
            Entity;
        ?EL_NOT_FOUND ->
            throw(?EL_NOT_FOUND)
    end.


call_get_resource(Request) ->
    #request{
        issuer = Issuer,
        el_plugin = ELPlugin,
        entity_id = EntityId,
        entity = Entity,
        resource = Resource
    } = Request,
    % Entity might be already prefetched, reuse it if possible.
    Result = case {EntityId, Entity, Resource} of
        {undefined, _, _} ->
            % EntityId is not defined -> external resource
            io:format("> get_external: ~p~n", [{Issuer, Resource}]),
            ELPlugin:get_external(Issuer, Resource);
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
            io:format("> get_internal: ~p~n", [{Issuer, EntityId, freshly_fetched_entity, Resource}]),
            ELPlugin:get_internal(Issuer, EntityId, FetchedEntity, Resource);
        {_EntityId, Entity, _} ->
            % EntityId is defined and some resource -> internal resource.
            % The Entity was not fetched yet, fetch and use it.
            io:format("> get_internal: ~p~n", [{Issuer, EntityId, prefetched_entity, Resource}]),
            ELPlugin:get_internal(Issuer, EntityId, Entity, Resource)
    end,
    case Result of
        {ok, _} ->
            Result;
        ?EL_NOT_FOUND ->
            throw(?EL_NOT_FOUND)
    end.


call_create(Request) ->
    #request{
        issuer = Issuer,
        el_plugin = ELPlugin,
        entity_id = EntityId,
        resource = Resource,
        data = Data
    } = Request,
    ELPlugin:create_impl(Issuer, EntityId, Resource, Data).

%%% TODO
%%call_add_relation(ELPlugin, Resource, ChildModel, ChildId) ->
%%    ELPlugin:add_relation_impl(Resource, ChildModel, ChildId).


call_update(Request) ->
    #request{
        el_plugin = ELPlugin,
        resource = Resource,
        data = Data
    } = Request,
    ELPlugin:update_impl(Resource, Data).


call_delete(Request) ->
    #request{
        el_plugin = ELPlugin,
        resource = Resource
    } = Request,
    ELPlugin:delete_impl(Resource).


call_exists(Request) ->
    #request{
        entity_id = EntityId,
        % TODO potrzeba operation??
        resource = Resource,
        el_plugin = ELPlugin
    } = Request,
    % Call the plugin to obtain auth verification procedures
    case ELPlugin:exists_impl(EntityId, Resource) of
        List when is_list(List) ->
            List;
        Item ->
            [Item]
    end.


call_authorize(Request) ->
    #request{
        issuer = Issuer,
        entity_id = EntityId,
        operation = Operation,
        resource = Resource,
        el_plugin = ELPlugin
    } = Request,
    % Call the plugin to obtain auth verification procedures
    case ELPlugin:authorize_impl(Issuer, Operation, EntityId, Resource) of
        List when is_list(List) ->
            List;
        Item ->
            [Item]
    end.


call_validate(Request) ->
    #request{
        operation = Operation,
        resource = Resource,
        el_plugin = ELPlugin
    } = Request,
    ELPlugin:validate_impl(Operation, Resource).


check_existence(Request) ->
    Verificators = call_exists(Request),
    check_authorization(Verificators, Request).
check_existence([], _) ->
    throw(?EL_NOT_FOUND);
check_existence([true | _], Request) ->
    Request;
check_existence([false | _], _) ->
    throw(?EL_UNAUTHORIZED);
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


check_authorization(Request) ->
    Verificators = call_authorize(Request),
    check_authorization(Verificators, Request).
check_authorization([], _) ->
    throw(?EL_UNAUTHORIZED);
check_authorization([true | _], Request) ->
    Request;
check_authorization([false | _], _) ->
    throw(?EL_UNAUTHORIZED);
check_authorization([{external, Fun} | Tail], Request) ->
    case Fun() of
        true ->
            Request;
        false ->
            check_authorization(Tail, Request)
    end;
check_authorization([{internal, _} | _] = List, #request{entity = undefined} = Req) ->
    Entity = call_get_entity(Req),
    check_existence(List, Req#request{entity = Entity});
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
                    throw(?EL_MISSING_REQUIRED_DATA(Key));
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
            throw(?EL_MISSING_ANY_DATA)
    end,
    Request#request{data = Data4}.


transform_and_check_value(Key, Data, Validator) ->
    case maps:get(Key, Data, undefined) of
        undefined ->
            false;
        Value ->
            Rule = maps:get(Key, Validator),
            case check_value(Rule, Value) of
                true ->
                    {true, Data};
                {true, NewValue} ->
                    {true, Data#{Key => NewValue}};
                false ->
                    throw(?EL_BAD_DATA(Key));
                empty ->
                    throw(?EL_EMPTY_DATA(Key))
            end
    end.


check_value(_, <<"">>) ->
    empty;
check_value(_, "") ->
    empty;
check_value(_, '') ->
    empty;
check_value(binary, Bin) when is_binary(Bin) ->
    true;
check_value({binary, AllowedValues}, Bin) when is_binary(Bin) ->
    lists:member(Bin, AllowedValues);
check_value(atom, Bin) when is_binary(Bin) ->
    try binary_to_existing_atom(Bin, utf8) of
        Atom ->
            check_value(atom, Atom)
    catch _:_ ->
        false
    end;
check_value(atom, Atom) when is_atom(Atom) ->
    true;
check_value({atom, AllowedValues}, Bin) when is_binary(Bin) ->
    try binary_to_existing_atom(Bin, utf8) of
        Atom ->
            check_value({atom, AllowedValues}, Atom)
    catch _:_ ->
        false
    end;
check_value({atom, AllowedValues}, Atom) when is_atom(Atom) ->
    case lists:member(Atom, AllowedValues) of
        false ->
            false;
        true ->
            {true, Atom}
    end;
check_value(Rule, _) ->
    ?error("Unknown validate rule: ~p", [Rule]),
    throw(?EL_INTERNAL_SERVER_ERROR).
