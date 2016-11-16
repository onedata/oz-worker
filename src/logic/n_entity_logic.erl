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

-export([create/4, get/3]).

create(Issuer, ELPlugin, Resource, Data) ->
    try
        check_authorization(Issuer, ELPlugin, create, Resource),
        check_validity(ELPlugin, create, Resource, Data),
        call_create(Issuer, ELPlugin, Resource, Data)
    catch
        throw:ElError ->
            ElError;
        Error:Message ->
            ?error_stacktrace("Error in data_logic:create - ~p:~p", [
                Error, Message
            ]),
            ?EL_INTERNAL_SERVER_ERROR
    end.

add_relation() ->
    ok.

get(Issuer, ELPlugin, Resource) ->  % Resource moze byc atom lub binary lub {RecordId, RecordResource}
    try
        {ok, Entity} = check_authorization(Issuer, ELPlugin, get, Resource),
            ?dump(Entity),
        % Entity might be already fetched after the auth procedures.
        % If so and the resource is connected to the entity, reuse it.
        case {Resource, Entity} of
            {Atom, _} when is_atom(Atom) ->
                call_get(ELPlugin, Atom);
            {Id, undefined} when is_binary(Id) ->
                Ent = call_get(ELPlugin, Id),
                {ok, Ent};
            {Id, Ent} when is_binary(Id) ->
                {ok, Ent};
            {{Id, SubResource}, undefined} ->
                Ent = call_get(ELPlugin, Id),
                call_get(ELPlugin, {Ent, SubResource});
            {{Id, SubResource}, Ent} ->
                call_get(ELPlugin, {Ent, SubResource})
        end
    catch
        throw:ElError ->
            ElError;
        Error:Message ->
            ?error_stacktrace("Error in data_logic:get - ~p:~p", [
                Error, Message
            ]),
            ?EL_INTERNAL_SERVER_ERROR
    end.

update(Issuer, EntityLogicPlugin, Resource, Data) ->
    ok.

delete(Issuer, EntityLogicPlugin, Resource) ->
    ok.


call_get(EntityLogicPlugin, Resource) ->
    case EntityLogicPlugin:get_impl(Resource) of
        {ok, E} ->
            E;
        ?EL_NOT_FOUND ->
            throw(?EL_NOT_FOUND)
    end.


call_create(Issuer, EntityLogicPlugin, Resource, Data) ->
    EntityLogicPlugin:create_impl(Issuer, Resource, Data).


check_authorization(Issuer, ELPlugin, Operation, Resource) ->
    % Call the plugin to obtain auth verification procedures
    Verificators = case ELPlugin:authorize_impl(Issuer, Operation, Resource) of
        List when is_list(List) ->
            List;
        Item ->
            [Item]
    end,
    check_authorization(Verificators, ELPlugin, Resource).
check_authorization([], _, _) ->
    throw(?EL_UNAUTHORIZED);
check_authorization([true | _], _, Entity) ->
    {ok, Entity};
check_authorization([false | _], _, _) ->
    throw(?EL_UNAUTHORIZED);
check_authorization([{external, Fun} | Tail], ELPlugin, Entity) ->
    case Fun() of
        true ->
            {ok, Entity};
        false ->
            check_authorization(Tail, ELPlugin, Entity)
    end;
check_authorization(List, ELPlugin, EntityId) when is_binary(EntityId) ->
    Entity = call_get(ELPlugin, EntityId),
    check_authorization(List, ELPlugin, Entity);
check_authorization(List, ELPlugin, {EntityId, _}) when is_binary(EntityId) ->
    Entity = call_get(ELPlugin, EntityId),
    check_authorization(List, ELPlugin, Entity);
check_authorization([{internal, Fun} | Tail], ELPlugin, Entity) ->
    case Fun(Entity) of
        true ->
            {ok, Entity};
        false ->
            check_authorization(Tail, ELPlugin, Entity)
    end.


check_validity(ELPlugin, Operation, Resource, Data) ->
    ValidatorsMap = ELPlugin:validate_impl(Operation, Resource),
    check_validity(maps:to_list(ValidatorsMap), Data).
check_validity([], _) ->
    ok;
check_validity([Validator | Tail], Data) ->
    {Key, Rule} = Validator,
    case maps:get(Key, Data, undefined) of
        undefined ->
            throw(?EL_MISSING_DATA(Key));
        Value ->
            case check_value(Rule, Value) of
                true ->
                    check_validity(Tail, Data);
                false ->
                    throw(?EL_BAD_DATA(Key))
            end
    end.


check_value(binary, Bin) when is_binary(Bin) -> true;
check_value(non_empty_binary, <<"">>) -> false;
check_value(non_empty_binary, Bin) when is_binary(Bin) -> true;
check_value(_, _) -> false.