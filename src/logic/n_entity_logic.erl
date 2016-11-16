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

-export([create/4, get/3, add_relation/5, update/4, delete/3]).

create(Issuer, ELPlugin, Resource, Data) ->
    try
        check_authorization(Issuer, ELPlugin, create, Resource),
        {ok, ValidatedData} = check_validity(ELPlugin, create, Resource, Data),
        ?dump(ValidatedData),
        call_create(Issuer, ELPlugin, Resource, ValidatedData)
    catch
        throw:ElError ->
            ElError;
        Error:Message ->
            ?error_stacktrace("Error in data_logic:create - ~p:~p", [
                Error, Message
            ]),
            ?EL_INTERNAL_SERVER_ERROR
    end.

get(Issuer, ELPlugin, Resource) ->
    try
        {ok, PrefetchedEntity} = check_existence(ELPlugin, Resource),
        {ok, PrefetchedEntity2} = check_authorization(
            Issuer, ELPlugin, get, Resource, PrefetchedEntity
        ),
        % Entity might be already fetched after the auth procedures.
        % If so and the resource is connected to the entity, reuse it.
        case {Resource, PrefetchedEntity2} of
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

add_relation(Issuer, ELPlugin, Resource, ChildModel, ChildId) ->
    try
        check_authorization(Issuer, ELPlugin, add_relation, Resource),
        call_add_relation(ELPlugin, Resource, ChildModel, ChildId)
    catch
        throw:ElError ->
            ElError;
        Error:Message ->
            ?error_stacktrace("Error in data_logic:add_relation - ~p:~p", [
                Error, Message
            ]),
            ?EL_INTERNAL_SERVER_ERROR
    end.

update(Issuer, ELPlugin, Resource, Data) ->
    try
        check_authorization(Issuer, ELPlugin, update, Resource),
        {ok, ValidatedData} = check_validity(ELPlugin, update, Resource, Data),
        ?dump(ValidatedData),
        call_update(ELPlugin, Resource, ValidatedData)
    catch
        throw:ElError ->
            ElError;
        Error:Message ->
            ?error_stacktrace("Error in data_logic:update - ~p:~p", [
                Error, Message
            ]),
            ?EL_INTERNAL_SERVER_ERROR
    end.

delete(Issuer, ELPlugin, Resource) ->
    try
        check_authorization(Issuer, ELPlugin, delete, Resource),
        call_delete(ELPlugin, Resource)
    catch
        throw:ElError ->
            ElError;
        Error:Message ->
            ?error_stacktrace("Error in data_logic:update - ~p:~p", [
                Error, Message
            ]),
            ?EL_INTERNAL_SERVER_ERROR
    end.


call_get(EntityLogicPlugin, Resource) ->
    case EntityLogicPlugin:get_impl(Resource) of
        {ok, E} ->
            E;
        ?EL_NOT_FOUND ->
            throw(?EL_NOT_FOUND)
    end.


call_create(Issuer, ELPlugin, Resource, Data) ->
    ELPlugin:create_impl(Issuer, Resource, Data).


call_add_relation(ELPlugin, Resource, ChildModel, ChildId) ->
    ELPlugin:add_relation_impl(Resource, ChildModel, ChildId).


call_update(ELPlugin, Resource, Data) ->
    ELPlugin:update_impl(Resource, Data).


call_delete(ELPlugin, Resource) ->
    ELPlugin:delete_impl(Resource).


check_existence(ELPlugin, Resource) ->
    case ELPlugin:exists_impl(Resource) of
        true ->
            {true, undefined};
        {true, PrefetchedEntity} ->
            {true, PrefetchedEntity};
        false ->
            throw(?EL_NOT_FOUND)
    end.


check_authorization(Issuer, ELPlugin, Operation, Resource) ->
    check_authorization(Issuer, ELPlugin, Operation, Resource, undefined).
check_authorization(Issuer, ELPlugin, Operation, Resource, PrefetchedEntity) ->
    % Call the plugin to obtain auth verification procedures
    Verificators = case ELPlugin:authorize_impl(Issuer, Operation, Resource) of
        List when is_list(List) ->
            List;
        Item ->
            [Item]
    end,
    Entity = case PrefetchedEntity of
        undefined ->
            Resource;
        _ ->
            PrefetchedEntity
    end,
    check_authorization(Verificators, ELPlugin, Entity).
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
    {ok, Data4}.


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


check_value(binary, <<"">>) ->
    empty;
check_value(binary, Bin) when is_binary(Bin) ->
    true;
check_value(atom, <<"">>) ->
    empty;
check_value(atom, Bin) when is_binary(Bin) ->
    try binary_to_existing_atom(Bin, utf8) of
        Atom ->
            {true, Atom}
    catch _:_ ->
        false
    end;
check_value({atom, _}, <<"">>) ->
    empty;
check_value({atom, AllowedValues}, Bin) when is_binary(Bin) ->
    try binary_to_existing_atom(Bin, utf8) of
        Atom ->
            case lists:member(Atom, AllowedValues) of
                false ->
                    false;
                true ->
                    {true, Atom}
            end
    catch _:_ ->
        false
    end;
check_value(_, _) ->
    false.
