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
-type el_plugin() :: module().
-type operation() :: gs_protocol:operation().
-type entity_id() :: undefined | od_user:id() | od_group:id() | od_space:id() |
od_share:id() | od_provider:id() | od_handle_service:id() | od_handle:id() | od_cluster:id().
-type entity_type() :: od_user | od_group | od_space | od_share | od_provider |
od_handle_service | od_handle | od_harvester | od_cluster | oz_privileges.
-type entity() :: undefined | #od_user{} | #od_group{} | #od_space{} |
#od_share{} | #od_provider{} | #od_handle_service{} | #od_handle{} | #od_harvester{} | #od_cluster{}.
-type revision() :: gs_protocol:revision().
-type versioned_entity() :: gs_protocol:versioned_entity().
-type aspect() :: gs_protocol:aspect().
-type scope() :: gs_protocol:scope().
-type data_format() :: gs_protocol:data_format().
-type data() :: gs_protocol:data().
-type gri() :: gs_protocol:gri().
-type auth_hint() :: gs_protocol:auth_hint().

-type create_result() :: gs_protocol:graph_create_result().
-type get_result() :: gs_protocol:graph_get_result() | {ok, term()} | {ok, gri(), term()}.
-type delete_result() :: gs_protocol:graph_delete_result().
-type update_result() :: gs_protocol:graph_update_result().
-type result() :: create_result() | get_result() | update_result() | delete_result().
-type error() :: gs_protocol:error().

-type type_validator() :: any | atom | list_of_atoms | binary |
list_of_binaries | integer | float | json | token | invite_token |
boolean | list_of_ipv4_addresses.

-type value_validator() :: any | non_empty |
fun((term()) -> boolean()) |
{not_lower_than, integer()} | {not_greater_than, integer()} |
{between, integer(), integer()} |
[term()] | % A list of accepted values
{exists, fun((entity_id()) -> boolean())} |
{not_exists, fun((entity_id()) -> boolean())} |
{relation_exists, atom(), binary(), atom(), binary(), fun((entity_id()) -> boolean())} |
invite_tokens:token_type() | % Compatible only with 'invite_token' type validator
subdomain | domain |
email | name |
full_name | username | password.

% The 'aspect' key word allows to validate the data provided in aspect
% identifier.
-type validity_verificator() :: #{
required => #{Key :: binary() | {aspect, binary()} => {type_validator(), value_validator()}},
at_least_one => #{Key :: binary() | {aspect, binary()} => {type_validator(), value_validator()}},
optional => #{Key :: binary() | {aspect, binary()} => {type_validator(), value_validator()}}
}.

% Common fields for all records
-type creation_time() :: non_neg_integer().  % UNIX timestamp in seconds

-export_type([
    el_plugin/0,
    operation/0,
    entity_id/0,
    entity_type/0,
    entity/0,
    revision/0,
    versioned_entity/0,
    aspect/0,
    scope/0,
    gri/0,
    data_format/0,
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
    validity_verificator/0,
    creation_time/0
]).

% Internal record containing the request data and state.
-record(state, {
    req = #el_req{} :: req(),
    plugin = undefined :: el_plugin(),
    versioned_entity = {undefined, 1} :: versioned_entity()
}).

-define(DEFAULT_ENTITY_NAME, <<"Unnamed">>).

-export([handle/1, handle/2]).
-export([is_authorized/2]).
-export([validate_name/1, validate_name/5, normalize_name/1, normalize_name/9]).


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
    handle(ElReq, {undefined, 1}).


%%--------------------------------------------------------------------
%% @doc
%% Handles an entity logic request expressed by a #el_req{} record. Entity can
%% be provided if it was prefetched.
%% @end
%%--------------------------------------------------------------------
-spec handle(req(), VersionedEntity :: versioned_entity()) -> result().
handle(#el_req{gri = #gri{type = EntityType}} = ElReq, VersionedEntity) ->
    try
        ElPlugin = EntityType:entity_logic_plugin(),
        handle_unsafe(#state{
            req = ElReq, plugin = ElPlugin, versioned_entity = VersionedEntity
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
-spec is_authorized(req(), versioned_entity()) -> {true, gs_protocol:gri()} | false.
is_authorized(#el_req{gri = #gri{type = EntityType}} = ElReq, VersionedEntity) ->
    try
        ElPlugin = EntityType:entity_logic_plugin(),
        % Existence must be checked too, as sometimes authorization depends
        % on that.
        NewState = ensure_authorized(
            ensure_exists(#state{
                req = ElReq, plugin = ElPlugin, versioned_entity = VersionedEntity
            })),
        {true, NewState#state.req#el_req.gri}
    catch
        _:_ ->
            false
    end.


%%--------------------------------------------------------------------
%% @doc
%% Validates entity name against universal name format.
%% @end
%%--------------------------------------------------------------------
-spec validate_name(binary()) -> boolean().
validate_name(Name) ->
    validate_name(
        Name, ?NAME_FIRST_CHARS_ALLOWED, ?NAME_MIDDLE_CHARS_ALLOWED,
        ?NAME_LAST_CHARS_ALLOWED, ?NAME_MAXIMUM_LENGTH
    ).


%%--------------------------------------------------------------------
%% @doc
%% Validates entity name against given format.
%% @end
%%--------------------------------------------------------------------
-spec validate_name(Name :: binary(), FirstRgx :: binary(), MiddleRgx :: binary(),
    LastRgx :: binary(), MaxLength :: non_neg_integer()) -> boolean().
validate_name(Name, _, _, _, _) when not is_binary(Name) ->
    false;
validate_name(Name, FirstRgx, MiddleRgx, LastRgx, MaxLength) ->
    Regexp = <<
        "^[", FirstRgx/binary, "][", MiddleRgx/binary,
        "]{0,", (integer_to_binary(MaxLength - 2))/binary,
        "}[", LastRgx/binary, "]$"
    >>,
    try re:run(Name, Regexp, [{capture, none}, unicode, ucp]) of
        match -> true;
        _ -> false
    catch _:_ ->
        false
    end.


%%--------------------------------------------------------------------
%% @doc
%% Trims disallowed characters from the beginning and the end of the string,
%% replaces disallowed characters in the middle with dashes('-').
%% If the name is too long, it is shortened to allowed size.
%% @end
%%--------------------------------------------------------------------
-spec normalize_name(binary()) -> binary().
normalize_name(Name) ->
    normalize_name(Name,
        ?NAME_FIRST_CHARS_ALLOWED, <<"">>,
        ?NAME_MIDDLE_CHARS_ALLOWED, <<"-">>,
        ?NAME_LAST_CHARS_ALLOWED, <<"">>,
        ?NAME_MAXIMUM_LENGTH, ?DEFAULT_ENTITY_NAME
    ).


%%--------------------------------------------------------------------
%% @doc
%% Normalizes given name according to Regexp for first, middle and last
%% characters (replaces disallowed characters with given).
%% If the name is too long, it is shortened to allowed size.
%% @end
%%--------------------------------------------------------------------
-spec normalize_name(Name :: binary(),
    FirstRgx :: binary(), FirstReplace :: binary(),
    MiddleRgx :: binary(), MiddleReplace :: binary(),
    LastRgx :: binary(), LastReplace :: binary(),
    MaxLength :: non_neg_integer(), DefaultName :: term()) -> term().
normalize_name(Name, FirstRgx, FirstReplace, MiddleRgx, MiddleReplace, LastRgx, LastReplace, MaxLength, DefaultName) ->
    TrimmedLeft = re:replace(Name,
        <<"^[^", FirstRgx/binary, "]*(?=[", FirstRgx/binary, "])">>, FirstReplace,
        [{return, binary}, unicode, ucp, global]
    ),
    TrimmedMiddle = re:replace(TrimmedLeft,
        <<"[^", MiddleRgx/binary, "]">>, MiddleReplace,
        [{return, binary}, unicode, ucp, global]
    ),
    % string module supports binaries in utf8
    Shortened = string:slice(TrimmedMiddle, 0, MaxLength),
    TrimmedRight = re:replace(Shortened,
        <<"(?<=[", LastRgx/binary, "])[^", LastRgx/binary, "]*$">>, LastReplace,
        [{return, binary}, unicode, ucp, global]
    ),
    case validate_name(TrimmedRight, FirstRgx, MiddleRgx, LastRgx, MaxLength) of
        false -> DefaultName;
        true -> TrimmedRight
    end.


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
        {{ok, resource, Resource}, #el_req{gri = #gri{aspect = instance}, auth = Auth}} ->
            % If an entity instance is created, log an information about it
            % (it's a significant operation and this information might be useful).
            {EntType, EntId} = case Resource of
                {#gri{type = Type, id = Id}, _} -> {Type, Id};
                {#gri{type = Type, id = Id}, _, _} -> {Type, Id}
            end,
            ?debug("~s has been created by client: ~s", [
                EntType:to_string(EntId),
                aai:auth_to_string(Auth)
            ]);
        _ ->
            ok
    end,
    Result;

handle_unsafe(State = #state{req = #el_req{operation = get}}) ->
    NewState = ensure_authorized(
        ensure_exists(
            fetch_entity(
                ensure_operation_supported(
                    State)))),
    {ok, GetResult} = call_get(NewState),
    % Return the new GRI if auto scope was requested
    case State#state.req#el_req.gri#gri.scope of
        auto -> {ok, NewState#state.req#el_req.gri, GetResult};
        _ -> {ok, GetResult}
    end;


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
        {ok, #el_req{gri = #gri{type = Type, id = Id, aspect = instance}, auth = Auth}} ->
            % If an entity instance is deleted, log an information about it
            % (it's a significant operation and this information might be useful).
            ?debug("~s has been deleted by client: ~s", [
                Type:to_string(Id),
                aai:auth_to_string(Auth)
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
fetch_entity(State = #state{versioned_entity = {Entity, _}}) when Entity /= undefined ->
    State;
fetch_entity(State = #state{req = #el_req{gri = #gri{id = undefined}}}) ->
    State;
fetch_entity(State = #state{req = #el_req{operation = create, gri = #gri{aspect = instance}}}) ->
    % Skip when creating an instance with predefined Id, set revision to 1
    State#state{versioned_entity = {undefined, 1}};
fetch_entity(State) ->
    case call_plugin(fetch_entity, State) of
        {ok, {Entity, Revision}} ->
            State#state{versioned_entity = {Entity, Revision}};
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
call_create(State = #state{req = #el_req{return_revision = true}, versioned_entity = {_, Rev}}) ->
    case call_plugin(create, State) of
        {ok, resource, {ResultGRI, ResData}} ->
            {ok, resource, {ResultGRI, {ResData, Rev}}};
        {ok, resource, {ResultGRI, NAuthHint, ResData}} ->
            {ok, resource, {ResultGRI, NAuthHint, {ResData, Rev}}};
        Other ->
            Other
    end;
call_create(State) ->
    call_plugin(create, State).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves an aspect specified in request by calling back
%% proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_get(State :: #state{}) -> get_result().
call_get(State = #state{req = #el_req{return_revision = true}, versioned_entity = {_, Rev}}) ->
    case call_plugin(get, State) of
        {ok, Data} -> {ok, {Data, Rev}};
        {error, _} = Error -> Error
    end;
call_get(State) ->
    call_plugin(get, State).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates an aspect of entity specified in request by calling back
%% proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_update(State :: #state{}) -> update_result().
call_update(State) ->
    call_plugin(update, State).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deletes an aspect of entity specified in request by calling back
%% proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_delete(State :: #state{}) -> delete_result().
call_delete(State) ->
    call_plugin(delete, State).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures requested operation is supported by calling back
%% proper entity logic plugin, throws a proper error of not.
%% @end
%%--------------------------------------------------------------------
-spec ensure_operation_supported(#state{}) -> #state{}.
ensure_operation_supported(State = #state{req = #el_req{gri = #gri{scope = auto}}}) ->
    % If auto scope is requested, defer the check until ensure_authorized
    State;
ensure_operation_supported(State) ->
    case ensure_operation_supported_internal(State) of
        true -> State;
        false -> throw(?ERROR_NOT_SUPPORTED)
    end.


-spec ensure_operation_supported_internal(#state{}) -> boolean().
ensure_operation_supported_internal(State) ->
    try
        call_plugin(operation_supported, State)
    catch _:_ ->
        % No need for log here, 'operation_supported' may crash depending on
        % what the request contains and this is expected.
        false
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures aspect of entity specified in request exists, throws on error.
%% @end
%%--------------------------------------------------------------------
-spec ensure_exists(#state{}) -> #state{}.
ensure_exists(State) ->
    case ensure_exists_internal(State) of
        true -> State;
        false -> throw(?ERROR_NOT_FOUND)
    end.


-spec ensure_exists_internal(#state{}) -> boolean().
ensure_exists_internal(#state{req = #el_req{gri = #gri{id = undefined}}}) ->
    true;
ensure_exists_internal(State) ->
    try
        call_plugin(exists, State)
    catch _:_ ->
        % No need for log here, 'exists' may crash depending on what the
        % request contains and this is expected.
        false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures client specified in request is authorized to perform the request,
%% throws on error.
%% @end
%%--------------------------------------------------------------------
-spec ensure_authorized(State :: #state{}) -> #state{}.
ensure_authorized(State = #state{req = #el_req{gri = #gri{scope = auto}}}) ->
    % Resolve auto scope - maximum scope allowed for given client and place it
    % in state, which will result in specific scope being used in the operation.
    case resolve_auto_scope(State) of
        {true, NewState} -> NewState;
        false -> report_unauthorized(State)
    end;
ensure_authorized(State) ->
    case ensure_authorized_internal(State) of
        true -> State;
        false -> report_unauthorized(State)
    end.


-spec ensure_authorized_internal(#state{}) -> boolean().
ensure_authorized_internal(#state{req = #el_req{auth = ?ROOT}}) ->
    % Root client is authorized to do everything (that client is only available
    % internally).
    true;
ensure_authorized_internal(State) ->
    is_client_authorized(State) orelse is_authorized_as_admin(State).


-spec resolve_auto_scope(#state{}) -> false | {true, #state{}}.
resolve_auto_scope(State) ->
    resolve_auto_scope([private, protected, shared, public], State).

-spec resolve_auto_scope([gs_protocol:scope()], #state{}) -> false | {true, #state{}}.
resolve_auto_scope([], _State) ->
    false;
resolve_auto_scope([Scope | Rest], State = #state{req = Req = #el_req{operation = Operation, gri = GRI}}) ->
    StateWithScope = State#state{req = Req#el_req{gri = GRI#gri{scope = Scope}}},
    Authorized = ensure_operation_supported_internal(StateWithScope) andalso
        (Operation == create orelse ensure_exists_internal(StateWithScope)) andalso
        ensure_authorized_internal(StateWithScope),
    case Authorized of
        true -> {true, StateWithScope};
        false -> resolve_auto_scope(Rest, State)
    end.


-spec is_client_authorized(#state{}) -> boolean().
is_client_authorized(State) ->
    try
        call_plugin(authorize, State)
    catch _:_ ->
        false
    end.


-spec is_authorized_as_admin(#state{}) -> boolean().
is_authorized_as_admin(#state{req = ElReq} = State) ->
    try
        case call_plugin(required_admin_privileges, State) of
            forbidden ->
                false;
            Privileges ->
                lists:all(fun(Privilege) ->
                    user_logic_plugin:auth_by_oz_privilege(ElReq, Privilege)
                end, Privileges)
        end
    catch _:_ ->
        false
    end.


-spec report_unauthorized(#state{}) -> no_return().
report_unauthorized(#state{req = #el_req{auth = ?NOBODY}}) ->
    % The client was not authenticated -> unauthorized
    throw(?ERROR_UNAUTHORIZED);
report_unauthorized(_) ->
    % The client was authenticated but cannot access the aspect -> forbidden
    throw(?ERROR_FORBIDDEN).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures data specified in request is valid, throws on error.
%% @end
%%--------------------------------------------------------------------
-spec ensure_valid(State :: #state{}) -> #state{}.
ensure_valid(State) ->
    #state{
        req = #el_req{gri = #gri{aspect = Aspect}, data = Data} = Req
    } = State,
    ValidatorsMap = call_plugin(validate, State),
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
                    {DataAcc, HasAtLeastOneAcc};
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
    case maps:find(Key, Data) of
        error ->
            false;
        {ok, Value} ->
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
        TransformedType = check_type(TypeRule, Key, Value),
        check_value(TypeRule, ValueRule, Key, TransformedType)
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
check_type(atom, _Key, Binary) when is_binary(Binary) ->
    try
        binary_to_existing_atom(Binary, utf8)
    catch
        _:_ ->
            % return empty atom so it can fail on value verification
            % (atoms can always have only predefined values)
            ''
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
        [check_type(atom, Key, Val) || Val <- Values]
    catch
        _:_ ->
            throw(?ERROR_BAD_VALUE_LIST_OF_ATOMS(Key))
    end;
check_type(binary, _Key, Binary) when is_binary(Binary) ->
    Binary;
check_type(binary, _Key, null) ->
    undefined;
check_type(binary, _Key, undefined) ->
    undefined;
check_type(binary, _Key, Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
check_type(binary, Key, _) ->
    throw(?ERROR_BAD_VALUE_BINARY(Key));
check_type(list_of_binaries, Key, Values) ->
    try
        lists:map(fun
            (Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
            (Bin) when is_binary(Bin) -> Bin
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
check_type(token, Key, Serialized) when is_binary(Serialized) ->
    case tokens:deserialize(Serialized) of
        {ok, Token} -> Token;
        ?ERROR_BAD_MACAROON -> throw(?ERROR_BAD_VALUE_TOKEN(Key))
    end;
check_type(token, Key, Token) ->
    case tokens:is_token(Token) of
        true -> Token;
        false -> throw(?ERROR_BAD_VALUE_TOKEN(Key))
    end;
check_type(invite_token, Key, <<>>) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_type(invite_token, Key, Token) when is_binary(Token) ->
    case invite_tokens:deserialize(Token) of
        {ok, Macaroon} -> Macaroon;
        ?ERROR_BAD_MACAROON -> throw(?ERROR_BAD_VALUE_TOKEN(Key))
    end;
check_type(invite_token, Key, Macaroon) ->
    case macaroon:is_macaroon(Macaroon) of
        true -> Macaroon;
        false -> throw(?ERROR_BAD_VALUE_TOKEN(Key))
    end;
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
    Value :: term()) -> TransformedValue :: term().
check_value(_, any, _Key, Value) ->
    Value;
check_value(atom, non_empty, Key, '') ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_value(list_of_atoms, non_empty, Key, []) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_value(binary, non_empty, Key, <<"">>) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_value(binary, non_empty, Key, undefined) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_value(list_of_binaries, non_empty, Key, []) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_value(json, non_empty, Key, Map) when map_size(Map) == 0 ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_value(_, non_empty, _Key, Value) ->
    Value;
check_value(_, {not_lower_than, Threshold}, Key, Value) ->
    case Value >= Threshold of
        true ->
            Value;
        false ->
            throw(?ERROR_BAD_VALUE_TOO_LOW(Key, Threshold))
    end;
check_value(_, {not_greater_than, Threshold}, Key, Value) ->
    case Value =< Threshold of
        true ->
            Value;
        false ->
            throw(?ERROR_BAD_VALUE_TOO_HIGH(Key, Threshold))
    end;
check_value(_, {between, Low, High}, Key, Value) ->
    case Value >= Low andalso Value =< High of
        true ->
            Value;
        false ->
            throw(?ERROR_BAD_VALUE_NOT_IN_RANGE(Key, Low, High))
    end;
check_value(binary, domain, Key, <<"">>) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_value(binary, domain, Key, Value) ->
    case size(Value) =< ?MAX_DOMAIN_LENGTH of
        true ->
            case re:run(Value, ?DOMAIN_VALIDATION_REGEXP, [{capture, none}]) of
                match -> Value;
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
                true -> Value;
                _ -> throw(?ERROR_BAD_VALUE_SUBDOMAIN)
            end;
        _ -> throw(?ERROR_BAD_VALUE_SUBDOMAIN)
    end;

check_value(binary, email, Key, <<"">>) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
check_value(binary, email, _Key, Value) ->
    case http_utils:validate_email(Value) of
        true -> Value;
        false -> throw(?ERROR_BAD_VALUE_EMAIL)
    end;

check_value(json, JsonValidator, Key, Map) ->
    maps:map(fun(NestedKey, {NestedTypeRule, NestedValueRule}) ->
        FullKey = <<Key/binary, ".", NestedKey/binary>>,
        case maps:find(NestedKey, Map) of
            error ->
                throw(?ERROR_BAD_VALUE_EMPTY(FullKey));
            {ok, Value} ->
                transform_and_check_value(NestedTypeRule, NestedValueRule, FullKey, Value)
        end
    end, JsonValidator);

check_value(_, AllowedVals, Key, Vals) when is_list(AllowedVals) andalso is_list(Vals) ->
    case ordsets:subtract(ordsets:from_list(Vals), ordsets:from_list(AllowedVals)) of
        [] ->
            Vals;
        _ ->
            throw(?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(Key, AllowedVals))
    end;
check_value(_, AllowedVals, Key, Val) when is_list(AllowedVals) ->
    case lists:member(Val, AllowedVals) of
        true ->
            Val;
        _ ->
            throw(?ERROR_BAD_VALUE_NOT_ALLOWED(Key, AllowedVals))
    end;
check_value(_, VerifyFun, Key, Vals) when is_function(VerifyFun, 1) andalso is_list(Vals) ->
    case lists:all(VerifyFun, Vals) of
        true ->
            Vals;
        false ->
            throw(?ERROR_BAD_DATA(Key))
    end;
check_value(_, VerifyFun, Key, Val) when is_function(VerifyFun, 1) ->
    case VerifyFun(Val) of
        true ->
            Val;
        false ->
            throw(?ERROR_BAD_DATA(Key))
    end;
check_value(Type, {exists, VerifyFun}, Key, Val) when is_function(VerifyFun, 1) ->
    check_value(Type, non_empty, Key, Val),
    case VerifyFun(Val) of
        true ->
            Val;
        false ->
            throw(?ERROR_BAD_VALUE_ID_NOT_FOUND(Key))
    end;
check_value(Type, {not_exists, VerifyFun}, Key, Val) when is_function(VerifyFun, 1) ->
    check_value(Type, non_empty, Key, Val),
    case VerifyFun(Val) of
        true ->
            Val;
        false ->
            throw(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(Key))
    end;
check_value(_, {relation_exists, ChType, ChId, ParType, ParId, VerifyFun}, _Key, Val) when is_function(VerifyFun, 1) ->
    case VerifyFun(Val) of
        true ->
            Val;
        false ->
            throw(?ERROR_RELATION_DOES_NOT_EXIST(ChType, ChId, ParType, ParId))
    end;
check_value(token, any, _Key, _Macaroon) ->
    ok;
check_value(invite_token, TokenType, Key, Macaroon) ->
    case invite_tokens:validate(Macaroon, TokenType) of
        ok ->
            Macaroon;
        inexistent ->
            throw(?ERROR_BAD_VALUE_TOKEN(Key));
        bad_macaroon ->
            throw(?ERROR_BAD_VALUE_TOKEN(Key));
        bad_type ->
            throw(?ERROR_BAD_VALUE_BAD_TOKEN_TYPE(Key))
    end;
check_value(binary, username, _Key, Value) ->
    case user_logic:validate_username(Value) of
        true -> Value;
        false -> throw(?ERROR_BAD_VALUE_USERNAME)
    end;
check_value(binary, full_name, _Key, Value) ->
    case user_logic:validate_full_name(Value) of
        true -> Value;
        false -> throw(?ERROR_BAD_VALUE_FULL_NAME)
    end;
check_value(binary, password, _Key, undefined) ->
    throw(?ERROR_BAD_VALUE_PASSWORD);
check_value(binary, password, _Key, Value) ->
    case size(Value) >= ?PASSWORD_MIN_LENGTH of
        true -> Value;
        false -> throw(?ERROR_BAD_VALUE_PASSWORD)
    end;
check_value(binary, name, _Key, Value) ->
    case validate_name(Value) of
        true -> Value;
        false -> throw(?ERROR_BAD_VALUE_NAME)
    end;
check_value(TypeRule, ValueRule, Key, _) ->
    ?error("Unknown {type, value} rule: {~p, ~p} for key: ~p", [
        TypeRule, ValueRule, Key
    ]),
    throw(?ERROR_INTERNAL_SERVER_ERROR).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs operation on proper entity logic plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_plugin(Operation :: atom(), #state{}) -> term().
call_plugin(fetch_entity, #state{plugin = Plugin, req = #el_req{gri = #gri{id = Id}}}) when is_binary(Id) ->
    Plugin:fetch_entity(Id);
call_plugin(fetch_entity, _) ->
    ?ERROR_NOT_FOUND;
call_plugin(operation_supported, #state{plugin = Plugin, req = #el_req{operation = Operation,
    gri = #gri{aspect = Aspect, scope = Scope}}}) ->
    Plugin:operation_supported(Operation, Aspect, Scope);
call_plugin(exists, #state{plugin = Plugin, req = ElReq, versioned_entity = {Entity, _}}) ->
    Plugin:exists(ElReq, Entity);
call_plugin(authorize, #state{plugin = Plugin, req = ElReq, versioned_entity = {Entity, _}}) ->
    Plugin:authorize(ElReq, Entity);
call_plugin(required_admin_privileges, #state{plugin = Plugin, req = ElReq}) ->
    Plugin:required_admin_privileges(ElReq);
call_plugin(get, #state{plugin = Plugin, req = ElReq, versioned_entity = {Entity, _}}) ->
    Plugin:get(ElReq, Entity);
call_plugin(update, #state{plugin = Plugin, req = ElReq}) ->
    Plugin:update(ElReq);
call_plugin(Operation, #state{plugin = Plugin, req = ElReq, versioned_entity = {Entity, _}}) ->
    % covers create, delete, validate
    case Plugin:Operation(ElReq) of
        Fun when is_function(Fun, 1) ->
            Fun(Entity);
        Result ->
            Result
    end.
