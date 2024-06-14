%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Helper module for entity_logic to sanitize input data.
%%% @end
%%%-------------------------------------------------------------------
-module(entity_logic_sanitizer).
-author("Lukasz Opiola").

-include("entity_logic.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/errors.hrl").

-export([validate_name/1, validate_name/5, normalize_name/2, normalize_name/9]).
-export([ensure_valid/3]).

% key and value in the data object
-type key() :: binary().
-type value() :: term().

-type type_spec() :: any | atom | list_of_atoms | binary
| list_of_binaries | integer | integer_or_infinity | float | json
| token | invite_token | token_type | caveats
| boolean | ipv4_address | list_of_ipv4_addresses
| {jsonable_record, single | list, jsonable_record:record_type()}
| {persistent_record, single, module()}.

-type value_spec() :: any | non_empty
| fun((term()) -> boolean())
| {not_lower_than, integer()} | {not_greater_than, integer()}
| {between, integer(), integer()}
| [term()] % A list of accepted values
| {text_length_limit, integer()}
| {exists, fun((entity_logic:entity_id()) -> boolean())}
| {not_exists, fun((entity_logic:entity_id()) -> boolean())}
| {relation_exists, atom(), binary(), atom(), binary(), fun((entity_logic:entity_id()) -> boolean())}
| token_type:invite_type() % Compatible only with 'invite_token' type validator
| subdomain | domain
| email | name
| full_name | username | password.

-type parameter_spec() :: {type_spec(), value_spec() | {all, [value_spec()]}}.
-type parameter_specs() :: #{key() | {aspect, binary()} => parameter_spec()}.

%% @formatter:off
% The 'aspect' key word allows to validate the data provided in aspect
% identifier.
-type sanitizer_spec() :: #{
    required => parameter_specs(),
    at_least_one => parameter_specs(),
    optional => parameter_specs()
}.
%% @formatter:on

-export_type([parameter_specs/0, sanitizer_spec/0, parameter_spec/0]).

%%%===================================================================
%%% API
%%%===================================================================

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
-spec normalize_name(binary(), DefaultName) -> binary() | DefaultName.
normalize_name(Name, DefaultName) ->
    normalize_name(Name,
        ?NAME_FIRST_CHARS_ALLOWED, <<"">>,
        ?NAME_MIDDLE_CHARS_ALLOWED, <<"-">>,
        ?NAME_LAST_CHARS_ALLOWED, <<"">>,
        ?NAME_MAXIMUM_LENGTH, DefaultName
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
        <<"^[^", FirstRgx/binary, "]*">>, FirstReplace,
        [{return, binary}, unicode, ucp, global]
    ),
    TrimmedMiddle = re:replace(TrimmedLeft,
        <<"[^", MiddleRgx/binary, "]">>, MiddleReplace,
        [{return, binary}, unicode, ucp, global]
    ),
    % string module supports binaries in utf8
    Shortened = string:slice(TrimmedMiddle, 0, MaxLength),
    TrimmedRight = re:replace(Shortened,
        <<"[^", LastRgx/binary, "]*$">>, LastReplace,
        [{return, binary}, unicode, ucp, global]
    ),
    case validate_name(TrimmedRight, FirstRgx, MiddleRgx, LastRgx, MaxLength) of
        false -> DefaultName;
        true -> TrimmedRight
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures the data is valid according to given sanitizer spec, throws on error.
%% Checks values, types and performs basic transformations.
%% @end
%%--------------------------------------------------------------------
-spec ensure_valid(sanitizer_spec(), gri:aspect(), entity_logic:data()) ->
    entity_logic:sanitized_data() | no_return().
ensure_valid(SanitizerSpec, GriAspect, Data) ->
    % Get all types of validators
    Required = maps:get(required, SanitizerSpec, #{}),
    Optional = maps:get(optional, SanitizerSpec, #{}),
    AtLeastOne = maps:get(at_least_one, SanitizerSpec, #{}),
    % Artificially add 'aspect' key to Data to simplify validation code.
    % This keyword allows verifying if data provided in aspect identifier
    % is valid.
    DataWithAspect = case Data of
        undefined -> #{aspect => GriAspect};
        Map when is_map(Map) -> Data#{aspect => GriAspect};
        _ -> throw(?ERROR_MALFORMED_DATA)
    end,
    % Start with required parameters. Transform the data if needed, fail when
    % any key is missing or cannot be validated.
    Data2 = lists:foldl(
        fun(Key, DataAcc) ->
            case transform_and_check_value(Key, DataWithAspect, Required) of
                false ->
                    throw(?ERROR_MISSING_REQUIRED_VALUE(Key));
                {true, SanitizedData} ->
                    maps:merge(DataAcc, SanitizedData)
            end
        end, #{}, maps:keys(Required)),
    % Now, optional parameters. Transform the data if needed, fail when
    % any of the keys exists in the data but cannot be validated.
    Data3 = lists:foldl(
        fun(Key, DataAcc) ->
            case transform_and_check_value(Key, DataWithAspect, Optional) of
                false ->
                    DataAcc;
                {true, SanitizedData} ->
                    maps:merge(DataAcc, SanitizedData)
            end
        end, Data2, maps:keys(Optional)),
    % Finally, "at least one" parameters. Transform the data if needed, fail
    % when less than one key exists in the data or any of the keys cannot
    % be validated.
    {Data4, HasAtLeastOne} = lists:foldl(
        fun(Key, {DataAcc, HasAtLeastOneAcc}) ->
            case transform_and_check_value(Key, DataWithAspect, AtLeastOne) of
                false ->
                    {DataAcc, HasAtLeastOneAcc};
                {true, SanitizedData} ->
                    {maps:merge(DataAcc, SanitizedData), true}
            end
        end, {Data3, false}, maps:keys(AtLeastOne)),
    case {length(maps:keys(AtLeastOne)), HasAtLeastOne} of
        {_, true} ->
            ok;
        {0, false} ->
            ok;
        {_, false} ->
            throw(?ERROR_MISSING_AT_LEAST_ONE_VALUE(lists:sort(maps:keys(AtLeastOne))))
    end,
    Data4.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs simple value conversion (if possible) and checks the type and value
%% of value for Key in Data. Takes into consideration special keyword
%% {aspect, key()}, that allows to validate data in aspect.
%% In such case, the Data map must include the 'aspect' key that hold the value.
%% The Key is an arbitrary name for the validated attribute, useful when
%% generating error messages.
%% @end
%%--------------------------------------------------------------------
-spec transform_and_check_value(key(), entity_logic:data(), parameter_specs()) ->
    {true, entity_logic:sanitized_data()} | false.
transform_and_check_value({aspect, Key}, Data, ParameterSpecs) ->
    {TypeRule, ValueRule} = maps:get({aspect, Key}, ParameterSpecs),
    %% Aspect validator supports only aspects that are tuples
    {_, Value} = maps:get(aspect, Data),
    % Ignore the returned value - the check will throw in case the value is
    % not valid
    transform_and_check_value(TypeRule, ValueRule, Key, Value),
    {true, #{}};
transform_and_check_value(Key, Data, ParameterSpecs) ->
    case maps:find(Key, Data) of
        error ->
            false;
        {ok, Value} ->
            {TypeRule, ValueRule} = maps:get(Key, ParameterSpecs),
            NewValue = transform_and_check_value(TypeRule, ValueRule, Key, Value),
            {true, #{Key => NewValue}}
    end.

-spec transform_and_check_value(type_spec(), value_spec(), key(), value()) -> value() | no_return().
transform_and_check_value(TypeRule, ValueRule, Key, Value) ->
    try
        TransformedType = sanitize_type(TypeRule, Key, Value),
        sanitize_value(TypeRule, ValueRule, Key, TransformedType)
    catch
        throw:Error ->
            throw(Error);
        Type:Message:Stacktrace ->
            ?error_stacktrace(
                "Error in entity_logic:transform_and_check_value - ~tp:~tp",
                [Type, Message],
                Stacktrace
            ),
            throw(?ERROR_BAD_DATA(Key))
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs simple value conversion (if possible) and checks the type of Value for Key.
%% @end
%%--------------------------------------------------------------------
-spec sanitize_type(type_spec(), key(), value()) -> value().
sanitize_type(any, _Key, Term) ->
    Term;
sanitize_type(atom, _Key, Atom) when is_atom(Atom) ->
    Atom;
sanitize_type(atom, _Key, Binary) when is_binary(Binary) ->
    try
        binary_to_existing_atom(Binary, utf8)
    catch
        _:_ ->
            % return empty atom so it can fail on value verification
            % (atoms can always have only predefined values)
            ''
    end;
sanitize_type(atom, Key, _) ->
    throw(?ERROR_BAD_VALUE_ATOM(Key));
sanitize_type(boolean, _Key, true) ->
    true;
sanitize_type(boolean, _Key, false) ->
    false;
sanitize_type(boolean, Key, _) ->
    throw(?ERROR_BAD_VALUE_BOOLEAN(Key));
sanitize_type(list_of_atoms, Key, Values) ->
    try
        [sanitize_type(atom, Key, Val) || Val <- Values]
    catch
        _:_ ->
            throw(?ERROR_BAD_VALUE_LIST_OF_ATOMS(Key))
    end;
sanitize_type(binary, _Key, Binary) when is_binary(Binary) ->
    Binary;
sanitize_type(binary, _Key, null) ->
    undefined;
sanitize_type(binary, _Key, undefined) ->
    undefined;
sanitize_type(binary, _Key, Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
sanitize_type(binary, Key, _) ->
    throw(?ERROR_BAD_VALUE_BINARY(Key));
sanitize_type(list_of_binaries, Key, Values) ->
    try
        lists:map(fun
            (Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
            (Bin) when is_binary(Bin) -> Bin
        end, Values)
    catch
        _:_ ->
            throw(?ERROR_BAD_VALUE_LIST_OF_BINARIES(Key))
    end;
sanitize_type(integer, Key, Bin) when is_binary(Bin) ->
    try
        binary_to_integer(Bin)
    catch _:_ ->
        throw(?ERROR_BAD_VALUE_INTEGER(Key))
    end;
sanitize_type(integer, _Key, Int) when is_integer(Int) ->
    Int;
sanitize_type(integer, Key, _) ->
    throw(?ERROR_BAD_VALUE_INTEGER(Key));
sanitize_type(integer_or_infinity, _Key, ?INFINITY) ->
    ?INFINITY;
sanitize_type(integer_or_infinity, Key, Value) ->
    sanitize_type(integer, Key, Value);
sanitize_type(float, Key, Bin) when is_binary(Bin) ->
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
sanitize_type(float, _Key, Int) when is_integer(Int) ->
    float(Int);
sanitize_type(float, _Key, Float) when is_float(Float) ->
    Float;
sanitize_type(float, Key, _) ->
    throw(?ERROR_BAD_VALUE_FLOAT(Key));
sanitize_type(json, _Key, JSON) when is_map(JSON) ->
    JSON;
sanitize_type(json, Key, _) ->
    throw(?ERROR_BAD_VALUE_JSON(Key));
sanitize_type(token, Key, <<>>) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
sanitize_type(token, Key, Serialized) when is_binary(Serialized) ->
    case tokens:deserialize(Serialized) of
        {ok, Token} -> Token;
        {error, _} = Error -> throw(?ERROR_BAD_VALUE_TOKEN(Key, Error))
    end;
sanitize_type(token, Key, Token) ->
    case tokens:is_token(Token) of
        true -> Token;
        false -> throw(?ERROR_BAD_VALUE_TOKEN(Key, ?ERROR_BAD_TOKEN))
    end;
sanitize_type(invite_token, Key, <<>>) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
sanitize_type(invite_token, Key, Serialized) when is_binary(Serialized) ->
    case tokens:deserialize(Serialized) of
        {ok, Token} -> Token;
        {error, _} = Error -> throw(?ERROR_BAD_VALUE_TOKEN(Key, Error))
    end;
sanitize_type(invite_token, Key, Token) ->
    case tokens:is_token(Token) of
        true -> Token;
        false -> throw(?ERROR_BAD_VALUE_TOKEN(Key, ?ERROR_BAD_TOKEN))
    end;
sanitize_type(token_type, Key, TokenType) ->
    case token_type:sanitize(TokenType) of
        {true, Sanitized} -> Sanitized;
        false -> throw(?ERROR_BAD_VALUE_TOKEN_TYPE(Key))
    end;
sanitize_type(invite_type, Key, InviteType) ->
    case token_type:sanitize_invite_type(InviteType) of
        {true, Sanitized} -> Sanitized;
        false -> throw(?ERROR_BAD_VALUE_INVITE_TYPE(Key))
    end;
sanitize_type(caveats, Key, Caveats) ->
    try
        lists:map(fun(Caveat) ->
            case caveats:sanitize(Caveat) of
                {true, Sanitized} ->
                    Sanitized;
                false ->
                    JsonableCaveat = case Caveat of
                        Map when is_map(Map) -> Map;
                        Bin when is_binary(Bin) -> Bin;
                        Term -> str_utils:format_bin("~tp", [Term])
                    end,
                    throw(?ERROR_BAD_VALUE_CAVEAT(JsonableCaveat))
            end
        end, Caveats)
    catch
        throw:{error, _} = Error -> throw(Error);
        _:_ -> throw(?ERROR_BAD_DATA(Key))
    end;
sanitize_type(ipv4_address, _Key, undefined) ->
    undefined;
sanitize_type(ipv4_address, _Key, null) ->
    undefined;
sanitize_type(ipv4_address, Key, IPAddress) ->
    case ip_utils:to_ip4_address(IPAddress) of
        {ok, Ip4Address} -> Ip4Address;
        {error, ?EINVAL} -> throw(?ERROR_BAD_VALUE_IPV4_ADDRESS(Key))
    end;
sanitize_type(list_of_ipv4_addresses, Key, ListOfIPs) ->
    try
        [sanitize_type(ipv4_address, Key, IP) || IP <- ListOfIPs]
    catch _:_ ->
        throw(?ERROR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(Key))
    end;
sanitize_type({jsonable_record, single, RecordType}, Key, Value) ->
    try
        jsonable_record:from_json(Value, RecordType)
    catch
        throw:{error, _} = Error ->
            throw(Error);
        _:_ ->
            throw(?ERROR_BAD_DATA(Key))
    end;
sanitize_type({jsonable_record, list, RecordType}, Key, Values) ->
    try
        lists:map(fun(Value) ->
            sanitize_type({jsonable_record, single, RecordType}, Key, Value)
        end, Values)
    catch
        throw:{error, _} = Error ->
            throw(Error);
        _:_ ->
            throw(?ERROR_BAD_DATA(Key))
    end;
sanitize_type({persistent_record, single, RecordType}, Key, Value) ->
    try
        persistent_record:from_json(Value, RecordType)
    catch
        throw:{error, _} = Error ->
            throw(Error);
        _:_ ->
            throw(?ERROR_BAD_DATA(Key))
    end;
sanitize_type(Rule, Key, _) ->
    ?error("Unknown type rule: ~tp for key: ~tp", [Rule, Key]),
    throw(?ERROR_INTERNAL_SERVER_ERROR).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs simple value conversion (if possible) and checks the Value for Key.
%% @end
%%--------------------------------------------------------------------
-spec sanitize_value(type_spec(), value_spec() | {all, [value_spec()]}, key(), value()) ->
    SanitizedValue :: value().
sanitize_value(Type, {all, Rules}, Key, Value) ->
    lists:foldl(fun(Rule, Acc) ->
        sanitize_value(Type, Rule, Key, Acc)
    end, Value, Rules);
sanitize_value(_, any, _Key, Value) ->
    Value;
sanitize_value(atom, non_empty, Key, '') ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
sanitize_value(list_of_atoms, non_empty, Key, []) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
sanitize_value(binary, non_empty, Key, <<"">>) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
sanitize_value(binary, non_empty, Key, undefined) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
sanitize_value(list_of_binaries, non_empty, Key, []) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
sanitize_value(json, non_empty, Key, Map) when map_size(Map) == 0 ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
sanitize_value(_, non_empty, _Key, Value) ->
    Value;
sanitize_value(_, {not_lower_than, _Threshold}, _Key, ?INFINITY) ->
    ?INFINITY;
sanitize_value(_, {not_lower_than, Threshold}, Key, Value) ->
    case Value >= Threshold of
        true ->
            Value;
        false ->
            throw(?ERROR_BAD_VALUE_TOO_LOW(Key, Threshold))
    end;
sanitize_value(_, {not_greater_than, Threshold}, Key, ?INFINITY) ->
    throw(?ERROR_BAD_VALUE_TOO_HIGH(Key, Threshold));
sanitize_value(_, {not_greater_than, Threshold}, Key, Value) ->
    case Value =< Threshold of
        true ->
            Value;
        false ->
            throw(?ERROR_BAD_VALUE_TOO_HIGH(Key, Threshold))
    end;
sanitize_value(_, {between, Low, High}, Key, Value) ->
    case Value >= Low andalso Value =< High of
        true ->
            Value;
        false ->
            throw(?ERROR_BAD_VALUE_NOT_IN_RANGE(Key, Low, High))
    end;
sanitize_value(binary, domain, Key, <<"">>) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
sanitize_value(binary, domain, _Key, Value) ->
    case size(Value) =< ?MAX_DOMAIN_LENGTH of
        true ->
            case re:run(Value, ?DOMAIN_VALIDATION_REGEXP, [{capture, none}]) of
                match -> Value;
                _ -> throw(?ERROR_BAD_VALUE_DOMAIN)
            end;
        _ -> throw(?ERROR_BAD_VALUE_DOMAIN)
    end;

sanitize_value(binary, subdomain, Key, <<"">>) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
sanitize_value(binary, subdomain, _Key, Value) ->
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

sanitize_value(binary, email, Key, <<"">>) ->
    throw(?ERROR_BAD_VALUE_EMPTY(Key));
sanitize_value(binary, email, _Key, Value) ->
    case http_utils:validate_email(Value) of
        true -> Value;
        false -> throw(?ERROR_BAD_VALUE_EMAIL)
    end;

sanitize_value(json, JsonValidator, Key, Map) when is_map(JsonValidator) ->
    maps:map(fun(NestedKey, {NestedTypeRule, NestedValueRule}) ->
        FullKey = <<Key/binary, ".", NestedKey/binary>>,
        case maps:find(NestedKey, Map) of
            error ->
                throw(?ERROR_BAD_VALUE_EMPTY(FullKey));
            {ok, Value} ->
                transform_and_check_value(NestedTypeRule, NestedValueRule, FullKey, Value)
        end
    end, JsonValidator);

sanitize_value(json, qos_parameters, _Key, Map) ->
    case maps:fold(fun(K, V, Acc) ->
        Acc andalso is_binary(K) andalso (is_binary(V) or is_number(V))
    end, true, Map) of
        true -> Map;
        false -> throw(?ERROR_BAD_VALUE_QOS_PARAMETERS)
    end;

sanitize_value(token_type, VerifyFun, Key, Val) when is_function(VerifyFun, 1) ->
    case VerifyFun(Val) of
        true ->
            Val;
        false ->
            throw(?ERROR_BAD_VALUE_TOKEN_TYPE(Key))
    end;

sanitize_value(_, AllowedVals, Key, Vals) when is_list(AllowedVals) andalso is_list(Vals) ->
    case ordsets:subtract(ordsets:from_list(Vals), ordsets:from_list(AllowedVals)) of
        [] ->
            Vals;
        _ ->
            throw(?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(Key, AllowedVals))
    end;
sanitize_value(_, AllowedVals, Key, Val) when is_list(AllowedVals) ->
    case lists:member(Val, AllowedVals) of
        true ->
            Val;
        _ ->
            throw(?ERROR_BAD_VALUE_NOT_ALLOWED(Key, AllowedVals))
    end;
sanitize_value(list_of_atoms, VerifyFun, Key, Vals) when is_function(VerifyFun, 1) andalso is_list(Vals) ->
    case VerifyFun(Vals) of
        true ->
            Vals;
        false ->
            throw(?ERROR_BAD_DATA(Key))
    end;
sanitize_value(_, VerifyFun, Key, Vals) when is_function(VerifyFun, 1) andalso is_list(Vals) ->
    case lists:all(VerifyFun, Vals) of
        true ->
            Vals;
        false ->
            throw(?ERROR_BAD_DATA(Key))
    end;
sanitize_value(_, VerifyFun, Key, Val) when is_function(VerifyFun, 1) ->
    case VerifyFun(Val) of
        true ->
            Val;
        false ->
            throw(?ERROR_BAD_DATA(Key))
    end;
sanitize_value(binary, {text_length_limit, SizeLimit}, Key, Val) ->
    % string:length/1 counts characters rather than bytes (one unicode character can be a couple of bytes long)
    try string:length(Val) =< SizeLimit of
        true ->
            Val;
        false ->
            throw(?ERROR_BAD_VALUE_TEXT_TOO_LARGE(Key, SizeLimit))
    catch _:_ ->
        throw(?ERROR_BAD_VALUE_BINARY(Key))
    end;
sanitize_value(_, {exists, VerifyFun}, Key, Val) when is_function(VerifyFun, 1) ->
    try VerifyFun(Val) of
        true ->
            Val;
        false ->
            throw(?ERROR_BAD_VALUE_ID_NOT_FOUND(Key))
    catch _:_ ->
        throw(?ERROR_BAD_VALUE_ID_NOT_FOUND(Key))
    end;
sanitize_value(Type, {not_exists, VerifyFun}, Key, Val) when is_function(VerifyFun, 1) ->
    sanitize_value(Type, non_empty, Key, Val),
    try VerifyFun(Val) of
        true ->
            Val;
        false ->
            throw(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(Key))
    catch _:_ ->
        throw(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(Key))
    end;
sanitize_value(_, {relation_exists, ChType, ChId, ParType, ParId, VerifyFun}, _Key, Val) when is_function(VerifyFun, 1) ->
    try VerifyFun(Val) of
        true ->
            Val;
        false ->
            throw(?ERROR_RELATION_DOES_NOT_EXIST(ChType, ChId, ParType, ParId))
    catch _:_ ->
        throw(?ERROR_RELATION_DOES_NOT_EXIST(ChType, ChId, ParType, ParId))
    end;
sanitize_value(token, any, _Key, _Token) ->
    ok;
sanitize_value(invite_token, ExpectedType, Key, Token = #token{type = ReceivedType}) ->
    case tokens:is_invite_token(Token, ExpectedType) of
        true -> Token;
        false -> throw(?ERROR_BAD_VALUE_TOKEN(Key, ?ERROR_NOT_AN_INVITE_TOKEN(ExpectedType, ReceivedType)))
    end;
sanitize_value(binary, username, _Key, Value) ->
    case user_logic:validate_username(Value) of
        true -> Value;
        false -> throw(?ERROR_BAD_VALUE_USERNAME)
    end;
sanitize_value(binary, full_name, _Key, Value) ->
    case user_logic:validate_full_name(Value) of
        true -> Value;
        false -> throw(?ERROR_BAD_VALUE_FULL_NAME)
    end;
sanitize_value(binary, password, _Key, undefined) ->
    throw(?ERROR_BAD_VALUE_PASSWORD);
sanitize_value(binary, password, _Key, Value) ->
    case size(Value) >= ?PASSWORD_MIN_LENGTH of
        true -> Value;
        false -> throw(?ERROR_BAD_VALUE_PASSWORD)
    end;
sanitize_value(binary, name, _Key, Value) ->
    case validate_name(Value) of
        true -> Value;
        false -> throw(?ERROR_BAD_VALUE_NAME)
    end;
sanitize_value(TypeRule, ValueRule, Key, _) ->
    ?error("Unknown {type, value} rule: {~tp, ~tp} for key: ~tp", [
        TypeRule, ValueRule, Key
    ]),
    throw(?ERROR_INTERNAL_SERVER_ERROR).