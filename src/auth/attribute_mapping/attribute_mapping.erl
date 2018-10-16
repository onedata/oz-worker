%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This modules handles mapping OIDC / SAML attributes received from Identity
%%% Providers into Onedata #linked_account{} records, according to mapping
%%% rules specified in auth.config.
%%%
%%% Onezone collects information about users from SAML / OpenID, including:
%%%        subjectId - user's unique identifier, mandatory
%%%                    (login will fail if mapping cannot be found)
%%%             name - user's displayed name
%%%            alias - user's short, unique identifier - used as an alternative
%%%                    to identifying users by name, is optional and can be
%%%                    set/unset anytime
%%%           emails - a list of user's emails
%%%     entitlements - a list of user's entitlements to groups in the IdP, which
%%%                    can later be mapped to groups in Onedata using
%%%                    entitlement mapping
%%%           custom - arbitrary term received from the IdP, can be later useful
%%%                    during integration with storage systems (LUMA)
%%%
%%% The config section concerning the attribute mapping in auth.config looks
%%% like the following:
%%%
%%%   attributeMapping => #{
%%%       subjectId => {required, <rules>},
%%%       name => {optional, <rules>},
%%%       alias => undefined,
%%%       emails => {optional, <rules>},
%%%       entitlements => undefined,
%%%       custom => {required, <rules>},
%%%   }
%%%
%%% Allowed mappings are:
%%%     undefined - this attribute is not mapped at all, it is equivalent to
%%%         deleting the attribute mapping completely from the config.
%%%
%%%     {required, <rules>} - this attribute will be mapped according to <rules>,
%%%         if it's not possible to resolve the attribute the login will fail.
%%%
%%%     {optional, <rules>} - this attribute will be mapped according to <rules>,
%%%         if it's not possible to resolve the attribute the mapped value will
%%%         be empty (undefined).
%%%
%%%     {plugin, Module} - allows to use arbitrary plugin to perform the mapping.
%%%         Module:map_attribute(Attr, IdPAttributes) will be called and should
%%%         return the resolved attribute value as {ok, Value}, or {error, Reason}
%%%         if it could not be found. IdPAttributes is an Erlang map (keys are
%%%         binaries). Module must be placed in the auth_plugins directory
%%%         (/etc/oz_worker/auth_plugins) to be loaded during Onezone startup.
%%%         Example:
%%%             name => {plugin, my_attr_mapper}
%%%                 would call my_attr_mapper:map_attribute(name, IdPAttributes)
%%%
%%% <rules> can be a complex term built from the following expressions:
%%%
%%%  "attrName" - name of an attribute. If such key is present in attributes
%%%     received from IdP, the rule is expanded to its value. Example:
%%%         subjectId => {required, "id"}
%%%         emails => {optional, "mail"}
%%%
%%%  {keyValue, "attrName"} - similar to the "attrName" rule, but the result
%%%     will contain both the attribute key and value as a json object. Example:
%%%         custom => {optional, {keyValue, "schacHomeOrganization"}}
%%%             would set user's custom value to:
%%%                 {"custom": {"schacHomeOrganization": "orgName"}}
%%%             instead of just
%%%                 {"custom": "orgName"}
%%%
%%%  {keyValue, "keyName", <rule>} - similar to the {keyValue, "attrName"}, but
%%%     <rule> can expand to any value and the key in result JSON can be
%%%     specified explicitly. Example:
%%%         custom => {optional, {keyValue, "organization", "schacHomeOrganization"}}
%%%             would set user's custom value to:
%%%                 {"custom": {"organization": "orgName"}}
%%%
%%%  {str, "literal"} - the rule will be expanded to the literal string. Example:
%%%         name => {required, {str, "John Doe"}}
%%%             would make all users have the same name; "John Doe"
%%%
%%%  {str_list, ["str1", "str2"]} - the rule will be expanded to a list of
%%%     literal strings. Example:
%%%         entitlements => {required, {str_list, ["group1", "group2, "group3"]}}
%%%             would make all users have the same three entitlements
%%%
%%%  {nested, ["key1", "key2", {list, "key3"}]} - the rule will be expanded to a
%%%     value nested in a JSON. Special expression: {list, key} can be used to
%%%     parse a list of json objects, each of which has certain key. Example:
%%%         emails => {nested, ["emails", {list, "email"}]}
%%%             would parse the following JSON:
%%%                 {"emails": [
%%%                     {"email": "abc@example.com", "verified": true},
%%%                     {"email": "def@example.com", "verified": false}
%%%                 ]}
%%%             into the following list of emails:
%%%                 ["abc@example.com", "def@example.com"]
%%%
%%%  {replace, "regex", "replacement", <rule>} - replaces matching substring with
%%%     given replacement. <rule> can expand to a string or a list of strings (in
%%%     which case the operation will be repeated on every string). Underneath,
%%%     it uses Erlang's re:replace/4 function, which means that regexes and
%%%     replacements must be built according to Erlang's (slightly different)
%%%     regex format. Example:
%%%         name => {replace, "(.*) (.*) (.*)", "\\1 \\3", "fullName"}
%%%             would change all 3-part names to 2-part, leaving out the middle
%%%             one (e.g. John II Doe -> John Doe). Unmatched strings are not
%%%             modified.
%%%
%%%  {concat, [<ruleA>, <ruleB>, ...]} - concatenates a list of strings into one
%%%     string, one by one: ((<ruleA> + <ruleB>) + <ruleC>) + <ruleD> ...
%%%     Every <rule> must expand to a string or a list of strings. If
%%%     a single string is concatenated with a list, it is done for every
%%%     element of the list. If two lists are concatenated, the elements are
%%%     concatenated in pairs, creating a new list. If any of the lists is
%%%     shorter, it is padded with empty strings. Examine possible combinations:
%%%         {concat, []} ->
%%%             undefined
%%%         {concat, [{str, "a"}]} ->
%%%             "a"
%%%         {concat, [{str, "a"}, {str, "b"}]} ->
%%%             "ab"
%%%         {concat, [{str, "a"}, {str_list, ["1", "2", "3"]}]} ->
%%%             ["a1", "a2", "a3"]
%%%         {concat, [{str, "a"}, {str_list, ["1", "2", "3"]}]} ->
%%%             ["a1", "a2", "a3"]
%%%         {concat, [{str_list, ["a", "b", "c"]}, {str, "1"}]} ->
%%%             ["a1", "b1", "c1"]
%%%         {concat, [{str_list, ["a", "b", "c"]}, {str_list, ["1", "2", "3"]}]} ->
%%%             ["a1", "b2", "c3"]
%%%         {concat, [{str_list, ["a", "b", "c", "d"]}, {str_list, ["1", "2"]}]} ->
%%%             ["a1", "b2", "c", "d"]
%%%     Example:
%%%         {entitlements => {concat, [{str, "group:"}, "groups"]}
%%%             would prefix every user's entitlement with "group:"
%%%
%%%  {join, "joinWith", <rule>} - joins a list of strings with given string.
%%%     <rule> must expand to a list of strings, or a single string (in which
%%%     case the join just return the string unchanged). Example:
%%%         {name => {required, {join, " ", "nameTokens"}}
%%%             would parse the following JSON:
%%%                 {"nameTokens": ["John", "Doe", "Junior"]}
%%%             into the following user name:
%%%                 "John Doe Junior"
%%%
%%%  {split, "splitWith", <rule>} - splits a string into a list of strings on
%%%     given "splitWith" string. <rule> must expand to a string, or a list of
%%%     strings in which case the results of splitting every string will be
%%%     appended in one result list. Example:
%%%         {entitlements => {optional, {split, ",", "groups"}}
%%%             would parse the following JSON:
%%%                 {"groups": "group1,team2,role3"}
%%%             into the following list of entitlements:
%%%                 ["group1", "team2", "role3"]
%%%             or the following JSON:
%%%                 {"groups": ["group1,group2", "team3,team4"]}
%%%             into the following list of entitlements:
%%%                 ["group1", "group2", "team3", "team4"]
%%%
%%%  {append, [<ruleA>, <ruleB>, ...]} - appends lists or json objects together.
%%%     Every <rule> must expand to a string, list or json. Examine possible
%%%     combinations:
%%%         {append, []} ->
%%%             []
%%%         {append, [{str, "a"}]} ->
%%%             ["a"]
%%%         {append, [{str, "a"}, {str_list, ["c", "d"]}]} ->
%%%             ["a", "c", "d"]
%%%         {append, [{str_list, ["a", "b"]}, {str_list, ["c", "d"]}]} ->
%%%             ["a", "b", "c", "d"]
%%%         {append, [{keyValue, "groups"}, {keyValue, "teams"}]} ->
%%%             {"groups": [...], "teams": [...]}
%%%     Example:
%%%         {custom => {append, [{keyValue, "organization"}, "customAttrs"]}
%%%             provided that "customAttrs" is a nested JSON, would give
%%%             something like:
%%%                 {"organization": "my-org", "cusAttr1": "val1", "cusAttr2": "val2"}
%%%
%%%  {filter, "regex", <rule>} - filters a list, leaving only the strings that
%%%     match given "regex". <rule> must expand to a list of strings, or a
%%%     string (in which case it will be treated as a list with one element).
%%%     Example:
%%%         {emails => {required, {filter, ".*@gmail.com", "emails"}}
%%%             would leave only the emails from gmail.com
%%%
%%%  {any, [<ruleA>, <ruleB>, ...]} - tries all rules one by one until any of
%%%     them gives a valid result. In case all of them fail, returns undefined
%%%     value. Example:
%%%         {name => {optional, {any, [{concat, [{str, "John "}, "surName"]}, "userName"]}}
%%%             would set all users' names to:
%%%                 a) "John <surName>" if the attribute "surName" was found
%%%                 b) "<userName>" if the attribute "userName" was found
%%%                 c) undefined (displayed in GUI as "Unnamed User")
%%%                    if none of the attributes was found (if 'optional' was
%%%                    changed to 'required', the login would fail).
%%%
%%% Note that if Attribute mapping is specified in defaultProtocolConfig, it will
%%% be inherited by all IdPs using that protocol (openid or saml). It is possible
%%% to override each key in the IdP config. For example, having the following
%%% config (default and IdP specific):
%%%  defaultProtocolConfig => #{
%%%      attributeMapping => #{
%%%          subjectId => {required, "eduPersonUniqueID"},
%%%          name => {required, ["displayName", "surName"]},
%%%          alias => {optional, "eduPersonPrincipalName"},
%%%          emails => {optional, "mail"}
%%%      }
%%%  }
%%%
%%%  {my_idp, #{
%%%      protocolConfig => #{
%%%          attributeMapping => #{
%%%              subjectId => {required, "eduPersonTargetedID"},
%%%              % if not explicitly set to undefined, alias rules will be
%%%              % inherited from defaultProtocolConfig!
%%%              alias => undefined,
%%%              entitlements => {optional, "groups"}
%%%          }
%%%      }
%%%  }
%%%
%%% Is the same as having such config for the IdP:
%%%  {my_idp, #{
%%%      protocolConfig => #{
%%%          attributeMapping => #{
%%%              subjectId => {required, "eduPersonTargetedID"},
%%%              name => {required, ["displayName", "surName"]},
%%%              alias => undefined,
%%%              emails => {optional, "mail"},
%%%              entitlements => {optional, "groups"}
%%%          }
%%%      }
%%%  }
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(attribute_mapping).

-include("auth/auth_common.hrl").
-include("auth/auth_errors.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

% Public types
-type idp_attributes() :: jiffy:json_value().
-type onedata_attribute() :: subjectId | name | alias | emails | entitlements | custom.
-type attribute_mapping() :: undefined | {required, rule()} | {optional, rule()} | {plugin, module()}.
-export_type([idp_attributes/0, onedata_attribute/0, attribute_mapping/0]).

% Private types
-type attribute_type() :: binary | binary_or_undef | list_of_binaries | json.
% Rules that can be used during attribute mapping, nesting is allowed.
-type rule() :: AttrName :: string() | binary() |
{keyValue, AttrName :: string() | binary()} |
{keyValue, KeyName :: string() | binary(), AttrName :: string() | binary()} |
{str, string()} |
{str_list, [string()]} |
{nested, [rule() | {list, rule()}]} |
{replace, PatternRegExp :: string(), Replacement :: string(), rule()} |
{concat, [rule()]} |
{join, JoinWith :: string(), rule()} |
{split, SplitWith :: string(), rule()} |
{append, [rule()]} |
{filter, PatternRegExp :: string(), rule()} |
{any, [rule()]}.

-define(replace(__Bin, __Regex, __Replacement), re:replace(
    __Bin, __Regex, __Replacement, [{return, binary}, unicode, ucp, global]
)).
-define(matches(__Bin, __Regex), match =:= re:run(
    __Bin, __Regex, [{capture, none}, unicode, ucp]
)).
-define(repeat(__Term, __N), lists:duplicate(__N, __Term)).
-define(split(__Bin, __SplitWith), binary:split(__Bin, __SplitWith, [global, trim_all])).

-export([map_attributes/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Maps attributes received from a IdP into Onedata attributes and build a
%% #linked_account{} record. Throws on failure.
%% @end
%%--------------------------------------------------------------------
-spec map_attributes(auth_config:idp(), idp_attributes()) -> #linked_account{}.
map_attributes(IdP, Attributes) ->
    #linked_account{
        idp = IdP,
        subject_id = map_attribute(IdP, subjectId, binary, Attributes),
        name = map_attribute(IdP, name, binary_or_undef, Attributes),
        alias = map_attribute(IdP, alias, binary_or_undef, Attributes),
        emails = map_attribute(IdP, emails, list_of_binaries, Attributes),
        entitlements = map_attribute(IdP, entitlements, list_of_binaries, Attributes),
        custom = map_attribute(IdP, custom, json, Attributes)
    }.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Maps given attribute based on IdP attribute mapping config.
%% @end
%%--------------------------------------------------------------------
-spec map_attribute(auth_config:idp(), onedata_attribute(), attribute_type(), idp_attributes()) ->
    jiffy:json_value() | no_return().
map_attribute(IdP, Attribute, Type, IdPAttributes) ->
    MappingRule = auth_config:get_attribute_mapping(IdP, Attribute),
    case apply_attribute_mapping(IdP, Attribute, MappingRule, IdPAttributes) of
        {ok, Value} ->
            case ensure_type(Value, Type) of
                {ok, V} -> V;
                {error, bad_type} -> throw(?ERROR_BAD_ATTRIBUTE_TYPE(Attribute, Type))
            end;
        {error, not_found} ->
            throw(?ERROR_CANNOT_RESOLVE_REQUIRED_ATTRIBUTE(Attribute));
        {error, {attribute_mapping_error, EType, EReason, Stacktrace}} ->
            throw(?ERROR_ATTRIBUTE_MAPPING_ERROR(Attribute, IdPAttributes, EType, EReason, Stacktrace))
    end.


-define(catch_errors(__Term), try
    __Term
catch __Type:__Reason ->
    {error, {attribute_mapping_error, __Type, __Reason, erlang:get_stacktrace()}}
end).


-spec apply_attribute_mapping(auth_config:idp(), onedata_attribute(), attribute_mapping(),
    idp_attributes()) ->
    {ok, jiffy:json_value()} | {error, bad_type} | {error, not_found} |
    {error, {attribute_mapping_error, throw | error | exit, term(), []}}.
apply_attribute_mapping(_IdP, _Attribute, undefined, _IdPAttributes) ->
    {ok, undefined};
apply_attribute_mapping(_IdP, _Attribute, {optional, Attr}, IdPAttributes) ->
    case ?catch_errors(compute_attribute(Attr, IdPAttributes)) of
        {ok, Value} -> {ok, Value};
        {error, _} -> {ok, undefined}
    end;
apply_attribute_mapping(_IdP, _Attribute, {required, Attr}, IdPAttributes) ->
    ?catch_errors(compute_attribute(Attr, IdPAttributes));
apply_attribute_mapping(IdP, Attribute, {plugin, Module}, IdPAttributes) ->
    ?catch_errors(Module:map_attribute(IdP, Attribute, IdPAttributes)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Computes attribute value according to (possibly nested) Rule.
%% @end
%%--------------------------------------------------------------------
-spec compute_attribute(rule(), idp_attributes()) ->
    {ok, jiffy:json_value()} | {error, bad_type} | {error, not_found}.
compute_attribute({any, [Rule]}, IdPAttributes) ->
    compute_attribute(Rule, IdPAttributes);
compute_attribute({any, [Rule | Rest]}, IdPAttributes) ->
    % ?catch_errors so as to move to the next rule after one of them crashes
    case ?catch_errors(compute_attribute(Rule, IdPAttributes)) of
        {ok, Value} -> {ok, Value};
        {error, _} -> compute_attribute({any, Rest}, IdPAttributes)
    end;

compute_attribute(AttrName, IdPAttributes) when is_list(AttrName) ->
    compute_attribute(str_utils:unicode_list_to_binary(AttrName), IdPAttributes);
compute_attribute(AttrName, IdPAttributes) when is_binary(AttrName) ->
    case maps:find(AttrName, IdPAttributes) of
        error -> {error, not_found};
        {ok, Value} -> {ok, Value}
    end;

compute_attribute({keyValue, AttrName}, IdPAttributes) ->
    compute_attribute({keyValue, AttrName, AttrName}, IdPAttributes);

compute_attribute({keyValue, KeyName, NestedRule}, IdPAttributes) ->
    case compute_attribute(NestedRule, IdPAttributes) of
        {ok, Value} -> {ok, #{?bin(KeyName) => Value}};
        {error, _} = Err -> Err
    end;

compute_attribute({str, String}, _IdPAttributes) ->
    {ok, ?bin(String)};

compute_attribute({str_list, StringList}, _IdPAttributes) ->
    {ok, [?bin(S) || S <- StringList]};

compute_attribute({nested, []}, NestedAttributes) ->
    {ok, NestedAttributes};
compute_attribute({nested, [{list, NestedAttrName} | Rest]}, NestedAttributes) ->
    {ok, lists:foldl(fun(NestedMap, AttrsAcc) ->
        case compute_attribute({nested, [NestedAttrName | Rest]}, NestedMap) of
            {ok, Value} -> lists:flatten(AttrsAcc ++ [Value]);
            {error, _} -> AttrsAcc
        end
    end, [], NestedAttributes)};
compute_attribute({nested, [NestedAttrName | Rest]}, NestedAttributes) ->
    case compute_attribute(NestedAttrName, NestedAttributes) of
        {ok, Attributes} ->
            compute_attribute({nested, Rest}, Attributes);
        {error, not_found} ->
            {error, not_found}
    end;

compute_attribute({replace, Regex, Replacement, NestedRule}, IdPAttributes) ->
    case compute_attribute(NestedRule, IdPAttributes) of
        {ok, Value} when is_binary(Value) ->
            {ok, ?replace(Value, ?bin(Regex), ?bin(Replacement))};
        {ok, List = [Value | _]} when is_binary(Value) ->
            {ok, [?replace(V, ?bin(Regex), ?bin(Replacement)) || V <- List]};
        {error, Error} ->
            {error, Error}
    end;

compute_attribute({concat, []}, _IdPAttributes) ->
    {error, not_found};
compute_attribute({concat, [NestedRuleA]}, IdPAttributes) ->
    % Only one rule given - compute its value as result
    compute_attribute(NestedRuleA, IdPAttributes);
compute_attribute({concat, [NestedRuleA, NestedRuleB | Rest]}, IdPAttributes) ->
    % Two or more rules given - concatenate
    ComputeAttr = fun(NestedRule) ->
        case compute_attribute(NestedRule, IdPAttributes) of
            {ok, Value} ->
                % Allowed types for concatenation are binaries or lists of binaries
                case ensure_type(Value, binary) of
                    {ok, Bin} -> {ok, Bin};
                    {error, _} -> ensure_type(Value, list_of_binaries)
                end;
            {error, _} = Err ->
                Err
        end
    end,
    Result = case {ComputeAttr(NestedRuleA), ComputeAttr(NestedRuleB)} of
        {{ok, BinA}, {ok, BinB}} when is_binary(BinA) andalso is_binary(BinB) ->
            {ok, <<BinA/binary, BinB/binary>>};
        {{ok, ValA}, {ok, ValB}} ->
            {ListA, ListB} = case {ValA, ValB} of
                {<<_/binary>>, [<<_/binary>> | _]} ->
                    {?repeat(ValA, length(ValB)), ValB};
                {[<<_/binary>> | _], <<_/binary>>} ->
                    {ValA, ?repeat(ValB, length(ValA))};
                {[<<_/binary>> | _], [<<_/binary>> | _]} ->
                    case length(ValA) - length(ValB) of
                        0 ->
                            {ValA, ValB};
                        LenDiff -> {
                            % Pad the lists with empty strings to equalize length
                                ValA ++ ?repeat(<<"">>, max(0, -LenDiff)),
                                ValB ++ ?repeat(<<"">>, max(0, LenDiff))
                        }
                    end
            end,
            {ok, [<<A/binary, B/binary>> || {A, B} <- lists:zip(ListA, ListB)]};
        {{error, Error}, _} ->
            {error, Error};
        {_, {error, Error}} ->
            {error, Error}
    end,
    case {Rest, Result} of
        {[], _} ->
            Result;
        {_, {error, _}} ->
            Result;
        {_, {ok, Bin}} when is_binary(Bin) ->
            compute_attribute({concat, [{str, Bin} | Rest]}, IdPAttributes);
        {_, {ok, List = [Bin | _]}} when is_binary(Bin) ->
            compute_attribute({concat, [{str_list, List} | Rest]}, IdPAttributes)
    end;

compute_attribute({join, JoinWith, NestedRule}, IdPAttributes) ->
    case compute_attribute(NestedRule, IdPAttributes) of
        {ok, Binary} when is_binary(Binary) ->
            {ok, Binary};
        {ok, List = [Binary | _]} when is_binary(Binary) ->
            {ok, str_utils:join_binary(List, ?bin(JoinWith))};
        {ok, []} ->
            {error, not_found};
        {error, Error} ->
            {error, Error}
    end;

compute_attribute({split, SplitWith, NestedRule}, IdPAttributes) ->
    case compute_attribute(NestedRule, IdPAttributes) of
        {ok, Value} when is_binary(Value) ->
            {ok, ?split(Value, ?bin(SplitWith))};
        {ok, List = [Value | _]} when is_binary(Value) ->
            {ok, lists:flatten([?split(V, ?bin(SplitWith)) || V <- List])};
        {error, Error} ->
            {error, Error}
    end;

compute_attribute({append, Rules}, IdPAttributes) ->
    lists:foldl(fun
        (_Rule, {error, AccErr}) ->
            {error, AccErr};
        (Rule, {ok, Acc}) ->
            case {Acc, compute_attribute(Rule, IdPAttributes)} of
                {[], {ok, Value}} when is_map(Value) ->
                    {ok, Value};
                {_, {ok, Value}} when is_map(Value) ->
                    {ok, maps:merge(Acc, Value)};
                {_, {ok, Value}} when is_list(Value) ->
                    {ok, Acc ++ Value};
                {_, {ok, Value}} ->
                    {ok, Acc ++ [Value]};
                {_, {error, Error}} ->
                    {error, Error}
            end
    end, {ok, []}, Rules);

compute_attribute({filter, Regex, NestedRule}, IdPAttributes) ->
    case compute_attribute(NestedRule, IdPAttributes) of
        {ok, Value} ->
            case ensure_type(Value, list_of_binaries) of
                {ok, List} ->
                    {ok, [V || V <- List, ?matches(V, ?bin(Regex))]};
                {error, _} = Err ->
                    Err
            end;
        {error, Error} ->
            {error, Error}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts value to conform to given attribute type, if required. If conversion
%% is not possible, returns {error, bad_type}.
%% @end
%%--------------------------------------------------------------------
-spec ensure_type(Value :: term(), attribute_type()) -> {ok, term()} | {error, bad_type}.
ensure_type(null, Type) ->
    empty(Type);
ensure_type(undefined, Type) ->
    empty(Type);

ensure_type(Bin, binary) when is_binary(Bin) ->
    {ok, Bin};
ensure_type(Int, binary) when is_integer(Int) ->
    {ok, integer_to_binary(Int)};
ensure_type(Float, binary) when is_float(Float) ->
    {ok, float_to_binary(Float)};
ensure_type(true, binary) ->
    {ok, <<"true">>};
ensure_type(false, binary) ->
    {ok, <<"false">>};

ensure_type(Bin, binary_or_undef) ->
    % undefined is covered above
    ensure_type(Bin, binary);

ensure_type(Bin, list_of_binaries) when is_binary(Bin) ->
    {ok, [Bin]};
ensure_type([], list_of_binaries) ->
    {ok, []};
ensure_type(List, list_of_binaries) when is_list(List) ->
    lists:foldl(fun
        (_Element, {error, bad_type}) ->
            {error, bad_type};
        (Element, {ok, Acc}) ->
            case ensure_type(Element, binary) of
                {ok, Value} -> {ok, Acc ++ [Value]};
                {error, bad_type} -> {error, bad_type}
            end
    end, {ok, []}, List);

ensure_type(Json, json) when is_binary(Json) orelse is_map(Json) orelse is_list(Json) ->
    {ok, Json};

ensure_type(_, _) ->
    {error, bad_type}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns empty value for given attribute type, or {error, bad_type} if empty
%% value is not allowed.
%% @end
%%--------------------------------------------------------------------
-spec empty(attribute_type()) -> {ok, term()} | {error, bad_type}.
empty(binary) ->
    {error, bad_type};
empty(binary_or_undef) ->
    {ok, undefined};
empty(list_of_binaries) ->
    {ok, []};
empty(json) ->
    {ok, #{}}.
