%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module can be used to implement custom IdP attribute mapping per IdP.
%%%
%%% validation_examples/0 callback can be used to provide examples to be
%%% evaluated upon the start of Onezone to make sure that mapping logic works
%%% as expected.
%%% @end
%%%-------------------------------------------------------------------
-module(custom_attribute_mapper_example).
-behavior(auth_plugin_behaviour).
-behavior(entitlement_parser_behaviour).

%% API
-export([type/0]).
-export([map_attribute/3]).
-export([validation_examples/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns the type of this plugin. Depending on the type, the plugin must
%% implement certain behaviour:
%%      entitlement_parser -> entitlement_parser_behaviour
%%      openid_plugin -> openid_plugin_behaviour
%%      attribute_mapper -> attribute_mapper_behaviour
%% @end
%%--------------------------------------------------------------------
-spec type() -> attribute_mapper.
type() ->
    attribute_mapper.


%%--------------------------------------------------------------------
%% @doc
%% Should parse the received IdP attributes and resolve given Onedata attribute.
%% The Onedata attribute is one of the below, and should return a value in
%% corresponding type:
%%      * subjectId - binary()
%%      * name - binary() | undefined
%%      * alias - binary() | undefined
%%      * emails - [binary()]
%%      * entitlements - [binary()]
%%      * custom - json_utils:json_term()
%% The callback should return one of:
%%      * {ok, Value} - upon success, but the Value is still subject to type
%%          check. 'undefined' can be returned, in this case the attribute will
%%          have empty (null) value.
%%      * {error, not_found} - if the attribute was not found, this will cause
%%          the login to fail. If you want to implement optional / default value,
%%          return it as {ok, Value}
%%      * let it crash - if there was an error that you would like to be logged
%%          in Onezone logs.
%% @end
%%--------------------------------------------------------------------
-spec map_attribute(auth_config:idp(), attribute_mapping:onedata_attribute(),
    attribute_mapping:idp_attributes()) -> {ok, term()} | {error, not_found}.
map_attribute(myIdP, subjectId, IdPAttributes) ->
    case maps:find(<<"id">>, IdPAttributes) of
        {ok, Id} -> {ok, Id};
        error -> {error, not_found}
    end;
map_attribute(myIdP, name, IdPAttributes) ->
    % Will crash if there is no "displayName" key
    {ok, maps:get(<<"displayName">>, IdPAttributes)}.


%%--------------------------------------------------------------------
%% @doc
%% Returns attribute mapping validation examples to be evaluated during startup.
%% The fourth tuple element is the expected output, on of:
%%  * {ok, Value}
%%  * {error, not_found}
%%  * {error, attribute_mapping_error} (if the code is expected to crash)
%% @end
%%--------------------------------------------------------------------
-spec validation_examples() ->
    [{auth_config:idp(), attribute_mapping:onedata_attribute(), attribute_mapping:idp_attributes(),
        {ok, term()} | {error, not_found} | {error, attribute_mapping_error}}].
validation_examples() -> [
    {myIdP, subjectId, #{<<"id">> => <<"12345">>}, {ok, <<"12345">>}},
    {myIdP, subjectId, #{<<"sub">> => <<"12345">>}, {error, not_found}},
    {myIdP, name, #{<<"displayName">> => <<"john">>}, {ok, <<"john">>}},
    {myIdP, name, #{<<"id">> => <<"12345">>}, {error, attribute_mapping_error}}
].
