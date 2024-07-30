%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour specifies the API for an auth plugin that maps IdP attributes
%%% into Onedata attributes.
%%% @end
%%%-------------------------------------------------------------------
-module(attribute_mapper_behaviour).

-include_lib("ctool/include/logging.hrl").


-export([validate_example/2]).


%%--------------------------------------------------------------------
%% @doc
%% Should parse the received IdP attributes and resolve given Onedata attribute.
%% The Onedata attribute is one of the below, and should return a value in
%% corresponding type:
%%      * subjectId - binary()
%%      * fullName - binary() | undefined
%%      * username - binary() | undefined
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
-callback map_attribute(auth_config:idp(), attribute_mapping:onedata_attribute(),
    attribute_mapping:idp_attributes()) -> {ok, term()} | {error, not_found}.


%%--------------------------------------------------------------------
%% @doc
%% Returns validation examples that will be tested when the plugin is loaded.
%% They serve as unit tests for the plugin.
%% The fourth tuple element is the expected output, one of:
%%  * {ok, Value}
%%  * {error, not_found}
%%  * {error, attribute_mapping_error} (if the code is expected to crash)
%% @end
%%--------------------------------------------------------------------
-callback validation_examples() ->
    [{auth_config:idp(), attribute_mapping:onedata_attribute(), attribute_mapping:idp_attributes(),
        {ok, term()} | {error, not_found} | {error, attribute_mapping_error}}].


%%%===================================================================
%%% API
%%%===================================================================


-spec validate_example(module(), {auth_config:idp(),
    attribute_mapping:onedata_attribute(), attribute_mapping:idp_attributes(),
    {ok, term()} | {error, not_found} | {error, attribute_mapping_error}}) -> ok | no_return().
validate_example(Module, {IdP, Attribute, IdPAttributes, ExpectedOutput}) ->
    ParsingResult = try
        Module:map_attribute(IdP, Attribute, IdPAttributes)
    catch
        Type:Reason:Stacktrace ->
            {error, attribute_mapping_error, Type, Reason, Stacktrace}
    end,
    case {ExpectedOutput, ParsingResult} of
        {Same, Same} ->
            ok;
        {{error, attribute_mapping_error}, {error, attribute_mapping_error, _, _, _}} ->
            ok;
        {_, {error, attribute_mapping_error, EType, EReason, EStacktrace}} ->
            ?error("Validation example crashed:~n"
            "IdP: ~tp~n"
            "Attribute: ~tp~n"
            "IdPAttributes: ~tp~n"
            "Expected: ~tp~n"
            "Error: ~tp~n"
            "Stacktrace: ~ts~n", [
                IdP, Attribute, IdPAttributes, ExpectedOutput, {EType, EReason},
                iolist_to_binary(lager:pr_stacktrace(EStacktrace))
            ]),
            throw(validation_failed);
        {_, Got} ->
            ?error("Validation example failed:~n"
            "IdP: ~tp~n"
            "Attribute: ~tp~n"
            "IdPAttributes: ~tp~n"
            "Expected: ~tp~n"
            "Got: ~tp", [IdP, Attribute, IdPAttributes, ExpectedOutput, Got]),
            throw(validation_failed)
    end.
