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
%% Returns attribute mapping validation examples to be evaluated during startup.
%% The fourth tuple element is the expected output, on of:
%%  * {ok, Value}
%%  * {error, not_found}
%%  * {error, attribute_mapping_error} (if the code is expected to crash)
%% @end
%%--------------------------------------------------------------------
-callback validation_examples() ->
    [{auth_config:idp(), attribute_mapping:onedata_attribute(), attribute_mapping:idp_attributes(),
        {ok, term()} | {error, not_found} | {error, attribute_mapping_error}}].


-optional_callbacks([validation_examples/0]).
