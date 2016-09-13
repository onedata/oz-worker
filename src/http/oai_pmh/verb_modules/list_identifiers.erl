%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(list_identifiers).
-author("Jakub Kudzia").

-behaviour(oai_verb_behaviour).


%% API
-export([required_arguments/0, optional_arguments/0, exclusive_arguments/0,
    required_response_elements/0, optional_response_elements/0, get_element/2]).

-include("registered_names.hrl").
-include("http/handlers/oai.hrl").


required_arguments() -> [<<"metadataPrefix">>].

optional_arguments() -> [<<"from">>, <<"until">>, <<"set">>].

exclusive_arguments() -> [<<"resumptionToken">>].

required_response_elements() -> [header].

optional_response_elements() -> [].

get_element(header, Args) -> [
        #oai_header{
            identifier = <<"id">>,
            datestamp = <<"date">>,
            setSpec = [<<"setspec1">>, <<"setspec2">>]},
        #oai_header{
            identifier = <<"id2">>,
            datestamp = <<"date2">>,
            setSpec = [<<"setspec3">>]},
        #oai_header{
            identifier = <<"id3">>,
            datestamp = <<"date3">>,
            setSpec = []}
    ].