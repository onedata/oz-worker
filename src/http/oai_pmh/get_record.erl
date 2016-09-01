%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(get_record).
-author("Jakub Kudzia").

-behaviour(oai_verb_behaviour).


%% API
-export([required_arguments/0, optional_arguments/0, required_response_elements/0, optional_response_elements/0, parse_arguments/1, get_element/1, parse_required_arguments/1, parse_optional_arguments/1]).


-include("http/handlers/oai.hrl").

required_arguments() ->[<<"identifier">>, <<"metedataPrefix">>].

optional_arguments() -> [].

parse_arguments(Args) ->
    parse_required_arguments(Args) ++ parse_optional_arguments(Args).

parse_required_arguments(Args) -> [].

parse_optional_arguments(Args) -> [].

required_response_elements() -> [record].

optional_response_elements() -> [].

get_element(record) ->
    #oai_record{
        header = #oai_header{
            identifier = <<"id">>,
            datestamp = <<"date">>,
            setSpec = ["set1", "set2"]},
        metadata = #oai_metadata{
            metadata_format = oai_dc,
            value = #{}
        }
    }.


