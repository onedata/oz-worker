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
-export([required_arguments/0, optional_arguments/0,
    required_response_elements/0, optional_response_elements/0
    , get_element/1
]).


-include("http/handlers/oai.hrl").

required_arguments() ->[<<"identifier">>, <<"metadataPrefix">>].

optional_arguments() -> [].

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


