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
-export([required_arguments/0, optional_arguments/0, exclusive_arguments/0,
    required_response_elements/0, optional_response_elements/0, get_element/2]).



-include("http/handlers/oai.hrl").

required_arguments() ->[<<"identifier">>, <<"metadataPrefix">>].

optional_arguments() -> [].

exclusive_arguments() -> [].

required_response_elements() -> [record].

optional_response_elements() -> [].

get_element(record, Args) ->
    Id = proplists:get_value(<<"identifier">>, Args),
%%    io:format("GET RECORD ~p~n", [share_logic:get_metadata(Id)]),
    {ok, Data} =  share_logic:get_metadata(Id),
    Metadata = proplists:get_value(<<"metadata">>, Data),
    DateTime = proplists:get_value(<<"metadata_timestamp">>, Data),
    {MetadataXML, _} = xmerl_scan:string(binary_to_list(Metadata)),

    #oai_record{
        header = #oai_header{
            identifier = Id,
            datestamp = oai_utils:datetime_to_oai_datestamp(DateTime) %TODO convert to proper format
%%            setSpec = [<<"set1">>, <<"set2">>]},
      },
        metadata = #oai_metadata{
            value = MetadataXML
        } %xmerl_scan:string(binary_to_list(MetadaBodyta))
    }.


%%    #oai_record{
%%        header = #oai_header{
%%            identifier = <<"id">>,
%%            datestamp = <<"date">>,
%%            setSpec = [<<"set1">>, <<"set2">>]},
%%        metadata = #oai_metadata{
%%            metadata_format = #oai_metadata_format{metadataPrefix = oai_dc},
%%            value = #{
%%                <<"title">> => <<"Potop">>,
%%                <<"creator">> => <<"Henryk Sienkiewicz">>,
%%                <<"description">> => <<"novel">>
%%            }
%%        }
%%    }.
