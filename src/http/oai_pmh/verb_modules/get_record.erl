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
-include("http/handlers/oai_errors.hrl").

required_arguments() ->[<<"identifier">>, <<"metadataPrefix">>].

optional_arguments() -> [].

exclusive_arguments() -> [].

required_response_elements() -> [record].

optional_response_elements() -> [].

get_element(record, Args) ->
    Id = proplists:get_value(<<"identifier">>, Args),
    MetadataPrefix = proplists:get_value(<<"metadataPrefix">>, Args),
    try
        {ok, MetadataInfo} =  share_logic:get_metadata(Id),
        Metadata = proplists:get_value(<<"metadata">>, MetadataInfo),
        DateTime = proplists:get_value(<<"metadata_timestamp">>, MetadataInfo),
        SupportedMetadataFormats = proplists:get_value(<<"metadata_formats">>, MetadataInfo),
        case lists:member(MetadataPrefix, SupportedMetadataFormats) of
            true ->
%%                {MetadataXML, _} = xmerl_scan:string(binary_to_list(Metadata)),
                #oai_record{
                    header = #oai_header{
                        identifier = Id,
                        datestamp = oai_utils:datetime_to_oai_datestamp(DateTime)
                    },
                    metadata = #oai_metadata{
                        metadata_format = #oai_metadata_format{metadataPrefix = MetadataPrefix},
                        value = Metadata % todo currently metadata is bare xml
                    } %xmerl_scan:string(binary_to_list(MetadaBodyta))
                };
            false ->    %todo currently only dc is supported
                {error, ?CANNOT_DISSEMINATE_FORMAT(str_utils:format(
                    "Metadata format ~s is not supported by this repository",
                    [MetadataPrefix]))}
        end
    catch
        E1:E2 -> io:format("ERROR: ~p~p", [E1, E2]),
            {error, ?ID_DOES_NOT_EXIST(str_utils:format(
                    "The value of the identifier argument \"~s\" "
                    "is unknown in this repository.", [Id]))}
    end.



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

