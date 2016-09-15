%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(list_records).
-author("Jakub Kudzia").

%% API
-export([required_arguments/0, optional_arguments/0, exclusive_arguments/0,
    required_response_elements/0, optional_response_elements/0, get_element/2]).

-include("http/handlers/oai.hrl").
-include("http/handlers/oai_errors.hrl").
-include("registered_names.hrl").


required_arguments() -> [<<"metadataPrefix">>].

optional_arguments() -> [<<"from">>, <<"until">>, <<"set">>].

exclusive_arguments() -> [<<"resumptionToken">>].

required_response_elements() -> [record].

optional_response_elements() -> [].

get_element(record, Args) ->
    MetadataPrefix = proplists:get_value(<<"metadataPrefix">>, Args),
    From = proplists:get_value(<<"from">>, Args),
    Until = proplists:get_value(<<"until">>, Args),
    HarvestedRecordIds = oai_utils:harvest(MetadataPrefix, From, Until),
    HarvestedRecords = lists:map(fun(Id) ->
        {ok, [
            {<<"metadata">>, Metadata},
            {_, _},
            {<<"metadata_timestamp">>, Timestamp}]} = share_logic:get_metadata(Id),
        #oai_record{
            header = #oai_header{
                identifier = Id,
                datestamp = oai_utils:datetime_to_oai_datestamp(Timestamp)
            },
            metadata = #oai_metadata{
                metadata_format = #oai_metadata_format{metadataPrefix = MetadataPrefix},
                value = Metadata % todo currently metadata is bare xml
            }
        }
    end, HarvestedRecordIds),
    case HarvestedRecords of
        [] -> {error, ?NO_RECORDS_MATCH};
        _ -> HarvestedRecords
    end .