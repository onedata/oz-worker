%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Module responsible for handling "ListRecords" OAI-PMH request.
%%% http://www.openarchives.org/OAI/2.0/openarchivesprotocol.htm#ListRecords
%%% @end
%%%-------------------------------------------------------------------
-module(list_records).
-author("Jakub Kudzia").

-include("http/handlers/oai.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").

%% API
-export([required_arguments/0, optional_arguments/0, exclusive_arguments/0,
    required_response_elements/0, optional_response_elements/0, get_response/2]).


%%%===================================================================
%%% API
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback required_arguments/0
%%% @end
%%%-------------------------------------------------------------------
-spec required_arguments() -> [binary()].
required_arguments() -> [<<"metadataPrefix">>].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback optional_arguments/0
%%% @end
%%%-------------------------------------------------------------------
-spec optional_arguments() -> [binary()].
optional_arguments() -> [<<"from">>, <<"until">>, <<"set">>].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback exclusive_arguments/0
%%% @end
%%%--------------------------------------------------------------------
-spec exclusive_arguments() -> [binary()].
exclusive_arguments() -> [<<"resumptionToken">>].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback required_response_elements/0
%%% @end
%%%-------------------------------------------------------------------
-spec required_response_elements() -> [binary()].
required_response_elements() -> [<<"record">>].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback optional_response_elements/0
%%% @end
%%%-------------------------------------------------------------------
-spec optional_response_elements() -> [binary()].
optional_response_elements() -> [].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback get_response/2
%%% @end
%%%-------------------------------------------------------------------
-spec get_response(binary(), [proplists:property()]) -> oai_response().
get_response(<<"record">>, Args) ->
    MetadataPrefix = proplists:get_value(<<"metadataPrefix">>, Args),
    From = proplists:get_value(<<"from">>, Args),
    Until = proplists:get_value(<<"until">>, Args),
    HarvestingFun = fun(Id, #od_handle{timestamp = Timestamp, metadata = Metadata}) ->
        #oai_record{
            header = #oai_header{
                identifier = oai_utils:oai_identifier_encode(Id),
                datestamp = oai_utils:datetime_to_oai_datestamp(Timestamp)
            },
            metadata = #oai_metadata{
                metadata_format = #oai_metadata_format{metadataPrefix = MetadataPrefix},
                value = Metadata
            }
        }
    end,
    oai_utils:harvest(MetadataPrefix, From, Until, HarvestingFun).

%%% TODO * support resumptionToken
