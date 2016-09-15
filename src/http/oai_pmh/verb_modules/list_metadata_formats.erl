%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(list_metadata_formats).
-author("Jakub Kudzia").

-include("http/handlers/oai.hrl").
-include("http/handlers/oai_errors.hrl").

-behaviour(oai_verb_behaviour).

%% API
-export([required_arguments/0, optional_arguments/0, exclusive_arguments/0,
    required_response_elements/0, optional_response_elements/0, get_element/2]).

required_arguments() -> [].

optional_arguments() -> [<<"identifier">>].

exclusive_arguments() -> [].

required_response_elements() -> [metadataFormat].

optional_response_elements() -> [].

get_element(metadataFormat, Args) ->
    case proplists:get_value(<<"identifier">>, Args) of
        undefined ->
            lists:map(fun(MetadataPrefix) ->
                get_metadata_format_info(MetadataPrefix)
            end, metadata_formats:supported_formats());
        Id ->
            try
                {ok, MetadataInfo} =  share_logic:get_metadata(Id),
                case proplists:get_value(<<"metadata_formats">>, MetadataInfo) of
                    [] -> throw(noMetadataFromats);
                    SupportedMetadataPrefixes ->
                        lists:map(fun(MetadataPrefix) ->
                            get_metadata_format_info(MetadataPrefix)
                        end, SupportedMetadataPrefixes)
                end
            catch
                throw:noMetadataFromats ->
                    {error, ?NO_METADATA_FORMATS(str_utils:format(
                        "There are no metadata formats available for item ~s", [Id]))};
                _:_ ->
                    {error, ?ID_DOES_NOT_EXIST(str_utils:format(
                        "The value of the identifier argument \"~s\" "
                        "is unknown in this repository.", [Id]))}
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_metadata_format_info(MetadataPrefix) ->
    try
        SchemaURL = metadata_formats:schema_URL(MetadataPrefix),
        Namespace = metadata_formats:main_namespace(MetadataPrefix),
        #oai_metadata_format{
            metadataPrefix=MetadataPrefix,
            schema=SchemaURL,
            metadataNamespace=Namespace
        }
    catch
        _:_ -> throw(noMetadataFromats)
    end.