%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Module responsible for handling "ListMetadataFormats" OAI-PMH request.
%%% http://www.openarchives.org/OAI/2.0/openarchivesprotocol.htm#ListMetadataFormats
%%% @end
%%%-------------------------------------------------------------------
-module(list_metadata_formats).
-author("Jakub Kudzia").

-include("http/handlers/oai.hrl").

-behaviour(oai_verb_behaviour).

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
required_arguments() -> [].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback optional_arguments/0
%%% @end
%%%-------------------------------------------------------------------
-spec optional_arguments() -> [binary()].
optional_arguments() -> [<<"identifier">>].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback exclusive_arguments/0
%%% @end
%%%--------------------------------------------------------------------
-spec exclusive_arguments() -> [binary()].
exclusive_arguments() -> [].

%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback required_response_elements/0
%%% @end
%%%-------------------------------------------------------------------
-spec required_response_elements() -> [binary()].
required_response_elements() -> [<<"metadataFormat">>].

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
get_response(<<"metadataFormat">>, Args) ->
    case proplists:get_value(<<"identifier">>, Args) of
        undefined ->
            lists:map(fun(MetadataPrefix) ->
                get_metadata_format_info(MetadataPrefix)
            end, metadata_formats:supported_formats());
        Id ->
            try
                {ok, MetadataInfo} = handle_logic:get_metadata(Id),
                case proplists:get_value(metadata, MetadataInfo) of
                    undefined -> throw(noMetadataFormats);
                    _ ->
                        lists:map(fun(MetadataPrefix) ->
                            get_metadata_format_info(MetadataPrefix)
                        end, metadata_formats:supported_formats())
                end
            catch
                throw:noMetadataFormats ->
                    throw({noMetadataFormats, str_utils:format_bin(
                        "There are no metadata formats available for item ~s", [Id])});
                _:_ ->
                    throw({idDoesNotExist, str_utils:format_bin(
                        "The value of the identifier argument \"~s\" "
                        "is unknown in this repository.", [Id])})
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Returns metadata format record associated with given MetadataPrefix.
%%% Throws if MetadataPrefix is unknown.
%%% @end
%%%-------------------------------------------------------------------
-spec get_metadata_format_info(binary()) -> oai_metadata_format().
get_metadata_format_info(MetadataPrefix) ->
    try
        SchemaURL = metadata_formats:schema_URL(MetadataPrefix),
        {_, Namespace} = metadata_formats:main_namespace(MetadataPrefix),
        #oai_metadata_format{
            metadataPrefix = MetadataPrefix,
            schema = SchemaURL,
            metadataNamespace = Namespace
        }
    catch
        _:_ -> throw(noMetadataFormats)
    end.