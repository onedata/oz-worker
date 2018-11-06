%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% This module is responsible for handling OAI-PMH "GetRecord" request.
%%% http://www.openarchives.org/OAI/2.0/openarchivesprotocol.htm#GetRecord
%%% @end
%%%-------------------------------------------------------------------
-module(get_record).
-author("Jakub Kudzia").

-include("http/handlers/oai.hrl").
-include_lib("ctool/include/logging.hrl").
-include("datastore/oz_datastore_models.hrl").

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
required_arguments() -> [<<"identifier">>, <<"metadataPrefix">>].


%%%-------------------------------------------------------------------
%%% @doc
%%% {@link oai_verb_behaviour} callback optional_arguments/0
%%% @end
%%%-------------------------------------------------------------------
-spec optional_arguments() -> [binary()].
optional_arguments() -> [].


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
    OaiId = proplists:get_value(<<"identifier">>, Args),
    MetadataPrefix = proplists:get_value(<<"metadataPrefix">>, Args),
    Handle = get_handle_safe(OaiId),
    %% TODO check if metadataPrefix is available for given identifier
    case lists:member(MetadataPrefix, metadata_formats:supported_formats()) of
        true ->
            oai_utils:build_oai_record(MetadataPrefix, OaiId, Handle);
        false ->
            throw({cannotDisseminateFormat, MetadataPrefix})
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Returns given handle metadata.
%%% If it fails, throws idDoesNotExist
%%% @end
%%%-------------------------------------------------------------------
-spec get_handle_safe(oai_id()) -> #od_handle{}.
get_handle_safe(OAIId) ->
    try
        Id = oai_utils:oai_identifier_decode(OAIId),
        oai_utils:get_handle(Id)
    catch
        throw:{illegalId, OAIId} ->
            throw({illegalId, OAIId});
        _:_ ->
            throw({idDoesNotExist, OAIId})
    end.