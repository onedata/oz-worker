%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Module responsible for handling "ListIdentifiers" OAI-PMH request.
%%% http://www.openarchives.org/OAI/2.0/openarchivesprotocol.htm#ListIdentifiers
%%% @end
%%%-------------------------------------------------------------------
-module(list_identifiers).
-author("Jakub Kudzia").

-include("registered_names.hrl").
-include("http/handlers/oai.hrl").
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
%%% NOTE: This operation does not fit the current framework for handling oai requests;
%%% it returns two different types or elements (header/record + resumptionToken) in one
%%% get_response call. We use only ElementName set to <<"header">> as w workaround -
%%% we do not want to list the two elements not to cause two listings.
%%% @end
%%%-------------------------------------------------------------------
-spec required_response_elements() -> [binary()].
required_response_elements() -> [<<"header">>].

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
get_response(<<"header">>, Args) ->
    ListingOpts = oai_utils:request_arguments_to_handle_listing_opts(list_identifiers, Args),
    oai_utils:harvest(ListingOpts, fun oai_utils:build_oai_header/1).

