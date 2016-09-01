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

-behaviour(oai_verb_behaviour).

%% API
-export([required_arguments/0, optional_arguments/0, exclusive_arguments/0,
    required_response_elements/0, optional_response_elements/0, get_element/1]).

required_arguments() -> [].

optional_arguments() -> [<<"identifier">>].

exclusive_arguments() -> [].

required_response_elements() -> [metadataFormat].

optional_response_elements() -> [].

get_element(metadataFormat) ->
    erlang:error(not_implemented).