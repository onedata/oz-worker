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

-include("registered_names.hrl").


required_arguments() -> [<<"metadataPrefix">>].

optional_arguments() -> [<<"from">>, <<"until">>, <<"set">>].

exclusive_arguments() -> [<<"resumptionToken">>].

required_response_elements() -> [record].

optional_response_elements() -> [].

get_element(record, Args) ->
    erlang:error(not_implemented).