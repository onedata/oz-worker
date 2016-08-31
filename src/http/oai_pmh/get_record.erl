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
-export([required_arguments/0, optional_arguments/0]).


-include("http/handlers/oai.hrl").

required_arguments() ->[<<"identifier">>, <<"metedataPrefix">>].

optional_arguments() -> [].

parse_arguments(Args) ->
    parse_required_arguments(Args) ++ parse_optional_arguments(Args).

parse_required_arguments(Args) -> [].

parse_optional_arguments(Args) -> [].

required_response_elements() -> [record].

optional_response_elements() -> [].

get_element(record) -> ok
    .