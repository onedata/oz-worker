%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(oai_verb_behaviour).
-author("Jakub Kudzia").

-include("http/handlers/oai.hrl").

-callback required_arguments() -> [binary()].

-callback optional_arguments() -> [binary()].

-callback exclusive_arguments() -> [binary()].

-callback required_response_elements() -> [atom()].

-callback optional_response_elements() -> [atom()].

-callback get_element(Element :: atom(), Args :: [{Key :: binary(), Value :: binary()}])
        -> binary() | [binary()] | #xmlElement{} | [#xmlElement{}].
