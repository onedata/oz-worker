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

-callback parse_arguments() -> #{}.

-callback parse_required_arguments() -> #{}.

-callback parse_optional_arguments() -> #{}.

-callback required_response_attributes() -> [atom()].

-callback optional_response_attributes() -> [atom()].

-callback get_attribute() -> binary() | [binary()] | #xmlElement{} | [#xmlElement{}].

%%-callback write(Data) -> iodata().
%%-callback TODO maybe optional anbd required response attributes