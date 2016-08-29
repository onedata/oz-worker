%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(oai_verb_processor_behaviour).
-author("Jakub Kudzia").

-callback arguments() -> [binary()].

-callback required_arguments() -> [binary()].

-callback optional_arguments() -> [binary()].

-callback process_request() -> ok .

-callback parse_arguments() -> #{}.

-callback parse_required_arguments() -> #{}.

-callback parse_optional_arguments() -> #{}.

%%-callback write(Data) -> iodata().
%%-callback TODO maybe optional and required response attributes