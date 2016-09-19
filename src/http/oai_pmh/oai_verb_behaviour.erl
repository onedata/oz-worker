%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% The behaviour that must be implemented by each module responsible
%%% for handling OAI-PMH requests for given OAI-PMH verb
%%% @end
%%%-------------------------------------------------------------------
-module(oai_verb_behaviour).
-author("Jakub Kudzia").

-include("http/handlers/oai.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns list of arguments that are required for given request.
%%& If such argument is not passed, request will be responded with
%%% badArgument error code.
%%% @end
%%%-------------------------------------------------------------------
-callback required_arguments() -> [binary()].

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns list of arguments that are optional for given request.
%%% @end
%%%-------------------------------------------------------------------
-callback optional_arguments() -> [binary()].

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns list of exclusive arguments for given request.
%%% Exclusive argument must be the only argument (except verb argument).
%%% @end
%%%-------------------------------------------------------------------
-callback exclusive_arguments() -> [binary()].

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns list of XML elements that are required in OIA-PMH response.
%%% @end
%%%-------------------------------------------------------------------
-callback required_response_elements() -> [binary()].

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns list of XML elements that are optional in OIA-PMH response.
%%% @end
%%%-------------------------------------------------------------------
-callback optional_response_elements() -> [binary()].


%%%-------------------------------------------------------------------
%%% @doc
%%% Function responsible for generating response to given request.
%%% If generating response fails, function must throw exception in ont
%%% of the following ways:
%%%     throw:ErrorCode
%%%     or
%%%     throw:{ErrorCode, Description}
%%% where ErrorCode is an atom defining error code as defined in OAI-PMH
%%% documentation available on:
%%% http://www.openarchives.org/OAI/2.0/openarchivesprotocol.htm#ErrorConditions
%%% Error will be handled by oai_errors module.
%%% This function must be defined for all elements returned by
%%% required_response_elements().
%%% @end
%%%-------------------------------------------------------------------

-callback get_response(Element :: binary(), Args :: proplist()) -> oai_response().