%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(identify).
-author("Jakub Kudzia").

-behaviour(oai_verb_processor_behaviour).


%% API
-export([required_arguments/0, optional_arguments/0, process_request/2, required_response_attributes/0, optional_response_attributes/0]).

-include("http/handlers/oai.hrl").

%%-record(response, {
%%   repositoryName :: binary(),
%%    baseURL :: binary(), % todo ? <<"www.onedata.org">>
%%    protocolVersion :: ?SUPPORTED_VERSION,
%%    earliestDatestamp :: term(), %TODO date in format of oai
%%    deletedRecord :: no | transient | persistent, % todo maybe should be binary
%%    granularity :: oai_granularity(),
%%    adminEmail :: [binary()],
%%    compression, % todo
%%    desription % todo
%%}).





required_arguments() ->[].


optional_arguments() -> [].


process_request(Args, Req) ->
    ParsedArgs = parse_arguments(Args),
    XML = ?ROOT_ELEMENT ,%#xmlElement,
    io:format(lists:flatten(xmerl:export_simple([XML], xmerl_xml))),

    ok.

parse_arguments(Args) ->
    maps:merge(parse_required_arguments(Args), parse_optional_arguments(Args)).


parse_required_arguments(Args) ->
    #{}.

parse_optional_arguments(Args) ->
    #{}.

required_response_attributes() ->
    [repositoryName, baseURL, protocolVersion, earliestDatestamp, granularity, adminEmail].

optional_response_attributes() ->
    [compression, description].

get_attribute(repositoryName) ->
    <<"REPOSITORY NAME">>; % TODO what should be the repository name
get_attribute(baseURL) ->
    {ok, Domain} = application:get_env(?APP_NAME, http_domain),
    {ok, OAI_PREFIX} = application:get_env(?APP_NAME, oai_pmh_prefix ),
    list_to_binary(Domain ++ OAI_PREFIX).
get_attribute(protocolVersion) ->
    % TODO




