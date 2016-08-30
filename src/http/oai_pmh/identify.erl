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

-behaviour(oai_verb_behaviour).


%% API
-export([required_arguments/0, optional_arguments/0, required_response_attributes/0,
    optional_response_attributes/0, get_attribute/1, parse_arguments/1,
    parse_required_arguments/1, parse_optional_arguments/1]).

-include("http/handlers/oai.hrl").
-include("registered_names.hrl").

required_arguments() ->[].

optional_arguments() -> [].

parse_arguments(Args) ->
    maps:merge(parse_required_arguments(Args), parse_optional_arguments(Args)).

parse_required_arguments(Args) -> #{}.

parse_optional_arguments(Args) -> #{}.

required_response_attributes() -> [
    repositoryName, baseURL, protocolVersion,
    earliestDatestamp, granularity, adminEmail].

optional_response_attributes() ->
    [compression, description].

get_attribute(repositoryName) ->
    <<"REPOSITORY NAME">>; % TODO what should be the repository name
get_attribute(baseURL) ->
    {ok, Domain} = application:get_env(?APP_Name, http_domain),
    {ok, OAI_PREFIX} = application:get_env(?APP_Name, oai_pmh_prefix ),
    list_to_binary(Domain ++ OAI_PREFIX);
get_attribute(protocolVersion) ->
    ?PROTOCOL_VERSION;
get_attribute(earliestDatestamp) ->
    <<"1970-01-01T00:00:00Z">>; % TODO how to get it
get_attribute(deletedRecord) ->
    <<"no">> ; % TODO can be no, transient, persistent
get_attribute(granularity) ->
    <<"YYYY-MM-DDThh:mm:ss:Z">> ;% TODO can be <<"YYYY-MM-DD">>
get_attribute(adminEmail) ->
    [<<"a@mail.com">>, <<"b@mail.com">>];
get_attribute(compression) ->
    <<"compression">>;
get_attribute(description) -> [
    #xmlElement{name=description1, attributes = [#xmlAttribute{name=xml_schema}]}, %TODO it's example
    #xmlElement{name=description2, attributes = [#xmlAttribute{name=xml_schema}]}].  %TODO it's example



