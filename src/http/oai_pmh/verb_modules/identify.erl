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
-export([required_arguments/0, optional_arguments/0, exclusive_arguments/0,
    required_response_elements/0, optional_response_elements/0, get_element/2]).

-include("http/handlers/oai.hrl").
-include("registered_names.hrl").

required_arguments() ->[].

optional_arguments() -> [].

exclusive_arguments() -> [].

required_response_elements() -> [
    repositoryName, baseURL, protocolVersion,
    earliestDatestamp, deletedRecord, granularity, adminEmail].

optional_response_elements() ->
    [compression, description].

get_element(repositoryName, _Args) ->
    <<"REPOSITORY NAME">>; % TODO what should be the repository name
get_element(baseURL, _Args) ->
    {ok, Domain} = application:get_env(?APP_Name, http_domain),
    {ok, OAI_PREFIX} = application:get_env(?APP_Name, oai_pmh_api_prefix ),
    list_to_binary(Domain ++ OAI_PREFIX);
get_element(protocolVersion, _Args) ->
    ?PROTOCOL_VERSION;
get_element(earliestDatestamp, _Args) ->
    <<"1970-01-01T00:00:00Z">>; % TODO how to get it
get_element(deletedRecord, _Args) ->
    <<"no">> ;
get_element(granularity, _Args) ->
    <<"YYYY-MM-DDThh:mm:ss:Z">> ;% TODO can be <<"YYYY-MM-DD">>
get_element(adminEmail, _Args) ->
    [<<"a@mail.com">>, <<"b@mail.com">>];
get_element(compression, _Args) ->
    <<"compression">>;
get_element(description, _Args) -> [
    #xmlElement{name=description1, attributes = [#xmlAttribute{name=xml_schema}]}, %TODO it's example
    #xmlElement{name=description2, attributes = [#xmlAttribute{name=xml_schema}]}].  %TODO it's example



