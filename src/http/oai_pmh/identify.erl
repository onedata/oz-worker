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
-export([required_arguments/0, optional_arguments/0, process_request/2,
    required_response_attributes/0, optional_response_attributes/0, get_attribute/1, get_attributes/1, generate_xml/2, generate_xml/3, parse_arguments/1, parse_required_arguments/1, parse_optional_arguments/1]).

-include("http/handlers/oai.hrl").
-include("registered_names.hrl").


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

    XML = ?ROOT_ELEMENT#xmlElement{content=get_attributes(Args)} ,%#xmlElement,

%%    io:format("DEBUG::~n~p~nDEBUG~n", [XML]),
    io:format(lists:flatten(xmerl:export_simple([XML], xmerl_xml))),
    xmerl:export_simple([XML], xmerl_xml).

get_attributes(Args) ->

    RequiredAttributes = lists:flatmap(fun(A) ->
        generate_xml(A, get_attribute(A))
    end, required_response_attributes()),

    OptionalAttributes = lists:flatmap(fun(A) ->
        try generate_xml(A, get_attribute(A)) of
            XML -> XML
        catch _:_ -> []
        end
    end, optional_response_attributes()),
    io:format("Optional ATTRIBUTES: ~n~p~nEND~n", [OptionalAttributes]),
    RequiredAttributes ++ OptionalAttributes.


generate_xml(Name, Value) ->
    generate_xml([], Name, Value).

%%    [generate_xml(Name, Value) | generate_xml(Name, Values)];
%%generate_xml(Name, Value) ->
%%    #xmlElement{name=Name, content=[Value]}.

generate_xml(XML, _Name, []) -> lists:reverse(XML);
generate_xml(XML, Name, [#xmlElement{} = Value | Values]) ->
    generate_xml([#xmlElement{name=Name, content=[Value]} | XML], Name, Values);
generate_xml(XML, Name, [Value | Values]) ->
    generate_xml([#xmlElement{name=Name, content=[binary_to_list(Value)]} | XML], Name, Values);
generate_xml(XML, Name, Value = #xmlElement{}) ->
    generate_xml([#xmlElement{name=Name, content=[Value]} | XML], Name, []);
generate_xml(XML, Name, Value) ->
    generate_xml([#xmlElement{name=Name, content=[binary_to_list(Value)]} | XML], Name, []).


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
%%get_attribute(compression) ->
%%    <<"compression">>;
get_attribute(description) ->
    [
        #xmlElement{name=description1, attributes = [#xmlAttribute{name=xml_schema}]},
        #xmlElement{name=description2, attributes = [#xmlAttribute{name=xml_schema}]}
    ].



