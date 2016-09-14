%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(dublin_core).
-author("Jakub Kudzia").

-behaviour(metadata_format_behaviour).

-include("http/handlers/oai.hrl").

%% API
-export([elements/0, encode/1, metadata_prefix/0, schema_URL/0, extra_namespaces/0, schema_location/0, main_namespace/0]).

-define(OAI_DC_XML_NAMESPACE, #xmlAttribute{
    name='xmlns:oai_dc',
    value= "http://www.openarchives.org/OAI/2.0/oai_dc/"}).
-define(DC_XML_NAMESPACE, #xmlAttribute{
    name='xmlns:dc',
    value="http://purl.org/dc/elements/1.1/"}).

-define(DC_XSI_SCHEMA_LOCATION, #xmlAttribute{
    name='xsi:schemaLocation',
    value = "http://www.openarchives.org/OAI/2.0/oai_dc/ http://www.openarchives.org/OAI/2.0/oai_dc.xsd"}).

metadata_prefix() -> <<"oai_dc">>.

schema_URL() -> <<"http://www.openarchives.org/OAI/2.0/oai_dc.xsd">>.

main_namespace() ->
    {'xmlns:oai_dc', <<"http://www.openarchives.org/OAI/2.0/oai_dc/">>}.

extra_namespaces() -> [
    {'xmlns:dc', "http://purl.org/dc/elements/1.1/"}
].

schema_location() ->
    {_, MainNamespace} = main_namespace(),
    str_utils:format_bin("~s ~s", [MainNamespace, schema_URL()]).


elements() -> [
    <<"title">>,
    <<"creator">>,
    <<"subject">>,
    <<"description">>,
    <<"publisher">>,
    <<"contributor">>,
    <<"date">>,
    <<"type">>,
    <<"format">>,
    <<"identifier">>,
    <<"source">>,
    <<"language">>,
    <<"relation">>,
    <<"coverage">>,
    <<"rights">>
].


format_info() -> ok.



encode(Metadata) ->
    {MetadataXML, _} = xmerl_scan:string(binary_to_list(Metadata)),
    MetadataXML. %todo currently bare xml is saved
%%    XMLElements = lists:flatmap(fun(Key) ->
%%        case maps:get(Key, Metadata, undefined) of
%%            undefined -> [];
%%            Value -> [#xmlElement{
%%                name=binary_to_atom(<<"dc:", Key/binary>>, latin1),
%%                content=[str_utils:to_list(Value)]}]
%%        end
%%    end, elements()),
%%
%%    #xmlElement{
%%        name='oai_dc:dc',
%%        attributes = [
%%            ?OAI_DC_XML_NAMESPACE,
%%            ?DC_XML_NAMESPACE,
%%            ?OAI_XML_SCHEMA_NAMESPACE,
%%            ?DC_XSI_SCHEMA_LOCATION],
%%        content = XMLElements}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
