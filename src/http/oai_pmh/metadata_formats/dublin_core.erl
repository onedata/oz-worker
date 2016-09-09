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

-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([elements/0, encode/1]).

-define(OAI_DC_XML_NAMESPACE, #xmlAttribute{
    name='xmlns:oai_dc',
    value= "http://www.openarchives.org/OAI/2.0/oai_dc/"}).
-define(DC_XML_NAMESPACE, #xmlAttribute{
    name='xmlns:dc',
    value="http://purl.org/dc/elements/1.1/"}).
-define(DC_XML_SCHEMA_NAMESPACE, #xmlAttribute{
    name='xml:xsi',
    value="http://www.w3.org/2001/XMLSchema-instance"}).

-define(DC_XSI_SCHEMA_LOCATION, #xmlAttribute{
    name='xsi:schemaLocation',
    value = "http://www.openarchives.org/OAI/2.0/oai_dc/ http://www.openarchives.org/OAI/2.0/oai_dc.xsd"}).

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


-spec encode(#{}) -> #xmlElement{}.
encode(Metadata) ->
    XMLElements = lists:flatmap(fun(Key) ->
        case maps:get(Key, Metadata, undefined) of
            undefined -> [];
            Value -> [#xmlElement{
                name=binary_to_atom(<<"dc:", Key/binary>>, latin1),
                content=[ensure_string(Value)]}]
        end
    end, elements()),

    #xmlElement{
        name='oai_dc:dc',
        attributes = [
            ?OAI_DC_XML_NAMESPACE,
            ?DC_XML_NAMESPACE,
            ?DC_XML_SCHEMA_NAMESPACE,
            ?DC_XSI_SCHEMA_LOCATION],
        content = XMLElements}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


ensure_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
ensure_string(Value) when is_atom(Value) ->
    atom_to_list(Value);
ensure_string(Value) when is_list(Value) ->
    Value.