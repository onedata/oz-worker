%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utils for XML parsing, manipulation and exporting.
%%%
%%% NOTE: xmerl scans strings in UTF8 (essentially the result of binary_to_list(<<_/utf8>>),
%%% but exports as a unicode erlang string - str_utils:unicode_list_to_binary/1
%%% must be called after the export.
%%% @end
%%%-------------------------------------------------------------------
-module(oai_xml).
-author("Lukasz Opiola").

-include("http/handlers/oai.hrl").


%% API
-export([parse/1, encode/1]).
-export([prepend_element_with_indent/3]).


%%%===================================================================
%%% API
%%%===================================================================

-spec parse(od_handle:raw_metadata()) -> {ok, od_handle:parsed_metadata()} | error.
parse(Metadata) ->
    try
        {RootElement, _} = xmerl_scan:string(binary_to_list(Metadata), [{quiet, true}]),
        {ok, RootElement}   % TODO VFS-7454 consider returning errors from xmerl to the client
    catch Class:Reason:Stacktrace ->
        ?debug_exception("Cannot parse handle metadata", Class, Reason, Stacktrace),
        error
    end.


-spec encode(od_handle:parsed_metadata()) -> od_handle:raw_metadata().
encode(Xml) ->
    str_utils:unicode_list_to_binary(xmerl:export_simple([Xml], xmerl_xml, [
        {prolog, ["<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"]}
    ])).


%%-------------------------------------------------------------------
%% @doc
%% This is the suggested way of inserting elements, because it does not
%% impact the formatting (whitespaces) of the original XML. Insertion
%% in the middle or the end is more complicated due to the #xmlText{}
%% entries between elements.
%%
%% May not be suitable if the element order is important.
%% @end
%%-------------------------------------------------------------------
-spec prepend_element_with_indent(non_neg_integer(), #xmlElement{}, [#xmlElement{} | #xmlText{}]) ->
    [#xmlElement{} | #xmlText{}].
prepend_element_with_indent(IndentSize, NewElement, BaseXmlContent) ->
    [#xmlText{value = "\n" ++ lists:duplicate(IndentSize, $ )}, NewElement | BaseXmlContent].