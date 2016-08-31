%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(learn_xmerl).
-author("Jakub Kudzia").


-include_lib("xmerl/include/xmerl.hrl").
%% API
-export([create_tree/0, write/1, start/0]).

create_tree() ->
    A1 = #xmlAttribute{name=id, value="001"},
    A2 = #xmlAttribute{name=hobby, value="birding"},
    T1 = #xmlText{value="DUPA"},
    N1 = #xmlElement{name=person, content=[T1], attributes=[A1, A2]},
    A3 = #xmlAttribute{name=id, value="002"},
    A4 = #xmlAttribute{name=hobby, value="swiming"},
    T2 = #xmlText{value="Becky"},
    N2 = #xmlElement{name=person, content=[T2], attributes=[A3, A4]},
    N3 = #xmlElement{name=people, content=[N1, N2]},
    N3.

write(Tree) ->
    io:format(lists:flatten(xmerl:export_simple([Tree], xmerl_xml))).

start() ->
    {Tree, _} = xmerl_scan:file("m.xml"),
    Tree.

%%person() ->
%%    io:format(lists:flatten(xmerl:export([Tree], xmerl_xml))).

%%'#xml-inheritance#'() -> [xmerl_xml].