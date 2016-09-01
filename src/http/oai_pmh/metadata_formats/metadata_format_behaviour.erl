%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(metadata_format_behaviour).
-author("Jakub Kudzia").

-include_lib("xmerl/include/xmerl.hrl").

-callback encode(Metadata :: #{}) -> #xmlElement{}.

-callback elements() -> [binary()].
