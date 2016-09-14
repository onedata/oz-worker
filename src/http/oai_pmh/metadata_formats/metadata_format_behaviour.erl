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

-callback metadata_prefix() -> binary().

-callback schema_URL() -> binary().

-callback main_namespace() -> {atom(), binary()}.

-callback extra_namespaces() -> [{atom(), binary()}].

-callback schema_location() -> binary().

-callback elements() -> [binary()].

-callback encode(Metadata :: #{}) -> #xmlElement{}.