%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% The behaviour that must be implemented by each module responsible
%%% for encoding metadata in given format i.e. Dublin Core
%%% @end
%%%-------------------------------------------------------------------
-module(metadata_format_behaviour).
-author("Jakub Kudzia").

-include_lib("xmerl/include/xmerl.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns metadata prefix for given metadata format.
%%% @end
%%%-------------------------------------------------------------------
-callback metadata_prefix() -> binary().

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns URL of XML schema for given metadata format.
%%% @end
%%%-------------------------------------------------------------------
-callback schema_URL() -> binary().

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns main XML namespace for given metadata format.
%%% @end
%%%-------------------------------------------------------------------
-callback main_namespace() -> {atom(), binary()}.

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns list of extra XML namespaces for given metadata format.
%%% @end
%%%-------------------------------------------------------------------
-callback extra_namespaces() -> [{atom(), binary()}].

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns XML schema location.
%%% @end
%%%-------------------------------------------------------------------
-callback schema_location() -> binary().

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns lists of attributes of given metadata format.
%%% @end
%%%-------------------------------------------------------------------
-callback elements() -> [binary()].

%%%-------------------------------------------------------------------
%%% @doc
%%% Function capable of encoding given metadata to XML.
%%% @end
%%%-------------------------------------------------------------------
-callback encode(Metadata :: #{} | binary()) -> #xmlElement{}.