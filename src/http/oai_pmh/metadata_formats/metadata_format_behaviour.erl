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
%%% Returns lists of attributes of given metadata format.
%%% @end
%%%-------------------------------------------------------------------
-callback elements() -> [binary()].

%%%-------------------------------------------------------------------
%%% @doc
%%% This function validates whether the provided metadata conforms to XML standards.
%%% @end
%%%-------------------------------------------------------------------
-callback sanitize_metadata(Metadata :: od_handle:metadata()) -> ok | errors:error().

%%%-------------------------------------------------------------------
%%% @doc
%%% Function capable of encoding given metadata to XML.
%%% AdditionalIdentifiers should be injected into the metadata.
%%% @end
%%%-------------------------------------------------------------------
-callback encode(Metadata :: binary(), AdditionalIdentifiers :: [binary()]) ->
    #xmlElement{}.

%%%-------------------------------------------------------------------
%%% @doc
%%% Returns lists of additional identifiers for given handle.
%%% @end
%%%-------------------------------------------------------------------
-callback resolve_additional_identifiers(Handle :: od_handle:record()) -> [od_handle:public_handle()].