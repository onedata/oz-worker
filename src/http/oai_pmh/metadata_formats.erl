%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% This module serves information about supported metadata formats.
%%% @end
%%%-------------------------------------------------------------------
-module(metadata_formats).
-author("Jakub Kudzia").

-include_lib("xmerl/include/xmerl.hrl").

%%% API
-export([module/1, supported_formats/0, schema_URL/1,
        main_namespace/1]).


%%%===================================================================
%%% API
%%%===================================================================

%%%--------------------------------------------------------------------
%%% @doc
%%% Returns list of currently supported metadata formats.
%%% @end
%%%--------------------------------------------------------------------
-spec supported_formats() -> [binary()].
supported_formats() ->[
    <<"oai_dc">>
].

%%%--------------------------------------------------------------------
%%% @doc
%%% Maps MetadataPrefix to name of module handling given format.
%%% @end
%%%--------------------------------------------------------------------
-spec module(MetadataPrefix :: binary()) -> atom().
module(<<"oai_dc">>) -> dublin_core;
module(_MetadataPrefix) -> not_supported.

%%%--------------------------------------------------------------------
%%% @doc
%%% Returns URL of XML schema of given metadata prefix (associated with
%%% MetadataPrefix).
%%% @end
%%%--------------------------------------------------------------------
-spec schema_URL(MetadataPrefix :: binary()) -> binary().
schema_URL(MetadataPrefix) ->
    Module= module(MetadataPrefix),
    Module:schema_URL().

%%%--------------------------------------------------------------------
%%% @doc
%%% Returns tuple {AttributeName, MainNamespace} where:
%%%     * AttributeName is name of XML attribute for main namespace of given
%%%       metadata format (associated with MetadataPrefix)
%%%     * MainNamespace is value of AttributeName that defines the
%%%       correspondence between a metadata format prefix
%%%       i.e. dc and the namespace URI
%%% @end
%%%--------------------------------------------------------------------
-spec main_namespace(MetadataPrefix :: binary()) -> {atom(), binary()}.
main_namespace(MetadataPrefix) ->
    Module= module(MetadataPrefix),
    Module:main_namespace().





