%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(metadata_formats).
-author("Jakub Kudzia").

-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([module/1, supported_formats/0, schema_URL/1,
        main_namespace/1, extra_namespaces/1, namespaces/1]).

supported_formats() ->[
    <<"oai_dc">>
].


module(<<"oai_dc">>) -> dublin_core;
module(_) -> not_supported.

schema_URL(MetadataPrefix) ->
    Module= module(MetadataPrefix),
    Module:schema_URL().

main_namespace(MetadataPrefix) ->
    Module= module(MetadataPrefix),
    {_, Namespace} = Module:main_namespace(),
    Namespace.

extra_namespaces(MetadataPrefix) ->
    Module= module(MetadataPrefix),
    Module:extra_namespace().

namespaces(MetadataPrefix) ->
    Module= module(MetadataPrefix),
    [Module:main_namespace() | Module:extra_namespaces()].



