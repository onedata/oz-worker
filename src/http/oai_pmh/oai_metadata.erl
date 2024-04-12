%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia, Lukasz Opiola
%%% @copyright (C) 2016-2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Generic module that handles metadata related operations by routing
%%% logic specific for different metadata formats to corresponding
%%% handle metadata plugins.
%%% @end
%%%-------------------------------------------------------------------
-module(oai_metadata).
-author("Jakub Kudzia").
-author("Lukasz Opiola").

-include("http/handlers/oai.hrl").


%% API
-export([supported_formats/0]).
-export([clear_plugin_cache/0]).
-export([schema_URL/1, main_namespace/1]).
-export([revise_for_publication/4, insert_public_handle/3, adapt_for_oai_pmh/2]).
-export([validation_examples/1]).
-export([parse_xml/1]).


-define(PLUGIN_CACHE_KEY, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================


-spec supported_formats() -> [od_handle:metadata_prefix()].
supported_formats() ->
    maps:keys(acquire_prefix_to_plugin_mapping()).


-spec clear_plugin_cache() -> ok.
clear_plugin_cache() ->
    node_cache:clear(?PLUGIN_CACHE_KEY).


-spec schema_URL(od_handle:metadata_prefix()) -> binary().
schema_URL(MetadataPrefix) ->
    Module = module(MetadataPrefix),
    Module:schema_URL().


%%--------------------------------------------------------------------
%% @doc
%% Returns tuple {AttributeName, MainNamespace} where:
%%     * AttributeName is name of XML attribute for main namespace of given
%%       metadata format (associated with MetadataPrefix)
%%     * MainNamespace is value of AttributeName that defines the
%%       correspondence between a metadata format prefix
%%       i.e. dc and the namespace URI
%% @end
%%--------------------------------------------------------------------
-spec main_namespace(od_handle:metadata_prefix()) -> {atom(), binary()}.
main_namespace(MetadataPrefix) ->
    Module = module(MetadataPrefix),
    Module:main_namespace().


-spec revise_for_publication(
    od_handle:metadata_prefix(), od_handle:parsed_metadata(), od_share:id(), od_share:record()
) ->
    {ok, od_handle:parsed_metadata()} | error.
revise_for_publication(MetadataPrefix, Metadata, ShareId, ShareRecord) ->
    Module = module(MetadataPrefix),
    Module:revise_for_publication(Metadata, ShareId, ShareRecord).


% TODO VFS-11906 the upgrade procedure should call revise_for_publication and insert_public_handle on
% DC metadata added in previous system versions, and handle cases when it's not a valid XML
-spec insert_public_handle(od_handle:metadata_prefix(), od_handle:parsed_metadata(), od_handle:public_handle()) ->
    od_handle:parsed_metadata().
insert_public_handle(MetadataPrefix, Metadata, PublicHandle) ->
    Module = module(MetadataPrefix),
    Module:insert_public_handle(Metadata, PublicHandle).


-spec adapt_for_oai_pmh(od_handle:metadata_prefix(), od_handle:parsed_metadata()) ->
    {ok, od_handle:parsed_metadata()} | errors:error().
adapt_for_oai_pmh(MetadataPrefix, Metadata) ->
    Module = module(MetadataPrefix),
    Module:adapt_for_oai_pmh(Metadata).


-spec validation_examples(od_handle:metadata_prefix()) -> [handle_metadata_plugin_behaviour:validation_example()].
validation_examples(MetadataPrefix) ->
    Module = module(MetadataPrefix),
    Module:validation_examples().


-spec parse_xml(od_handle:raw_metadata()) -> {ok, od_handle:parsed_metadata()} | error.
parse_xml(Metadata) ->
    try
        % NOTE: xmerl scans strings in UTF8 (essentially the result of binary_to_list(<<_/utf8>>),
        % but exports as a unicode erlang string - str_utils:unicode_list_to_binary/1
        % must be called after the export
        {RootElement, _} = xmerl_scan:string(binary_to_list(Metadata), [{quiet, true}]),
        {ok, RootElement}   % TODO VFS-11906 consider returning errors from xmerl to the client
    catch Class:Reason:Stacktrace ->
        ?debug_exception("Cannot parse handle metadata", Class, Reason, Stacktrace),
        error
    end.

%%%===================================================================
%%% helpers
%%%===================================================================

%% @private
-spec module(od_handle:metadata_prefix()) -> module().
module(MetadataPrefix) ->
    maps:get(MetadataPrefix, acquire_prefix_to_plugin_mapping()).


%% @private
-spec acquire_prefix_to_plugin_mapping() -> #{od_handle:metadata_prefix() => module()}.
acquire_prefix_to_plugin_mapping() ->
    ?check(node_cache:acquire(?PLUGIN_CACHE_KEY, fun() ->
        PrefixToPlugin = maps_utils:generate_from_list(fun(PluginModule) ->
            {PluginModule:metadata_prefix(), PluginModule}
        end, onezone_plugins:get_plugins(handle_metadata_plugin)),
        {ok, PrefixToPlugin, infinity}
    end)).
