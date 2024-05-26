%%%-------------------------------------------------------------------
%%% @author Katarzyna Such
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Link-tree-based persistent storage for deleted OpenData handles.
%%% @end
%%%-------------------------------------------------------------------
-module(deleted_handles).
-author("Katarzyna Such").

-include("http/handlers/oai.hrl").

%% API
-export([insert/4, lookup/1, foreach/1, remove/1]).

% link value() encodes 3 pieces of information:
%  1) metadata prefix - specification of metadata format of deleted handle.
%  2) handle service_id - id of the handle service in which the handle has been registered.
%  3) timestamp - contains information about time of handle deletion.
-type link_value() :: binary().

-define(CTX, (od_handle:get_ctx())).

-define(FOREST, <<"handle-forest">>).
-define(TREE_FOR_DELETED_HANDLES, <<"deleted-handle-tree">>).

-define(VALUE_SEP, <<":">>).


%%%===================================================================
%%% API
%%%===================================================================


-spec insert(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds()) -> ok.
insert(MetadataPrefix, HandleServiceId, HandleId, Timestamp) ->
    Link = {HandleId, encode_link_value(MetadataPrefix, HandleServiceId, Timestamp)},
    case datastore_model:add_links(?CTX, ?FOREST, ?TREE_FOR_DELETED_HANDLES, Link) of
        {ok, _} -> ok;
        {error, already_exists} -> throw(?ERROR_ALREADY_EXISTS)
    end.


-spec lookup(od_handle:id()) -> error | {ok, handles:handle_listing_entry(), od_handle:metadata_prefix()}.
lookup(HandleId) ->
    case datastore_model:get_links(?CTX, ?FOREST, ?TREE_FOR_DELETED_HANDLES, HandleId) of
        {ok, [#link{target = LinkValue}]} ->
            {MetadataPrefix, HandleServiceId, Timestamp} = decode_link_value(LinkValue),
            {ok, #handle_listing_entry{
                timestamp = Timestamp,
                service_id = HandleServiceId,
                handle_id = HandleId,
                status = deleted
            }, MetadataPrefix};
        {error, not_found} -> error
    end.


-spec foreach(function()) -> ok.
foreach(ForeachFun) ->
    FoldFun = fun(#link{name = HandleId, target = Value}, Acc) ->
        {MetadataPrefix, HandleServiceId, Timestamp} = decode_link_value(Value),
        {ok, [{MetadataPrefix, #handle_listing_entry{
            timestamp = Timestamp,
            service_id = HandleServiceId,
            handle_id = HandleId,
            status = deleted
        }} | Acc]}
    end,
    {ok, InternalEntries} = datastore_model:fold_links(
        ?CTX, ?FOREST, ?TREE_FOR_DELETED_HANDLES, FoldFun, [], #{size => 1000}
    ),
    case InternalEntries of
        [] ->
            ok;
        _ ->
            lists:foreach(ForeachFun, InternalEntries),
            foreach(ForeachFun)
    end.


-spec remove(od_handle:id()) -> ok.
remove(HandleId) ->
    case datastore_model:delete_links(?CTX, ?FOREST, ?TREE_FOR_DELETED_HANDLES, HandleId) of
        ok -> ok;
        {error, not_found} -> ok
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec encode_link_value(od_handle:metadata_prefix(), od_handle_service:id(),
    od_handle:timestamp_seconds()) -> link_value().
encode_link_value(MetadataPrefix, HandleServiceId, Timestamp) ->
    str_utils:join_binary([MetadataPrefix, HandleServiceId, integer_to_binary(Timestamp)], ?VALUE_SEP).


%% @private
-spec decode_link_value(link_value()) -> {od_handle:metadata_prefix(), od_handle_service:id(),
    od_handle:timestamp_seconds()}.
decode_link_value(Value) ->
    [MetadataPrefix, HandleServiceId, TimestampBin] = binary:split(Value, [?VALUE_SEP], [global]),
    {MetadataPrefix, HandleServiceId, binary_to_integer(TimestampBin)}.

