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
-module(deleted_handle_registry).
-author("Katarzyna Such").

-include("http/handlers/oai.hrl").

%% API
-export([insert/4, lookup/1, remove/1, foreach/1]).

% link value() encodes 3 pieces of information:
%  1) metadata prefix - specification of metadata format of deleted handle.
%  2) handle service_id - id of the handle service in which the handle has been registered.
%  3) timestamp - contains information about time of handle deletion.
-type link_value() :: binary().

-type entry() :: {od_handle:metadata_prefix(), handle_registry:handle_listing_entry()}.

-define(CTX, (od_handle:get_ctx())).

-define(FOREST, <<"handle-forest">>).
-define(TREE_FOR_DELETED_HANDLES, <<"deleted-handle-tree">>).

-define(LINK_VALUE_SEPARATOR, <<":">>).


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


-spec lookup(od_handle:id()) -> error | {ok, entry()}.
lookup(HandleId) ->
    case datastore_model:get_links(?CTX, ?FOREST, ?TREE_FOR_DELETED_HANDLES, HandleId) of
        {error, not_found} ->
            error;
        {ok, [Link]} ->
            {ok, decode_link(Link)}
    end.


-spec remove(od_handle:id()) -> ok.
remove(HandleId) ->
    case datastore_model:delete_links(?CTX, ?FOREST, ?TREE_FOR_DELETED_HANDLES, HandleId) of
        ok -> ok;
        {error, not_found} -> ok
    end.


-spec foreach(fun((entry()) -> ok)) -> ok.
foreach(ForeachFun) ->
    foreach(ForeachFun, <<"">>).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec foreach(fun((entry()) -> ok), od_handle:id()) -> ok.
foreach(ForeachFun, StartAfterHandleId) ->
    FoldOpts = #{
        size => 1000,
        prev_link_name => StartAfterHandleId,
        prev_tree_id => ?TREE_FOR_DELETED_HANDLES,  % necessary for inclusive => false to work
        inclusive => false
    },

    FoldFun = fun(Link, Acc) ->
        {ok, [decode_link(Link) | Acc]}
    end,

    case datastore_model:fold_links(?CTX, ?FOREST, ?TREE_FOR_DELETED_HANDLES, FoldFun, [], FoldOpts) of
        {ok, []} ->
            ok;
        {ok, ReversedEntries} ->
            lists:foreach(ForeachFun, ReversedEntries),
            {_, LastEntry} = hd(ReversedEntries),
            foreach(ForeachFun, LastEntry#handle_listing_entry.handle_id)
    end.


%% @private
-spec decode_link(datastore:link()) -> entry().
decode_link(#link{name = HandleId, target = LinkValue}) ->
    {MetadataPrefix, HandleServiceId, Timestamp} = decode_link_value(LinkValue),
    {MetadataPrefix, #handle_listing_entry{
        timestamp = Timestamp,
        handle_id = HandleId,
        service_id = HandleServiceId,
        status = deleted
    }}.


%% @private
-spec encode_link_value(od_handle:metadata_prefix(), od_handle_service:id(),
    od_handle:timestamp_seconds()) -> link_value().
encode_link_value(MetadataPrefix, HandleServiceId, Timestamp) ->
    str_utils:join_binary([MetadataPrefix, HandleServiceId, integer_to_binary(Timestamp)], ?LINK_VALUE_SEPARATOR).


%% @private
-spec decode_link_value(link_value()) -> {od_handle:metadata_prefix(), od_handle_service:id(),
    od_handle:timestamp_seconds()}.
decode_link_value(Value) ->
    [MetadataPrefix, HandleServiceId, TimestampBin] = binary:split(Value, [?LINK_VALUE_SEPARATOR], [global]),
    {MetadataPrefix, HandleServiceId, binary_to_integer(TimestampBin)}.
