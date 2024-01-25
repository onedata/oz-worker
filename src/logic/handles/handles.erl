%%%-------------------------------------------------------------------
%%% @author Katarzyna Such
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Link-tree-based persistent storage for OpenData handles,
%%% suitable for browsing by time periods and specific handle services.
%%% @end
%%%-------------------------------------------------------------------
-module(handles).
-author("Katarzyna Such").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

-export([index/2]).
-export([add/4, delete/4, update/5]).
-export([list/1, get_earliest_timestamp/0]).

% index() consists of 2 parts:
%  1) timestamp (in seconds) - so that links would be sorted by time.
%  2) handle id - this part allows to distinguish links associated with handles
%                that have the same timestamp.
-type index() :: binary().

-type limit() :: pos_integer().
-type size() :: pos_integer().
-type metadata_prefix() :: binary().
-type service_id() :: binary().

%% @formatter:off
-type listing_opts() :: #{
    size := size(),
    limit := limit(),
    start_index => index(),
    metadata_prefix => metadata_prefix(),
    service_id => service_id()
}.
%% @formatter:on

-type resumption_token() :: index().

-define(CTX, (od_handle:get_ctx())).

-define(FOREST, <<"handle-forest">>).
-define(TREE_FOR_METADATA_PREFIX(Prefix),
    <<"handle-metadata", Prefix/binary, "-all-tree">>).
-define(TREE_FOR_METADATA_PREFIX_AND_HSERVICE(Prefix, HServiceId),
    <<"tree-for-", Prefix/binary, "-of-service-", HServiceId/binary>>).

% Uses null for separator to ensure alphabetical sorting
-define(INDEX_SEP, 0).
-define(DEFAULT_LIST_LIMIT, 1000).
-define(MAX_LIST_LIMIT, 1000).
-define(MAX_TIMESTAMP, 99999999999).


%%%===================================================================
%%% API
%%%===================================================================


-spec index(time:seconds(), od_handle:id()) -> index().
index(From, first) ->
    case From of
        undefined -> <<>>;
        _ -> str_utils:format_bin("~11..0B", [From])
    end;
index(TimeSeconds, HandleId) ->
    FormattedTimeSeconds = str_utils:format_bin("~11..0B", [TimeSeconds]),
    <<(FormattedTimeSeconds)/binary, ?INDEX_SEP, HandleId/binary>>.


-spec decode_index(index()) -> {time:seconds(), od_handle:id()}.
decode_index(Index) ->
    <<TimeSeconds:11/binary, 0, HandleId/binary>> = Index,
    {binary_to_integer(TimeSeconds), HandleId}.


-spec add(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(), time:seconds()) -> ok.
add(MetadataPrefix, HandleServiceId, HandleId, TimeSeconds) ->
    Link = {index(TimeSeconds, HandleId), HandleId},

    lists:foreach(fun(TreeId) ->
        case datastore_model:add_links(?CTX, ?FOREST, TreeId, Link) of
            {ok, _} -> ok;
            {error, already_exists} -> throw(?ERROR_ALREADY_EXISTS)
        end
    end, [
        ?TREE_FOR_METADATA_PREFIX(MetadataPrefix),
        ?TREE_FOR_METADATA_PREFIX_AND_HSERVICE(MetadataPrefix, HandleServiceId)
    ]).


-spec delete(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(), time:seconds()) -> ok.
delete(MetadataPrefix, HandleServiceId, HandleId, TimeSeconds) ->
    Index = index(TimeSeconds, HandleId),
    lists:foreach(fun(TreeId) ->
        case datastore_model:delete_links(?CTX, ?FOREST, TreeId, Index) of
            ok -> ok;
            {error, not_found} -> ok
        end
    end, [
        ?TREE_FOR_METADATA_PREFIX(MetadataPrefix),
        ?TREE_FOR_METADATA_PREFIX_AND_HSERVICE(MetadataPrefix, HandleServiceId)
    ]).


-spec update(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    time:seconds(), time:seconds()) -> ok.
update(MetadataPrefix, HandleServiceId, HandleId, OldTimestamp, NewTimestamp) ->
    delete(MetadataPrefix, HandleServiceId, HandleId, OldTimestamp),
    add(MetadataPrefix, HandleServiceId, HandleId, NewTimestamp).


-spec list(listing_opts()) -> [od_handle:id()] | {[od_handle:id()], resumption_token()}.
list(ListingOpts) ->
    ServiceId = maps:get(service_id, ListingOpts, undefined),
    MetadataPrefix = maps:get(metadata_prefix, ListingOpts),
    TreeId = case {ServiceId, MetadataPrefix} of
        {undefined, _} -> ?TREE_FOR_METADATA_PREFIX(MetadataPrefix);
        {_, _} -> ?TREE_FOR_METADATA_PREFIX_AND_HSERVICE(MetadataPrefix, ServiceId)
    end,
    Size = maps:get(size, ListingOpts, ?DEFAULT_LIST_LIMIT),
    From = maps:get(from, ListingOpts, undefined),
    Until = maps:get(until, ListingOpts, ?MAX_TIMESTAMP),
    Token = maps:get(resumption_token, ListingOpts, <<>>),
    StartIndex = case Token of
        <<>> -> index(From, first);
        _ -> Token
    end,
    FoldOpts = case Token == <<>> andalso From == undefined of
        true -> #{
            size => Size,
            prev_tree_id => TreeId,
            inclusive => false
        };
        false -> #{
            size => Size,
            prev_link_name => StartIndex,
            prev_tree_id => TreeId,
            inclusive => false
        }
    end,
    FoldFun = fun(#link{name = Index, target = HandleId}, Acc) ->
        {TimeSeconds, _HId} = decode_index(Index),
        Result = case TimeSeconds > Until of
            true -> {stop, Acc};
            false -> {ok, [{TimeSeconds, HandleId} | Acc]}
        end,
        Result
    end,
    {ok, InternalEntries} = datastore_model:fold_links(
        ?CTX, ?FOREST, TreeId, FoldFun, [], FoldOpts
    ),
    case InternalEntries of
        [] -> {InternalEntries, undefined};
        _ -> build_result_from_reversed_listing(InternalEntries, Size)
    end.


-spec get_earliest_timestamp() -> none | time:seconds().
get_earliest_timestamp() ->
    EarliestTimestamps = lists:flatmap(fun(MetadataFormat) ->
        ListingOpts = #{size => 1, metadata_prefix => MetadataFormat},
        {List, undefined} = list(ListingOpts),
        List
    end, metadata_formats:supported_formats()),
    ?warning("EARLIEST TIMESTAMPS ~p~n", [EarliestTimestamps] ),
    case EarliestTimestamps of
        [] -> none;
        _ ->
            Timestamps = lists:map(fun(HandleId) ->
                {ok, #document{value = #od_handle{
                    timestamp = TimeSecondsBin
                }}} = od_handle:get(HandleId),
                TimeSecondsBin
            end, EarliestTimestamps),
            lists:min(Timestamps)
    end.


%% @private
build_result_from_reversed_listing(InternalEntries, Size) ->
    NewToken = pack_resumption_token(InternalEntries, Size),
    ReversedEntries = lists:reverse(InternalEntries),
    Handles = lists:map(fun({_, HandleId}) -> HandleId end, ReversedEntries),
    {Handles, NewToken}.


%% @private
pack_resumption_token(InternalEntries, Size) ->
    case length(InternalEntries) < Size orelse Size < ?MAX_LIST_LIMIT of
        true -> undefined;
        false ->
            {TimeSeconds, HandleId} = hd(InternalEntries),
            index(TimeSeconds, HandleId)
    end.