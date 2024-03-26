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
-include("http/handlers/oai.hrl").
-include_lib("ctool/include/logging.hrl").

-export([add/4, delete/4, update_timestamp/5]).
-export([list/1, get_earliest_timestamp/0]).

% index() consists of 2 parts:
%  1) timestamp (in seconds) - so that links would be sorted by time.
%  2) handle id - this part allows to distinguish links associated with handles
%                that have the same timestamp.
-type index() :: binary().

-type resumption_token() :: binary() | undefined.

-type limit() :: pos_integer().

%% @formatter:off
-type listing_opts() :: #{
    metadata_prefix => od_handle:metadata_prefix(),   % required unless resumption_token is provided
    resumption_token => resumption_token(),   % exclusive argument; if present, all other argument must not be provided
    limit => limit(),
    service_id => od_handle_service:id(),
    from => undefined | od_handle:timestamp_seconds(),
    until => undefined | od_handle:timestamp_seconds()
}.
%% @formatter:on

-export_type([listing_opts/0, resumption_token/0]).

-define(CTX, (od_handle:get_ctx())).

-define(FOREST, <<"handle-forest">>).
-define(TREE_FOR_METADATA_PREFIX(Prefix),
    <<"handle-metadata", Prefix/binary, "-all-tree">>).
-define(TREE_FOR_METADATA_PREFIX_AND_HSERVICE(Prefix, HServiceId),
    <<"tree-for-", Prefix/binary, "-of-service-", HServiceId/binary>>).

% Uses null for separator to ensure alphabetical sorting
-define(INDEX_SEP, 0).
-define(TOKEN_SEP, <<",">>).
-define(MAX_LIST_LIMIT, 1000).
-define(MAX_TIMESTAMP, 99999999999).


%%%===================================================================
%%% API
%%%===================================================================


-spec add(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds()) -> ok.
add(MetadataPrefix, HandleServiceId, HandleId, TimeSeconds) ->
    Link = {encode_index(TimeSeconds, HandleId), HandleId},

    lists:foreach(fun(TreeId) ->
        case datastore_model:add_links(?CTX, ?FOREST, TreeId, Link) of
            {ok, _} -> ok;
            {error, already_exists} -> throw(?ERROR_ALREADY_EXISTS)
        end
    end, [
        ?TREE_FOR_METADATA_PREFIX(MetadataPrefix),
        ?TREE_FOR_METADATA_PREFIX_AND_HSERVICE(MetadataPrefix, HandleServiceId)
    ]).


-spec delete(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds()) -> ok.
delete(MetadataPrefix, HandleServiceId, HandleId, TimeSeconds) ->
    Index = encode_index(TimeSeconds, HandleId),
    lists:foreach(fun(TreeId) ->
        case datastore_model:delete_links(?CTX, ?FOREST, TreeId, Index) of
            ok -> ok;
            {error, not_found} -> ok
        end
    end, [
        ?TREE_FOR_METADATA_PREFIX(MetadataPrefix),
        ?TREE_FOR_METADATA_PREFIX_AND_HSERVICE(MetadataPrefix, HandleServiceId)
    ]).


%%--------------------------------------------------------------------
%% @doc NOTE: cannot be run in parallel!
%% @end
%%--------------------------------------------------------------------
-spec update_timestamp(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds(), od_handle:timestamp_seconds()) -> ok.
update_timestamp(MetadataPrefix, HandleServiceId, HandleId, OldTimestamp, NewTimestamp) ->
    case OldTimestamp == NewTimestamp of
        true -> ok;
        false ->
            delete(MetadataPrefix, HandleServiceId, HandleId, OldTimestamp),
            add(MetadataPrefix, HandleServiceId, HandleId, NewTimestamp)
    end.


-spec list(listing_opts()) -> [od_handle:id()] | {[od_handle:id()], resumption_token()}.
list(ListingOpts) ->
    HServiceId = maps:get(service_id, ListingOpts, undefined),
    Token = maps:get(resumption_token, ListingOpts, <<>>),

    {MetadataPrefix, Limit, From, Until, StartIndex} = case Token of
        <<>> ->
            MetadataPrefix1 = maps:get(metadata_prefix, ListingOpts),
            Limit1 = maps:get(limit, ListingOpts, ?DEFAULT_LIST_LIMIT),
            From1 = maps:get(from, ListingOpts, undefined),
            Until1 = maps:get(until, ListingOpts, ?MAX_TIMESTAMP),
            StartIndex1 = encode_index(From1, first),
            {MetadataPrefix1, Limit1, From1, Until1, StartIndex1};
        _ ->
            {TimeSecondsToken, MetadataPrefix2, HandleID, Limit2, From2, Until2} = unpack_resumption_token(Token),
            StartIndex2 = encode_index(TimeSecondsToken, HandleID),
            {MetadataPrefix2, Limit2, From2, Until2, StartIndex2}
    end,

    TreeId = case HServiceId of
        undefined -> ?TREE_FOR_METADATA_PREFIX(MetadataPrefix);
        _ -> ?TREE_FOR_METADATA_PREFIX_AND_HSERVICE(MetadataPrefix, HServiceId)
    end,
    FoldOpts =  #{
        size => Limit + 1,
        prev_link_name => StartIndex,
        prev_tree_id => TreeId,
        inclusive => false
    },
    FoldFun = fun(#link{name = Index, target = HandleId}, Acc) ->
        {TimeSeconds, _HId} = decode_index(Index),
        Result = case TimeSeconds > Until of
            true -> {stop, Acc};
            false -> {ok, [{TimeSeconds, HandleId} | Acc]}
        end,
        Result
    end,
    {ok, ReversedEntries} = datastore_model:fold_links(
        ?CTX, ?FOREST, TreeId, FoldFun, [], FoldOpts
    ),
    {Handles, NewToken} = build_result_from_reversed_listing(ReversedEntries, Limit,
        MetadataPrefix, From, Until),

    %% if the entire list is returned at once, the resumptionToken is not included in the response
    case NewToken == undefined andalso Token == <<>> of
        true -> Handles;
        false -> {Handles, NewToken}
    end.


-spec get_earliest_timestamp() -> undefined | od_handle:timestamp_seconds().
get_earliest_timestamp() ->
    EarliestTimestamps = lists:flatmap(fun(MetadataPrefix) ->
        ListingOpts = #{limit => 1, metadata_prefix => MetadataPrefix},
        case list(ListingOpts) of
            {List, _} -> List;
            List -> List
        end
    end, metadata_formats:supported_formats()),
    case EarliestTimestamps of
        [] -> undefined;
        _ ->
            Timestamps = lists:map(fun(HandleId) ->
                {ok, #document{value = #od_handle{
                    timestamp = TimeSecondsBin
                }}} = od_handle:get(HandleId),
                TimeSecondsBin
            end, EarliestTimestamps),
            lists:min(Timestamps)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec encode_index(undefined | od_handle:timestamp_seconds(), od_handle:id() | first) -> index().
encode_index(From, first) ->
    case From of
        undefined -> <<>>;
        _ -> str_utils:format_bin("~11..0B", [From])
    end;
encode_index(TimeSeconds, HandleId) ->
    FormattedTimeSeconds = str_utils:format_bin("~11..0B", [TimeSeconds]),
    <<(FormattedTimeSeconds)/binary, ?INDEX_SEP, HandleId/binary>>.


%% @private
-spec decode_index(index()) -> {od_handle:timestamp_seconds(), od_handle:id()}.
decode_index(Index) ->
    <<TimeSeconds:11/binary, 0, HandleId/binary>> = Index,
    {binary_to_integer(TimeSeconds), HandleId}.


%% @private
-spec pack_resumption_token(od_handle:timestamp_seconds(), od_handle:id(),
    od_handle:metadata_prefix(), limit(), od_handle:timestamp_seconds(),
    od_handle:timestamp_seconds()) -> resumption_token().
pack_resumption_token(TimeSeconds, HandleId, MetadataPrefix, Limit, From, Until) ->
    FormattedLimit = integer_to_binary(utils:ensure_defined(Limit, ?DEFAULT_LIST_LIMIT)),
    FormattedFrom = integer_to_binary(utils:ensure_defined(From, 0)),
    FormattedUntil = integer_to_binary(utils:ensure_defined(Until, ?MAX_TIMESTAMP)),
    str_utils:join_binary([integer_to_binary(TimeSeconds), MetadataPrefix,
        FormattedLimit, FormattedFrom, FormattedUntil, HandleId], ?TOKEN_SEP).


%% @private
-spec unpack_resumption_token(resumption_token()) -> {od_handle:timestamp_seconds(), od_handle:metadata_prefix(),
    od_handle:id(), limit(), od_handle:timestamp_seconds(), od_handle:timestamp_seconds()}.
unpack_resumption_token(Token) ->
    [TimeSeconds, MetadataPrefix, Limit, From, Until, HandleId] = binary:split(Token, [?TOKEN_SEP], [global]),
    {binary_to_integer(TimeSeconds), MetadataPrefix, HandleId, binary_to_integer(Limit),
        binary_to_integer(From), binary_to_integer(Until)}.


%% @private
-spec build_result_from_reversed_listing(datastore:fold_acc(), limit(), od_handle:metadata_prefix(),
    od_handle:timestamp_seconds(), od_handle:timestamp_seconds()) -> {[od_handle:id()], resumption_token()}.
build_result_from_reversed_listing(ReversedEntries, Limit, MetadataPrefix, From, Until) ->
    {NewToken, ReversedLimitedEntries} = case length(ReversedEntries) =< Limit of
        true ->
            {undefined, ReversedEntries};
        false ->
            ReversedLimitedEnt = tl(ReversedEntries),
            {TimeSeconds, HandleId} = hd(ReversedLimitedEnt),
            {pack_resumption_token(TimeSeconds, HandleId, MetadataPrefix, Limit, From, Until),
                ReversedLimitedEnt}
    end,
    LimitedEntries = lists:reverse(ReversedLimitedEntries),
    Handles = lists:map(fun({_, HandleId}) -> HandleId end, LimitedEntries),
    {Handles, NewToken}.
