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

-export([report_created/4, delete/5, update_timestamp/5]).
-export([list/1, get_earliest_timestamp/0, get_exists_flag/1]).
-export([purge_deleted_entry/4, purge_deleted_entry_all/0]).

% link_key() consists of 2 parts:
%  1) timestamp (in seconds) - so that links would be sorted by time.
%  2) handle id - this part allows to distinguish links associated with handles
%                that have the same timestamp.
-type link_key() :: binary().

% link value() encodes 2 pieces of information:
%  1) handle_service_id - id of the handle service in which the handle has been registered.
%  2) exists flag - contains information whether a previously existing handle has been deleted.
-type link_value() :: binary().

% the resumption token is used to continue listing when an incomplete list (batch) is returned;
% an 'undefined' value is returned when there are no more entries to list
-type resumption_token() :: binary() | undefined.

-type limit() :: pos_integer().

-type exists_flag() :: boolean().

%% @formatter:off
-type listing_opts() :: #{
    metadata_prefix => od_handle:metadata_prefix(),   % required unless resumption_token is provided
    resumption_token => resumption_token(),   % exclusive argument; if present, all other argument must not be provided
    limit => limit(),
    service_id => od_handle_service:id(),
    from => undefined | od_handle:timestamp_seconds(),
    until => undefined | od_handle:timestamp_seconds(),
    include_deleted => false | boolean()
}.
%% @formatter:on

-export_type([listing_opts/0, resumption_token/0, exists_flag/0]).

-define(CTX, (od_handle:get_ctx())).

-define(FOREST, <<"handle-forest">>).
-define(TREE_FOR_METADATA_PREFIX(Prefix),
    <<"handle-metadata", Prefix/binary, "-all-tree">>).
-define(TREE_FOR_METADATA_PREFIX_AND_HSERVICE(Prefix, HServiceId),
    <<"tree-for-", Prefix/binary, "-of-service-", HServiceId/binary>>).


-define(DEFAULT_LIST_LIMIT, oz_worker:get_env(default_handle_list_limit, 1000)).
-define(MAX_LIST_LIMIT, 1000).

% uses null for separator to ensure alphabetical sorting
-define(KEY_SEP, 0).
-define(VALUE_SEP, <<":">>).
-define(RESUMPTION_TOKEN_SEP, <<",">>).

-define(MAX_TIMESTAMP, 99999999999).


%%%===================================================================
%%% API
%%%===================================================================

-spec report_created(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds()) -> ok.
report_created(MetadataPrefix, HandleServiceId, HandleId, TimeSeconds) ->
    add(MetadataPrefix, HandleServiceId, HandleId, TimeSeconds, true).


-spec delete(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds(), od_handle:timestamp_seconds()) -> ok.
delete(MetadataPrefix, HandleServiceId, HandleId, OldTimestamp, DeletionTimestamp) ->
    purge_deleted_entry(MetadataPrefix, HandleServiceId, HandleId, OldTimestamp),
    report_deleted(MetadataPrefix, HandleServiceId, HandleId, DeletionTimestamp).


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
            purge_deleted_entry(MetadataPrefix, HandleServiceId, HandleId, OldTimestamp),
            report_created(MetadataPrefix, HandleServiceId, HandleId, NewTimestamp)
    end.


-spec list(listing_opts()) -> {[handle_listing_entry()], resumption_token()}.
list(ListingOpts) ->
    Token = maps:get(resumption_token, ListingOpts, <<>>),

    {MetadataPrefix, HServiceId, Limit, From, Until, StartIndex, IncludeDeleted} = case Token of
        <<>> ->
            MetadataPrefix1 = maps:get(metadata_prefix, ListingOpts),
            HServiceId1 = maps:get(service_id, ListingOpts, undefined),
            Limit1 = maps:get(limit, ListingOpts, ?DEFAULT_LIST_LIMIT),
            From1 = maps:get(from, ListingOpts, undefined),
            Until1 = maps:get(until, ListingOpts, ?MAX_TIMESTAMP),
            StartIndex1 = encode_link_key(From1, first),
            IncludeDeleted1 = maps:get(include_deleted, ListingOpts, false),
            {MetadataPrefix1, HServiceId1, Limit1, From1, Until1, StartIndex1, IncludeDeleted1};
        _ ->
            {TimeSecondsToken, MetadataPrefix2, HandleID, HServiceId2, Limit2,
                From2, Until2, IncludeDeleted2} = unpack_resumption_token(Token),
            StartIndex2 = encode_link_key(TimeSecondsToken, HandleID),

            {MetadataPrefix2, HServiceId2, Limit2, From2, Until2, StartIndex2, IncludeDeleted2}
    end,

    TreeId = case HServiceId of
        undefined -> ?TREE_FOR_METADATA_PREFIX(MetadataPrefix);
        _ -> ?TREE_FOR_METADATA_PREFIX_AND_HSERVICE(MetadataPrefix, HServiceId)
    end,
    FoldOpts =  #{
        %% Limit + 1 is used to determine if it's the end of the list
        %% and if a resumption token needs to be included
        size => Limit + 1,
        prev_link_name => StartIndex,
        prev_tree_id => TreeId,
        inclusive => false
    },
    FoldFun = fun(#link{name = Key, target = Value}, Acc) ->
        {TimeSeconds, HandleId} = decode_link_key(Key),
        Result = case TimeSeconds > Until of
            true ->
                {stop, Acc};
            false ->
                {HandleServiceId, ExistsFlag} = decode_link_value(Value),
                case {IncludeDeleted, ExistsFlag} of
                    {false, false} ->
                        {ok, Acc};
                    _ ->
                        {ok, [#handle_listing_entry{
                            timestamp = TimeSeconds,
                            service_id = HandleServiceId,
                            exists_flag = ExistsFlag,
                            handle_id = HandleId
                        } | Acc]}
                end
        end,
        Result
    end,
    {ok, ReversedEntries} = datastore_model:fold_links(
        ?CTX, ?FOREST, TreeId, FoldFun, [], FoldOpts
    ),
    build_result_from_reversed_listing(ReversedEntries, Limit, MetadataPrefix, From, Until, HServiceId, IncludeDeleted).


-spec get_earliest_timestamp() -> undefined | od_handle:timestamp_seconds().
get_earliest_timestamp() ->
    EntriesWithEarliestTimestamps = lists:flatmap(fun(MetadataPrefix) ->
        ListingOpts = #{limit => 1, metadata_prefix => MetadataPrefix},
        {List, _} = list(ListingOpts),
        List
    end, oai_metadata:supported_formats()),

    case EntriesWithEarliestTimestamps of
        [] ->
            undefined;
        _ ->
            lists:min([Timestamp || #handle_listing_entry{timestamp = Timestamp} <- EntriesWithEarliestTimestamps])
    end.


get_exists_flag(HandleId) ->
    #od_handle{
        handle_service = HandleService,
        timestamp = TimeStamp,
        metadata_prefix = MetadataPrefix
    } = oai_utils:get_handle(HandleId),

    {List, _Token} = list(#{
        metadata_prefix => MetadataPrefix,
        handle_service_id => HandleService,
        from => TimeStamp,
        until => TimeStamp,
        include_deleted => true
    }),
    [ExistsFlag] = [
        ExsFlag || #handle_listing_entry{handle_id = HId, exists_flag = ExsFlag} <- List,
        HId =:= HandleId
    ],
    ExistsFlag.



-spec purge_deleted_entry(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds()) -> ok.
purge_deleted_entry(MetadataPrefix, HandleServiceId, HandleId, Timestamp) ->
    Key = encode_link_key(Timestamp, HandleId),
    lists:foreach(fun(TreeId) ->
        case datastore_model:delete_links(?CTX, ?FOREST, TreeId, Key) of
            ok -> ok;
            {error, not_found} -> ok
        end
    end, [
        ?TREE_FOR_METADATA_PREFIX(MetadataPrefix),
        ?TREE_FOR_METADATA_PREFIX_AND_HSERVICE(MetadataPrefix, HandleServiceId)
    ]).


-spec purge_deleted_entry_all() -> ok.
purge_deleted_entry_all() ->
    lists:foreach(fun(MetadataPrefix) ->
        All = list(#{metadata_prefix => MetadataPrefix, include_deleted => true}),
        lists:foreach(fun(#handle_listing_entry{
            timestamp = Timestamp,
            service_id = HandleServiceId,
            handle_id = HandleId
        }) ->
            purge_deleted_entry(MetadataPrefix, HandleServiceId, HandleId, Timestamp)
        end, All)
    end, ozt_handles:supported_metadata_prefixes()).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec encode_link_key(undefined | od_handle:timestamp_seconds(), first | od_handle:id()) -> link_key().
encode_link_key(From, first) ->
    case From of
        undefined -> <<>>;
        _ -> str_utils:format_bin("~11..0B", [From])
    end;
encode_link_key(TimeSeconds, HandleId) ->
    FormattedTimeSeconds = str_utils:format_bin("~11..0B", [TimeSeconds]),
    <<(FormattedTimeSeconds)/binary, ?KEY_SEP, HandleId/binary>>.


%% @private
-spec decode_link_key(link_key()) -> {od_handle:timestamp_seconds(), od_handle:id()}.
decode_link_key(Key) ->
    <<TimeSeconds:11/binary, ?KEY_SEP, HandleId/binary>> = Key,
    {binary_to_integer(TimeSeconds), HandleId}.


%% @private
-spec encode_link_value(od_handle_service:id(), exists_flag()) -> link_value().
encode_link_value(HandleServiceId, ExistsFlag) ->
    str_utils:join_binary([exists_flag_to_binary(ExistsFlag), HandleServiceId], ?VALUE_SEP).


%% @private
-spec decode_link_value(link_value()) -> {od_handle_service:id(), exists_flag()}.
decode_link_value(Value) ->
    [ExistsFlag, HandleServiceId] = binary:split(Value, [?VALUE_SEP], [global]),
    {HandleServiceId, binary_to_exists_flag(ExistsFlag)}.


%% @private
exists_flag_to_binary(true) -> <<"1">>;
exists_flag_to_binary(false) -> <<"0">>.

%% @private
binary_to_exists_flag(<<"1">>) -> true;
binary_to_exists_flag(<<"0">>) -> false.


%% @private
-spec pack_resumption_token(od_handle:metadata_prefix(), limit(), od_handle:timestamp_seconds(),
    od_handle:timestamp_seconds(), od_handle_service:id(), boolean(), handle_listing_entry()) -> resumption_token().
pack_resumption_token(MetadataPrefix, Limit, From, Until, HServiceId, IncludeDeleted, LastListedEntry) ->
    FormattedLimit = integer_to_binary(utils:ensure_defined(Limit, ?DEFAULT_LIST_LIMIT)),
    FormattedFrom = integer_to_binary(utils:ensure_defined(From, 0)),
    FormattedUntil = integer_to_binary(utils:ensure_defined(Until, ?MAX_TIMESTAMP)),
    HServiceIdBin = utils:ensure_defined(HServiceId, <<>>),
    %% If HServiceId in ListingOpts was <<>>, then in token must also be
    %% <<>> in order to list from the correct tree.
    str_utils:join_binary([
        integer_to_binary(LastListedEntry#handle_listing_entry.timestamp),
        MetadataPrefix, FormattedLimit, FormattedFrom, FormattedUntil,
        LastListedEntry#handle_listing_entry.handle_id, HServiceIdBin,
        exists_flag_to_binary(IncludeDeleted)
    ], ?RESUMPTION_TOKEN_SEP).


%% @private
-spec unpack_resumption_token(resumption_token()) -> {od_handle:timestamp_seconds(), od_handle:metadata_prefix(),
    od_handle:id(), od_handle_service:id(), limit(), od_handle:timestamp_seconds(), od_handle:timestamp_seconds(), boolean()}.
unpack_resumption_token(Token) ->
    [TimeSeconds, MetadataPrefix, Limit, From, Until, HandleId, HServiceIdBin, IncludeDeletedBin] = binary:split(Token,
        [?RESUMPTION_TOKEN_SEP], [global]),
    HandleServiceId = case HServiceIdBin of <<>> -> undefined; _ -> HServiceIdBin end,

    {binary_to_integer(TimeSeconds), MetadataPrefix, HandleId, HandleServiceId, binary_to_integer(Limit),
        binary_to_integer(From), binary_to_integer(Until), binary_to_exists_flag(IncludeDeletedBin)}.


%% @private
-spec build_result_from_reversed_listing(datastore:fold_acc(), limit(), od_handle:metadata_prefix(),
    od_handle:timestamp_seconds(), od_handle:timestamp_seconds(), od_handle_service:id(), boolean()) ->
    {[handle_listing_entry()], resumption_token()}.
build_result_from_reversed_listing(ReversedEntries, Limit, MetadataPrefix, From, Until, HServiceId, IncludeDeleted) ->
    % the internal listing limit is always one element greater than the requested limit,
    % which allows determining if this is the last batch of the complete list
    {ReversedLimitedEntries, NewToken} = case length(ReversedEntries) =:= Limit + 1 of
        false ->
            {ReversedEntries, undefined};
        true ->
            ReversedEntriesTail = tl(ReversedEntries),
            LastListedEntry = hd(ReversedEntriesTail),
            {ReversedEntriesTail, pack_resumption_token(MetadataPrefix, Limit,
                From, Until, HServiceId, IncludeDeleted, LastListedEntry)}
    end,
    LimitedEntries = lists:reverse(ReversedLimitedEntries),
    {LimitedEntries, NewToken}.


%% @private
-spec report_deleted(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds()) -> ok.
report_deleted(MetadataPrefix, HandleServiceId, HandleId, TimeSeconds) ->
    add(MetadataPrefix, HandleServiceId, HandleId, TimeSeconds, false).


%% @private
-spec add(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds(), exists_flag()) -> ok.
add(MetadataPrefix, HandleServiceId, HandleId, TimeSeconds, ExistsFlag) ->
    Link = {encode_link_key(TimeSeconds, HandleId), encode_link_value(HandleServiceId, ExistsFlag)},

    lists:foreach(fun(TreeId) ->
        case datastore_model:add_links(?CTX, ?FOREST, TreeId, Link) of
            {ok, _} -> ok;
            {error, already_exists} -> throw(?ERROR_ALREADY_EXISTS)
        end
    end, [
        ?TREE_FOR_METADATA_PREFIX(MetadataPrefix),
        ?TREE_FOR_METADATA_PREFIX_AND_HSERVICE(MetadataPrefix, HandleServiceId)
    ]).
