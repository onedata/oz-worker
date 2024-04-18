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

-export([add/5, delete/5, update_timestamp/5]).
-export([list/1, get_earliest_timestamp/0, get_exists_flag/1]).
-export([purge/4]).

% link_key() consists of 2 parts:
%  1) timestamp (in seconds) - so that links would be sorted by time.
%  2) handle id - this part allows to distinguish links associated with handles
%                that have the same timestamp.
-type link_key() :: binary().

% link value() consists of 2 parts:
%  1) handle_service_id - id of the handle service in which the handle has been registered.
%  2) exists flag - if record exists flag value is 1, otherwise it's 0
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
    until => undefined | od_handle:timestamp_seconds()
}.
%% @formatter:on

-type listing_entry() :: {od_handle:timestamp_seconds(), od_handle_service:id(), od_handle:id()}.

-export_type([listing_opts/0, resumption_token/0, listing_entry/0, exists_flag/0]).

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


-spec delete(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds(), od_handle:timestamp_seconds()) -> ok.
delete(MetadataPrefix, HandleServiceId, HandleId, OldTimestamp, DeletionTimestamp) ->
    purge(MetadataPrefix, HandleServiceId, HandleId, OldTimestamp),
    add(MetadataPrefix, HandleServiceId, HandleId, DeletionTimestamp, false).


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
            purge(MetadataPrefix, HandleServiceId, HandleId, OldTimestamp),
            add(MetadataPrefix, HandleServiceId, HandleId, NewTimestamp, true)
    end.


-spec list(listing_opts()) -> {[listing_entry()], resumption_token()}.
list(ListingOpts) ->
    Token = maps:get(resumption_token, ListingOpts, <<>>),

    {MetadataPrefix, HServiceId, Limit, From, Until, StartIndex} = case Token of
        <<>> ->
            MetadataPrefix1 = maps:get(metadata_prefix, ListingOpts),
            HServiceId1 = maps:get(service_id, ListingOpts, undefined),
            Limit1 = maps:get(limit, ListingOpts, ?DEFAULT_LIST_LIMIT),
            From1 = maps:get(from, ListingOpts, undefined),
            Until1 = maps:get(until, ListingOpts, ?MAX_TIMESTAMP),
            StartIndex1 = encode_link_key(From1, first),
            {MetadataPrefix1, HServiceId1, Limit1, From1, Until1, StartIndex1};
        _ ->
            {TimeSecondsToken, MetadataPrefix2, HandleID, HServiceId2, Limit2,
                From2, Until2} = unpack_resumption_token(Token),
            StartIndex2 = encode_link_key(TimeSecondsToken, HandleID),

            {MetadataPrefix2, HServiceId2, Limit2, From2, Until2, StartIndex2}
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
        {HandleServiceId, ExistsFlag} = decode_link_value(Value),
        Result = case TimeSeconds > Until of
            true -> {stop, Acc};
            false -> {ok, [{TimeSeconds, HandleServiceId, HandleId, ExistsFlag} | Acc]}
        end,
        Result
    end,
    {ok, ReversedEntries} = datastore_model:fold_links(
        ?CTX, ?FOREST, TreeId, FoldFun, [], FoldOpts
    ),
    build_result_from_reversed_listing(ReversedEntries, Limit, MetadataPrefix, From, Until, HServiceId).


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
            lists:min([Timestamp || {Timestamp, _, _, _} <- EntriesWithEarliestTimestamps])
    end.


get_exists_flag(HandleId) ->
    #od_handle{
        handle_service = HandleService,
        timestamp = TimeStamp,
        metadata_prefix = MetadataPrefix
    } = oai_utils:get_handle(HandleId),

    {List, _Token} = handles:list(#{
        metadata_prefix => MetadataPrefix,
        handle_service_id => HandleService,
        from => TimeStamp,
        until => TimeStamp
    }),
    [ExistsFlag] = [
        ExsFlag || {_TimeStamp, _HService, HId, ExsFlag} <- List,
        HId =:= HandleId
    ],
    ExistsFlag.


-spec purge(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds()) -> ok.
purge(MetadataPrefix, HandleServiceId, HandleId, Timestamp) ->
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
-spec encode_link_value(od_handle_service:id(), exists_flag()) -> link_value().
encode_link_value(HandleServiceId, ExistsFlag) ->
    str_utils:join_binary([atom_to_binary(ExistsFlag), HandleServiceId], ?VALUE_SEP).


%% @private
-spec decode_link_key(link_key()) -> {od_handle:timestamp_seconds(), od_handle:id()}.
decode_link_key(Key) ->
    <<TimeSeconds:11/binary, ?KEY_SEP, HandleId/binary>> = Key,
    {binary_to_integer(TimeSeconds), HandleId}.


%% @private
-spec decode_link_value(link_value()) -> {od_handle_service:id(), exists_flag()}.
decode_link_value(Value) ->
    [ExistsFlag, HandleServiceId] = binary:split(Value, [?VALUE_SEP], [global]),
    {HandleServiceId, binary_to_atom(ExistsFlag)}.

%% @private
-spec pack_resumption_token(od_handle:metadata_prefix(), limit(), od_handle:timestamp_seconds(),
    od_handle:timestamp_seconds(), od_handle_service:id(), listing_entry()) -> resumption_token().
pack_resumption_token(MetadataPrefix, Limit, From, Until, HServiceId, LastListedEntry) ->
    FormattedLimit = integer_to_binary(utils:ensure_defined(Limit, ?DEFAULT_LIST_LIMIT)),
    FormattedFrom = integer_to_binary(utils:ensure_defined(From, 0)),
    FormattedUntil = integer_to_binary(utils:ensure_defined(Until, ?MAX_TIMESTAMP)),
    HServiceIdBin = utils:ensure_defined(HServiceId, <<>>),
    %% If HServiceId in ListingOpts was <<>>, then in token must also be
    %% <<>> in order to list from the correct tree.
    {TimeSeconds, _HServiceId, HandleId, _ExistsFlag} = LastListedEntry,
    str_utils:join_binary([integer_to_binary(TimeSeconds), MetadataPrefix,
        FormattedLimit, FormattedFrom, FormattedUntil, HandleId, HServiceIdBin], ?RESUMPTION_TOKEN_SEP).


%% @private
-spec unpack_resumption_token(resumption_token()) -> {od_handle:timestamp_seconds(), od_handle:metadata_prefix(),
    od_handle:id(), od_handle_service:id(), limit(), od_handle:timestamp_seconds(), od_handle:timestamp_seconds()}.
unpack_resumption_token(Token) ->
    [TimeSeconds, MetadataPrefix, Limit, From, Until, HandleId, HServiceIdBin] = binary:split(Token,
        [?RESUMPTION_TOKEN_SEP], [global]),
    HandleServiceId = case HServiceIdBin of <<>> -> undefined; _ -> HServiceIdBin end,
    {binary_to_integer(TimeSeconds), MetadataPrefix, HandleId, HandleServiceId, binary_to_integer(Limit),
        binary_to_integer(From), binary_to_integer(Until)}.


%% @private
-spec build_result_from_reversed_listing(datastore:fold_acc(), limit(), od_handle:metadata_prefix(),
    od_handle:timestamp_seconds(), od_handle:timestamp_seconds(), od_handle_service:id()) ->
    {[listing_entry()], resumption_token()}.
build_result_from_reversed_listing(ReversedEntries, Limit, MetadataPrefix, From, Until, HServiceId) ->
    % the internal listing limit is always one element greater than the requested limit,
    % which allows determining if this is the last batch of the complete list
    {ReversedLimitedEntries, NewToken} = case length(ReversedEntries) =:= Limit + 1 of
        false ->
            {ReversedEntries, undefined};
        true ->
            ReversedEntriesTail = tl(ReversedEntries),
            LastListedEntry = hd(ReversedEntriesTail),
            {ReversedEntriesTail, pack_resumption_token(MetadataPrefix, Limit,
                From, Until, HServiceId, LastListedEntry)}
    end,
    LimitedEntries = lists:reverse(ReversedLimitedEntries),
    {LimitedEntries, NewToken}.
