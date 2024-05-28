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
-module(handle_registry).
-author("Katarzyna Such").

-include("datastore/oz_datastore_models.hrl").
-include("http/handlers/oai.hrl").
-include_lib("ctool/include/logging.hrl").

-export([report_created/4, report_updated/5, report_deleted/5]).
-export([lookup_deleted/1, purge_all_deleted_entries/0]).
-export([get_earliest_timestamp/0]).
-export([gather_by_all_prefixes/0, gather_by_all_prefixes/1, list_completely/1, list_portion/1]).

% link_key() consists of 2 parts:
%  1) timestamp (in seconds) - so that links would be sorted by time.
%  2) handle id - this part allows to distinguish links associated with handles
%                that have the same timestamp.
-type link_key() :: binary().

% link value() encodes 2 pieces of information:
%  1) handle service id - id of the handle service in which the handle has been registered.
%  2) exists flag - contains information whether a previously existing handle has been deleted.
-type link_value() :: binary().

% the resumption token is used to continue listing when an incomplete list (batch) is returned;
% an 'undefined' value is returned when there are no more entries to list
-type resumption_token() :: binary() | undefined.

-type limit() :: pos_integer().

% deleted status indicates that the handle had existed in the past, but was deleted;
% such handles can be listed using the 'include_deleted' flag
-type status() :: present | deleted.

%% @formatter:off
-type listing_opts() :: #{
    metadata_prefix => od_handle:metadata_prefix(),   % required unless resumption_token is provided
    resumption_token => resumption_token(),   % exclusive argument; if present, all other argument must not be provided
    limit => limit(),
    service_id => od_handle_service:id(),
    from => undefined | od_handle:timestamp_seconds(),
    until => undefined | od_handle:timestamp_seconds(),
    include_deleted => boolean()
}.
%% @formatter:on

-type handle_listing_entry() :: #handle_listing_entry{}.

-export_type([listing_opts/0, resumption_token/0, status/0, handle_listing_entry/0]).

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

-define(critical_section_for_handle(HandleId, Fun),
    critical_section:run({?MODULE, HandleId}, Fun)).

%%%===================================================================
%%% API
%%%===================================================================

-spec report_created(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds()) -> ok.
report_created(MetadataPrefix, HandleServiceId, HandleId, Timestamp) ->
    add_entry(MetadataPrefix, HandleServiceId, HandleId, Timestamp, present).


%%--------------------------------------------------------------------
%% @doc NOTE: cannot be run in parallel!
%% @end
%%--------------------------------------------------------------------
-spec report_updated(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds(), od_handle:timestamp_seconds()) -> ok.
report_updated(MetadataPrefix, HandleServiceId, HandleId, PreviousTimestamp, UpdateTimestamp) ->
    case PreviousTimestamp == UpdateTimestamp of
        true ->
            ok;
        false ->
            ?critical_section_for_handle(HandleId, fun() ->
                delete_entry(MetadataPrefix, HandleServiceId, HandleId, PreviousTimestamp),
                add_entry(MetadataPrefix, HandleServiceId, HandleId, UpdateTimestamp, present)
            end)
    end.


-spec report_deleted(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds(), od_handle:timestamp_seconds()) -> ok.
report_deleted(MetadataPrefix, HandleServiceId, HandleId, PreviousTimestamp, DeletionTimestamp) ->
    ?critical_section_for_handle(HandleId, fun() ->
        delete_entry(MetadataPrefix, HandleServiceId, HandleId, PreviousTimestamp),
        add_entry(MetadataPrefix, HandleServiceId, HandleId, DeletionTimestamp, deleted),
        deleted_handle_registry:insert(MetadataPrefix, HandleServiceId, HandleId, DeletionTimestamp)
    end).


-spec lookup_deleted(od_handle:id()) -> error | {ok, od_handle:metadata_prefix(), handle_listing_entry()}.
lookup_deleted(HandleId) ->
    deleted_handle_registry:lookup(HandleId).


-spec purge_all_deleted_entries() -> ok.
purge_all_deleted_entries() ->
    ForeachFun = fun({MetadataPrefix, #handle_listing_entry{
        timestamp = Timestamp,
        handle_id = HandleId,
        service_id = HandleServiceId,
        status = deleted
    }}) ->
        % TODO VFS-11906 nested critical section - sort it out
        ?critical_section_for_handle(HandleId, fun() ->
            deleted_handle_registry:remove(HandleId),
            delete_entry(MetadataPrefix, HandleServiceId, HandleId, Timestamp)
        end)
    end,
    deleted_handle_registry:foreach(ForeachFun).


-spec get_earliest_timestamp() -> undefined | od_handle:timestamp_seconds().
get_earliest_timestamp() ->
    EntriesWithEarliestTimestamps = lists:flatmap(fun(MetadataPrefix) ->
        ListingOpts = #{limit => 1, metadata_prefix => MetadataPrefix},
        {List, _} = list_portion(ListingOpts),
        List
    end, oai_metadata:supported_formats()),

    case EntriesWithEarliestTimestamps of
        [] -> undefined;
        _ -> lists:min([E#handle_listing_entry.timestamp || E <- EntriesWithEarliestTimestamps])
    end.


-spec gather_by_all_prefixes() -> [handle_listing_entry()].
gather_by_all_prefixes() ->
    gather_by_all_prefixes(#{}).

-spec gather_by_all_prefixes(listing_opts()) -> [handle_listing_entry()].
gather_by_all_prefixes(ListingOpts) ->
    lists:umerge(lists:map(fun(MetadataPrefix) ->
        list_completely(ListingOpts#{metadata_prefix => MetadataPrefix})
    end, oai_metadata:supported_formats())).


-spec list_completely(listing_opts()) -> [handle_listing_entry()].
list_completely(ListingOpts) ->
    case list_portion(ListingOpts) of
        {List, undefined} -> List;
        {List, ResumptionToken} -> List ++ list_completely(#{resumption_token => ResumptionToken})
    end.


-spec list_portion(listing_opts()) -> {[handle_listing_entry()], resumption_token()}.
list_portion(ListingOpts) ->
    Token = maps:get(resumption_token, ListingOpts, <<>>),

    % TODO VFS-11906 can this be simplified?
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
            {TimestampToken, MetadataPrefix2, HandleID, HServiceId2, Limit2,
                From2, Until2, IncludeDeleted2} = unpack_resumption_token(Token),
            StartIndex2 = encode_link_key(TimestampToken, HandleID),

            {MetadataPrefix2, HServiceId2, Limit2, From2, Until2, StartIndex2, IncludeDeleted2}
    end,

    TreeId = case HServiceId of
        undefined -> ?TREE_FOR_METADATA_PREFIX(MetadataPrefix);
        _ -> ?TREE_FOR_METADATA_PREFIX_AND_HSERVICE(MetadataPrefix, HServiceId)
    end,
    FoldOpts =  #{
        %% Limit + 1 is used to determine if it's the end of the list
        %% and if a resumption token needs to be included
        prev_link_name => StartIndex,
        %% prev_tree_id option is necessary for inclusive => false to work
        prev_tree_id => TreeId,
        inclusive => false
    },
    FoldFun = fun(#link{name = Key, target = Value}, Acc) ->
        {Timestamp, HandleId} = decode_link_key(Key),
        case Timestamp > Until orelse length(Acc) =:= (Limit + 1) of
            true ->
                {stop, Acc};
            false ->
                {HandleServiceId, Status} = decode_link_value(Value),
                case {IncludeDeleted, Status} of
                    {false, deleted} ->
                        {ok, Acc};
                    _ ->
                        {ok, [#handle_listing_entry{
                            timestamp = Timestamp,
                            handle_id = HandleId,
                            service_id = HandleServiceId,
                            status = Status
                        } | Acc]}
                end
        end
    end,
    {ok, ReversedEntries} = datastore_model:fold_links(
        ?CTX, ?FOREST, TreeId, FoldFun, [], FoldOpts
    ),
    build_result_from_reversed_listing(ReversedEntries, Limit, MetadataPrefix, From, Until, HServiceId, IncludeDeleted).

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
encode_link_key(Timestamp, HandleId) ->
    FormattedTimestamp = str_utils:format_bin("~11..0B", [Timestamp]),
    <<(FormattedTimestamp)/binary, ?KEY_SEP, HandleId/binary>>.


%% @private
-spec decode_link_key(link_key()) -> {od_handle:timestamp_seconds(), od_handle:id()}.
decode_link_key(Key) ->
    <<Timestamp:11/binary, ?KEY_SEP, HandleId/binary>> = Key,
    {binary_to_integer(Timestamp), HandleId}.


%% @private
-spec encode_link_value(od_handle_service:id(), status()) -> link_value().
encode_link_value(HandleServiceId, Status) ->
    str_utils:join_binary([status_to_binary(Status), HandleServiceId], ?VALUE_SEP).


%% @private
-spec decode_link_value(link_value()) -> {od_handle_service:id(), status()}.
decode_link_value(Value) ->
    [Status, HandleServiceId] = binary:split(Value, [?VALUE_SEP], [global]),
    {HandleServiceId, binary_to_status(Status)}.


%% @private
status_to_binary(present) -> <<"1">>;
status_to_binary(deleted) -> <<"0">>.

%% @private
binary_to_status(<<"1">>) -> present;
binary_to_status(<<"0">>) -> deleted.


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
        case is_binary(IncludeDeleted) of false -> atom_to_binary(IncludeDeleted); true -> IncludeDeleted end
    ], ?RESUMPTION_TOKEN_SEP).


%% @private
-spec unpack_resumption_token(resumption_token()) -> {od_handle:timestamp_seconds(), od_handle:metadata_prefix(),
    od_handle:id(), od_handle_service:id(), limit(), od_handle:timestamp_seconds(), od_handle:timestamp_seconds(), boolean()}.
unpack_resumption_token(Token) ->
    [Timestamp, MetadataPrefix, Limit, From, Until, HandleId, HServiceIdBin, IncludeDeletedBin] = binary:split(Token,
        [?RESUMPTION_TOKEN_SEP], [global]),
    HandleServiceId = case HServiceIdBin of <<>> -> undefined; _ -> HServiceIdBin end,

    {binary_to_integer(Timestamp), MetadataPrefix, HandleId, HandleServiceId, binary_to_integer(Limit),
        binary_to_integer(From), binary_to_integer(Until), binary_to_atom(IncludeDeletedBin)}.


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
-spec add_entry(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds(), status()) -> ok.
add_entry(MetadataPrefix, HandleServiceId, HandleId, Timestamp, Status) ->
    Link = {encode_link_key(Timestamp, HandleId), encode_link_value(HandleServiceId, Status)},

    lists:foreach(fun(TreeId) ->
        case datastore_model:add_links(?CTX, ?FOREST, TreeId, Link) of
            {ok, _} -> ok;
            {error, already_exists} -> throw(?ERROR_ALREADY_EXISTS)
        end
    end, [
        ?TREE_FOR_METADATA_PREFIX(MetadataPrefix),
        ?TREE_FOR_METADATA_PREFIX_AND_HSERVICE(MetadataPrefix, HandleServiceId)
    ]).


%% @private
-spec delete_entry(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds()) -> ok.
delete_entry(MetadataPrefix, HandleServiceId, HandleId, Timestamp) ->
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