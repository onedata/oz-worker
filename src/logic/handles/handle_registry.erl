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
-export([list_portion/1, list_completely/1, gather_by_all_prefixes/0, gather_by_all_prefixes/1]).

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
    resumption_token => resumption_token(),   % exclusive argument; if present, all other argument must not be provided
    metadata_prefix => od_handle:metadata_prefix(),   % required unless resumption_token is provided
    service_id => od_handle_service:id(),
    limit => limit(),
    from => undefined | od_handle:timestamp_seconds(),  % inclusive
    until => undefined | od_handle:timestamp_seconds(),  % inclusive
    include_deleted => boolean()
}.
%% @formatter:on

-record(internal_listing_opts, {
    tree_id :: binary(),
    limit :: limit(),
    start_after_key :: link_key(),  % key to start listing from, exclusively
    until_timestamp :: od_handle:timestamp_seconds(),  % timestamp to finish listing at, inclusively
    include_deleted :: boolean()
}).
-type internal_listing_opts() :: #internal_listing_opts{}.

-type handle_listing_entry() :: #handle_listing_entry{}.

-export_type([listing_opts/0, resumption_token/0, status/0, handle_listing_entry/0]).

-define(CTX, (od_handle:get_ctx())).

-define(FOREST, <<"handle-forest">>).
-define(TREE_FOR_METADATA_PREFIX(Prefix),
    <<Prefix/binary, "-records-all">>).
-define(TREE_FOR_METADATA_PREFIX_AND_HSERVICE(Prefix, HServiceId),
    <<Prefix/binary, "-records-of-service-", HServiceId/binary>>).


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
report_created(MetadataPrefix, HandleServiceId, HandleId, Timestamp) ->
    add_entry(MetadataPrefix, HandleServiceId, HandleId, Timestamp, present).


%% @doc NOTE: not thread-safe, must not be run in parallel with itself or report_deleted/5!
-spec report_updated(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds(), od_handle:timestamp_seconds()) -> ok.
report_updated(MetadataPrefix, HandleServiceId, HandleId, PreviousTimestamp, UpdateTimestamp) ->
    case UpdateTimestamp == PreviousTimestamp of
        true ->
            ok;
        false ->
            delete_entry(MetadataPrefix, HandleServiceId, HandleId, PreviousTimestamp),
            add_entry(MetadataPrefix, HandleServiceId, HandleId, UpdateTimestamp, present)
    end.


%% @doc NOTE: not thread-safe, must not be run in parallel with itself or report_updated/5!
-spec report_deleted(od_handle:metadata_prefix(), od_handle_service:id(), od_handle:id(),
    od_handle:timestamp_seconds(), od_handle:timestamp_seconds()) -> ok.
report_deleted(MetadataPrefix, HandleServiceId, HandleId, PreviousTimestamp, DeletionTimestamp) ->
    delete_entry(MetadataPrefix, HandleServiceId, HandleId, PreviousTimestamp),
    add_entry(MetadataPrefix, HandleServiceId, HandleId, DeletionTimestamp, deleted),
    deleted_handle_registry:insert(MetadataPrefix, HandleServiceId, HandleId, DeletionTimestamp).


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
        deleted_handle_registry:remove(HandleId),
        delete_entry(MetadataPrefix, HandleServiceId, HandleId, Timestamp)
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


-spec list_portion(listing_opts()) -> {[handle_listing_entry()], resumption_token()}.
list_portion(ListingOpts) ->
    list_internal(build_internal_listing_opts(ListingOpts)).


-spec list_completely(listing_opts()) -> [handle_listing_entry()].
list_completely(ListingOpts) ->
    case list_portion(ListingOpts) of
        {List, undefined} -> List;
        {List, ResumptionToken} -> List ++ list_completely(#{resumption_token => ResumptionToken})
    end.


-spec gather_by_all_prefixes() -> [handle_listing_entry()].
gather_by_all_prefixes() ->
    gather_by_all_prefixes(#{}).

-spec gather_by_all_prefixes(listing_opts()) -> [handle_listing_entry()].
gather_by_all_prefixes(ListingOpts) ->
    lists:umerge(lists:map(fun(MetadataPrefix) ->
        list_completely(ListingOpts#{metadata_prefix => MetadataPrefix})
    end, oai_metadata:supported_formats())).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec list_internal(internal_listing_opts()) -> {[handle_listing_entry()], resumption_token()}.
list_internal(#internal_listing_opts{
    limit = Limit,
    tree_id = TreeId,
    start_after_key = StartAfterKey,
    until_timestamp = Until,
    include_deleted = IncludeDeleted
} = Opts) ->

    FoldOpts = #{
        prev_link_name => StartAfterKey,
        prev_tree_id => TreeId,  % necessary for inclusive => false to work
        inclusive => false
    },

    FoldFun = fun(#link{name = Key, target = Value}, Acc) ->
        {Timestamp, HandleId} = decode_link_key(Key),
        % Limit + 1 is used to determine if we have reached the end of the list - see the logic below
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
    {ok, ReversedEntries} = datastore_model:fold_links(?CTX, ?FOREST, TreeId, FoldFun, [], FoldOpts),
    % the internal listing limit is always one element greater than the requested limit,
    % which allows determining if this is the last batch of the complete list
    case length(ReversedEntries) =:= Limit + 1 of
        false ->
            {lists:reverse(ReversedEntries), undefined};
        true ->
            LimitedReversedEntries = tl(ReversedEntries),
            ResumptionToken = pack_resumption_token(Opts#internal_listing_opts{
                start_after_key = encode_link_key(hd(LimitedReversedEntries))
            }),
            {lists:reverse(LimitedReversedEntries), ResumptionToken}
    end.


%% @private
-spec build_internal_listing_opts(listing_opts()) -> internal_listing_opts().
build_internal_listing_opts(#{resumption_token := ResumptionToken}) ->
    unpack_resumption_token(ResumptionToken);
build_internal_listing_opts(#{metadata_prefix := MetadataPrefix} = ListingOpts) ->
    #internal_listing_opts{
        tree_id = case maps:get(service_id, ListingOpts, undefined) of
            undefined -> ?TREE_FOR_METADATA_PREFIX(MetadataPrefix);
            HServiceId -> ?TREE_FOR_METADATA_PREFIX_AND_HSERVICE(MetadataPrefix, HServiceId)
        end,
        limit = utils:ensure_defined(maps:get(limit, ListingOpts, undefined), ?DEFAULT_LIST_LIMIT),
        start_after_key = encode_link_key(utils:ensure_defined(maps:get(from, ListingOpts, undefined), 0), first),
        until_timestamp = utils:ensure_defined(maps:get(until, ListingOpts, undefined), ?MAX_TIMESTAMP),
        include_deleted = maps:get(include_deleted, ListingOpts, false)
    }.


%% @private
-spec pack_resumption_token(internal_listing_opts()) -> resumption_token().
pack_resumption_token(#internal_listing_opts{
    tree_id = TreeId,
    limit = Limit,
    start_after_key = StartAfterKey,
    until_timestamp = UntilTimestamp,
    include_deleted = IncludeDeleted
}) ->
    str_utils:join_binary([
        TreeId,
        integer_to_binary(Limit),
        StartAfterKey,
        integer_to_binary(UntilTimestamp),
        atom_to_binary(IncludeDeleted)
    ], ?RESUMPTION_TOKEN_SEP).


%% @private
-spec unpack_resumption_token(resumption_token()) -> internal_listing_opts().
unpack_resumption_token(ResumptionToken) ->
    [
        TreeId,
        LimitBin,
        StartAfterKey,
        UntilTimestampBin,
        IncludeDeletedBin
    ] = binary:split(ResumptionToken, ?RESUMPTION_TOKEN_SEP, [global]),
    #internal_listing_opts{
        tree_id = TreeId,
        limit = binary_to_integer(LimitBin),
        start_after_key = StartAfterKey,
        until_timestamp = binary_to_integer(UntilTimestampBin),
        include_deleted = binary_to_existing_atom(IncludeDeletedBin)
    }.


%% @private
-spec encode_link_key(handle_listing_entry()) -> link_key().
encode_link_key(#handle_listing_entry{timestamp = Timestamp, handle_id = HandleId}) ->
    encode_link_key(Timestamp, HandleId).

%% @private
-spec encode_link_key(od_handle:timestamp_seconds(), first | od_handle:id()) -> link_key().
encode_link_key(Timestamp, first) ->
    encode_link_key(Timestamp, <<"">>);
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
-spec status_to_binary(status()) -> binary().
status_to_binary(present) -> <<"1">>;
status_to_binary(deleted) -> <<"0">>.

%% @private
-spec binary_to_status(binary()) -> status().
binary_to_status(<<"1">>) -> present;
binary_to_status(<<"0">>) -> deleted.


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