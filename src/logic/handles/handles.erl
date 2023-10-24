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

-export([index/2]).
-export([add/4, delete/4]).
-export([list/0, list/1, get_earliest_timestamp/0]).

% index() consists of 2 parts:
%  1) timestamp (in seconds) - so that links would be sorted by time.
%  2) handle id - this part allows to distinguish links associated with handles
%                that have the same timestamp.
-type index() :: binary().

-type limit() :: pos_integer().
-type size() :: pos_integer().

-type listing_opts() :: #{
size := size(),
limit := limit(),
start_index => index()
}.

-type resumption_token() :: index().

-define(CTX, (od_handle:get_ctx())).

-define(FOREST, <<"handle-forest">>).

-define(ALL_TREE_ID, <<"handle-all-tree">>).
-define(HANDLE_METADATA_FORMAT_TREE_ID(__MdFormat),
    <<"handle-metadata", __MdFormat/binary, "-all-tree">>).
-define(HSERVICE_TREE_ID(__HsId), <<"handle-tree-for-", __HsId/binary>>).
-define(METADATA_FORMAT_HSERVICE_TREE_ID(__HsId, __MdFormat),
    <<"handle-metadata", __MdFormat/binary," -tree-for-", __HsId/binary>>).

% Uses null for separator to ensure alphabetical sorting
-define(INDEX_SEP, 0).
-define(MAX_LIST_LIMIT, 1000).
-define(DEFAULT_LIST_LIMIT, 1000).


%%%===================================================================
%%% API
%%%===================================================================


-spec index(time:seconds(), od_handle:id()) -> index().
index(first, From) ->
    case From of
        <<>> -> From;
        _ -> str_utils:format_bin("~11..0B", [From])
    end;
index(TimeSeconds, HandleId) ->
    FormattedTimeSeconds = str_utils:format_bin("~11..0B", [TimeSeconds]),
    <<(FormattedTimeSeconds)/binary, ?INDEX_SEP, HandleId/binary>>.


-spec decode_index(index()) -> {time:seconds(), od_handle:id()}.
decode_index(Index) ->
    <<TimeSeconds:11/binary, 0, HandleId/binary>> = Index,
    {binary_to_integer(TimeSeconds), HandleId}.


-spec add(time:seconds(), od_handle:id(), od_handle_service:id(), od_handle:metadata_prefix()) -> ok.
add(TimeSeconds, HandleId, HandleServiceId, MetadataPrefix) ->
    Link = {index(TimeSeconds, HandleId), HandleId},

    lists:foreach(fun(TreeId) ->
        case datastore_model:add_links(?CTX, ?FOREST, TreeId, Link) of
            {ok, _} -> ok;
            {error, already_exists} -> throw(?ERROR_ALREADY_EXISTS)
        end
    end, [
        ?ALL_TREE_ID,
        ?HSERVICE_TREE_ID(HandleServiceId),
        ?HANDLE_METADATA_FORMAT_TREE_ID(MetadataPrefix),
        ?METADATA_FORMAT_HSERVICE_TREE_ID(HandleServiceId, MetadataPrefix)
        ]).


-spec delete(time:seconds(), od_handle:id(), od_handle_service:id(), od_handle:metadata_prefix()) -> ok.
delete(TimeSeconds, HandleId, HandleServiceId, MetadataPrefix) ->
    Index = index(TimeSeconds, HandleId),
    lists:foreach(fun(TreeId) ->
        case datastore_model:delete_links(?CTX, ?FOREST, TreeId, Index) of
            ok -> ok;
            {error, not_found} -> ok
        end
    end, [
        ?ALL_TREE_ID,
        ?HSERVICE_TREE_ID(HandleServiceId),
        ?HANDLE_METADATA_FORMAT_TREE_ID(MetadataPrefix),
        ?METADATA_FORMAT_HSERVICE_TREE_ID(HandleServiceId, MetadataPrefix)
    ]).


-spec list(listing_opts()) -> [od_handle:id()] |
{[od_handle:id()], resumption_token()}.
list() ->
    list(#{}).
list(ListingOpts) ->
    ServiceId = resolve_when_undefined(service_id, ListingOpts, undefined),
    MetadataPrefix = resolve_when_undefined(metadata_prefix, ListingOpts, undefined),

    TreeId = case {ServiceId, MetadataPrefix} of
        {undefined, undefined} -> ?ALL_TREE_ID;
        {undefined, _} -> ?HANDLE_METADATA_FORMAT_TREE_ID(MetadataPrefix);
        {_, undefined} -> ?HSERVICE_TREE_ID(ServiceId);
        {_, _} -> ?METADATA_FORMAT_HSERVICE_TREE_ID(ServiceId, MetadataPrefix)
    end,
    Size = maps:get(size, ListingOpts, ?DEFAULT_LIST_LIMIT),
    From = resolve_when_undefined(from, ListingOpts, <<>>),
    Until = resolve_when_undefined(until, ListingOpts, <<>>),
    Token = maps:get(resumption_token, ListingOpts, <<>>),
    StartIndex = case Token of
        <<>> -> index(first, From);
        _ -> Token
    end,
    FoldOpts = #{
        size => Size,
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
    {ok, InternalEntries} = datastore_model:fold_links(
        ?CTX, ?FOREST, TreeId, FoldFun, [], FoldOpts
    ),
    case InternalEntries of
        [] -> {InternalEntries, undefined};
        _ -> build_result_from_reversed_listing(InternalEntries, Token)
    end.


-spec get_earliest_timestamp() -> none | calendar:datetime().
get_earliest_timestamp() ->
    ListingOpts = #{size => 1},
    {List, undefined} = handles:list(ListingOpts),
    case List of
        [] -> none;
        [HandleId] ->
            {ok, #document{value = #od_handle{
                timestamp = TimeSecondsBin
            }}} = od_handle:get(HandleId),
            time:seconds_to_datetime(TimeSecondsBin)
    end.


%% @private
build_result_from_reversed_listing(InternalEntries, Token) ->
    NewToken = case length(InternalEntries) < ?MAX_LIST_LIMIT of
        true -> <<>>;
        false ->
            {TimeSeconds, HandleId} = hd(InternalEntries),
            index(TimeSeconds, HandleId)
    end,
    ReversedEntries = lists:reverse(InternalEntries),
    Handles = lists:map(fun({_, HandleId}) -> HandleId end, ReversedEntries),
    case length(ReversedEntries) < ?MAX_LIST_LIMIT andalso Token == <<>> of
        true -> {Handles, undefined};
        false -> {Handles, NewToken}
    end.


%% @private
resolve_when_undefined(Atom, ListingOpts, WhenUndefined) ->
    DefinedElement = case maps:get(Atom, ListingOpts, undefined) of
        undefined -> WhenUndefined;
        Element -> Element
    end,
    DefinedElement.