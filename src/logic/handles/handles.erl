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
-export([add/3, delete/3]).
-export([list/0, list/2, get_earliest_timestamp/0]).

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
-type until() :: pos_integer().

-define(CTX, (od_handle:get_ctx())).

-define(FOREST, <<"handle-forest">>).

-define(ALL_TREE_ID, <<"handle-all-tree">>).
-define(HSERVICE_TREE_ID(__HsId), <<"handle-tree-for-", __HsId/binary>>).

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
        _ -> integer_to_binary(From)
    end;
index(TimeSeconds, HandleId) ->
    FormattedTimeSeconds = str_utils:format_bin("~11..0B", [TimeSeconds]),
    <<(FormattedTimeSeconds)/binary, ?INDEX_SEP, HandleId/binary>>.


-spec decode_index(index()) -> {time:seconds(), od_handle:id()}.
decode_index(Index) ->
    <<TimeSeconds:11/binary, 0, HandleId/binary>> = Index,
    {binary_to_integer(TimeSeconds), HandleId}.


-spec add(time:seconds(), od_handle:id(), od_handle_service:id()) -> ok.
add(TimeSeconds, HandleId, HandleServiceId) ->
    Link = {index(TimeSeconds, HandleId), HandleId},

    lists:foreach(fun(TreeId) ->
        case datastore_model:add_links(?CTX, ?FOREST, TreeId, Link) of
            {ok, _} -> ok;
            {error, already_exists} -> ok
        end
    end, [?ALL_TREE_ID, ?HSERVICE_TREE_ID(HandleServiceId)]).


-spec delete(time:seconds(), od_handle:id(), od_handle_service:id()) -> ok.
delete(TimeSeconds, HandleId, HandleServiceId) ->
    Index = index(TimeSeconds, HandleId),
    lists:foreach(fun(TreeId) ->
        case datastore_model:delete_links(?CTX, ?FOREST, TreeId, Index) of
            ok -> ok;
            {error, not_found} -> ok
        end
    end, [?ALL_TREE_ID, ?HSERVICE_TREE_ID(HandleServiceId)]).


-spec list(all | od_handle_service:id(), listing_opts()) -> [od_handle:id()] |
{[od_handle:id()], {resumption_token(), until()}}.
list() ->
    list(all, #{}).
list(WhatToList, ListingOpts) ->
    TreeId = case WhatToList of
        all -> ?ALL_TREE_ID;
        _ -> ?HSERVICE_TREE_ID(WhatToList)
    end,
    Size = maps:get(size, ListingOpts, ?DEFAULT_LIST_LIMIT),
    From = maps:get(from, ListingOpts, <<>>),
    Until = maps:get(until, ListingOpts, <<>>),
    Token = maps:get(resumption_token, ListingOpts, <<>>),
    StartIndex = case Token of
        <<>> -> index(first, From);
        _ -> Token
    end,
    FoldOpts = #{
        size => Size,
        prev_link_name => StartIndex,
        inclusive => false
    },
    FoldFun = fun(#link{name = Index, target = HandleId}, Acc) ->
        {TimeSeconds, _HId} = decode_index(Index),
        % TODO
        case TimeSeconds > Until of
            true -> {stop, [Acc]};
            false -> {ok, [{TimeSeconds, HandleId} | Acc]}
        end
    end,
%%    FoldFun = fun(#link{name = Index, target = HandleId}, Acc) ->
%%        {TimeSeconds, _HId} = decode_index(Index),
%%        {ok, [{TimeSeconds, HandleId} | Acc]}
%%    end,

    {ok, InternalEntries} = datastore_model:fold_links(
        ?CTX, ?FOREST, TreeId, FoldFun, [], FoldOpts
    ),
%%    InternalEntries.
    case InternalEntries of
        [] -> InternalEntries;
        _ -> handle_entries(InternalEntries, Token, Until)
    end.


-spec get_earliest_timestamp() -> none | calendar:datetime().
get_earliest_timestamp() ->
    ListingOpts = #{size => 1},
    List = handles:list(all, ListingOpts),
    case List of
        [HandleId] ->
            {ok, #document{value = #od_handle{
                timestamp = TimeSecondsBin
            }}} = od_handle:get(HandleId),
            time:seconds_to_datetime(binary_to_integer(TimeSecondsBin));
        [] -> none
    end.


%% @private
handle_entries(InternalEntries, Token, Until) ->
    ReversedEntries = lists:reverse(InternalEntries),
    NewToken = case length(ReversedEntries) < ?MAX_LIST_LIMIT of
        true -> <<>>;
        false ->
            {TimeSeconds, HandleId} = lists:last(ReversedEntries),
            index(TimeSeconds, HandleId)
    end,
    Handles = case Token of
        <<>> -> ReversedEntries;
        _ -> tl(ReversedEntries)
    end,
    EntriesNoTime = lists:map(fun({_, HandleId}) -> HandleId end, Handles),
    case length(ReversedEntries) < ?MAX_LIST_LIMIT andalso Token == <<>> of
        true -> EntriesNoTime;
        false -> {EntriesNoTime, {{resumption_token, NewToken}, {until, Until}}}
    end.



