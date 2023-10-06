%%%-------------------------------------------------------------------
%%% @author Katarzyna Such
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles storing handles in handle services
%%% @end
%%%-------------------------------------------------------------------
-module(handle).
-author("Katarzyna Such").

-include("datastore/oz_datastore_models.hrl").

-export([index/2]).
-export([add/3, delete/3]).
-export([list/2]).

% index()/internal_index() consists of 2 parts:
%  1) time seconds - so that links would be sorted by time.
%  2) handle id - this part allows to distinguish links associated with handles
%                that have the same data stamp.
-type index() :: binary().

-type limit() :: pos_integer().

-type listing_opts() :: #{
    limit := limit(),
    start_index => index()
}.

-type entries() :: [{index(), od_handle:id()}].
-type resumption_token() :: index().

-define(CTX, (od_handle:get_ctx())).

-define(FOREST, <<"handle-forest">>).

-define(ALL_TREE_ID, <<"handle-all-tree">>).
-define(SERVICE_TREE_ID(__HANDLE_SERVICE), <<"handle-service-tree-", __HANDLE_SERVICE/binary>>).

% Uses null for separator to ensure alphabetical sorting
-define(INDEX_SEP, 0).
-define(SIZE, 1000).


%%%===================================================================
%%% API
%%%===================================================================


-spec index(time:seconds(), od_handle:id()) -> index().
index(TimeSeconds, HandleId) ->
    <<(TimeSeconds)/binary, ?INDEX_SEP, HandleId/binary>>.

-spec add(time:seconds(), od_handle:id(), od_handle_service:id()) -> ok.
add(TimeSeconds, HandleId, HandleServiceId) ->
    Link = {index(integer_to_binary(TimeSeconds), HandleId), HandleId},

    lists:foreach(fun(TreeId) ->
        case datastore_model:add_links(?CTX, ?FOREST, TreeId, Link) of
            {ok, _} -> ok;
            {error, already_exists} -> ok
        end
    end, [?ALL_TREE_ID, ?SERVICE_TREE_ID(HandleServiceId)]).


-spec delete(time:seconds(), od_handle:id(), od_handle_service:id()) -> ok.
delete(TimeSeconds, HandleId, HandleServiceId) ->
    Index = index(TimeSeconds, HandleId),
    lists:foreach(fun(TreeId) ->
        case datastore_model:delete_links(?CTX, ?FOREST, TreeId, Index) of
            ok -> ok;
            {error, not_found} -> ok
        end
    end, [?ALL_TREE_ID, ?SERVICE_TREE_ID(HandleServiceId)]).


-spec list(all | od_handle_service:id(), listing_opts()) -> {entries(), resumption_token()}.
list(WhatToList, ListingOpts) ->
    TreeId = case WhatToList of
        all -> ?ALL_TREE_ID;
        _ -> ?SERVICE_TREE_ID(WhatToList)
    end,
    From =maps:get(from, ListingOpts, <<>>),
    Until =maps:get(until, ListingOpts, <<>>),
    Token = maps:get(resumption_token, ListingOpts, <<>>),
    Start = case Token of
        <<>> ->
            case From of
                <<>> -> Token;
                _ -> integer_to_binary(From)
            end ;
        _ -> Token
    end,
    FoldOpts = #{
        size => ?SIZE,
        prev_link_name => Start,
        inclusive => false
    },
    FoldFun = fun(#link{name = Index, target = _HandleId}, Acc) ->
        %% this will work for timestamp >= 1000000000 (i.e {2001, 9, 9}, {3, 46, 40}})
        <<TimeSeconds:10/binary, 0, IndexHandleId/binary>> = Index,
        {ok, [{TimeSeconds, IndexHandleId} | Acc]}
    end,
    {ok, InternalEntries} = datastore_model:fold_links(
        ?CTX, ?FOREST, TreeId, FoldFun, [], FoldOpts
    ),
    handle_entries(InternalEntries, Token, Until).


%% @private
handle_entries(InternalEntries, Token, Until) ->
    %% checking head because this is before reverse
    {TimeSecondsBin, _HandleId} = hd(InternalEntries),
    TimeSeconds = binary_to_integer(TimeSecondsBin),
    Entries = case TimeSeconds > Until of
        false -> InternalEntries;
        true ->
            %% before reverse, so the newest are at the beginning
            lists:dropwhile(fun({TimeSecondsBin, _HandleId}) ->
                binary_to_integer(TimeSecondsBin) > Until
            end, InternalEntries)
    end,
    ReversedEntries =  lists:reverse(Entries),
    Handles = case Token of
        <<>> -> ReversedEntries;
        _ -> tl(ReversedEntries)
    end,
    NewToken = case length(ReversedEntries) < ?SIZE of
        true -> <<>>;
        false -> lists:last(Handles)
    end,
    case length(ReversedEntries) < ?SIZE andalso Token == <<>> of
        true -> Handles;
        false -> {Handles, {{resumption_token, NewToken}, {until, Until}}}
    end.
