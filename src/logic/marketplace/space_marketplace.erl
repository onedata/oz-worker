%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles storing spaces advertised in marketplace
%%% @end
%%%-------------------------------------------------------------------
-module(space_marketplace).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").

-export([add/2, update/3, delete/2]).
-export([list_all/0]).

% index() consists of 2 parts:
%  1) space name - so that links would be sorted by name.
%  2) space id - this part allows to distinguish links associated with spaces
%                using the same name.
-type index() :: binary().

-define(CTX, (od_provider:get_ctx())).

-define(FOREST, <<"space-marketplace-forest">>).
-define(TREE_ID, <<"space-marketplace-tree">>).


%%%===================================================================
%%% API
%%%===================================================================


-spec add(od_space:name(), od_space:id()) -> ok | {error, term()}.
add(SpaceName, SpaceId) ->
    Index = index(SpaceName, SpaceId),

    case datastore_model:add_links(?CTX, ?FOREST, ?TREE_ID, {Index, SpaceId}) of
        {ok, _} -> ok;
        {error, already_exists} -> ?ERROR_ALREADY_EXISTS
    end.


-spec update(od_space:name(), od_space:id(), od_space:name()) ->
    ok | {error, term()}.
update(PrevName, SpaceId, NewName) ->
    case add(NewName, SpaceId) of
        ok ->
            delete(PrevName, SpaceId);
        ?ERROR_ALREADY_EXISTS ->
            ?ERROR_ALREADY_EXISTS
    end.


-spec delete(od_space:name(), od_space:id()) ->
    ok | {error, term()}.
delete(SpaceName, SpaceId) ->
    Index = index(SpaceName, SpaceId),

    datastore_model:delete_links(?CTX, ?FOREST, ?TREE_ID, Index).


-spec list_all() -> {ok, [od_space:id()]} | {error, term()}.
list_all() ->
    FoldFun = fun(Link, Acc) -> {ok, [Link#link.target | Acc]} end,

    case datastore_model:fold_links(?CTX, ?FOREST, ?TREE_ID, FoldFun, [], #{}) of
        {ok, SpaceIds} -> {ok, lists:reverse(SpaceIds)};
        {error, _} = Error -> Error
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec index(od_space:name(), od_space:id()) -> index().
index(SpaceName, SpaceId) ->
    <<SpaceName/binary, "@", SpaceId/binary>>.
