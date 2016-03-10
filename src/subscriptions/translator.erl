%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Translates documents to structures required by the provider.
%%% @end
%%%-------------------------------------------------------------------
-module(translator).
-author("Michal Zmuda").

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([get_msg/3, get_ignore_msg/1]).

%%%-------------------------------------------------------------------
%%% @doc
%%% Translates documents to structures required by the provider.
%%% Those structures are serializable to json.
%%% @end
%%%-------------------------------------------------------------------
-spec get_ignore_msg(Seq :: pos_integer())
        -> term().

get_ignore_msg(Seq) ->
    [{seq, Seq}, {ignore, true}].

%%%-------------------------------------------------------------------
%%% @doc
%%% Translates documents to structures required by the provider.
%%% Those structures are serializable to json.
%%% @end
%%%-------------------------------------------------------------------
-spec get_msg(Seq :: pos_integer(), Doc :: datastore:document(), Model :: atom())
        -> term().

get_msg(Seq, Doc, space) ->
    #document{value = Value, key = ID} = Doc,
    #space{name = Name} = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {space, [
        {id, ID},
        {name, Name}
    ]}];
get_msg(Seq, Doc, user_group) ->
    #document{value = Value, key = ID} = Doc,
    #user_group{name = Name} = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {group, [
        {name, Name}
    ]}];
get_msg(Seq, Doc, onedata_user) ->
    #document{value = Value, key = ID} = Doc,
    #onedata_user{name = Name, spaces = Spaces, groups = Groups} = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {user, [
        {name, Name},
        {space_ids, Spaces},
        {group_ids, Groups}
    ]}];
get_msg(_Seq, _Doc, _Model) ->
    [].

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec revs_prop(Doc :: datastore:document()) -> term().
revs_prop(#document{rev = Revs}) when is_tuple(Revs) ->
    {Start, Hashes} = Revs,
    Numbers = lists:seq(Start, Start - length(Hashes) + 1, -1),
    PrefixedRevs = lists:zipwith(fun(N, H) ->
        list_to_binary(integer_to_list(N) ++ "-" ++ binary_to_list(H))
    end, Numbers, Hashes),
    {revs, PrefixedRevs};
revs_prop(#document{rev = Rev}) ->
    {revs, [Rev]}.