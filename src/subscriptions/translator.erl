%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(translator).
-author("Michal Zmuda").

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([get_msg/3]).


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
get_msg(_Seq, _Doc, _Type) ->
    [].

revs_prop(Doc) ->
    {revs, element(2, Doc#document.rev)}.