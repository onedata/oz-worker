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

-export([get_msg/3]).


get_msg(Seq, Doc, space) ->
    #document{value = Value, key = ID} = Doc,
    #space{name = Name, groups = Groups, users = Users} = Value,
    [{seq, Seq}, {space, [
        {id, ID},
        {name, Name},
        {groups, Groups},
        {users, Users}
    ]}];
get_msg(Seq, Doc, user_group) ->
    #document{value = Value, key = ID} = Doc,
    #user_group{users = Users, name = Name, spaces = Spaces} = Value,
    [{seq, Seq}, {group, [
        {id, ID},
        {name, Name},
        {spaces, Spaces},
        {users, Users}
    ]}];
get_msg(Seq, Doc, onedata_user) ->
    #document{value = Value, key = ID} = Doc,
    #onedata_user{name = Name, spaces = Spaces, groups = Groups} = Value,
    [{seq, Seq}, {user, [
        {id, ID},
        {name, Name},
        {spaces, Spaces},
        {groups, Groups}
    ]}];

get_msg(_Seq, _Doc, _Type) ->
    [].