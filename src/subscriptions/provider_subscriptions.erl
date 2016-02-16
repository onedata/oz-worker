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
-module(provider_subscriptions).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([renew/3]).


-spec renew(ProviderID :: binary(), LastSeenSeq :: non_neg_integer(), Endpoint :: binary()) -> no_return().
renew(ProviderID, LastSeenSeq, Endpoint) ->
    Dispatcher = get_callback(Endpoint),
    ?info("Adding subscription ~p", [[ProviderID, LastSeenSeq, Endpoint, Dispatcher]]),
    worker_proxy:call(subscriptions, {provider_subscribe, ProviderID, Dispatcher, LastSeenSeq}).


get_callback(Endpoint) ->
    fun(Seq, Doc, Type) ->
        Contents = get_msg(Seq, Doc, Type),
        case Contents of
            [] -> ok;
            _ -> spawn(fun() ->
                Message = json_utils:encode(Contents),
                ?info("Sending ~p", [[Endpoint, Contents]]),
                http_client:post(Endpoint, [{async, once}], Message)
            end)
        end
    end.


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