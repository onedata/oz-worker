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

-include("registered_names.hrl").
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
-spec get_msg(Seq :: pos_integer(), Doc :: datastore:document(),
    Model :: subscriptions:model()) -> term().

get_msg(Seq, Doc = #document{deleted = true, key = ID}, Model) ->
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), delete}];
get_msg(Seq, Doc, space = Model) ->
    #document{value = Value, key = ID} = Doc,
    #space{name = Name, users = Users, groups = Groups,
        providers_supports = Supports} = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
        {id, ID},
        {name, Name},
        {providers_supports, Supports},
        {users, Users},
        {groups, Groups}
    ]}];
get_msg(Seq, Doc, user_group = Model) ->
    #document{value = Value, key = ID} = Doc,
    #user_group{name = Name, spaces = Spaces, users = Users} = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
        {name, Name},
        {spaces, Spaces},
        {users, Users}
    ]}];
get_msg(Seq, Doc, onedata_user = Model) ->
    #document{value = Value, key = ID} = Doc,
    #onedata_user{name = Name, spaces = Spaces, groups = Groups} = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
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
    {ok, MaxSize} = application:get_env(?APP_Name, subscriptions_sent_revisions_limit),
    {Start, Hashes} = Revs,
    Size = min(MaxSize, length(Hashes)),

    Numbers = lists:seq(Start, Start - Size + 1, -1),
    PrefixedRevs = lists:zipwith(fun(N, H) ->
        list_to_binary(integer_to_list(N) ++ "-" ++ binary_to_list(H))
    end, Numbers, lists:sublist(Hashes, Size)),
    {revs, PrefixedRevs};
revs_prop(#document{rev = Rev}) ->
    {revs, [Rev]}.

-spec message_model(subscriptions:model()) -> atom().
message_model(space) -> space;
message_model(onedata_user) -> user;
message_model(user_group) -> group.