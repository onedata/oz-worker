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

-export([get_ignore_msg/1, as_msg/3]).

%%%-------------------------------------------------------------------
%%% @doc
%%% Translates documents to structures required by the provider.
%%% Those structures are serializable to json.
%%% @end
%%%-------------------------------------------------------------------
-spec as_msg(Seq :: subscriptions:seq(), Doc :: datastore:document(),
    Ignore :: boolean()) -> term().
as_msg(Seq, Doc = #document{value = Value}, false) ->
    get_msg(Seq, Doc, element(1, Value));
as_msg(-1, Doc = #document{value = Value}, _) ->
    get_msg(-1, Doc, element(1, Value));
as_msg(Seq, Doc = #document{value = Val = #onedata_user{}}, true) ->
    get_public_msg(Seq, Doc, element(1, Val));
as_msg(Seq, Doc = #document{value = Val = #provider{}}, true) ->
    get_public_msg(Seq, Doc, element(1, Val));
as_msg(Seq, _Doc, _Ignore) ->
    get_ignore_msg(Seq).

%%%-------------------------------------------------------------------
%%% @doc
%%% Translates documents to structures required by the provider.
%%% Those structures are serializable to json and contain "ignore" commands.
%%% @end
%%%-------------------------------------------------------------------
-spec get_ignore_msg(Seq :: subscriptions:seq()) -> term().
get_ignore_msg(Seq) ->
    [{seq, Seq}, {ignore, true}].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% @private
%%% Translates documents to structures required by the provider.
%%% Those structures are serializable to json and provide details from documents.
%%% @end
%%%-------------------------------------------------------------------
-spec get_msg(Seq :: subscriptions:seq(), Doc :: datastore:document(),
    Model :: subscriptions:model()) -> term().

get_msg(Seq, Doc = #document{deleted = true, key = ID}, Model) ->
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), delete}];
get_msg(Seq, Doc, space = Model) ->
    #document{value = Value, key = ID} = Doc,
    #space{
        name = Name,
        users = Users,
        groups = Groups,
        providers_supports = Supports,
        shares = Shares
    } = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
        {id, ID},
        {name, Name},
        {providers_supports, Supports},
        {users, Users},
        {groups, Groups},
        {shares, Shares}
    ]}];
get_msg(Seq, Doc, share = Model) ->
    #document{value = Value, key = ID} = Doc,
    #share{
        name = Name,
        public_url = PublicURL,
        root_file_id = RootFileId,
        parent_space = ParentSpace
    } = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
        {id, ID},
        {name, Name},
        {parent_space, ParentSpace},
        {root_file_id, RootFileId},
        {public_url, PublicURL}
    ]}];
get_msg(Seq, Doc, user_group = Model) ->
    #document{value = Value, key = ID} = Doc,
    #user_group{name = Name, spaces = Spaces, users = Users, nested_groups = NGroups,
        parent_groups = PGroups, effective_users = EUsers, type = Type} = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
        {name, Name},
        {type, Type},
        {spaces, Spaces},
        {users, Users},
        {effective_users, EUsers},
        {nested_groups, NGroups},
        {parent_groups, PGroups}
    ]}];
get_msg(Seq, Doc, onedata_user = Model) ->
    #document{value = Value, key = ID} = Doc,
    #onedata_user{name = Name, space_names = SpaceNames, groups = Groups,
        default_space = DefaultSpace, effective_groups = EGroups} = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
        {name, Name},
        {space_names, maps:to_list(SpaceNames)},
        {group_ids, Groups},
        {effective_group_ids, EGroups},
        {default_space, DefaultSpace},
        {public_only, false}
    ]}];
get_msg(Seq, Doc, provider = Model) ->
    #document{value = Value, key = ID} = Doc,
    #provider{client_name = Name, urls = URLs, spaces = SpaceIDs} = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
        {client_name, Name},
        {urls, URLs},
        {space_ids, SpaceIDs},
        {public_only, false}
    ]}];
get_msg(_Seq, _Doc, _Model) ->
    ?warning("Requesting message for unexpected model ~p", [_Model]),
    [].


%%%-------------------------------------------------------------------
%%% @doc
%%% @private
%%% Translates documents to structures required by the provider.
%%% Only public information is included.
%%% Those structures are serializable to json and provide details from documents.
%%% @end
%%%-------------------------------------------------------------------
-spec get_public_msg(Seq :: subscriptions:seq(), Doc :: datastore:document(),
    Model :: subscriptions:model()) -> term().

get_public_msg(Seq, Doc, onedata_user = Model) ->
    #document{value = #onedata_user{name = Name}, key = ID} = Doc,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
        {name, Name},
        {space_ids, []},
        {group_ids, []},
        {effective_group_ids, []},
        {default_space, undefined},
        {public_only, true}
    ]}];

get_public_msg(Seq, Doc, provider = Model) ->
    #document{key = ID, value = #provider{client_name = Name, urls = URLs}} = Doc,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
        {client_name, Name},
        {urls, URLs},
        {space_ids, []},
        {public_only, true}
    ]}].

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
message_model(share) -> share;
message_model(provider) -> provider;
message_model(onedata_user) -> user;
message_model(user_group) -> group.
