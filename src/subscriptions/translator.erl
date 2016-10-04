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

-export([get_ignore_msg/1, as_msg/3, serialize_timestamp/1]).

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
as_msg(Seq, Doc = #document{value = Val = #od_user{}}, true) ->
    get_public_msg(Seq, Doc, element(1, Val));
as_msg(Seq, Doc = #document{value = Val = #od_provider{}}, true) ->
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
    #od_space{
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
    #od_share{
        name = Name,
        public_url = PublicURL,
        root_file_id = RootFileId,
        parent_space = ParentSpace,
        handle = Handle
    } = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
        {id, ID},
        {name, Name},
        {parent_space, ParentSpace},
        {root_file_id, RootFileId},
        {public_url, PublicURL},
        {handle, Handle}
    ]}];
get_msg(Seq, Doc, user_group = Model) ->
    #document{value = Value, key = ID} = Doc,
    #od_group{name = Name, spaces = Spaces, users = Users, children = NGroups,
        parents = PGroups, eff_users = EUsers, type = Type,
        handle_services = HandleServices, handles = Handles} = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
        {name, Name},
        {type, Type},
        {spaces, Spaces},
        {users, Users},
        {effective_users, EUsers},
        {nested_groups, NGroups},
        {parent_groups, PGroups},
        {handle_services, HandleServices},
        {handles, Handles}
    ]}];
get_msg(Seq, Doc, onedata_user = Model) ->
    #document{value = Value, key = ID} = Doc,
    #od_user{name = Name, space_aliases = SpaceNames, groups = Groups,
        default_space = DefaultSpace, eff_groups = EGroups,
        handle_services = HandleServices, handles = Handles} = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
        {name, Name},
        {space_names, maps:to_list(SpaceNames)},
        {group_ids, Groups},
        {effective_group_ids, EGroups},
        {default_space, DefaultSpace},
        {handle_services, HandleServices},
        {handles, Handles},
        {public_only, false}
    ]}];
get_msg(Seq, Doc, provider = Model) ->
    #document{value = Value, key = ID} = Doc,
    #od_provider{client_name = Name, urls = URLs, spaces = SpaceIDs} = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
        {client_name, Name},
        {urls, URLs},
        {space_ids, SpaceIDs},
        {public_only, false}
    ]}];
get_msg(Seq, Doc, handle_service = Model) ->
    #document{value = Value, key = ID} = Doc,
    #od_handle_service{
        name = Name,
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties,
        users = Users,
        groups = Groups
    } = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
        {id, ID},
        {name, Name},
        {proxy_endpoint, ProxyEndpoint},
        {service_properties, ServiceProperties},
        {users, Users},
        {groups, Groups}
    ]}];
get_msg(Seq, Doc, handle = Model) ->
    #document{value = Value, key = ID} = Doc,
    #od_handle{
        handle_service_id = HandleServiceId,
        public_handle = PublicHandle,
        resource_type = ResourceType,
        resource_id = ResourceId,
        metadata = Metadata,
        users = Users,
        groups = Groups,
        timestamp = Timestamp
    } = Value,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
        {id, ID},
        {handle_service_id, HandleServiceId},
        {public_handle, PublicHandle},
        {resource_type, ResourceType},
        {resource_id, ResourceId},
        {metadata, Metadata},
        {users, Users},
        {groups, Groups},
        {timestamp, serialize_timestamp(Timestamp)}
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
    #document{value = #od_user{name = Name}, key = ID} = Doc,
    [{seq, Seq}, revs_prop(Doc), {id, ID}, {message_model(Model), [
        {name, Name},
        {space_ids, []},
        {group_ids, []},
        {effective_group_ids, []},
        {default_space, undefined},
        {handle_services, []},
        {handles, []},
        {public_only, true}
    ]}];

get_public_msg(Seq, Doc, provider = Model) ->
    #document{key = ID, value = #od_provider{client_name = Name, urls = URLs}} = Doc,
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
message_model(user_group) -> group;
message_model(handle) -> handle;
message_model(handle_service) -> handle_service.


%%-------------------------------------------------------------------
%% @doc
%% @private
%% Translates erlang datetime format into a list of integers, which can be
%% safely send in JSON.
%% @end
%%-------------------------------------------------------------------
-spec serialize_timestamp(calendar:datetime()) -> [integer()].
serialize_timestamp({{A, B, C}, {D, E, F}}) ->
    [A, B, C, D, E, F].
