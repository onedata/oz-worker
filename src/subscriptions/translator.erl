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

get_msg(Seq, Doc = #document{deleted = true, key = Id}, Model) ->
    [{seq, Seq}, revs_prop(Doc), {id, Id}, {message_model(Model), delete}];
get_msg(Seq, Doc, od_user = Model) ->
    #document{value = Value, key = Id} = Doc,
    #od_user{
        name = Name,
        default_space = DefaultSpace,
        space_aliases = SpaceAliases,

        groups = Groups,
        handle_services = HandleServices,
        handles = Handles,

        eff_groups = EffGroups
    } = Value,
    [{seq, Seq}, revs_prop(Doc), {id, Id}, {message_model(Model), [
        {name, Name},
        {alias, <<"">>}, % TODO currently always empty
        {email_list, []}, % TODO currently always empty
        {connected_accounts, []}, % TODO currently always empty
        {default_space, DefaultSpace},
        {space_aliases, maps:to_list(SpaceAliases)},

        % Direct relations to other entities
        {groups, Groups},
        {spaces, []}, % TODO currently always empty
        {handle_services, HandleServices},
        {handles, Handles},

        % Effective relations to other entities
        {eff_groups, EffGroups},
        {eff_spaces, []}, % TODO currently always empty
        {eff_shares, []}, % TODO currently always empty
        {eff_providers, []}, % TODO currently always empty
        {eff_handle_services, []}, % TODO currently always empty
        {eff_handles, []}, % TODO currently always empty

        {public_only, false}
    ]}];
get_msg(Seq, Doc, od_group = Model) ->
    #document{value = Value, key = Id} = Doc,
    #od_group{
        name = Name,
        type = Type,

        children = Children,
        parents = Parents,

        users = Users,
        spaces = Spaces,
        handle_services = HandleServices,
        handles = Handles,

        eff_users = EUsers
    } = Value,
    [{seq, Seq}, revs_prop(Doc), {id, Id}, {message_model(Model), [
        {name, Name},
        {type, Type},

        % Group graph related entities (direct and effective)
        {parents, Parents},
        {children, Children},
        {eff_children, []}, % TODO currently always empty
        {eff_parents, []}, % TODO currently always empty

        % Direct relations to other entities
        {users, Users},
        {spaces, Spaces},
        {handle_services, HandleServices},
        {handles, Handles},

        % Effective relations to other entities
        {eff_users, EUsers},
        {eff_spaces, []}, % TODO currently always empty
        {eff_shares, []}, % TODO currently always empty
        {eff_providers, []}, % TODO currently always empty
        {eff_handle_services, []}, % TODO currently always empty
        {eff_handles, []} % TODO currently always empty
    ]}];
get_msg(Seq, Doc, od_space = Model) ->
    #document{value = Value, key = Id} = Doc,
    #od_space{
        name = Name,

        providers_supports = Supports,
        users = Users,
        groups = Groups,
        shares = Shares
    } = Value,
    [{seq, Seq}, revs_prop(Doc), {id, Id}, {message_model(Model), [
        {id, Id},
        {name, Name},

        % Direct relations to other entities
        {providers_supports, Supports},
        {users, Users},
        {groups, Groups},
        {shares, Shares},

        % Effective relations to other entities
        {eff_users, []}, % TODO currently always empty
        {eff_groups, []} % TODO currently always empty
    ]}];
get_msg(Seq, Doc, od_share = Model) ->
    #document{value = Value, key = Id} = Doc,
    #od_share{
        name = Name,
        public_url = PublicURL,

        space = Space,
        handle = Handle,
        root_file = RootFile
    } = Value,
    [{seq, Seq}, revs_prop(Doc), {id, Id}, {message_model(Model), [
        {id, Id},
        {name, Name},
        {public_url, PublicURL},

        % Direct relations to other entities
        {space, Space},
        {handle, Handle},
        {root_file, RootFile},

        % Effective relations to other entities
        {eff_users, []}, % TODO currently always empty
        {eff_groups, []} % TODO currently always empty
    ]}];
get_msg(Seq, Doc, od_provider = Model) ->
    #document{value = Value, key = Id} = Doc,
    #od_provider{client_name = Name, urls = URLs, spaces = SpaceIds} = Value,
    [{seq, Seq}, revs_prop(Doc), {id, Id}, {message_model(Model), [
        {client_name, Name},
        {urls, URLs},

        % Direct relations to other entities
        {spaces, SpaceIds},

        {public_only, false}
    ]}];
get_msg(Seq, Doc, od_handle_service = Model) ->
    #document{value = Value, key = Id} = Doc,
    #od_handle_service{
        name = Name,
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties,
        users = Users,
        groups = Groups
    } = Value,
    [{seq, Seq}, revs_prop(Doc), {id, Id}, {message_model(Model), [
        {id, Id},
        {name, Name},
        {proxy_endpoint, ProxyEndpoint},
        {service_properties, ServiceProperties},

        % Direct relations to other entities
        {users, Users},
        {groups, Groups},

        % Effective relations to other entities
        {eff_users, []}, % TODO currently always empty
        {eff_groups, []} % TODO currently always empty
    ]}];
get_msg(Seq, Doc, od_handle = Model) ->
    #document{value = Value, key = Id} = Doc,
    #od_handle{
        public_handle = PublicHandle,
        resource_type = ResourceType,
        resource_id = ResourceId,
        metadata = Metadata,
        timestamp = Timestamp,

        handle_service = HandleService,
        users = Users,
        groups = Groups
    } = Value,
    [{seq, Seq}, revs_prop(Doc), {id, Id}, {message_model(Model), [
        {id, Id},
        {public_handle, PublicHandle},
        {resource_type, ResourceType},
        {resource_id, ResourceId},
        {metadata, Metadata},
        {timestamp, timestamp_utils:datetime_to_datestamp(Timestamp)},

        % Direct relations to other entities
        {handle_service, HandleService},
        {users, Users},
        {groups, Groups},

        % Effective relations to other entities
        {eff_users, []}, % TODO currently always empty
        {eff_groups, []} % TODO currently always empty
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

get_public_msg(Seq, Doc, od_user = Model) ->
    #document{value = #od_user{name = Name}, key = Id} = Doc,
    [{seq, Seq}, revs_prop(Doc), {id, Id}, {message_model(Model), [
        {name, Name},
        {alias, <<"">>},
        {email_list, []},
        {connected_accounts, []},
        {default_space, undefined},
        {space_aliases, []},

        % Direct relations to other entities
        {groups, []},
        {spaces, []},
        {handle_services, []},
        {handles, []},

        % Effective relations to other entities
        {eff_groups, []},
        {eff_spaces, []},
        {eff_shares, []},
        {eff_providers, []},
        {eff_handle_services, []},
        {eff_handles, []},

        {public_only, true}
    ]}];

get_public_msg(Seq, Doc, od_provider = Model) ->
    #document{key = Id, value = #od_provider{client_name = Name, urls = URLs}} = Doc,
    [{seq, Seq}, revs_prop(Doc), {id, Id}, {message_model(Model), [
        {client_name, Name},
        {urls, URLs},

        {spaces, []},

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
message_model(od_space) -> od_space;
message_model(od_share) -> od_share;
message_model(od_provider) -> od_provider;
message_model(od_user) -> od_user;
message_model(od_group) -> od_group;
message_model(od_handle) -> od_handle;
message_model(od_handle_service) -> od_handle_service.