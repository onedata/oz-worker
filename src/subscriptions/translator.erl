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
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/utils/utils.hrl").

-export([get_ignore_msg/1, as_msg/3]).

% TODO VFS-2918
-export([calculate_space_aliases/2]).

-define(MIN_SUFFIX_HASH_LEN, 6).

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
        linked_accounts = LinkedAccounts,

        groups = Groups,
        handle_services = HandleServices,
        handles = Handles,

        eff_groups = EffGroups,
        eff_spaces = EffSpaces
    } = Value,
    [{seq, Seq}, revs_prop(Doc), {id, Id}, {message_model(Model), [
        {name, Name},
        {alias, <<"">>}, % TODO currently always empty
        {email_list, []}, % TODO currently always empty
        {connected_accounts, serialize_linked_accounts(LinkedAccounts)},
        {default_space, DefaultSpace},
        {space_aliases, maps:to_list(translator:calculate_space_aliases(
            eff_relation_to_proplist(EffSpaces), SpaceAliases
        ))},

        % Direct relations to other entities
        {groups, Groups},
        {spaces, []}, % TODO currently always empty
        {handle_services, HandleServices},
        {handles, Handles},

        % Effective relations to other entities
        {eff_groups, eff_relation_to_proplist(EffGroups)},
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
        {children, relation_with_attrs_to_proplist(Children)},
        {eff_children, []}, % TODO currently always empty
        {eff_parents, []}, % TODO currently always empty

        % Direct relations to other entities
        {users, relation_with_attrs_to_proplist(Users)},
        {spaces, Spaces},
        {handle_services, HandleServices},
        {handles, Handles},

        % Effective relations to other entities
        {eff_users, eff_relation_with_attrs_to_proplist(EUsers)},
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

        providers = Supports,
        users = Users,
        groups = Groups,
        shares = Shares
    } = Value,
    [{seq, Seq}, revs_prop(Doc), {id, Id}, {message_model(Model), [
        {id, Id},
        {name, Name},

        % Direct relations to other entities
        {providers_supports, relation_with_attrs_to_proplist(Supports)},
        {users, relation_with_attrs_to_proplist(Users)},
        {groups, relation_with_attrs_to_proplist(Groups)},
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
    #od_provider{
        name = Name, urls = URLs,
        latitude = Latitude, longitude = Longitude,
        spaces = Spaces
    } = Value,
    [{seq, Seq}, revs_prop(Doc), {id, Id}, {message_model(Model), [
        {client_name, Name},
        {urls, URLs},
        {latitude, Latitude},
        {longitude, Longitude},

        % Direct relations to other entities
        {spaces, relation_to_proplist(Spaces)},

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
        {service_properties, maps:to_list(ServiceProperties)},

        % Direct relations to other entities
        {users, relation_with_attrs_to_proplist(Users)},
        {groups, relation_with_attrs_to_proplist(Groups)},

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
        {timestamp, time_utils:datetime_to_datestamp(Timestamp)},

        % Direct relations to other entities
        {handle_service, HandleService},
        {users, relation_with_attrs_to_proplist(Users)},
        {groups, relation_with_attrs_to_proplist(Groups)},

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
        {eff_shares, []}, % TODO currently always empty
        {eff_providers, []},
        {eff_handle_services, []},
        {eff_handles, []},

        {public_only, true}
    ]}];

get_public_msg(Seq, Doc, od_provider = Model) ->
    #document{key = Id, value = #od_provider{
        name = Name, urls = URLs,
        latitude = Latitude, longitude = Longitude
    }} = Doc,
    [{seq, Seq}, revs_prop(Doc), {id, Id}, {message_model(Model), [
        {client_name, Name},
        {urls, URLs},
        {latitude, Latitude},
        {longitude, Longitude},

        {spaces, []},

        {public_only, true}
    ]}].

-spec revs_prop(Doc :: datastore:document()) -> term().
revs_prop(#document{rev = Revs}) when is_list(Revs) ->
    {ok, MaxSize} = application:get_env(?APP_NAME, subscriptions_sent_revisions_limit),
    Size = min(MaxSize, length(Revs)),
    {revs, lists:sublist(Revs, Size)}.

-spec message_model(subscriptions:model()) -> atom().
message_model(od_space) -> od_space;
message_model(od_share) -> od_share;
message_model(od_provider) -> od_provider;
message_model(od_user) -> od_user;
message_model(od_group) -> od_group;
message_model(od_handle) -> od_handle;
message_model(od_handle_service) -> od_handle_service.


% TODO VFS-2918
calculate_space_aliases(SpaceIds, SpaceAliases) ->
    SpaceNames = maps:from_list(lists:map(
        fun(SpaceId) ->
            {ok, #document{
                value = #od_space{name = Name}
            }} = od_space:get(SpaceId),
            {SpaceId, Name}
        end, SpaceIds)),
    % Overwrite names with existing aliases
    SpaceNamesMerged = maps:merge(SpaceNames, SpaceAliases),
    AllNames = maps:values(SpaceNamesMerged),
    % Duplicated names should get the id of space concatenated
    UniqueNames = maps:map(
        fun(SpId, SpName) ->
            case lists:member(SpName, AllNames -- [SpName]) of
                false ->
                    SpName;
                true ->
                    <<SpName/binary, "#", (binary:part(SpId, 0, 8))/binary>>
            end
        end, SpaceNamesMerged),
    UniqueNames.


serialize_linked_accounts(LinkedAccounts) ->
    lists:map(fun(Account) ->
        ?record_to_list(linked_account, Account)
    end, LinkedAccounts).


% TODO VFS-2918
relation_to_proplist(Map) ->
    maps:keys(Map).


% TODO VFS-2918
relation_with_attrs_to_proplist(Map) ->
    maps:to_list(Map).


% TODO VFS-2918
eff_relation_to_proplist(Map) ->
    maps:keys(Map).


% TODO VFS-2918
eff_relation_with_attrs_to_proplist(Map) ->
    maps:to_list(maps:map(
        fun(_K, {Attrs, _}) ->
            Attrs
        end, Map)).
