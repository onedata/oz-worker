%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all cluster logic functionalities.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_logic).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/logging.hrl").

-export([
    create_oneprovider_cluster/2,
    delete_oneprovider_cluster/1
]).
-export([
    get/2,
    get_protected_data/2,
    list/1
]).
-export([
    update_version_info/4,
    update_onepanel_proxy/3,
    update/3
]).
-export([
    create_user_invite_token/2,
    create_group_invite_token/2,

    add_user/3, add_user/4,
    add_group/3, add_group/4,
    create_group/3, create_group/4,

    get_users/2, get_eff_users/2,
    get_user/3, get_eff_user/3,
    get_user_privileges/3, get_eff_user_privileges/3,
    get_eff_user_membership_intermediaries/3,

    get_groups/2, get_eff_groups/2,
    get_group/3, get_eff_group/3,
    get_group_privileges/3, get_eff_group_privileges/3,
    get_eff_group_membership_intermediaries/3,

    update_user_privileges/5, update_user_privileges/4,
    update_group_privileges/5, update_group_privileges/4,

    remove_user/3,
    remove_group/3
]).
-export([
    exists/1,
    has_eff_privilege/3,
    has_eff_user/2,
    has_direct_user/2,
    has_eff_group/2,
    is_provider_cluster/2
]).
-export([
    set_up_oz_worker_service/0,
    get_onezone_cluster_id/0
]).
-export([
    get_domain/1,
    version_info_to_json/1,
    json_to_version_info/1
]).

-define(GUI_PACKAGE_PATH, oz_worker:get_env(gui_package_path)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Not available in REST/GS API - reserved for internal Onezone logic.
%% Creates a new cluster document in database and links it with given provider
%% and provider creator (first member of the cluster), if specified.
%% Cluster has the same Id as the provider.
%% @end
%%--------------------------------------------------------------------
-spec create_oneprovider_cluster(CreatorUserId :: undefined | od_user:id(), od_provider:id()) ->
    ok | no_return().
create_oneprovider_cluster(CreatorUserId, ProviderId) ->
    ClusterId = ProviderId,

    {ok, _} = od_cluster:create(#document{key = ClusterId, value = #od_cluster{
        type = ?ONEPROVIDER,
        creator = case CreatorUserId of
            undefined -> ?ROOT;
            _ -> ?USER(CreatorUserId)
        end
    }}),

    CreatorUserId /= undefined andalso
        add_user(?ROOT, ClusterId, CreatorUserId, privileges:cluster_admin()),

    gui_static:link_gui(onedata:service_shortname(?OP_WORKER), ClusterId, ?EMPTY_GUI_HASH),
    gui_static:link_gui(onedata:service_shortname(?OP_PANEL), ClusterId, ?EMPTY_GUI_HASH),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Not available in REST/GS API - reserved for internal Onezone logic.
%% Deletes a cluster document linked with given provider from database and
%% cleans static GUI links.
%% Cluster has the same Id as the provider.
%% NOTE: all relations to member users/groups should be removed beforehand.
%% @end
%%--------------------------------------------------------------------
-spec delete_oneprovider_cluster(od_provider:id()) -> ok | no_return().
delete_oneprovider_cluster(ProviderId) ->
    ClusterId = ProviderId,

    entity_graph:delete_entity(od_cluster, ClusterId),

    gui_static:unlink_gui(onedata:service_shortname(?OP_WORKER), ClusterId),
    gui_static:unlink_gui(onedata:service_shortname(?OP_PANEL), ClusterId),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a cluster record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(Client :: entity_logic:client(), ClusterId :: od_cluster:id()) ->
    {ok, #od_cluster{}} | {error, term()}.
get(Client, ClusterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves protected cluster data from database.
%% @end
%%--------------------------------------------------------------------
-spec get_protected_data(Client :: entity_logic:client(), ClusterId :: od_cluster:id()) ->
    {ok, maps:map()} | {error, term()}.
get_protected_data(Client, ClusterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = instance, scope = protected}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Lists all clusters (their ids) in database.
%% @end
%%--------------------------------------------------------------------
-spec list(Client :: entity_logic:client()) ->
    {ok, [od_cluster:id()]} | {error, term()}.
list(Client) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_cluster, id = undefined, aspect = list}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates the version info of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec update_version_info(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    onedata:service_type(), VersionInfo :: od_cluster:version_info()) ->
    ok | {error, term()}.
update_version_info(Client, ClusterId, ?WORKER, {Release, Build, GuiHash}) ->
    update(Client, ClusterId, #{<<"workerVersion">> => #{
        <<"release">> => Release,
        <<"build">> => Build,
        <<"gui">> => GuiHash
    }});
update_version_info(Client, ClusterId, ?ONEPANEL, {Release, Build, GuiHash}) ->
    update(Client, ClusterId, #{<<"onepanelVersion">> => #{
        <<"release">> => Release,
        <<"build">> => Build,
        <<"gui">> => GuiHash
    }}).


%%--------------------------------------------------------------------
%% @doc
%% Toggles onepanelProxy on/off in given cluster.
%% @end
%%--------------------------------------------------------------------
-spec update_onepanel_proxy(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    OnepanelProxy :: boolean()) -> ok | {error, term()}.
update_onepanel_proxy(Client, ClusterId, OnepanelProxy) when is_boolean(OnepanelProxy) ->
    update(Client, ClusterId, #{<<"onepanelProxy">> => OnepanelProxy}).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given cluster. Data should contain at least one of:
%%  * <<"workerVersion">>
%%  * <<"onepanelVersion">>
%%  * <<"onepanelProxy">>
%% @end
%%--------------------------------------------------------------------
-spec update(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    Data :: #{}) -> ok | {error, term()}.
update(Client, ClusterId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = instance},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a user invite token, which can be used by any user to join
%% given cluster.
%% @end
%%--------------------------------------------------------------------
-spec create_user_invite_token(Client :: entity_logic:client(), ClusterId :: od_cluster:id()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
create_user_invite_token(Client, ClusterId) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = invite_user_token},
        data = #{}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a group invite token, which can be used by any group to join
%% given cluster.
%% @end
%%--------------------------------------------------------------------
-spec create_group_invite_token(Client :: entity_logic:client(), ClusterId :: od_cluster:id()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
create_group_invite_token(Client, ClusterId) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = invite_group_token},
        data = #{}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given cluster with default member privileges.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Client :: entity_logic:client(),
    ClusterId :: od_cluster:id(), UserId :: od_user:id()) ->
    {ok, od_user:id()} | {error, term()}.
add_user(Client, ClusterId, UserId) ->
    add_user(Client, ClusterId, UserId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given cluster.
%% Allows to specify the privileges of the newly added user. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Client :: entity_logic:client(),
    ClusterId :: od_cluster:id(), UserId :: od_user:id(),
    PrivilegesPrivilegesOrData :: [privileges:cluster_privileges()] | #{}) ->
    {ok, od_user:id()} | {error, term()}.
add_user(Client, ClusterId, UserId, Privileges) when is_list(Privileges) ->
    add_user(Client, ClusterId, UserId, #{
        <<"privileges">> => Privileges
    });
add_user(Client, ClusterId, UserId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = {user, UserId}},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified group to given cluster.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Client :: entity_logic:client(),
    ClusterId :: od_cluster:id(), GroupId :: od_group:id()) ->
    {ok, od_group:id()} | {error, term()}.
add_group(Client, ClusterId, GroupId) ->
    add_group(Client, ClusterId, GroupId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified group to given cluster.
%% Allows to specify the privileges of the newly added group. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Client :: entity_logic:client(),
    ClusterId :: od_cluster:id(), GroupId :: od_group:id(),
    PrivilegesOrData :: [privileges:cluster_privileges()] | #{}) ->
    {ok, od_group:id()} | {error, term()}.
add_group(Client, ClusterId, GroupId, Privileges) when is_list(Privileges) ->
    add_group(Client, ClusterId, GroupId, #{
        <<"privileges">> => Privileges
    });
add_group(Client, ClusterId, GroupId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = {group, GroupId}},
        data = Data
    })).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new group in the cluster based on group name and type.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Client :: entity_logic:client(), od_cluster:id(), od_group:name(),
    od_group:type()) -> {ok, od_group:id()} | {error, term()}.
create_group(Client, ClusterId, Name, Type) ->
    create_group(Client, ClusterId, #{<<"name">> => Name, <<"type">> => Type}).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new group in the cluster. Has two variants:
%% 1) Group Name is given explicitly (the new group will be of default type)
%% 2) Group name is provided in a proper Data object, group type is optional.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Client :: entity_logic:client(), od_cluster:id(),
    NameOrData :: od_group:name() | #{}) -> {ok, od_group:id()} | {error, term()}.
create_group(Client, ClusterId, Name) when is_binary(Name) ->
    create_group(Client, ClusterId, #{<<"name">> => Name});
create_group(Client, ClusterId, Data) ->
    AuthHint = case Client of
        ?USER(UserId) -> ?AS_USER(UserId);
        _ -> undefined
    end,
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = group},
        data = Data,
        auth_hint = AuthHint
    })).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of users of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_users(Client :: entity_logic:client(), ClusterId :: od_cluster:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_users(Client, ClusterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = users}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective users of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_users(Client :: entity_logic:client(), ClusterId :: od_cluster:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_eff_users(Client, ClusterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = eff_users}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific user among users of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_user(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_user(Client, ClusterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_CLUSTER(ClusterId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective user among
%% effective users of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_eff_user(Client, ClusterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_CLUSTER(ClusterId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific user among users of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    UserId :: od_user:id()) -> {ok, [privileges:cluster_privileges()]} | {error, term()}.
get_user_privileges(Client, ClusterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = {user_privileges, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective user
%% among effective users of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user_privileges(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    UserId :: od_user:id()) -> {ok, [privileges:cluster_privileges()]} | {error, term()}.
get_eff_user_privileges(Client, ClusterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = {eff_user_privileges, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the membership intermediaries of specific effective user
%% among effective users of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user_membership_intermediaries(Client :: entity_logic:client(),
    ClusterId :: od_cluster:id(), UserId :: od_user:id()) ->
    {ok, entity_graph:intermediaries()} | {error, term()}.
get_eff_user_membership_intermediaries(Client, ClusterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = {eff_user_membership, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of groups of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(Client :: entity_logic:client(), ClusterId :: od_cluster:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_groups(Client, ClusterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = groups}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective groups of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_groups(Client :: entity_logic:client(), ClusterId :: od_cluster:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_eff_groups(Client, ClusterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = eff_groups}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific group among groups of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_group(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_group(Client, ClusterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_CLUSTER(ClusterId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective group among
%% effective groups of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_eff_group(Client, ClusterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_CLUSTER(ClusterId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific group among groups of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_group_privileges(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:cluster_privileges()]} | {error, term()}.
get_group_privileges(Client, ClusterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = {group_privileges, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective group
%% among effective groups of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group_privileges(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:cluster_privileges()]} | {error, term()}.
get_eff_group_privileges(Client, ClusterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = {eff_group_privileges, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the membership intermediaries of specific effective group
%% among effective groups of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group_membership_intermediaries(Client :: entity_logic:client(),
    ClusterId :: od_cluster:id(), GroupId :: od_group:id()) ->
    {ok, entity_graph:intermediaries()} | {error, term()}.
get_eff_group_membership_intermediaries(Client, ClusterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = {eff_group_membership, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given cluster.
%% Allows to specify privileges to grant and to revoke.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    UserId :: od_user:id(), PrivsToGrant :: [privileges:cluster_privilege()],
    PrivsToRevoke :: [privileges:cluster_privilege()]) -> ok | {error, term()}.
update_user_privileges(Client, ClusterId, UserId, PrivsToGrant, PrivsToRevoke) ->
    update_user_privileges(Client, ClusterId, UserId, #{
        <<"grant">> => PrivsToGrant,
        <<"revoke">> => PrivsToRevoke
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given cluster.
%% Privileges to grant and revoke must be included in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    UserId :: od_user:id(), Data :: #{}) -> ok | {error, term()}.
update_user_privileges(Client, ClusterId, UserId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = {user_privileges, UserId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified group of given cluster.
%% Allows to specify privileges to grant and to revoke.
%% @end
%%--------------------------------------------------------------------
-spec update_group_privileges(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    GroupId :: od_group:id(), PrivsToGrant :: [privileges:cluster_privilege()],
    PrivsToRevoke :: [privileges:cluster_privilege()]) -> ok | {error, term()}.
update_group_privileges(Client, ClusterId, GroupId, PrivsToGrant, PrivsToRevoke) ->
    update_group_privileges(Client, ClusterId, GroupId, #{
        <<"grant">> => PrivsToGrant,
        <<"revoke">> => PrivsToRevoke
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified group of given cluster.
%% Privileges to grant and revoke must be included in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_group_privileges(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    GroupId :: od_user:id(), Data :: #{}) -> ok | {error, term()}.
update_group_privileges(Client, ClusterId, GroupId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = {group_privileges, GroupId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified user from given cluster.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    UserId :: od_user:id()) -> ok | {error, term()}.
remove_user(Client, ClusterId, UserId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = {user, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified group from given cluster.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(Client :: entity_logic:client(), ClusterId :: od_cluster:id(),
    GroupId :: od_group:id()) -> ok | {error, term()}.
remove_group(Client, ClusterId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        client = Client,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = {group, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a cluster exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(ClusterId :: od_cluster:id()) -> boolean().
exists(ClusterId) ->
    {ok, Exists} = od_cluster:exists(ClusterId),
    Exists.


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified effective user has specified
%% effective privilege in given cluster.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_privilege(ClusterOrId :: od_cluster:id() | #od_cluster{},
    UserId :: od_user:id(), Privilege :: privileges:cluster_privileges()) ->
    boolean().
has_eff_privilege(ClusterId, UserId, Privilege) when is_binary(ClusterId) ->
    entity_graph:has_privilege(effective, bottom_up, od_user, UserId, Privilege, od_cluster, ClusterId);
has_eff_privilege(Cluster, UserId, Privilege) ->
    entity_graph:has_privilege(effective, bottom_up, od_user, UserId, Privilege, Cluster).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified user is an effective user of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_user(ClusterOrId :: od_cluster:id() | #od_cluster{},
    UserId :: od_user:id()) -> boolean().
has_eff_user(ClusterId, UserId) when is_binary(ClusterId) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, od_cluster, ClusterId);
has_eff_user(Cluster, UserId) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Cluster).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified user is a direct member of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec has_direct_user(ClusterOrId :: od_cluster:id() | #od_cluster{},
    UserId :: od_cluster:id()) -> boolean().
has_direct_user(ClusterId, UserId) when is_binary(ClusterId) ->
    case od_cluster:get(ClusterId) of
        {ok, #document{value = Cluster}} ->
            has_direct_user(Cluster, UserId);
        _ ->
            false
    end;
has_direct_user(#od_cluster{users = Users}, UserId) ->
    maps:is_key(UserId, Users).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified group is an effective group of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_group(ClusterOrId :: od_cluster:id() | #od_cluster{},
    GroupId :: od_group:id()) -> boolean().
has_eff_group(ClusterId, GroupId) when is_binary(ClusterId) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, od_cluster, ClusterId);
has_eff_group(Cluster, GroupId) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, Cluster).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether given cluster is a cluster of given provider.
%% @end
%%--------------------------------------------------------------------
-spec is_provider_cluster(od_cluster:id(), od_provider:id()) -> boolean().
is_provider_cluster(ClusterId, ProviderId) ->
    ClusterId =:= ProviderId.


%%--------------------------------------------------------------------
%% @doc
%% Creates a new onezone cluster singleton if it does not exist and sets up
%% oz_worker GUI.
%% @end
%%--------------------------------------------------------------------
-spec set_up_oz_worker_service() -> ok.
set_up_oz_worker_service() ->
    ?info("Setting up Onezone worker service"),
    ok = od_cluster:ensure_onezone_cluster(),
    {ok, GuiHash} = gui:package_hash(?GUI_PACKAGE_PATH),
    ok = gui_static:deploy_package(onedata:service_shortname(?OZ_WORKER), ?GUI_PACKAGE_PATH),
    ok = update_version_info(?ROOT, ?ONEZONE_CLUSTER_ID, ?WORKER, {
        oz_worker:get_version(),
        oz_worker:get_build_version(),
        GuiHash
    }),
    ?info("Onezone worker service successfully set up").


%%--------------------------------------------------------------------
%% @doc
%% Return Onezone's cluster singleton Id.
%% @end
%%--------------------------------------------------------------------
-spec get_onezone_cluster_id() -> od_cluster:id().
get_onezone_cluster_id() ->
    ?ONEZONE_CLUSTER_ID.


%%--------------------------------------------------------------------
%% @doc
%% Return the domain of given cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_domain(od_cluster:id()) -> {ok, binary()} | {error, term()}.
get_domain(?ONEZONE_CLUSTER_ID) ->
    {ok, oz_worker:get_domain()};
get_domain(ProviderId) ->
    case od_provider:get(ProviderId) of
        {ok, #document{value = #od_provider{domain = Domain}}} -> {ok, Domain};
        {error, _} = Error -> Error
    end.


-spec version_info_to_json(od_cluster:version_info()) -> maps:map().
version_info_to_json({Release, Build, GuiHash}) ->
    #{
        <<"release">> => Release,
        <<"build">> => Build,
        <<"gui">> => GuiHash
    }.


-spec json_to_version_info(maps:map()) -> od_cluster:version_info().
json_to_version_info(Data) ->
    Release = maps:get(<<"release">>, Data),
    Build = maps:get(<<"build">>, Data),
    GuiHash = maps:get(<<"gui">>, Data),
    {Release, Build, GuiHash}.
