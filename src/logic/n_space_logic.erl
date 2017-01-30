%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all space logic functionalities.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(n_space_logic).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-define(PLUGIN, n_space_logic_plugin).

-export([
    create/2
]).
-export([
    get/2,
    get_data/2,
    list/1
]).
-export([
    update/3
]).
-export([
    delete/2
]).
-export([
    create_user_invite_token/2,
    create_group_invite_token/2,
    create_provider_invite_token/2,

    add_user/3, add_user/4,
    add_group/3, add_group/4,

    get_users/2, get_eff_users/2,
    get_user/3, get_eff_user/3,
    get_user_privileges/3, get_eff_user_privileges/3,

    get_groups/2, get_eff_groups/2,
    get_group/3, get_eff_group/3,
    get_group_privileges/3, get_eff_group_privileges/3,

    get_shares/2, get_share/3,

    get_providers/2, get_provider/3,

    update_user_privileges/5, update_user_privileges/4,
    update_group_privileges/5, update_group_privileges/4,

    leave_provider/3,

    remove_user/3,
    remove_group/3
]).
-export([
    exists/1,
    has_eff_privilege/3,
    has_eff_user/2
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new space document in database. Has two variants:
%% 1) Space Name is given explicitly
%% 2) Space name is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: n_entity_logic:client(), NameOrData :: binary() | #{}) ->
    {ok, od_space:id()} | {error, term()}.
create(Client, Name) when is_binary(Name) ->
    create(Client, #{<<"name">> => Name});
create(Client, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, undefined, entity, Data).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a space record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(Client :: n_entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, #od_space{}} | {error, term()}.
get(Client, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, entity).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves information about a space record from database.
%% @end
%%--------------------------------------------------------------------
-spec get_data(Client :: n_entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, #{}} | {error, term()}.
get_data(Client, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, data).


%%--------------------------------------------------------------------
%% @doc
%% Lists all spaces (their ids) in database.
%% @end
%%--------------------------------------------------------------------
-spec list(Client :: n_entity_logic:client()) ->
    {ok, [od_space:id()]} | {error, term()}.
list(Client) ->
    n_entity_logic:get(Client, ?PLUGIN, undefined, list).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given space (currently only name is supported).
%% Has two variants:
%% 1) Space Name is given explicitly
%% 2) Space name is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    Data :: #{}) -> ok | {error, term()}.
update(Client, SpaceId, NewName) when is_binary(NewName) ->
    update(Client, SpaceId, #{<<"name">> => NewName});
update(Client, SpaceId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, SpaceId, entity, Data).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given space from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(Client :: n_entity_logic:client(), SpaceId :: od_space:id()) ->
    ok | {error, term()}.
delete(Client, SpaceId) ->
    n_entity_logic:delete(Client, ?PLUGIN, SpaceId, entity).


%%--------------------------------------------------------------------
%% @doc
%% Creates a user invite token, which can be used by any user to join
%% given space.
%% @end
%%--------------------------------------------------------------------
-spec create_user_invite_token(Client :: n_entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
create_user_invite_token(Client, SpaceId) ->
    n_entity_logic:create(Client, ?PLUGIN, SpaceId, invite_user_token, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Creates a group invite token, which can be used by any group to join
%% given space.
%% @end
%%--------------------------------------------------------------------
-spec create_group_invite_token(Client :: n_entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
create_group_invite_token(Client, SpaceId) ->
    n_entity_logic:create(Client, ?PLUGIN, SpaceId, invite_group_token, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Creates a provider invite token (support token), which can be used by any
%% provider to grant support to given space.
%% @end
%%--------------------------------------------------------------------
-spec create_provider_invite_token(Client :: n_entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
create_provider_invite_token(Client, SpaceId) ->
    n_entity_logic:create(Client, ?PLUGIN, SpaceId, invite_provider_token, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given space.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Client :: n_entity_logic:client(),
    SpaceId :: od_space:id(), UserId :: od_user:id()) ->
    {ok, od_user:id()} | {error, term()}.
add_user(Client, SpaceId, UserId) ->
    add_user(Client, SpaceId, UserId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given space.
%% Allows to specify the privileges of the newly added user. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Client :: n_entity_logic:client(),
    SpaceId :: od_space:id(), UserId :: od_user:id(),
    PrivilegesPrivilegesOrData :: [privileges:space_privileges()] | #{}) ->
    {ok, od_user:id()} | {error, term()}.
add_user(Client, SpaceId, UserId, Privileges) when is_list(Privileges) ->
    add_user(Client, SpaceId, UserId, #{
        <<"privileges">> => Privileges
    });
add_user(Client, SpaceId, UserId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, SpaceId, {user, UserId}, Data).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified group to given space.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Client :: n_entity_logic:client(),
    SpaceId :: od_space:id(), GroupId :: od_group:id()) ->
    {ok, od_group:id()} | {error, term()}.
add_group(Client, SpaceId, GroupId) ->
    add_group(Client, SpaceId, GroupId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified group to given space.
%% Allows to specify the privileges of the newly added group. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Client :: n_entity_logic:client(),
    SpaceId :: od_space:id(), GroupId :: od_group:id(),
    PrivilegesOrData :: [privileges:space_privileges()] | #{}) ->
    {ok, od_group:id()} | {error, term()}.
add_group(Client, SpaceId, GroupId, Privileges) when is_list(Privileges) ->
    add_group(Client, SpaceId, GroupId, #{
        <<"privileges">> => Privileges
    });
add_group(Client, SpaceId, GroupId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, SpaceId, {group, GroupId}, Data).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of users of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_users(Client :: n_entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_users(Client, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, users).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective users of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_users(Client :: n_entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_eff_users(Client, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, eff_users).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific user among users of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_user(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_user(Client, SpaceId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, {user, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective user among
%% effective users of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_eff_user(Client, SpaceId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, {eff_user, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific user among users of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    UserId :: od_user:id()) -> {ok, [privileges:space_privileges()]} | {error, term()}.
get_user_privileges(Client, SpaceId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, {user_privileges, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective user
%% among effective users of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user_privileges(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    UserId :: od_user:id()) -> {ok, [privileges:space_privileges()]} | {error, term()}.
get_eff_user_privileges(Client, SpaceId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, {eff_user_privileges, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of groups of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(Client :: n_entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_groups(Client, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, groups).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective groups of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_groups(Client :: n_entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_eff_groups(Client, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, eff_groups).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific group among groups of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_group(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_group(Client, SpaceId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, {group, GroupId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective group among
%% effective groups of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_eff_group(Client, SpaceId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, {eff_group, GroupId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific group among groups of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_group_privileges(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:space_privileges()]} | {error, term()}.
get_group_privileges(Client, SpaceId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, {group_privileges, GroupId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective group
%% among effective groups of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group_privileges(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:space_privileges()]} | {error, term()}.
get_eff_group_privileges(Client, SpaceId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, {eff_group_privileges, GroupId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of shares of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_shares(Client :: n_entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, [od_share:id()]} | {error, term()}.
get_shares(Client, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, shares).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific share among shares of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_share(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    ShareId :: od_share:id()) -> {ok, #{}} | {error, term()}.
get_share(Client, SpaceId, ShareId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, {share, ShareId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of providers of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_providers(Client :: n_entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, [od_provider:id()]} | {error, term()}.
get_providers(Client, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, providers).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific provider among providers of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_provider(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    ProviderId :: od_provider:id()) -> {ok, #{}} | {error, term()}.
get_provider(Client, SpaceId, ProviderId) ->
    n_entity_logic:get(Client, ?PLUGIN, SpaceId, {provider, ProviderId}).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given space.
%% Allows to specify operation (set | grant | revoke) and the privileges.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    UserId :: od_user:id(), Operation :: entity_graph:privileges_operation(),
    Privs :: [privileges:space_privilege()]) -> ok | {error, term()}.
update_user_privileges(Client, SpaceId, UserId, Operation, Privs) when is_list(Privs) ->
    update_user_privileges(Client, SpaceId, UserId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given space.
%% Privileges must be included in proper Data object, operation is optional.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    UserId :: od_user:id(), Data :: #{}) -> ok | {error, term()}.
update_user_privileges(Client, SpaceId, UserId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, SpaceId, {user_privileges, UserId}, Data).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified group of given space.
%% Allows to specify operation (set | grant | revoke) and the privileges.
%% @end
%%--------------------------------------------------------------------
-spec update_group_privileges(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    GroupId :: od_group:id(), Operation :: entity_graph:privileges_operation(),
    Privs :: [privileges:space_privilege()]) -> ok | {error, term()}.
update_group_privileges(Client, SpaceId, GroupId, Operation, Privs) when is_list(Privs) ->
    update_group_privileges(Client, SpaceId, GroupId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified group of given space.
%% Privileges must be included in proper Data object, operation is optional.
%% @end
%%--------------------------------------------------------------------
-spec update_group_privileges(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    GroupId :: od_user:id(), Data :: #{}) -> ok | {error, term()}.
update_group_privileges(Client, SpaceId, GroupId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, SpaceId, {group_privileges, GroupId}, Data).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified provider (ceases support for given space).
%% @end
%%--------------------------------------------------------------------
-spec leave_provider(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    ProviderId :: od_provider:id()) -> ok | {error, term()}.
leave_provider(Client, SpaceId, ProviderId) ->
    n_entity_logic:delete(Client, ?PLUGIN, SpaceId, {provider, ProviderId}).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified user from given space.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    UserId :: od_user:id()) -> ok | {error, term()}.
remove_user(Client, SpaceId, UserId) ->
    n_entity_logic:delete(Client, ?PLUGIN, SpaceId, {user, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified group from given space.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(Client :: n_entity_logic:client(), SpaceId :: od_space:id(),
    GroupId :: od_group:id()) -> ok | {error, term()}.
remove_group(Client, SpaceId, GroupId) ->
    n_entity_logic:delete(Client, ?PLUGIN, SpaceId, {group, GroupId}).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a space exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(SpaceId :: od_space:id()) -> boolean().
exists(SpaceId) ->
    od_space:exists(SpaceId).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified effective user has specified
%% effective privilege in given space.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_privilege(SpaceOrId :: od_space:id() | #od_space{},
    UserId :: od_user:id(), Privilege :: privileges:space_privileges()) ->
    boolean().
has_eff_privilege(SpaceId, UserId, Privilege) when is_binary(SpaceId) ->
    case od_space:get(SpaceId) of
        {ok, #document{value = Space}} ->
            has_eff_privilege(Space, UserId, Privilege);
        _ ->
            false
    end;
has_eff_privilege(#od_space{eff_users = UsersPrivileges}, UserId, Privilege) ->
    {UserPrivileges, _} = maps:get(UserId, UsersPrivileges, {[], []}),
    lists:member(Privilege, UserPrivileges).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified user is an effective user in given space.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_user(SpaceOrId :: od_space:id() | #od_space{},
    UserId :: od_user:id()) -> boolean().
has_eff_user(SpaceId, UserId) when is_binary(SpaceId) ->
    case od_space:get(SpaceId) of
        {ok, #document{value = Space}} ->
            has_eff_user(Space, UserId);
        _ ->
            false
    end;
has_eff_user(#od_space{eff_users = EffUsers}, UserId) ->
    maps:is_key(UserId, EffUsers).

