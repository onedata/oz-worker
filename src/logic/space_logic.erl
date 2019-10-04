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
-module(space_logic).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

-export([
    create/2
]).
-export([
    get/2,
    get_protected_data/2,
    list/1,
    list_privileges/0
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
    create_group/3, create_group/4,
    
    join_harvester/3,
    harvest_metadata/3, harvest_metadata/6,

    get_users/2, get_eff_users/2,
    get_user/3, get_eff_user/3,
    get_user_privileges/3, get_eff_user_privileges/3,
    get_eff_user_membership_intermediaries/3,

    get_groups/2, get_eff_groups/2,
    get_group/3, get_eff_group/3,
    get_group_privileges/3, get_eff_group_privileges/3,
    get_eff_group_membership_intermediaries/3,

    get_shares/2, get_share/3,

    get_providers/2, get_provider/3,

    get_harvesters/2, get_harvester/3,

    update_user_privileges/5, update_user_privileges/4,
    update_group_privileges/5, update_group_privileges/4,

    leave_provider/3,
    remove_harvester/3,

    remove_user/3,
    remove_group/3
]).
-export([
    exists/1,
    has_eff_privilege/3,
    has_direct_user/2,
    has_eff_user/2,
    has_eff_group/2,
    has_provider/2,
    has_harvester/2
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
-spec create(Auth :: aai:auth(), NameOrData :: binary() | #{}) ->
    {ok, od_space:id()} | {error, term()}.
create(Auth, Name) when is_binary(Name) ->
    create(Auth, #{<<"name">> => Name});
create(Auth, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_space, id = undefined, aspect = instance},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a space record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(Auth :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, #od_space{}} | {error, term()}.
get(Auth, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves protected space data from database.
%% @end
%%--------------------------------------------------------------------
-spec get_protected_data(Auth :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, map()} | {error, term()}.
get_protected_data(Auth, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = instance, scope = protected}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Lists all spaces (their ids) in database.
%% @end
%%--------------------------------------------------------------------
-spec list(Auth :: aai:auth()) ->
    {ok, [od_space:id()]} | {error, term()}.
list(Auth) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = undefined, aspect = list}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Get all possible space privileges.
%% @end
%%--------------------------------------------------------------------
-spec list_privileges() -> {ok, map()} | {error, term()}.
list_privileges() ->
    entity_logic:handle(#el_req{
        operation = get,
        gri = #gri{type = od_space, id = undefined, aspect = privileges}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given space (currently only name is supported).
%% Has two variants:
%% 1) Space Name is given explicitly
%% 2) Space name is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update(Auth :: aai:auth(), SpaceId :: od_space:id(),
    NameOrData :: binary() | #{}) -> ok | {error, term()}.
update(Auth, SpaceId, NewName) when is_binary(NewName) ->
    update(Auth, SpaceId, #{<<"name">> => NewName});
update(Auth, SpaceId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = instance},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given space from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(Auth :: aai:auth(), SpaceId :: od_space:id()) ->
    ok | {error, term()}.
delete(Auth, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a user invite token, which can be used by any user to join
%% given space.
%% @end
%%--------------------------------------------------------------------
-spec create_user_invite_token(Auth :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, tokens:token()} | {error, term()}.
create_user_invite_token(Auth, SpaceId) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = invite_user_token},
        data = #{}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a group invite token, which can be used by any group to join
%% given space.
%% @end
%%--------------------------------------------------------------------
-spec create_group_invite_token(Auth :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, tokens:token()} | {error, term()}.
create_group_invite_token(Auth, SpaceId) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = invite_group_token},
        data = #{}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a provider invite token (support token), which can be used by any
%% provider to grant support to given space.
%% @end
%%--------------------------------------------------------------------
-spec create_provider_invite_token(Auth :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, tokens:token()} | {error, term()}.
create_provider_invite_token(Auth, SpaceId) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = invite_provider_token},
        data = #{}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given space.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Auth :: aai:auth(),
    SpaceId :: od_space:id(), UserId :: od_user:id()) ->
    {ok, od_user:id()} | {error, term()}.
add_user(Auth, SpaceId, UserId) ->
    add_user(Auth, SpaceId, UserId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified user to given space.
%% Allows to specify the privileges of the newly added user. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_user(Auth :: aai:auth(),
    SpaceId :: od_space:id(), UserId :: od_user:id(),
    PrivilegesPrivilegesOrData :: [privileges:space_privilege()] | #{}) ->
    {ok, od_user:id()} | {error, term()}.
add_user(Auth, SpaceId, UserId, Privileges) when is_list(Privileges) ->
    add_user(Auth, SpaceId, UserId, #{
        <<"privileges">> => Privileges
    });
add_user(Auth, SpaceId, UserId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = {user, UserId}},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified group to given space.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Auth :: aai:auth(),
    SpaceId :: od_space:id(), GroupId :: od_group:id()) ->
    {ok, od_group:id()} | {error, term()}.
add_group(Auth, SpaceId, GroupId) ->
    add_group(Auth, SpaceId, GroupId, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Adds specified group to given space.
%% Allows to specify the privileges of the newly added group. Has two variants:
%% 1) Privileges are given explicitly
%% 2) Privileges are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec add_group(Auth :: aai:auth(),
    SpaceId :: od_space:id(), GroupId :: od_group:id(),
    PrivilegesOrData :: [privileges:space_privilege()] | #{}) ->
    {ok, od_group:id()} | {error, term()}.
add_group(Auth, SpaceId, GroupId, Privileges) when is_list(Privileges) ->
    add_group(Auth, SpaceId, GroupId, #{
        <<"privileges">> => Privileges
    });
add_group(Auth, SpaceId, GroupId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = {group, GroupId}},
        data = Data
    })).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new group in the space based on group name and type.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Auth :: aai:auth(), od_space:id(), od_group:name(),
    od_group:type()) -> {ok, od_group:id()} | {error, term()}.
create_group(Auth, SpaceId, Name, Type) ->
    create_group(Auth, SpaceId, #{<<"name">> => Name, <<"type">> => Type}).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new group in the space. Has two variants:
%% 1) Group Name is given explicitly (the new group will be of default type)
%% 2) Group name is provided in a proper Data object, group type is optional.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Auth :: aai:auth(), od_space:id(),
    NameOrData :: od_group:name() | #{}) -> {ok, od_group:id()} | {error, term()}.
create_group(Auth, SpaceId, Name) when is_binary(Name) ->
    create_group(Auth, SpaceId, #{<<"name">> => Name});
create_group(Auth, SpaceId, Data) ->
    AuthHint = case Auth of
        ?USER(UserId) -> ?AS_USER(UserId);
        _ -> undefined
    end,
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = group},
        data = Data,
        auth_hint = AuthHint
    })).


%%--------------------------------------------------------------------
%% @doc
%% Joins a harvester on behalf of given group based on harvester_invite_space token.
%% Has two variants:
%% 1) Token is given explicitly
%% 2) Token is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec join_harvester(Auth :: aai:auth(), SpaceId :: od_group:id(),
    TokenOrData :: tokens:serialized() | tokens:token() | map()) ->
    {ok, od_harvester:id()} | {error, term()}.
join_harvester(Auth, SpaceId, Data) when is_map(Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = undefined, aspect = join},
        auth_hint = ?AS_SPACE(SpaceId),
        data = Data
    }));
join_harvester(Auth, SpaceId, Token) ->
    join_harvester(Auth, SpaceId, #{<<"token">> => Token}).


%%--------------------------------------------------------------------
%% @doc
%% Submits given batch to harvesters given in Destination.
%% Destination, maxSeq and Batch are given explicitly.
%% @end
%%--------------------------------------------------------------------
-spec harvest_metadata(Auth :: aai:auth(), SpaceId :: od_space:id(),
    Destination :: map(), MaxStreamSeq :: integer(), MaxSeq :: non_neg_integer(), Batch :: map()) ->
    {ok, map()} | {error, term()}.
harvest_metadata(Auth, SpaceId, Destination, MaxStreamSeq, MaxSeq, Batch) ->
    harvest_metadata(Auth, SpaceId, #{
        <<"destination">> => Destination,
        <<"maxStreamSeq">> => MaxStreamSeq,
        <<"maxSeq">> => MaxSeq,
        <<"batch">> => Batch
    }).


%%--------------------------------------------------------------------
%% @doc
%% Submits given batch to harvesters given in Destination.
%% Destination, maxSeq and Batch are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec harvest_metadata(Auth :: aai:auth(), SpaceId :: od_space:id(),
    Data :: map()) -> {ok, map()} | {error, term()}.
harvest_metadata(Auth, SpaceId, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = harvest_metadata},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of users of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_users(Auth :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_users(Auth, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = users}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective users of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_users(Auth :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_eff_users(Auth, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = eff_users}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific user among users of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_user(Auth :: aai:auth(), SpaceId :: od_space:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_user(Auth, SpaceId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_SPACE(SpaceId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective user among
%% effective users of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user(Auth :: aai:auth(), SpaceId :: od_space:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_eff_user(Auth, SpaceId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_SPACE(SpaceId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific user among users of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(Auth :: aai:auth(), SpaceId :: od_space:id(),
    UserId :: od_user:id()) -> {ok, [privileges:space_privilege()]} | {error, term()}.
get_user_privileges(Auth, SpaceId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = {user_privileges, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective user
%% among effective users of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user_privileges(Auth :: aai:auth(), SpaceId :: od_space:id(),
    UserId :: od_user:id()) -> {ok, [privileges:space_privilege()]} | {error, term()}.
get_eff_user_privileges(Auth, SpaceId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = {eff_user_privileges, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the membership intermediaries of specific effective user
%% among effective users of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user_membership_intermediaries(Auth :: aai:auth(),
    SpaceId :: od_space:id(), UserId :: od_user:id()) ->
    {ok, entity_graph:intermediaries()} | {error, term()}.
get_eff_user_membership_intermediaries(Auth, SpaceId, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = {eff_user_membership, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of groups of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(Auth :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_groups(Auth, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = groups}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective groups of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_groups(Auth :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_eff_groups(Auth, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = eff_groups}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific group among groups of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_group(Auth :: aai:auth(), SpaceId :: od_space:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_group(Auth, SpaceId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_SPACE(SpaceId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective group among
%% effective groups of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group(Auth :: aai:auth(), SpaceId :: od_space:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_eff_group(Auth, SpaceId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_SPACE(SpaceId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the privileges of specific group among groups of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_group_privileges(Auth :: aai:auth(), SpaceId :: od_space:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:space_privilege()]} | {error, term()}.
get_group_privileges(Auth, SpaceId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = {group_privileges, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the effective privileges of specific effective group
%% among effective groups of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group_privileges(Auth :: aai:auth(), SpaceId :: od_space:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:space_privilege()]} | {error, term()}.
get_eff_group_privileges(Auth, SpaceId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = {eff_group_privileges, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the membership intermediaries of specific effective group
%% among effective groups of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group_membership_intermediaries(Auth :: aai:auth(),
    SpaceId :: od_space:id(), GroupId :: od_group:id()) ->
    {ok, entity_graph:intermediaries()} | {error, term()}.
get_eff_group_membership_intermediaries(Auth, SpaceId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = {eff_group_membership, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of shares of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_shares(Auth :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, [od_share:id()]} | {error, term()}.
get_shares(Auth, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = shares}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific share among shares of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_share(Auth :: aai:auth(), SpaceId :: od_space:id(),
    ShareId :: od_share:id()) -> {ok, #{}} | {error, term()}.
get_share(Auth, SpaceId, ShareId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_share, id = ShareId, aspect = instance, scope = private},
        auth_hint = ?THROUGH_SPACE(SpaceId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of providers of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_providers(Auth :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, [od_provider:id()]} | {error, term()}.
get_providers(Auth, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = providers}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific provider among providers of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_provider(Auth :: aai:auth(), SpaceId :: od_space:id(),
    ProviderId :: od_provider:id()) -> {ok, #{}} | {error, term()}.
get_provider(Auth, SpaceId, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_provider, id = ProviderId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_SPACE(SpaceId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of harvesters of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_harvesters(Auth :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, [od_harvester:id()]} | {error, term()}.
get_harvesters(Auth, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = harvesters}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific harvester among harvesters of given space.
%% @end
%%--------------------------------------------------------------------
-spec get_harvester(Auth :: aai:auth(), SpaceId :: od_space:id(),
    HarvesterId :: od_harvester:id()) -> {ok, #{}} | {error, term()}.
get_harvester(Auth, SpaceId, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_SPACE(SpaceId)
    }).


%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given space.
%% Allows to specify privileges to grant and to revoke.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Auth :: aai:auth(), SpaceId :: od_space:id(),
    UserId :: od_user:id(), PrivsToGrant :: [privileges:space_privilege()],
    PrivsToRevoke :: [privileges:space_privilege()]) -> ok | {error, term()}.
update_user_privileges(Auth, SpaceId, UserId, PrivsToGrant, PrivsToRevoke) ->
    update_user_privileges(Auth, SpaceId, UserId, #{
        <<"grant">> => PrivsToGrant,
        <<"revoke">> => PrivsToRevoke
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified user of given space.
%% Privileges to grant and revoke must be included in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_user_privileges(Auth :: aai:auth(), SpaceId :: od_space:id(),
    UserId :: od_user:id(), Data :: #{}) -> ok | {error, term()}.
update_user_privileges(Auth, SpaceId, UserId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = {user_privileges, UserId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified group of given space.
%% Allows to specify privileges to grant and to revoke.
%% @end
%%--------------------------------------------------------------------
-spec update_group_privileges(Auth :: aai:auth(), SpaceId :: od_space:id(),
    GroupId :: od_group:id(), PrivsToGrant :: [privileges:space_privilege()],
    PrivsToRevoke :: [privileges:space_privilege()]) -> ok | {error, term()}.
update_group_privileges(Auth, SpaceId, GroupId, PrivsToGrant, PrivsToRevoke) ->
    update_group_privileges(Auth, SpaceId, GroupId, #{
        <<"grant">> => PrivsToGrant,
        <<"revoke">> => PrivsToRevoke
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates privileges of specified group of given space.
%% Privileges to grant and revoke must be included in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_group_privileges(Auth :: aai:auth(), SpaceId :: od_space:id(),
    GroupId :: od_user:id(), Data :: #{}) -> ok | {error, term()}.
update_group_privileges(Auth, SpaceId, GroupId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = {group_privileges, GroupId}},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified provider (ceases support for given space).
%% @end
%%--------------------------------------------------------------------
-spec leave_provider(Auth :: aai:auth(), SpaceId :: od_space:id(),
    ProviderId :: od_provider:id()) -> ok | {error, term()}.
leave_provider(Auth, SpaceId, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = {provider, ProviderId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes space from specified harvester.
%% @end
%%--------------------------------------------------------------------
-spec remove_harvester(Auth :: aai:auth(), SpaceId :: od_space:id(),
    HarvesterId :: od_harvester:id()) -> ok | {error, term()}.
remove_harvester(Auth, SpaceId, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = {harvester, HarvesterId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified user from given space.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(Auth :: aai:auth(), SpaceId :: od_space:id(),
    UserId :: od_user:id()) -> ok | {error, term()}.
remove_user(Auth, SpaceId, UserId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = {user, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Removes specified group from given space.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(Auth :: aai:auth(), SpaceId :: od_space:id(),
    GroupId :: od_group:id()) -> ok | {error, term()}.
remove_group(Auth, SpaceId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = {group, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a space exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(SpaceId :: od_space:id()) -> boolean().
exists(SpaceId) ->
    {ok, Exists} = od_space:exists(SpaceId),
    Exists.


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified effective user has specified
%% effective privilege in given space.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_privilege(SpaceOrId :: od_space:id() | #od_space{},
    UserId :: od_user:id(), Privilege :: privileges:space_privilege()) ->
    boolean().
has_eff_privilege(SpaceId, UserId, Privilege) when is_binary(SpaceId) ->
    entity_graph:has_privilege(effective, bottom_up, od_user, UserId, Privilege, od_space, SpaceId);
has_eff_privilege(Space, UserId, Privilege) ->
    entity_graph:has_privilege(effective, bottom_up, od_user, UserId, Privilege, Space).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified user is an direct user of given space.
%% @end
%%--------------------------------------------------------------------
-spec has_direct_user(SpaceOrId :: od_space:id() | #od_space{},
    UserId :: od_user:id()) -> boolean().
has_direct_user(SpaceId, UserId) when is_binary(SpaceId) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, od_space, SpaceId);
has_direct_user(Space, UserId) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, Space).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified user is an effective user of given space.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_user(SpaceOrId :: od_space:id() | #od_space{},
    UserId :: od_user:id()) -> boolean().
has_eff_user(SpaceId, UserId) when is_binary(SpaceId) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, od_space, SpaceId);
has_eff_user(Space, UserId) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Space).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified group is an effective group of given space.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_group(SpaceOrId :: od_space:id() | #od_space{},
    GroupId :: od_group:id()) -> boolean().
has_eff_group(SpaceId, GroupId) when is_binary(SpaceId) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, od_space, SpaceId);
has_eff_group(Space, GroupId) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, Space).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified provider supports given space.
%% @end
%%--------------------------------------------------------------------
-spec has_provider(SpaceOrId :: od_space:id() | #od_space{},
    ProviderId :: od_provider:id()) -> boolean().
has_provider(SpaceId, ProviderId) when is_binary(SpaceId) ->
    entity_graph:has_relation(direct, top_down, od_provider, ProviderId, od_space, SpaceId);
has_provider(Space, ProviderId) ->
    entity_graph:has_relation(direct, top_down, od_provider, ProviderId, Space).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether given space is member of specified harvester.
%% @end
%%--------------------------------------------------------------------
-spec has_harvester(SpaceOrId :: od_space:id() | #od_space{},
    HarvesterId :: od_harvester:id()) -> boolean().
has_harvester(SpaceId, HarvesterId) when is_binary(SpaceId) ->
    entity_graph:has_relation(direct, bottom_up, od_harvester, HarvesterId, od_space, SpaceId);
has_harvester(Space, HarvesterId) ->
    entity_graph:has_relation(direct, bottom_up, od_harvester, HarvesterId, Space).
