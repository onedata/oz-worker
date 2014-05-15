%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The module implementing the business logic for spaces in the registry.
%% This module serves as a buffer between the database and the REST API.
%% @end
%% ===================================================================
-module(space_logic).
-author("Konrad Zemek").


%% API
-export([exists/1, has_provider/2, has_user/2, has_group/2, has_privilege/3]).
-export([create/2, create/3, modify/2, set_privileges/3, join/2, support/2]).
-export([get_data/2, get_users/2, get_groups/1, get_providers/2, get_user/3,
    get_group/2, get_provider/3, get_privileges/2]).
-export([remove/1, remove_user/2, remove_group/2, remove_provider/2]).


%% exists/1
%% ====================================================================
%% @doc Returns whether a Space exists.
%% ====================================================================
-spec exists(SpaceId :: binary()) -> boolean().
%% ====================================================================
exists(SpaceId) ->
    true.


%% has_provider/2
%% ====================================================================
%% @doc Returns whether the provider identified by ProviderId supports the
%% Space. Shall return false in any other case (Space doesn't exist, etc).
%% @end
%% ====================================================================
-spec has_provider(SpaceId :: binary(), ProviderId :: binary()) -> boolean().
%% ====================================================================
has_provider(SpaceId, ProviderId) ->
    true.


%% has_user/2
%% ====================================================================
%% @doc Returns whether the user identified by UserId is a member of the Space.
%% Shall return false in any other case (Space doesn't exist, etc).
%% @end
%% ====================================================================
-spec has_user(SpaceId :: binary(), UserId :: binary()) -> boolean().
%% ====================================================================
has_user(SpaceId, UserId) ->
    true.


%% has_group/2
%% ====================================================================
%% @doc Returns whether the group identified by GroupId is a member of the
%% Space. Shall return false in any other case (Space doesn't exist, etc).
%% @end
%% ====================================================================
-spec has_group(SpaceId :: binary(), GroupId :: binary()) -> boolean().
%% ====================================================================
has_group(SpaceId, GroupId) ->
    true.


%% has_privilege/3
%% ====================================================================
%% @doc Returns whether the Space's user identified by UserId has privilege
%% in the Space. Shall return false in any other case (Space doesn't exist,
%% user is not Space's member, etc).
%% @end
%% ====================================================================
-spec has_privilege(SpaceId :: binary(), UserId :: binary(),
    Privilege :: privileges:space_privilege()) -> boolean().
%% ====================================================================
has_privilege(SpaceId, UserId, Privilege) ->
    true.


%% create/2
%% ====================================================================
%% @doc Creates a Space for a user.
%% ====================================================================
-spec create({user | group, Id :: binary()}, Name :: binary()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: any()}.
%% ====================================================================
create({Type, Id}, Name) ->
    {ok, <<"new_space_id">>}.


%% create/3
%% ====================================================================
%% @doc Creates a Space for a user, by a provider that will support it.
%% ====================================================================
-spec create({provider, ProviderId :: binary()}, Name :: binary(),
             Token :: binary()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: any()}.
%% ====================================================================
create({provider, ProviderId}, Name, Token) ->
    {ok, <<"new_supported_space_id">>}.


%% modify/2
%% ====================================================================
%% @doc Modifies Space's data.
%% ====================================================================
-spec modify(SpaceId :: binary(), Name :: binary()) ->
    ok | {error, Reason :: any()}.
%% ====================================================================
modify(SpaceId, Name) ->
    ok.


%% set_privileges/3
%% ====================================================================
%% @doc Sets privileges for a member of the Space.
%% ====================================================================
-spec set_privileges(SpaceId :: binary(), {user | group, Id :: binary()},
                     Privileges :: [privileges:space_privilege()]) ->
    ok | {error, Reason :: any()}.
%% ====================================================================
set_privileges(SpaceId, {user, UserId}, Privileges) ->
    ok;
set_privileges(SpaceId, {group, GroupId}, Privileges) ->
    ok.


%% join/2
%% ====================================================================
%% @doc Adds a new member to a Space identified by a token.
%% ====================================================================
-spec join({group | user, Id :: binary()}, Token :: binary()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: any()}.
%% ====================================================================
join(Member, Token) ->
    {ok, <<"the space you joined">>}.


%% support/2
%% ====================================================================
%% @doc Adds a new supporting provider to a Space identified by a token.
%% ====================================================================
-spec support(ProviderId :: binary(), Token :: binary()) ->
    {ok, SpaceId :: binary()} | {error, Reason :: any()}.
%% ====================================================================
support(Member, Token) ->
    {ok, <<"the space you joined">>}.


%% get_data/2
%% ====================================================================
%% @doc Returns details about the Space.
%% ====================================================================
-spec get_data(SpaceId :: binary(), Client :: user | provider) ->
    {ok, [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_data(SpaceId, Client) ->
    {ok, [{name, <<"spacename">>}]}.


%% get_users/2
%% ====================================================================
%% @doc Returns details about Space's users.
%% ====================================================================
-spec get_users(SpaceId :: binary(), Client :: user | provider) ->
    {ok, [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_users(SpaceId, Client) ->
    {ok, [{users, [<<"user">>]}]}.


%% get_groups/1
%% ====================================================================
%% @doc Returns details about Space's groups.
%% ====================================================================
-spec get_groups(SpaceId :: binary()) ->
    {ok, [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_groups(SpaceId) ->
    {ok, [{groups, [<<"group">>]}]}.


%% get_providers/2
%% ====================================================================
%% @doc Returns details about Space's providers.
%% ====================================================================
-spec get_providers(SpaceId :: binary(), Client :: user | provider) ->
    {ok, [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_providers(SpaceId, Client) ->
    {ok, [{providers, [<<"providers">>]}]}.


%% get_user/3
%% ====================================================================
%% @doc Returns details about Space's user.
%% ====================================================================
-spec get_user(SpaceId :: binary(), Client :: user | provider,
               UserId :: binary()) ->
    {ok, [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_user(SpaceId, Client, UserId) ->
    {ok, [{name, [<<"name">>]}]}.


%% get_group/2
%% ====================================================================
%% @doc Returns details about Space's group.
%% ====================================================================
-spec get_group(SpaceId :: binary(), GroupId :: binary()) ->
    {ok, [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_group(SpaceId, GroupId) ->
    {ok, [{name, [<<"name">>]}]}.


%% get_provider/3
%% ====================================================================
%% @doc Returns details about Space's provider.
%% ====================================================================
-spec get_provider(SpaceId :: binary(), Client :: user | provider,
                   UserId :: binary()) ->
    {ok, [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_provider(SpaceId, Client, ProviderId) ->
    {ok, [{name, [<<"name">>]}]}.


%% get_privileges/2
%% ====================================================================
%% @doc Returns list of Space's member privileges.
%% ====================================================================
-spec get_privileges(SpaceId :: binary(), {user | group, Id :: binary()}) ->
    {ok, [privileges:space_privilege()]} | {error, Reason :: any()}.
%% ====================================================================
get_privileges(SpaceId, {MemberType, Id}) ->
    sets:to_list(privileges:space_admin()).


%% remove/1
%% ====================================================================
%% @doc Removes the Space. Should return true if after the call the Space
%% doesn't exist; in particular if it never existed at all.
%% @end
%% ====================================================================
-spec remove(SpaceId :: binary()) -> boolean().
%% ====================================================================
remove(SpaceId) ->
    true.


%% remove_user/2
%% ====================================================================
%% @doc Removes user from the Space. Should return true if after the call the
%% user will no longer be a member of the Space; in particular if he never
%% was a member or the Space didn't exist.
%% @end
%% ====================================================================
-spec remove_user(SpaceId :: binary(), UserId :: binary()) -> boolean().
%% ====================================================================
remove_user(SpaceId, UserId) ->
    true.


%% remove_group/2
%% ====================================================================
%% @doc Removes group from the Space. Should return true if after the call the
%% group will no longer be a member of the Space; in particular if it never
%% was a member or the Space didn't exist.
%% @end
%% ====================================================================
-spec remove_group(SpaceId :: binary(), GroupId :: binary()) -> boolean().
%% ====================================================================
remove_group(SpaceId, GroupId) ->
    true.


%% remove_provider/2
%% ====================================================================
%% @doc Removes provider from the Space. Should return true if after the call
%% the provider will no longer support the Space; in particular if he never
%% supported it or the Space didn't exist.
%% @end
%% ====================================================================
-spec remove_provider(SpaceId :: binary(), ProviderId :: binary()) -> boolean().
%% ====================================================================
remove_provider(SpaceId, ProviderId) ->
    true.
