%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module implement the business logic for OZ API privileges.
%%% This module serves as a buffer between the database and the REST API.
%%% @end
%%%-------------------------------------------------------------------
-module(oz_api_privileges_logic).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models_def.hrl").

-export([get/2, modify/3, remove/2, has_effective_privilege/2, resolve_id/2]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns onezone_api_privileges for given user/group by their ID.
%% If such record does not exist, an empty list of privileges is returned.
%% @end
%%--------------------------------------------------------------------
-spec get(EntityId :: binary(), EntityType :: onedata_user | user_group) ->
    {ok, [oz_api_privileges:privilege()]}.
get(EntityId, EntityType) ->
    Key = resolve_id(EntityId, EntityType),
    Privs = case oz_api_privileges:get(Key) of
        {error, {not_found, oz_api_privileges}} ->
            [];
        {ok, #document{value = #oz_api_privileges{privileges = Privileges}}} ->
            Privileges
    end,
    {ok, Privs}.


%%--------------------------------------------------------------------
%% @doc
%% Modifies privileges of given entity (user/group).
%% If such record does not exist, it is created.
%% If provided privileges are empty, the record (if exists) is deleted.
%% If user/group with given ID does not exists, an error is returned.
%% @end
%%--------------------------------------------------------------------
-spec modify(EntityId :: binary(), EntityType,
    NewPrivileges :: [oz_api_privileges:privilege()]) ->
    ok | {error, {not_found, EntityType}}
    when EntityType :: oz_api_privileges:entity_type().
modify(EntityId, EntityType, NewPrivileges) ->
    case entity_exists(EntityId, EntityType) of
        false ->
            {error, {not_found, EntityType}};
        true ->
            case NewPrivileges of
                [] ->
                    % Empty privileges, delete the record (if present)
                    true = remove(EntityId, EntityType);
                _ ->
                    Key = resolve_id(EntityId, EntityType),
                    Doc = #document{
                        key = Key, value = #oz_api_privileges{
                            privileges = NewPrivileges
                        }},
                    UpdateFun = fun(OZPrivileges = #oz_api_privileges{}) ->
                        {ok, OZPrivileges#oz_api_privileges{
                            privileges = NewPrivileges
                        }}
                    end,
                    {ok, Key} =
                        oz_api_privileges:create_or_update(Doc, UpdateFun)
            end,
            ok
    end.


%%--------------------------------------------------------------------
%% @doc
%% Removes all privileges of given entity by deleting the corresponding
%% oz_api_privileges record (succeeds as well when the record is not present).
%% @end
%%--------------------------------------------------------------------
-spec remove(EntityId :: binary(), EntityType :: onedata_user | user_group) ->
    boolean().
remove(EntityId, EntityType) ->
    Key = resolve_id(EntityId, EntityType),
    ok = oz_api_privileges:delete(Key),
    true.


%%--------------------------------------------------------------------
%% @doc
%% Checks if given user has a given privilege to onezone API. Check direct
%% privileges of the user and privileges of all his effective groups.
%% @end
%%--------------------------------------------------------------------
-spec has_effective_privilege(UserId :: binary(),
    Privilege :: oz_api_privileges:privilege()) -> boolean().
has_effective_privilege(UserId, Privilege) ->
    {ok, UserPrivileges} = get(UserId, onedata_user),
    case lists:member(Privilege, UserPrivileges) of
        true ->
            % User has this privilege, return true
            true;
        false ->
            % Check all of user's effective groups if they have the privilege
            % @TODO this is not optimal!
            % @TODO there should be a view or a cache that could quickly
            % return effective user privileges to OZ API. Maybe the mechanism
            % for effective privileges from spaces / groups can be used?
            {ok, [{effective_groups, UserGroups}]} =
                user_logic:get_effective_groups(UserId),
            lists:foldl(
                fun(GroupId, Acc) ->
                    case Acc of
                        true ->
                            true;
                        false ->
                            {ok, GroupPrivileges} = get(GroupId, user_group),
                            lists:member(Privilege, GroupPrivileges)
                    end
                end, false, UserGroups)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns record key based on entity ID and type (onedata_user/user_group).
%% @end
%%--------------------------------------------------------------------
-spec resolve_id(EntityId :: binary(),
    EntityType :: oz_api_privileges:entity_type()) -> binary().
resolve_id(EntityId, EntityType) ->
    case EntityType of
        onedata_user ->
            <<"user:", EntityId/binary>>;
        user_group ->
            <<"group:", EntityId/binary>>
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @private
%% Checks if entity of given ID and type exists in the system.
%% @end
%%--------------------------------------------------------------------
-spec entity_exists(EntityId :: binary(),
    EntityType :: onedata_user | user_group) -> boolean().
entity_exists(EntityId, onedata_user) ->
    user_logic:exists(EntityId);
entity_exists(EntityId, user_group) ->
    group_logic:exists(EntityId).

