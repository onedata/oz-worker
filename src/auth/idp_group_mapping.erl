%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles mapping user group memberships coming from external
%%% Identity Providers onto onedata groups.
%%% This modules operates on standardized groups and membership identifiers
%%% (see the type specs).
%%% @end
%%%-------------------------------------------------------------------
-module(idp_group_mapping).

-include("auth_common.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("idp_group_mapping.hrl").
-include_lib("ctool/include/logging.hrl").

-type idp_group() :: #idp_group{}.
-type idp_entitlement() :: #idp_entitlement{}.

-export_type([idp_group/0, idp_entitlement/0]).

%%  User will be added with given privileges to a group denoted by a path.
%%  When there is more that one group in idp entitlement path, a structure of
%%  groups will be created where every group is a member of its predecessor
%%  (previous group in path) with regular member privileges in that group.

%% SUPER GROUPS
%% It is possible to specify a super group for each IdP. A super group will be
%% added to every group originating from that IdP with admin rights. By
%% specifying a group that actually exists in the IdP, it is possible for new
%% users to become an admin immediately after login (they are automatically
%% added to the group if it appears in their groups from that IdP).

%% API
-export([
    coalesce_groups/4,
    gen_group_id/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Analyzes the changes in user's memberships as provided by given IdP and
%% performs operations to ensure given group structure exists and user is
%% added / removed to group accordingly.
%% @end
%%--------------------------------------------------------------------
-spec coalesce_groups(IdP :: atom(), UserId :: od_user:id(),
    OldGroups :: [idp_entitlement()], NewGroups :: [idp_entitlement()]) -> ok.
coalesce_groups(IdP, UserId, OldGroups, NewGroups) ->
    SuperGroup = auth_utils:get_super_group(IdP),
    ToRmv = OldGroups -- NewGroups,
    ToAdd = NewGroups -- OldGroups,
    [remove_membership(IdpEntitlement, UserId) || IdpEntitlement <- ToRmv],
    [add_membership(IdpEntitlement, UserId, SuperGroup) || IdpEntitlement <- ToAdd],
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Encodes a group spec into group id used in database. Group specs can have
%% different chars, depending on IdP, and this way the ids in zone's database
%% are always safe.
%% @end
%%--------------------------------------------------------------------
-spec gen_group_id(idp_entitlement() | [idp_group()]) -> binary().
gen_group_id(#idp_entitlement{path = Path}) ->
    gen_group_id(Path);
gen_group_id(Path) ->
    datastore_utils:gen_key(<<"">>, term_to_binary(Path)).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds a new membership for user based on idp entitlement.
%% @end
%%--------------------------------------------------------------------
-spec add_membership(IdpEntitlement :: idp_entitlement(), UserId :: od_user:id(),
    SuperGroupPath :: undefined | [idp_group()]) -> ok.
add_membership(IdpEntitlement, UserId, SuperGroupPath) ->
    case SuperGroupPath of
        undefined -> ok;
        _ -> ensure_group_structure(SuperGroupPath, SuperGroupPath)
    end,
    Path = IdpEntitlement#idp_entitlement.path,
    ensure_group_structure(Path, SuperGroupPath),
    ensure_member(gen_group_id(Path), UserId, user,
        IdpEntitlement#idp_entitlement.privileges).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes an existing membership for user based on idp entitlement.
%% @end
%%--------------------------------------------------------------------
-spec remove_membership(IdpEntitlement :: idp_entitlement(),
    UserId :: od_user:id()) -> ok.
remove_membership(IdpEntitlement, UserId) ->
    % No need to delete groups, just user's memberships in them.
    ParentId = gen_group_id(IdpEntitlement#idp_entitlement.path),
    ensure_user_not_member(ParentId, UserId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Analyzes a chain of groups denoted by list of idp groups and creates missing groups,
%% adding proper relations between them. In case super group is specified,
%% it is added to every new group that is created in the process.
%% @end
%%--------------------------------------------------------------------
-spec ensure_group_structure(Path :: [idp_group()],
    SuperGroupPath :: undefined | [idp_group()]) -> ok.
ensure_group_structure(Path, SuperGroupPath) ->
    ensure_group_structure(Path, 1, SuperGroupPath).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Internal, recursive version of ensure_group_structure/2 that analyses a list
%% of idp groups, creating new groups and relations accordingly.
%% @end
%%--------------------------------------------------------------------
-spec ensure_group_structure(Path :: [idp_group()], Depth :: integer(),
    SuperGroupPath :: undefined | [idp_group()]) -> ok.
ensure_group_structure(Path, Depth, _) when Depth > length(Path) ->
    ok;
ensure_group_structure(Path, Depth, SuperGroupPath) ->
    CurrentGroupPath = lists:sublist(Path, Depth),
    CurrentGroupId = gen_group_id(CurrentGroupPath),
    CurrentGroup = lists:nth(Depth, Path),
    {ok, _} = od_group:update(CurrentGroupId, fun(Group) -> {ok, Group} end, #od_group{
        name = group_logic:normalize_name(CurrentGroup#idp_group.name),
        type = CurrentGroup#idp_group.type,
        protected = true
    }),
    case Depth > 1 of
        true ->
            % If the current group is the super group, it should be added with
            % admin privileges to its parent group.
            {MemberType, MemberRole} = case CurrentGroupPath of
                SuperGroupPath -> {group, admin};
                _ -> {group , member}
            end,
            ParentGroupPath = lists:sublist(Path, Depth - 1),
            ensure_member(gen_group_id(ParentGroupPath), CurrentGroupId,
                MemberType, MemberRole);
        false ->
            ok
    end,
    case SuperGroupPath of
        undefined ->
            ok;
        CurrentGroupPath ->
            % Do not add the super group to itself
            ok;
        _ ->
            % Super group is defined and different than the group, add it with
            % admin privileges.
            ensure_member(CurrentGroupId, gen_group_id(SuperGroupPath),
                group, admin)
    end,
    ensure_group_structure(Path, Depth + 1, SuperGroupPath).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that given entity (user or group) is a member of group denoted by id.
%% @end
%%--------------------------------------------------------------------
-spec ensure_member(ParentId :: od_group:id(), MemberId :: od_group:id() | od_user:id(),
    MemberType :: user | group, Role :: atom()) -> ok.
ensure_member(ParentId, MemberId, MemberType, Role) ->
    Privileges = role_to_privileges(Role),
    case MemberType of
        user ->
            group_logic:add_user(?ROOT, ParentId, MemberId, Privileges);
        group ->
            group_logic:add_group(?ROOT, ParentId, MemberId, Privileges)
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that given user is not a member of group denoted by id.
%% @end
%%--------------------------------------------------------------------
-spec ensure_user_not_member(ParentId :: od_group:id(), UserId :: od_user:id()) -> ok.
ensure_user_not_member(ParentId, UserId) ->
    group_logic:remove_user(?ROOT, ParentId, UserId),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts group role identifier used in group/membership specs into group
%% privileges recognized in onedata.
%% @end
%%--------------------------------------------------------------------
-spec role_to_privileges(atom()) -> [privileges:group_privilege()].
role_to_privileges(member) -> privileges:group_user();
role_to_privileges(manager) -> privileges:group_manager();
role_to_privileges(admin) -> privileges:group_admin().
