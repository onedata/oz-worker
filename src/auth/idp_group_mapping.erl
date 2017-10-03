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
-include_lib("ctool/include/logging.hrl").


-type group_spec() :: binary().
%% Format of group spec:
%%  Group structure is expressed by a chain where group ids are separated by "/"
%%  Group names are identical to group ids from spec
%%  Type is denoted by two letter abbreviations:
%%      # vo - organization
%%      # ut - unit
%%      # tm - team
%%      # rl - role
%%  Examples:
%%      # vo:egi.eu
%%      # vo:egi.eu/ut:some-egi-unit
%%      # vo:egi.eu/ut:some-egi-unit/tm:some-egi-team
%%  When there is more that one group in group spec, a structure of groups will
%%      be created where every group is a member of its predecessor
%%      (left-hand side in spec) with regular member privileges in that group.
-type membership_spec() :: binary().
%% Format of membership spec:
%%  The format is almost the same as for group spec, except the chain MUST end
%%      in a string denoting member type and role, separated by ":".
%%  Allowed types:
%%      # user
%%      # group
%%  Allowed roles:
%%      # member
%%      # manager
%%      # admin
%%  Examples:
%%      # vo:egi.eu/user:member
%%      # vo:egi.eu/ut:some-egi-unit/group:admin
%%      # vo:egi.eu/ut:some-egi-unit/tm:some-egi-team/user:manager
-export_type([group_spec/0, membership_spec/0]).

%% SUPER GROUPS
%% It is possible to specify a super group for each IdP. A super group will be
%% added to every group originating from that IdP with admin rights. By
%% specifying a group that actually exists in the IdP, it is possible for new
%% users to become an admin immediately after login (they are automatically
%% added to the group if it appears in their groups from that IdP).

%% API
-export([
    coalesce_groups/4,
    group_spec_to_db_id/1,
    membership_spec_to_group_spec/1
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
    OldGroups :: [membership_spec()], NewGroups :: [membership_spec()]) -> ok.
coalesce_groups(IdP, UserId, OldGroups, NewGroups) ->
    SuperGroup = auth_utils:get_super_group(IdP),
    ToRmv = OldGroups -- NewGroups,
    ToAdd = NewGroups -- OldGroups,
    [remove_membership(GrSpec, UserId) || GrSpec <- ToRmv],
    [add_membership(GrSpec, UserId, SuperGroup) || GrSpec <- ToAdd],
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Encodes a group spec into group id used in database. Group specs can have
%% different chars, depending on IdP, and this way the ids in zone's database
%% are always safe.
%% @end
%%--------------------------------------------------------------------
-spec group_spec_to_db_id(group_spec()) -> binary().
group_spec_to_db_id(GroupSpec) ->
    datastore_utils:gen_key(<<"">>, GroupSpec).


%%--------------------------------------------------------------------
%% @doc
%% Converts membership spec to group spec, i.e. strips the string denoting
%% member type and role.
%% @end
%%--------------------------------------------------------------------
-spec membership_spec_to_group_spec(membership_spec()) -> group_spec().
membership_spec_to_group_spec(MembershipSpec) ->
    Tokens = spec_to_tokens(MembershipSpec),
    ParentGroupTokens = lists:sublist(Tokens, length(Tokens) - 1),
    tokens_to_spec(ParentGroupTokens).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Adds a new membership for user based on membership spec.
%% @end
%%--------------------------------------------------------------------
-spec add_membership(MembershipSpec :: membership_spec(), UserId :: od_user:id(),
    SuperGroupSpec :: undefined | group_spec()) -> ok.
add_membership(MembershipSpec, UserId, SuperGroupSpec) ->
    case SuperGroupSpec of
        undefined -> ok;
        _ -> ensure_group_structure(SuperGroupSpec, SuperGroupSpec)
    end,
    ParentGroupSpec = membership_spec_to_group_spec(MembershipSpec),
    ensure_group_structure(ParentGroupSpec, SuperGroupSpec),
    ensure_member(true, MembershipSpec, UserId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes an existing membership for user based on membership spec.
%% @end
%%--------------------------------------------------------------------
-spec remove_membership(MembershipSpec :: membership_spec(),
    UserId :: od_user:id()) -> ok.
remove_membership(MembershipSpec, UserId) ->
    % No need to delete groups, just user's memberships in them.
    ensure_member(false, MembershipSpec, UserId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Analyzes a chain of groups denoted by group spec and creates missing groups,
%% adding proper relations between them. In case super group is specified,
%% it is added to every new group that is created in the process.
%% The group spec can also be given as a list of binaries, which is the result
%% of splitting it on the "/" char.
%% @end
%%--------------------------------------------------------------------
-spec ensure_group_structure(GroupSpec :: group_spec() | [binary()],
    SuperGroupSpec :: undefined | group_spec()) -> ok.
ensure_group_structure(GroupSpec, SuperGroupSpec) when is_binary(GroupSpec) ->
    ensure_group_structure(spec_to_tokens(GroupSpec), SuperGroupSpec);
ensure_group_structure(GroupSpecTokens, SuperGroupSpec) ->
    ensure_group_structure(GroupSpecTokens, 1, SuperGroupSpec).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Internal, recursive version of ensure_group_structure/2 that analyses a list
%% of group spec tokens, creating new groups and relations accordingly.
%% @end
%%--------------------------------------------------------------------
-spec ensure_group_structure(GroupSpecTokens :: [binary()], Depth :: integer(),
    SuperGroupSpec :: undefined | group_spec()) -> ok.
ensure_group_structure(GroupSpecTokens, Depth, _) when Depth > length(GroupSpecTokens) ->
    ok;
ensure_group_structure(GroupSpecTokens, Depth, SuperGroupSpec) ->
    GroupSpec = tokens_to_spec(lists:sublist(GroupSpecTokens, Depth)),
    GroupId = group_spec_to_db_id(GroupSpec),
    SubgroupId = lists:nth(Depth, GroupSpecTokens),
    <<GroupTypeStr:2/binary, ":", GroupName/binary>> = SubgroupId,
    {ok, _} = od_group:update(GroupId, fun(Group) -> {ok, Group} end, #od_group{
        name = GroupName, type = str_to_type(GroupTypeStr)
    }),
    case Depth > 1 of
        true ->
            % If the current group is the super group, it should be added with
            % admin privileges to its parent group.
            MembershipType = case GroupSpec of
                SuperGroupSpec -> <<"group:admin">>;
                _ -> <<"group:member">>
            end,
            ParentGroupSpec = tokens_to_spec(lists:sublist(GroupSpecTokens, Depth - 1)),
            MembershipSpec = <<ParentGroupSpec/binary, "/", MembershipType/binary>>,
            ensure_member(true, MembershipSpec, GroupId);
        false ->
            ok
    end,
    case {SuperGroupSpec, GroupSpec} of
        {undefined, _} ->
            ok;
        {Spec, Spec} ->
            % Do not add the super group to itself
            ok;
        _ ->
            % Super group is defined and different that the group, add it with
            % admin privileges.
            ensure_member(
                true,
                <<GroupSpec/binary, "/group:admin">>,
                group_spec_to_db_id(SuperGroupSpec)
            )
    end,
    ensure_group_structure(GroupSpecTokens, Depth + 1, SuperGroupSpec).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that given entity (user or group) is, or is not, a member of group
%% denoted by membership spec.
%% @end
%%--------------------------------------------------------------------
-spec ensure_member(IsMember :: boolean(), MembershipSpec :: membership_spec(),
    SuperGroupSpec :: undefined | group_spec()) -> ok.
ensure_member(IsMember, MembershipSpec, MemberId) when is_binary(MembershipSpec) ->
    Tokens = spec_to_tokens(MembershipSpec),
    ParentId = group_spec_to_db_id(tokens_to_spec(lists:sublist(Tokens, length(Tokens) - 1))),
    [MemberType, RoleStr] = binary:split(lists:last(Tokens), <<":">>, [global]),
    Privileges = str_role_to_privileges(RoleStr),
    case {MemberType, IsMember} of
        {<<"user">>, true} ->
            group_logic:add_user(?ROOT, ParentId, MemberId, Privileges);
        {<<"user">>, false} ->
            group_logic:remove_user(?ROOT, ParentId, MemberId);
        {<<"group">>, true} ->
            group_logic:add_group(?ROOT, ParentId, MemberId, Privileges);
        {<<"group">>, false} ->
            group_logic:remove_group(?ROOT, ParentId, MemberId)
    end,
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Splits a group spec into tokens on "/" char.
%% @end
%%--------------------------------------------------------------------
-spec spec_to_tokens(Spec :: group_spec()) -> [binary()].
spec_to_tokens(Spec) ->
    binary:split(Spec, <<"/">>, [global]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Joins tokens into a group spec using "/" char.
%% @end
%%--------------------------------------------------------------------
-spec tokens_to_spec([binary()]) -> Spec :: group_spec().
tokens_to_spec(Tokens) ->
    str_utils:join_binary(Tokens, <<"/">>).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts group type identifier used in group/membership specs into types
%% recognized in onedata.
%% @end
%%--------------------------------------------------------------------
-spec str_to_type(binary()) -> od_group:type().
str_to_type(<<"vo">>) -> organization;
str_to_type(<<"ut">>) -> unit;
str_to_type(<<"tm">>) -> team;
str_to_type(<<"rl">>) -> role.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts group role identifier used in group/membership specs into group
%% privileges recognized in onedata.
%% @end
%%--------------------------------------------------------------------
-spec str_role_to_privileges(binary()) -> [privileges:group_privilege()].
str_role_to_privileges(<<"member">>) -> privileges:group_user();
str_role_to_privileges(<<"manager">>) -> privileges:group_manager();
str_role_to_privileges(<<"admin">>) -> privileges:group_admin().
