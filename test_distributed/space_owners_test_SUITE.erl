%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Tests verifying the space owner logic and various edge cases related to
%%% owner manipulation. The tests use only the internal logic API - all APIs
%%% are tested in space_users_api_test_suite, along with privileges to perform
%%% certain operations regarding owners.
%%% @end
%%%-------------------------------------------------------------------
-module(space_owners_test_SUITE).
-author("Lukasz Opiola").

-include("ozt.hrl").

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    space_creator_becomes_the_first_owner_when_creating_a_space_for_self/1,
    space_creator_becomes_the_first_owner_when_creating_a_space_for_his_group/1,
    space_owner_effectively_has_all_privileges_regardless_of_those_assigned/1,

    non_space_member_cannot_be_granted_ownership/1,
    space_member_can_be_granted_ownership/1,
    space_member_cannot_be_granted_ownership_twice/1,

    the_only_space_owner_cannot_be_revoked_of_ownership/1,
    one_of_space_owners_can_be_revoked_of_ownership/1,
    non_owner_cannot_remove_a_user_that_is_an_owner_from_space_or_revoke_him_of_ownership/1,
    one_of_space_owners_cannot_be_revoked_of_ownership_twice/1,
    removed_member_that_was_an_owner_loses_ownership/1,

    new_owner_becomes_a_direct_member_if_he_was_not_one_and_gets_admin_privs/1,
    user_revoked_of_ownership_does_not_lose_direct_membership_nor_privileges/1,

    the_only_space_owner_cannot_be_removed_from_a_space_with_other_direct_users/1,
    the_only_space_owner_cannot_be_removed_from_a_space_with_other_effective_users/1,

    the_only_space_owner_can_be_removed_from_a_space_with_no_indirect_users_being_the_last_direct_user/1,
    the_only_space_owner_can_be_removed_from_a_space_being_the_last_effective_user/1,

    no_one_is_assigned_ownership_if_a_populated_group_is_added_to_an_empty_space/1,

    first_user_added_to_an_empty_space_becomes_an_owner/1,
    first_user_joining_an_empty_space_becomes_an_owner/1,

    first_user_added_to_an_empty_but_once_populated_space_becomes_an_owner/1,
    first_user_joining_an_empty_but_once_populated_space_becomes_an_owner/1,

    first_user_added_to_a_space_with_empty_groups_becomes_an_owner/1,
    first_user_joining_a_space_with_empty_groups_becomes_an_owner/1,

    first_user_added_to_a_space_with_indirect_members_only_becomes_an_owner/1,
    first_user_joining_a_space_with_indirect_members_only_becomes_an_owner/1,

    user_cannot_be_deleted_if_he_is_the_last_owner_in_any_space_with_more_members/1,
    user_can_be_deleted_if_he_is_the_last_member_and_owner_in_a_space/1,
    user_can_be_deleted_if_he_is_not_the_last_owner_in_a_space/1,
    populated_space_with_owners_can_be_entirely_deleted/1,

    adding_owners_in_parallel_is_safe_from_race_conditions/1,
    removing_owners_in_parallel_is_safe_from_race_conditions/1,
    adding_and_removing_owners_in_parallel_is_safe_from_race_conditions/1,

    adding_owner_while_removing_corresponding_member_is_safe_from_race_conditions/1,
    removing_last_member_being_owner_while_adding_a_new_member_is_safe_from_race_conditions/1,

    adding_an_indirect_user_as_owner_while_removing_him_indirectly_from_space_is_safe_from_race_conditions/1,
    adding_an_indirect_user_as_owner_while_removing_him_directly_from_space_is_safe_from_race_conditions/1
]).

all() -> ?ALL([
    space_creator_becomes_the_first_owner_when_creating_a_space_for_self,
    space_creator_becomes_the_first_owner_when_creating_a_space_for_his_group,
    space_owner_effectively_has_all_privileges_regardless_of_those_assigned,

    non_space_member_cannot_be_granted_ownership,
    space_member_can_be_granted_ownership,
    space_member_cannot_be_granted_ownership_twice,

    the_only_space_owner_cannot_be_revoked_of_ownership,
    one_of_space_owners_can_be_revoked_of_ownership,
    non_owner_cannot_remove_a_user_that_is_an_owner_from_space_or_revoke_him_of_ownership,
    one_of_space_owners_cannot_be_revoked_of_ownership_twice,
    removed_member_that_was_an_owner_loses_ownership,

    new_owner_becomes_a_direct_member_if_he_was_not_one_and_gets_admin_privs,
    user_revoked_of_ownership_does_not_lose_direct_membership_nor_privileges,

    the_only_space_owner_cannot_be_removed_from_a_space_with_other_direct_users,
    the_only_space_owner_cannot_be_removed_from_a_space_with_other_effective_users,

    the_only_space_owner_can_be_removed_from_a_space_with_no_indirect_users_being_the_last_direct_user,
    the_only_space_owner_can_be_removed_from_a_space_being_the_last_effective_user,

    no_one_is_assigned_ownership_if_a_populated_group_is_added_to_an_empty_space,

    first_user_added_to_an_empty_space_becomes_an_owner,
    first_user_joining_an_empty_space_becomes_an_owner,

    first_user_added_to_an_empty_but_once_populated_space_becomes_an_owner,
    first_user_joining_an_empty_but_once_populated_space_becomes_an_owner,

    first_user_added_to_a_space_with_empty_groups_becomes_an_owner,
    first_user_joining_a_space_with_empty_groups_becomes_an_owner,

    first_user_added_to_a_space_with_indirect_members_only_becomes_an_owner,
    first_user_joining_a_space_with_indirect_members_only_becomes_an_owner,

    user_cannot_be_deleted_if_he_is_the_last_owner_in_any_space_with_more_members,
    user_can_be_deleted_if_he_is_the_last_member_and_owner_in_a_space,
    user_can_be_deleted_if_he_is_not_the_last_owner_in_a_space,
    populated_space_with_owners_can_be_entirely_deleted,

    adding_owners_in_parallel_is_safe_from_race_conditions,
    removing_owners_in_parallel_is_safe_from_race_conditions,
    adding_and_removing_owners_in_parallel_is_safe_from_race_conditions,

    adding_owner_while_removing_corresponding_member_is_safe_from_race_conditions,
    removing_last_member_being_owner_while_adding_a_new_member_is_safe_from_race_conditions,

    adding_an_indirect_user_as_owner_while_removing_him_indirectly_from_space_is_safe_from_race_conditions,
    adding_an_indirect_user_as_owner_while_removing_him_directly_from_space_is_safe_from_race_conditions
]).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ozt:init_per_suite(Config).

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%%===================================================================
%%% Test functions
%%%===================================================================

space_creator_becomes_the_first_owner_when_creating_a_space_for_self(_Config) ->
    User = ozt_users:create(),
    Space = ozt_users:create_space_for(User),
    ?assert(is_owner(Space, User)),
    ?assert(is_direct_member(Space, User)).

space_creator_becomes_the_first_owner_when_creating_a_space_for_his_group(_Config) ->
    User = ozt_users:create(),
    Group = ozt_users:create_group_for(User),
    {ok, Space} = ?assertMatch({ok, _}, ozt:rpc(group_logic, create_space, [?USER(User), Group, <<"space">>])),
    ?assert(is_owner(Space, User)),
    ?assert(is_direct_member(Space, User)).

space_owner_effectively_has_all_privileges_regardless_of_those_assigned(_Config) ->
    Owner = ozt_users:create(),
    Space = ozt_users:create_space_for(Owner),
    ?assert(has_each_of_privileges(Space, Owner, privileges:space_admin())),
    lists:foreach(fun(_) ->
        ozt_spaces:set_user_privileges(Space, Owner, lists_utils:random_sublist(privileges:space_admin())),
        ?assert(has_each_of_privileges(Space, Owner, privileges:space_admin()))
    end, lists:seq(1, 50)).


non_space_member_cannot_be_granted_ownership(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),
    AnotherUser = ozt_users:create(),
    ?assertEqual(
        ?ERROR_RELATION_DOES_NOT_EXIST(od_space, Space, od_user, AnotherUser),
        ozt:rpc(space_logic, add_owner, [?ROOT, Space, AnotherUser])
    ).

space_member_can_be_granted_ownership(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),
    UserAlpha = ozt_users:create(),
    ozt_spaces:add_user(Space, UserAlpha),
    ?assertEqual(ok, ozt:rpc(space_logic, add_owner, [?ROOT, Space, UserAlpha])),
    ?assert(is_owner(Space, UserAlpha)),

    UserBeta = ozt_users:create(),
    ozt_spaces:add_user(Space, UserBeta),
    ?assertEqual(ok, ozt:rpc(space_logic, add_owner, [?ROOT, Space, UserBeta])),
    ?assert(is_owner(Space, UserBeta)),
    ?assert(has_owners(Space, [OriginalOwner, UserAlpha, UserBeta])).

space_member_cannot_be_granted_ownership_twice(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),
    AnotherUser = ozt_users:create(),
    ozt_spaces:add_user(Space, AnotherUser),
    ?assertEqual(ok, ozt:rpc(space_logic, add_owner, [?ROOT, Space, AnotherUser])),
    ?assertEqual(?ERROR_ALREADY_EXISTS, ozt:rpc(space_logic, add_owner, [?ROOT, Space, AnotherUser])).


the_only_space_owner_cannot_be_revoked_of_ownership(_Config) ->
    Owner = ozt_users:create(),
    Space = ozt_users:create_space_for(Owner),
    ?assertEqual(
        ?ERROR_CANNOT_REMOVE_LAST_OWNER(od_space, Space),
        ozt:rpc(space_logic, remove_owner, [?ROOT, Space, Owner])
    ).

one_of_space_owners_can_be_revoked_of_ownership(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),
    AnotherUser = ozt_users:create(),
    ozt_spaces:add_user(Space, AnotherUser),
    ozt_spaces:add_owner(Space, AnotherUser),
    ?assertEqual(ok, ozt:rpc(space_logic, remove_owner, [?ROOT, Space, AnotherUser])).

non_owner_cannot_remove_a_user_that_is_an_owner_from_space_or_revoke_him_of_ownership(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),
    AnotherOwner = ozt_users:create(),
    NonOwnerButAdmin = ozt_users:create(),
    ozt_spaces:add_user(Space, AnotherOwner),
    ozt_spaces:add_owner(Space, AnotherOwner),
    ozt_spaces:add_user(Space, NonOwnerButAdmin, privileges:space_admin()),
    ?assertEqual(?ERROR_FORBIDDEN, ozt:rpc(space_logic, remove_owner, [?USER(NonOwnerButAdmin), Space, AnotherOwner])),
    ?assertEqual(?ERROR_FORBIDDEN, ozt:rpc(space_logic, remove_user, [?USER(NonOwnerButAdmin), Space, AnotherOwner])).

one_of_space_owners_cannot_be_revoked_of_ownership_twice(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),
    AnotherUser = ozt_users:create(),
    ozt_spaces:add_user(Space, AnotherUser),
    ozt_spaces:add_owner(Space, AnotherUser),
    ?assertEqual(ok, ozt:rpc(space_logic, remove_owner, [?ROOT, Space, AnotherUser])),
    ?assertEqual(?ERROR_NOT_FOUND, ozt:rpc(space_logic, remove_owner, [?ROOT, Space, AnotherUser])).


removed_member_that_was_an_owner_loses_ownership(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),

    AnotherUser = ozt_users:create(),
    ozt_spaces:add_user(Space, AnotherUser),
    ozt_spaces:add_owner(Space, AnotherUser),
    ?assert(is_owner(Space, AnotherUser)),
    ozt_spaces:remove_user(Space, AnotherUser),
    ?assert(not is_owner(Space, AnotherUser)),

    ThirdUser = ozt_users:create(),
    ozt_spaces:add_user(Space, ThirdUser),
    ozt_spaces:add_owner(Space, ThirdUser),
    ?assert(is_owner(Space, ThirdUser)),
    ozt_users:leave_space(ThirdUser, Space),
    ?assert(not is_owner(Space, ThirdUser)).


new_owner_becomes_a_direct_member_if_he_was_not_one_and_gets_admin_privs(_Config) ->
    Space = ozt_spaces:create(),
    NewOwner = ozt_users:create(),
    ozt_spaces:add_group(Space, ozt_users:create_group_for(NewOwner)),
    ozt:reconcile_entity_graph(),
    ?assert(not is_direct_member(Space, NewOwner)),
    ozt_spaces:add_owner(Space, NewOwner),
    ?assert(is_owner(Space, NewOwner)),
    ?assert(is_direct_member(Space, NewOwner)),
    ?assert(has_direct_privileges(Space, NewOwner, privileges:space_admin())),
    ?assert(has_each_of_privileges(Space, NewOwner, privileges:space_admin())),

    AnotherOwner = ozt_users:create(),
    ozt_spaces:add_group(Space, ozt_users:create_group_for(AnotherOwner)),
    ozt:reconcile_entity_graph(),
    ?assert(not is_direct_member(Space, AnotherOwner)),
    ozt_spaces:add_owner(Space, AnotherOwner),
    ?assert(is_owner(Space, AnotherOwner)),
    ?assert(is_direct_member(Space, AnotherOwner)),
    ?assert(has_direct_privileges(Space, AnotherOwner, privileges:space_admin())),
    ?assert(has_each_of_privileges(Space, AnotherOwner, privileges:space_admin())).



user_revoked_of_ownership_does_not_lose_direct_membership_nor_privileges(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),
    NewOwner = ozt_users:create(),
    ozt_spaces:add_group(Space, ozt_users:create_group_for(NewOwner)),
    ozt:reconcile_entity_graph(),
    ozt_spaces:add_owner(Space, NewOwner),
    ozt_spaces:set_user_privileges(Space, NewOwner, privileges:space_manager()),
    ozt_spaces:remove_owner(Space, NewOwner),
    ?assert(not is_owner(Space, NewOwner)),
    ?assert(is_direct_member(Space, NewOwner)),
    ?assert(has_direct_privileges(Space, NewOwner, privileges:space_manager())),
    ?assert(has_each_of_privileges(Space, NewOwner, privileges:space_manager())),
    ?assert(has_none_of_privileges(Space, NewOwner, privileges:space_admin() -- privileges:space_manager())).


the_only_space_owner_cannot_be_removed_from_a_space_with_other_direct_users(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),
    ozt_spaces:add_user(Space, ozt_users:create()),
    ozt_spaces:add_user(Space, ozt_users:create()),
    ozt:reconcile_entity_graph(),
    ?assertEqual(
        ?ERROR_CANNOT_REMOVE_LAST_OWNER(od_space, Space),
        ozt:rpc(space_logic, remove_user, [?ROOT, Space, OriginalOwner])
    ),
    ?assertEqual(
        ?ERROR_CANNOT_REMOVE_LAST_OWNER(od_space, Space),
        ozt:rpc(user_logic, leave_space, [?ROOT, OriginalOwner, Space])
    ).

the_only_space_owner_cannot_be_removed_from_a_space_with_other_effective_users(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),
    EffectiveUser = ozt_users:create(),
    ozt_spaces:add_group(Space, ozt_users:create_group_for(EffectiveUser)),
    ozt:reconcile_entity_graph(),
    ?assertEqual(
        ?ERROR_CANNOT_REMOVE_LAST_OWNER(od_space, Space),
        ozt:rpc(space_logic, remove_user, [?ROOT, Space, OriginalOwner])
    ),
    ?assertEqual(
        ?ERROR_CANNOT_REMOVE_LAST_OWNER(od_space, Space),
        ozt:rpc(user_logic, leave_space, [?ROOT, OriginalOwner, Space])
    ).


the_only_space_owner_can_be_removed_from_a_space_with_no_indirect_users_being_the_last_direct_user(_Config) ->
    FirstOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(FirstOwner),
    ?assertEqual(ok, ozt:rpc(space_logic, remove_user, [?ROOT, Space, FirstOwner])),
    SecondOwner = ozt_users:create(),
    ozt_spaces:add_user(Space, SecondOwner),
    ozt:reconcile_entity_graph(),
    ?assertEqual(ok, ozt:rpc(user_logic, leave_space, [?ROOT, SecondOwner, Space])),

    OwnerAlpha = ozt_users:create(),
    AnotherSpace = ozt_users:create_space_for(OwnerAlpha),
    ?assertEqual(ok, ozt:rpc(user_logic, leave_space, [?ROOT, OwnerAlpha, AnotherSpace])),
    OwnerBeta = ozt_users:create(),
    ozt_spaces:add_user(AnotherSpace, OwnerBeta),
    ozt:reconcile_entity_graph(),
    ?assertEqual(ok, ozt:rpc(space_logic, remove_user, [?ROOT, AnotherSpace, OwnerBeta])).



the_only_space_owner_can_be_removed_from_a_space_being_the_last_effective_user(_Config) ->
    OwnerAlpha = ozt_users:create(),
    SpaceAlpha = ozt_users:create_space_for(OwnerAlpha),
    GroupAlpha = ozt_groups:create(),
    ozt_spaces:add_group(SpaceAlpha, GroupAlpha),
    ozt_groups:add_child(GroupAlpha, ozt_groups:create()),
    ozt:reconcile_entity_graph(),
    ?assertEqual(ok, ozt:rpc(space_logic, remove_user, [?ROOT, SpaceAlpha, OwnerAlpha])),

    OwnerBeta = ozt_users:create(),
    SpaceBeta = ozt_users:create_space_for(OwnerBeta),
    ozt_spaces:add_group(SpaceBeta, ozt_groups:create()),
    ozt:reconcile_entity_graph(),
    ?assertEqual(ok, ozt:rpc(user_logic, leave_space, [?ROOT, OwnerBeta, SpaceBeta])).


no_one_is_assigned_ownership_if_a_populated_group_is_added_to_an_empty_space(_Config) ->
    Space = ozt_spaces:create(),
    FirstGroupUser = ozt_users:create(),
    AnotherGroupUser = ozt_users:create(),
    Group = ozt_users:create_group_for(FirstGroupUser),
    ozt_groups:add_user(Group, AnotherGroupUser),

    ozt_spaces:add_group(Space, Group),
    ?assert(has_owners(Space, [])),
    ?assert(has_direct_members(Space, [])),
    ozt_spaces:remove_group(Space, Group),

    OzAdmin = ozt_users:create_admin(),
    InviteToken = ozt_spaces:create_group_invite_token(Space, OzAdmin),
    ozt_groups:join_space(Group, FirstGroupUser, InviteToken),
    ?assert(has_owners(Space, [])),
    ?assert(has_direct_members(Space, [])).


first_user_added_to_an_empty_space_becomes_an_owner(_Config) ->
    common_first_user_becomes_an_owner(user_is_added_by_admin).

first_user_joining_an_empty_space_becomes_an_owner(_Config) ->
    common_first_user_becomes_an_owner(user_joins_with_token).


first_user_added_to_an_empty_but_once_populated_space_becomes_an_owner(_Config) ->
    common_first_user_becomes_an_owner(user_is_added_by_admin, fun(Space) ->
        FirstUser = ozt_users:create(),
        ozt_spaces:add_user(Space, FirstUser),
        AnotherUser = ozt_users:create(),
        Group = ozt_users:create_group_for(AnotherUser),
        ozt_spaces:add_group(Space, Group),
        ozt:reconcile_entity_graph(),

        ozt_groups:remove_user(Group, AnotherUser),
        ozt:reconcile_entity_graph(),
        ozt_spaces:remove_user(Space, FirstUser),
        ozt:reconcile_entity_graph()
    end).

first_user_joining_an_empty_but_once_populated_space_becomes_an_owner(_Config) ->
    common_first_user_becomes_an_owner(user_joins_with_token, fun(Space) ->
        TopGroup = ozt_groups:create(),
        ChildGroup = ozt_groups:create(),
        ozt_groups:add_child(TopGroup, ChildGroup),
        GroupUser = ozt_users:create(),
        ozt_groups:add_user(ChildGroup, GroupUser),
        ozt_spaces:add_group(Space, TopGroup),

        FirstUser = ozt_users:create(),
        SecondUser = ozt_users:create(),
        ozt_spaces:add_user(Space, FirstUser),
        ozt_spaces:add_user(Space, SecondUser),
        ozt:reconcile_entity_graph(),

        ozt_groups:delete(ChildGroup),
        ozt:reconcile_entity_graph(),
        ozt_spaces:remove_user(Space, SecondUser),
        ozt:reconcile_entity_graph(),
        ozt_spaces:remove_user(Space, FirstUser),
        ozt:reconcile_entity_graph()
    end).


first_user_added_to_a_space_with_empty_groups_becomes_an_owner(_Config) ->
    common_first_user_becomes_an_owner(user_is_added_by_admin, fun(Space) ->
        ozt_spaces:add_group(Space, ozt_groups:create()),
        ozt_spaces:add_group(Space, ozt_groups:create())
    end).

first_user_joining_a_space_with_empty_groups_becomes_an_owner(_Config) ->
    common_first_user_becomes_an_owner(user_joins_with_token, fun(Space) ->
        ozt_spaces:add_group(Space, ozt_groups:create()),
        ozt_spaces:add_group(Space, ozt_groups:create())
    end).


first_user_added_to_a_space_with_indirect_members_only_becomes_an_owner(_Config) ->
    common_first_user_becomes_an_owner(user_is_added_by_admin, fun(Space) ->
        UserAlpha = ozt_users:create(),
        UserBeta = ozt_users:create(),
        ozt_spaces:add_group(Space, ozt_users:create_group_for(UserAlpha)),
        ozt_spaces:add_group(Space, ozt_users:create_group_for(UserBeta))
    end).

first_user_joining_a_space_with_indirect_members_only_becomes_an_owner(_Config) ->
    common_first_user_becomes_an_owner(user_joins_with_token, fun(Space) ->
        UserAlpha = ozt_users:create(),
        UserBeta = ozt_users:create(),
        ozt_spaces:add_group(Space, ozt_users:create_group_for(UserAlpha)),
        ozt_spaces:add_group(Space, ozt_users:create_group_for(UserBeta))
    end).


user_cannot_be_deleted_if_he_is_the_last_owner_in_any_space_with_more_members(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),
    AnotherUser = ozt_users:create(),
    ozt_spaces:add_user(Space, AnotherUser),
    ozt:reconcile_entity_graph(),
    ?assertEqual(
        ?ERROR_CANNOT_REMOVE_LAST_OWNER(od_space, Space),
        ozt:rpc(user_logic, delete, [?ROOT, OriginalOwner])
    ),
    ?assert(is_owner(Space, OriginalOwner)),
    ?assert(is_direct_member(Space, OriginalOwner)).

user_can_be_deleted_if_he_is_the_last_member_and_owner_in_a_space(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),
    ?assertEqual(ok, ozt:rpc(user_logic, delete, [?ROOT, OriginalOwner])),
    ?assert(not is_owner(Space, OriginalOwner)),
    ?assert(not is_direct_member(Space, OriginalOwner)).

user_can_be_deleted_if_he_is_not_the_last_owner_in_a_space(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),
    AnotherUser = ozt_users:create(),
    ozt_spaces:add_user(Space, AnotherUser),
    ozt_spaces:add_owner(Space, AnotherUser),
    ?assertEqual(ok, ozt:rpc(user_logic, delete, [?ROOT, OriginalOwner])),
    ?assert(not is_owner(Space, OriginalOwner)),
    ?assert(not is_direct_member(Space, OriginalOwner)),
    ?assert(is_owner(Space, AnotherUser)),
    ?assert(is_direct_member(Space, AnotherUser)).


populated_space_with_owners_can_be_entirely_deleted(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),
    TopGroup = ozt_groups:create(),
    ChildGroup = ozt_groups:create(),
    ozt_groups:add_child(TopGroup, ChildGroup),
    GroupUserAlpha = ozt_users:create(),
    GroupUserBeta = ozt_users:create(),
    ozt_groups:add_user(ChildGroup, GroupUserAlpha),
    ozt_groups:add_user(ChildGroup, GroupUserBeta),
    ozt_spaces:add_group(Space, TopGroup),

    FirstUser = ozt_users:create(),
    SecondUser = ozt_users:create(),
    ozt_spaces:add_user(Space, FirstUser),
    ozt_spaces:add_user(Space, SecondUser),
    ozt:reconcile_entity_graph(),
    ozt_spaces:add_owner(Space, GroupUserBeta),

    ?assert(user_has_space(FirstUser, Space)),
    ?assert(user_has_space(SecondUser, Space)),
    ?assert(user_has_eff_space(GroupUserAlpha, Space)),
    ?assert(user_has_space(GroupUserBeta, Space)),

    ?assertEqual(ok, ozt:rpc(space_logic, delete, [?ROOT, Space])),
    ozt:reconcile_entity_graph(),
    ?assert(not user_has_space(FirstUser, Space)),
    ?assert(not user_has_space(SecondUser, Space)),
    ?assert(not user_has_eff_space(GroupUserAlpha, Space)),
    ?assert(not user_has_space(GroupUserBeta, Space)).


%% @private
common_first_user_becomes_an_owner(UserAddMethod) ->
    common_first_user_becomes_an_owner(UserAddMethod, fun(_Space) -> ok end).
common_first_user_becomes_an_owner(UserAddMethod, PrepareSpaceFun) ->
    Space = ozt_spaces:create(),
    PrepareSpaceFun(Space),

    FirstUser = ozt_users:create(),
    add_user_using_method(UserAddMethod, Space, FirstUser),
    ?assert(is_owner(Space, FirstUser)),

    SecondUser = ozt_users:create(),
    add_user_using_method(UserAddMethod, Space, SecondUser),
    SecondUserPrivs = ozt_spaces:get_user_privileges(Space, SecondUser),

    ?assert(not is_owner(Space, SecondUser)),
    ?assert(is_owner(Space, FirstUser)),
    ?assert(has_none_of_privileges(Space, SecondUser, privileges:space_admin() -- SecondUserPrivs)),
    ?assert(has_each_of_privileges(Space, FirstUser, privileges:space_admin())).


adding_owners_in_parallel_is_safe_from_race_conditions(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),

    UserCount = 200,
    NewUsers = lists:map(fun(_) ->
        User = ozt_users:create(),
        ozt_spaces:add_user(Space, User),
        User
    end, lists:seq(1, UserCount)),

    ozt:pforeach(fun(User) ->
        ozt_spaces:add_owner(Space, User)
    end, NewUsers),

    ?assert(has_owners(Space, [OriginalOwner | NewUsers])).

removing_owners_in_parallel_is_safe_from_race_conditions(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),

    UserCount = 200,
    NewOwners = lists:map(fun(_) ->
        User = ozt_users:create(),
        ozt_spaces:add_user(Space, User),
        ozt_spaces:add_owner(Space, User),
        User
    end, lists:seq(1, UserCount)),

    AllOwners = lists_utils:shuffle([OriginalOwner | NewOwners]),

    Results = ozt:pmap(fun(User) ->
        {User, ozt:rpc(space_logic, remove_owner, [?ROOT, Space, User])}
    end, AllOwners),

    OwnersFailedToBeRemoved = lists:filtermap(fun({User, Result}) ->
        case Result of
            ok -> false;
            ?ERROR_CANNOT_REMOVE_LAST_OWNER(od_space, Space) -> {true, User}
        end
    end, Results),
    % only one of the owners should fail to be removed
    ?assertMatch([_], OwnersFailedToBeRemoved),
    ?assert(has_owners(Space, OwnersFailedToBeRemoved)).

adding_and_removing_owners_in_parallel_is_safe_from_race_conditions(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),

    FutureOwnersCount = 100,
    FutureOwners = lists:map(fun(_) ->
        User = ozt_users:create(),
        ozt_spaces:add_user(Space, User),
        User
    end, lists:seq(1, FutureOwnersCount)),

    PreviousOwnersCount = 100,
    PreviousOwners = lists:map(fun(_) ->
        User = ozt_users:create(),
        ozt_spaces:add_user(Space, User),
        ozt_spaces:add_owner(Space, User),
        User
    end, lists:seq(1, PreviousOwnersCount)),

    % add all non owners as owners and remove all owners except the original
    Operations = lists_utils:shuffle(lists:flatten([
        lists:zip(FutureOwners, lists:duplicate(FutureOwnersCount, add)),
        lists:zip(PreviousOwners, lists:duplicate(PreviousOwnersCount, remove))
    ])),

    ozt:pforeach(fun
        ({User, add}) -> ozt_spaces:add_owner(Space, User);
        ({User, remove}) -> ozt_spaces:remove_owner(Space, User)
    end, Operations),

    ?assert(has_owners(Space, [OriginalOwner | FutureOwners])).


adding_owner_while_removing_corresponding_member_is_safe_from_race_conditions(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),

    UserCount = 100,
    NewUsers = lists:map(fun(_) ->
        User = ozt_users:create(),
        ozt_spaces:add_user(Space, User),
        User
    end, lists:seq(1, UserCount)),

    % For each user, try to add the user as owner and remove him from space at
    % the same time. There are two possible, correct scenarios:
    %   1) the user is first made an owner and then removed
    %   2) the user is first removed and then making him an owner fails
    % Regardless of which scenario takes place, the user should not be in the
    % space nor be an owner afterwards.

    Results = ozt:pmap(fun(User) ->
        Ref = ozt:run_async(fun() ->
            timer:sleep(rand:uniform(2)),
            case rand:uniform(2) of
                1 -> ozt_spaces:remove_user(Space, User);
                2 -> ozt_users:leave_space(User, Space)
            end
        end),
        timer:sleep(rand:uniform(4)),
        Result = ozt:rpc(space_logic, add_owner, [?ROOT, Space, User]),
        ozt:await_async(Ref),
        {User, Result}
    end, NewUsers),

    lists:foreach(fun({User, Result}) ->
        case Result of
            ok -> ok;
            ?ERROR_RELATION_DOES_NOT_EXIST(od_space, Space, od_user, User) -> ok;
            Other -> ?assertMatch(unexpected_result, Other)
        end
    end, Results),

    ?assert(has_owners(Space, [OriginalOwner])),
    ?assert(has_direct_members(Space, [OriginalOwner])).

removing_last_member_being_owner_while_adding_a_new_member_is_safe_from_race_conditions(_Config) ->
    SpaceCount = 100,
    SpacesAndOwners = lists:map(fun(_) ->
        OriginalOwner = ozt_users:create(),
        Space = ozt_users:create_space_for(OriginalOwner),
        {Space, OriginalOwner}
    end, lists:seq(1, SpaceCount)),

    % For each space, try to remove the original owner and add a new member at
    % the same time. There are two possible, correct scenarios:
    %   1) the original owner is first removed and then a new member is added,
    %      becoming a new owner
    %   2) a new member is first added, which blocks the original owner from being
    %      removed from the space.

    Results = ozt:pmap(fun({Space, OriginalOwner}) ->
        NewUser = ozt_users:create(),
        Ref = ozt:run_async(fun() ->
            timer:sleep(rand:uniform(2)),
            case rand:uniform(2) of
                1 -> add_user_using_method(user_is_added_by_admin, Space, NewUser);
                2 -> add_user_using_method(user_joins_with_token, Space, NewUser)
            end
        end),
        timer:sleep(rand:uniform(5)),
        Result = case rand:uniform(2) of
            1 -> ozt:rpc(space_logic, remove_user, [?ROOT, Space, OriginalOwner]);
            2 -> ozt:rpc(user_logic, leave_space, [?ROOT, OriginalOwner, Space])
        end,
        ozt:await_async(Ref),
        {Space, OriginalOwner, NewUser, Result}
    end, SpacesAndOwners),

    lists:foreach(fun({Space, OriginalOwner, NewUser, Result}) ->
        case Result of
            ok ->
                % scenario 1)
                ?assert(has_owners(Space, [NewUser])),
                ?assert(has_direct_members(Space, [NewUser]));
            ?ERROR_CANNOT_REMOVE_LAST_OWNER(od_space, Space) ->
                % scenario 2)
                ?assert(has_owners(Space, [OriginalOwner])),
                ?assert(has_direct_members(Space, [OriginalOwner, NewUser]));
            Other ->
                ?assertMatch(unexpected_result, Other)
        end
    end, Results).


adding_an_indirect_user_as_owner_while_removing_him_indirectly_from_space_is_safe_from_race_conditions(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),

    IndirectUserCount = 100,
    IndirectUsersAndTheirGroups = lists:map(fun(_) ->
        User = ozt_users:create(),
        Group = ozt_users:create_group_for(User),
        ozt_spaces:add_group(Space, Group),
        {User, Group}
    end, lists:seq(1, IndirectUserCount)),
    ozt:reconcile_entity_graph(),

    % For each indirect user, try to make him an owner and at the same time remove his
    % effective relation in the space - remove his group or remove him from the group.
    % There are two possible, correct scenarios:
    %   1) the new owner is added and the user is made a direct member first, then
    %      his group or his group membership is removed
    %   2) user's group or his group membership is removed first, preventing him
    %      from becoming a new owner

    Results = ozt:pmap(fun({User, Group}) ->
        Ref = ozt:run_async(fun() ->
            timer:sleep(rand:uniform(2)),
            case rand:uniform(3) of
                1 -> ozt:rpc(space_logic, remove_group, [?ROOT, Space, Group]);
                2 -> ozt:rpc(group_logic, leave_space, [?ROOT, Group, Space]);
                3 -> ozt:rpc(group_logic, delete, [?ROOT, Group])
            end
        end),
        timer:sleep(rand:uniform(5)),
        Result = ozt:rpc(space_logic, add_owner, [?ROOT, Space, User]),
        ozt:await_async(Ref),
        {User, Group, Result}
    end, IndirectUsersAndTheirGroups),
    ozt:reconcile_entity_graph(),

    ExpectedOwners = lists:filtermap(fun({User, Group, Result}) ->
        ?assert(not has_group(Space, Group)),
        case Result of
            ok ->
                % scenario 1)
                {true, User};
            ?ERROR_RELATION_DOES_NOT_EXIST(od_space, Space, od_user, User) ->
                % scenario 2)
                false
        end
    end, Results),

    ?assert(has_owners(Space, [OriginalOwner | ExpectedOwners])),
    ?assert(has_direct_members(Space, [OriginalOwner | ExpectedOwners])),
    % there should be no other effective members that direct ones as all the
    % groups were removed from the space
    ?assert(has_eff_members(Space, [OriginalOwner | ExpectedOwners])).

adding_an_indirect_user_as_owner_while_removing_him_directly_from_space_is_safe_from_race_conditions(_Config) ->
    OriginalOwner = ozt_users:create(),
    Space = ozt_users:create_space_for(OriginalOwner),

    IndirectUserCount = 100,
    IndirectUsers = lists:map(fun(_) ->
        User = ozt_users:create(),
        Group = ozt_users:create_group_for(User),
        ozt_spaces:add_group(Space, Group),
        User
    end, lists:seq(1, IndirectUserCount)),
    ozt:reconcile_entity_graph(),

    % For each indirect user, try to make him an owner and at the same time remove his
    % direct membership in the space - assuming that adding as owner in the meantime might
    % have made him a direct member (it is done before marking the user as owner,
    % and without a transaction).
    % There are three possible, correct scenarios:
    %   1) the user is added as direct member and the ownership is granted first,
    %      then the user is removed as direct member, which makes him lose ownership
    %   2) the user is added as direct member but immediately removed before the
    %      user can be granted ownership - in such case the operation fails
    %   3) the removal request comes first and fails because the user is not yet
    %      a direct member, then the user is successfully added as direct member
    %      and granted ownership

    Results = ozt:pmap(fun(User) ->
        Master = self(),
        Ref = ozt:run_async(fun() ->
            timer:sleep(rand:uniform(2)),
            RemoveResult = case rand:uniform(2) of
                1 -> ozt:rpc(space_logic, remove_user, [?ROOT, Space, User]);
                2 -> ozt:rpc(user_logic, leave_space, [?ROOT, User, Space])
            end,
            Master ! {remove_result, RemoveResult}
        end),
        timer:sleep(rand:uniform(5)),
        AddOwnerResult = ozt:rpc(space_logic, add_owner, [?ROOT, Space, User]),
        ozt:await_async(Ref),
        {User, AddOwnerResult, receive {remove_result, R} -> R end}
    end, IndirectUsers),

    ExpectedOwners = lists:filtermap(fun({User, AddOwnerResult, RemoveResult}) ->
        case {AddOwnerResult, RemoveResult} of
            {ok, ok} ->
                % scenario 1)
                false;
            {?ERROR_RELATION_DOES_NOT_EXIST(od_space, Space, od_user, User), ok} ->
                % scenario 2)
                false;
            {ok, ?ERROR_NOT_FOUND} ->
                % scenario 3)
                {true, User};
            Other ->
                ?assertMatch(unexpected_result, Other)
        end
    end, Results),

    ?assert(has_owners(Space, [OriginalOwner | ExpectedOwners])),
    ?assert(has_direct_members(Space, [OriginalOwner | ExpectedOwners])).


%%%===================================================================
%%% Helper functions
%%%===================================================================

%% @private
is_owner(Space, User) ->
    #od_space{owners = Owners} = ozt_spaces:get(Space),
    lists:member(User, Owners).


%% @private
has_owners(Space, UsersToCheck) ->
    #od_space{owners = Owners} = ozt_spaces:get(Space),
    lists:sort(UsersToCheck) =:= lists:sort(Owners).


%% @private
is_direct_member(Space, User) ->
    #od_space{users = Users} = ozt_spaces:get(Space),
    maps:is_key(User, Users).


%% @private
has_direct_members(Space, UsersToCheck) ->
    #od_space{users = Users} = ozt_spaces:get(Space),
    lists:sort(UsersToCheck) =:= lists:sort(maps:keys(Users)).


%% @private
has_eff_members(Space, UsersToCheck) ->
    #od_space{eff_users = EffUsers} = ozt_spaces:get(Space),
    lists:sort(UsersToCheck) =:= lists:sort(maps:keys(EffUsers)).


%% @private
has_direct_privileges(Space, User, PrivilegesToCheck) ->
    #od_space{users = Users} = ozt_spaces:get(Space),
    lists:sort(PrivilegesToCheck) =:= lists:sort(maps:get(User, Users)).


%% @private
has_each_of_privileges(Space, User, PrivilegesToCheck) ->
    lists:all(fun(Privilege) ->
        ozt:rpc(space_logic, has_eff_privilege, [Space, User, Privilege])
    end, PrivilegesToCheck).


%% @private
has_none_of_privileges(Space, User, PrivilegesToCheck) ->
    not lists:any(fun(Privilege) ->
        ozt:rpc(space_logic, has_eff_privilege, [Space, User, Privilege])
    end, PrivilegesToCheck).


%% @private
has_group(Space, Group) ->
    #od_space{groups = Groups} = ozt_spaces:get(Space),
    maps:is_key(Group, Groups).


%% @private
user_has_space(User, Space) ->
    #od_user{spaces = Spaces} = ozt_users:get(User),
    lists:member(Space, Spaces).


%% @private
user_has_eff_space(User, Space) ->
    #od_user{eff_spaces = EffSpaces} = ozt_users:get(User),
    maps:is_key(Space, EffSpaces).


%% @private
add_user_using_method(user_is_added_by_admin, Space, User) ->
    ozt_spaces:add_user(Space, User);
add_user_using_method(user_joins_with_token, Space, User) ->
    OzAdmin = ozt_users:create_admin(),
    InviteToken = ozt_spaces:create_user_invite_token(Space, OzAdmin),
    ozt_users:join_space(User, InviteToken).
