%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018-2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests for IdP entitlement mapping onto Onedata groups.
%%% @end
%%%-------------------------------------------------------------------
-module(entitlement_mapping_test_SUITE).
-author("Lukasz Opiola").

-include("auth/entitlement_mapping.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-define(CUSTOM_ENTITLEMENT_PARSER, my_custom_entitlement_parser).

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    entitlement_mapping_disabled_upon_first_login/1,
    entitlement_mapping_disabled_upon_consecutive_login/1,
    entitlement_mapping_disabled_upon_account_link/1,
    entitlement_mapping_disabled_upon_non_idp_user_account_link/1,
    no_entitlements_upon_first_login/1,
    no_entitlements_upon_consecutive_login/1,
    no_entitlements_upon_account_link/1,
    no_entitlements_upon_non_idp_user_account_link/1,
    entitlements_added_upon_first_login/1,
    entitlements_added_upon_consecutive_login/1,
    entitlements_removed_upon_consecutive_login/1,
    entitlements_coalesced_upon_consecutive_login/1,
    entitlements_added_with_admin_group_upon_account_link/1,
    entitlements_added_with_admin_group_upon_consecutive_login_after_enabling_entitlement_mapping/1,
    entitlements_removed_upon_consecutive_login_after_disabling_entitlement_mapping/1,
    entitlements_added_with_vo_group_upon_first_login/1,
    entitlements_added_with_vo_group_upon_consecutive_login/1,
    entitlements_added_with_admin_and_vo_group_upon_first_login/1,
    entitlements_added_with_admin_and_vo_group_upon_consecutive_login/1,
    highest_role_prevails_when_entitlements_are_duplicated/1,
    privileges_are_modified_on_consecutive_login/1,
    entitlements_coalesced_correctly_in_a_mixed_scenario/1,
    entitlement_groups_are_protected/1
]).

all() ->
    ?ALL([
        entitlement_mapping_disabled_upon_first_login,
        entitlement_mapping_disabled_upon_consecutive_login,
        entitlement_mapping_disabled_upon_account_link,
        entitlement_mapping_disabled_upon_non_idp_user_account_link,
        no_entitlements_upon_first_login,
        no_entitlements_upon_consecutive_login,
        no_entitlements_upon_account_link,
        no_entitlements_upon_non_idp_user_account_link,
        entitlements_added_upon_first_login,
        entitlements_added_upon_consecutive_login,
        entitlements_removed_upon_consecutive_login,
        entitlements_coalesced_upon_consecutive_login,
        entitlements_added_with_admin_group_upon_account_link,
        entitlements_added_with_admin_group_upon_consecutive_login_after_enabling_entitlement_mapping,
        entitlements_removed_upon_consecutive_login_after_disabling_entitlement_mapping,
        entitlements_added_with_vo_group_upon_first_login,
        entitlements_added_with_vo_group_upon_consecutive_login,
        entitlements_added_with_admin_and_vo_group_upon_first_login,
        entitlements_added_with_admin_and_vo_group_upon_consecutive_login,
        highest_role_prevails_when_entitlements_are_duplicated,
        privileges_are_modified_on_consecutive_login,
        entitlements_coalesced_correctly_in_a_mixed_scenario,
        entitlement_groups_are_protected
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================

-define(DUMMY_IDP, dummyIdP).
-define(OTHER_IDP, anotherIdP).
-define(THIRD_IDP, thirdIdP).

-define(LINKED_ACC(IdP, Entitlements), #linked_account{
    idp = IdP,
    subject_id = <<(atom_to_binary(IdP, utf8))/binary, "-subjectId">>,
    entitlements = Entitlements
}).

% Below macros assume that set_test_data/2 function has been called at the
% beginning of the tests and reuse data stored in process dictionary for
% clearer test code
-define(RETRIES, 100).
-define(INTERVAL, 100). % 100 Attempts every 100 ms - 10 seconds

-define(assertEqualWithRetries(TermA, TermB),
    ?assertEqual(TermA, TermB, ?RETRIES, ?INTERVAL)
).

-define(assertGroupExists(Flag, IdP, RawEntitlement),
    ?assertEqual(
        Flag, group_exists(get_test_config(), IdP, RawEntitlement)
    )
).

-define(assertGroupProtected(IdP, RawEntitlement),
    ?assert(
        is_protected(get_test_config(), IdP, RawEntitlement)
    )
).

-define(assertTotalGroupsCount(Count),
    ?assertEqual(Count, total_group_count(get_test_config()))
).

-define(assertHasGroup(Flag, IdP, RawEntitlement, RelationType),
    ?assertEqualWithRetries(
        Flag, has_group(get_test_config(), get_test_user(), IdP, RawEntitlement, RelationType)
    )
).

-define(assertUserGroupsCount(Direct, Effective),
    ?assertEqualWithRetries(
        {Direct, Effective}, get_groups_count(get_test_config(), get_test_user())
    )
).

-define(assertGroupStructure(IdP, ParentRawEntitlement, ChildRawEntitlement, RelationType),
    ?assertEqualWithRetries(true, check_group_structure(
        get_test_config(), IdP, ParentRawEntitlement, ChildRawEntitlement, RelationType
    ))
).

-define(assertHasLinkedAccount(LinkedAcc),
    ?assert(
        has_linked_account(get_test_config(), get_test_user(), LinkedAcc)
    )
).

-define(assertLinkedAccountsCount(Number),
    ?assertEqual(
        Number, length(get_linked_accounts(get_test_config(), get_test_user()))
    )
).


entitlement_mapping_disabled_upon_first_login(Config) ->
    store_test_config(Config),
    overwrite_config(?DUMMY_IDP, false),
    simulate_first_login(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>]),
    ?assertUserGroupsCount(0, 0),
    ?assertTotalGroupsCount(0).


entitlement_mapping_disabled_upon_consecutive_login(Config) ->
    store_test_config(Config),
    overwrite_config(?DUMMY_IDP, false),
    simulate_first_login(?DUMMY_IDP, []),
    simulate_consecutive_login(?DUMMY_IDP, [<<"group/subgroup">>, <<"another">>]),
    ?assertUserGroupsCount(0, 0),
    ?assertTotalGroupsCount(0).


entitlement_mapping_disabled_upon_account_link(Config) ->
    store_test_config(Config),
    overwrite_config(?DUMMY_IDP, false),
    overwrite_config(?OTHER_IDP, false),
    simulate_first_login(?DUMMY_IDP, []),
    simulate_account_link(?OTHER_IDP, [<<"group/subgroup">>, <<"another">>]),
    ?assertUserGroupsCount(0, 0),
    ?assertTotalGroupsCount(0).


entitlement_mapping_disabled_upon_non_idp_user_account_link(Config) ->
    store_test_config(Config),
    overwrite_config(?DUMMY_IDP, false),
    create_non_idp_user(),
    simulate_account_link(?DUMMY_IDP, [<<"group/subgroup">>, <<"another">>]),
    ?assertUserGroupsCount(0, 0),
    ?assertTotalGroupsCount(0).


no_entitlements_upon_first_login(Config) ->
    store_test_config(Config),
    overwrite_config(?DUMMY_IDP, true, flat_entitlement_parser),
    simulate_first_login(?DUMMY_IDP, []),
    ?assertUserGroupsCount(0, 0),
    ?assertTotalGroupsCount(0).


no_entitlements_upon_consecutive_login(Config) ->
    store_test_config(Config),
    overwrite_config(?DUMMY_IDP, true, flat_entitlement_parser),
    simulate_first_login(?DUMMY_IDP, []),
    simulate_consecutive_login(?DUMMY_IDP, []),
    ?assertUserGroupsCount(0, 0),
    ?assertTotalGroupsCount(0).


no_entitlements_upon_account_link(Config) ->
    store_test_config(Config),
    overwrite_config(?DUMMY_IDP, true, flat_entitlement_parser),
    overwrite_config(?OTHER_IDP, true, flat_entitlement_parser),
    simulate_first_login(?DUMMY_IDP, []),
    simulate_account_link(?OTHER_IDP, []),
    ?assertUserGroupsCount(0, 0),
    ?assertTotalGroupsCount(0).


no_entitlements_upon_non_idp_user_account_link(Config) ->
    store_test_config(Config),
    overwrite_config(?DUMMY_IDP, true, flat_entitlement_parser),
    create_non_idp_user(),
    simulate_account_link(?DUMMY_IDP, []),
    ?assertUserGroupsCount(0, 0),
    ?assertTotalGroupsCount(0).


entitlements_added_upon_first_login(Config) ->
    store_test_config(Config),
    overwrite_config(?DUMMY_IDP, true, flat_entitlement_parser),
    simulate_first_login(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>]),
    ?assertUserGroupsCount(2, 2),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"thirdGroup">>, direct),
    ?assertTotalGroupsCount(2),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"group/subgroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"anotherGroup">>),
    ?assertGroupExists(false, ?DUMMY_IDP, <<"thirdGroup">>).


entitlements_added_upon_consecutive_login(Config) ->
    store_test_config(Config),
    overwrite_config(?DUMMY_IDP, true, flat_entitlement_parser),
    simulate_first_login(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>]),
    simulate_consecutive_login(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>, <<"thirdGroup">>]),
    ?assertUserGroupsCount(3, 3),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"thirdGroup">>, direct),
    ?assertTotalGroupsCount(3),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"group/subgroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"thirdGroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"anotherGroup">>).


entitlements_removed_upon_consecutive_login(Config) ->
    store_test_config(Config),
    overwrite_config(?DUMMY_IDP, true, flat_entitlement_parser),
    simulate_first_login(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>]),
    simulate_consecutive_login(?DUMMY_IDP, []),
    ?assertUserGroupsCount(0, 0),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertTotalGroupsCount(2),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"group/subgroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"anotherGroup">>).


entitlements_coalesced_upon_consecutive_login(Config) ->
    store_test_config(Config),
    overwrite_config(?DUMMY_IDP, true, flat_entitlement_parser),
    simulate_first_login(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>]),
    simulate_consecutive_login(?DUMMY_IDP, [<<"firstGroup">>, <<"anotherGroup">>, <<"thirdGroup">>]),
    ?assertUserGroupsCount(3, 3),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"firstGroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"thirdGroup">>, direct),
    ?assertTotalGroupsCount(4),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"group/subgroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"firstGroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"anotherGroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"thirdGroup">>).


entitlements_added_with_admin_group_upon_account_link(Config) ->
    store_test_config(Config),
    overwrite_config(?DUMMY_IDP, true, flat_entitlement_parser),
    overwrite_config(?OTHER_IDP, true, nested_entitlement_parser, [{adminGroup, "users/admins"}]),
    simulate_first_login(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>]),
    simulate_account_link(?OTHER_IDP, [<<"users/admins">>, <<"users/developers">>, <<"users/technicians">>]),
    ?assertUserGroupsCount(5, 6),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/admins">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/developers">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/technicians">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users">>, effective),
    ?assertTotalGroupsCount(6),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"group/subgroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"anotherGroup">>),
    ?assertGroupExists(true, ?OTHER_IDP, <<"users">>),
    ?assertGroupExists(true, ?OTHER_IDP, <<"users/admins">>),
    ?assertGroupExists(true, ?OTHER_IDP, <<"users/developers">>),
    ?assertGroupExists(true, ?OTHER_IDP, <<"users/technicians">>),
    ?assertGroupStructure(?OTHER_IDP, <<"users">>, <<"users/admins">>, direct),
    ?assertGroupStructure(?OTHER_IDP, <<"users">>, <<"users/developers">>, direct),
    ?assertGroupStructure(?OTHER_IDP, <<"users">>, <<"users/technicians">>, direct),
    % The admin group should belong to all groups
    ?assertGroupStructure(?OTHER_IDP, <<"users/developers">>, <<"users/admins">>, direct),
    ?assertGroupStructure(?OTHER_IDP, <<"users/technicians">>, <<"users/admins">>, direct).


entitlements_added_with_admin_group_upon_consecutive_login_after_enabling_entitlement_mapping(Config) ->
    store_test_config(Config),
    overwrite_config(?OTHER_IDP, false),
    simulate_first_login(?OTHER_IDP, [<<"users/admins">>, <<"users/developers">>, <<"users/technicians">>]),
    ?assertUserGroupsCount(0, 0),
    ?assertTotalGroupsCount(0),
    overwrite_config(?OTHER_IDP, true, nested_entitlement_parser, [{adminGroup, "users/admins"}]),
    simulate_consecutive_login(?OTHER_IDP, [<<"users/admins">>, <<"users/developers">>, <<"users/technicians">>]),
    ?assertUserGroupsCount(3, 4),
    ?assertTotalGroupsCount(4).


entitlements_removed_upon_consecutive_login_after_disabling_entitlement_mapping(Config) ->
    store_test_config(Config),
    overwrite_config(?OTHER_IDP, true, nested_entitlement_parser, [{adminGroup, "users/admins"}]),
    simulate_first_login(?OTHER_IDP, [<<"users/admins">>, <<"users/developers">>, <<"users/technicians">>]),
    ?assertUserGroupsCount(3, 4),
    ?assertTotalGroupsCount(4),
    overwrite_config(?OTHER_IDP, false),
    simulate_consecutive_login(?OTHER_IDP, [<<"users/admins">>, <<"users/developers">>, <<"users/technicians">>]),
    ?assertUserGroupsCount(0, 0),

    ?assertTotalGroupsCount(4),
    % Group structure should remain unchanged though the entitlements are removed from the user
    % config need to be present for the group structure checks to work
    overwrite_config(?OTHER_IDP, true, nested_entitlement_parser, [{adminGroup, "users/admins"}]),
    ?assertGroupStructure(?OTHER_IDP, <<"users">>, <<"users/admins">>, direct),
    ?assertGroupStructure(?OTHER_IDP, <<"users">>, <<"users/developers">>, direct),
    ?assertGroupStructure(?OTHER_IDP, <<"users">>, <<"users/technicians">>, direct),
    ?assertGroupStructure(?OTHER_IDP, <<"users/developers">>, <<"users/admins">>, direct),
    ?assertGroupStructure(?OTHER_IDP, <<"users/technicians">>, <<"users/admins">>, direct).


entitlements_added_with_vo_group_upon_first_login(Config) ->
    store_test_config(Config),
    overwrite_config(?THIRD_IDP, true, nested_entitlement_parser, [{voGroupName, "Third-VO"}]),
    simulate_first_login(?THIRD_IDP, [
        <<"staff/vm-operators">>, <<"task4.1">>, <<"testGroup">>, <<"staff/admins/privileged">>
    ]),
    ?assertUserGroupsCount(4, 7),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff/vm-operators">>, direct),
    ?assertHasGroup(true, ?THIRD_IDP, <<"task4.1">>, direct),
    ?assertHasGroup(true, ?THIRD_IDP, <<"testGroup">>, direct),
    ?assertHasGroup(true, ?THIRD_IDP, <<"Third-VO">>, effective),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff">>, effective),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff/admins">>, effective),

    ?assertTotalGroupsCount(7),
    % All groups should belong to their parents and the top parent to the VO group
    ?assertGroupStructure(?THIRD_IDP, <<"staff">>, <<"staff/vm-operators">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"staff">>, <<"staff/admins">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"staff/admins">>, <<"staff/admins/privileged">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"staff">>, <<"staff/admins/privileged">>, effective),
    ?assertGroupStructure(?THIRD_IDP, <<"Third-VO">>, <<"staff">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"Third-VO">>, <<"staff/vm-operators">>, effective),
    ?assertGroupStructure(?THIRD_IDP, <<"Third-VO">>, <<"staff/admins">>, effective),
    ?assertGroupStructure(?THIRD_IDP, <<"Third-VO">>, <<"staff/admins/privileged">>, effective).


entitlements_added_with_vo_group_upon_consecutive_login(Config) ->
    store_test_config(Config),
    overwrite_config(?THIRD_IDP, true, nested_entitlement_parser, [{voGroupName, "Third-VO"}]),
    simulate_first_login(?THIRD_IDP, []),
    ?assertUserGroupsCount(0, 0),
    ?assertTotalGroupsCount(0),
    simulate_consecutive_login(?THIRD_IDP, [
        <<"staff/vm-operators">>, <<"task4.1">>, <<"testGroup">>,
        <<"staff/admins/readonly">>, <<"staff/admins/privileged">>
    ]),
    ?assertUserGroupsCount(5, 8),
    ?assertTotalGroupsCount(8).


entitlements_added_with_admin_and_vo_group_upon_first_login(Config) ->
    store_test_config(Config),
    overwrite_config(?THIRD_IDP, true, ?CUSTOM_ENTITLEMENT_PARSER, [
        {voGroupName, "Third-VO"}, {adminGroup, "staff/admins/privileged"}
    ]),
    simulate_first_login(?THIRD_IDP, [
        <<"staff:member/vm-operators:member/user:manager">>,
        <<"task4.1:manager/user:member">>,
        <<"testGroup:admin/user:admin">>,
        <<"staff:member/admins:member/readonly:member/user:member">>,
        <<"staff:member/admins:member/privileged:admin/user:manager">>
    ]),
    ?assertUserGroupsCount(5, 8),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff:member/vm-operators:member/user:manager">>, direct),
    ?assertHasGroup(true, ?THIRD_IDP, <<"task4.1:manager/user:member">>, direct),
    ?assertHasGroup(true, ?THIRD_IDP, <<"testGroup:admin/user:admin">>, direct),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff:member/admins:member/readonly:member/user:member">>, direct),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff:member/admins:member/privileged:admin/user:manager">>, direct),
    ?assertHasGroup(true, ?THIRD_IDP, <<"Third-VO">>, effective),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff:member">>, effective),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff:member/admins:member">>, effective),

    ?assertTotalGroupsCount(8),
    % All groups should belong to their parents and the top parent to the VO group
    ?assertGroupStructure(?THIRD_IDP,
        <<"staff:member">>, <<"staff:member/vm-operators:member/user:manager">>, direct),
    ?assertGroupStructure(?THIRD_IDP,
        <<"staff:member">>, <<"staff:member/admins:member">>, direct),
    ?assertGroupStructure(?THIRD_IDP,
        <<"staff:member/admins:member">>, <<"staff:member/admins:member/readonly:member/user:member">>, direct),
    ?assertGroupStructure(?THIRD_IDP,
        <<"staff:member/admins:member">>, <<"staff:member/admins:member/privileged:admin/user:manager">>, direct),
    ?assertGroupStructure(?THIRD_IDP,
        <<"staff:member">>, <<"staff:member/admins:member/readonly:member/user:member">>, effective),
    ?assertGroupStructure(?THIRD_IDP,
        <<"staff:member">>, <<"staff:member/admins:member/privileged:admin/user:manager">>, effective),
    ?assertGroupStructure(?THIRD_IDP,
        <<"Third-VO">>, <<"staff:member">>, direct),
    ?assertGroupStructure(?THIRD_IDP,
        <<"Third-VO">>, <<"staff:member/vm-operators:member/user:manager">>, effective),
    ?assertGroupStructure(?THIRD_IDP,
        <<"Third-VO">>, <<"staff:member/admins:member">>, effective),
    ?assertGroupStructure(?THIRD_IDP,
        <<"Third-VO">>, <<"staff:member/admins:member/readonly:member/user:member">>, effective),
    ?assertGroupStructure(?THIRD_IDP,
        <<"Third-VO">>, <<"staff:member/admins:member/privileged:admin/user:manager">>, effective),
    % The admin group should belong to all groups
    ?assertGroupStructure(?THIRD_IDP,
        <<"staff:member">>, <<"staff:member/admins:member/privileged:admin">>, direct),
    ?assertGroupStructure(?THIRD_IDP,
        <<"staff:member/admins:member">>, <<"staff:member/admins:member/privileged:admin">>, direct),
    ?assertGroupStructure(?THIRD_IDP,
        <<"staff:member/admins:member/readonly:member/user:member">>, <<"staff:member/admins:member/privileged:admin">>, direct),
    ?assertGroupStructure(?THIRD_IDP,
        <<"task4.1:manager/user:member">>, <<"staff:member/admins:member/privileged:admin">>, direct),
    ?assertGroupStructure(?THIRD_IDP,
        <<"testGroup:admin/user:admin">>, <<"staff:member/admins:member/privileged:admin">>, direct),
    ?assertGroupStructure(?THIRD_IDP,
        <<"Third-VO">>, <<"staff:member/admins:member/privileged:admin">>, direct).


entitlements_added_with_admin_and_vo_group_upon_consecutive_login(Config) ->
    store_test_config(Config),
    overwrite_config(?THIRD_IDP, true, ?CUSTOM_ENTITLEMENT_PARSER, [
        {voGroupName, "Third-VO"}, {adminGroup, "staff/admins/privileged"}
    ]),
    simulate_first_login(?THIRD_IDP, []),
    ?assertUserGroupsCount(0, 0),
    ?assertTotalGroupsCount(0),
    simulate_consecutive_login(?THIRD_IDP, [
        <<"staff:member/vm-operators:member/user:manager">>,
        <<"task4.1:manager/user:member">>,
        <<"testGroup:admin/user:admin">>,
        <<"staff:member/admins:member/readonly:member/user:member">>,
        <<"staff:member/admins:member/privileged:admin/user:manager">>
    ]),
    ?assertUserGroupsCount(5, 8),
    ?assertTotalGroupsCount(8).


highest_role_prevails_when_entitlements_are_duplicated(Config) ->
    store_test_config(Config),
    overwrite_config(?THIRD_IDP, true, ?CUSTOM_ENTITLEMENT_PARSER, [
        {voGroupName, "Third-VO"}, {adminGroup, "staff/admins/privileged"}
    ]),

    simulate_first_login(?THIRD_IDP, [
        <<"staff:member/vm-operators:member/user:manager">>,
        <<"staff:member/vm-operators:member/user:admin">>,
        <<"staff:member/vm-operators:member/user:member">>
    ]),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff:member/vm-operators:member/user:admin">>, direct),

    simulate_consecutive_login(?THIRD_IDP, [
        <<"staff:member/vm-operators:member/user:member">>,
        <<"staff:member/vm-operators:member/user:admin">>
    ]),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff:member/vm-operators:member/user:admin">>, direct),

    simulate_consecutive_login(?THIRD_IDP, [
        <<"staff:member/vm-operators:member/user:admin">>,
        <<"staff:member/vm-operators:member/user:manager">>
    ]),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff:member/vm-operators:member/user:admin">>, direct),

    simulate_consecutive_login(?THIRD_IDP, [
        <<"staff:member/vm-operators:member/user:member">>,
        <<"staff:member/vm-operators:member/user:manager">>
    ]),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff:member/vm-operators:member/user:manager">>, direct),

    simulate_consecutive_login(?THIRD_IDP, [
        <<"staff:member/vm-operators:member/user:member">>,
        <<"staff:member/vm-operators:member/user:member">>
    ]),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff:member/vm-operators:member/user:member">>, direct).


privileges_are_modified_on_consecutive_login(Config) ->
    store_test_config(Config),
    overwrite_config(?THIRD_IDP, true, ?CUSTOM_ENTITLEMENT_PARSER, [
        {voGroupName, "Third-VO"}, {adminGroup, "staff/admins/privileged"}
    ]),

    simulate_first_login(?THIRD_IDP, [<<"staff:member/vm-operators:member/user:member">>]),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff:member/vm-operators:member/user:member">>, direct),

    simulate_first_login(?THIRD_IDP, [<<"staff:admin/vm-operators:manager/user:admin">>]),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff:admin/vm-operators:manager/user:admin">>, direct),

    simulate_first_login(?THIRD_IDP, [<<"staff:manager/vm-operators:member/user:manager">>]),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff:manager/vm-operators:member/user:manager">>, direct),

    simulate_first_login(?THIRD_IDP, [<<"staff:member/vm-operators:admin/user:admin">>]),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff:member/vm-operators:admin/user:admin">>, direct),

    simulate_first_login(?THIRD_IDP, [<<"staff:member/vm-operators:manager/user:member">>]),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff:member/vm-operators:manager/user:member">>, direct),

    simulate_first_login(?THIRD_IDP, []),
    ?assertHasGroup(false, ?THIRD_IDP, <<"staff:member/vm-operators:manager/user:member">>, direct),

    simulate_first_login(?THIRD_IDP, [<<"staff:admin/vm-operators:member/user:manager">>]),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff:admin/vm-operators:member/user:manager">>, direct).


entitlements_coalesced_correctly_in_a_mixed_scenario(Config) ->
    store_test_config(Config),
    overwrite_config(?DUMMY_IDP, true, flat_entitlement_parser),
    overwrite_config(?OTHER_IDP, true, nested_entitlement_parser, [
        {adminGroup, "users/admins"}
    ]),
    overwrite_config(?THIRD_IDP, true, ?CUSTOM_ENTITLEMENT_PARSER, [
        {voGroupName, "Third-VO"}, {adminGroup, "staff/admins/privileged"}
    ]),

    create_non_idp_user(),
    ?assertUserGroupsCount(0, 0),
    ?assertTotalGroupsCount(0),

    simulate_account_link(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>]),
    ?assertUserGroupsCount(2, 2),
    ?assertTotalGroupsCount(2),

    simulate_account_link(?OTHER_IDP, [<<"users/admins">>, <<"users/developers">>, <<"users/technicians">>]),
    ?assertUserGroupsCount(5, 6),
    ?assertTotalGroupsCount(6),

    overwrite_config(?DUMMY_IDP, false),
    simulate_consecutive_login(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>]),
    ?assertUserGroupsCount(3, 4),
    ?assertTotalGroupsCount(6),

    overwrite_config(?DUMMY_IDP, true, flat_entitlement_parser),
    simulate_account_link(?THIRD_IDP, [
        <<"staff:member/vm-operators:member/user:manager">>,
        <<"task4.1:manager/user:member">>,
        <<"testGroup:admin/user:admin">>,
        <<"staff:member/admins:member/readonly:member/user:member">>,
        <<"staff:member/admins:member/privileged:admin/user:manager">>
    ]),
    ?assertUserGroupsCount(10, 14),
    ?assertTotalGroupsCount(14),

    simulate_consecutive_login(?OTHER_IDP, []),
    ?assertUserGroupsCount(7, 10),
    ?assertTotalGroupsCount(14),

    simulate_consecutive_login(?OTHER_IDP, [<<"users/admins">>]),
    ?assertUserGroupsCount(8, 14),  % Admins group belongs to all groups from ?OTHER_IDP
    ?assertTotalGroupsCount(14),

    overwrite_config(?DUMMY_IDP, false),
    overwrite_config(?OTHER_IDP, false),
    overwrite_config(?THIRD_IDP, false),
    simulate_consecutive_login(?OTHER_IDP, [<<"users/admins">>, <<"users/developers">>, <<"users/technicians">>]),

    ?assertUserGroupsCount(0, 0),
    ?assertTotalGroupsCount(14),

    overwrite_config(?DUMMY_IDP, true, flat_entitlement_parser),
    overwrite_config(?OTHER_IDP, true, nested_entitlement_parser, [
        {adminGroup, "users/admins"}
    ]),
    overwrite_config(?THIRD_IDP, true, ?CUSTOM_ENTITLEMENT_PARSER, [
        {voGroupName, "Third-VO"}, {adminGroup, "staff/admins/privileged"}
    ]),
    simulate_consecutive_login(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>]),
    ?assertUserGroupsCount(10, 14),
    ?assertTotalGroupsCount(14).


entitlement_groups_are_protected(Config) ->
    store_test_config(Config),
    overwrite_config(?DUMMY_IDP, true, flat_entitlement_parser),
    overwrite_config(?OTHER_IDP, true, nested_entitlement_parser, [
        {adminGroup, "users/admins"}
    ]),
    overwrite_config(?THIRD_IDP, true, ?CUSTOM_ENTITLEMENT_PARSER, [
        {voGroupName, "Third-VO"}, {adminGroup, "staff/admins/privileged"}
    ]),

    create_non_idp_user(),
    simulate_account_link(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>, <<"thirdGroup">>]),
    simulate_account_link(?OTHER_IDP, [<<"users/admins">>, <<"users/developers">>, <<"users/technicians">>]),
    simulate_account_link(?THIRD_IDP, [
        <<"staff:member/vm-operators:member/user:manager">>,
        <<"task4.1:manager/user:member">>,
        <<"testGroup:admin/user:admin">>,
        <<"staff:member/admins:member/readonly:member/user:member">>,
        <<"staff:member/admins:member/privileged:admin/user:manager">>
    ]),

    ?assertGroupProtected(?DUMMY_IDP, <<"group/subgroup">>),
    ?assertGroupProtected(?DUMMY_IDP, <<"anotherGroup">>),
    ?assertGroupProtected(?DUMMY_IDP, <<"thirdGroup">>),

    ?assertGroupProtected(?OTHER_IDP, <<"users">>),
    ?assertGroupProtected(?OTHER_IDP, <<"users/admins">>),
    ?assertGroupProtected(?OTHER_IDP, <<"users/developers">>),
    ?assertGroupProtected(?OTHER_IDP, <<"users/technicians">>),

    ?assertGroupProtected(?THIRD_IDP, <<"staff:member">>),
    ?assertGroupProtected(?THIRD_IDP, <<"staff:member/vm-operators:member/user:manager">>),
    ?assertGroupProtected(?THIRD_IDP, <<"staff:member/admins:member">>),
    ?assertGroupProtected(?THIRD_IDP, <<"staff:member/admins:member/privileged:admin/user:manager">>),
    ?assertGroupProtected(?THIRD_IDP, <<"staff:member/admins:member/readonly:member/user:member">>),
    ?assertGroupProtected(?THIRD_IDP, <<"task4.1:manager/user:member">>),
    ?assertGroupProtected(?THIRD_IDP, <<"testGroup:admin/user:admin">>).

%%%===================================================================
%%% Internal functions
%%%===================================================================

store_test_config(Config) ->
    put(test_data_config, Config).


get_test_config() ->
    case get(test_data_config) of
        undefined -> error("Call init_test at the beggining of the test.");
        Config -> Config
    end.


create_non_idp_user() ->
    Config = get_test_config(),
    {ok, UserId} = oz_test_utils:create_user(Config),
    put(test_data_user, UserId),
    UserId.


simulate_first_login(IdP, Entitlements) ->
    LinkedAccount = ?LINKED_ACC(IdP, Entitlements),
    Config = get_test_config(),
    {ok, #document{key = UserId}} = oz_test_utils:call_oz(
        Config, linked_accounts, acquire_user, [LinkedAccount]
    ),
    put(test_data_user, UserId),
    ?assertHasLinkedAccount(LinkedAccount),
    ?assertLinkedAccountsCount(1),
    UserId.


simulate_consecutive_login(IdP, Entitlements) ->
    LinkedAccount = ?LINKED_ACC(IdP, Entitlements),
    Config = get_test_config(),
    UserId = get_test_user(),
    LinkedAccountsCount = length(get_linked_accounts(Config, UserId)),
    case oz_test_utils:call_oz(Config, linked_accounts, find_user, [LinkedAccount]) of
        {ok, #document{key = UserId}} ->
            oz_test_utils:call_oz(Config, linked_accounts, merge, [UserId, LinkedAccount]),
            ?assertHasLinkedAccount(LinkedAccount),
            ?assertLinkedAccountsCount(LinkedAccountsCount);
        _ ->
            % Make sure the test code makes sense
            error("This user does not have this account linked.")
    end.


simulate_account_link(IdP, Entitlements) ->
    LinkedAccount = ?LINKED_ACC(IdP, Entitlements),
    Config = get_test_config(),
    UserId = get_test_user(),
    LinkedAccountsCount = length(get_linked_accounts(Config, UserId)),
    case oz_test_utils:call_oz(Config, linked_accounts, find_user, [LinkedAccount]) of
        {error, not_found} ->
            oz_test_utils:call_oz(Config, linked_accounts, merge, [UserId, LinkedAccount]),
            ?assertHasLinkedAccount(LinkedAccount),
            ?assertLinkedAccountsCount(LinkedAccountsCount + 1);
        _ ->
            % Make sure the test code makes sense
            error("This account is already linked.")
    end.


get_test_user() ->
    case get(test_data_user) of
        undefined -> error("Call simulate_first_login or create_non_idp_user first.");
        UserId -> UserId
    end.


group_exists(Config, IdP, RawEntitlement) ->
    try
        IdPEntitlement = expected_parsing_result(Config, IdP, RawEntitlement),
        GroupId = entitlement_mapping:gen_group_id(IdPEntitlement),
        oz_test_utils:call_oz(Config, group_logic, exists, [GroupId])
    catch _:_ ->
        false
    end.


total_group_count(Config) ->
    {ok, Groups} = oz_test_utils:list_groups(Config),
    length(Groups).


%% RelationType :: direct | effective
has_group(Config, UserId, IdP, RawEntitlement, RelationType) ->
    try
        IdPEntitlement = #idp_entitlement{
            idp = IdP,
            path = Path,
            privileges = Privileges
        } = expected_parsing_result(Config, IdP, RawEntitlement),
        #idp_group{name = Name, type = Type} = lists:last(Path),
        NormalizedName = entity_logic:normalize_name(Name),
        GroupId = entitlement_mapping:gen_group_id(IdPEntitlement),

        UserGroups = get_groups(Config, UserId, RelationType),
        BelongsToGroup = lists:member(GroupId, UserGroups),
        {ok, #od_group{
            name = GroupName, type = GroupType
        }} = oz_test_utils:call_oz(Config, group_logic, get, [?ROOT, GroupId]),
        NameAndTypeMatch = GroupName =:= NormalizedName andalso GroupType =:= Type,
        case BelongsToGroup andalso NameAndTypeMatch of
            false ->
                false;
            true ->
                case RelationType of
                    direct ->
                        {ok, UserPrivileges} = oz_test_utils:call_oz(
                            Config, group_logic, get_user_privileges, [?ROOT, GroupId, UserId]
                        ),
                        UserPrivileges =:= entitlement_mapping:map_privileges(Privileges);
                    effective ->
                        % Do not check effective privileges as they are hard
                        % to predict - might be inherited via different
                        % membership paths than the one currently examined
                        true
                end
        end
    catch _:_ ->
        false
    end.


get_groups_count(Config, UserId) ->
    DirectCount = length(get_groups(Config, UserId, direct)),
    EffectiveCount = length(get_groups(Config, UserId, effective)),
    EntitlementsCount = length(get_entitlements(Config, UserId)),
    ?assertEqual(EntitlementsCount, DirectCount),
    {DirectCount, EffectiveCount}.


check_group_structure(Config, IdP, ParentRawEntitlement, ChildRawEntitlement, RelationType) ->
    try
        ParentEntitlement = #idp_entitlement{
            idp = IdP,
            path = ParentPath
        } = expected_parsing_result(Config, IdP, ParentRawEntitlement),
        ChildEntitlement = #idp_entitlement{
            idp = IdP,
            path = ChildPath
        } = expected_parsing_result(Config, IdP, ChildRawEntitlement),
        #idp_group{name = ParentName, type = ParentType} = lists:last(ParentPath),
        #idp_group{name = ChildName, type = ChildType} = lists:last(ChildPath),
        NormalizedParentName = entity_logic:normalize_name(ParentName),
        NormalizedChildName = entity_logic:normalize_name(ChildName),
        % If the child group is the adminGroup, it should have admin privileges in parents
        ParentGroupId = entitlement_mapping:gen_group_id(ParentEntitlement),
        ChildGroupId = entitlement_mapping:gen_group_id(ChildEntitlement),
        % Check if names and types of groups are as expected
        {ok, #od_group{
            name = NormalizedParentName, type = ParentType
        }} = oz_test_utils:get_group(Config, ParentGroupId),
        {ok, #od_group{
            name = NormalizedChildName, type = ChildType
        }} = oz_test_utils:get_group(Config, ChildGroupId),
        case RelationType of
            effective ->
                % Do not check effective privileges as they are hard
                % to predict - might be inherited via different
                % membership paths than the one currently examined
                {ok, _} = oz_test_utils:call_oz(
                    Config, group_logic, get_eff_child, [?ROOT, ParentGroupId, ChildGroupId]
                ),
                true;
            direct ->
                {ok, ActualChildPrivs} = oz_test_utils:call_oz(
                    Config, group_logic, get_child_privileges, [?ROOT, ParentGroupId, ChildGroupId]
                ),
                ExpChildPrivs = (lists:last(ChildPath))#idp_group.privileges,
                ActualChildPrivs =:= entitlement_mapping:map_privileges(ExpChildPrivs)
        end
    catch _:_ ->
        false
    end.


is_protected(Config, IdP, RawEntitlement) ->
    IdPEntitlement = expected_parsing_result(Config, IdP, RawEntitlement),
    GroupId = entitlement_mapping:gen_group_id(IdPEntitlement),
    case oz_test_utils:get_group(Config, GroupId) of
        {ok, #od_group{protected = true}} -> true;
        _ -> false
    end.


expected_parsing_result(Config, IdP, RawEntitlement) ->
    Parser = get_parser(Config, IdP),
    Parser =:= undefined andalso error(undefined_parser),
    Entitlement = #idp_entitlement{
        idp = IdP,
        path = ParentPath
    } = oz_test_utils:call_oz(Config, Parser, parse, [IdP, RawEntitlement, parser_config(Parser)]),
    % Include the VO group if it was specified and is not the same as the entitlement
    % (in such case just update its type to organization).
    EntitlementWithVoGroup = case get_vo_group(Config, IdP) of
        undefined ->
            Entitlement;
        RawEntitlement ->
            % The RawEntitlement is the Vo Group
            Entitlement#idp_entitlement{
                path = [#idp_group{type = organization, name = RawEntitlement}]
            };
        VoName ->
            % Prepend the Vo to the group path
            Entitlement#idp_entitlement{
                path = [#idp_group{type = organization, name = VoName} | ParentPath]
            }
    end,
    % Include the admin group privs if the admin group was specified and is the same
    % as the entitlement
    case get_admin_group(Config, IdP) of
        RawEntitlement ->
            [Last | T] = lists:reverse(EntitlementWithVoGroup#idp_entitlement.path),
            OverridenPrivs = Last#idp_group{privileges = admin},
            EntitlementWithVoGroup#idp_entitlement{path = lists:reverse([OverridenPrivs | T])};
        _ ->
            EntitlementWithVoGroup
    end.


get_groups(Config, UserId, RelationType) ->
    {ok, UserGroups} = case RelationType of
        direct -> oz_test_utils:user_get_groups(Config, UserId);
        effective -> oz_test_utils:user_get_eff_groups(Config, UserId)
    end,
    UserGroups.


get_entitlements(Config, UserId) ->
    {ok, #od_user{entitlements = Entitlements}} = oz_test_utils:get_user(Config, UserId),
    Entitlements.


has_linked_account(Config, UserId, LinkedAccount) ->
    lists:member(LinkedAccount, get_linked_accounts(Config, UserId)).


get_linked_accounts(Config, UserId) ->
    {ok, #od_user{
        linked_accounts = LinkedAccounts
    }} = oz_test_utils:get_user(Config, UserId),
    LinkedAccounts.


overwrite_config(IdP, Enabled) ->
    overwrite_config(IdP, Enabled, undefined, []).

overwrite_config(IdP, Enabled, Parser) ->
    overwrite_config(IdP, Enabled, Parser, []).

% Opts :: [{adminGroup, string()} | {voGroupName, string()}]
overwrite_config(IdP, Enabled, Parser, Opts) ->
    Config = get_test_config(),
    OldAuthConfig = oz_test_utils:read_auth_config(Config),
    SupportedIdPs = maps:get(supportedIdps, OldAuthConfig, []),

    IdPConfig = {IdP, #{
        protocol => openid,
        protocolConfig => #{
            plugin => default_oidc_plugin,
            entitlementMapping => #{
                enabled => Enabled,
                voGroupName => proplists:get_value(voGroupName, Opts, undefined),
                adminGroup => proplists:get_value(adminGroup, Opts, undefined),
                parser => Parser,
                parserConfig => parser_config(Parser)
            }
        }
    }},

    oz_test_utils:overwrite_auth_config(Config, #{
        openidConfig => #{
            enabled => true
        },
        supportedIdps => lists:keystore(IdP, 1, SupportedIdPs, IdPConfig)
    }).


get_admin_group(Config, IdP) ->
    AdminGroup = oz_test_utils:call_oz(Config, auth_config, get_entitlement_mapping_config, [
        IdP, [adminGroup], {default, undefined}
    ]),
    case AdminGroup of
        undefined -> undefined;
        Str -> list_to_binary(Str)
    end.


get_vo_group(Config, IdP) ->
    VoGroup = oz_test_utils:call_oz(Config, auth_config, get_entitlement_mapping_config, [
        IdP, [voGroupName], {default, undefined}
    ]),
    case VoGroup of
        undefined -> undefined;
        Str -> list_to_binary(Str)
    end.


get_parser(Config, IdP) ->
    oz_test_utils:call_oz(Config, auth_config, get_entitlement_mapping_config, [
        IdP, [parser], {default, undefined}
    ]).


parser_config(undefined) -> #{
};
parser_config(flat_entitlement_parser) -> #{
    groupType => team,
    groupPrivilegesInVo => member,
    userPrivileges => member
};
parser_config(nested_entitlement_parser) -> #{
    splitWith => "/",
    topGroupType => unit,
    topGroupPrivilegesInVo => member,
    subGroupsType => team,
    subGroupsPrivilegesInParent => member,
    userPrivileges => member
};
parser_config(?CUSTOM_ENTITLEMENT_PARSER) -> #{
}.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

init_per_testcase(_, Config) ->
    store_test_config(Config),
    oz_test_utils:toggle_basic_auth(Config, true),
    overwrite_config(?DUMMY_IDP, false),
    overwrite_config(?OTHER_IDP, false),
    overwrite_config(?THIRD_IDP, false),
    oz_test_utils:delete_all_entities(Config, true),
    mock_custom_entitlement_parser(Config),
    Config.

end_per_testcase(_, Config) ->
    unmock_custom_entitlement_parser(Config),
    oz_test_utils:delete_all_entities(Config),
    ok.

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().

mock_custom_entitlement_parser(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_new(Nodes, ?CUSTOM_ENTITLEMENT_PARSER, [non_strict]),
    test_utils:mock_expect(Nodes, ?CUSTOM_ENTITLEMENT_PARSER, type, fun() ->
        entitlement_parser
    end),
    test_utils:mock_expect(Nodes, ?CUSTOM_ENTITLEMENT_PARSER, parse, fun(IdP, RawEntitlement, _) ->
        % Parses entitlements like nested parser, but recognizes the privileges on all levels.
        % Example: "top-group:member/bottom-group:manager/user:admin" entitlement means
        % that "top-group" would be added as member to the VO group (if any),
        % "bottom-group" would be added as manager to "top-group" and
        % the user would be added as admin to "bottom-group".
        % If no privileges are specified, they default to member.
        % The last element ("user:privileges") can be skipped.
        Tokens = binary:split(RawEntitlement, <<"/">>, [global]),
        ElementsWithPrivileges = lists:map(fun(Token) ->
            case binary:split(Token, <<":">>, [global]) of
                [Name, Privileges] -> {Name, binary_to_existing_atom(Privileges, utf8)};
                [Name] -> {Name, member}
            end
        end, Tokens),
        {UserPrivileges, GroupsWithPrivileges} = case lists:last(ElementsWithPrivileges) of
            {<<"user">>, UserPrivs} -> {UserPrivs, lists:droplast(ElementsWithPrivileges)};
            _ -> {member, ElementsWithPrivileges}
        end,
        #idp_entitlement{
            idp = IdP,
            path = lists:map(fun({GroupName, PrivilegesInParent}) ->
                #idp_group{type = team, name = GroupName, privileges = PrivilegesInParent}
            end, GroupsWithPrivileges),
            privileges = UserPrivileges
        }
    end),
    ok.

unmock_custom_entitlement_parser(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, ?CUSTOM_ENTITLEMENT_PARSER).
