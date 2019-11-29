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

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    coalesce_entitlements_test/1
]).

all() ->
    ?ALL([
        coalesce_entitlements_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================

% Below macros use the variables Config and UserId, which appear in merge_groups_in_linked_accounts_test/1
-define(RETRIES, 600).
-define(INTERVAL, 100). % 600 Attempts every 100 ms - 1 minute

-define(assertGroupExists(__Flag, __IdP, __RawEntitlement),
    ?assertEqual(
        __Flag,
        check_group_exists(Config, __IdP, __RawEntitlement),
        ?RETRIES,
        ?INTERVAL
    )
).

-define(assertHasGroup(__Flag, __IdP, __RawEntitlement, __RelationType),
    ?assertEqual(
        __Flag,
        check_has_group(Config, UserId, __IdP, __RawEntitlement, __RelationType),
        ?RETRIES,
        ?INTERVAL
    )
).

-define(assertGroupsCount(__Direct, __Effective),
    ?assertEqual(
        true,
        check_groups_count(Config, UserId, __Direct, __Effective),
        ?RETRIES,
        ?INTERVAL
    )
).

-define(assertGroupStructure(__IdP, __ParentRawEntitlement, __ChildRawEntitlement, __RelationType),
    ?assertEqual(
        true,
        check_group_structure(Config, __IdP, __ParentRawEntitlement, __ChildRawEntitlement, __RelationType),
        ?RETRIES,
        ?INTERVAL
    )
).

-define(assertHasLinkedAccount(__LinkedAcc),
    ?assert(check_has_linked_account(Config, UserId, __LinkedAcc))
).

-define(assertLinkedAccountsCount(__Number),
    ?assertEqual(
        __Number,
        length(get_linked_accounts(Config, UserId)),
        ?RETRIES,
        ?INTERVAL
    )
).

-define(DUMMY_IDP, dummyIdP).
-define(OTHER_IDP, anotherIdP).
-define(THIRD_IDP, thirdIdP).

coalesce_entitlements_test(Config) ->
    overwrite_entitlement_mapping(Config, ?DUMMY_IDP, false, undefined, undefined, undefined),
    overwrite_entitlement_mapping(Config, ?OTHER_IDP, false, undefined, undefined, undefined),
    overwrite_entitlement_mapping(Config, ?THIRD_IDP, false, undefined, undefined, undefined),

    % Helpers for shorter code
    LinkedAcc = fun(IdP, Entitlements) ->
        #linked_account{
            idp = IdP,
            subject_id = <<(atom_to_binary(IdP, utf8))/binary, "-subjectId">>,
            entitlements = Entitlements
        }
    end,

    FirstLinkedAcc = LinkedAcc(?DUMMY_IDP, []),
    {ok, #document{key = UserId}} = oz_test_utils:call_oz(
        Config, linked_accounts, acquire_user, [FirstLinkedAcc]
    ),
    ?assertHasLinkedAccount(FirstLinkedAcc),
    ?assertGroupsCount(0, 0),
    ?assertLinkedAccountsCount(1),

    MergeAcc = fun(IdP, Entitlements) ->
        oz_test_utils:call_oz(Config, linked_accounts, merge, [
            UserId, LinkedAcc(IdP, Entitlements)
        ]),
        % Make sure
        ?assertHasLinkedAccount(LinkedAcc(IdP, Entitlements))
    end,

    % Entitlement mapping is disabled for now
    MergeAcc(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>]),
    ?assertGroupsCount(0, 0),
    ?assertLinkedAccountsCount(1),
    ?assertGroupExists(false, ?DUMMY_IDP, <<"group/subgroup">>),
    ?assertGroupExists(false, ?DUMMY_IDP, <<"anotherGroup">>),
    ?assertGroupExists(false, ?DUMMY_IDP, <<"thirdGroup">>),

    % Enable entitlement mapping that uses a flat_entitlement_parser
    overwrite_entitlement_mapping(Config, ?DUMMY_IDP, true, flat_entitlement_parser, undefined, undefined),
    MergeAcc(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>]),
    ?assertGroupsCount(2, 2),
    ?assertLinkedAccountsCount(1),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"group/subgroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"anotherGroup">>),
    ?assertGroupExists(false, ?DUMMY_IDP, <<"thirdGroup">>),

    % Simulate a situation when a new entitlement appears
    MergeAcc(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>, <<"thirdGroup">>]),
    ?assertGroupsCount(3, 3),
    ?assertLinkedAccountsCount(1),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"thirdGroup">>, direct),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"group/subgroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"anotherGroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"thirdGroup">>),

    % Simulate a situation when two user entitlements are withdrawn
    MergeAcc(?DUMMY_IDP, [<<"anotherGroup">>]),
    ?assertGroupsCount(1, 1),
    ?assertLinkedAccountsCount(1),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"thirdGroup">>, direct),
    % The groups should not be removed
    ?assertGroupExists(true, ?DUMMY_IDP, <<"group/subgroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"anotherGroup">>),
    ?assertGroupExists(true, ?DUMMY_IDP, <<"thirdGroup">>),

    % Link a new user account in IdP that has currently disabled entitlement mapping
    MergeAcc(?OTHER_IDP, [<<"users/admins">>, <<"users/developers">>, <<"users/technicians">>]),
    ?assertGroupsCount(1, 1),
    ?assertLinkedAccountsCount(2),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"thirdGroup">>, direct),
    ?assertHasGroup(false, ?OTHER_IDP, <<"users/admins">>, direct),
    ?assertHasGroup(false, ?OTHER_IDP, <<"users/developers">>, direct),
    ?assertHasGroup(false, ?OTHER_IDP, <<"users/technicians">>, direct),
    ?assertGroupExists(false, ?DUMMY_IDP, <<"users/admins">>),
    ?assertGroupExists(false, ?DUMMY_IDP, <<"users/developers">>),
    ?assertGroupExists(false, ?DUMMY_IDP, <<"users/technicians">>),

    % Turn on entitlement mapping in ?ANOTHER_IDP, with "users/admins" as admin Group
    overwrite_entitlement_mapping(Config, ?OTHER_IDP, true, nested_entitlement_parser, undefined, "users/admins"),
    MergeAcc(?OTHER_IDP, [<<"users/admins">>, <<"users/developers">>, <<"users/technicians">>]),
    ?assertGroupsCount(4, 5),
    ?assertLinkedAccountsCount(2),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"thirdGroup">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/admins">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/developers">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/technicians">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users">>, effective),
    ?assertGroupExists(true, ?OTHER_IDP, <<"users/admins">>),
    ?assertGroupExists(true, ?OTHER_IDP, <<"users/developers">>),
    ?assertGroupExists(true, ?OTHER_IDP, <<"users/technicians">>),
    ?assertGroupStructure(?OTHER_IDP, <<"users">>, <<"users/admins">>, direct),
    ?assertGroupStructure(?OTHER_IDP, <<"users">>, <<"users/developers">>, direct),
    ?assertGroupStructure(?OTHER_IDP, <<"users">>, <<"users/technicians">>, direct),
    % The admin group should belong to all groups
    ?assertGroupStructure(?OTHER_IDP, <<"users/developers">>, <<"users/admins">>, direct),
    ?assertGroupStructure(?OTHER_IDP, <<"users/technicians">>, <<"users/admins">>, direct),

    % Turn off entitlement mapping for the DUMMY_IDP, which should remove related entitlements
    overwrite_entitlement_mapping(Config, ?DUMMY_IDP, false, undefined, undefined, undefined),
    MergeAcc(?DUMMY_IDP, [<<"group/subgroup">>, <<"anotherGroup">>, <<"thirdGroup">>]),
    ?assertGroupsCount(3, 4),
    ?assertLinkedAccountsCount(2),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertHasGroup(false, ?DUMMY_IDP, <<"thirdGroup">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/admins">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/developers">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/technicians">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users">>, effective),

    % Turn on entitlement mapping for the DUMMY_IDP again
    overwrite_entitlement_mapping(Config, ?DUMMY_IDP, true, flat_entitlement_parser, undefined, undefined),
    % Turn on entitlement mapping for the ?THIRD_IDP, including a VO and admin group
    overwrite_entitlement_mapping(Config, ?THIRD_IDP, true, nested_entitlement_parser, "Third-VO", "staff/admins/privileged"),
    % Single login from ?THIRD_IDP should also trigger adding the entitlements from ?DUMMY_IDP
    MergeAcc(?THIRD_IDP, [<<"staff/vm-operators">>, <<"task4.1">>, <<"testGroup">>, <<"staff/admins/privileged">>]),
    ?assertGroupsCount(10, 14),
    ?assertLinkedAccountsCount(3),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"group/subgroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"anotherGroup">>, direct),
    ?assertHasGroup(true, ?DUMMY_IDP, <<"thirdGroup">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/admins">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/developers">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users/technicians">>, direct),
    ?assertHasGroup(true, ?OTHER_IDP, <<"users">>, effective),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff/vm-operators">>, direct),
    ?assertHasGroup(true, ?THIRD_IDP, <<"task4.1">>, direct),
    ?assertHasGroup(true, ?THIRD_IDP, <<"testGroup">>, direct),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff/admins/privileged">>, direct),
    ?assertHasGroup(true, ?THIRD_IDP, <<"Third-VO">>, effective),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff">>, effective),
    ?assertHasGroup(true, ?THIRD_IDP, <<"staff/admins">>, effective),
    % All groups should belong to their parents and the top parent to the VO group
    ?assertGroupStructure(?THIRD_IDP, <<"staff">>, <<"staff/vm-operators">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"staff">>, <<"staff/admins">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"staff/admins">>, <<"staff/admins/privileged">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"staff">>, <<"staff/admins/privileged">>, effective),
    ?assertGroupStructure(?THIRD_IDP, <<"Third-VO">>, <<"staff">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"Third-VO">>, <<"staff/vm-operators">>, effective),
    ?assertGroupStructure(?THIRD_IDP, <<"Third-VO">>, <<"staff/admins">>, effective),
    ?assertGroupStructure(?THIRD_IDP, <<"Third-VO">>, <<"staff/admins/privileged">>, effective),
    % The admin group should belong to all groups
    ?assertGroupStructure(?THIRD_IDP, <<"staff">>, <<"staff/admins/privileged">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"staff/admins">>, <<"staff/admins/privileged">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"task4.1">>, <<"staff/admins/privileged">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"testGroup">>, <<"staff/admins/privileged">>, direct),
    ?assertGroupStructure(?THIRD_IDP, <<"Third-VO">>, <<"staff/admins/privileged">>, direct),

    % Turn off entitlement mapping for all IdPs
    overwrite_entitlement_mapping(Config, ?DUMMY_IDP, false, undefined, undefined, undefined),
    overwrite_entitlement_mapping(Config, ?OTHER_IDP, false, undefined, undefined, undefined),
    overwrite_entitlement_mapping(Config, ?THIRD_IDP, false, undefined, undefined, undefined),
    % Merge one of the accounts, which should trigger removal of all entitlements
    MergeAcc(?THIRD_IDP, [<<"staff/vm-operators">>, <<"task4.1">>, <<"testGroup">>, <<"staff/admins/privileged">>]),
    ?assertGroupsCount(0, 0),
    ?assertLinkedAccountsCount(3),

    % Turn the mapping on again
    overwrite_entitlement_mapping(Config, ?DUMMY_IDP, true, flat_entitlement_parser, undefined, undefined),
    overwrite_entitlement_mapping(Config, ?OTHER_IDP, true, nested_entitlement_parser, undefined, "users/admins"),
    overwrite_entitlement_mapping(Config, ?THIRD_IDP, true, nested_entitlement_parser, "Third-VO", "staff/admins/privileged"),
    % Merge one of the accounts, which should add the entitlements again
    MergeAcc(?THIRD_IDP, [<<"staff/vm-operators">>, <<"task4.1">>, <<"testGroup">>, <<"staff/admins/privileged">>]),
    ?assertGroupsCount(10, 14),
    ?assertLinkedAccountsCount(3),

    % Check if all groups are protected
    lists:foreach(fun({IdP, Entitlement}) ->
        ?assert(is_protected(Config, IdP, Entitlement))
    end, [
        {?DUMMY_IDP, <<"group/subgroup">>},
        {?DUMMY_IDP, <<"anotherGroup">>},
        {?DUMMY_IDP, <<"thirdGroup">>},

        {?OTHER_IDP, <<"users/admins">>},
        {?OTHER_IDP, <<"users/developers">>},
        {?OTHER_IDP, <<"users/technicians">>},
        {?OTHER_IDP, <<"users">>},

        {?THIRD_IDP, <<"staff/vm-operators">>},
        {?THIRD_IDP, <<"task4.1">>},
        {?THIRD_IDP, <<"testGroup">>},
        {?THIRD_IDP, <<"staff/admins/privileged">>},
        {?THIRD_IDP, <<"staff">>},
        {?THIRD_IDP, <<"staff/admins">>}
    ]).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

init_per_testcase(_, Config) ->
    oz_test_utils:toggle_basic_auth(Config, true),
    Config.

end_per_testcase(_, Config) ->
    oz_test_utils:delete_all_entities(Config),
    ok.

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().


%%%===================================================================
%%% Internal functions
%%%===================================================================

check_group_exists(Config, IdP, RawEntitlement) ->
    try
        IdPEntitlement = expected_parsing_result(Config, IdP, RawEntitlement),
        GroupId = entitlement_mapping:gen_group_id(IdPEntitlement),
        oz_test_utils:call_oz(Config, group_logic, exists, [GroupId])
    catch _:_ ->
        false
    end.


%% RelationType :: direct | effective
check_has_group(Config, UserId, IdP, RawEntitlement, RelationType) ->
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
        }} = oz_test_utils:get_group(Config, GroupId),
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


check_groups_count(Config, UserId, ExpDirectCount, ExpEffectiveCount) ->
    DirectCount = length(get_groups(Config, UserId, direct)),
    EffectiveCount = length(get_groups(Config, UserId, effective)),
    EntitlementsCount = length(get_entitlements(Config, UserId)),
    DirectCount =:= ExpDirectCount andalso
        EntitlementsCount =:= ExpDirectCount andalso
        EffectiveCount =:= ExpEffectiveCount.


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
    Parser =:= undefined andalso throw(undefined_parser),
    Entitlement = #idp_entitlement{
        idp = IdP,
        path = ParentPath
    } = Parser:parse(IdP, RawEntitlement, parser_config(Parser)),
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


check_has_linked_account(Config, UserId, LinkedAccount) ->
    lists:member(LinkedAccount, get_linked_accounts(Config, UserId)).


get_linked_accounts(Config, UserId) ->
    {ok, #od_user{
        linked_accounts = LinkedAccounts
    }} = oz_test_utils:get_user(Config, UserId),
    LinkedAccounts.


overwrite_entitlement_mapping(Config, IdP, Enabled, Parser, VoGroupName, AdminGroup) ->
    OldAuthConfig = oz_test_utils:read_auth_config(Config),
    SupportedIdPs = maps:get(supportedIdps, OldAuthConfig, []),

    IdPConfig = {IdP, #{
        protocol => openid,
        protocolConfig => #{
            plugin => default_oidc_plugin,
            entitlementMapping => #{
                enabled => Enabled,
                voGroupName => VoGroupName,
                adminGroup => AdminGroup,
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
}.
