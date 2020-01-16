%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This suite contains tests for invite tokens that cover the aspects of
%%% authorization, carried privileges, multi-use tokens and caveats.
%%% @end
%%%-------------------------------------------------------------------
-module(invite_tokens_test_SUITE).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("api_test_utils.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("cluster_worker/include/graph_sync/graph_sync.hrl").

% Mocked connection and geo db data
-define(PEER_IP, {189, 51, 103, 211}).
-define(CORRECT_MASK_1, {{189, 51, 103, 211}, 32}).
-define(CORRECT_MASK_2, {{189, 51, 0, 0}, 16}).
-define(INCORRECT_MASK_1, {{1, 2, 3, 4}, 19}).
-define(INCORRECT_MASK_2, {{100, 78, 9, 0}, 24}).
-define(CLIENT_ASN, 721).
-define(INCORRECT_ASN, 8341).
-define(CLIENT_COUNTRY, <<"PL">>).
-define(INCORRECT_COUNTRY, <<"DE">>).
-define(CLIENT_REGIONS, [<<"Europe">>, <<"EU">>]).
-define(INCORRECT_REGIONS, [<<"Asia">>, <<"Oceania">>]).

% Generic name for type of privileges that are being modified, interpreted
% according to target entity type
-type privileges_type() :: to_invite | to_set_privs | to_consume.
-type privileges_modification() :: {grant | revoke, privileges_type()}.

-record(consume_request, {
    logic_call_args :: {Module :: atom(), Function :: atom(), Args :: [term()]},
    rest_call_args :: not_available | {ozt_http:urn_tokens(), entity_logic:data()},
    graph_sync_args :: {gri:gri(), gs_protocol:auth_hint(), entity_logic:data()}
}).

-record(testcase, {
    token_type :: tokens:type(),

    % What subjects are eligible to create an invite token of above type
    % (but authorization is still subject to privileges)
    eligible_to_invite :: [aai:subject()],
    % Indicates if special privileges are required to be able to invite
    requires_privileges_to_invite :: boolean(),
    % What admin privilege is required to issue an invite token
    admin_privilege_to_invite :: privileges:oz_privilege(),

    % Indicates if the token type supports adding carried privileges to the token
    % and if so, what are the allowed and default privileges.
    supports_carried_privileges :: false | true,
    % Indicates if additional privileges are required to request carried privileges
    requires_privileges_to_set_privileges :: false | true,
    allowed_carried_privileges :: token_metadata:invite_privileges(),
    default_carried_privileges :: token_metadata:invite_privileges(),
    % What admin privilege is required to set privileges in the target entity
    % (if applicable)
    admin_privilege_to_set_privileges :: undefined | privileges:oz_privilege(),

    % List of subject types that are eligible to consume the token. If a tuple
    % is given, it means that by default such subject is not eligible and
    % requires additional setup to be eligible (e.g. add a user to group that is
    % being joined to an entity). The second element is the function used to
    % perform this setup.
    eligible_consumer_types :: [aai:subject_type() | {aai:subject_type(), fun((aai:subject()) -> ok)}],
    % Indicates if special privileges are required to be able to consume the
    % token (e.g. in a group that is being joined to some other entity).
    requires_privileges_to_consume :: boolean(),
    admin_privilege_to_consume :: undefined | privileges:oz_privilege(),

    % Function that modifies the subject's privileges as requested
    modify_privileges_fun :: fun((aai:subject(), privileges_modification()) -> ok),
    % Function that checks if given subject has given privileges
    check_privileges_fun :: fun((aai:subject(), token_metadata:invite_privileges()) -> boolean()),

    % Function that prepares arguments for consuming the token in logic/REST/GraphSync
    prepare_consume_request :: fun((aai:auth(), tokens:token()) -> #consume_request{}),

    % Function that deletes the target entity of the token
    delete_target_entity_fun :: fun(() -> ok),
    % Expected result when the token is used again after successful consumption
    % (assuming it is a multi-use token) - function that generates the expected
    % result based on consumer subject
    expected_reuse_result_fun :: fun((Consumer :: aai:subject()) -> ok | errors:error())
}).

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    user_join_group_token/1,
    group_join_group_token/1,
    user_join_space_token/1,
    group_join_space_token/1,
    support_space_token/1,
    register_oneprovider_token/1,
    user_join_cluster_token/1,
    group_join_cluster_token/1,
    user_join_harvester_token/1,
    group_join_harvester_token/1,
    space_join_harvester_token/1
]).

all() -> ?ALL([
    user_join_group_token,
    group_join_group_token,
    user_join_space_token,
    group_join_space_token,
    support_space_token,
    register_oneprovider_token,
    user_join_cluster_token,
    group_join_cluster_token,
    user_join_harvester_token,
    group_join_harvester_token,
    space_join_harvester_token
]).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    ozt:init_per_suite(Config).

init_per_testcase(_, Config) ->
    ozt_mocks:mock_gui_static(),
    ozt_mocks:mock_time(),
    ozt_mocks:mock_harvester_plugins(),
    ozt_mocks:mock_peer_ip_of_all_connections(?PEER_IP),
    ozt_mocks:mock_geo_db_entry_for_all_ips(?CLIENT_ASN, ?CLIENT_COUNTRY, ?CLIENT_REGIONS),
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unmock_time(),
    ozt_mocks:unmock_gui_static(),
    ozt_mocks:unmock_harvester_plugins(),
    ozt_mocks:unmock_peer_ip_of_all_connections(),
    ozt_mocks:unmock_geo_db_entry_for_all_ips().

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().

%%%===================================================================
%%% Test cases
%%%===================================================================

user_join_group_token(_Config) ->
    GroupCreatorUserId = ozt_users:create(),
    GroupId = ozt_users:create_group_for(GroupCreatorUserId),

    ?assert(run_invite_token_tests(#testcase{
        token_type = ?INVITE_TOKEN(?USER_JOIN_GROUP, GroupId),

        eligible_to_invite = [?SUB(user, GroupCreatorUserId)],
        requires_privileges_to_invite = true,
        admin_privilege_to_invite = ?OZ_GROUPS_ADD_RELATIONSHIPS,

        supports_carried_privileges = true,
        requires_privileges_to_set_privileges = true,
        allowed_carried_privileges = privileges:group_privileges(),
        default_carried_privileges = privileges:group_member(),
        admin_privilege_to_set_privileges = ?OZ_GROUPS_SET_PRIVILEGES,

        eligible_consumer_types = [user],
        requires_privileges_to_consume = false,
        admin_privilege_to_consume = undefined,

        modify_privileges_fun = fun(?SUB(user, UserId), Modification) ->
            set_user_privs_in_group(GroupId, UserId, Modification)
        end,
        check_privileges_fun = fun(?SUB(user, UserId), ExpectedPrivileges) ->
            ActualPrivileges = ozt_groups:get_user_privileges(GroupId, UserId),
            lists:sort(ExpectedPrivileges) =:= lists:sort(ActualPrivileges)
        end,

        prepare_consume_request = fun(Auth = #auth{subject = ?SUB(_, UserId)}, Token) ->
            Data = #{<<"token">> => ozt_tokens:ensure_serialized(Token)},
            #consume_request{
                logic_call_args = {user_logic, join_group, [Auth, UserId, Data]},
                rest_call_args = {<<"/user/groups/join">>, Data},
                graph_sync_args = {?GRI(od_group, undefined, join, private), ?AS_USER(UserId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            ozt_groups:delete(GroupId)
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            ?ERROR_RELATION_ALREADY_EXISTS(od_user, UserId, od_group, GroupId)
        end
    })).


group_join_group_token(_Config) ->
    GroupCreatorUserId = ozt_users:create(),
    ParentGroupId = ozt_users:create_group_for(GroupCreatorUserId),

    ?assert(run_invite_token_tests(#testcase{
        token_type = ?INVITE_TOKEN(?GROUP_JOIN_GROUP, ParentGroupId),

        eligible_to_invite = [?SUB(user, GroupCreatorUserId)],
        requires_privileges_to_invite = true,
        admin_privilege_to_invite = ?OZ_GROUPS_ADD_RELATIONSHIPS,

        supports_carried_privileges = true,
        requires_privileges_to_set_privileges = true,
        allowed_carried_privileges = privileges:group_privileges(),
        default_carried_privileges = privileges:group_member(),
        admin_privilege_to_set_privileges = ?OZ_GROUPS_SET_PRIVILEGES,

        eligible_consumer_types = [{user, fun(?SUB(user, UserId)) ->
            ChildGroupId = ozt_users:create_group_for(UserId),
            % Store the group in memory for use in different callbacks
            simple_cache:put({user_group, UserId}, ChildGroupId)
        end}],
        requires_privileges_to_consume = true,
        admin_privilege_to_consume = ?OZ_GROUPS_ADD_RELATIONSHIPS,

        modify_privileges_fun = fun(?SUB(user, UserId), Modification) ->
            case Modification of
                {_, to_invite} ->
                    set_user_privs_in_group(ParentGroupId, UserId, Modification);
                {_, to_set_privs} ->
                    set_user_privs_in_group(ParentGroupId, UserId, Modification);
                {_, to_consume} ->
                    {ok, ChildGroupId} = simple_cache:get({user_group, UserId}),
                    set_user_privs_in_group(ChildGroupId, UserId, Modification)
            end
        end,
        check_privileges_fun = fun(?SUB(user, UserId), ExpectedPrivileges) ->
            {ok, ChildGroupId} = simple_cache:get({user_group, UserId}),
            ActualPrivileges = ozt_groups:get_child_privileges(ParentGroupId, ChildGroupId),
            lists:sort(ExpectedPrivileges) =:= lists:sort(ActualPrivileges)
        end,

        prepare_consume_request = fun(Auth = #auth{subject = ?SUB(_, SubjectId)}, Token) ->
            ChildGroupId = case simple_cache:get({user_group, SubjectId}) of
                {ok, GrId} ->
                    % Covers users that were set up according to eligible_consumer_types
                    GrId;
                _ ->
                    % Covers users with no group and other consumer types.
                    % Other consumer than user does not make sense, but for the
                    % sake of checking if bad consumer is properly handled, try
                    % to use a freshly created group
                    ozt_groups:create()
            end,
            Data = #{<<"token">> => ozt_tokens:ensure_serialized(Token)},
            #consume_request{
                logic_call_args = {group_logic, join_group, [Auth, ChildGroupId, Data]},
                rest_call_args = {[<<"/groups/">>, ChildGroupId, <<"/parents/join">>], Data},
                graph_sync_args = {?GRI(od_group, undefined, join, private), ?AS_GROUP(ChildGroupId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            ozt_groups:delete(ParentGroupId)
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            {ok, ChildGroupId} = simple_cache:get({user_group, UserId}),
            ?ERROR_RELATION_ALREADY_EXISTS(od_group, ChildGroupId, od_group, ParentGroupId)
        end
    })).


user_join_space_token(_Config) ->
    SpaceCreatorUserId = ozt_users:create(),
    SpaceId = ozt_users:create_space_for(SpaceCreatorUserId),

    ?assert(run_invite_token_tests(#testcase{
        token_type = ?INVITE_TOKEN(?USER_JOIN_SPACE, SpaceId),

        eligible_to_invite = [?SUB(user, SpaceCreatorUserId)],
        requires_privileges_to_invite = true,
        admin_privilege_to_invite = ?OZ_SPACES_ADD_RELATIONSHIPS,

        supports_carried_privileges = true,
        requires_privileges_to_set_privileges = true,
        allowed_carried_privileges = privileges:space_privileges(),
        default_carried_privileges = privileges:space_member(),
        admin_privilege_to_set_privileges = ?OZ_SPACES_SET_PRIVILEGES,

        eligible_consumer_types = [user],
        requires_privileges_to_consume = false,
        admin_privilege_to_consume = undefined,

        modify_privileges_fun = fun(?SUB(user, UserId), Modification) ->
            set_user_privs_in_space(SpaceId, UserId, Modification)
        end,
        check_privileges_fun = fun(?SUB(user, UserId), ExpectedPrivileges) ->
            ActualPrivileges = ozt_spaces:get_user_privileges(SpaceId, UserId),
            lists:sort(ExpectedPrivileges) =:= lists:sort(ActualPrivileges)
        end,

        prepare_consume_request = fun(Auth = #auth{subject = ?SUB(_, UserId)}, Token) ->
            Data = #{<<"token">> => ozt_tokens:ensure_serialized(Token)},
            #consume_request{
                logic_call_args = {user_logic, join_space, [Auth, UserId, Data]},
                rest_call_args = {<<"/user/spaces/join">>, Data},
                graph_sync_args = {?GRI(od_space, undefined, join, private), ?AS_USER(UserId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            ozt_spaces:delete(SpaceId)
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            ?ERROR_RELATION_ALREADY_EXISTS(od_user, UserId, od_space, SpaceId)
        end
    })).


group_join_space_token(_Config) ->
    SpaceCreatorUserId = ozt_users:create(),
    SpaceId = ozt_users:create_space_for(SpaceCreatorUserId),

    ?assert(run_invite_token_tests(#testcase{
        token_type = ?INVITE_TOKEN(?GROUP_JOIN_SPACE, SpaceId),

        eligible_to_invite = [?SUB(user, SpaceCreatorUserId)],
        requires_privileges_to_invite = true,
        admin_privilege_to_invite = ?OZ_SPACES_ADD_RELATIONSHIPS,

        supports_carried_privileges = true,
        requires_privileges_to_set_privileges = true,
        allowed_carried_privileges = privileges:space_privileges(),
        default_carried_privileges = privileges:space_member(),
        admin_privilege_to_set_privileges = ?OZ_SPACES_SET_PRIVILEGES,

        eligible_consumer_types = [{user, fun(?SUB(user, UserId)) ->
            GroupId = ozt_users:create_group_for(UserId),
            % Store the group in memory for use in different callbacks
            simple_cache:put({user_group, UserId}, GroupId)
        end}],
        requires_privileges_to_consume = true,
        admin_privilege_to_consume = ?OZ_GROUPS_ADD_RELATIONSHIPS,

        modify_privileges_fun = fun(?SUB(user, UserId), Modification) ->
            case Modification of
                {_, to_invite} ->
                    set_user_privs_in_space(SpaceId, UserId, Modification);
                {_, to_set_privs} ->
                    set_user_privs_in_space(SpaceId, UserId, Modification);
                {_, to_consume} ->
                    {ok, GroupId} = simple_cache:get({user_group, UserId}),
                    set_user_privs_in_group(GroupId, UserId, Modification)
            end
        end,
        check_privileges_fun = fun(?SUB(user, UserId), ExpectedPrivileges) ->
            {ok, GroupId} = simple_cache:get({user_group, UserId}),
            ActualPrivileges = ozt_spaces:get_group_privileges(SpaceId, GroupId),
            lists:sort(ExpectedPrivileges) =:= lists:sort(ActualPrivileges)
        end,

        prepare_consume_request = fun(Auth = #auth{subject = ?SUB(_, SubjectId)}, Token) ->
            GroupId = case simple_cache:get({user_group, SubjectId}) of
                {ok, GrId} ->
                    % Covers users that were set up according to eligible_consumer_types
                    GrId;
                _ ->
                    % Covers users with no group and other consumer types.
                    % Other consumer than user does not make sense, but for the
                    % sake of checking if bad consumer is properly handled, try
                    % to use a freshly created group
                    ozt_groups:create()
            end,
            Data = #{<<"token">> => ozt_tokens:ensure_serialized(Token)},
            #consume_request{
                logic_call_args = {group_logic, join_space, [Auth, GroupId, Data]},
                rest_call_args = {[<<"/groups/">>, GroupId, <<"/spaces/join">>], Data},
                graph_sync_args = {?GRI(od_space, undefined, join, private), ?AS_GROUP(GroupId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            ozt_spaces:delete(SpaceId)
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            {ok, GroupId} = simple_cache:get({user_group, UserId}),
            ?ERROR_RELATION_ALREADY_EXISTS(od_group, GroupId, od_space, SpaceId)
        end
    })).


support_space_token(_Config) ->
    SpaceCreatorUserId = ozt_users:create(),
    SpaceId = ozt_users:create_space_for(SpaceCreatorUserId),

    ?assert(run_invite_token_tests(#testcase{
        token_type = ?INVITE_TOKEN(?SUPPORT_SPACE, SpaceId),

        eligible_to_invite = [?SUB(user, SpaceCreatorUserId)],
        requires_privileges_to_invite = true,
        admin_privilege_to_invite = ?OZ_SPACES_ADD_RELATIONSHIPS,

        supports_carried_privileges = false,
        requires_privileges_to_set_privileges = false,
        allowed_carried_privileges = undefined,
        default_carried_privileges = undefined,
        admin_privilege_to_set_privileges = undefined,

        eligible_consumer_types = [?ONEPROVIDER],
        requires_privileges_to_consume = false,
        admin_privilege_to_consume = undefined,

        modify_privileges_fun = fun(?SUB(user, UserId), Modification) ->
            set_user_privs_in_space(SpaceId, UserId, Modification)
        end,
        check_privileges_fun = undefined,

        prepare_consume_request = fun(Auth, Token) ->
            ProviderId = case Auth of
                ?PROVIDER(PrId) ->
                    PrId;
                _ ->
                    % Other consumer than provider does not make sense, but for the
                    % sake of checking if bad consumer is properly handled, try to
                    % use a storage of another, unrelated provider
                    ozt_providers:create()
            end,
            % Use a storage with the same id as provider for easier test code
            StorageId = ProviderId,
            ozt_providers:ensure_storage(ProviderId, StorageId),
            Data = #{
                <<"token">> => ozt_tokens:ensure_serialized(Token),
                <<"size">> => ozt_spaces:minimum_support_size()
            },
            #consume_request{
                logic_call_args = {storage_logic, support_space, [Auth, StorageId, Data]},
                rest_call_args = not_available,
                graph_sync_args = {?GRI(od_storage, StorageId, support, private), undefined, Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            ozt_spaces:delete(SpaceId)
        end,
        expected_reuse_result_fun = fun(?SUB(?ONEPROVIDER, ProviderId)) ->
            StorageId = ProviderId,
            ?ERROR_RELATION_ALREADY_EXISTS(od_space, SpaceId, od_storage, StorageId)
        end
    })).


register_oneprovider_token(_Config) ->
    Testcase = fun(AdminUserId, CheckPolicies) -> #testcase{
        token_type = ?INVITE_TOKEN(?REGISTER_ONEPROVIDER, AdminUserId),

        eligible_to_invite = [?SUB(user, AdminUserId)],
        requires_privileges_to_invite = (CheckPolicies =:= open_and_restricted_policies),
        admin_privilege_to_invite = ?OZ_PROVIDERS_INVITE,

        supports_carried_privileges = false,
        requires_privileges_to_set_privileges = false,
        allowed_carried_privileges = undefined,
        default_carried_privileges = undefined,
        admin_privilege_to_set_privileges = undefined,

        eligible_consumer_types = [user, ?ONEPROVIDER, nobody],
        requires_privileges_to_consume = false,
        admin_privilege_to_consume = undefined,

        % Creating a provider registration token might require admin privileges
        % depending on the provider_registration_policy. In the testcase where both
        % policies are checked, changing the policy can be perceived as granting or
        % revoking the privilege to create registration tokens for all users.
        modify_privileges_fun = case CheckPolicies of
            only_open_policy -> undefined;
            open_and_restricted_policies -> fun
                (_, {grant, to_invite}) ->
                    ozt:set_env(provider_registration_policy, open);
                (_, {revoke, to_invite}) ->
                    ozt:set_env(provider_registration_policy, restricted)
            end
        end,
        check_privileges_fun = undefined,

        prepare_consume_request = fun(Auth, Token) ->
            Data = #{
                <<"token">> => ozt_tokens:ensure_serialized(Token),
                <<"name">> => ?UNIQUE_STRING,
                <<"subdomainDelegation">> => false,
                <<"domain">> => <<"oneprovider.example.com">>,
                <<"adminEmail">> => <<"admin@example.com">>
            },
            #consume_request{
                logic_call_args = {provider_logic, create, [Auth, Data]},
                rest_call_args = {<<"/providers/">>, Data},
                graph_sync_args = {?GRI(od_provider, undefined, instance, private), undefined, Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            ozt_users:delete(AdminUserId)
        end,
        expected_reuse_result_fun = fun(_) ->
            ok
        end
    } end,

    ozt:set_env(provider_registration_policy, open),
    UserAlpha = ozt_users:create(),
    ?assert(run_invite_token_tests(Testcase(UserAlpha, only_open_policy))),

    ozt:set_env(provider_registration_policy, restricted),
    UserBeta = ozt_users:create(),
    ?assert(run_invite_token_tests(Testcase(UserBeta, open_and_restricted_policies))).


user_join_cluster_token(_Config) ->
    Testcase = fun(ClusterId, EligibleToInvite = ?SUB(SubjectType, _)) -> #testcase{
        token_type = ?INVITE_TOKEN(?USER_JOIN_CLUSTER, ClusterId),

        eligible_to_invite = [EligibleToInvite],
        requires_privileges_to_invite = (SubjectType =:= user),
        admin_privilege_to_invite = ?OZ_CLUSTERS_ADD_RELATIONSHIPS,

        supports_carried_privileges = true,
        requires_privileges_to_set_privileges = (SubjectType =:= user),
        allowed_carried_privileges = privileges:cluster_privileges(),
        default_carried_privileges = privileges:cluster_admin(), %% @TODO VFS-5815 temp. solution
        admin_privilege_to_set_privileges = ?OZ_CLUSTERS_SET_PRIVILEGES,

        eligible_consumer_types = [user],
        requires_privileges_to_consume = false,
        admin_privilege_to_consume = undefined,

        modify_privileges_fun = fun(?SUB(user, UserId), Modification) ->
            set_user_privs_in_cluster(ClusterId, UserId, Modification)
        end,
        check_privileges_fun = fun(?SUB(user, UserId), ExpectedPrivileges) ->
            ActualPrivileges = ozt_clusters:get_user_privileges(ClusterId, UserId),
            lists:sort(ExpectedPrivileges) =:= lists:sort(ActualPrivileges)
        end,

        prepare_consume_request = fun(Auth = #auth{subject = ?SUB(_, UserId)}, Token) ->
            Data = #{<<"token">> => ozt_tokens:ensure_serialized(Token)},
            #consume_request{
                logic_call_args = {user_logic, join_cluster, [Auth, UserId, Data]},
                rest_call_args = {<<"/user/clusters/join">>, Data},
                graph_sync_args = {?GRI(od_cluster, undefined, join, private), ?AS_USER(UserId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            ProviderId = ClusterId,
            ozt_providers:delete(ProviderId)
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            ?ERROR_RELATION_ALREADY_EXISTS(od_user, UserId, od_cluster, ClusterId)
        end
    } end,

    ProviderCreatorUserId = ozt_users:create(),
    ProviderId = ozt_providers:create_for_admin_user(ProviderCreatorUserId),
    ClusterId = ProviderId,
    UserEligibleToInvite = ?SUB(user, ProviderCreatorUserId),
    ?assert(run_invite_token_tests(Testcase(ClusterId, UserEligibleToInvite))),

    AnotherProviderId = ozt_providers:create(),
    AnotherClusterId = AnotherProviderId,
    ProviderEligibleToInvite = ?SUB(?ONEPROVIDER, AnotherProviderId),
    ?assert(run_invite_token_tests(Testcase(AnotherClusterId, ProviderEligibleToInvite))).


group_join_cluster_token(_Config) ->
    Testcase = fun(ClusterId, EligibleToInvite = ?SUB(SubjectType, _)) -> #testcase{
        token_type = ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, ClusterId),

        eligible_to_invite = [EligibleToInvite],
        requires_privileges_to_invite = (SubjectType =:= user),
        admin_privilege_to_invite = ?OZ_CLUSTERS_ADD_RELATIONSHIPS,

        supports_carried_privileges = true,
        requires_privileges_to_set_privileges = (SubjectType =:= user),
        allowed_carried_privileges = privileges:cluster_privileges(),
        default_carried_privileges = privileges:cluster_member(),
        admin_privilege_to_set_privileges = ?OZ_CLUSTERS_SET_PRIVILEGES,

        eligible_consumer_types = [{user, fun(?SUB(user, UserId)) ->
            GroupId = ozt_users:create_group_for(UserId),
            % Store the group in memory for use in different callbacks
            simple_cache:put({user_group, UserId}, GroupId)
        end}],
        requires_privileges_to_consume = true,
        admin_privilege_to_consume = ?OZ_GROUPS_ADD_RELATIONSHIPS,

        modify_privileges_fun = fun(?SUB(user, UserId), Modification) ->
            case Modification of
                {_, to_invite} ->
                    set_user_privs_in_cluster(ClusterId, UserId, Modification);
                {_, to_set_privs} ->
                    set_user_privs_in_cluster(ClusterId, UserId, Modification);
                {_, to_consume} ->
                    {ok, GroupId} = simple_cache:get({user_group, UserId}),
                    set_user_privs_in_group(GroupId, UserId, Modification)
            end
        end,
        check_privileges_fun = fun(?SUB(user, UserId), ExpectedPrivileges) ->
            {ok, GroupId} = simple_cache:get({user_group, UserId}),
            ActualPrivileges = ozt_clusters:get_group_privileges(ClusterId, GroupId),
            lists:sort(ExpectedPrivileges) =:= lists:sort(ActualPrivileges)
        end,

        prepare_consume_request = fun(Auth = #auth{subject = ?SUB(_, SubjectId)}, Token) ->
            GroupId = case simple_cache:get({user_group, SubjectId}) of
                {ok, GrId} ->
                    % Covers users that were set up according to eligible_consumer_types
                    GrId;
                _ ->
                    % Covers users with no group and other consumer types.
                    % Other consumer than user does not make sense, but for the
                    % sake of checking if bad consumer is properly handled, try
                    % to use a freshly created group
                    ozt_groups:create()
            end,
            Data = #{<<"token">> => ozt_tokens:ensure_serialized(Token)},
            #consume_request{
                logic_call_args = {group_logic, join_cluster, [Auth, GroupId, Data]},
                rest_call_args = {[<<"/groups/">>, GroupId, <<"/clusters/join">>], Data},
                graph_sync_args = {?GRI(od_cluster, undefined, join, private), ?AS_GROUP(GroupId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            ProviderId = ClusterId,
            ozt_providers:delete(ProviderId)
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            {ok, GroupId} = simple_cache:get({user_group, UserId}),
            ?ERROR_RELATION_ALREADY_EXISTS(od_group, GroupId, od_cluster, ClusterId)
        end
    } end,

    ProviderCreatorUserId = ozt_users:create(),
    ProviderId = ozt_providers:create_for_admin_user(ProviderCreatorUserId),
    ClusterId = ProviderId,
    UserEligibleToInvite = ?SUB(user, ProviderCreatorUserId),
    ?assert(run_invite_token_tests(Testcase(ClusterId, UserEligibleToInvite))),

    AnotherProviderId = ozt_providers:create(),
    AnotherClusterId = AnotherProviderId,
    ProviderEligibleToInvite = ?SUB(?ONEPROVIDER, AnotherProviderId),
    ?assert(run_invite_token_tests(Testcase(AnotherClusterId, ProviderEligibleToInvite))).


user_join_harvester_token(_Config) ->
    HarvesterCreatorUserId = ozt_users:create(),
    HarvesterId = ozt_users:create_harvester_for(HarvesterCreatorUserId),

    ?assert(run_invite_token_tests(#testcase{
        token_type = ?INVITE_TOKEN(?USER_JOIN_HARVESTER, HarvesterId),

        eligible_to_invite = [?SUB(user, HarvesterCreatorUserId)],
        requires_privileges_to_invite = true,
        admin_privilege_to_invite = ?OZ_HARVESTERS_ADD_RELATIONSHIPS,

        supports_carried_privileges = true,
        requires_privileges_to_set_privileges = true,
        allowed_carried_privileges = privileges:harvester_privileges(),
        default_carried_privileges = privileges:harvester_member(),
        admin_privilege_to_set_privileges = ?OZ_HARVESTERS_SET_PRIVILEGES,

        eligible_consumer_types = [user],
        requires_privileges_to_consume = false,
        admin_privilege_to_consume = undefined,

        modify_privileges_fun = fun(?SUB(user, UserId), Modification) ->
            set_user_privs_in_harvester(HarvesterId, UserId, Modification)
        end,
        check_privileges_fun = fun(?SUB(user, UserId), ExpectedPrivileges) ->
            ActualPrivileges = ozt_harvesters:get_user_privileges(HarvesterId, UserId),
            lists:sort(ExpectedPrivileges) =:= lists:sort(ActualPrivileges)
        end,

        prepare_consume_request = fun(Auth = #auth{subject = ?SUB(_, UserId)}, Token) ->
            Data = #{<<"token">> => ozt_tokens:ensure_serialized(Token)},
            #consume_request{
                logic_call_args = {user_logic, join_harvester, [Auth, UserId, Data]},
                rest_call_args = {<<"/user/harvesters/join">>, Data},
                graph_sync_args = {?GRI(od_harvester, undefined, join, private), ?AS_USER(UserId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            ozt_harvesters:delete(HarvesterId)
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            ?ERROR_RELATION_ALREADY_EXISTS(od_user, UserId, od_harvester, HarvesterId)
        end
    })).


group_join_harvester_token(_Config) ->
    HarvesterCreatorUserId = ozt_users:create(),
    HarvesterId = ozt_users:create_harvester_for(HarvesterCreatorUserId),

    ?assert(run_invite_token_tests(#testcase{
        token_type = ?INVITE_TOKEN(?GROUP_JOIN_HARVESTER, HarvesterId),

        eligible_to_invite = [?SUB(user, HarvesterCreatorUserId)],
        requires_privileges_to_invite = true,
        admin_privilege_to_invite = ?OZ_HARVESTERS_ADD_RELATIONSHIPS,

        supports_carried_privileges = true,
        requires_privileges_to_set_privileges = true,
        allowed_carried_privileges = privileges:harvester_privileges(),
        default_carried_privileges = privileges:harvester_member(),
        admin_privilege_to_set_privileges = ?OZ_HARVESTERS_SET_PRIVILEGES,

        eligible_consumer_types = [{user, fun(?SUB(user, UserId)) ->
            GroupId = ozt_users:create_group_for(UserId),
            % Store the group in memory for use in different callbacks
            simple_cache:put({user_group, UserId}, GroupId)
        end}],
        requires_privileges_to_consume = true,
        admin_privilege_to_consume = ?OZ_GROUPS_ADD_RELATIONSHIPS,

        modify_privileges_fun = fun(?SUB(user, UserId), Modification) ->
            case Modification of
                {_, to_invite} ->
                    set_user_privs_in_harvester(HarvesterId, UserId, Modification);
                {_, to_set_privs} ->
                    set_user_privs_in_harvester(HarvesterId, UserId, Modification);
                {_, to_consume} ->
                    {ok, GroupId} = simple_cache:get({user_group, UserId}),
                    set_user_privs_in_group(GroupId, UserId, Modification)
            end
        end,
        check_privileges_fun = fun(?SUB(user, UserId), ExpectedPrivileges) ->
            {ok, GroupId} = simple_cache:get({user_group, UserId}),
            ActualPrivileges = ozt_harvesters:get_group_privileges(HarvesterId, GroupId),
            lists:sort(ExpectedPrivileges) =:= lists:sort(ActualPrivileges)
        end,

        prepare_consume_request = fun(Auth = #auth{subject = ?SUB(_, SubjectId)}, Token) ->
            GroupId = case simple_cache:get({user_group, SubjectId}) of
                {ok, GrId} ->
                    % Covers users that were set up according to eligible_consumer_types
                    GrId;
                _ ->
                    % Covers users with no group and other consumer types.
                    % Other consumer than user does not make sense, but for the
                    % sake of checking if bad consumer is properly handled, try
                    % to use a freshly created group
                    ozt_groups:create()
            end,
            Data = #{<<"token">> => ozt_tokens:ensure_serialized(Token)},
            #consume_request{
                logic_call_args = {group_logic, join_harvester, [Auth, GroupId, Data]},
                rest_call_args = {[<<"/groups/">>, GroupId, <<"/harvesters/join">>], Data},
                graph_sync_args = {?GRI(od_harvester, undefined, join, private), ?AS_GROUP(GroupId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            ozt_harvesters:delete(HarvesterId)
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            {ok, GroupId} = simple_cache:get({user_group, UserId}),
            ?ERROR_RELATION_ALREADY_EXISTS(od_group, GroupId, od_harvester, HarvesterId)
        end
    })).


space_join_harvester_token(_Config) ->
    HarvesterCreatorUserId = ozt_users:create(),
    HarvesterId = ozt_users:create_harvester_for(HarvesterCreatorUserId),

    ?assert(run_invite_token_tests(#testcase{
        token_type = ?INVITE_TOKEN(?SPACE_JOIN_HARVESTER, HarvesterId),

        eligible_to_invite = [?SUB(user, HarvesterCreatorUserId)],
        requires_privileges_to_invite = true,
        admin_privilege_to_invite = ?OZ_HARVESTERS_ADD_RELATIONSHIPS,

        supports_carried_privileges = false,
        requires_privileges_to_set_privileges = false,
        allowed_carried_privileges = undefined,
        default_carried_privileges = undefined,
        admin_privilege_to_set_privileges = undefined,

        eligible_consumer_types = [{user, fun(?SUB(user, UserId)) ->
            SpaceId = ozt_users:create_space_for(UserId),
            % Store the space in memory for use in different callbacks
            simple_cache:put({user_space, UserId}, SpaceId)
        end}],
        requires_privileges_to_consume = true,
        admin_privilege_to_consume = ?OZ_SPACES_ADD_RELATIONSHIPS,

        modify_privileges_fun = fun(?SUB(user, UserId), Modification) ->
            case Modification of
                {_, to_invite} ->
                    set_user_privs_in_harvester(HarvesterId, UserId, Modification);
                {_, to_set_privs} ->
                    set_user_privs_in_harvester(HarvesterId, UserId, Modification);
                {_, to_consume} ->
                    {ok, SpaceId} = simple_cache:get({user_space, UserId}),
                    set_user_privs_in_space(SpaceId, UserId, Modification)
            end
        end,
        check_privileges_fun = undefined,

        prepare_consume_request = fun(Auth = #auth{subject = ?SUB(_, SubjectId)}, Token) ->
            SpaceId = case simple_cache:get({user_space, SubjectId}) of
                {ok, SpId} ->
                    % Covers users that were set up according to eligible_consumer_types
                    SpId;
                _ ->
                    % Covers users with no space and other consumer types.
                    % Other consumer than user does not make sense, but for the
                    % sake of checking if bad consumer is properly handled, try
                    % to use a freshly created group
                    ozt_spaces:create()
            end,
            Data = #{<<"token">> => ozt_tokens:ensure_serialized(Token)},
            #consume_request{
                logic_call_args = {space_logic, join_harvester, [Auth, SpaceId, Data]},
                rest_call_args = {[<<"/spaces/">>, SpaceId, <<"/harvesters/join">>], Data},
                graph_sync_args = {?GRI(od_harvester, undefined, join, private), ?AS_SPACE(SpaceId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            ozt_harvesters:delete(HarvesterId)
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            {ok, SpaceId} = simple_cache:get({user_space, UserId}),
            ?ERROR_RELATION_ALREADY_EXISTS(od_harvester, HarvesterId, od_space, SpaceId)
        end
    })).

%%%===================================================================
%%% Testing framework
%%%===================================================================

run_invite_token_tests(Testcase) ->
    try
        check_bad_token_scenarios(Testcase),
        check_invalid_subject_scenarios(Testcase),
        check_valid_subject_scenarios(Testcase),
        check_invalid_consumer_scenarios(Testcase),
        check_valid_consumer_scenarios(Testcase),
        check_adding_carried_privileges_to_temporary_token(Testcase),
        check_adding_carried_privileges_to_named_token(Testcase),
        check_multi_use_temporary_token(Testcase),
        check_multi_use_named_token(Testcase),
        check_multi_use_privileges_carrying_named_token(Testcase),
        check_temporary_token_revocation(Testcase),
        check_named_token_revocation(Testcase),
        check_token_reuse(Testcase),
        check_invalid_target_scenarios(Testcase),
        check_token_caveats_handling(Testcase),
        % This must be run last as it deletes the target entity id
        check_subject_or_target_entity_deleted_scenarios(Testcase),
        true
    catch Type:Reason ->
        ct:pal("Invite token tests failed due to ~p:~p~nStacktrace: ~s", [
            Type, Reason, lager:pr_stacktrace(erlang:get_stacktrace())
        ]),
        false
    end.


% Check if trying to consume a bad token returns proper errors
check_bad_token_scenarios(Tc = #testcase{token_type = ?INVITE_TOKEN(InviteTokenType, TargetEntityId)}) ->
    DummyProvider = ozt_providers:create(),
    DummyUser = ozt_users:create(),
    SessId = ozt_http:simulate_login(DummyUser),

    AccessToken = ozt_tokens:create(named, ?SUB(?ONEPROVIDER, DummyProvider)),
    GuiAccessToken = ozt_tokens:create_gui_access_token(DummyUser, SessId, ?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)),
    BadToken = <<"not-a-token-definitely-I've-seen-one-I-would-know">>,
    ForgedToken = tokens:construct(#token{
        onezone_domain = ozt:get_domain(),
        id = <<"123123123123">>,
        subject = ?SUB(user, <<"123">>),
        type = ?INVITE_TOKEN(InviteTokenType, TargetEntityId),
        persistent = false
    }, <<"secret">>, []),

    lists:foreach(fun(EligibleConsumerType) ->
        Consumer = create_consumer_with_privs_to_consume(Tc, EligibleConsumerType),
        ?assertEqual(
            ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_NOT_AN_INVITE_TOKEN(InviteTokenType, ?ACCESS_TOKEN)),
            consume_token(Tc, Consumer, AccessToken)
        ),
        ?assertEqual(
            ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_NOT_AN_INVITE_TOKEN(InviteTokenType, ?GUI_ACCESS_TOKEN(SessId))),
            consume_token(Tc, Consumer, GuiAccessToken)
        ),
        ?assertEqual(
            ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN),
            consume_token(Tc, Consumer, BadToken)
        ),
        ?assertEqual(
            % This token passes data validation but is declined in internal logic
            ?ERROR_TOKEN_INVALID,
            consume_token(Tc, Consumer, ForgedToken)
        )
    end, Tc#testcase.eligible_consumer_types).


% It should not be possible to create an invite token for a subject that is not
% authorized to issue invites to the target entity.
%   Auth - authorization of the entity that requests the create operation,
%          does not have to be the same as subject
%   Subject - user or provider for whom the token is created - can be perceived
%             as the one who invites
check_invalid_subject_scenarios(Tc = #testcase{token_type = TokenType}) ->
    SomeUser = ozt_users:create(),
    SomeProvider = ozt_providers:create(),
    TokenManager = ozt_users:create_admin([?OZ_TOKENS_MANAGE]),

    assert_creation_fails(
        ?USER(SomeUser), ?SUB(user, SomeUser), #{<<"type">> => TokenType},
        ?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED
    ),
    assert_creation_fails(
        ?USER(TokenManager), ?SUB(user, SomeUser), #{<<"type">> => TokenType},
        ?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED
    ),
    assert_creation_fails(
        ?PROVIDER(SomeProvider), ?SUB(?ONEPROVIDER, SomeProvider), #{<<"type">> => TokenType},
        ?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED
    ),
    assert_creation_fails(
        ?USER(TokenManager), ?SUB(?ONEPROVIDER, SomeProvider), #{<<"type">> => TokenType},
        ?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED
    ),

    lists:foreach(fun(EligibleSubject) ->
        % SomeUser and SomeProvider should not be able to create tokens for other subject
        assert_creation_fails(
            ?USER(SomeUser), EligibleSubject, #{<<"type">> => TokenType},
            ?ERROR_FORBIDDEN
        ),
        assert_creation_fails(
            ?PROVIDER(SomeProvider), EligibleSubject, #{<<"type">> => TokenType},
            ?ERROR_FORBIDDEN
        )
    end, Tc#testcase.eligible_to_invite).


% The eligible subject should be allowed to create the token, provided that he
% has the required privileges. Admin with ?OZ_TOKENS_MANAGE privileges should
% be able to create tokens on behalf of the eligible subject.
check_valid_subject_scenarios(Tc = #testcase{token_type = TokenType}) ->
    TokenManager = ozt_users:create_admin([?OZ_TOKENS_MANAGE]),
    AdminOfTargetEntity = ozt_users:create_admin([Tc#testcase.admin_privilege_to_invite]),

    % Admin that has the invite privileges in target entity can issue invites to the entity
    assert_creation_succeeds(?USER(TokenManager), ?SUB(user, AdminOfTargetEntity), #{<<"type">> => TokenType}),
    assert_creation_succeeds(?USER(AdminOfTargetEntity), ?SUB(user, AdminOfTargetEntity), #{<<"type">> => TokenType}),

    ModifyPrivsFun = Tc#testcase.modify_privileges_fun,
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            Auth = #auth{subject = EligibleSubject},
            case Tc#testcase.requires_privileges_to_invite of
                false ->
                    assert_creation_succeeds(
                        Persistence, Auth, EligibleSubject, #{<<"type">> => TokenType}
                    ),
                    assert_creation_succeeds(
                        Persistence, ?USER(TokenManager), EligibleSubject, #{<<"type">> => TokenType}
                    );
                true ->
                    ModifyPrivsFun(EligibleSubject, {revoke, to_invite}),
                    assert_creation_fails(
                        Persistence, Auth, EligibleSubject, #{<<"type">> => TokenType},
                        ?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED
                    ),
                    assert_creation_fails(
                        Persistence, ?USER(TokenManager), EligibleSubject, #{<<"type">> => TokenType},
                        ?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED
                    ),
                    ModifyPrivsFun(EligibleSubject, {grant, to_invite}),
                    assert_creation_succeeds(
                        Persistence, Auth, EligibleSubject, #{<<"type">> => TokenType}
                    ),
                    assert_creation_succeeds(
                        Persistence, ?USER(TokenManager), EligibleSubject, #{<<"type">> => TokenType}
                    ),

                    % If the invite privileges are revoked in the meantime, the
                    % token should stop working
                    lists:foreach(fun(EligibleConsumerType) ->
                        Token = ozt_tokens:create(Persistence, EligibleSubject, TokenType),
                        ModifyPrivsFun(EligibleSubject, {revoke, to_invite}),
                        Consumer = create_consumer_by_type(EligibleConsumerType),
                        ?assertMatch(?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED, consume_token(Tc, Consumer, Token)),
                        ModifyPrivsFun(EligibleSubject, {grant, to_invite}),
                        ?assertMatch({ok, _}, consume_token(Tc, Consumer, Token))
                    end, Tc#testcase.eligible_consumer_types)
            end
        end, Tc#testcase.eligible_to_invite)
    end, [named, temporary]).


% Each token supports only specific types of consumers, other types should be disallowed
check_invalid_consumer_scenarios(Tc = #testcase{token_type = TokenType}) ->
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            ensure_privileges_to_invite(Tc, EligibleSubject),
            Token = ozt_tokens:create(Persistence, EligibleSubject, TokenType),
            EligibleConsumerTypes = Tc#testcase.eligible_consumer_types,
            InvalidConsumerTypes = all_consumer_types() -- EligibleConsumerTypes,
            lists:foreach(fun(InvalidConsumerType) ->
                InvalidConsumer = create_consumer_by_type(InvalidConsumerType),
                Error = ?assertMatch({error, _}, consume_token(Tc, InvalidConsumer, Token)),
                ExpectedErrors = case InvalidConsumerType of
                    nobody -> [?ERROR_UNAUTHORIZED];
                    _ -> [?ERROR_FORBIDDEN, ?ERROR_INVITE_TOKEN_CONSUMER_INVALID(InvalidConsumer)]
                end,
                ?assert(lists:member(Error, ExpectedErrors))
            end, InvalidConsumerTypes)
        end, Tc#testcase.eligible_to_invite)
    end, [named, temporary]).


% Depending on token type, additional privileges might be required to consume
% the token (e.g. privileges in a group being joined to other entity). Only
% tokens consumable by users can require privileges, so the code assumes
% that the Subject is always a user in such case.
check_valid_consumer_scenarios(Tc = #testcase{token_type = TokenType}) ->
    ModifyPrivsFun = Tc#testcase.modify_privileges_fun,
    CheckPrivilegesFun = Tc#testcase.check_privileges_fun,
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            ensure_privileges_to_invite(Tc, EligibleSubject),
            lists:foreach(fun(EligibleConsumerType) ->
                Token = ozt_tokens:create(Persistence, EligibleSubject, TokenType),
                Consumer = create_consumer_by_type(EligibleConsumerType),
                case Tc#testcase.requires_privileges_to_consume of
                    false ->
                        ?assertMatch({ok, _}, consume_token(Tc, Consumer, Token)),
                        Tc#testcase.supports_carried_privileges andalso
                            CheckPrivilegesFun(Consumer, Tc#testcase.default_carried_privileges);
                    true ->
                        ModifyPrivsFun(Consumer, {revoke, to_consume}),
                        ?assertMatch(?ERROR_FORBIDDEN, consume_token(Tc, Consumer, Token)),
                        ModifyPrivsFun(Consumer, {grant, to_consume}),
                        ?assertMatch({ok, _}, consume_token(Tc, Consumer, Token)),
                        Tc#testcase.supports_carried_privileges andalso
                            CheckPrivilegesFun(Consumer, Tc#testcase.default_carried_privileges)
                end
            end, Tc#testcase.eligible_consumer_types),

            % Check admin privileges to consume
            Tc#testcase.requires_privileges_to_consume andalso lists:foreach(fun(EligibleConsumerType) ->
                Token = ozt_tokens:create(Persistence, EligibleSubject, TokenType),
                % Privileges are required only for user consumer type
                Consumer = ?SUB(user, UserId) = create_consumer_by_type(EligibleConsumerType),
                ModifyPrivsFun(Consumer, {revoke, to_consume}),
                ozt_users:revoke_oz_privileges(UserId, [Tc#testcase.admin_privilege_to_consume]),
                ?assertMatch(?ERROR_FORBIDDEN, consume_token(Tc, Consumer, Token)),
                ozt_users:grant_oz_privileges(UserId, [Tc#testcase.admin_privilege_to_consume]),
                ?assertMatch({ok, _}, consume_token(Tc, Consumer, Token)),
                Tc#testcase.supports_carried_privileges andalso
                    CheckPrivilegesFun(Consumer, Tc#testcase.default_carried_privileges)
            end, Tc#testcase.eligible_consumer_types)
        end, Tc#testcase.eligible_to_invite)
    end, [named, temporary]).


% Temporary tokens can only carry default privileges - make sure this works as expected
check_adding_carried_privileges_to_temporary_token(#testcase{supports_carried_privileges = false}) ->
    ok;
check_adding_carried_privileges_to_temporary_token(Tc = #testcase{token_type = TokenType}) ->
    ModifyPrivsFun = Tc#testcase.modify_privileges_fun,
    CheckPrivilegesFun = Tc#testcase.check_privileges_fun,
    lists:foreach(fun(EligibleSubject) ->
        Tc#testcase.requires_privileges_to_set_privileges andalso ModifyPrivsFun(EligibleSubject, {grant, to_set_privs}),
        lists:foreach(fun(EligibleConsumerType) ->
            Token = ozt_tokens:create(temporary, EligibleSubject, #{
                <<"type">> => TokenType, <<"privileges">> => Tc#testcase.allowed_carried_privileges
            }),
            Consumer = create_consumer_with_privs_to_consume(Tc, EligibleConsumerType),
            ?assertMatch({ok, _}, consume_token(Tc, Consumer, Token)),
            % Although higher privileges were requested, the temporary token should
            % only grant default ones (privileges should be ignored as they are
            % not supported by the endpoint).
            ?assert(CheckPrivilegesFun(Consumer, Tc#testcase.default_carried_privileges))
        end, Tc#testcase.eligible_consumer_types)
    end, Tc#testcase.eligible_to_invite).


% Privileges can be added to named tokens, but this requires additional privileges
% from the token subject. If the privileges are lost after the token has been
% created, it should no longer work.
check_adding_carried_privileges_to_named_token(#testcase{supports_carried_privileges = false}) ->
    ok;
check_adding_carried_privileges_to_named_token(Tc = #testcase{token_type = TokenType}) ->
    AdminWithoutSetPrivs = ozt_users:create_admin([
        Tc#testcase.admin_privilege_to_invite
    ]),
    AdminWithSetPrivs = ozt_users:create_admin([
        Tc#testcase.admin_privilege_to_invite, Tc#testcase.admin_privilege_to_set_privileges
    ]),

    AllowedPrivileges = Tc#testcase.allowed_carried_privileges,
    CustomPrivileges = lists_utils:random_sublist(AllowedPrivileges),
    DefaultPrivileges = Tc#testcase.default_carried_privileges,
    ModifyPrivsFun = Tc#testcase.modify_privileges_fun,
    CheckPrivilegesFun = Tc#testcase.check_privileges_fun,
    lists:foreach(fun(EligibleSubject) ->
        Auth = #auth{subject = EligibleSubject},
        case Tc#testcase.requires_privileges_to_set_privileges of
            false ->
                assert_creation_succeeds(
                    named, Auth, EligibleSubject, #{<<"type">> => TokenType, <<"privileges">> => CustomPrivileges}
                );
            true ->
                ModifyPrivsFun(EligibleSubject, {revoke, to_invite}),
                ModifyPrivsFun(EligibleSubject, {revoke, to_set_privs}),
                assert_creation_fails(
                    named, Auth, EligibleSubject, #{<<"type">> => TokenType, <<"privileges">> => CustomPrivileges},
                    ?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED
                ),
                ModifyPrivsFun(EligibleSubject, {grant, to_invite}),
                ModifyPrivsFun(EligibleSubject, {revoke, to_set_privs}),
                assert_creation_fails(
                    named, Auth, EligibleSubject, #{<<"type">> => TokenType, <<"privileges">> => CustomPrivileges},
                    ?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED
                ),
                ModifyPrivsFun(EligibleSubject, {grant, to_set_privs}),
                ModifyPrivsFun(EligibleSubject, {revoke, to_invite}),
                assert_creation_fails(
                    named, Auth, EligibleSubject, #{<<"type">> => TokenType, <<"privileges">> => CustomPrivileges},
                    ?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED
                ),
                ModifyPrivsFun(EligibleSubject, {grant, to_invite}),
                ModifyPrivsFun(EligibleSubject, {grant, to_set_privs}),
                assert_creation_succeeds(
                    named, Auth, EligibleSubject, #{<<"type">> => TokenType, <<"privileges">> => CustomPrivileges}
                )
        end,

        % Check if privileges are correctly validated
        assert_creation_fails(
            named, Auth, EligibleSubject, #{<<"type">> => TokenType, <<"privileges">> => [abc | CustomPrivileges]},
            ?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(<<"privileges">>, AllowedPrivileges)
        ),

        % Additional privileges are required to create a token carrying privileges,
        % as well as at the moment of consumption
        Tc#testcase.requires_privileges_to_set_privileges andalso lists:foreach(fun(EligibleConsumerType) ->
            Token = ozt_tokens:create(named, EligibleSubject, #{
                <<"type">> => TokenType, <<"privileges">> => CustomPrivileges
            }),
            Consumer = create_consumer_with_privs_to_consume(Tc, EligibleConsumerType),
            % Token becomes invalid when the subject loses privileges to set privileges,
            % however if the carried privileges in an already existing token are
            % identical to default, consuming is still possible
            case lists:sort(CustomPrivileges) =:= lists:sort(DefaultPrivileges) of
                true ->
                    ?assertMatch({ok, _}, consume_token(Tc, Consumer, Token)),
                    ?assert(CheckPrivilegesFun(Consumer, CustomPrivileges));
                false ->
                    ModifyPrivsFun(EligibleSubject, {revoke, to_set_privs}),
                    ?assertMatch(?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED, consume_token(Tc, Consumer, Token)),
                    % But it becomes valid again if the privileges are restored
                    ModifyPrivsFun(EligibleSubject, {grant, to_invite}),
                    ModifyPrivsFun(EligibleSubject, {grant, to_set_privs}),
                    ?assertMatch({ok, _}, consume_token(Tc, Consumer, Token)),
                    ?assert(CheckPrivilegesFun(Consumer, CustomPrivileges))
            end
        end, Tc#testcase.eligible_consumer_types),

        % Check admin privileges to create a token with privileges
        assert_creation_fails(
            named, ?USER(AdminWithoutSetPrivs), ?SUB(user, AdminWithoutSetPrivs),
            #{<<"type">> => TokenType, <<"privileges">> => CustomPrivileges},
            ?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED
        ),
        assert_creation_succeeds(
            named, ?USER(AdminWithSetPrivs), ?SUB(user, AdminWithSetPrivs),
            #{<<"type">> => TokenType, <<"privileges">> => CustomPrivileges}
        )
    end, Tc#testcase.eligible_to_invite).


% Temporary tokens cannot have a usage limit and must have a time caveat. This
% makes them inherently multi-use within time validity.
check_multi_use_temporary_token(Tc = #testcase{token_type = TokenType}) ->
    lists:foreach(fun(EligibleSubject) ->
        ensure_privileges_to_invite(Tc, EligibleSubject),
        Token = ozt_tokens:create(temporary, EligibleSubject, #{
            <<"type">> => TokenType, <<"usageLimit">> => 3
        }),
        % Although usage limit was requested, the temporary token should always
        % be multi-use (usageLimit should be ignored as it is not supported by
        % the endpoint).
        lists:foreach(fun(_) ->
            Consumer = create_consumer_with_privs_to_consume(Tc, random_eligible),
            ?assertMatch({ok, _}, consume_token(Tc, Consumer, Token))
        end, lists:seq(1, 7))
    end, Tc#testcase.eligible_to_invite).


% Named tokens can have a usage limit. If not specified, it defaults to infinite.
check_multi_use_named_token(Tc = #testcase{token_type = TokenType}) ->
    lists:foreach(fun(EligibleSubject) ->
        ensure_privileges_to_invite(Tc, EligibleSubject),

        ?ERROR_BAD_VALUE_TOO_LOW(<<"usageLimit">>, 1) = ozt_tokens:try_create(
            named, EligibleSubject, #{<<"type">> => TokenType, <<"usageLimit">> => 0}
        ),

        SingleUseToken = ozt_tokens:create(named, EligibleSubject, #{
            <<"type">> => TokenType, <<"usageLimit">> => 1
        }),
        ConsumerAlpha = create_consumer_with_privs_to_consume(Tc, random_eligible),
        ConsumerBeta = create_consumer_with_privs_to_consume(Tc, random_eligible),
        ?assertMatch({ok, _}, consume_token(Tc, ConsumerAlpha, SingleUseToken)),
        ?assertMatch(?ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED, consume_token(Tc, ConsumerBeta, SingleUseToken)),

        UsageLimit = 17,
        MultiUseToken = ozt_tokens:create(named, EligibleSubject, #{
            <<"type">> => TokenType, <<"usageLimit">> => UsageLimit
        }),

        Consumers = lists:map(fun(_) ->
            create_consumer_with_privs_to_consume(Tc, random_eligible)
        end, lists:seq(1, UsageLimit)),
        utils:pforeach(fun(Consumer) ->
            ?assertMatch({ok, _}, consume_token(Tc, Consumer, MultiUseToken))
        end, Consumers),

        ConsumerGamma = create_consumer_with_privs_to_consume(Tc, random_eligible),
        ?assertMatch(?ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED, consume_token(Tc, ConsumerGamma, MultiUseToken))
    end, Tc#testcase.eligible_to_invite).


% Check the combination of multi-use and privilege carrying in a named token.
check_multi_use_privileges_carrying_named_token(#testcase{supports_carried_privileges = false}) ->
    ok;
check_multi_use_privileges_carrying_named_token(Tc = #testcase{token_type = TokenType}) ->
    AllowedPrivileges = Tc#testcase.allowed_carried_privileges,
    CustomPrivileges = lists_utils:random_sublist(AllowedPrivileges),
    ModifyPrivsFun = Tc#testcase.modify_privileges_fun,
    CheckPrivilegesFun = Tc#testcase.check_privileges_fun,

    lists:foreach(fun(EligibleSubject) ->
        Tc#testcase.requires_privileges_to_invite andalso ModifyPrivsFun(EligibleSubject, {grant, to_invite}),
        Tc#testcase.requires_privileges_to_set_privileges andalso ModifyPrivsFun(EligibleSubject, {grant, to_set_privs}),

        Token = ozt_tokens:create(named, EligibleSubject, #{
            <<"type">> => TokenType, <<"usageLimit">> => 3, <<"privileges">> => CustomPrivileges
        }),
        ConsumerAlpha = create_consumer_by_type(lists_utils:random_element(Tc#testcase.eligible_consumer_types)),
        case Tc#testcase.requires_privileges_to_consume of
            false ->
                ?assertMatch({ok, _}, consume_token(Tc, ConsumerAlpha, Token));
            true ->
                ModifyPrivsFun(ConsumerAlpha, {revoke, to_consume}),
                ?assertMatch(?ERROR_FORBIDDEN, consume_token(Tc, ConsumerAlpha, Token)),
                ModifyPrivsFun(ConsumerAlpha, {grant, to_consume}),
                ?assertMatch({ok, _}, consume_token(Tc, ConsumerAlpha, Token))
        end,
        CheckPrivilegesFun(ConsumerAlpha, CustomPrivileges),
        % There should be 2 uses left
        ConsumerBeta = create_consumer_with_privs_to_consume(Tc, random_eligible),
        case Tc#testcase.requires_privileges_to_set_privileges of
            false ->
                ok;
            true ->
                ModifyPrivsFun(EligibleSubject, {revoke, to_set_privs}),
                ?assertMatch(?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED, consume_token(Tc, ConsumerBeta, Token)),
                ModifyPrivsFun(EligibleSubject, {grant, to_invite}),
                ModifyPrivsFun(EligibleSubject, {grant, to_set_privs})
        end,
        ?assertMatch({ok, _}, consume_token(Tc, ConsumerBeta, Token)),
        CheckPrivilegesFun(ConsumerBeta, CustomPrivileges),
        % There should be 1 use left
        ConsumerGamma = create_consumer_by_type(lists_utils:random_element(Tc#testcase.eligible_consumer_types)),
        case Tc#testcase.requires_privileges_to_consume of
            false ->
                ?assertMatch({ok, _}, consume_token(Tc, ConsumerGamma, Token));
            true ->
                % Privileges are required only for user consumer type
                ?SUB(user, UserId) = ConsumerGamma,
                ModifyPrivsFun(ConsumerGamma, {revoke, to_consume}),
                ozt_users:revoke_oz_privileges(UserId, [Tc#testcase.admin_privilege_to_consume]),
                ?assertMatch(?ERROR_FORBIDDEN, consume_token(Tc, ConsumerGamma, Token)),
                ozt_users:grant_oz_privileges(UserId, [Tc#testcase.admin_privilege_to_consume]),
                ?assertMatch({ok, _}, consume_token(Tc, ConsumerGamma, Token))
        end,
        CheckPrivilegesFun(ConsumerGamma, CustomPrivileges),
        % There should be no uses left
        ConsumerDelta = create_consumer_with_privs_to_consume(Tc, random_eligible),
        ?assertMatch(?ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED, consume_token(Tc, ConsumerDelta, Token))
    end, Tc#testcase.eligible_to_invite).


% Temporary tokens are not revocable individually, but can be revoked all at once
check_temporary_token_revocation(Tc = #testcase{token_type = TokenType}) ->
    lists:foreach(fun(EligibleSubject) ->
        ensure_privileges_to_invite(Tc, EligibleSubject),
        TokenAlpha = ozt_tokens:create(temporary, EligibleSubject, TokenType),
        TokenBeta = ozt_tokens:create(temporary, EligibleSubject, TokenType),
        TokenGamma = ozt_tokens:create(temporary, EligibleSubject, TokenType),
        lists:foreach(fun(_) ->
            lists:foreach(fun(Token) ->
                Consumer = create_consumer_with_privs_to_consume(Tc, random_eligible),
                ?assertMatch({ok, _}, consume_token(Tc, Consumer, Token))
            end, [TokenAlpha, TokenBeta, TokenGamma])
        end, lists:seq(1, 9)),
        ozt_tokens:revoke_all_temporary_tokens(EligibleSubject),
        lists:foreach(fun(Token) ->
            Consumer = create_consumer_with_privs_to_consume(Tc, random_eligible),
            ?assertMatch(?ERROR_TOKEN_INVALID, consume_token(Tc, Consumer, Token))
        end, [TokenAlpha, TokenBeta, TokenGamma])
    end, Tc#testcase.eligible_to_invite).


% Named tokens are revocable individually. The revocation can be undone at will.
check_named_token_revocation(Tc = #testcase{token_type = TokenType}) ->
    lists:foreach(fun(EligibleSubject) ->
        ensure_privileges_to_invite(Tc, EligibleSubject),
        Token = ozt_tokens:create(named, EligibleSubject, TokenType),
        Consumer = create_consumer_with_privs_to_consume(Tc, random_eligible),
        ozt_tokens:toggle_revoked(Token, true),
        ?assertMatch(?ERROR_TOKEN_REVOKED, consume_token(Tc, Consumer, Token)),
        ozt_tokens:toggle_revoked(Token, false),
        ?assertMatch({ok, _}, consume_token(Tc, Consumer, Token)),
        ozt_tokens:toggle_revoked(Token, true),
        ?assertMatch(?ERROR_TOKEN_REVOKED, consume_token(Tc, Consumer, Token))
    end, Tc#testcase.eligible_to_invite).


% Depending on the token type, some tokens can be consumed once (e.g. joining
% a space is not possible if the user is already a member), or infinitely
% (e.g. registering Oneproviders with a token). It should depend on the token's
% target entity rather than the token itself. In other words; there should be no
% difference if this is one multi-use token, or two different invite tokens to
% the same target entity.
check_token_reuse(Tc = #testcase{token_type = TokenType}) ->
    ExpectedReuseResultFun = Tc#testcase.expected_reuse_result_fun,
    lists:foreach(fun(EligibleSubject) ->
        ensure_privileges_to_invite(Tc, EligibleSubject),
        lists:foreach(fun(EligibleConsumerType) ->
            % Check two tokens to the same target entity
            lists:foreach(fun(Persistence) ->
                TokenAlpha = ozt_tokens:create(Persistence, EligibleSubject, TokenType),
                TokenBeta = ozt_tokens:create(Persistence, EligibleSubject, TokenType),
                Consumer = create_consumer_with_privs_to_consume(Tc, EligibleConsumerType),
                ?assertMatch({ok, _}, consume_token(Tc, Consumer, TokenAlpha)),
                case ExpectedReuseResultFun(Consumer) of
                    ok ->
                        ?assertMatch({ok, _}, consume_token(Tc, Consumer, TokenBeta));
                    {error, _} = Error ->
                        ?assertEqual(Error, consume_token(Tc, Consumer, TokenBeta))
                end
            end, [named, temporary]),

            % Check one multi-use token (only named tokens can be multi-use)
            UsageLimit = 6,
            MultiUseToken = ozt_tokens:create(named, EligibleSubject, #{
                <<"type">> => TokenType, <<"usageLimit">> => UsageLimit
            }),
            AnotherConsumer = create_consumer_with_privs_to_consume(Tc, EligibleConsumerType),
            ?assertMatch({ok, _}, consume_token(Tc, AnotherConsumer, MultiUseToken)),
            lists:foreach(fun(_) ->
                case ExpectedReuseResultFun(AnotherConsumer) of
                    ok ->
                        ?assertMatch({ok, _}, consume_token(Tc, AnotherConsumer, MultiUseToken));
                    {error, _} = Error ->
                        ?assertEqual(Error, consume_token(Tc, AnotherConsumer, MultiUseToken))
                end
            end, lists:seq(1, UsageLimit - 1)),

            % If reuse is allowed, the token should have reached its usage limit
            case ExpectedReuseResultFun(AnotherConsumer) of
                ok ->
                    ?assertMatch(?ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED, consume_token(Tc, AnotherConsumer, MultiUseToken));
                _ ->
                    ok
            end
        end, Tc#testcase.eligible_consumer_types)
    end, Tc#testcase.eligible_to_invite).


% It should not be possible to create a token for an inexistent target entity id
check_invalid_target_scenarios(Tc = #testcase{token_type = ?INVITE_TOKEN(InviteTokenType, _)}) ->
    Admin = ozt_users:create_admin(),
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            EligibleAuth = #auth{subject = EligibleSubject},
            ensure_privileges_to_invite(Tc, EligibleSubject),
            lists:foreach(fun(Auth) ->
                BadTokenType = ?INVITE_TOKEN(InviteTokenType, <<"1234">>),
                Error = ?assertMatch(
                    {error, _},
                    ozt_tokens:try_create(Auth, Persistence, EligibleSubject, #{<<"type">> => BadTokenType})
                ),
                ?assert(lists:member(Error, [?ERROR_FORBIDDEN, ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"1234">>)]))
            end, [EligibleAuth, ?USER(Admin)])
        end, Tc#testcase.eligible_to_invite)
    end, [named, temporary]).


% Check if adding caveats to invite tokens works as expected.
check_token_caveats_handling(Tc = #testcase{token_type = TokenType}) ->
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            ensure_privileges_to_invite(Tc, EligibleSubject),
            lists:foreach(fun(_) ->
                Consumer = create_consumer_with_privs_to_consume(Tc, random_eligible),
                RandCorrectCaveats = lists_utils:random_sublist(gen_correct_caveats(Consumer)),
                RandUnverifiedCaveats = lists_utils:random_sublist(gen_unverified_caveats(Consumer)),
                Token = ozt_tokens:create(Persistence, EligibleSubject, #{
                    <<"type">> => TokenType,
                    <<"caveats">> => RandCorrectCaveats ++ RandUnverifiedCaveats
                }),
                Result = consume_token(Tc, Consumer, Token),
                case RandUnverifiedCaveats of
                    [] ->
                        ?assertMatch({ok, _}, Result);
                    _ ->
                        ?assertMatch(?ERROR_TOKEN_CAVEAT_UNVERIFIED(_), Result),
                        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(UnverifiedCaveat) = Result,
                        ?assert(lists:member(UnverifiedCaveat, RandUnverifiedCaveats))
                end
            end, lists:seq(1, 20))
        end, Tc#testcase.eligible_to_invite)
    end, [named, temporary]).


gen_correct_caveats(Consumer) -> lists:flatten([
    #cv_time{valid_until = ozt:cluster_time_seconds() + 10},
    #cv_ip{whitelist = [?CORRECT_MASK_1]},
    #cv_ip{whitelist = [?CORRECT_MASK_2]},
    #cv_ip{whitelist = [{{0, 0, 0, 0}, 8}, ?CORRECT_MASK_1, ?CORRECT_MASK_2]},
    #cv_asn{whitelist = [?CLIENT_ASN]},
    #cv_country{type = whitelist, list = [?CLIENT_COUNTRY]},
    #cv_country{type = blacklist, list = [?INCORRECT_COUNTRY]},
    #cv_region{type = whitelist, list = ?CLIENT_REGIONS},
    #cv_region{type = blacklist, list = ?INCORRECT_REGIONS},
    case Consumer of
        ?SUB(user, UserId) ->
            Group = ozt_users:create_group_for(UserId),
            ozt:reconcile_entity_graph(),
            [
                #cv_audience{whitelist = [?AUD(group, Group)]},
                #cv_audience{whitelist = [?AUD(group, <<"*">>)]},
                #cv_audience{whitelist = [?AUD(user, UserId)]},
                #cv_audience{whitelist = [?AUD(user, <<"*">>)]}
            ];
        ?SUB(?ONEPROVIDER, ProviderId) ->
            [
                #cv_audience{whitelist = [?AUD(?OP_WORKER, ProviderId)]},
                #cv_audience{whitelist = [?AUD(?OP_WORKER, <<"*">>)]}
            ];
        ?SUB(nobody) ->
            []
    end
]).


gen_unverified_caveats(Consumer) -> lists:flatten([
    #cv_time{valid_until = ozt:cluster_time_seconds() - 1},
    #cv_authorization_none{},
    #cv_ip{whitelist = [?INCORRECT_MASK_1]},
    #cv_ip{whitelist = [?INCORRECT_MASK_2]},
    #cv_asn{whitelist = [?INCORRECT_ASN]},
    #cv_country{type = whitelist, list = [?INCORRECT_COUNTRY]},
    #cv_country{type = blacklist, list = [?CLIENT_COUNTRY]},
    #cv_region{type = whitelist, list = ?INCORRECT_REGIONS},
    #cv_region{type = blacklist, list = ?CLIENT_REGIONS},
    case Consumer of
        ?SUB(user, _) ->
            [
                #cv_audience{whitelist = [?AUD(user, <<"123">>)]},
                #cv_audience{whitelist = [?AUD(group, <<"123">>)]},
                #cv_audience{whitelist = [?AUD(?OP_WORKER, <<"*">>)]}
            ];
        ?SUB(?ONEPROVIDER, _) ->
            [
                #cv_audience{whitelist = [?AUD(user, <<"*">>)]},
                #cv_audience{whitelist = [?AUD(group, <<"*">>)]},
                #cv_audience{whitelist = [?AUD(?OP_WORKER, <<"123">>)]}
            ];
        ?SUB(nobody) ->
            [
                #cv_audience{whitelist = [?AUD(user, <<"123">>)]},
                #cv_audience{whitelist = [?AUD(user, <<"*">>)]},
                #cv_audience{whitelist = [?AUD(group, <<"123">>)]},
                #cv_audience{whitelist = [?AUD(group, <<"*">>)]},
                #cv_audience{whitelist = [?AUD(?OP_WORKER, <<"123">>)]},
                #cv_audience{whitelist = [?AUD(?OP_WORKER, <<"*">>)]}
            ]
    end,
    % Below caveats are not supported by invite tokens
    % (should cause verification failure)
    #cv_interface{interface = graphsync},
    #cv_interface{interface = oneclient},
    #cv_api{whitelist = [{all, all, ?GRI_PATTERN('*', '*', '*', '*')}]},
    #cv_data_readonly{},
    #cv_data_path{whitelist = [<<"/space/dir/file.txt">>]},
    #cv_data_objectid{whitelist = [element(2, {ok, _} = file_id:guid_to_objectid(
        file_id:pack_guid(<<"123456">>, <<"spaceId">>)
    ))]}
]).


% If the token has been created correctly, but the target entity or
% inviting subject is deleted in the meantime, it should stop working
check_subject_or_target_entity_deleted_scenarios(Tc = #testcase{token_type = TokenType}) ->
    DeleteTargetEntityFun = Tc#testcase.delete_target_entity_fun,
    ?INVITE_TOKEN(_, TargetEntityId) = TokenType,
    TokensToCheck = lists:flatmap(fun(Persistence) ->
        lists:flatmap(fun(EligibleSubject) ->
            ensure_privileges_to_invite(Tc, EligibleSubject),
            Token = ozt_tokens:create(Persistence, EligibleSubject, TokenType),
            [{EligibleSubject, Token}]
        end, Tc#testcase.eligible_to_invite)
    end, [named, temporary]),

    DeleteTargetEntityFun(),
    lists:foreach(fun({Subject, TokenToCheck}) ->
        lists:foreach(fun(EligibleConsumerType) ->
            Consumer = create_consumer_with_privs_to_consume(Tc, EligibleConsumerType),
            ExpError = case Subject of
                ?SUB(_, TargetEntityId) -> ?ERROR_TOKEN_INVALID;
                _ -> ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(TargetEntityId)
            end,
            ?assertEqual(ExpError, consume_token(Tc, Consumer, TokenToCheck))
        end, Tc#testcase.eligible_consumer_types)
    end, TokensToCheck),

    % If the token subject is deleted, the token should become invalid
    lists:foreach(fun({Subject, TokenToCheck}) ->
        case Subject of
            ?SUB(user, UserId) -> ozt:rpc(user_logic, delete, [?ROOT, UserId]);
            ?SUB(?ONEPROVIDER, PrId) -> ozt:rpc(provider_logic, delete, [?ROOT, PrId])
        end,
        lists:foreach(fun(EligibleConsumerType) ->
            Consumer = create_consumer_with_privs_to_consume(Tc, EligibleConsumerType),
            ?assertMatch(?ERROR_TOKEN_INVALID, consume_token(Tc, Consumer, TokenToCheck))
        end, Tc#testcase.eligible_consumer_types)
    end, TokensToCheck).

%%%===================================================================
%%% Helper functions
%%%===================================================================

set_user_privs_in_group(GroupId, UserId, {GrantOrRevoke, to_invite}) ->
    set_user_privs(group_logic, GroupId, UserId, GrantOrRevoke, [?GROUP_ADD_USER, ?GROUP_ADD_CHILD]);
set_user_privs_in_group(GroupId, UserId, {GrantOrRevoke, to_consume}) ->
    set_user_privs(group_logic, GroupId, UserId, GrantOrRevoke, [
        ?GROUP_ADD_PARENT, ?GROUP_ADD_SPACE, ?GROUP_ADD_CLUSTER, ?GROUP_ADD_HARVESTER
    ]);
set_user_privs_in_group(GroupId, UserId, {GrantOrRevoke, to_set_privs}) ->
    set_user_privs(group_logic, GroupId, UserId, GrantOrRevoke, [?GROUP_SET_PRIVILEGES]).

set_user_privs_in_space(SpaceId, UserId, {GrantOrRevoke, to_invite}) ->
    set_user_privs(space_logic, SpaceId, UserId, GrantOrRevoke, [?SPACE_ADD_USER, ?SPACE_ADD_GROUP, ?SPACE_ADD_SUPPORT]);
set_user_privs_in_space(SpaceId, UserId, {GrantOrRevoke, to_consume}) ->
    set_user_privs(space_logic, SpaceId, UserId, GrantOrRevoke, [?SPACE_ADD_HARVESTER]);
set_user_privs_in_space(SpaceId, UserId, {GrantOrRevoke, to_set_privs}) ->
    set_user_privs(space_logic, SpaceId, UserId, GrantOrRevoke, [?SPACE_SET_PRIVILEGES]).

set_user_privs_in_cluster(ClusterId, UserId, {GrantOrRevoke, to_invite}) ->
    set_user_privs(cluster_logic, ClusterId, UserId, GrantOrRevoke, [?CLUSTER_ADD_USER, ?CLUSTER_ADD_GROUP]);
set_user_privs_in_cluster(ClusterId, UserId, {GrantOrRevoke, to_set_privs}) ->
    set_user_privs(cluster_logic, ClusterId, UserId, GrantOrRevoke, [?CLUSTER_SET_PRIVILEGES]).

set_user_privs_in_harvester(HarvesterId, UserId, {GrantOrRevoke, to_invite}) ->
    set_user_privs(harvester_logic, HarvesterId, UserId, GrantOrRevoke, [
        ?HARVESTER_ADD_USER, ?HARVESTER_ADD_GROUP, ?HARVESTER_ADD_SPACE
    ]);
set_user_privs_in_harvester(HarvesterId, UserId, {GrantOrRevoke, to_set_privs}) ->
    set_user_privs(harvester_logic, HarvesterId, UserId, GrantOrRevoke, [?HARVESTER_SET_PRIVILEGES]).


% Grants or revokes privileges to perform an operation. In case of revoke,
% randomly removes the member from the entity (making him effectively lose all
% privileges) or revokes given privileges. These two scenarios which should be
% equivalent from the point of view of authorization to invite .
set_user_privs(LogicModule, EntityId, UserId, grant, Privileges) ->
    ozt:rpc(LogicModule, add_user, [?ROOT, EntityId, UserId]),
    ozt:rpc(LogicModule, update_user_privileges, [?ROOT, EntityId, UserId, Privileges, []]);
set_user_privs(LogicModule, EntityId, UserId, revoke, Privileges) ->
    case rand:uniform(2) of
        1 -> ozt:rpc(LogicModule, remove_user, [?ROOT, EntityId, UserId]);
        2 -> ozt:rpc(LogicModule, update_user_privileges, [?ROOT, EntityId, UserId, [], Privileges])
    end.


assert_creation_succeeds(Auth, Subject, Data) ->
    assert_creation_succeeds(named, Auth, Subject, Data),
    assert_creation_succeeds(temporary, Auth, Subject, Data).
assert_creation_succeeds(Persistence, Auth, Subject, Data) ->
    ?assertMatch({ok, _}, ozt_tokens:try_create(Auth, Persistence, Subject, Data)).


assert_creation_fails(Auth, Subject, Data, ExpError) ->
    assert_creation_fails(named, Auth, Subject, Data, ExpError),
    assert_creation_fails(temporary, Auth, Subject, Data, ExpError).
assert_creation_fails(Persistence, Auth, Subject, Data, {error, _} = ExpError) ->
    ?assertEqual(ExpError, ozt_tokens:try_create(Auth, Persistence, Subject, Data)).


ensure_privileges_to_invite(#testcase{requires_privileges_to_invite = false}, _Subject) ->
    ok;
ensure_privileges_to_invite(Tc = #testcase{requires_privileges_to_invite = true}, Subject) ->
    ModifyPrivsFun = Tc#testcase.modify_privileges_fun,
    ModifyPrivsFun(Subject, {grant, to_invite}).


create_consumer_by_type({SubjectType, MakeConsumerEligibleFun}) ->
    Subject = create_consumer_by_type(SubjectType),
    MakeConsumerEligibleFun(Subject),
    Subject;
create_consumer_by_type(nobody) ->
    ?SUB(nobody);
create_consumer_by_type(user) ->
    ?SUB(user, ozt_users:create());
create_consumer_by_type(?ONEPROVIDER) ->
    ?SUB(?ONEPROVIDER, ozt_providers:create()).


create_consumer_with_privs_to_consume(Tc, random_eligible) ->
    ConsumerType = lists_utils:random_element(Tc#testcase.eligible_consumer_types),
    create_consumer_with_privs_to_consume(Tc, ConsumerType);
create_consumer_with_privs_to_consume(Tc, ConsumerType) ->
    Consumer = create_consumer_by_type(ConsumerType),
    ModifyPrivsFun = Tc#testcase.modify_privileges_fun,
    Tc#testcase.requires_privileges_to_consume andalso ModifyPrivsFun(Consumer, {grant, to_consume}),
    Consumer.


all_consumer_types() -> [
    user, ?ONEPROVIDER, nobody
].


consume_token(Tc, Consumer, Token) ->
    PrepareConsumeRequest = Tc#testcase.prepare_consume_request,
    Auth = #auth{subject = Consumer, peer_ip = ?PEER_IP},
    ConsumeRequest = PrepareConsumeRequest(Auth, Token),
    consume_token(Auth, ConsumeRequest).


consume_token(Auth, ConsumeRequest) ->
    AvailableInterfaces = case ConsumeRequest#consume_request.rest_call_args of
        not_available -> [logic, graphsync];
        _ -> [logic, graphsync, rest]
    end,
    case lists_utils:random_element(AvailableInterfaces) of
        logic -> consume_by_logic_call(ConsumeRequest);
        graphsync -> consume_by_graphsync_request(Auth, ConsumeRequest);
        rest -> consume_by_rest_call(Auth, ConsumeRequest)
    end.


consume_by_logic_call(#consume_request{logic_call_args = {Module, Function, Args}}) ->
    ozt:rpc(Module, Function, Args).


consume_by_rest_call(Auth, #consume_request{rest_call_args = {UrnTokens, Data}}) ->
    ozt_http:rest_call(auth_to_client_auth(Auth), post, UrnTokens, Data).


consume_by_graphsync_request(Auth, #consume_request{graph_sync_args = {GRI, AuthHint, Data}}) ->
    Endpoint = case Auth of
        ?PROVIDER(_) -> oneprovider;
        _ -> gui
    end,
    % EntityId in the AuthHint can be undefined due to testing different
    % combinations of subjects - make sure it is serializable as GS expects a
    % binary entity type.
    SerializableAuthHint = case AuthHint of
        undefined -> undefined;
        {HintType, undefined} -> undefined;
        {HintType, EntityId} -> {HintType, EntityId}
    end,
    GsReq = #gs_req{
        subtype = graph,
        request = #gs_req_graph{
            gri = GRI,
            operation = create,
            data = Data,
            subscribe = false,
            auth_hint = SerializableAuthHint
        }
    },
    ozt_gs:connect_and_request(Endpoint, auth_to_client_auth(Auth), GsReq).


auth_to_client_auth(?NOBODY) ->
    undefined;
auth_to_client_auth(?USER(UserId)) ->
    {token, ozt_tokens:create(temporary, ?SUB(user, UserId))};
auth_to_client_auth(?PROVIDER(PrId)) ->
    {token, ozt_tokens:create(temporary, ?SUB(?ONEPROVIDER, PrId))}.
