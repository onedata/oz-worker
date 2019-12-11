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
    rest_call_args :: not_available | {PathOrTokens :: binary() | [binary()], Data :: map()},
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
    % being joined to an entity). The seconds element is the function used to
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
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

init_per_testcase(_, Config) ->
    oz_test_utils:mock_gui_static(Config),
    oz_test_utils:mock_time(Config),
    oz_test_utils:mock_harvester_plugins(Config, ?HARVESTER_MOCK_PLUGIN),
    oz_test_utils:mock_peer_ip_of_all_connections(Config, ?PEER_IP),
    oz_test_utils:mock_geo_db_entry_for_all_ips(Config, ?CLIENT_ASN, ?CLIENT_COUNTRY, ?CLIENT_REGIONS),
    oz_test_utils:delete_all_entities(Config),
    store_test_config(Config),
    Config.

end_per_testcase(_, Config) ->
    oz_test_utils:unmock_time(Config),
    oz_test_utils:unmock_gui_static(Config),
    oz_test_utils:unmock_harvester_plugins(Config, ?HARVESTER_MOCK_PLUGIN),
    oz_test_utils:unmock_peer_ip_of_all_connections(Config),
    oz_test_utils:unmock_geo_db_entry_for_all_ips(Config),
    ok.

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().

store_test_config(Config) ->
    simple_cache:put(test_config, Config).

get_test_config() ->
    case simple_cache:get(test_config) of
        {ok, Config} -> Config;
        _ -> error("Call store_test_config at the beggining of the test.")
    end.

%%%===================================================================
%%% Test cases
%%%===================================================================

user_join_group_token(Config) ->
    {ok, GroupCreatorUserId} = oz_test_utils:create_user(Config),
    {ok, GroupId} = oz_test_utils:create_group(Config, ?USER(GroupCreatorUserId)),

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
            {ok, ActualPrivileges} = oz_rpc(group_logic, get_user_privileges, [?ROOT, GroupId, UserId]),
            lists:sort(ExpectedPrivileges) =:= lists:sort(ActualPrivileges)
        end,

        prepare_consume_request = fun(Auth = #auth{subject = ?SUB(_, UserId)}, Token) ->
            Data = #{<<"token">> => ensure_token_serialized(Token)},
            #consume_request{
                logic_call_args = {user_logic, join_group, [Auth, UserId, Data]},
                rest_call_args = {<<"/user/groups/join">>, Data},
                graph_sync_args = {?GRI(od_group, undefined, join, private), ?AS_USER(UserId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            oz_rpc(group_logic, delete, [?ROOT, GroupId])
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            ?ERROR_RELATION_ALREADY_EXISTS(od_user, UserId, od_group, GroupId)
        end
    })).


group_join_group_token(Config) ->
    {ok, GroupCreatorUserId} = oz_test_utils:create_user(Config),
    {ok, ParentGroupId} = oz_test_utils:create_group(Config, ?USER(GroupCreatorUserId)),

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
            {ok, ChildGroupId} = oz_test_utils:create_group(get_test_config(), ?USER(UserId)),
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
            {ok, ActualPrivileges} = oz_rpc(group_logic, get_child_privileges, [?ROOT, ParentGroupId, ChildGroupId]),
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
                    {ok, GrId} = oz_test_utils:create_group(Config, ?ROOT),
                    GrId
            end,
            Data = #{<<"token">> => ensure_token_serialized(Token)},
            #consume_request{
                logic_call_args = {group_logic, join_group, [Auth, ChildGroupId, Data]},
                rest_call_args = {[<<"/groups/">>, ChildGroupId, <<"/parents/join">>], Data},
                graph_sync_args = {?GRI(od_group, undefined, join, private), ?AS_GROUP(ChildGroupId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            oz_rpc(group_logic, delete, [?ROOT, ParentGroupId])
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            {ok, ChildGroupId} = simple_cache:get({user_group, UserId}),
            ?ERROR_RELATION_ALREADY_EXISTS(od_group, ChildGroupId, od_group, ParentGroupId)
        end
    })).


user_join_space_token(Config) ->
    {ok, SpaceCreatorUserId} = oz_test_utils:create_user(Config),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(SpaceCreatorUserId)),

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
            {ok, ActualPrivileges} = oz_rpc(space_logic, get_user_privileges, [?ROOT, SpaceId, UserId]),
            lists:sort(ExpectedPrivileges) =:= lists:sort(ActualPrivileges)
        end,

        prepare_consume_request = fun(Auth = #auth{subject = ?SUB(_, UserId)}, Token) ->
            Data = #{<<"token">> => ensure_token_serialized(Token)},
            #consume_request{
                logic_call_args = {user_logic, join_space, [Auth, UserId, Data]},
                rest_call_args = {<<"/user/spaces/join">>, Data},
                graph_sync_args = {?GRI(od_space, undefined, join, private), ?AS_USER(UserId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            oz_rpc(space_logic, delete, [?ROOT, SpaceId])
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            ?ERROR_RELATION_ALREADY_EXISTS(od_user, UserId, od_space, SpaceId)
        end
    })).


group_join_space_token(Config) ->
    {ok, SpaceCreatorUserId} = oz_test_utils:create_user(Config),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(SpaceCreatorUserId)),

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
            {ok, GroupId} = oz_test_utils:create_group(get_test_config(), ?USER(UserId)),
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
            {ok, ActualPrivileges} = oz_rpc(space_logic, get_group_privileges, [?ROOT, SpaceId, GroupId]),
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
                    {ok, GrId} = oz_test_utils:create_group(Config, ?ROOT),
                    GrId
            end,
            Data = #{<<"token">> => ensure_token_serialized(Token)},
            #consume_request{
                logic_call_args = {group_logic, join_space, [Auth, GroupId, Data]},
                rest_call_args = {[<<"/groups/">>, GroupId, <<"/spaces/join">>], Data},
                graph_sync_args = {?GRI(od_space, undefined, join, private), ?AS_GROUP(GroupId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            oz_rpc(space_logic, delete, [?ROOT, SpaceId])
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            {ok, GroupId} = simple_cache:get({user_group, UserId}),
            ?ERROR_RELATION_ALREADY_EXISTS(od_group, GroupId, od_space, SpaceId)
        end
    })).


support_space_token(Config) ->
    {ok, SpaceCreatorUserId} = oz_test_utils:create_user(Config),
    {ok, SpaceId} = oz_test_utils:create_space(Config, ?USER(SpaceCreatorUserId)),

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
                    {ok, {AnotherProvider, _}} = oz_test_utils:create_provider(Config),
                    AnotherProvider
            end,
            % Use a storage with the same id as provider for easier test code
            StorageId = ProviderId,
            % Ignore errors if already exists
            oz_rpc(storage_logic, create, [?PROVIDER(ProviderId), StorageId, ?STORAGE_NAME1]),
            Data = #{
                <<"token">> => ensure_token_serialized(Token),
                <<"size">> => oz_test_utils:minimum_support_size(Config)
            },
            #consume_request{
                logic_call_args = {storage_logic, support_space, [Auth, StorageId, Data]},
                rest_call_args = not_available,
                graph_sync_args = {?GRI(od_storage, StorageId, support, private), undefined, Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            oz_rpc(space_logic, delete, [?ROOT, SpaceId])
        end,
        expected_reuse_result_fun = fun(?SUB(?ONEPROVIDER, ProviderId)) ->
            StorageId = ProviderId,
            ?ERROR_RELATION_ALREADY_EXISTS(od_space, SpaceId, od_storage, StorageId)
        end
    })).


register_oneprovider_token(Config) ->
    Testcase = fun(AdminUserId, Policy) -> #testcase{
        token_type = ?INVITE_TOKEN(?REGISTER_ONEPROVIDER, AdminUserId),

        eligible_to_invite = [?SUB(user, AdminUserId)],
        requires_privileges_to_invite = (Policy =:= restricted),
        admin_privilege_to_invite = ?OZ_PROVIDERS_INVITE,

        supports_carried_privileges = false,
        requires_privileges_to_set_privileges = false,
        allowed_carried_privileges = undefined,
        default_carried_privileges = undefined,
        admin_privilege_to_set_privileges = undefined,

        eligible_consumer_types = [user, ?ONEPROVIDER, nobody],
        requires_privileges_to_consume = false,
        admin_privilege_to_consume = undefined,

        modify_privileges_fun = case Policy of
            open -> undefined;
            restricted -> fun
                (_, {grant, to_invite}) ->
                    oz_test_utils:set_env(Config, provider_registration_policy, open);
                (_, {revoke, to_invite}) ->
                    oz_test_utils:set_env(Config, provider_registration_policy, restricted)
            end
        end,
        check_privileges_fun = undefined,

        prepare_consume_request = fun(Auth, Token) ->
            Data = #{
                <<"token">> => ensure_token_serialized(Token),
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
            oz_rpc(user_logic, delete, [?ROOT, AdminUserId])
        end,
        expected_reuse_result_fun = fun(_) ->
            ok
        end
    } end,

    oz_test_utils:set_env(Config, provider_registration_policy, open),
    {ok, UserAlpha} = oz_test_utils:create_user(Config),
    ?assert(run_invite_token_tests(Testcase(UserAlpha, open))),

    oz_test_utils:set_env(Config, provider_registration_policy, restricted),
    {ok, UserBeta} = oz_test_utils:create_user(Config),
    ?assert(run_invite_token_tests(Testcase(UserBeta, restricted))).


user_join_cluster_token(Config) ->
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
            {ok, ActualPrivileges} = oz_rpc(cluster_logic, get_user_privileges, [?ROOT, ClusterId, UserId]),
            lists:sort(ExpectedPrivileges) =:= lists:sort(ActualPrivileges)
        end,

        prepare_consume_request = fun(Auth = #auth{subject = ?SUB(_, UserId)}, Token) ->
            Data = #{<<"token">> => ensure_token_serialized(Token)},
            #consume_request{
                logic_call_args = {user_logic, join_cluster, [Auth, UserId, Data]},
                rest_call_args = {<<"/user/clusters/join">>, Data},
                graph_sync_args = {?GRI(od_cluster, undefined, join, private), ?AS_USER(UserId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            ProviderId = ClusterId,
            oz_rpc(provider_logic, delete, [?ROOT, ProviderId])
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            ?ERROR_RELATION_ALREADY_EXISTS(od_user, UserId, od_cluster, ClusterId)
        end
    } end,

    {ok, ProviderCreatorUserId} = oz_test_utils:create_user(Config),
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, ProviderCreatorUserId, ?UNIQUE_STRING),
    ClusterId = ProviderId,
    UserEligibleToInvite = ?SUB(user, ProviderCreatorUserId),
    ?assert(run_invite_token_tests(Testcase(ClusterId, UserEligibleToInvite))),

    {ok, {AnotherProviderId, _}} = oz_test_utils:create_provider(Config),
    AnotherClusterId = AnotherProviderId,
    ProviderEligibleToInvite = ?SUB(?ONEPROVIDER, AnotherProviderId),
    ?assert(run_invite_token_tests(Testcase(AnotherClusterId, ProviderEligibleToInvite))).


group_join_cluster_token(Config) ->
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
            {ok, GroupId} = oz_test_utils:create_group(get_test_config(), ?USER(UserId)),
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
            {ok, ActualPrivileges} = oz_rpc(cluster_logic, get_group_privileges, [?ROOT, ClusterId, GroupId]),
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
                    {ok, GrId} = oz_test_utils:create_group(Config, ?ROOT),
                    GrId
            end,
            Data = #{<<"token">> => ensure_token_serialized(Token)},
            #consume_request{
                logic_call_args = {group_logic, join_cluster, [Auth, GroupId, Data]},
                rest_call_args = {[<<"/groups/">>, GroupId, <<"/clusters/join">>], Data},
                graph_sync_args = {?GRI(od_cluster, undefined, join, private), ?AS_GROUP(GroupId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            ProviderId = ClusterId,
            oz_rpc(provider_logic, delete, [?ROOT, ProviderId])
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            {ok, GroupId} = simple_cache:get({user_group, UserId}),
            ?ERROR_RELATION_ALREADY_EXISTS(od_group, GroupId, od_cluster, ClusterId)
        end
    } end,

    {ok, ProviderCreatorUserId} = oz_test_utils:create_user(Config),
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, ProviderCreatorUserId, ?UNIQUE_STRING),
    ClusterId = ProviderId,
    UserEligibleToInvite = ?SUB(user, ProviderCreatorUserId),
    ?assert(run_invite_token_tests(Testcase(ClusterId, UserEligibleToInvite))),

    {ok, {AnotherProviderId, _}} = oz_test_utils:create_provider(Config),
    AnotherClusterId = AnotherProviderId,
    ProviderEligibleToInvite = ?SUB(?ONEPROVIDER, AnotherProviderId),
    ?assert(run_invite_token_tests(Testcase(AnotherClusterId, ProviderEligibleToInvite))).


user_join_harvester_token(Config) ->
    HarvesterCreatorUserId = create_admin([?OZ_HARVESTERS_CREATE]),
    {ok, HarvesterId} = oz_test_utils:create_harvester(Config, ?USER(HarvesterCreatorUserId)),

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
            {ok, ActualPrivileges} = oz_rpc(harvester_logic, get_user_privileges, [?ROOT, HarvesterId, UserId]),
            lists:sort(ExpectedPrivileges) =:= lists:sort(ActualPrivileges)
        end,

        prepare_consume_request = fun(Auth = #auth{subject = ?SUB(_, UserId)}, Token) ->
            Data = #{<<"token">> => ensure_token_serialized(Token)},
            #consume_request{
                logic_call_args = {user_logic, join_harvester, [Auth, UserId, Data]},
                rest_call_args = {<<"/user/harvesters/join">>, Data},
                graph_sync_args = {?GRI(od_harvester, undefined, join, private), ?AS_USER(UserId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            oz_rpc(harvester_logic, delete, [?ROOT, HarvesterId])
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            ?ERROR_RELATION_ALREADY_EXISTS(od_user, UserId, od_harvester, HarvesterId)
        end
    })).


group_join_harvester_token(Config) ->
    HarvesterCreatorUserId = create_admin([?OZ_HARVESTERS_CREATE]),
    {ok, HarvesterId} = oz_test_utils:create_harvester(Config, ?USER(HarvesterCreatorUserId)),

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
            {ok, GroupId} = oz_test_utils:create_group(get_test_config(), ?USER(UserId)),
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
            {ok, ActualPrivileges} = oz_rpc(harvester_logic, get_group_privileges, [?ROOT, HarvesterId, GroupId]),
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
                    {ok, GrId} = oz_test_utils:create_group(Config, ?ROOT),
                    GrId
            end,
            Data = #{<<"token">> => ensure_token_serialized(Token)},
            #consume_request{
                logic_call_args = {group_logic, join_harvester, [Auth, GroupId, Data]},
                rest_call_args = {[<<"/groups/">>, GroupId, <<"/harvesters/join">>], Data},
                graph_sync_args = {?GRI(od_harvester, undefined, join, private), ?AS_GROUP(GroupId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            oz_rpc(harvester_logic, delete, [?ROOT, HarvesterId])
        end,
        expected_reuse_result_fun = fun(?SUB(user, UserId)) ->
            {ok, GroupId} = simple_cache:get({user_group, UserId}),
            ?ERROR_RELATION_ALREADY_EXISTS(od_group, GroupId, od_harvester, HarvesterId)
        end
    })).


space_join_harvester_token(Config) ->
    HarvesterCreatorUserId = create_admin([?OZ_HARVESTERS_CREATE]),
    {ok, HarvesterId} = oz_test_utils:create_harvester(Config, ?USER(HarvesterCreatorUserId)),

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
            {ok, SpaceId} = oz_test_utils:create_space(get_test_config(), ?USER(UserId)),
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
                    {ok, SpId} = oz_test_utils:create_space(Config, ?ROOT),
                    SpId
            end,
            Data = #{<<"token">> => ensure_token_serialized(Token)},
            #consume_request{
                logic_call_args = {space_logic, join_harvester, [Auth, SpaceId, Data]},
                rest_call_args = {[<<"/spaces/">>, SpaceId, <<"/harvesters/join">>], Data},
                graph_sync_args = {?GRI(od_harvester, undefined, join, private), ?AS_SPACE(SpaceId), Data}
            }
        end,

        delete_target_entity_fun = fun() ->
            oz_rpc(harvester_logic, delete, [?ROOT, HarvesterId])
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
        ct:pal("Invite token tests failed due to ~p:~p~nStacktrace: ~p", [
            Type, Reason, erlang:get_stacktrace()
        ]),
        false
    end.


% Check if trying to consume a bad token returns proper errors
check_bad_token_scenarios(Tc = #testcase{token_type = ?INVITE_TOKEN(InviteTokenType, TargetEntityId)}) ->
    {ok, {DummyProvider, _}} = oz_test_utils:create_provider(get_test_config()),
    {ok, DummyUser} = oz_test_utils:create_user(get_test_config()),
    {ok, {SessId, _}} = oz_test_utils:log_in(get_test_config(), DummyUser),
    {ok, AccessToken} = oz_rpc(token_logic, create_provider_named_token, [
        ?PROVIDER(DummyProvider), DummyProvider, #{<<"name">> => ?UNIQUE_STRING}
    ]),
    {ok, {GuiAccessToken, _}} = oz_rpc(token_logic, create_gui_access_token, [
        ?USER(DummyUser), DummyUser, SessId, ?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)
    ]),
    BadToken = <<"not-a-token-definitely-I've-seen-one-I-would-know">>,
    ForgedToken = tokens:construct(#token{
        onezone_domain = oz_test_utils:oz_domain(get_test_config()),
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
    {ok, SomeUser} = oz_test_utils:create_user(get_test_config()),
    {ok, {SomeProvider, _}} = oz_test_utils:create_provider(get_test_config()),
    TokenManager = create_admin([?OZ_TOKENS_MANAGE]),

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
    TokenManager = create_admin([?OZ_TOKENS_MANAGE]),
    AdminOfTargetEntity = create_admin([Tc#testcase.admin_privilege_to_invite]),

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
                        {ok, Token} = create_invite_token(
                            Persistence, Auth, EligibleSubject, #{<<"type">> => TokenType}
                        ),
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
            Auth = #auth{subject = EligibleSubject},
            ensure_privileges_to_invite(Tc, EligibleSubject),
            {ok, Token} = create_invite_token(Persistence, Auth, EligibleSubject, #{<<"type">> => TokenType}),
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
            Auth = #auth{subject = EligibleSubject},
            ensure_privileges_to_invite(Tc, EligibleSubject),
            lists:foreach(fun(EligibleConsumerType) ->
                {ok, Token} = create_invite_token(Persistence, Auth, EligibleSubject, #{<<"type">> => TokenType}),
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
                {ok, Token} = create_invite_token(Persistence, Auth, EligibleSubject, #{<<"type">> => TokenType}),
                Consumer = create_consumer_by_type(EligibleConsumerType),
                ModifyPrivsFun(Consumer, {revoke, to_consume}),
                set_admin_privileges(Consumer, revoke, [Tc#testcase.admin_privilege_to_consume]),
                ?assertMatch(?ERROR_FORBIDDEN, consume_token(Tc, Consumer, Token)),
                set_admin_privileges(Consumer, grant, [Tc#testcase.admin_privilege_to_consume]),
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
        Auth = #auth{subject = EligibleSubject},
        Tc#testcase.requires_privileges_to_set_privileges andalso ModifyPrivsFun(EligibleSubject, {grant, to_set_privs}),
        lists:foreach(fun(EligibleConsumerType) ->
            {ok, Token} = create_invite_token(temporary, Auth, EligibleSubject, #{
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
    AdminWithoutSetPrivs = create_admin([Tc#testcase.admin_privilege_to_invite]),
    AdminWithSetPrivs = create_admin([Tc#testcase.admin_privilege_to_invite, Tc#testcase.admin_privilege_to_set_privileges]),

    AllowedPrivileges = Tc#testcase.allowed_carried_privileges,
    CustomPrivileges = utils:random_sublist(AllowedPrivileges),
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
            {ok, Token} = create_invite_token(named, Auth, EligibleSubject, #{
                <<"type">> => TokenType, <<"privileges">> => CustomPrivileges
            }),
            Consumer = create_consumer_with_privs_to_consume(Tc, EligibleConsumerType),
            % Token becomes invalid when the subject loses privileges to set privileges
            ModifyPrivsFun(EligibleSubject, {revoke, to_set_privs}),
            ?assertMatch(?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED, consume_token(Tc, Consumer, Token)),
            % But it becomes valid again if the privileges are restored
            ModifyPrivsFun(EligibleSubject, {grant, to_invite}),
            ModifyPrivsFun(EligibleSubject, {grant, to_set_privs}),
            ?assertMatch({ok, _}, consume_token(Tc, Consumer, Token)),
            ?assert(CheckPrivilegesFun(Consumer, CustomPrivileges))
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
        Auth = #auth{subject = EligibleSubject},
        ensure_privileges_to_invite(Tc, EligibleSubject),
        {ok, Token} = create_invite_token(temporary, Auth, EligibleSubject, #{
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
        Auth = #auth{subject = EligibleSubject},
        ensure_privileges_to_invite(Tc, EligibleSubject),

        ?ERROR_BAD_VALUE_TOO_LOW(<<"usageLimit">>, 1) = create_invite_token(
            named, Auth, EligibleSubject, #{<<"type">> => TokenType, <<"usageLimit">> => 0}
        ),

        {ok, SingleUseToken} = create_invite_token(named, Auth, EligibleSubject, #{
            <<"type">> => TokenType, <<"usageLimit">> => 1
        }),
        ConsumerAlpha = create_consumer_with_privs_to_consume(Tc, random_eligible),
        ConsumerBeta = create_consumer_with_privs_to_consume(Tc, random_eligible),
        ?assertMatch({ok, _}, consume_token(Tc, ConsumerAlpha, SingleUseToken)),
        ?assertMatch(?ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED, consume_token(Tc, ConsumerBeta, SingleUseToken)),

        UsageLimit = 17,
        {ok, MultiUseToken} = create_invite_token(named, Auth, EligibleSubject, #{
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
    CustomPrivileges = utils:random_sublist(AllowedPrivileges),
    ModifyPrivsFun = Tc#testcase.modify_privileges_fun,
    CheckPrivilegesFun = Tc#testcase.check_privileges_fun,

    lists:foreach(fun(EligibleSubject) ->
        Auth = #auth{subject = EligibleSubject},
        Tc#testcase.requires_privileges_to_invite andalso ModifyPrivsFun(EligibleSubject, {grant, to_invite}),
        Tc#testcase.requires_privileges_to_set_privileges andalso ModifyPrivsFun(EligibleSubject, {grant, to_set_privs}),

        {ok, Token} = create_invite_token(named, Auth, EligibleSubject, #{
            <<"type">> => TokenType, <<"usageLimit">> => 3, <<"privileges">> => CustomPrivileges
        }),
        ConsumerAlpha = create_consumer_by_type(utils:random_element(Tc#testcase.eligible_consumer_types)),
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
        ConsumerGamma = create_consumer_by_type(utils:random_element(Tc#testcase.eligible_consumer_types)),
        case Tc#testcase.requires_privileges_to_consume of
            false ->
                ?assertMatch({ok, _}, consume_token(Tc, ConsumerGamma, Token));
            true ->
                ModifyPrivsFun(ConsumerGamma, {revoke, to_consume}),
                set_admin_privileges(ConsumerGamma, revoke, [Tc#testcase.admin_privilege_to_consume]),
                ?assertMatch(?ERROR_FORBIDDEN, consume_token(Tc, ConsumerGamma, Token)),
                set_admin_privileges(ConsumerGamma, grant, [Tc#testcase.admin_privilege_to_consume]),
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
        Auth = #auth{subject = EligibleSubject},
        ensure_privileges_to_invite(Tc, EligibleSubject),
        {ok, TokenAlpha} = create_invite_token(temporary, Auth, EligibleSubject, #{<<"type">> => TokenType}),
        {ok, TokenBeta} = create_invite_token(temporary, Auth, EligibleSubject, #{<<"type">> => TokenType}),
        {ok, TokenGamma} = create_invite_token(temporary, Auth, EligibleSubject, #{<<"type">> => TokenType}),
        lists:foreach(fun(_) ->
            lists:foreach(fun(Token) ->
                Consumer = create_consumer_with_privs_to_consume(Tc, random_eligible),
                ?assertMatch({ok, _}, consume_token(Tc, Consumer, Token))
            end, [TokenAlpha, TokenBeta, TokenGamma])
        end, lists:seq(1, 9)),
        case EligibleSubject of
            ?SUB(user, UserId) ->
                oz_rpc(token_logic, revoke_all_user_temporary_tokens, [?USER(UserId), UserId]);
            ?SUB(?ONEPROVIDER, PrId) ->
                oz_rpc(token_logic, revoke_all_provider_temporary_tokens, [?PROVIDER(PrId), PrId])
        end,
        lists:foreach(fun(Token) ->
            Consumer = create_consumer_with_privs_to_consume(Tc, random_eligible),
            ?assertMatch(?ERROR_TOKEN_INVALID, consume_token(Tc, Consumer, Token))
        end, [TokenAlpha, TokenBeta, TokenGamma])
    end, Tc#testcase.eligible_to_invite).


% Named tokens are revocable individually. The revocation can be undone at will.
check_named_token_revocation(Tc = #testcase{token_type = TokenType}) ->
    lists:foreach(fun(EligibleSubject) ->
        Auth = #auth{subject = EligibleSubject},
        ensure_privileges_to_invite(Tc, EligibleSubject),
        {ok, #token{id = TokenId} = Token} = create_invite_token(named, Auth, EligibleSubject, #{
            <<"type">> => TokenType
        }),
        Consumer = create_consumer_with_privs_to_consume(Tc, random_eligible),
        oz_rpc(token_logic, update_named_token, [Auth, TokenId, #{<<"revoked">> => true}]),
        ?assertMatch(?ERROR_TOKEN_REVOKED, consume_token(Tc, Consumer, Token)),
        oz_rpc(token_logic, update_named_token, [Auth, TokenId, #{<<"revoked">> => false}]),
        ?assertMatch({ok, _}, consume_token(Tc, Consumer, Token)),
        oz_rpc(token_logic, update_named_token, [Auth, TokenId, #{<<"revoked">> => true}]),
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
        Auth = #auth{subject = EligibleSubject},
        ensure_privileges_to_invite(Tc, EligibleSubject),
        lists:foreach(fun(EligibleConsumerType) ->
            % Check two tokens to the same target entity
            lists:foreach(fun(Persistence) ->
                {ok, TokenAlpha} = create_invite_token(Persistence, Auth, EligibleSubject, #{<<"type">> => TokenType}),
                {ok, TokenBeta} = create_invite_token(Persistence, Auth, EligibleSubject, #{<<"type">> => TokenType}),
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
            {ok, MultiUseToken} = create_invite_token(named, Auth, EligibleSubject, #{
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
    Admin = create_admin(privileges:oz_admin()),
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            EligibleAuth = #auth{subject = EligibleSubject},
            ensure_privileges_to_invite(Tc, EligibleSubject),
            lists:foreach(fun(Auth) ->
                BadTokenType = ?INVITE_TOKEN(InviteTokenType, <<"1234">>),
                Error = ?assertMatch(
                    {error, _},
                    create_invite_token(Persistence, Auth, EligibleSubject, #{<<"type">> => BadTokenType})
                ),
                ?assert(lists:member(Error, [?ERROR_FORBIDDEN, ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"1234">>)]))
            end, [EligibleAuth, ?USER(Admin)])
        end, Tc#testcase.eligible_to_invite)
    end, [named, temporary]).


% Check if adding caveats to invite tokens works as expected.
check_token_caveats_handling(Tc = #testcase{token_type = TokenType}) ->
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            Auth = #auth{subject = EligibleSubject, peer_ip = ?PEER_IP},
            ensure_privileges_to_invite(Tc, EligibleSubject),
            lists:foreach(fun(_) ->
                Consumer = create_consumer_with_privs_to_consume(Tc, random_eligible),
                RandCorrectCaveats = utils:random_sublist(gen_correct_caveats(Consumer)),
                RandUnverifiedCaveats = utils:random_sublist(gen_unverified_caveats(Consumer)),
                {ok, Token} = create_invite_token(Persistence, Auth, EligibleSubject, #{
                    <<"type">> => TokenType,
                    <<"caveats">> => RandCorrectCaveats ++ RandUnverifiedCaveats
                }),
                case RandUnverifiedCaveats of
                    [] ->
                        ?assertMatch({ok, _}, consume_token(Tc, Consumer, Token));
                    _ ->
                        ?assertMatch(?ERROR_TOKEN_CAVEAT_UNVERIFIED(_), consume_token(Tc, Consumer, Token)),
                        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(UnverifiedCaveat) = consume_token(Tc, Consumer, Token),
                        ?assert(lists:member(UnverifiedCaveat, RandUnverifiedCaveats))
                end
            end, lists:seq(1, 20))
        end, Tc#testcase.eligible_to_invite)
    end, [named, temporary]).


gen_correct_caveats(Consumer) -> lists:flatten([
    #cv_time{valid_until = oz_test_utils:cluster_time_seconds(get_test_config()) + 10},
    #cv_ip{whitelist = [?CORRECT_MASK_1]},
    #cv_ip{whitelist = [?CORRECT_MASK_2]},
    #cv_asn{whitelist = [?CLIENT_ASN]},
    #cv_country{type = whitelist, list = [?CLIENT_COUNTRY]},
    #cv_country{type = blacklist, list = [?INCORRECT_COUNTRY]},
    #cv_region{type = whitelist, list = ?CLIENT_REGIONS},
    #cv_region{type = blacklist, list = ?INCORRECT_REGIONS},
    case Consumer of
        ?SUB(user, UserId) ->
            {ok, Group} = oz_test_utils:create_group(get_test_config(), ?USER(UserId)),
            oz_test_utils:ensure_entity_graph_is_up_to_date(get_test_config()),
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
    #cv_time{valid_until = oz_test_utils:cluster_time_seconds(get_test_config()) - 1},
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
            Auth = #auth{subject = EligibleSubject},
            ensure_privileges_to_invite(Tc, EligibleSubject),
            {ok, Token} = create_invite_token(Persistence, Auth, EligibleSubject, #{<<"type">> => TokenType}),
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
        delete_subject(Subject),
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
    oz_rpc(LogicModule, add_user, [?ROOT, EntityId, UserId]),
    oz_rpc(LogicModule, update_user_privileges, [?ROOT, EntityId, UserId, Privileges, []]);
set_user_privs(LogicModule, EntityId, UserId, revoke, Privileges) ->
    case rand:uniform(2) of
        1 -> oz_rpc(LogicModule, remove_user, [?ROOT, EntityId, UserId]);
        2 -> oz_rpc(LogicModule, update_user_privileges, [?ROOT, EntityId, UserId, [], Privileges])
    end.


assert_creation_succeeds(Auth, Subject, Data) ->
    assert_creation_succeeds(named, Auth, Subject, Data),
    assert_creation_succeeds(temporary, Auth, Subject, Data).
assert_creation_succeeds(Persistence, Auth, Subject, Data) ->
    ?assertMatch({ok, _}, create_invite_token(Persistence, Auth, Subject, Data)).


assert_creation_fails(Auth, Subject, Data, ExpError) ->
    assert_creation_fails(named, Auth, Subject, Data, ExpError),
    assert_creation_fails(temporary, Auth, Subject, Data, ExpError).
assert_creation_fails(Persistence, Auth, Subject, Data, {error, _} = ExpError) ->
    ?assertEqual(ExpError, create_invite_token(Persistence, Auth, Subject, Data)).


create_invite_token(named, Auth, ?SUB(user, UserId), Data) ->
    oz_rpc(token_logic, create_user_named_token, [
        Auth, UserId, Data#{<<"name">> => ?UNIQUE_STRING}
    ]);
create_invite_token(temporary, Auth, ?SUB(user, UserId), Data) ->
    Caveats = maps:get(<<"caveats">>, Data, []),
    oz_rpc(token_logic, create_user_temporary_token, [
        Auth, UserId, Data#{<<"caveats">> => ensure_time_caveat(Caveats)}
    ]);
create_invite_token(named, Auth, ?SUB(?ONEPROVIDER, ProviderId), Data) ->
    oz_rpc(token_logic, create_provider_named_token, [
        Auth, ProviderId, Data#{<<"name">> => ?UNIQUE_STRING}
    ]);
create_invite_token(temporary, Auth, ?SUB(?ONEPROVIDER, ProviderId), Data) ->
    Caveats = maps:get(<<"caveats">>, Data, []),
    oz_rpc(token_logic, create_provider_temporary_token, [
        Auth, ProviderId, Data#{<<"caveats">> => ensure_time_caveat(Caveats)}
    ]).


% Time caveat is required in temporary tokens - add one if there isn't any
ensure_time_caveat(Caveats) ->
    Now = oz_test_utils:cluster_time_seconds(get_test_config()),
    case caveats:find(cv_time, Caveats) of
        false -> [#cv_time{valid_until = Now + 3600} | Caveats];
        {true, _} -> Caveats
    end.


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
    {ok, User} = oz_test_utils:create_user(get_test_config()),
    ?SUB(user, User);
create_consumer_by_type(?ONEPROVIDER) ->
    {ok, {Provider, _}} = oz_test_utils:create_provider(get_test_config()),
    ?SUB(?ONEPROVIDER, Provider).


create_consumer_with_privs_to_consume(Tc, random_eligible) ->
    ConsumerType = utils:random_element(Tc#testcase.eligible_consumer_types),
    create_consumer_with_privs_to_consume(Tc, ConsumerType);
create_consumer_with_privs_to_consume(Tc, ConsumerType) ->
    Consumer = create_consumer_by_type(ConsumerType),
    ModifyPrivsFun = Tc#testcase.modify_privileges_fun,
    Tc#testcase.requires_privileges_to_consume andalso ModifyPrivsFun(Consumer, {grant, to_consume}),
    Consumer.


create_admin(Privileges) ->
    {ok, Admin} = oz_test_utils:create_admin(get_test_config(), Privileges),
    Admin.


set_admin_privileges(?SUB(user, UserId), grant, Privileges) ->
    oz_test_utils:user_set_oz_privileges(get_test_config(), UserId, Privileges, []);
set_admin_privileges(?SUB(user, UserId), revoke, Privileges) ->
    oz_test_utils:user_set_oz_privileges(get_test_config(), UserId, [], Privileges).


delete_subject(?SUB(user, UserId)) ->
    oz_rpc(user_logic, delete, [?ROOT, UserId]);
delete_subject(?SUB(?ONEPROVIDER, ProviderId)) ->
    oz_rpc(provider_logic, delete, [?ROOT, ProviderId]).


all_consumer_types() -> [
    user, ?ONEPROVIDER, nobody
].


oz_rpc(Module, Function, Args) ->
    oz_test_utils:call_oz(get_test_config(), Module, Function, Args).


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
    case utils:random_element(AvailableInterfaces) of
        logic -> consume_by_logic_call(ConsumeRequest);
        graphsync -> consume_by_graphsync_request(Auth, ConsumeRequest);
        rest -> consume_by_rest_call(Auth, ConsumeRequest)
    end.


consume_by_logic_call(#consume_request{logic_call_args = {Module, Function, Args}}) ->
    oz_rpc(Module, Function, Args).


consume_by_rest_call(Auth, #consume_request{rest_call_args = {PathOrTokens, Data}}) ->
    Config = get_test_config(),
    Url = oz_test_utils:oz_rest_url(Config, PathOrTokens),
    AuthHeader = case create_access_token(Auth) of
        undefined -> #{};
        Token -> #{?HDR_X_AUTH_TOKEN => Token}
    end,
    Headers = AuthHeader#{?HDR_CONTENT_TYPE => <<"application/json">>},
    Opts = [
        {ssl_options, ssl_opts()},
        {connect_timeout, timer:seconds(30)},
        {recv_timeout, timer:seconds(30)}
    ],
    case http_client:request(post, Url, Headers, json_utils:encode(Data), Opts) of
        {ok, OkCode, _, Body} when OkCode >= 200 andalso OkCode < 300 ->
            {ok, json_utils:decode(Body)};
        {ok, _, _, ErrorBody} ->
            #{<<"error">> := ErrorJson} = json_utils:decode(ErrorBody),
            errors:from_json(ErrorJson);
        Other ->
            ct:pal("REST call failed unexpectedly with: ~tp", [Other]),
            error(rest_call_failed)
    end.


consume_by_graphsync_request(Auth, #consume_request{graph_sync_args = {GRI, AuthHint, Data}}) ->
    Type = case Auth of
        ?PROVIDER(_) -> provider;
        _ -> gui
    end,
    Url = oz_test_utils:graph_sync_url(get_test_config(), Type),
    % EntityId in the AuthHint can be undefined due to testing different
    % combinations of subjects - make sure it is serializable as GS expects a
    % binary entity type.
    SerializableAuthHint = case AuthHint of
        undefined -> undefined;
        {HintType, undefined} -> undefined;
        {HintType, EntityId} -> {HintType, EntityId}
    end,
    GsAuth = case create_access_token(Auth) of
        undefined -> undefined;
        Token -> {token, Token}
    end,
    {ok, GsClient, _} = gs_client:start_link(
        Url, GsAuth, ?SUPPORTED_PROTO_VERSIONS, fun(_) -> ok end, ssl_opts()
    ),
    Result = gs_client:graph_request(GsClient, GRI, create, Data, false, SerializableAuthHint),
    gs_client:kill(GsClient),
    Result.


create_access_token(?USER(UserId) = Auth) ->
    {ok, UserToken} = oz_rpc(token_logic, create_user_temporary_token, [
        Auth, UserId, #{<<"caveats">> => ensure_time_caveat([])}
    ]),
    {ok, Serialized} = tokens:serialize(UserToken),
    Serialized;
create_access_token(?PROVIDER(ProviderId) = Auth) ->
    {ok, ProviderToken} = oz_rpc(token_logic, create_provider_temporary_token, [
        Auth, ProviderId, #{<<"caveats">> => ensure_time_caveat([])}
    ]),
    {ok, Serialized} = tokens:serialize(ProviderToken),
    Serialized;
create_access_token(?NOBODY) ->
    undefined.


ssl_opts() ->
    [{secure, only_verify_peercert}, {cacerts, oz_test_utils:gui_ca_certs(get_test_config())}].


ensure_token_serialized(Serialized) when is_binary(Serialized) ->
    Serialized;
ensure_token_serialized(Token) ->
    {ok, Serialized} = tokens:serialize(Token),
    Serialized.