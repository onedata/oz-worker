%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @author Lukasz Opiola
%%% @copyright (C) 2014-2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Common functions for ct tests.
%%% @end
%%%-------------------------------------------------------------------
-module(oz_test_utils).

-include("entity_logic.hrl").
-include("graph_sync/oz_graph_sync.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("registered_names.hrl").
-include("api_test_utils.hrl").
-include("auth/auth_common.hrl").
-include("ozt.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").

-define(OZ_NODES(Config), ?config(oz_worker_nodes, Config)).
-define(TIME_MOCK_STARTING_TIMESTAMP, 1500000000).


%% API
-export([
    call_oz/4,
    get_env/2, get_env/3,
    set_env/3,
    set_app_env/4,
    oz_domain/1,
    oz_url/2, oz_url/3,
    oz_rest_url/2,
    all_oz_privileges/1
]).
% Operations corresponding to logic modules
-export([
    list_users/1,
    create_user/1, create_user/2,
    get_user/2,
    delete_user/2,

    create_client_token/2,
    list_client_tokens/2,

    user_get_oz_privileges/2,
    user_get_eff_oz_privileges/2,
    user_get_groups/2,
    user_get_spaces/2,
    user_get_eff_groups/2,
    user_get_harvesters/2,
    user_get_clusters/2,
    user_set_oz_privileges/4,
    user_set_space_alias/4,
    user_get_space_alias/3,
    user_unset_space_alias/3,

    user_leave_space/3,
    user_leave_harvester/3,
    user_leave_cluster/3,

    create_provider_registration_token/3,
    create_temporary_provider_registration_token/3
]).
-export([
    list_groups/1,
    create_group/2, create_group/3,
    create_parent_group/4,
    get_group/2,
    update_group/3,
    delete_group/2,
    mark_group_protected/3,

    group_get_children/2,
    group_get_parents/2,
    group_get_users/2,
    group_get_spaces/2,
    group_get_harvesters/2,
    group_get_clusters/2,
    group_get_oz_privileges/2,
    group_get_eff_oz_privileges/2,
    group_set_oz_privileges/4,

    group_add_user/3, group_add_user/4,
    group_set_user_privileges/5, group_set_user_privileges/6,
    group_add_group/3, group_add_group/4,
    group_remove_user/3,
    group_remove_group/3,
    group_leave_space/3,
    group_leave_handle_service/3,
    group_invite_group_token/3,
    group_invite_user_token/3,

    group_get_eff_users/2,
    group_get_user_privileges/3,
    group_get_eff_user_privileges/3,

    group_get_group_privileges/3,
    group_set_group_privileges/5,

    group_create_space/3,
    group_create_harvester/3
]).
-export([
    create_space/2, create_space/3,
    get_space/2,
    list_spaces/1,
    update_space/3,
    delete_space/2,

    space_get_users/2,
    space_get_groups/2,
    space_get_providers/2,
    space_get_harvesters/2,
    space_get_storages/2,

    space_remove_storage/3,
    space_remove_provider/3,
    space_remove_harvester/3,

    space_add_owner/3, space_remove_owner/3,
    create_space_owner/2,
    space_add_user/3,
    space_remove_user/3,
    space_add_group/3,
    space_get_user_privileges/3,
    space_set_user_privileges/5,
    space_get_group_privileges/3,
    space_set_group_privileges/5,
    space_invite_user_token/3,
    space_invite_group_token/3,
    create_space_support_token/3,
    space_has_effective_user/3,

    space_remove_group/3,
    space_harvest_metadata/7
]).
-export([
    create_share/6,
    create_share/3,
    list_shares/1,
    get_share/2,
    delete_share/2,
    get_share_public_url/2
]).
-export([
    create_provider/1, create_provider/2, create_provider/3,
    get_provider/2,
    list_providers/1,
    delete_provider/2,
    support_space_by_provider/3,
    support_space/4, support_space/5, support_space_using_token/5,
    support_space_by_legacy_storage/3,
    unsupport_space/3,
    enable_subdomain_delegation/4,
    set_provider_domain/3
]).
-export([
    list_handle_services/1,
    create_handle_service/5, create_handle_service/3,
    get_handle_service/2,
    delete_handle_service/2,

    handle_service_get_groups/2,
    handle_service_get_users/2,
    handle_service_get_user_privileges/3,
    handle_service_get_group_privileges/3,
    handle_service_add_user/3,
    handle_service_remove_user/3,
    handle_service_set_user_privileges/5,
    handle_service_add_group/3,
    handle_service_remove_group/3,
    handle_service_set_group_privileges/5
]).
-export([
    create_handle/6, create_handle/3,
    list_handles/1,
    get_handle/2,
    update_handle/3, update_handle/5,
    delete_handle/2,

    handle_get_groups/2,
    handle_get_users/2,

    handle_add_user/3,
    handle_remove_user/3,
    handle_set_user_privileges/5,
    handle_get_user_privileges/3,

    handle_add_group/3,
    handle_remove_group/3,
    handle_set_group_privileges/5,
    handle_get_group_privileges/3
]).
-export([
    create_harvester/2, create_harvester/3,
    get_harvester/2,
    list_harvesters/1,
    update_harvester/3,
    delete_harvester/2,

    harvester_get_users/2,
    harvester_get_groups/2,
    harvester_get_spaces/2,

    harvester_add_user/3,
    harvester_add_group/3,
    harvester_add_space/3,
    harvester_get_user_privileges/3,
    harvester_set_user_privileges/5,
    harvester_get_group_privileges/3,
    harvester_set_group_privileges/5,
    harvester_invite_user_token/3,
    harvester_invite_group_token/3,
    harvester_invite_space_token/3,
    harvester_has_effective_user/3,

    harvester_remove_user/3,
    harvester_remove_group/3,
    harvester_remove_space/3,

    harvester_create_index/3,
    harvester_get_index/3,
    harvester_get_index_stats/3,

    harvester_submit_entry/5,
    harvester_delete_entry/5,
    harvester_submit_batch/8
]).
-export([
    list_clusters/1,
    get_cluster/2,
    update_cluster/3,
    delete_cluster/2,

    cluster_get_groups/2,
    cluster_get_users/2,

    cluster_add_user/3,
    cluster_remove_user/3,
    cluster_set_user_privileges/5,
    cluster_get_user_privileges/3,
    cluster_invite_user_token/3,
    cluster_invite_group_token/3,

    cluster_add_group/3,
    cluster_remove_group/3,
    cluster_set_group_privileges/5,
    cluster_get_group_privileges/3
]).
-export([
    create_storage/3,
    create_storage/4,
    create_imported_storage/3,
    get_storage/2,
    update_storage/3,
    delete_storage/2
]).
-export([
    assert_invite_token_usage_limit_reached/3,
    authenticate_by_token/2, authenticate_by_token/3,
    acquire_temporary_token/2,
    create_legacy_access_token/2,
    confine_token_with_legacy_auth_none_caveat/1,
    request_gui_token/2,
    request_gui_token/4
]).
-export([
    delete_all_entities/1,
    delete_all_entities/2
]).
% Convenience functions
-export([
    create_3_nested_groups/2, create_3_nested_groups/5,
    create_and_support_3_spaces/2,
    minimum_support_size/1,
    mock_harvesting_backends/2,
    unmock_harvesting_backends/2,
    mock_handle_proxy/1,
    unmock_handle_proxy/1,
    timestamp_seconds/1,
    mock_time/1, unmock_time/1,
    get_mocked_time/1,
    simulate_time_passing/2,
    gui_ca_certs/1,
    ensure_entity_graph_is_up_to_date/1, ensure_entity_graph_is_up_to_date/2,
    toggle_basic_auth/2,
    read_auth_config/1,
    overwrite_config/3,
    overwrite_auth_config/2,
    overwrite_test_auth_config/2,
    overwrite_compatibility_registry/2,
    create_dummy_gui_package/0,
    deploy_dummy_gui/2,
    copy_file_to_onezone_nodes/2,
    copy_file_to_node/2
]).
-export([
    log_in/2, log_out/2, parse_resp_session_cookie/1,
    graph_sync_url/2,
    get_gs_supported_proto_versions/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Works like an rpc:call, but automatically retrieves oz_worker node to call
%% from config and wraps the call in a try-catch, so better error reporting
%% can be done.
%% @end
%%--------------------------------------------------------------------
-spec call_oz(Config :: term(), Module :: atom(), Function :: atom(),
    Args :: [term()]) -> term() | {error, term()}.
call_oz(Config, Module, Function, Args) ->
    FunWrapper = fun() ->
        try
            erlang:apply(Module, Function, Args)
        catch Type:Reason ->
            {crash, Type, Reason, lager:pr_stacktrace(erlang:get_stacktrace())}
        end
    end,
    Nodes = ?OZ_NODES(Config),
    Node = lists:nth(rand:uniform(length(Nodes)), Nodes),
    case rpc:call(Node, erlang, apply, [FunWrapper, []]) of
        {crash, Type, Reason, Stacktrace} ->
            % Log a bad rpc - very useful when debugging tests.
            ct:pal(
                "RPC call in oz_test_utils crashed!~n"
                "Module: ~p~n"
                "Function: ~p~n"
                "Args: ~p~n"
                "Error: ~p:~p~n"
                "Stacktrace: ~s",
                [Module, Function, Args, Type, Reason, Stacktrace]
            ),
            {error, {badrpc, Reason}};
        Result ->
            Result
    end.


%%--------------------------------------------------------------------
%% @doc
%% Creates a user in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_user(Config :: term()) -> {ok, od_user:id()}.
create_user(Config) ->
    ?assertMatch({ok, _}, call_oz(
        Config, user_logic, create, [?ROOT]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a user in onezone. full_name, username and password can be provided
%% in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create_user(Config :: term(), Data :: #{}) -> {ok, od_user:id()}.
create_user(Config, Data) ->
    ?assertMatch({ok, _}, call_oz(
        Config, user_logic, create, [?ROOT, Data]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a client token for given user.
%% @end
%%--------------------------------------------------------------------
-spec create_client_token(Config :: term(), UserId :: od_user:id()) ->
    {ok, Token :: binary()}.
create_client_token(Config, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, user_logic, create_client_token, [?USER(UserId), UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves client tokens from onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_client_tokens(Config :: term(), UserId :: od_user:id()) ->
    {ok, Tokens :: [binary()]}.
list_client_tokens(Config, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, user_logic, list_client_tokens, [?USER(UserId), UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves user data from onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_user(Config :: term(), UserId :: od_user:id()) -> {ok, #od_user{}}.
get_user(Config, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, user_logic, get, [?ROOT, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Returns OZ privileges of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_get_oz_privileges(Config :: term(), UserId :: od_user:id()) ->
    {ok, [privileges:oz_privilege()]}.
user_get_oz_privileges(Config, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, user_logic, get_oz_privileges, [
        ?ROOT, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns effective OZ privileges of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_get_eff_oz_privileges(Config :: term(), UserId :: od_user:id()) ->
    {ok, [privileges:oz_privilege()]}.
user_get_eff_oz_privileges(Config, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, user_logic, get_eff_oz_privileges, [
        ?ROOT, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns groups of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_get_groups(Config :: term(), UserId :: od_user:id()) ->
    {ok, [od_group:id()]}.
user_get_groups(Config, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, user_logic, get_groups, [
        ?ROOT, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns effective groups of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_get_eff_groups(Config :: term(), UserId :: od_user:id()) ->
    {ok, [od_group:id()]}.
user_get_eff_groups(Config, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, user_logic, get_eff_groups, [
        ?ROOT, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns spaces of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_get_spaces(Config :: term(), UserId :: od_user:id()) ->
    {ok, [od_space:id()]}.
user_get_spaces(Config, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, user_logic, get_spaces, [
        ?ROOT, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns harvesters of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_get_harvesters(Config :: term(), UserId :: od_user:id()) ->
    {ok, [od_harvester:id()]}.
user_get_harvesters(Config, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, user_logic, get_harvesters, [
        ?ROOT, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns clusters of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_get_clusters(Config :: term(), UserId :: od_user:id()) ->
    {ok, [od_cluster:id()]}.
user_get_clusters(Config, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, user_logic, get_clusters, [
        ?ROOT, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of all users in onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_users(Config :: term()) -> {ok, [od_user:id()]}.
list_users(Config) ->
    ?assertMatch({ok, _}, call_oz(
        Config, user_logic, list, [?ROOT]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Sets OZ privileges of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_set_oz_privileges(Config :: term(), UserId :: od_user:id(),
    PrivsToGrant :: [privileges:oz_privilege()],
    PrivsToRevoke :: [privileges:oz_privilege()]) -> ok.
user_set_oz_privileges(Config, UserId, PrivsToGrant, PrivsToRevoke) ->
    ?assertMatch(ok, call_oz(Config, user_logic, update_oz_privileges, [
        ?ROOT, UserId, PrivsToGrant, PrivsToRevoke
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets alias for a space of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_set_space_alias(Config :: term(), UserId :: od_user:id(),
    SpaceId :: od_space:id(), Alias :: binary()) -> ok.
user_set_space_alias(Config, UserId, SpaceId, Alias) ->
    ?assertMatch(ok, call_oz(Config, user_logic, set_space_alias, [
        ?ROOT, UserId, SpaceId, Alias
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves alias for a space of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_get_space_alias(Config :: term(), UserId :: od_user:id(),
    SpaceId :: od_space:id()) -> binary().
user_get_space_alias(Config, UserId, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, user_logic, get_space_alias, [?ROOT, UserId, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Sets alias for a space of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_unset_space_alias(Config :: term(), UserId :: od_user:id(),
    SpaceId :: od_space:id()) -> ok.
user_unset_space_alias(Config, UserId, SpaceId) ->
    ?assertMatch(ok, call_oz(Config, user_logic, delete_space_alias, [
        ?ROOT, UserId, SpaceId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given user from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_user(Config :: term(), UserId :: od_user:id()) -> ok.
delete_user(Config, UserId) ->
    ?assertMatch(ok, call_oz(
        Config, user_logic, delete, [?ROOT, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Leaves space as a user.
%% @end
%%--------------------------------------------------------------------
-spec user_leave_space(Config :: term(), UserId :: od_user:id(),
    SpaceId :: od_space:id()) -> ok.
user_leave_space(Config, UserId, SpaceId) ->
    ?assertMatch(ok, call_oz(
        Config, user_logic, leave_space, [?ROOT, UserId, SpaceId]
    )).

%%--------------------------------------------------------------------
%% @doc
%% Leaves harvester as a user.
%% @end
%%--------------------------------------------------------------------
-spec user_leave_harvester(Config :: term(), UserId :: od_user:id(),
    HarvesterId :: od_harvester:id()) -> ok.
user_leave_harvester(Config, UserId, HarvesterId) ->
    ?assertMatch(ok, call_oz(
        Config, user_logic, leave_harvester, [?ROOT, UserId, HarvesterId]
    )).

%%--------------------------------------------------------------------
%% @doc
%% Leaves cluster as a user.
%% @end
%%--------------------------------------------------------------------
-spec user_leave_cluster(Config :: term(), UserId :: od_user:id(),
    ClusterId :: od_cluster:id()) -> ok.
user_leave_cluster(Config, UserId, ClusterId) ->
    ?assertMatch(ok, call_oz(
        Config, user_logic, leave_cluster, [?ROOT, UserId, ClusterId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a NAMED provider registration token.
%% @end
%%--------------------------------------------------------------------
-spec create_provider_registration_token(Config :: term(),
    Client :: aai:auth(), od_user:id()) -> {ok, tokens:token()}.
create_provider_registration_token(Config, Client, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, user_logic, create_provider_registration_token, [Client, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a TEMPORARY provider registration token.
%% @end
%%--------------------------------------------------------------------
-spec create_temporary_provider_registration_token(Config :: term(),
    Client :: aai:auth(), od_user:id()) -> {ok, tokens:token()}.
create_temporary_provider_registration_token(Config, Client, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, token_logic, create_user_temporary_token, [Client, UserId, #{
            <<"type">> => ?INVITE_TOKEN(?REGISTER_ONEPROVIDER, UserId),
            <<"caveats">> => [#cv_time{valid_until = timestamp_seconds(Config) + 3600}]
        }]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates group in onezone with a random name.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Config :: term(), Client :: aai:auth()) ->
    {ok, Id :: binary()}.
create_group(Config, Client) ->
    create_group(Config, Client, ?UNIQUE_STRING).


%%--------------------------------------------------------------------
%% @doc
%% Creates group in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Config :: term(), Client :: aai:auth(),
    NameOrData :: od_group:name() | #{}) -> {ok, Id :: binary()}.
create_group(Config, Client, NameOrData) ->
    Result = case Client of
        ?USER(UserId) ->
            call_oz(Config, user_logic, create_group, [Client, UserId, NameOrData]);
        _ ->
            call_oz(Config, group_logic, create, [Client, NameOrData])
    end,
    ?assertMatch({ok, _}, Result).


%%--------------------------------------------------------------------
%% @doc
%% Creates a parent group for given group in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_parent_group(Config :: term(), Client :: aai:auth(),
    ChildGroupId :: od_group:id(), NameOrData :: od_group:name() | #{}) ->
    {ok, Id :: binary()}.
create_parent_group(Config, Client, ChildGroupId, NameOrData) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, create_parent_group, [Client, ChildGroupId, NameOrData]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves group data from onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_group(Config :: term(), GroupId :: od_group:id()) ->
    {ok, #od_group{}}.
get_group(Config, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, get, [?ROOT, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Updates group name.
%% @end
%%--------------------------------------------------------------------
-spec update_group(Config :: term(), GroupId :: od_group:id(),
    Name :: od_group:name()) -> ok.
update_group(Config, GroupId, Name) ->
    ?assertMatch(ok, call_oz(
        Config, group_logic, update, [?ROOT, GroupId, #{<<"name">> => Name}]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves child groups of given group from onezone.
%% @end
%%--------------------------------------------------------------------
-spec group_get_children(Config :: term(), GroupId :: od_group:id()) ->
    {ok, [od_group:id()]}.
group_get_children(Config, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, get_children, [?ROOT, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves parent groups of given group from onezone.
%% @end
%%--------------------------------------------------------------------
-spec group_get_parents(Config :: term(), GroupId :: od_group:id()) ->
    {ok, [od_group:id()]}.
group_get_parents(Config, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, get_parents, [?ROOT, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves users of given group from onezone.
%% @end
%%--------------------------------------------------------------------
-spec group_get_users(Config :: term(), GroupId :: od_group:id()) ->
    {ok, [od_user:id()]}.
group_get_users(Config, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, get_users, [?ROOT, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves spaces of given group from onezone.
%% @end
%%--------------------------------------------------------------------
-spec group_get_spaces(Config :: term(), GroupId :: od_group:id()) ->
    {ok, [od_space:id()]}.
group_get_spaces(Config, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, get_spaces, [?ROOT, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves clusters of given group from onezone.
%% @end
%%--------------------------------------------------------------------
-spec group_get_clusters(Config :: term(), GroupId :: od_group:id()) ->
    {ok, [od_cluster:id()]}.
group_get_clusters(Config, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, get_clusters, [?ROOT, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves harvesters of given group from onezone.
%% @end
%%--------------------------------------------------------------------
-spec group_get_harvesters(Config :: term(), GroupId :: od_group:id()) ->
    {ok, [od_harvester:id()]}.
group_get_harvesters(Config, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, get_harvesters, [?ROOT, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Returns OZ privileges of a group.
%% @end
%%--------------------------------------------------------------------
-spec group_get_oz_privileges(Config :: term(), GroupId :: od_group:id()) ->
    {ok, [privileges:oz_privilege()]}.
group_get_oz_privileges(Config, GroupId) ->
    ?assertMatch({ok, _}, call_oz(Config, group_logic, get_oz_privileges, [
        ?ROOT, GroupId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns effective OZ privileges of a group.
%% @end
%%--------------------------------------------------------------------
-spec group_get_eff_oz_privileges(Config :: term(), GroupId :: od_group:id()) ->
    {ok, [privileges:oz_privilege()]}.
group_get_eff_oz_privileges(Config, GroupId) ->
    ?assertMatch({ok, _}, call_oz(Config, group_logic, get_eff_oz_privileges, [
        ?ROOT, GroupId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of all groups in onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_groups(Config :: term()) -> {ok, [od_group:id()]}.
list_groups(Config) ->
    ?assertMatch({ok, _}, call_oz(Config, group_logic, list, [?ROOT])).


%%--------------------------------------------------------------------
%% @doc
%% Sets OZ privileges of a group.
%% @end
%%--------------------------------------------------------------------
-spec group_set_oz_privileges(Config :: term(), GroupId :: od_group:id(),
    Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:oz_privilege()]) -> ok.
group_set_oz_privileges(Config, GroupId, Operation, Privileges) ->
    ?assertMatch(ok, call_oz(Config, group_logic, update_oz_privileges, [
        ?ROOT, GroupId, Operation, Privileges
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given group from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_group(Config :: term(), GroupId :: od_group:id()) -> ok.
delete_group(Config, GroupId) ->
    ?assertMatch(ok, call_oz(
        Config, group_logic, delete, [?ROOT, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Marks group as protected
%% @end
%%--------------------------------------------------------------------
-spec mark_group_protected(Config :: term(), od_group:id(), Protected :: boolean()) -> ok.
mark_group_protected(Config, GroupId, Protected) ->
    ?assertMatch({ok, _}, call_oz(
        Config, od_group, update,
        [GroupId, fun(Group) ->
            {ok, Group#od_group{protected = Protected}}
        end]
    )),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Adds a user a to group in onezone with ROOT auth.
%% @end
%%--------------------------------------------------------------------
-spec group_add_user(Config :: term(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> {ok, UserId :: od_user:id()}.
group_add_user(Config, GroupId, UserId) ->
    group_add_user(Config, ?ROOT, GroupId, UserId).


%%--------------------------------------------------------------------
%% @doc
%% Adds a user a to group in onezone.
%% @end
%%--------------------------------------------------------------------
-spec group_add_user(Config :: term(), aai:auth(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> {ok, UserId :: od_user:id()}.
group_add_user(Config, Client, GroupId, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, add_user, [Client, GroupId, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Adds a group a to group in onezone with ROOT auth.
%% @end
%%--------------------------------------------------------------------
-spec group_add_group(Config :: term(), GroupId :: od_group:id(),
    ChildGroupId :: od_group:id()) -> {ok, ChildGroupId :: od_group:id()}.
group_add_group(Config, GroupId, ChildGroupId) ->
    group_add_group(Config, ?ROOT, GroupId, ChildGroupId).


%%--------------------------------------------------------------------
%% @doc
%% Adds a group a to group in onezone.
%% @end
%%--------------------------------------------------------------------
-spec group_add_group(Config :: term(), aai:auth(), GroupId :: od_group:id(),
    ChildGroupId :: od_group:id()) -> {ok, ChildGroupId :: od_group:id()}.
group_add_group(Config, Client, GroupId, ChildGroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, add_group, [Client, GroupId, ChildGroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Removes user from a group from onezone.
%% @end
%%--------------------------------------------------------------------
-spec group_remove_user(Config :: term(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> ok.
group_remove_user(Config, GroupId, UserId) ->
    ?assertMatch(ok, call_oz(
        Config, group_logic, remove_user, [?ROOT, GroupId, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Removes child group from a group from onezone.
%% @end
%%--------------------------------------------------------------------
-spec group_remove_group(Config :: term(), GroupId :: od_group:id(),
    ChildGroupId :: od_group:id()) -> ok.
group_remove_group(Config, GroupId, ChildGroupId) ->
    ?assertMatch(ok, call_oz(
        Config, group_logic, remove_group, [?ROOT, GroupId, ChildGroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Leaves space as a group.
%% @end
%%--------------------------------------------------------------------
-spec group_leave_space(Config :: term(), GroupId :: od_group:id(),
    SpaceId :: od_space:id()) -> ok.
group_leave_space(Config, GroupId, SpaceId) ->
    ?assertMatch(ok, call_oz(
        Config, group_logic, leave_space, [?ROOT, GroupId, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Leaves handle service as a group.
%% @end
%%--------------------------------------------------------------------
-spec group_leave_handle_service(Config :: term(), GroupId :: od_group:id(),
    HandleServiceId :: od_handle_service:id()) -> ok.
group_leave_handle_service(Config, GroupId, HandleServiceId) ->
    ?assertMatch(ok, call_oz(
        Config, group_logic, leave_handle_service,
        [?ROOT, GroupId, HandleServiceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Returns the list of effective users in given group.
%% @end
%%--------------------------------------------------------------------
-spec group_get_eff_users(Config :: term(), GroupId :: od_group:id()) ->
    {ok, [od_user:id()]}.
group_get_eff_users(Config, GroupId) ->
    ?assertMatch({ok, _}, call_oz(Config, group_logic, get_eff_users, [
        ?ROOT, GroupId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns privileges of a user in given group.
%% @end
%%--------------------------------------------------------------------
-spec group_get_user_privileges(Config :: term(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> {ok, [privileges:group_privilege()]}.
group_get_user_privileges(Config, GroupId, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, group_logic, get_user_privileges, [
        ?ROOT, GroupId, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns effective privileges of a user in given group.
%% @end
%%--------------------------------------------------------------------
-spec group_get_eff_user_privileges(Config :: term(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> {ok, [privileges:group_privilege()]}.
group_get_eff_user_privileges(Config, GroupId, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, group_logic, get_eff_user_privileges, [
        ?ROOT, GroupId, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns privileges of a subgroup in given group.
%% @end
%%--------------------------------------------------------------------
-spec group_get_group_privileges(Config :: term(), GroupId :: od_group:id(),
    UserId :: od_group:id()) -> {ok, [privileges:group_privilege()]}.
group_get_group_privileges(Config, GroupId, ChildGroupId) ->
    ?assertMatch({ok, _}, call_oz(Config, group_logic, get_child_privileges, [
        ?ROOT, GroupId, ChildGroupId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Creates a space in onezone with a random name.
%% @end
%%--------------------------------------------------------------------
-spec create_space(Config :: term(), Client :: aai:auth()) ->
    {ok, od_space:id()}.
create_space(Config, Client) ->
    create_space(Config, Client, ?UNIQUE_STRING).


%%--------------------------------------------------------------------
%% @doc
%% Creates a space in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_space(Config :: term(), Client :: aai:auth(),
    Name :: od_space:name() | entity_logic:data()) -> {ok, od_space:id()}.
create_space(Config, Client, NameOdData) ->
    Result = case Client of
        ?USER(UserId) ->
            call_oz(Config, user_logic, create_space, [Client, UserId, NameOdData]);
        _ ->
            call_oz(Config, space_logic, create, [Client, NameOdData])
    end,
    ?assertMatch({ok, _}, Result).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves space data from onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_space(Config :: term(), SpaceId :: od_space:id()) ->
    {ok, #od_space{}}.
get_space(Config, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, get, [?ROOT, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of all spaces in onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_spaces(Config :: term()) -> {ok, [od_space:id()]}.
list_spaces(Config) ->
    ?assertMatch({ok, _}, call_oz(Config, space_logic, list, [?ROOT])).


%%--------------------------------------------------------------------
%% @doc
%% Updates space name.
%% @end
%%--------------------------------------------------------------------
-spec update_space(Config :: term(), SpaceId :: od_space:id(),
    Name :: od_space:name()) -> ok.
update_space(Config, SpaceId, Name) ->
    ?assertMatch(ok, call_oz(
        Config, space_logic, update, [?ROOT, SpaceId, Name]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given space from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_space(Config :: term(), SpaceId :: od_space:id()) -> ok.
delete_space(Config, SpaceId) ->
    ?assertMatch(ok, call_oz(
        Config, space_logic, delete, [?ROOT, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves users of given space from onezone.
%% @end
%%--------------------------------------------------------------------
-spec space_get_users(Config :: term(),
    SpaceId :: od_space:id()) -> {ok, [od_user:id()]}.
space_get_users(Config, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, get_users, [?ROOT, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves groups of given space from onezone.
%% @end
%%--------------------------------------------------------------------
-spec space_get_groups(Config :: term(),
    SpaceId :: od_space:id()) -> {ok, [od_group:id()]}.
space_get_groups(Config, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, get_groups, [?ROOT, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves effective providers of given space from onezone.
%% @end
%%--------------------------------------------------------------------
-spec space_get_providers(Config :: term(),
    SpaceId :: od_space:id()) -> {ok, [od_provider:id()]}.
space_get_providers(Config, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, get_eff_providers, [?ROOT, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves harvesters of given space from onezone.
%% @end
%%--------------------------------------------------------------------
-spec space_get_harvesters(Config :: term(),
    SpaceId :: od_space:id()) -> {ok, [od_harvester:id()]}.
space_get_harvesters(Config, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, get_harvesters, [?ROOT, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves supporting storages of given space from onezone.
%% @end
%%--------------------------------------------------------------------
-spec space_get_storages(Config :: term(),
    SpaceId :: od_space:id()) -> {ok, [od_storage:id()]}.
space_get_storages(Config, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, get_storages, [?ROOT, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified storage (ceases support for given space).
%% @end
%%--------------------------------------------------------------------
-spec space_remove_storage(Config :: term(),
    SpaceId :: od_space:id(), StorageId :: od_storage:id()) -> ok.
space_remove_storage(Config, SpaceId, StorageId) ->
    ?assertMatch(ok, call_oz(
        Config, space_logic, remove_storage, [?ROOT, SpaceId, StorageId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified provider (ceases support for given space by all
%% storages belonging to given provider).
%% @end
%%--------------------------------------------------------------------
-spec space_remove_provider(Config :: term(),
    SpaceId :: od_space:id(), ProviderId :: od_provider:id()) -> ok.
space_remove_provider(Config, SpaceId, ProviderId) ->
    ?assertMatch(ok, call_oz(
        Config, space_logic, remove_provider, [?ROOT, SpaceId, ProviderId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Remove space from given harvester.
%% @end
%%--------------------------------------------------------------------
-spec space_remove_harvester(Config :: term(),
    SpaceId :: od_space:id(), HarvesterId :: od_harvester:id()) -> ok.
space_remove_harvester(Config, SpaceId, HarvesterId) ->
    ?assertMatch(ok, call_oz(
        Config, space_logic, remove_harvester, [?ROOT, SpaceId, HarvesterId]
    )).


-spec space_add_owner(Config :: term(), od_space:id(), od_user:id()) -> {ok, od_user:id()}.
space_add_owner(Config, SpaceId, UserId) ->
    ?assertMatch(ok, call_oz(
        Config, space_logic, add_owner, [?ROOT, SpaceId, UserId]
    )).


-spec space_remove_owner(Config :: term(), od_space:id(), od_user:id()) -> ok.
space_remove_owner(Config, SpaceId, UserId) ->
    ?assertMatch(ok, call_oz(
        Config, space_logic, remove_owner, [?ROOT, SpaceId, UserId]
    )).


-spec create_space_owner(Config :: term(), od_space:id()) -> {ok, od_user:id()}.
create_space_owner(Config, SpaceId) ->
    {ok, NewUser} = create_user(Config),
    space_add_user(Config, SpaceId, NewUser),
    % the owner gets no privileges in the space, but being the owner should let
    % them do any API operation anyway
    space_add_owner(Config, SpaceId, NewUser),
    space_set_user_privileges(Config, SpaceId, NewUser, [], privileges:space_admin()),
    {ok, NewUser}.


%%--------------------------------------------------------------------
%% @doc
%% Adds a user to a space.
%% @end
%%--------------------------------------------------------------------
-spec space_add_user(Config :: term(), SpaceId :: od_space:id(),
    UserId :: od_user:id()) -> {ok, od_user:id()}.
space_add_user(Config, SpaceId, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, add_user, [?ROOT, SpaceId, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Remove user from space.
%% @end
%%--------------------------------------------------------------------
-spec space_remove_user(Config :: term(), SpaceId :: od_space:id(),
    UserId :: od_user:id()) -> {ok, od_user:id()}.
space_remove_user(Config, SpaceId, UserId) ->
    ?assertMatch(ok, call_oz(
        Config, space_logic, remove_user, [?ROOT, SpaceId, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Adds a group to a space.
%% @end
%%--------------------------------------------------------------------
-spec space_add_group(Config :: term(), SpaceId :: od_space:id(),
    GroupId :: od_group:id()) -> {ok, od_group:id()}.
space_add_group(Config, SpaceId, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, add_group, [?ROOT, SpaceId, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves space privileges of given user from onezone.
%% @end
%%--------------------------------------------------------------------
-spec space_get_user_privileges(Config :: term(), SpaceId :: od_space:id(),
    UserId :: od_user:id()) -> {ok, [privileges:space_privilege()]}.
space_get_user_privileges(Config, SpaceId, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, get_user_privileges, [?ROOT, SpaceId, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Sets privileges of a user in a space.
%% @end
%%--------------------------------------------------------------------
-spec space_set_user_privileges(Config :: term(), SpaceId :: od_space:id(),
    UserId :: od_user:id(), PrivsToGrant :: [privileges:space_privilege()],
    PrivsToRevoke :: [privileges:space_privilege()]) -> ok.
space_set_user_privileges(Config, SpaceId, UserId, PrivsToGrant, PrivsToRevoke) ->
    ?assertMatch(ok, call_oz(Config, space_logic, update_user_privileges, [
        ?ROOT, SpaceId, UserId, PrivsToGrant, PrivsToRevoke
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves space privileges of given group from onezone.
%% @end
%%--------------------------------------------------------------------
-spec space_get_group_privileges(Config :: term(), SpaceId :: od_space:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:space_privilege()]}.
space_get_group_privileges(Config, SpaceId, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, get_group_privileges, [?ROOT, SpaceId, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Sets privileges of a group in a space.
%% @end
%%--------------------------------------------------------------------
-spec space_set_group_privileges(Config :: term(), SpaceId :: od_space:id(),
    GroupId :: od_group:id(), PrivsToGrant :: [privileges:space_privilege()],
    PrivsToRevoke :: [privileges:space_privilege()]) -> ok.
space_set_group_privileges(Config, SpaceId, GroupId, PrivsToGrant, PrivsToRevoke) ->
    ?assertMatch(ok, call_oz(Config, space_logic, update_group_privileges, [
        ?ROOT, SpaceId, GroupId, PrivsToGrant, PrivsToRevoke
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Creates a user invite token to given space.
%% @end
%%--------------------------------------------------------------------
-spec space_invite_user_token(Config :: term(),
    Client :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, tokens:token()}.
space_invite_user_token(Config, Client, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, create_user_invite_token, [Client, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a group invite token to given space.
%% @end
%%--------------------------------------------------------------------
-spec space_invite_group_token(Config :: term(),
    Client :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, tokens:token()}.
space_invite_group_token(Config, Client, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, create_group_invite_token, [Client, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a provider invite token to given space.
%% @end
%%--------------------------------------------------------------------
-spec create_space_support_token(Config :: term(),
    Client :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, tokens:token()}.
create_space_support_token(Config, Client, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, create_space_support_token, [Client, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Checks if given space has given effective user.
%% @end
%%--------------------------------------------------------------------
-spec space_has_effective_user(Config :: term(), SpaceId :: od_space:id(),
    UserId :: od_user:id()) -> boolean().
space_has_effective_user(Config, SpaceId, UserId) ->
    {ok, EffUsers} = ?assertMatch({ok, _}, call_oz(
        Config, space_logic, get_eff_users, [?ROOT, SpaceId]
    )),
    lists:member(UserId, EffUsers).


%%--------------------------------------------------------------------
%% @doc
%% Creates a share in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_share(Config :: term(), Client :: aai:auth(),
    ShareId :: od_share:id(), Name :: od_share:name(), RootFileId :: binary(),
    SpaceId :: od_space:id()) -> {ok, od_share:id()}.
create_share(Config, Client, ShareId, Name, RootFileId, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(Config, share_logic, create, [
        Client, ShareId, Name, RootFileId, SpaceId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Creates a share in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_share(Config :: term(), Client :: aai:auth(),
    Data :: map()) -> {ok, od_share:id()}.
create_share(Config, Client, Data) ->
    ?assertMatch({ok, _}, call_oz(
        Config, share_logic, create, [Client, Data]
    )).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all shares in onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_shares(Config :: term()) -> {ok, [od_share:id()]}.
list_shares(Config) ->
    ?assertMatch({ok, _}, call_oz(Config, share_logic, list, [?ROOT])).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves share data from onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_share(Config :: term(), ShareId :: od_share:id()) ->
    {ok, #od_share{}}.
get_share(Config, ShareId) ->
    ?assertMatch({ok, #od_share{}}, call_oz(
        Config, share_logic, get, [?ROOT, ShareId])
    ).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given share from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_share(Config :: term(), ShareId :: od_share:id()) -> ok.
delete_share(Config, ShareId) ->
    ?assertMatch(ok, call_oz(Config, share_logic, delete, [?ROOT, ShareId])).


%%--------------------------------------------------------------------
%% @doc
%% Get share public URL.
%% @end
%%--------------------------------------------------------------------
-spec get_share_public_url(Config :: term(), ShareId :: od_share:id()) -> binary().
get_share_public_url(Config, ShareId) ->
    ?assertMatch(<<_/binary>>, call_oz(
        Config, share_logic, share_id_to_public_url, [ShareId])
    ).


%%--------------------------------------------------------------------
%% @doc
%% Creates a provider without a creator user and with a random name.
%% @end
%%--------------------------------------------------------------------
-spec create_provider(Config :: term()) ->
    {ok, {od_provider:id(), ProviderRootToken :: tokens:serialized()}}.
create_provider(Config) ->
    create_provider(Config, ?UNIQUE_STRING).


%%--------------------------------------------------------------------
%% @doc
%% Creates a provider without a creator user.
%% @end
%%--------------------------------------------------------------------
-spec create_provider(Config :: term(), NameOrData :: od_provider:name() | #{}) ->
    {ok, {od_provider:id(), ProviderRootToken :: tokens:serialized()}}.
create_provider(Config, NameOrData) ->
    create_provider(Config, undefined, NameOrData).

%%--------------------------------------------------------------------
%% @doc
%% Creates a provider with given creator user id, who is automatically added to
%% the corresponding cluster.
%% %% @end
%%--------------------------------------------------------------------
-spec create_provider(Config :: term(), CreatorUserId :: od_user:id(),
    NameOrData :: od_provider:name() | #{}) ->
    {ok, {od_provider:id(), ProviderRootToken :: tokens:serialized()}}.
create_provider(Config, undefined, Name) ->
    % Create an admin for the provider if none was specified
    {ok, User} = create_user(Config),
    user_set_oz_privileges(Config, User, [?OZ_PROVIDERS_INVITE], []),
    create_provider(Config, User, Name);
create_provider(Config, CreatorUserId, Name) when is_binary(Name) ->
    create_provider(Config, CreatorUserId, #{
        <<"name">> => Name,
        <<"adminEmail">> => <<"admin@oneprovider.example.com">>,
        <<"domain">> => <<"oneprovider.example.com">>,
        <<"subdomainDelegation">> => false,
        <<"latitude">> => 0.0,
        <<"longitude">> => 0.0
    });
create_provider(Config, CreatorUserId, Data) ->
    {ok, RegistrationToken} = create_temporary_provider_registration_token(
        Config, ?USER(CreatorUserId), CreatorUserId
    ),
    {ok, {ProviderId, RootToken}} = ?assertMatch({ok, _}, call_oz(
        Config, provider_logic, create, [
            ?USER(CreatorUserId), Data#{<<"token">> => RegistrationToken}
        ]
    )),
    {ok, SerializedRootToken} = tokens:serialize(RootToken),
    {ok, {ProviderId, SerializedRootToken}}.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves provider data from onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_provider(Config :: term(), ProviderId :: od_provider:id()) ->
    {ok, #od_provider{}}.
get_provider(Config, ProviderId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, provider_logic, get, [?ROOT, ProviderId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of all providers in onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_providers(Config :: term()) -> {ok, [od_provider:id()]}.
list_providers(Config) ->
    ?assertMatch({ok, _}, call_oz(Config, provider_logic, list, [?ROOT])).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given provider from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_provider(Config :: term(), ProviderId :: od_provider:id()) -> ok.
delete_provider(Config, ProviderId) ->
    ?assertMatch(ok, call_oz(Config, provider_logic, delete, [
        ?ROOT, ProviderId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Supports a space by a provider based on space id
%% (with default support size and new dummy storage).
%% @end
%%--------------------------------------------------------------------
-spec support_space_by_provider(Config :: term(), ProviderId :: od_provider:id(),
    SpaceId :: od_space:id()) -> {ok, SpaceId :: od_space:id()}.
support_space_by_provider(Config, ProviderId, SpaceId) ->
    % Create dummy storage
    {ok, StorageId} = ?assertMatch({ok, _}, create_storage(
        Config, ?PROVIDER(ProviderId), ?STORAGE_NAME1)
    ),
    support_space(Config, ?PROVIDER(ProviderId), StorageId, SpaceId, minimum_support_size(Config)).


%%--------------------------------------------------------------------
%% @doc
%% Supports a space by a storage based on space id (with default support size).
%% @end
%%--------------------------------------------------------------------
-spec support_space(Config :: term(), Client :: aai:auth(), StorageId :: od_storage:id(),
    SpaceId :: od_space:id()) -> {ok, SpaceId :: od_space:id()}.
support_space(Config, Client, StorageId, SpaceId) ->
    support_space(Config, Client, StorageId, SpaceId, minimum_support_size(Config)).


%%--------------------------------------------------------------------
%% @doc
%% Supports a space by a provider based on space id and support size.
%% @end
%%--------------------------------------------------------------------
-spec support_space(Config :: term(), Client :: aai:auth(), StorageId :: od_storage:id(),
    SpaceId :: od_space:id(), Size :: non_neg_integer()) ->
    {ok, {ProviderId :: binary(), KeyFile :: string(), CertFile :: string()}}.
support_space(Config, Client, StorageId, SpaceId, Size) ->
    {ok, OzAdmin} = create_user(Config),
    user_set_oz_privileges(Config, OzAdmin, privileges:oz_admin(), []),
    {ok, Token} = create_space_support_token(Config, ?USER(OzAdmin), SpaceId),
    support_space_using_token(Config, Client, StorageId, Token, Size).


%%--------------------------------------------------------------------
%% @doc
%% Supports a space by a provider based on token and support size.
%% @end
%%--------------------------------------------------------------------
-spec support_space_using_token(Config :: term(), Client :: aai:auth(),
    StorageId :: od_storage:id(), Token :: binary() | tokens:token(),
    Size :: non_neg_integer()) -> {ok, SpaceId :: od_space:id()}.
support_space_using_token(Config, Client, StorageId, Token, Size) ->
    ?assertMatch({ok, _}, call_oz(Config, storage_logic, support_space, [
        Client, StorageId, Token, Size
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Supports a space by a provider based on space id
%% (with default support size and virtual storage with id equal to providers).
%% @end
%%--------------------------------------------------------------------
-spec support_space_by_legacy_storage(Config :: term(), ProviderId :: od_provider:id(),
    SpaceId :: od_space:id()) -> {ok, SpaceId :: od_space:id()}.
support_space_by_legacy_storage(Config, ProviderId, SpaceId) ->
    case call_oz(Config, provider_logic, has_storage, [ProviderId, ProviderId]) of
        true -> ok;
        false ->
            ?assertMatch({ok, _}, create_storage(
                Config, ?PROVIDER(ProviderId), ProviderId, ?STORAGE_NAME1)
            )
    end,
    support_space(Config, ?PROVIDER(ProviderId), ProviderId, SpaceId, minimum_support_size(Config)).

%%--------------------------------------------------------------------
%% @doc
%% Revoke space support by given storage.
%% @end
%%--------------------------------------------------------------------
-spec unsupport_space(Config :: term(), StorageId :: od_storage:id(),
    SpaceId :: od_space:id()) -> ok.
unsupport_space(Config, StorageId, SpaceId) ->
    ?assertMatch(ok, call_oz(Config, storage_logic, revoke_support, [
        ?ROOT, StorageId, SpaceId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets provider subdomain.
%% @end
%%--------------------------------------------------------------------
-spec enable_subdomain_delegation(Config :: term(),
    ProviderId :: od_provider:id(), Subdomain :: binary(),
    IPs :: [inet:ip4_address()]) -> ok.
enable_subdomain_delegation(Config, ProviderId, Subdomain, IPs) ->
    Data = #{
        <<"subdomainDelegation">> => true,
        <<"subdomain">> => Subdomain,
        <<"ipList">> => IPs},
    ?assertMatch(ok, call_oz(Config, provider_logic, update_domain_config, [?ROOT, ProviderId, Data])).


%%--------------------------------------------------------------------
%% @doc
%% Sets provider domain (not a delegated subdomain).
%% @end
%%--------------------------------------------------------------------
-spec set_provider_domain(Config :: term(), ProviderId :: od_provider:od(),
    Domain :: binary()) -> ok.
set_provider_domain(Config, ProviderId, Domain) ->
    Data = #{
        <<"subdomainDelegation">> => false,
        <<"domain">> => Domain},
    ?assertMatch(ok, call_oz(Config, provider_logic, update_domain_config, [?ROOT, ProviderId, Data])).


%%--------------------------------------------------------------------
%% @doc
%% Creates a handle service.
%% @end
%%--------------------------------------------------------------------
-spec create_handle_service(Config :: term(), Client :: aai:auth(),
    Name :: od_handle_service:name(),
    ProxyEndpoint :: od_handle_service:proxy_endpoint(),
    ServiceProperties :: od_handle_service:service_properties()) ->
    {ok, od_handle_service:id()}.
create_handle_service(Config, Client, Name, ProxyEndpoint, ServiceProperties) ->
    Result = case Client of
        ?USER(UserId) ->
            call_oz(Config, user_logic, create_handle_service, [
                Client, UserId, Name, ProxyEndpoint, ServiceProperties
            ]);
        _ ->
            call_oz(Config, handle_service_logic, create, [
                Client, Name, ProxyEndpoint, ServiceProperties
            ])
    end,
    ?assertMatch({ok, _}, Result).


%%--------------------------------------------------------------------
%% @doc
%% Creates a handle service.
%% @end
%%--------------------------------------------------------------------
-spec create_handle_service(Config :: term(), Client :: aai:auth(),
    Data :: map()) -> {ok, od_handle_service:id()}.
create_handle_service(Config, Client, Data) ->
    Result = case Client of
        ?USER(UserId) ->
            call_oz(Config, user_logic, create_handle_service, [Client, UserId, Data]);
        _ ->
            call_oz(Config, handle_service_logic, create, [Client, Data])
    end,
    ?assertMatch({ok, _}, Result).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of all handle_services in onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_handle_services(Config :: term()) -> {ok, [od_handle_service:id()]}.
list_handle_services(Config) ->
    ?assertMatch({ok, _}, call_oz(Config, handle_service_logic, list, [
        ?ROOT
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves handle service data from onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_handle_service(Config :: term(),
    HandleServiceId :: od_handle_service:id()) -> {ok, #od_handle_service{}}.
get_handle_service(Config, HandleServiceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, handle_service_logic, get, [?ROOT, HandleServiceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves groups belonging to handle service from onezone.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_get_groups(Config :: term(),
    HandleServiceId :: od_handle_service:id()) -> {ok, [od_group:id()]}.
handle_service_get_groups(Config, HandleServiceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, handle_service_logic, get_groups, [?ROOT, HandleServiceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves users belonging to handle service from onezone.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_get_users(Config :: term(),
    HandleServiceId :: od_handle_service:id()) -> {ok, [od_user:id()]}.
handle_service_get_users(Config, HandleServiceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, handle_service_logic, get_users, [?ROOT, HandleServiceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves privileges of user belonging to handle service from onezone.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_get_user_privileges(Config :: term(),
    HandleServiceId :: od_handle_service:id(),
    UserId :: od_user:id()) -> {ok, [atom()]}.
handle_service_get_user_privileges(Config, HandleServiceId, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, handle_service_logic, get_user_privileges, [
            ?ROOT, HandleServiceId, UserId
        ]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves privileges of group belonging to handle service from onezone.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_get_group_privileges(Config :: term(),
    HandleServiceId :: od_handle_service:id(),
    GroupId :: od_group:id()) -> {ok, [atom()]}.
handle_service_get_group_privileges(Config, HandleServiceId, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, handle_service_logic, get_group_privileges, [
            ?ROOT, HandleServiceId, GroupId
        ]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given handle_service from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_handle_service(Config :: term(),
    HandleServiceId :: od_handle_service:id()) -> ok.
delete_handle_service(Config, HandleServiceId) ->
    ?assertMatch(ok, call_oz(Config, handle_service_logic, delete, [
        ?ROOT, HandleServiceId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Adds a user to a handle service.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_add_user(Config :: term(),
    HServiceId :: od_handle_service:id(), UserId :: od_user:id()) ->
    {ok, od_user:id()}.
handle_service_add_user(Config, HServiceId, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, handle_service_logic, add_user, [
        ?ROOT, HServiceId, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Removes user from handle service.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_remove_user(Config :: term(),
    HServiceId :: od_handle_service:id(), UserId :: od_user:id()) -> ok.
handle_service_remove_user(Config, HServiceId, UserId) ->
    ?assertMatch(ok, call_oz(Config, handle_service_logic, remove_user, [
        ?ROOT, HServiceId, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets privileges of a user in a handle service.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_set_user_privileges(Config :: term(),
    HServiceId :: od_handle_service:id(), UserId :: od_user:id(),
    PrivsToGrant :: [privileges:handle_service_privilege()],
    PrivsToRevoke :: [privileges:handle_service_privilege()]) -> ok.
handle_service_set_user_privileges(
    Config, HServiceId, UserId, PrivsToGrant, PrivsToRevoke
) ->
    ?assertMatch(ok, call_oz(Config, handle_service_logic,
        update_user_privileges, [?ROOT, HServiceId, UserId, PrivsToGrant, PrivsToRevoke]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Adds a group to a handle service.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_add_group(Config :: term(),
    HServiceId :: od_handle_service:id(), GroupId :: od_group:id()) ->
    {ok, od_group:id()}.
handle_service_add_group(Config, HServiceId, GroupId) ->
    ?assertMatch({ok, _}, call_oz(Config, handle_service_logic, add_group, [
        ?ROOT, HServiceId, GroupId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Removes a group from handle service.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_remove_group(Config :: term(),
    HServiceId :: od_handle_service:id(), GroupId :: od_group:id()) -> ok.
handle_service_remove_group(Config, HServiceId, GroupId) ->
    ?assertMatch(ok, call_oz(Config, handle_service_logic, remove_group, [
        ?ROOT, HServiceId, GroupId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets privileges of a group in a handle service.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_set_group_privileges(Config :: term(),
    HServiceId :: od_handle_service:id(), GroupId :: od_group:id(),
    PrivsToGrant :: [privileges:handle_service_privilege()],
    PrivsToRevoke :: [privileges:handle_service_privilege()]) -> ok.
handle_service_set_group_privileges(
    Config, HServiceId, GroupId, PrivsToGrant, PrivsToRevoke
) ->
    ?assertMatch(ok, call_oz(Config, handle_service_logic,
        update_group_privileges, [?ROOT, HServiceId, GroupId, PrivsToGrant, PrivsToRevoke]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a handle.
%% @end
%%--------------------------------------------------------------------
-spec create_handle(Config :: term(), Client :: aai:auth(),
    HandleServiceId :: od_handle_service:id(),
    ResourceType :: od_handle:resource_type(),
    ResourceId :: od_handle:resource_id(), Metadata :: od_handle:metadata()) ->
    {ok, od_handle:id()}.
create_handle(Config, Client, HandleServiceId, ResourceType, ResourceId, Metadata) ->
    Result = case Client of
        ?USER(UserId) ->
            call_oz(Config, user_logic, create_handle, [
                Client, UserId, HandleServiceId, ResourceType, ResourceId, Metadata
            ]);
        _ ->
            call_oz(Config, handle_logic, create, [
                Client, HandleServiceId, ResourceType, ResourceId, Metadata
            ])
    end,
    ?assertMatch({ok, _}, Result).


%%--------------------------------------------------------------------
%% @doc
%% Creates a handle.
%% @end
%%--------------------------------------------------------------------
-spec create_handle(Config :: term(), Client :: aai:auth(),
    Data :: map()) -> {ok, od_handle:id()}.
create_handle(Config, Client, Data) ->
    Result = case Client of
        ?USER(UserId) ->
            call_oz(Config, user_logic, create_handle, [Client, UserId, Data]);
        _ ->
            call_oz(Config, handle_logic, create, [Client, Data])
    end,
    ?assertMatch({ok, _}, Result).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of all handles in onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_handles(Config :: term()) -> {ok, [od_handle:id()]}.
list_handles(Config) ->
    ?assertMatch({ok, _}, call_oz(Config, handle_logic, list, [
        ?ROOT
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves handle data from onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_handle(Config :: term(), HandleId :: od_handle:id()) ->
    {ok, #od_handle{}}.
get_handle(Config, HandleId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, handle_logic, get, [?ROOT, HandleId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Updates a handle
%% @end
%%--------------------------------------------------------------------
-spec update_handle(Config :: term(), HandleId :: od_handle:id(),
    Data :: map()) -> ok.
update_handle(Config, HandleId, Data) ->
    ?assertMatch(ok, call_oz(Config, handle_logic, update, [
        ?ROOT, HandleId, Data
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Updates a handle
%% @end
%%--------------------------------------------------------------------
-spec update_handle(Config :: term(), HandleId :: od_handle:id(),
    NewResourceType :: od_handle:resource_type(),
    NewResourceId :: od_handle:resource_id(),
    NewMetadata :: od_handle:metadata()) -> ok.
update_handle(Config, HandleId, NewResourceType, NewResourceId, NewMetadata) ->
    ?assertMatch(ok, call_oz(Config, handle_logic, update, [?ROOT, HandleId, #{
        <<"resourceType">> => NewResourceType,
        <<"resourceId">> => NewResourceId,
        <<"metadata">> => NewMetadata
    }])).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given handle from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_handle(Config :: term(), HandleId :: od_handle:id()) -> ok.
delete_handle(Config, HandleId) ->
    ?assertMatch(ok, call_oz(Config, handle_logic, delete, [?ROOT, HandleId])).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves groups belonging to handle from onezone.
%% @end
%%--------------------------------------------------------------------
-spec handle_get_groups(Config :: term(), HandleId :: od_handle:id()) ->
    {ok, [od_group:id()]}.
handle_get_groups(Config, HandleId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, handle_logic, get_groups, [?ROOT, HandleId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves users belonging to handle from onezone.
%% @end
%%--------------------------------------------------------------------
-spec handle_get_users(Config :: term(), HandleId :: od_handle:id()) ->
    {ok, [od_user:id()]}.
handle_get_users(Config, HandleId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, handle_logic, get_users, [?ROOT, HandleId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Adds a user to a handle.
%% @end
%%--------------------------------------------------------------------
-spec handle_add_user(Config :: term(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> {ok, od_user:id()}.
handle_add_user(Config, HandleId, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, handle_logic, add_user, [
        ?ROOT, HandleId, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Removes user from handle.
%% @end
%%--------------------------------------------------------------------
-spec handle_remove_user(Config :: term(), HandleId :: od_handle:id(),
    UserId :: od_user:id()) -> {ok, od_user:id()}.
handle_remove_user(Config, HandleId, UserId) ->
    ?assertMatch(ok, call_oz(Config, handle_logic, remove_user, [
        ?ROOT, HandleId, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets privileges of a user in a handle.
%% @end
%%--------------------------------------------------------------------
-spec handle_set_user_privileges(Config :: term(),
    HandleId :: od_handle:id(), UserId :: od_user:id(),
    PrivsToGrant :: [privileges:handle_privilege()],
    PrivsToRevoke :: [privileges:handle_privilege()]) -> ok.
handle_set_user_privileges(Config, HandleId, UserId, PrivsToGrant, PrivsToRevoke) ->
    ?assertMatch(ok, call_oz(Config, handle_logic,
        update_user_privileges, [?ROOT, HandleId, UserId, PrivsToGrant, PrivsToRevoke]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves privileges of user belonging to handle from onezone.
%% @end
%%--------------------------------------------------------------------
-spec handle_get_user_privileges(Config :: term(),
    HandleId :: od_service:id(), UserId :: od_user:id()) -> {ok, [atom()]}.
handle_get_user_privileges(Config, HandleId, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, handle_logic, get_user_privileges, [?ROOT, HandleId, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Adds a group to a handle.
%% @end
%%--------------------------------------------------------------------
-spec handle_add_group(Config :: term(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> {ok, od_group:id()}.
handle_add_group(Config, HandleId, GroupId) ->
    ?assertMatch({ok, _}, call_oz(Config, handle_logic, add_group, [
        ?ROOT, HandleId, GroupId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Removes group from handle.
%% @end
%%--------------------------------------------------------------------
-spec handle_remove_group(Config :: term(), HandleId :: od_handle:id(),
    GroupId :: od_group:id()) -> {ok, od_user:id()}.
handle_remove_group(Config, HandleId, GroupId) ->
    ?assertMatch(ok, call_oz(Config, handle_logic, remove_group, [
        ?ROOT, HandleId, GroupId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets privileges of a group in a handle.
%% @end
%%--------------------------------------------------------------------
-spec handle_set_group_privileges(Config :: term(),
    HandleId :: od_handle:id(), GroupId :: od_user:id(),
    Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:handle_privilege()]) -> ok.
handle_set_group_privileges(Config, HandleId, GroupId, Operation, Privs) ->
    ?assertMatch(ok, call_oz(Config, handle_logic,
        update_group_privileges, [?ROOT, HandleId, GroupId, Operation, Privs]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves privileges of group belonging to handle from onezone.
%% @end
%%--------------------------------------------------------------------
-spec handle_get_group_privileges(Config :: term(),
    HandleId :: od_service:id(), GroupId :: od_user:id()) -> {ok, [atom()]}.
handle_get_group_privileges(Config, HandleId, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, handle_logic, get_group_privileges, [?ROOT, HandleId, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a harvester in onezone with default data.
%% @end
%%--------------------------------------------------------------------
-spec create_harvester(Config :: term(), Client :: aai:auth()) ->
    {ok, od_harvester:id()}.
create_harvester(Config, Client) ->
    create_harvester(Config, Client, ?HARVESTER_CREATE_DATA).


%%--------------------------------------------------------------------
%% @doc
%% Creates a harvester in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_harvester(Config :: term(), Client :: aai:auth(),
    Data :: #{}) -> {ok, od_harvester:id()}.
create_harvester(Config, Client, Data) ->
    Result = case Client of
        ?USER(UserId) ->
            call_oz(Config, user_logic, create_harvester, [Client, UserId, Data]);
        _ ->
            call_oz(Config, harvester_logic, create, [Client, Data])
    end,

    ?assertMatch({ok, _}, Result).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves harvester data from onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_harvester(Config :: term(), HarvesterId :: od_harvester:id()) ->
    {ok, #od_harvester{}}.
get_harvester(Config, HarvesterId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, get, [?ROOT, HarvesterId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of all harvesters in onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_harvesters(Config :: term()) -> {ok, [od_harvester:id()]}.
list_harvesters(Config) ->
    ?assertMatch({ok, _}, call_oz(Config, harvester_logic, list, [?ROOT])).


%%--------------------------------------------------------------------
%% @doc
%% Updates harvester name.
%% @end
%%--------------------------------------------------------------------
-spec update_harvester(Config :: term(), HarvesterId :: od_harvester:id(),
    Name :: od_harvester:name()) -> ok.
update_harvester(Config, HarvesterId, Name) ->
    ?assertMatch(ok, call_oz(
        Config, harvester_logic, update, [?ROOT, HarvesterId, Name]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given harvester from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_harvester(Config :: term(), HarvesterId :: od_harvester:id()) -> ok.
delete_harvester(Config, HarvesterId) ->
    ?assertMatch(ok, call_oz(
        Config, harvester_logic, delete, [?ROOT, HarvesterId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves users of given harvester from onezone.
%% @end
%%--------------------------------------------------------------------
-spec harvester_get_users(Config :: term(),
    HarvesterId :: od_harvester:id()) -> {ok, [od_user:id()]}.
harvester_get_users(Config, HarvesterId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, get_users, [?ROOT, HarvesterId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves groups of given harvester from onezone.
%% @end
%%--------------------------------------------------------------------
-spec harvester_get_groups(Config :: term(),
    HarvesterId :: od_harvester:id()) -> {ok, [od_group:id()]}.
harvester_get_groups(Config, HarvesterId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, get_groups, [?ROOT, HarvesterId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves spaces of given harvester from onezone.
%% @end
%%--------------------------------------------------------------------
-spec harvester_get_spaces(Config :: term(),
    HarvesterId :: od_harvester:id()) -> {ok, [od_provider:id()]}.
harvester_get_spaces(Config, HarvesterId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, get_spaces, [?ROOT, HarvesterId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Remove space from given harvester.
%% @end
%%--------------------------------------------------------------------
-spec harvester_remove_space(Config :: term(),
    HarvesterId :: od_harvester:id(), ProviderId :: od_provider:id()) -> ok.
harvester_remove_space(Config, HarvesterId, SpaceId) ->
    ?assertMatch(ok, call_oz(
        Config, harvester_logic, remove_space, [?ROOT, HarvesterId, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Adds a user to a harvester.
%% @end
%%--------------------------------------------------------------------
-spec harvester_add_user(Config :: term(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id()) -> {ok, od_user:id()}.
harvester_add_user(Config, HarvesterId, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, add_user, [?ROOT, HarvesterId, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Remove user from harvester.
%% @end
%%--------------------------------------------------------------------
-spec harvester_remove_user(Config :: term(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id()) -> {ok, od_user:id()}.
harvester_remove_user(Config, HarvesterId, UserId) ->
    ?assertMatch(ok, call_oz(
        Config, harvester_logic, remove_user, [?ROOT, HarvesterId, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Adds a group to a harvester.
%% @end
%%--------------------------------------------------------------------
-spec harvester_add_group(Config :: term(), HarvesterId :: od_harvester:id(),
    GroupId :: od_group:id()) -> {ok, od_group:id()}.
harvester_add_group(Config, HarvesterId, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, add_group, [?ROOT, HarvesterId, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Adds a space to a harvester.
%% @end
%%--------------------------------------------------------------------
-spec harvester_add_space(Config :: term(), HarvesterId :: od_harvester:id(),
    SpaceId :: od_space:id()) -> {ok, od_space:id()}.
harvester_add_space(Config, HarvesterId, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, add_space, [?ROOT, HarvesterId, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves harvester privileges of given user from onezone.
%% @end
%%--------------------------------------------------------------------
-spec harvester_get_user_privileges(Config :: term(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id()) -> {ok, [privileges:harvester_privilege()]}.
harvester_get_user_privileges(Config, HarvesterId, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, get_user_privileges, [?ROOT, HarvesterId, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Sets privileges of a user in a harvester.
%% @end
%%--------------------------------------------------------------------
-spec harvester_set_user_privileges(Config :: term(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id(), PrivsToGrant :: [privileges:harvester_privilege()],
    PrivsToRevoke :: [privileges:harvester_privilege()]) -> ok.
harvester_set_user_privileges(Config, HarvesterId, UserId, PrivsToGrant, PrivsToRevoke) ->
    ?assertMatch(ok, call_oz(Config, harvester_logic, update_user_privileges, [
        ?ROOT, HarvesterId, UserId, PrivsToGrant, PrivsToRevoke
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves harvester privileges of given group from onezone.
%% @end
%%--------------------------------------------------------------------
-spec harvester_get_group_privileges(Config :: term(), HarvesterId :: od_harvester:id(),
    GroupId :: od_group:id()) -> {ok, [privileges:harvester_privilege()]}.
harvester_get_group_privileges(Config, HarvesterId, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, get_group_privileges, [?ROOT, HarvesterId, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Sets privileges of a group in a harvester.
%% @end
%%--------------------------------------------------------------------
-spec harvester_set_group_privileges(Config :: term(), HarvesterId :: od_harvester:id(),
    GroupId :: od_group:id(), PrivsToGrant :: [privileges:harvester_privilege()],
    PrivsToRevoke :: [privileges:harvester_privilege()]) -> ok.
harvester_set_group_privileges(Config, HarvesterId, GroupId, PrivsToGrant, PrivsToRevoke) ->
    ?assertMatch(ok, call_oz(Config, harvester_logic, update_group_privileges, [
        ?ROOT, HarvesterId, GroupId, PrivsToGrant, PrivsToRevoke
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Creates a user invite token to given harvester.
%% @end
%%--------------------------------------------------------------------
-spec harvester_invite_user_token(Config :: term(),
    Client :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    {ok, tokens:token()}.
harvester_invite_user_token(Config, Client, HarvesterId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, create_user_invite_token, [Client, HarvesterId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a group invite token to given harvester.
%% @end
%%--------------------------------------------------------------------
-spec harvester_invite_group_token(Config :: term(),
    Client :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    {ok, tokens:token()}.
harvester_invite_group_token(Config, Client, HarvesterId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, create_group_invite_token, [Client, HarvesterId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a space invite token to given harvester.
%% @end
%%--------------------------------------------------------------------
-spec harvester_invite_space_token(Config :: term(),
    Client :: aai:auth(), HarvesterId :: od_harvester:id()) ->
    {ok, tokens:token()}.
harvester_invite_space_token(Config, Client, HarvesterId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, create_space_invite_token, [Client, HarvesterId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Checks if given harvester has given effective user.
%% @end
%%--------------------------------------------------------------------
-spec harvester_has_effective_user(Config :: term(), HarvesterId :: od_harvester:id(),
    UserId :: od_user:id()) -> boolean().
harvester_has_effective_user(Config, HarvesterId, UserId) ->
    {ok, EffUsers} = ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, get_eff_users, [?ROOT, HarvesterId]
    )),
    lists:member(UserId, EffUsers).


%%--------------------------------------------------------------------
%% @doc
%% Removes group from harvester in onezone.
%% @end
%%--------------------------------------------------------------------
-spec harvester_remove_group(Config :: term(), HarvesterId :: od_harvester:id(),
    GroupId :: od_group:id()) -> ok.
harvester_remove_group(Config, HarvesterId, GroupId) ->
    ?assertMatch(ok, call_oz(
        Config, harvester_logic, remove_group, [?ROOT, HarvesterId, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates index in given harvester.
%% @end
%%--------------------------------------------------------------------
-spec harvester_create_index(Config :: term(), HarvesterId :: od_harvester:id(),
    Data :: map()) -> ok.
harvester_create_index(Config, HarvesterId, Data) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, create_index, [?ROOT, HarvesterId, Data]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves index from given harvester.
%% @end
%%--------------------------------------------------------------------
-spec harvester_get_index(Config :: term(), HarvesterId :: od_harvester:id(),
    IndexId :: od_harvester:index_id()) -> ok.
harvester_get_index(Config, HarvesterId, IndexId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, get_index, [?ROOT, HarvesterId, IndexId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves index stats from given harvester.
%% @end
%%--------------------------------------------------------------------
-spec harvester_get_index_stats(Config :: term(), HarvesterId :: od_harvester:id(),
    IndexId :: od_harvester:index_id()) -> ok.
harvester_get_index_stats(Config, HarvesterId, IndexId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, get_index_stats, [?ROOT, HarvesterId, IndexId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Submits entry in given harvester as provider.
%% @end
%%--------------------------------------------------------------------
-spec harvester_submit_entry(Config :: term(), ProviderId :: od_provider:id(),
    HarvesterId :: od_harvester:id(), FileId :: file_id:objectid(), Data :: map()) -> ok.
harvester_submit_entry(Config, ProviderId, HarvesterId, FileId, Data) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, submit_entry, [?PROVIDER(ProviderId), HarvesterId, FileId, Data]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Deletes entry in given harvester as provider.
%% @end
%%--------------------------------------------------------------------
-spec harvester_delete_entry(Config :: term(), ProviderId :: od_provider:id(),
    HarvesterId :: od_harvester:id(), FileId :: file_id:objectid(), Data :: map()) -> ok.
harvester_delete_entry(Config, ProviderId, HarvesterId, FileId, Data) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, delete_entry, [?PROVIDER(ProviderId), HarvesterId, FileId, Data]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Submits batch in given harvester as provider.
%% @end
%%--------------------------------------------------------------------
-spec harvester_submit_batch(Config :: term(), ProviderId :: od_provider:id(),
    HarvesterId :: od_harvester:id(), Indices :: od_harvester:indices(),
    SpaceId :: od_space:id(), Batch :: od_harvester:batch(), MaxSeq :: integer(), MaxStreamSeq :: integer()) -> ok.
harvester_submit_batch(Config, ProviderId, HarvesterId, Indices, SpaceId, Batch, MaxStreamSeq, MaxSeq) ->
    ?assertMatch({ok, _}, call_oz(
        Config, harvester_logic, submit_batch,
        [?PROVIDER(ProviderId), HarvesterId, Indices, SpaceId, Batch, MaxStreamSeq, MaxSeq]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of all clusters in onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_clusters(Config :: term()) -> {ok, [od_cluster:id()]}.
list_clusters(Config) ->
    ?assertMatch({ok, _}, call_oz(Config, cluster_logic, list, [
        ?ROOT
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves cluster data from onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_cluster(Config :: term(), ClusterId :: od_cluster:id()) ->
    {ok, #od_cluster{}}.
get_cluster(Config, ClusterId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, cluster_logic, get, [?ROOT, ClusterId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Updates a cluster
%% @end
%%--------------------------------------------------------------------
-spec update_cluster(Config :: term(), ClusterId :: od_cluster:id(),
    Data :: map()) -> ok.
update_cluster(Config, ClusterId, Data) ->
    ?assertMatch(ok, call_oz(Config, cluster_logic, update, [
        ?ROOT, ClusterId, Data
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given cluster from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_cluster(Config :: term(), ClusterId :: od_cluster:id()) -> ok.
delete_cluster(Config, ClusterId) ->
    ?assertMatch(ok, call_oz(Config, cluster_logic, delete, [?ROOT, ClusterId])).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves groups belonging to cluster from onezone.
%% @end
%%--------------------------------------------------------------------
-spec cluster_get_groups(Config :: term(), ClusterId :: od_cluster:id()) ->
    {ok, [od_group:id()]}.
cluster_get_groups(Config, ClusterId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, cluster_logic, get_groups, [?ROOT, ClusterId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves users belonging to cluster from onezone.
%% @end
%%--------------------------------------------------------------------
-spec cluster_get_users(Config :: term(), ClusterId :: od_cluster:id()) ->
    {ok, [od_user:id()]}.
cluster_get_users(Config, ClusterId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, cluster_logic, get_users, [?ROOT, ClusterId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Adds a user to a cluster.
%% @end
%%--------------------------------------------------------------------
-spec cluster_add_user(Config :: term(), ClusterId :: od_cluster:id(),
    UserId :: od_user:id()) -> {ok, od_user:id()}.
cluster_add_user(Config, ClusterId, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, cluster_logic, add_user, [
        ?ROOT, ClusterId, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Removes user from cluster.
%% @end
%%--------------------------------------------------------------------
-spec cluster_remove_user(Config :: term(), ClusterId :: od_cluster:id(),
    UserId :: od_user:id()) -> {ok, od_user:id()}.
cluster_remove_user(Config, ClusterId, UserId) ->
    ?assertMatch(ok, call_oz(Config, cluster_logic, remove_user, [
        ?ROOT, ClusterId, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets privileges of a user in a cluster
%% @end
%%--------------------------------------------------------------------
-spec cluster_set_user_privileges(Config :: term(),
    ClusterId :: od_cluster:id(), UserId :: od_user:id(),
    PrivsToGrant :: [privileges:cluster_privilege()],
    PrivsToRevoke :: [privileges:cluster_privilege()]) -> ok.
cluster_set_user_privileges(Config, ClusterId, UserId, PrivsToGrant, PrivsToRevoke) ->
    ?assertMatch(ok, call_oz(Config, cluster_logic,
        update_user_privileges, [?ROOT, ClusterId, UserId, PrivsToGrant, PrivsToRevoke]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves privileges of user belonging to cluster from onezone.
%% @end
%%--------------------------------------------------------------------
-spec cluster_get_user_privileges(Config :: term(),
    ClusterId :: od_service:id(), UserId :: od_user:id()) -> {ok, [atom()]}.
cluster_get_user_privileges(Config, ClusterId, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, cluster_logic, get_user_privileges, [?ROOT, ClusterId, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a user invite token to given cluster.
%% @end
%%--------------------------------------------------------------------
-spec cluster_invite_user_token(Config :: term(),
    Client :: aai:auth(), ClusterId :: od_cluster:id()) ->
    {ok, tokens:token()}.
cluster_invite_user_token(Config, Client, ClusterId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, cluster_logic, create_user_invite_token, [Client, ClusterId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a group invite token to given cluster.
%% @end
%%--------------------------------------------------------------------
-spec cluster_invite_group_token(Config :: term(),
    Client :: aai:auth(), ClusterId :: od_cluster:id()) ->
    {ok, tokens:token()}.
cluster_invite_group_token(Config, Client, ClusterId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, cluster_logic, create_group_invite_token, [Client, ClusterId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Adds a group to a cluster.
%% @end
%%--------------------------------------------------------------------
-spec cluster_add_group(Config :: term(), ClusterId :: od_cluster:id(),
    GroupId :: od_group:id()) -> {ok, od_group:id()}.
cluster_add_group(Config, ClusterId, GroupId) ->
    ?assertMatch({ok, _}, call_oz(Config, cluster_logic, add_group, [
        ?ROOT, ClusterId, GroupId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Removes group from cluster.
%% @end
%%--------------------------------------------------------------------
-spec cluster_remove_group(Config :: term(), ClusterId :: od_cluster:id(),
    GroupId :: od_group:id()) -> {ok, od_user:id()}.
cluster_remove_group(Config, ClusterId, GroupId) ->
    ?assertMatch(ok, call_oz(Config, cluster_logic, remove_group, [
        ?ROOT, ClusterId, GroupId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets privileges of a group in a cluster.
%% @end
%%--------------------------------------------------------------------
-spec cluster_set_group_privileges(Config :: term(),
    ClusterId :: od_cluster:id(), GroupId :: od_user:id(),
    Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:cluster_privilege()]) -> ok.
cluster_set_group_privileges(Config, ClusterId, GroupId, Operation, Privs) ->
    ?assertMatch(ok, call_oz(Config, cluster_logic,
        update_group_privileges, [?ROOT, ClusterId, GroupId, Operation, Privs]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves privileges of group belonging to cluster from onezone.
%% @end
%%--------------------------------------------------------------------
-spec cluster_get_group_privileges(Config :: term(),
    ClusterId :: od_service:id(), GroupId :: od_user:id()) -> {ok, [atom()]}.
cluster_get_group_privileges(Config, ClusterId, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, cluster_logic, get_group_privileges, [?ROOT, ClusterId, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Sets privileges for group's user with ?ROOT auth.
%% @end
%%--------------------------------------------------------------------
-spec group_set_user_privileges(Config :: term(), GroupId :: od_group:id(),
    UserId :: od_user:id(), PrivsToGrant :: [privileges:group_privilege()],
    PrivsToRevoke :: [privileges:group_privilege()]) -> ok.
group_set_user_privileges(Config, GroupId, UserId, PrivsToGrant, PrivsToRevoke) ->
    group_set_user_privileges(Config, ?ROOT, GroupId, UserId, PrivsToGrant, PrivsToRevoke).


%%--------------------------------------------------------------------
%% @doc
%% Sets privileges for group's user.
%% @end
%%--------------------------------------------------------------------
-spec group_set_user_privileges(Config :: term(), aai:auth(),
    GroupId :: od_group:id(), UserId :: od_user:id(),
    PrivsToGrant :: [privileges:group_privilege()],
    PrivsToRevoke :: [privileges:group_privilege()]) -> ok.
group_set_user_privileges(Config, Client, GroupId, UserId, PrivsToGrant, PrivsToRevoke) ->
    ?assertMatch(ok, call_oz(Config, group_logic, update_user_privileges, [
        Client, GroupId, UserId, PrivsToGrant, PrivsToRevoke
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets privileges for subgroup.
%% @end
%%--------------------------------------------------------------------
-spec group_set_group_privileges(Config :: term(), GroupId :: od_group:id(),
    ChildGroupId :: od_group:id(), Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:group_privilege()]) -> ok.
group_set_group_privileges(Config, GroupId, ChildGroupId, Operation, Privs) ->
    ?assertMatch(ok, call_oz(Config, group_logic, update_child_privileges, [
        ?ROOT, GroupId, ChildGroupId, Operation, Privs
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Creates a space in onezone.
%% @end
%%--------------------------------------------------------------------
-spec group_create_space(Config :: term(), GroupId :: od_group:id(),
    NameOrData :: od_space:name() | #{}) -> {ok, od_space:id()}.
group_create_space(Config, GroupId, NameOrData) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, create_space, [?ROOT, GroupId, NameOrData]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a harvester for group.
%% @end
%%--------------------------------------------------------------------
-spec group_create_harvester(Config :: term(), GroupId :: od_group:id(),
    Data :: #{}) -> {ok, od_harvester:id()}.
group_create_harvester(Config, GroupId, Data) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, create_harvester, [?ROOT, GroupId, Data]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Removes group from space in onezone.
%% @end
%%--------------------------------------------------------------------
-spec space_remove_group(Config :: term(), SpaceId :: od_space:id(),
    GroupId :: od_group:id()) -> ok.
space_remove_group(Config, SpaceId, GroupId) ->
    ?assertMatch(ok, call_oz(
        Config, space_logic, remove_group, [?ROOT, SpaceId, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Submits given batch to harvesters given in Destination.
%% @end
%%--------------------------------------------------------------------
-spec space_harvest_metadata(Config :: term(),
    ClientProviderId :: od_provider:id(), SpaceId :: od_space:id(),
    Destination :: map(), MaxStreamSeq :: non_neg_integer(),
    MaxSeq :: non_neg_integer(), Batch :: map()) ->
    {ok, map()} | {error, term()}.
space_harvest_metadata(Config, ClientProviderId, SpaceId, Destination, MaxStreamSeq, MaxSeq, Batch) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, harvest_metadata, [?PROVIDER(ClientProviderId), SpaceId, Destination, MaxStreamSeq, MaxSeq, Batch]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a group invite token to given group.
%% @end
%%--------------------------------------------------------------------
-spec group_invite_group_token(Config :: term(),
    Client :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, tokens:token()}.
group_invite_group_token(Config, Client, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, create_group_invite_token, [Client, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a group invite token to given user.
%% @end
%%--------------------------------------------------------------------
-spec group_invite_user_token(Config :: term(),
    Client :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, tokens:token()}.
group_invite_user_token(Config, Client, GroupId) -> ?assertMatch({ok, _}, call_oz(
    Config, group_logic, create_user_invite_token, [Client, GroupId]
)).


%%--------------------------------------------------------------------
%% @doc
%% Creates a storage in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_storage(Config :: term(), Client :: aai:auth(),
    Name :: od_storage:name()) -> {ok, od_storage:id()}.
create_storage(Config, Client, Name) ->
    ?assertMatch({ok, _}, call_oz(
        Config, storage_logic, create, [Client, Name]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a storage in onezone with given id.
%% @end
%%--------------------------------------------------------------------
-spec create_storage(Config :: term(), Client :: aai:auth(),
    od_storage:id(), od_storage:name()) -> {ok, od_storage:id()}.
create_storage(Config, Client, Id, Name) ->
    ?assertMatch({ok, _}, call_oz(
        Config, storage_logic, create, [Client, Id, Name]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates an imported storage in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_imported_storage(Config :: term(), Client :: aai:auth(),
    od_storage:name()) -> {ok, od_storage:id()}.
create_imported_storage(Config, Client, Name) ->
    ?assertMatch({ok, _}, call_oz(
        Config, storage_logic, create, [Client, #{<<"name">> => Name, <<"imported">> => true}]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves storage data from onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_storage(Config :: term(), StorageId :: od_storage:id()) ->
    {ok, #od_storage{}}.
get_storage(Config, StorageId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, storage_logic, get, [?ROOT, StorageId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Updates storage.
%% @end
%%--------------------------------------------------------------------
-spec update_storage(Config :: term(), StorageId :: od_storage:id(),
    Data :: map()) -> ok.
update_storage(Config, StorageId, Data) ->
    ?assertMatch(ok, call_oz(
        Config, storage_logic, update, [?ROOT, StorageId, Data]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given storage from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_storage(Config :: term(), StorageId :: od_storage:id()) -> ok.
delete_storage(Config, StorageId) ->
    ?assertMatch(ok, call_oz(
        Config, storage_logic, delete, [?ROOT, StorageId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Asserts that an invite token's usage limit was reached or not
%% (depending on the Expected flag).
%% @end
%%--------------------------------------------------------------------
-spec assert_invite_token_usage_limit_reached(Config :: term(), Expected :: boolean(),
    TokenId :: od_token:id()) -> ok.
assert_invite_token_usage_limit_reached(Config, Expected, TokenId) ->
    {ok, #document{value = #od_token{
        metadata = Metadata
    }}} = ?assertMatch({ok, _}, call_oz(Config, od_token, get, [TokenId])),
    UsageCount = maps:get(<<"usageCount">>, Metadata, 0),
    IsReached = case maps:get(<<"usageLimit">>, Metadata, <<"infinity">>) of
        <<"infinity">> ->
            false;
        UsageLimit when is_integer(UsageLimit) ->
            UsageCount >= UsageLimit
    end,
    ?assertEqual(Expected, IsReached).


%%--------------------------------------------------------------------
%% @doc
%% Verifies the token and returns resulting auth object on success. Default
%% auth_ctx is used, which will cause verification failure if any caveat that
%% requires certain context is included in the token.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_token(Config :: term(), tokens:token() | tokens:serialized()) ->
    {true, aai:auth()} | errors:error().
authenticate_by_token(Config, Token) ->
    authenticate_by_token(Config, Token, #auth_ctx{}).


%%--------------------------------------------------------------------
%% @doc
%% Verifies the token against given auth_ctx and returns resulting auth object
%% on success.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_token(Config :: term(), tokens:token() | tokens:serialized(), aai:auth_ctx()) ->
    {true, aai:auth()} | errors:error().
authenticate_by_token(Config, Token, AuthCtx) ->
    call_oz(Config, token_auth, authenticate, [Token, AuthCtx]).


-spec acquire_temporary_token(Config :: term(), aai:subject()) -> tokens:serialized().
acquire_temporary_token(Config, Subject = ?SUB(SubType, SubId)) ->
    {ok, Cached} = simple_cache:get({temp_token, Subject}, fun() ->
        Data = #{<<"caveats">> => [#cv_time{valid_until = timestamp_seconds(Config) + 36000}]},
        Fun = case SubType of
            user -> create_user_temporary_token;
            ?ONEPROVIDER -> create_provider_temporary_token
        end,
        {ok, Token} = call_oz(Config, token_logic, Fun, [?ROOT, SubId, Data]),
        {ok, Serialized} = tokens:serialize(Token),
        {true, Serialized}
    end),
    Cached.


-spec create_legacy_access_token(Config :: term(), aai:subject()) -> tokens:token().
create_legacy_access_token(Config, Subject) ->
    TokenId = datastore_key:new(),
    Secret = tokens:generate_secret(),
    NamedToken = #od_token{
        name = datastore_key:new(),
        version = 1,
        subject = Subject,
        type = ?ACCESS_TOKEN,
        caveats = [],
        metadata = #{},
        secret = Secret
    },
    call_oz(Config, od_token, create, [#document{key = TokenId, value = NamedToken}]),
    call_oz(Config, od_token, named_token_to_token, [TokenId, NamedToken]).


-spec confine_token_with_legacy_auth_none_caveat(tokens:token()) -> tokens:token().
confine_token_with_legacy_auth_none_caveat(Token) ->
    Token#token{
        macaroon = macaroon:add_first_party_caveat(
            Token#token.macaroon,
            <<"authorization = none">>
        )
    }.


%%--------------------------------------------------------------------
%% @doc
%% Acquires a Onezone gui token issued for the session denoted by given cookie.
%% @end
%%--------------------------------------------------------------------
-spec request_gui_token(Config :: term(), Cookie :: binary()) ->
    {ok, Token :: binary()} | {error, term()}.
request_gui_token(Config, Cookie) ->
    request_gui_token(Config, Cookie, ?OZ_WORKER_GUI, ?ONEZONE_CLUSTER_ID).


%%--------------------------------------------------------------------
%% @doc
%% Acquires a gui token issued for the session denoted by given cookie,
%% for use by given service (defined via cluster type and id).
%% @end
%%--------------------------------------------------------------------
-spec request_gui_token(Config :: term(), Cookie :: binary(), onedata:gui(), od_cluster:id()) ->
    {ok, Token :: binary()} | {error, term()}.
request_gui_token(Config, Cookie, GuiType, ClusterId) ->
    GuiPrefix = onedata:gui_prefix(GuiType),
    Result = http_client:post(
        oz_url(Config, str_utils:format_bin("/~s/~s/gui-preauthorize", [GuiPrefix, ClusterId])),
        #{
            ?HDR_CONTENT_TYPE => <<"application/json">>,
            ?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>
        },
        <<"">>,
        [{ssl_options, [{cacerts, gui_ca_certs(Config)}]}]
    ),
    case Result of
        {ok, 200, _, Response} ->
            #{<<"token">> := Token} = json_utils:decode(Response),
            {ok, Token};
        {ok, _, _, Response} ->
            #{<<"error">> := Error} = json_utils:decode(Response),
            errors:from_json(Error)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Deletes all entities from onezone
%% (users, groups, spaces, shares, providers).
%% NOTE: Does not remove predefined groups! Use remove_all_entities/2 for that.
%% @end
%%--------------------------------------------------------------------
-spec delete_all_entities(Config :: term()) -> ok.
delete_all_entities(Config) ->
    delete_all_entities(Config, false).


%%--------------------------------------------------------------------
%% @doc
%% Deletes all entities from onezone
%% (users, groups, spaces, shares, providers).
%% RemovePredefinedGroups decides if predefined groups should be removed too.
%% @end
%%--------------------------------------------------------------------
-spec delete_all_entities(Config :: term(),
    RemovePredefinedGroups :: boolean()) -> ok.
delete_all_entities(Config, RemovePredefinedGroups) ->
    {ok, Providers} = list_providers(Config),
    {ok, Shares} = list_shares(Config),
    {ok, Spaces} = list_spaces(Config),
    {ok, Handles} = list_handles(Config),
    {ok, HServices} = list_handle_services(Config),
    {ok, Groups} = list_groups(Config),
    {ok, Users} = list_users(Config),
    {ok, Harvesters} = list_harvesters(Config),
    lists_utils:pforeach(fun(PId) -> delete_provider(Config, PId) end, Providers),
    lists_utils:pforeach(fun(HId) -> delete_handle(Config, HId) end, Handles),
    lists_utils:pforeach(fun(ShId) -> delete_share(Config, ShId) end, Shares),
    lists_utils:pforeach(fun(SpId) -> delete_space(Config, SpId) end, Spaces),
    lists_utils:pforeach(fun(HSId) -> delete_handle_service(Config, HSId) end, HServices),
    lists_utils:pforeach(fun(HId) -> delete_harvester(Config, HId) end, Harvesters),
    % Clusters and storages are removed together with providers

    % Check if predefined groups should be removed too.
    GroupsToDelete = case RemovePredefinedGroups of
        true ->
            Groups;
        false ->
            % Filter out predefined groups
            PredefinedGroupsMapping = get_env(Config, predefined_groups),
            PredefinedGroups = [Id || #{id := Id} <- PredefinedGroupsMapping],
            Groups -- PredefinedGroups
    end,
    lists_utils:pforeach(fun(GroupId) -> mark_group_protected(Config, GroupId, false) end, GroupsToDelete),
    lists_utils:pforeach(fun(GroupId) -> delete_group(Config, GroupId) end, GroupsToDelete),

    lists_utils:pforeach(fun(UId) -> delete_user(Config, UId) end, Users).


%%--------------------------------------------------------------------
%% @doc
%% Creates a group for user which belongs to a group, which belongs to
%% another group.
%% The structure looks as follows: User -> G1 -> G2 -> G3
%% @end
%%--------------------------------------------------------------------
-spec create_3_nested_groups(Config :: term(), TestUser :: od_user:id()) -> ok.
create_3_nested_groups(Config, TestUser) ->
    create_3_nested_groups(Config, TestUser, <<"group">>, <<"group">>, <<"group">>).


%%--------------------------------------------------------------------
%% @doc
%% Creates a group for user which belongs to a group, which belongs to
%% another group. Allows to specify group names.
%% The structure looks as follows: User -> G1 -> G2 -> G3
%% @end
%%--------------------------------------------------------------------
-spec create_3_nested_groups(Config :: term(), TestUser :: od_user:id(),
    BotGrName :: binary(), MidGrName :: binary(), TopGrName :: binary()) -> ok.
create_3_nested_groups(Config, TestUser, BotGrName, MidGrName, TopGrName) ->
    {ok, BottomGroup} = create_group(Config, ?USER(TestUser), BotGrName),
    % Dummy user will be used only to create groups
    {ok, MiddleGroup} = create_group(Config, ?ROOT, MidGrName),
    {ok, TopGroup} = create_group(Config, ?ROOT, TopGrName),
    {ok, BottomGroup} = group_add_group(Config, MiddleGroup, BottomGroup),
    {ok, MiddleGroup} = group_add_group(Config, TopGroup, MiddleGroup),
    {BottomGroup, MiddleGroup, TopGroup}.


%%--------------------------------------------------------------------
%% @doc
%% Creates three spaces and grant support for them on behalf of given provider.
%% @end
%%--------------------------------------------------------------------
-spec create_and_support_3_spaces(Config :: term(),
    ProviderId :: od_provider:id()) ->
    [{SpaceId :: od_space:id(), SpaceName :: od_space:name()}].
create_and_support_3_spaces(Config, ProviderId) ->
    MinimumSupportSize = minimum_support_size(Config),
    S1Name = <<"Space 1">>,
    S2Name = <<"Space 2">>,
    S3Name = <<"Space 3">>,
    {ok, S1} = create_space(Config, ?ROOT, S1Name),
    {ok, S2} = create_space(Config, ?ROOT, S2Name),
    {ok, S3} = create_space(Config, ?ROOT, S3Name),
    % Support them by the provider
    {ok, S1} = support_space(Config, ProviderId, S1, MinimumSupportSize),
    {ok, S2} = support_space(Config, ProviderId, S2, MinimumSupportSize),
    {ok, S3} = support_space(Config, ProviderId, S3, MinimumSupportSize),
    [
        {S1, S1Name},
        {S2, S2Name},
        {S3, S3Name}
    ].


%%--------------------------------------------------------------------
%% @doc
%% Return minimum support size for space, as specified in oz_worker env.
%% @end
%%--------------------------------------------------------------------
-spec minimum_support_size(Config :: term()) -> integer().
minimum_support_size(Config) ->
    get_env(Config, minimum_space_support_size).


%%--------------------------------------------------------------------
%% @doc
%% Mocks harvester plugins on all nodes of onezone.
%% @end
%%--------------------------------------------------------------------
mock_harvesting_backends(Config, Backends) when is_list(Backends) ->
    Nodes = ?OZ_NODES(Config),
    lists:foreach(fun(Plugin) -> mock_harvesting_backends(Config, Nodes, Plugin) end, Backends),
    test_utils:mock_new(Nodes, onezone_plugins),

    test_utils:mock_expect(Nodes, onezone_plugins, get_plugins,
        fun(Type) -> Backends ++ meck:passthrough([Type]) end),
    Config;
mock_harvesting_backends(Config, Plugin) ->
    mock_harvesting_backends(Config, [Plugin]).

%%--------------------------------------------------------------------
%% @doc
%% Creates mocked harvester plugin on all nodes of onezone.
%% @end
%%--------------------------------------------------------------------
-spec mock_harvesting_backends(Config :: term(), Nodes :: list(), PluginName :: atom()) -> ok.
mock_harvesting_backends(Config, Nodes, BackendName) ->
    test_utils:mock_new(Nodes, BackendName, [non_strict]),
    test_utils:mock_expect(Nodes, BackendName, type, fun() -> harvesting_backend end),
    test_utils:mock_expect(Nodes, BackendName, get_name, fun() -> atom_to_binary(BackendName, utf8) end),
    test_utils:mock_expect(Nodes, BackendName, ping,
        fun(?HARVESTER_ENDPOINT1) -> ok;
            (?HARVESTER_ENDPOINT2) -> ok;
            (Endpoint) ->
                case get_env(Config, default_harvesting_backend_endpoint) of
                    Endpoint -> ok;
                    _ -> ?ERROR_TEMPORARY_FAILURE
                end
        end),
    test_utils:mock_expect(Nodes, BackendName, submit_batch, fun(_, HarvesterId, Indices, Batch) ->
        FirstSeq = maps:get(<<"seq">>, lists:nth(1, Batch)),
        {LastSeq, ErrorSeq} = lists:foldl(
            fun(BatchEntry, {A, undefined}) ->
                CurrentSeq = maps:get(<<"seq">>, BatchEntry),
                case maps:get(<<"operation">>, BatchEntry) of
                    fail -> {A, {CurrentSeq, <<"error_seq">>}};
                    _ -> {CurrentSeq, undefined}
                end;
                (_, Acc) -> Acc
            end, {undefined, undefined}, Batch),
        Res = case ErrorSeq of
            undefined -> ok;
            {FailedSeq, ErrorMsg} -> {error, LastSeq, FailedSeq, ErrorMsg}
        end,
        {ok, lists:map(fun(Index) ->
            case harvester_get_index(Config, HarvesterId, Index) of
                {ok, #harvester_index{name = <<"fail">>}} -> {Index, {error, undefined, FirstSeq, <<"error_index">>}};
                _ -> {Index, Res}
            end
        end, maps:keys(Indices))}
    end),
    test_utils:mock_expect(Nodes, BackendName, create_index, fun(_, _, _, _) -> ok end),
    test_utils:mock_expect(Nodes, BackendName, delete_index, fun(_, _) -> ok end),
    test_utils:mock_expect(Nodes, BackendName, query_index, fun(_, _, _) -> {ok, ?HARVESTER_MOCKED_QUERY_DATA_MAP} end),
    test_utils:mock_expect(Nodes, BackendName, query_validator, fun() -> ?HARVESTER_BACKEND:query_validator() end).


%%--------------------------------------------------------------------
%% @doc
%% Unmocks harvester plugins on all nodes of onezone.
%% @end
%%--------------------------------------------------------------------
-spec unmock_harvesting_backends(Config :: term(), Plugins :: atom() | list()) -> ok.
unmock_harvesting_backends(Config, PluginName) when is_atom(PluginName) ->
    unmock_harvesting_backends(Config, [PluginName]);

unmock_harvesting_backends(Config, Plugins) ->
    test_utils:mock_unload(?OZ_NODES(Config), onezone_plugins),
    lists:foreach(fun(PluginName) -> test_utils:mock_unload(?OZ_NODES(Config), PluginName) end, Plugins).


%%--------------------------------------------------------------------
%% @doc
%% Mocks handle proxy on all nodes of onezone.
%% @end
%%--------------------------------------------------------------------
-spec mock_handle_proxy(Config :: term()) -> ok.
mock_handle_proxy(Config) ->
    Nodes = ?OZ_NODES(Config),
    ok = test_utils:mock_new(Nodes, handle_proxy_client, [passthrough]),
    ok = test_utils:mock_expect(Nodes, handle_proxy_client, put,
        fun(_, <<"/handle?hndl=", Hndl/binary>>, _, _) ->
            ResponseBody = json_utils:encode(#{
                <<"handle">> => <<"http://hndl.service.example.com/", Hndl/binary>>
            }),
            {ok, 201, #{}, ResponseBody}
        end),
    ok = test_utils:mock_expect(Nodes, handle_proxy_client, patch,
        fun(_, <<"/handle", _/binary>>, _, _) ->
            {ok, 204, #{}, <<"">>}
        end),
    ok = test_utils:mock_expect(Nodes, handle_proxy_client, delete,
        fun(_, <<"/handle", _/binary>>, _, _) ->
            {ok, 200, #{}, <<"">>}
        end).


%%--------------------------------------------------------------------
%% @doc
%% Unmocks handle proxy on all nodes of onezone.
%% @end
%%--------------------------------------------------------------------
-spec unmock_handle_proxy(Config :: term()) -> ok.
unmock_handle_proxy(Config) ->
    test_utils:mock_unload(?OZ_NODES(Config), handle_proxy_client).


%%--------------------------------------------------------------------
%% @doc
%% Returns the current time.
%% @end
%%--------------------------------------------------------------------
-spec timestamp_seconds(Config :: term()) -> time_utils:seconds().
timestamp_seconds(Config) ->
    call_oz(Config, time_utils, timestamp_seconds, []).


%%--------------------------------------------------------------------
%% @doc
%% Mocks the time - stops the clock at one value and allows to manually
%% simulate time passing.
%% @end
%%--------------------------------------------------------------------
-spec mock_time(Config :: term()) -> ok.
mock_time(Config) ->
    simulate_time_passing(Config, 0),
    ok = test_utils:mock_new(?OZ_NODES(Config), time_utils, [passthrough]),
    ok = test_utils:mock_expect(?OZ_NODES(Config), time_utils, timestamp_seconds, fun() ->
        oz_worker:get_env(mocked_time, ?TIME_MOCK_STARTING_TIMESTAMP)
    end).


%%--------------------------------------------------------------------
%% @doc
%% Clears the time mock.
%% @end
%%--------------------------------------------------------------------
-spec unmock_time(Config :: term()) -> ok.
unmock_time(Config) ->
    ok = test_utils:mock_unload(?OZ_NODES(Config), time_utils).


%%--------------------------------------------------------------------
%% @doc
%% Returns the current timestamp indicated by the mocked clock.
%% @end
%%--------------------------------------------------------------------
-spec get_mocked_time(Config :: term()) -> non_neg_integer().
get_mocked_time(Config) ->
    get_env(Config, mocked_time, ?TIME_MOCK_STARTING_TIMESTAMP).


%%--------------------------------------------------------------------
%% @doc
%% Modifies the value returned by time mock by given amount of seconds.
%% @end
%%--------------------------------------------------------------------
-spec simulate_time_passing(Config :: term(), Seconds :: non_neg_integer()) -> ok.
simulate_time_passing(Config, Seconds) ->
    set_env(Config, mocked_time, get_mocked_time(Config) + Seconds).


%%--------------------------------------------------------------------
%% @doc
%% Returns the list of DER encoded ca certs used in gui server.
%% @end
%%--------------------------------------------------------------------
-spec gui_ca_certs(Config :: term()) -> [public_key:der_encoded()].
gui_ca_certs(Config) ->
    call_oz(Config, https_listener, get_cert_chain_pems, []).


%%--------------------------------------------------------------------
%% @doc
%% Waits for entity graph to be reconciled, with 60 retries.
%% @end
%%--------------------------------------------------------------------
-spec ensure_entity_graph_is_up_to_date(Config :: term()) -> boolean().
ensure_entity_graph_is_up_to_date(Config) ->
    ensure_entity_graph_is_up_to_date(Config, 60).


%%--------------------------------------------------------------------
%% @doc
%% Waits for entity graph to be reconciled, with given number of retries.
%% @end
%%--------------------------------------------------------------------
-spec ensure_entity_graph_is_up_to_date(Config :: term(), Retries :: non_neg_integer()) ->
    boolean().
ensure_entity_graph_is_up_to_date(Config, Retries) ->
    ?assertMatch(true, call_oz(Config, entity_graph, ensure_up_to_date, []), Retries).


%%--------------------------------------------------------------------
%% @doc
%% Turns the basicAuth (login with username & password) on/off by overwriting
%% auth.config.
%% @end
%%--------------------------------------------------------------------
-spec toggle_basic_auth(Config :: term(), boolean()) -> ok.
toggle_basic_auth(Config, Flag) ->
    AuthConfigData = #{
        basicAuthConfig => #{
            enabled => Flag
        },
        supportedIdps => [
            {basicAuth, #{
                protocol => basicAuth
            }}
        ]
    },
    overwrite_auth_config(Config, AuthConfigData).


%%--------------------------------------------------------------------
%% @doc
%% Returns parsed auth.config.
%% @end
%%--------------------------------------------------------------------
-spec read_auth_config(Config :: term()) -> auth_config:config_v2_or_later().
read_auth_config(Config) ->
    AuthConfigPath = get_env(Config, auth_config_file),
    {ok, [AuthConfig]} = call_oz(Config, file, consult, [AuthConfigPath]),
    AuthConfig.


%%--------------------------------------------------------------------
%% @doc
%% Overwrites given config file (specified by OZ env) with given data.
%% @end
%%--------------------------------------------------------------------
-spec overwrite_config(TestConfig :: term(), ConfigFileEnv :: atom(), ConfigData :: term()) -> ok.
overwrite_config(TestConfig, ConfigFileEnv, ConfigData) ->
    ConfigPath = get_env(TestConfig, ConfigFileEnv),
    rpc:multicall(?OZ_NODES(TestConfig), file, write_file, [
        ConfigPath, io_lib:format("~tp.~n", [ConfigData])
    ]),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Overwrites auth.config with given data.
%% @end
%%--------------------------------------------------------------------
-spec overwrite_auth_config(TestConfig :: term(), auth_config:config_v2_or_later()) -> ok.
overwrite_auth_config(TestConfig, AuthConfigData) ->
    overwrite_config(TestConfig, auth_config_file, AuthConfigData#{version => ?CURRENT_CONFIG_VERSION}),
    rpc:multicall(?OZ_NODES(TestConfig), auth_config, force_auth_config_reload, []),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Overwrites test.auth.config with given data.
%% @end
%%--------------------------------------------------------------------
-spec overwrite_test_auth_config(TestConfig :: term(), auth_config:config_v2_or_later()) -> ok.
overwrite_test_auth_config(TestConfig, AuthConfigData) ->
    overwrite_config(TestConfig, test_auth_config_file, AuthConfigData#{version => ?CURRENT_CONFIG_VERSION}),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Overwrites compatibility registry with given data (first it is serialized to JSON).
%% @end
%%--------------------------------------------------------------------
overwrite_compatibility_registry(TestConfig, Registry) ->
    {ok, RegistryPath} = call_oz(TestConfig, application, get_env, [ctool, compatibility_registry_path]),
    {ok, DefaultRegistryPath} = call_oz(TestConfig, application, get_env, [ctool, default_compatibility_registry]),
    rpc:multicall(?OZ_NODES(TestConfig), file, write_file, [RegistryPath, json_utils:encode(Registry)]),
    rpc:multicall(?OZ_NODES(TestConfig), file, write_file, [DefaultRegistryPath, json_utils:encode(Registry)]),
    rpc:multicall(?OZ_NODES(TestConfig), compatibility, clear_registry_cache, []),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Creates a dummy GUI package on the test master node and returns path to the
%% package tarball.
%% @end
%%--------------------------------------------------------------------
-spec create_dummy_gui_package() -> string().
create_dummy_gui_package() ->
    TempDir = mochitemp:mkdtemp(),
    DummyGuiRoot = filename:join(TempDir, "gui_static"),
    ok = file:make_dir(DummyGuiRoot),
    DummyIndex = filename:join(DummyGuiRoot, "index.html"),
    IndexContent = datastore_key:new(),
    ok = file:write_file(DummyIndex, IndexContent),

    DummyPackage = filename:join(TempDir, "gui_static.tar.gz"),
    % Use tar to create archive as erl_tar is limited when it comes to tarring directories
    [] = os:cmd(str_utils:format("tar -C ~s -czf ~s ~s", [TempDir, DummyPackage, "gui_static"])),
    {DummyPackage, IndexContent}.


-spec deploy_dummy_gui(Config :: term(), onedata:gui()) -> {ok, GuiHash :: binary()}.
deploy_dummy_gui(Config, GuiType) ->
    {GuiPackage, IndexContent} = create_dummy_gui_package(),
    copy_file_to_onezone_nodes(Config, GuiPackage),
    {ok, GuiHash} = call_oz(Config, gui_static, deploy_package, [
        % Gui package verification is turned off in tests so valid release
        % version doesn't have to be provided
        GuiType, <<"dummy-version">>, GuiPackage
    ]),
    {GuiHash, IndexContent}.


%%--------------------------------------------------------------------
%% @doc
%% Copies given file from the test master node to given node, under the same path.
%% @end
%%--------------------------------------------------------------------
-spec copy_file_to_onezone_nodes(Config :: term(), file:filename_all()) -> ok.
copy_file_to_onezone_nodes(Config, Path) ->
    lists:foreach(fun(Node) ->
        copy_file_to_node(Node, Path)
    end, ?OZ_NODES(Config)).


%%--------------------------------------------------------------------
%% @doc
%% Copies given file from the test master node to given node, under the same path.
%% @end
%%--------------------------------------------------------------------
-spec copy_file_to_node(node(), file:filename_all()) -> ok.
copy_file_to_node(Node, Path) ->
    {ok, Content} = file:read_file(Path),
    ok = rpc:call(Node, filelib, ensure_dir, [Path]),
    ok = rpc:call(Node, file, write_file, [Path, Content]).


%%--------------------------------------------------------------------
%% @doc
%% Creates a session for user, simulating a login.
%% @end
%%--------------------------------------------------------------------
-spec log_in(Config :: term(), UserId :: od_user:id()) -> {ok, {session:id(), Cookie :: binary()}}.
log_in(Config, UserId) ->
    MockedReq = #{},
    RespReq = ?assertMatch(#{}, call_oz(Config, gui_session, log_in, [UserId, MockedReq])),
    SessionCookie = parse_resp_session_cookie(RespReq),
    {ok, SessionId} = call_oz(Config, gui_session, peek_session_id, [SessionCookie]),
    {ok, {SessionId, SessionCookie}}.


%%--------------------------------------------------------------------
%% @doc
%% Destroys given session, simulating a user logout.
%% @end
%%--------------------------------------------------------------------
-spec log_out(Config :: term(), Cookie :: binary()) -> ok.
log_out(Config, Cookie) ->
    MockedReq = #{
        resp_headers => #{},
        headers => #{?HDR_COOKIE => <<(?SESSION_COOKIE_KEY)/binary, "=", Cookie/binary>>}
    },
    ?assertMatch(#{}, call_oz(Config, gui_session, log_out, [MockedReq])),
    ok.


-spec parse_resp_session_cookie(cowboy_req:req() | binary()) -> undefined | binary().
parse_resp_session_cookie(SetCookieHeader) when is_binary(SetCookieHeader) ->
    % SetCookieHeader: SID=99e4557|4bec19d7; Version=1; Expires=Mon, 13-May-2019 07:46:05 GMT; ...
    Key = ?SESSION_COOKIE_KEY,
    Len = byte_size(?SESSION_COOKIE_KEY),
    [<<Key:Len/binary, "=", SessionCookie/binary>> | _] = binary:split(SetCookieHeader, <<";">>, [global, trim_all]),
    SessionCookie;
parse_resp_session_cookie(Req) ->
    SessionCookieKey = ?SESSION_COOKIE_KEY,
    case Req of
        #{resp_cookies := #{SessionCookieKey := SetCookieHeader}} ->
            parse_resp_session_cookie(iolist_to_binary(SetCookieHeader));
        _ ->
            undefined
    end.


%%--------------------------------------------------------------------
%% @doc
%% Get all atoms representing oz privileges.
%% @end
%%--------------------------------------------------------------------
-spec all_oz_privileges(Config :: term()) -> [atom()].
all_oz_privileges(Config) ->
    call_oz(Config, privileges, oz_privileges, []).


%%--------------------------------------------------------------------
%% @doc
%% Returns the value of the oz_worker's environment variable 'Name'.
%% @end
%%--------------------------------------------------------------------
-spec get_env(Config :: term(), Name :: atom()) -> term().
get_env(Config, Name) ->
    call_oz(Config, oz_worker, get_env, [Name]).


%%--------------------------------------------------------------------
%% @doc
%% Returns the value of the oz_worker's environment variable 'Name' or default
%% if undefined.
%% @end
%%--------------------------------------------------------------------
-spec get_env(Config :: term(), Name :: atom(), Default :: term()) -> term().
get_env(Config, Name, Default) ->
    call_oz(Config, oz_worker, get_env, [Name, Default]).


%%--------------------------------------------------------------------
%% @doc
%% Sets oz_worker's environment variable to desired value on all cluster nodes.
%% @end
%%--------------------------------------------------------------------
-spec set_env(Config :: term(), Name :: atom(), Value :: term()) -> ok.
set_env(Config, Name, Value) ->
    {_, []} = rpc:multicall(?OZ_NODES(Config), oz_worker, set_env, [Name, Value]),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Sets applications's environment variable to desired value on all cluster nodes.
%% @end
%%--------------------------------------------------------------------
-spec set_app_env(Config :: term(), App :: atom(), Name :: atom(), Value :: term()) -> ok.
set_app_env(Config, App, Name, Value) ->
    {_, []} = rpc:multicall(?OZ_NODES(Config), application, set_env, [App, Name, Value]),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Get zone domain.
%% @end
%%--------------------------------------------------------------------
-spec oz_domain(Config :: term()) -> binary().
oz_domain(Config) ->
    call_oz(Config, oz_worker, get_domain, []).


%%--------------------------------------------------------------------
%% @doc
%% Get Onezone's URL to a specific resource defined by Path or Tokens, e.g.:
%%      Path = <<"/ozw/onezone/gui-upload">> OR
%%      Tokens = [<<"/ozw/">>, <<"onezone">>, <<"/gui-upload">>]
%%      -> https://onezone.example.com/ozw/onezone/gui-upload
%% @end
%%--------------------------------------------------------------------
-spec oz_url(Config :: term(), PathOrTokens :: binary() | [binary()]) -> binary().
oz_url(Config, PathOrTokens) ->
    oz_url(Config, <<"https">>, PathOrTokens).


%%--------------------------------------------------------------------
%% @doc
%% Get Onezone's URL to a specific resource with given Scheme defined by Path or Tokens, e.g.:
%%      Scheme = <<"ftp">>
%%      Path = <<"/ozw/onezone/gui-upload">> OR
%%      Tokens = [<<"/ozw/">>, <<"onezone">>, <<"/gui-upload">>]
%%      -> ftp://onezone.example.com/ozw/onezone/gui-upload
%% @end
%%--------------------------------------------------------------------
-spec oz_url(Config :: term(), Scheme :: binary(), PathOrTokens :: binary() | [binary()]) ->
    binary().
oz_url(Config, Scheme, Tokens) when is_list(Tokens) ->
    oz_url(Config, Scheme, str_utils:join_binary(Tokens));
oz_url(Config, Scheme, Path) ->
    PortStr = case get_env(Config, https_server_port) of
        443 -> <<"">>;
        Port -> <<":", Port/binary>>
    end,
    str_utils:format_bin("~s://~s~s~s", [
        Scheme,
        oz_domain(Config),
        PortStr,
        Path
    ]).


%%--------------------------------------------------------------------
%% @doc
%% Get Onezone's REST URL to a specific resource defined by Path or Tokens, e.g.:
%%      Path = <<"/providers/12sdfagsregsdg/domain_config">> OR
%%      Tokens = [<<"/providers">>, <<"/12sdfagsregsdg">>, <<"/domain_config">>]
%%      -> https://onezone.example.com/api/v3/onezone/providers/12sdfagsregsdg/domain_config
%% @end
%%--------------------------------------------------------------------
-spec oz_rest_url(Config :: term(), PathOrTokens :: binary() | [binary()]) -> binary().
oz_rest_url(Config, PathOrTokens) ->
    RestPrefix = list_to_binary(get_env(Config, rest_api_prefix)),
    case PathOrTokens of
        Tokens when is_list(Tokens) -> oz_url(Config, [RestPrefix | Tokens]);
        Path when is_binary(Path) -> oz_url(Config, [RestPrefix, Path])
    end.


%%--------------------------------------------------------------------
%% @doc
%% Get graph sync WebSocket URL for zone.
%% @end
%%--------------------------------------------------------------------
-spec graph_sync_url(Config :: term(), Type :: provider | gui) -> binary().
graph_sync_url(Config, provider) ->
    oz_url(Config, <<"wss">>, <<?PROVIDER_GRAPH_SYNC_WS_PATH>>);
graph_sync_url(Config, gui) ->
    oz_url(Config, <<"wss">>, <<?GUI_GRAPH_SYNC_WS_PATH>>).


%%--------------------------------------------------------------------
%% @doc
%% Get supported graph sync protocol versions.
%% @end
%%--------------------------------------------------------------------
-spec get_gs_supported_proto_versions(Config :: term()) ->
    SupportedVersions :: [integer()].
get_gs_supported_proto_versions(Config) ->
    ?assertMatch([_ | _], call_oz(
        Config, gs_protocol, supported_versions, [])
    ).
