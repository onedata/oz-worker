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
-include_lib("ctool/include/test/test_utils.hrl").


%% API
-export([
    call_oz/4,
    get_env/3,
    get_oz_domain/1,
    get_rest_api_prefix/1,
    get_rest_port/1,
    all_oz_privileges/1
]).
% Operations corresponding to logic modules
-export([
    list_users/1,
    create_user/2,
    get_user/2,
    delete_user/2,

    create_client_token/2,
    list_client_tokens/2,

    user_get_oz_privileges/2,
    user_get_eff_oz_privileges/2,
    user_get_groups/2,
    user_get_spaces/2,
    user_get_eff_groups/2,
    user_set_oz_privileges/4,
    user_set_default_space/3,
    user_get_default_space/2,
    user_unset_default_space/2,
    user_set_space_alias/4,
    user_get_space_alias/3,
    user_unset_space_alias/3,
    user_set_default_provider/3,
    user_get_default_provider/2,
    user_unset_default_provider/2,

    user_leave_space/3
]).
-export([
    all_group_privileges/1,

    list_groups/1,
    create_group/3,
    get_group/2,
    delete_group/2,

    group_get_children/2,
    group_get_spaces/2,
    group_get_users/2,
    group_get_oz_privileges/2,
    group_get_eff_oz_privileges/2,
    group_set_oz_privileges/4,

    group_add_user/3,
    group_set_user_privileges/5,
    group_add_group/3,
    group_remove_user/3,
    group_remove_group/3,
    group_leave_space/3,
    group_leave_handle_service/3,
    group_invite_group_token/3,
    group_invite_user_token/3,

    group_get_user_privileges/3,
    group_get_eff_user_privileges/3,

    group_get_group_privileges/3,
    group_set_group_privileges/5,

    group_create_space/3
]).
-export([
    all_space_privileges/1,

    create_space/3,
    get_space/2,
    list_spaces/1,
    update_space/3,
    delete_space/2,

    space_get_users/2,
    space_get_groups/2,
    space_get_providers/2,

    space_leave_provider/3,

    space_add_user/3,
    space_remove_user/3,
    space_add_group/3,
    space_get_user_privileges/3,
    space_set_user_privileges/5,
    space_get_group_privileges/3,
    space_set_group_privileges/5,
    space_invite_user_token/3,
    space_invite_group_token/3,
    space_invite_provider_token/3,
    space_has_effective_user/3,

    space_remove_group/3
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
    create_provider/2,
    create_provider_registration_token/2,
    get_provider/2,
    list_providers/1,
    delete_provider/2,
    support_space/4,
    support_space/5,
    unsupport_space/3,
    enable_subdomain_delegation/4,
    set_provider_domain/3
]).
-export([
    all_handle_service_privileges/1,

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
    all_handle_privileges/1,

    list_handles/1,
    create_handle/6, create_handle/3,
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
    delete_all_entities/1,
    delete_all_entities/2
]).
% Convenience functions
-export([
    create_3_nested_groups/2, create_3_nested_groups/5,
    create_and_support_3_spaces/2,
    minimum_support_size/1,
    mock_handle_proxy/1,
    unmock_handle_proxy/1,
    gui_ca_certs/1
]).

% Convenience functions for gs
-export([
    create_session/3,
    get_gs_ws_url/1,
    get_gs_supported_proto_versions/1,
    decode_gri/2
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
    Nodes = ?config(oz_worker_nodes, Config),
    Node = lists:nth(rand:uniform(length(Nodes)), Nodes),
    case rpc:call(Node, erlang, apply, [FunWrapper, []]) of
        {crash, Type, Reason, Stacktrace} ->
            % Log a bad rpc - very useful when debugging tests.
            ct:print(
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
-spec create_user(Config :: term(), User :: #od_user{}) -> {ok, od_user:id()}.
create_user(Config, User) ->
    ?assertMatch({ok, _}, call_oz(
        Config, user_logic, create, [User]
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
        Config, user_logic, create_client_token, [?ROOT, UserId]
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
        Config, user_logic, list_client_tokens, [?ROOT, UserId]
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
    Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:oz_privilege()]) -> ok.
user_set_oz_privileges(Config, UserId, Operation, Privileges) ->
    ?assertMatch(ok, call_oz(Config, user_logic, update_oz_privileges, [
        ?ROOT, UserId, Operation, Privileges
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets default space of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_set_default_space(Config :: term(), UserId :: od_user:id(),
    SpaceId :: od_space:id()) -> ok.
user_set_default_space(Config, UserId, SpaceId) ->
    ?assertMatch(ok, call_oz(Config, user_logic, set_default_space, [
        ?ROOT, UserId, SpaceId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Retrieve default space of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_get_default_space(Config :: term(),
    UserId :: od_user:id()) -> SpaceId :: od_space:id().
user_get_default_space(Config, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, user_logic, get_default_space, [?ROOT, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Unsets default space of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_unset_default_space(Config :: term(), UserId :: od_user:id()) -> ok.
user_unset_default_space(Config, UserId) ->
    ?assertMatch(ok, call_oz(Config, user_logic, unset_default_space, [
        ?ROOT, UserId
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
%% Sets default provider of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_set_default_provider(Config :: term(), UserId :: od_user:id(),
    ProviderId :: od_provider:id()) -> ok.
user_set_default_provider(Config, UserId, ProviderId) ->
    ?assertMatch(ok, call_oz(Config, user_logic, set_default_provider, [
        ?ROOT, UserId, ProviderId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets default provider of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_get_default_provider(Config :: term(),
    UserId :: od_user:id()) -> {ok, ProviderId :: od_provider:id()}.
user_get_default_provider(Config, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, user_logic, get_default_provider, [
        ?ROOT, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Unsets default provider of a user.
%% @end
%%--------------------------------------------------------------------
-spec user_unset_default_provider(Config :: term(), UserId :: od_user:id()) ->
    ok.
user_unset_default_provider(Config, UserId) ->
    ?assertMatch(ok, call_oz(Config, user_logic, unset_default_provider, [
        ?ROOT, UserId
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
%% Get all atoms representing group privileges.
%% @end
%%--------------------------------------------------------------------
-spec all_group_privileges(Config :: term()) -> [atom()].
all_group_privileges(Config) ->
    call_oz(Config, privileges, group_privileges, []).


%%--------------------------------------------------------------------
%% @doc
%% Creates group in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Config :: term(), Client :: entity_logic:client(),
    NameOrData :: od_group:name() | #{}) -> {ok, Id :: binary()}.
create_group(Config, Client, NameOrData) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, create, [Client, NameOrData]
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
%% Retrieves group children groups from onezone.
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
%% Retrieves group spaces from onezone.
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
%% Retrieves group users from onezone.
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
%% Adds a user a to group in onezone.
%% @end
%%--------------------------------------------------------------------
-spec group_add_user(Config :: term(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> {ok, UserId :: od_user:id()}.
group_add_user(Config, GroupId, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, add_user, [?ROOT, GroupId, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Adds a group a to group in onezone.
%% @end
%%--------------------------------------------------------------------
-spec group_add_group(Config :: term(), GroupId :: od_group:id(),
    ChildGroupId :: od_group:id()) -> {ok, ChildGroupId :: od_group:id()}.
group_add_group(Config, GroupId, ChildGroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, add_group, [?ROOT, GroupId, ChildGroupId]
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
%% Get all atoms representing space privileges.
%% @end
%%--------------------------------------------------------------------
-spec all_space_privileges(Config :: term()) -> [atom()].
all_space_privileges(Config) ->
    call_oz(Config, privileges, space_privileges, []).


%%--------------------------------------------------------------------
%% @doc
%% Creates a space in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_space(Config :: term(), Client :: entity_logic:client(),
    Name :: od_space:name()) -> {ok, od_space:id()}.
create_space(Config, Client, Name) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, create, [Client, Name]
    )).


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
%% Retrieves supporting providers of given space from onezone.
%% @end
%%--------------------------------------------------------------------
-spec space_get_providers(Config :: term(),
    SpaceId :: od_space:id()) -> {ok, [od_provider:id()]}.
space_get_providers(Config, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, get_providers, [?ROOT, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Leave space from given provider.
%% @end
%%--------------------------------------------------------------------
-spec space_leave_provider(Config :: term(),
    SpaceId :: od_space:id(), ProviderId :: od_provider:id()) -> ok.
space_leave_provider(Config, SpaceId, ProviderId) ->
    ?assertMatch(ok, call_oz(
        Config, space_logic, leave_provider, [?ROOT, SpaceId, ProviderId]
    )).


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
    UserId :: od_user:id(), Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:space_privilege()]) -> ok.
space_set_user_privileges(Config, SpaceId, UserId, Operation, Privs) ->
    ?assertMatch(ok, call_oz(Config, space_logic, update_user_privileges, [
        ?ROOT, SpaceId, UserId, Operation, Privs
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
    GroupId :: od_group:id(), Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:space_privilege()]) -> ok.
space_set_group_privileges(Config, SpaceId, GroupId, Operation, Privs) ->
    ?assertMatch(ok, call_oz(Config, space_logic, update_group_privileges, [
        ?ROOT, SpaceId, GroupId, Operation, Privs
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Creates a user invite token to given space.
%% @end
%%--------------------------------------------------------------------
-spec space_invite_user_token(Config :: term(),
    Client :: entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, macaroon:macaroon()}.
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
    Client :: entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, macaroon:macaroon()}.
space_invite_group_token(Config, Client, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, create_group_invite_token, [Client, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a provider invite token to given space.
%% @end
%%--------------------------------------------------------------------
-spec space_invite_provider_token(Config :: term(),
    Client :: entity_logic:client(), SpaceId :: od_space:id()) ->
    {ok, macaroon:macaroon()}.
space_invite_provider_token(Config, Client, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, create_provider_invite_token, [Client, SpaceId]
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
-spec create_share(Config :: term(), Client :: entity_logic:client(),
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
-spec create_share(Config :: term(), Client :: entity_logic:client(),
    Data :: maps:map()) -> {ok, od_share:id()}.
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
%% Creates a provider (automatically generates certificates).
%% @end
%%--------------------------------------------------------------------
-spec create_provider(Config :: term(),
    NameOrData :: od_provider:name() | #{}) ->
    {ok, {ProviderId :: binary(), KeyFile :: string(), CertFile :: string()}}.
create_provider(Config, Name) when is_binary(Name) ->
    create_provider(Config, #{
        <<"name">> => Name,
        <<"adminEmail">> => <<"admin@onedata.org">>,
        <<"domain">> => ?UNIQUE_DOMAIN,
        <<"subdomainDelegation">> => false,
        <<"latitude">> => 0.0,
        <<"longitude">> => 0.0
    });
create_provider(Config, Data) ->
    {ok, {ProviderId, Macaroon}} = ?assertMatch({ok, _}, call_oz(
        Config, provider_logic, create, [?NOBODY, Data]
    )),
    {ok, MacaroonBin} = onedata_macaroons:serialize(Macaroon),
    {ok, {ProviderId, MacaroonBin}}.


%%--------------------------------------------------------------------
%% @doc
%% Creates a provider registration token.
%% @end
%%--------------------------------------------------------------------
-spec create_provider_registration_token(Config :: term(),
    Client :: entity_logic:client()) -> {ok, macaroon:macaroon()}.
create_provider_registration_token(Config, Client) ->
    ?assertMatch({ok, _}, call_oz(
        Config, provider_logic, create_provider_registration_token, [Client]
    )).


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
%% Supports a space by a provider based on space id.
%% @end
%%--------------------------------------------------------------------
-spec support_space(Config :: term(), ProviderId :: od_provider:id(),
    SpaceId :: od_space:id(), Size :: non_neg_integer()) ->
    {ok, {ProviderId :: binary(), KeyFile :: string(), CertFile :: string()}}.
support_space(Config, ProviderId, SpaceId, Size) ->
    {ok, Macaroon} = ?assertMatch({ok, _}, space_invite_provider_token(
        Config, ?ROOT, SpaceId
    )),
    ?assertMatch({ok, _}, call_oz(Config, provider_logic, support_space, [
        ?PROVIDER(ProviderId), ProviderId, Macaroon, Size
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Supports a space by a provider based on token.
%% @end
%%--------------------------------------------------------------------
-spec support_space(Config :: term(), Client :: entity_logic:client(),
    ProviderId :: od_provider:id(), Token :: binary() | macaroon:macaroon(),
    Size :: non_neg_integer()) ->
    {ok, {ProviderId :: binary(), KeyFile :: string(), CertFile :: string()}}.
support_space(Config, Client, ProviderId, Token, Size) ->
    ?assertMatch({ok, _}, call_oz(Config, provider_logic, support_space, [
        Client, ProviderId, Token, Size
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Revoke space support by given provider.
%% @end
%%--------------------------------------------------------------------
-spec unsupport_space(Config :: term(), ProviderId :: od_provider:id(),
    SpaceId :: od_space:id()) -> ok.
unsupport_space(Config, ProviderId, SpaceId) ->
    ?assertMatch(ok, call_oz(Config, provider_logic, revoke_support, [
        ?ROOT, ProviderId, SpaceId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Get all atoms representing handle service privileges.
%% @end
%%--------------------------------------------------------------------
-spec all_handle_service_privileges(Config :: term()) -> [atom()].
all_handle_service_privileges(Config) ->
    call_oz(Config, privileges, handle_service_privileges, []).


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
    ?assertMatch(ok, oz_test_utils:call_oz(Config,
        provider_logic, update_domain_config, [?ROOT, ProviderId, Data])).


%%--------------------------------------------------------------------
%% @doc
%% Sets provider domain (not a delegated subdomain).
%% @end
%%--------------------------------------------------------------------
-spec set_provider_domain(Config :: term(), ProviderId :: od_provider:od(),
    Domain :: binary())  -> ok.
set_provider_domain(Config, ProviderId, Domain) ->
    Data = #{
      <<"subdomainDelegation">> => false,
      <<"domain">> => Domain},
    ?assertMatch(ok, oz_test_utils:call_oz(Config,
        provider_logic, update_domain_config, [?ROOT, ProviderId, Data])).



%%--------------------------------------------------------------------
%% @doc
%% Creates a handle service.
%% @end
%%--------------------------------------------------------------------
-spec create_handle_service(Config :: term(), Client :: entity_logic:client(),
    Name :: od_handle_service:name(),
    ProxyEndpoint :: od_handle_service:proxy_endpoint(),
    ServiceProperties :: od_handle_service:service_properties()) ->
    {ok, od_handle_service:id()}.
create_handle_service(Config, Client, Name, ProxyEndpoint, ServiceProperties) ->
    ?assertMatch({ok, _}, call_oz(Config, handle_service_logic, create, [
        Client, Name, ProxyEndpoint, ServiceProperties
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Creates a handle service.npr
%% @end
%%--------------------------------------------------------------------
-spec create_handle_service(Config :: term(), Client :: entity_logic:client(),
    Data :: maps:map()) -> {ok, od_handle_service:id()}.
create_handle_service(Config, Client, Data) ->
    ?assertMatch({ok, _}, call_oz(Config, handle_service_logic, create, [
        Client, Data
    ])).


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
    Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:handle_service_privilege()]) -> ok.
handle_service_set_user_privileges(
    Config, HServiceId, UserId, Operation, Privs
) ->
    ?assertMatch(ok, call_oz(Config, handle_service_logic,
        update_user_privileges, [?ROOT, HServiceId, UserId, Operation, Privs]
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
    HServiceId :: od_handle_service:id(), GroupId:: od_group:id(),
    Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:handle_service_privilege()]) -> ok.
handle_service_set_group_privileges(
    Config, HServiceId, GroupId, Operation, Privs
) ->
    ?assertMatch(ok, call_oz(Config, handle_service_logic,
        update_group_privileges, [?ROOT, HServiceId, GroupId, Operation, Privs]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Get all atoms representing handles privileges.
%% @end
%%--------------------------------------------------------------------
-spec all_handle_privileges(Config :: term()) -> [atom()].
all_handle_privileges(Config) ->
    call_oz(Config, privileges, handle_privileges, []).


%%--------------------------------------------------------------------
%% @doc
%% Creates a handle.
%% @end
%%--------------------------------------------------------------------
-spec create_handle(Config :: term(), Client :: entity_logic:client(),
    HandleServiceId :: od_handle_service:id(),
    ResourceType :: od_handle:resource_type(),
    ResourceId :: od_handle:resource_id(), Metadata :: od_handle:metadata()) ->
    {ok, od_handle:id()}.
create_handle(Config, Client, HandleServiceId, ResourceType, ResourceId, Metadata) ->
    ?assertMatch({ok, _}, call_oz(Config, handle_logic, create, [
        Client, HandleServiceId, ResourceType, ResourceId, Metadata
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Creates a handle.
%% @end
%%--------------------------------------------------------------------
-spec create_handle(Config :: term(), Client :: entity_logic:client(),
    Data :: maps:map()) -> {ok, od_handle:id()}.
create_handle(Config, Client, Data) ->
    ?assertMatch({ok, _}, call_oz(Config, handle_logic, create, [
        Client, Data
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
%% Updates a handle
%% @end
%%--------------------------------------------------------------------
-spec update_handle(Config :: term(), HandleId :: od_handle:id(),
    Data :: maps:map()) -> ok.
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
    Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:handle_privilege()]) -> ok.
handle_set_user_privileges(Config, HandleId, UserId, Operation, Privs) ->
    ?assertMatch(ok, call_oz(Config, handle_logic,
        update_user_privileges, [?ROOT, HandleId, UserId, Operation, Privs]
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
%% Sets privileges for group's user.
%% @end
%%--------------------------------------------------------------------
-spec group_set_user_privileges(Config :: term(), GroupId :: od_group:id(),
    UserId :: od_user:id(), Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:group_privilege()]) -> ok.
group_set_user_privileges(Config, GroupId, UserId, Operation, Privs) ->
    ?assertMatch(ok, call_oz(Config, group_logic, update_user_privileges, [
        ?ROOT, GroupId, UserId, Operation, Privs
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
%% Creates a group invite token to given group.
%% @end
%%--------------------------------------------------------------------
-spec group_invite_group_token(Config :: term(),
    Client :: entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, macaroon:macaroon()}.
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
    Client :: entity_logic:client(), GroupId :: od_group:id()) ->
    {ok, macaroon:macaroon()}.
group_invite_user_token(Config, Client, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, create_user_invite_token, [Client, GroupId]
    )).


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
    [delete_provider(Config, PId) || PId <- Providers],
    [delete_handle(Config, HId) || HId <- Handles],
    [delete_share(Config, ShId) || ShId <- Shares],
    [delete_space(Config, SpId) || SpId <- Spaces],
    [delete_handle_service(Config, HSId) || HSId <- HServices],
    % Check if predefined groups should be removed too.
    GroupsToDelete = case RemovePredefinedGroups of
        true ->
            Groups;
        false ->
            % Filter out predefined groups
            [Node | _] = ?config(oz_worker_nodes, Config),
            {ok, PredefinedGroupsMapping} = test_utils:get_env(
                Node, oz_worker, predefined_groups
            ),
            PredefinedGroups = [Id || #{id := Id} <- PredefinedGroupsMapping],
            lists:filter(
                fun(GroupId) ->
                    not lists:member(GroupId, PredefinedGroups)
                end, Groups)
    end,
    [?assertMatch(ok, delete_group(Config, GId)) || GId <- GroupsToDelete],
    [?assertMatch(ok, delete_user(Config, UId)) || UId <- Users],
    ok.


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
    {ok, BottomGroup} = oz_test_utils:create_group(
        Config, ?USER(TestUser), BotGrName
    ),
    % Dummy user will be used only to create groups
    {ok, MiddleGroup} = oz_test_utils:create_group(
        Config, ?ROOT, MidGrName
    ),
    {ok, TopGroup} = oz_test_utils:create_group(
        Config, ?ROOT, TopGrName
    ),
    {ok, BottomGroup} = oz_test_utils:group_add_group(
        Config, MiddleGroup, BottomGroup
    ),
    {ok, MiddleGroup} = oz_test_utils:group_add_group(
        Config, TopGroup, MiddleGroup
    ),
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
    {ok, MinimumSupportSize} = call_oz(
        Config, application, get_env, [oz_worker, minimum_space_support_size]
    ),
    MinimumSupportSize.


%%--------------------------------------------------------------------
%% @doc
%% Mocks handle proxy on all nodes of onezone.
%% @end
%%--------------------------------------------------------------------
-spec mock_handle_proxy(Config :: term()) -> ok.
mock_handle_proxy(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, handle_proxy_client, [passthrough]),
    ok = test_utils:mock_expect(Nodes, handle_proxy_client, put,
        fun(_, <<"/handle", _/binary>>, _, _) ->
            {ok, 201, #{<<"location">> => <<"/test_location">>}, <<"">>}
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
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, handle_proxy_client).


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
%% Creates a session for user.
%% @end
%%--------------------------------------------------------------------
-spec create_session(Config :: term(),
    UserId :: term(), CustomArgs :: [term()]) ->
    {ok, SessId :: binary()}.
create_session(Config, UserId, CustomArgs) ->
    ?assertMatch({ok, _}, call_oz(
        Config, gui_session_plugin, create_session, [UserId, CustomArgs]
    )).


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
%% Returns the value of the environment variable 'Name' for 'Application'.
%% @end
%%--------------------------------------------------------------------
-spec get_env(Config :: term(), Application :: atom(), Name :: atom()) ->
    {ok, Value :: term()}.
get_env(Config, Application, Name) ->
    ?assertMatch({ok, _}, call_oz(
        Config, application, get_env, [Application, Name]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Get REST listener port for zone.
%% @end
%%--------------------------------------------------------------------
-spec get_rest_port(Config :: term()) -> {ok, Port :: inet:port_number()}.
get_rest_port(Config) ->
    get_env(Config, ?APP_NAME, https_server_port).


%%--------------------------------------------------------------------
%% @doc
%% Get zone domain.
%% @end
%%--------------------------------------------------------------------
-spec get_oz_domain(Config :: term()) -> {ok, Domain :: string()}.
get_oz_domain(Config) ->
    get_env(Config, ?APP_NAME, http_domain).


%%--------------------------------------------------------------------
%% @doc
%% Get rest api prefix.
%% @end
%%--------------------------------------------------------------------
-spec get_rest_api_prefix(Config :: term()) -> {ok, Prefix :: string()}.
get_rest_api_prefix(Config) ->
    get_env(Config, ?APP_NAME, rest_api_prefix).


%%--------------------------------------------------------------------
%% @doc
%% Get graph sync ws url for zone.
%% @end
%%--------------------------------------------------------------------
-spec get_gs_ws_url(Config :: term()) -> binary().
get_gs_ws_url(Config) ->
    {ok, ZoneDomain} = get_oz_domain(Config),
    {ok, GsPort} = get_rest_port(Config),
    str_utils:format_bin(
        "wss://~s:~B/~s",
        [ZoneDomain, GsPort, string:strip(?PROVIDER_GRAPH_SYNC_WS_PATH, both, $/)]
    ).


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


%%--------------------------------------------------------------------
%% @doc
%% Get supported graph sync protocol versions.
%% @end
%%--------------------------------------------------------------------
-spec decode_gri(Config :: term(),
    EncodedGri :: binary()) -> Gri :: #gri{}.
decode_gri(Config, EncodedGri) ->
    ?assertMatch(#gri{}, call_oz(
        Config, gs_protocol, string_to_gri, [EncodedGri])
    ).
