%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @author Lukasz Opiola
%%% @copyright (C): 2014-2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Common functions for ct tests.
%%% @end
%%%-------------------------------------------------------------------
-module(oz_test_utils).

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").


%% API
-export([
    create_user/2,
    create_client_token/2,
    get_user/2,
    list_users/1,
    set_user_oz_privileges/4,
    delete_user/2,

    user_leave_space/3
]).

-export([
    create_group/3,
    get_group/2,
    list_groups/1,
    set_group_oz_privileges/4,
    delete_group/2,

    add_user_to_group/4,
    add_group_to_group/4,
    group_remove_user/3,
    group_leave_space/3
]).

-export([
    create_space/3,
    get_space/2,
    list_spaces/1,
    update_space/3,
    delete_space/2,

    add_user_to_space/4,
    add_group_to_space/4,
    space_set_user_privileges/5,
    space_set_group_privileges/5,
    space_invite_user_token/3,
    space_invite_group_token/3,
    space_invite_provider_token/3,
    space_has_effective_user/3
]).

-export([
    create_share/6,
    create_share/3,
    list_shares/1,
    delete_share/2
]).

-export([
    create_provider_and_certs/2,
    create_provider/2,
    support_space/5,
    list_providers/1,
    delete_provider/2
]).

-export([
    create_handle_service/5, create_handle_service/3,
    add_user_to_handle_service/4,
    add_group_to_handle_service/4,
    list_handle_services/1,
    delete_handle_service/2
]).

-export([
    create_handle/6, create_handle/3,
    add_user_to_handle/4,
    add_group_to_handle/4,
    list_handles/1,
    modify_handle/5,
    delete_handle/2
]).

-export([
    delete_all_entities/1,
    delete_all_entities/2
]).

-export([
    call_oz/4,
    generate_provider_cert_files/0,
    ensure_eff_graph_up_to_date/1,
    mock_handle_proxy/1,
    unmock_handle_proxy/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a user in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_user(Config :: term(), User :: #od_user{}) -> {ok, od_user:id()}.
create_user(Config, User) ->
    ?assertMatch({ok, _}, call_oz(
        Config, n_user_logic, create, [User]
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
        Config, n_user_logic, create_client_token, [?ROOT, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves user data from onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_user(Config :: term(), UserId :: od_user:id()) -> {ok, #od_user{}}.
get_user(Config, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, n_user_logic, get, [?ROOT, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of all users in onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_users(Config :: term()) -> {ok, [od_user:id()]}.
list_users(Config) ->
    ?assertMatch({ok, _}, call_oz(
        Config, n_user_logic, list, [?ROOT]
    )).

%%--------------------------------------------------------------------
%% @doc
%% Sets OZ privileges of a user.
%% @end
%%--------------------------------------------------------------------
-spec set_user_oz_privileges(Config :: term(), UserId :: od_user:id(),
    Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:oz_privilege()]) -> ok.
set_user_oz_privileges(Config, UserId, Operation, Privileges) ->
    ?assertMatch(ok, call_oz(Config, n_user_logic, update_oz_privileges, [
        ?ROOT, UserId, Operation, Privileges
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given user from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_user(Config :: term(), UserId :: od_user:id()) -> ok.
delete_user(Config, UserId) ->
    ?assertMatch(ok, call_oz(
        Config, n_user_logic, delete, [?ROOT, UserId]
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
        Config, n_user_logic, leave_space, [?ROOT, UserId, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates group in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Config :: term(), Client :: n_entity_logic:client(),
    Name :: binary()) -> {ok, Id :: binary()}.
create_group(Config, Client, Name) ->
    ?assertMatch({ok, _}, call_oz(
        Config, n_group_logic, create, [Client, Name]
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
        Config, n_group_logic, get, [?ROOT, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of all groups in onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_groups(Config :: term()) -> {ok, [od_group:id()]}.
list_groups(Config) ->
    ?assertMatch({ok, _}, call_oz(Config, n_group_logic, list, [?ROOT])).


%%--------------------------------------------------------------------
%% @doc
%% Sets OZ privileges of a group.
%% @end
%%--------------------------------------------------------------------
-spec set_group_oz_privileges(Config :: term(), GroupId :: od_group:id(),
    Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:oz_privilege()]) -> ok.
set_group_oz_privileges(Config, GroupId, Operation, Privileges) ->
    ?assertMatch({ok, _}, call_oz(Config, n_group_logic, update_oz_privileges, [
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
        Config, n_group_logic, delete, [?ROOT, GroupId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Adds a user a to group in onezone.
%% @end
%%--------------------------------------------------------------------
-spec add_user_to_group(Config :: term(), Client :: n_entity_logic:client(),
    GroupId :: od_group:id(), UserId :: od_user:id()) ->
    {ok, UserId :: od_user:id()}.
add_user_to_group(Config, Client, GroupId, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, n_group_logic, add_user, [
        Client, GroupId, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Adds a group a to group in onezone.
%% @end
%%--------------------------------------------------------------------
-spec add_group_to_group(Config :: term(), Client :: n_entity_logic:client(),
    GroupId :: od_group:id(), ChildGroupId :: od_group:id()) ->
    {ok, ChildGroupId :: od_group:id()}.
add_group_to_group(Config, Client, GroupId, ChildGroupId) ->
    ?assertMatch({ok, _}, call_oz(Config, n_group_logic, add_group, [
        Client, GroupId, ChildGroupId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Removes user from a group from onezone.
%% @end
%%--------------------------------------------------------------------
-spec group_remove_user(Config :: term(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> ok.
group_remove_user(Config, GroupId, UserId) ->
    ?assertMatch(ok, call_oz(
        Config, n_group_logic, remove_user, [?ROOT, GroupId, UserId]
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
        Config, n_group_logic, leave_space, [?ROOT, GroupId, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Creates a space in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_space(Config :: term(), Client :: n_entity_logic:client(),
    Name :: binary()) -> {ok, od_space:id()}.
create_space(Config, Client, Name) ->
    ?assertMatch({ok, _}, call_oz(
        Config, n_space_logic, create, [Client, Name]
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
        Config, n_space_logic, get, [?ROOT, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of all spaces in onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_spaces(Config :: term()) -> {ok, [od_space:id()]}.
list_spaces(Config) ->
    ?assertMatch({ok, _}, call_oz(Config, n_space_logic, list, [?ROOT])).


%%--------------------------------------------------------------------
%% @doc
%% Updates space name.
%% @end
%%--------------------------------------------------------------------
-spec update_space(Config :: term(), SpaceId :: od_space:id(),
    Name :: binary()) -> ok.
update_space(Config, SpaceId, Name) ->
    ?assertMatch(ok, call_oz(
        Config, n_space_logic, update, [?ROOT, SpaceId, Name]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given space from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_space(Config :: term(), SpaceId :: od_space:id()) -> ok.
delete_space(Config, SpaceId) ->
    ?assertMatch(ok, call_oz(Config, n_space_logic, delete, [
        ?ROOT, SpaceId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Add a user to a space.
%% @end
%%--------------------------------------------------------------------
-spec add_user_to_space(Config :: term(), Client :: n_entity_logic:client(),
    SpaceId :: od_space:id(), UserId :: od_user:id()) -> {ok, od_user:id()}.
add_user_to_space(Config, Client, SpaceId, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, n_space_logic, add_user, [
        Client, SpaceId, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Add a group to a space.
%% @end
%%--------------------------------------------------------------------
-spec add_group_to_space(Config :: term(), Client :: n_entity_logic:client(),
    SpaceId :: od_space:id(), GroupId :: od_group:id()) -> {ok, od_group:id()}.
add_group_to_space(Config, Client, SpaceId, GroupId) ->
    ?assertMatch({ok, _}, call_oz(Config, n_space_logic, add_group, [
        Client, SpaceId, GroupId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets privileges of a user in a space.
%% @end
%%--------------------------------------------------------------------
-spec space_set_user_privileges(Config :: term(), SpaceId :: od_space:id(),
    UserId :: od_user:id(), Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:space_privilege()]) -> ok.
space_set_user_privileges(Config, SpaceId, UserId, Operation, Privs) ->
    ?assertMatch(ok, call_oz(Config, n_space_logic, update_user_privileges, [
        SpaceId, UserId, Operation, Privs
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets privileges of a group in a space.
%% @end
%%--------------------------------------------------------------------
-spec space_set_group_privileges(Config :: term(), SpaceId :: od_space:id(),
    GroupId :: od_group:id(), Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:space_privilege()]) -> ok.
space_set_group_privileges(Config, SpaceId, GroupId, Operation, Privs) ->
    ?assertMatch(ok, call_oz(Config, n_space_logic, update_group_privileges, [
        SpaceId, GroupId, Operation, Privs
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Creates a user invite token to given space.
%% @end
%%--------------------------------------------------------------------
-spec space_invite_user_token(Config :: term(), Client :: n_entity_logic:client(),
    SpaceId :: od_space:id()) -> {ok, macaroon:macaroon()}.
space_invite_user_token(Config, Client, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, n_space_logic, create_user_invite_token, [Client, SpaceId]
    )).


space_invite_group_token(Config, Client, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, n_space_logic, create_group_invite_token, [Client, SpaceId]
    )).


space_invite_provider_token(Config, Client, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, n_space_logic, create_provider_invite_token, [Client, SpaceId]
    )).


%%--------------------------------------------------------------------
%% @doc Checks if given space has given effective user.
%% @end
%%--------------------------------------------------------------------
-spec space_has_effective_user(Config :: term(), SpaceId :: od_space:id(),
    UserId :: od_user:id()) -> boolean().
space_has_effective_user(Config, SpaceId, UserId) ->
    {ok, EffUsers} = ?assertMatch({ok, _}, call_oz(
        Config, n_space_logic, get_eff_users, [?ROOT, SpaceId]
    )),
    lists:member(UserId, EffUsers).


create_share(Config, Client, ShareId, Name, RootFileId, SpaceId) ->
    ?assertMatch({ok, _}, call_oz(Config, n_share_logic, create, [
        Client, ShareId, Name, RootFileId, SpaceId
    ])).


create_share(Config, Client, Data) ->
    ?assertMatch({ok, _}, call_oz(Config, n_share_logic, create, [
        Client, Data
    ])).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all shares in onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_shares(Config :: term()) -> {ok, [od_share:id()]}.
list_shares(Config) ->
    ?assertMatch({ok, _}, call_oz(Config, n_share_logic, list, [?ROOT])).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given share from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_share(Config :: term(), ShareId :: od_share:id()) -> ok.
delete_share(Config, ShareId) ->
    ?assertMatch(ok, call_oz(Config, share_logic, delete, [?ROOT, ShareId])).


%%--------------------------------------------------------------------
%% @doc Creates a provider.
%% @end
%%--------------------------------------------------------------------
-spec create_provider_and_certs(Config :: term(), Name :: binary()) ->
    {ok, {ProviderId :: binary(), KeyFile :: string(), CertFile :: string()}}.
create_provider_and_certs(Config, Name) ->
    {KeyFile, CSRFile, CertFile} = generate_provider_cert_files(),
    {ok, CSR} = file:read_file(CSRFile),
    {ok, {ProviderId, Certificate}} = ?assertMatch({ok, _}, call_oz(
        Config, n_provider_logic, create, [
            ?NOBODY,
            Name,
            [<<"127.0.0.1">>],
            <<"127.0.0.1">>,
            CSR
        ])),
    ok = file:write_file(CertFile, Certificate),
    {ok, {ProviderId, KeyFile, CertFile}}.


create_provider(Config, Data) ->
    ?assertMatch({ok, _}, call_oz(Config, n_provider_logic, create, [
        ?NOBODY, Data
    ])).


%%--------------------------------------------------------------------
%% @doc Supports space by provider.
%% @end
%%--------------------------------------------------------------------
support_space(Config, Client, ProviderId, Token, Size) ->
    ?assertMatch({ok, _}, call_oz(Config, n_space_logic, support_space, [
        Client, ProviderId, Token, Size
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of all providers in onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_providers(Config :: term()) -> {ok, [od_provider:id()]}.
list_providers(Config) ->
    ?assertMatch({ok, _}, call_oz(Config, n_provider_logic, list, [?ROOT])).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given provider from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_provider(Config :: term(), ProviderId :: od_provider:id()) -> ok.
delete_provider(Config, ProviderId) ->
    ?assertMatch(ok, call_oz(Config, n_provider_logic, delete, [
        ?ROOT, ProviderId
    ])).


create_handle_service(Config, Client, Name, ProxyEndpoint, ServiceProperties) ->
    ?assertMatch({ok, _}, call_oz(Config, n_handle_service_logic, create, [
        Client, Name, ProxyEndpoint, ServiceProperties
    ])).


create_handle_service(Config, Client, Data) ->
    ?assertMatch({ok, _}, call_oz(Config, n_handle_service_logic, create, [
        Client, Data
    ])).


add_user_to_handle_service(Config, Client, HServiceId, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, n_handle_service_logic, add_user, [
        Client, HServiceId, UserId
    ])).


add_group_to_handle_service(Config, Client, HServiceId, GroupId) ->
    ?assertMatch({ok, _}, call_oz(Config, n_handle_service_logic, add_group, [
        Client, HServiceId, GroupId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of all handle_services in onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_handle_services(Config :: term()) -> {ok, [od_handle_service:id()]}.
list_handle_services(Config) ->
    ?assertMatch({ok, _}, call_oz(Config, n_handle_service_logic, list, [
        ?ROOT
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given handle_service from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_handle_service(Config :: term(),
    HandleServiceId :: od_handle_service:id()) -> ok.
delete_handle_service(Config, HandleServiceId) ->
    ?assertMatch(ok, call_oz(Config, n_handle_service_logic, delete, [
        ?ROOT, HandleServiceId
    ])).


create_handle(Config, Client, HandleServiceId, ResourceType, ResourceId, Metadata) ->
    ?assertMatch({ok, _}, call_oz(Config, n_handle_logic, create, [
        Client, HandleServiceId, ResourceType, ResourceId, Metadata
    ])).


create_handle(Config, Client, Data) ->
    ?assertMatch({ok, _}, call_oz(Config, n_handle_logic, create, [
        Client, Data
    ])).


add_user_to_handle(Config, Client, HandleId, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, n_handle_logic, add_user, [
        Client, HandleId, UserId
    ])).


add_group_to_handle(Config, Client, HandleId, GroupId) ->
    ?assertMatch({ok, _}, call_oz(Config, n_handle_logic, add_group, [
        Client, HandleId, GroupId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of all handles in onezone.
%% @end
%%--------------------------------------------------------------------
-spec list_handles(Config :: term()) -> {ok, [od_handle:id()]}.
list_handles(Config) ->
    ?assertMatch({ok, _}, call_oz(Config, n_handle_logic, list, [
        ?ROOT
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given handle from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_handle(Config :: term(), HandleId :: od_handle:id()) -> ok.
delete_handle(Config, HandleId) ->
    call_oz(Config, handle_logic, delete, [?ROOT, HandleId]).


%%--------------------------------------------------------------------
%% @doc Modifies handle
%% @end
%%--------------------------------------------------------------------
-spec modify_handle(Config :: term(), od_handle:id(), od_handle:resource_type(),
    od_handle:resource_id(), od_handle:metadata()) -> ok.
modify_handle(Config, HandleId, NewResourceType, NewResourceId, NewMetadata) ->
    call_oz(Config, handle_logic, modify,
        [HandleId, NewResourceType, NewResourceId, NewMetadata]).


%%--------------------------------------------------------------------
%% @doc Deletes all entities from onezone
%% (users, groups, spaces, shares, providers).
%% NOTE: Does not remove predefined groups! Use remove_all_entities/2 for that.
%% @end
%%--------------------------------------------------------------------
-spec delete_all_entities(Config :: term()) -> ok.
delete_all_entities(Config) ->
    delete_all_entities(Config, false).


%%--------------------------------------------------------------------
%% @doc Deletes all entities from onezone
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
    [true = delete_provider(Config, PId) || PId <- Providers],
    [true = delete_share(Config, ShId) || ShId <- Shares],
    [true = delete_space(Config, SpId) || SpId <- Spaces],
    [true = delete_handle(Config, HId) || HId <- Handles],
    [true = delete_handle_service(Config, HSId) || HSId <- HServices],
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
    [true = delete_group(Config, GId) || GId <- GroupsToDelete],
    [true = delete_user(Config, UId) || UId <- Users],
    ok.


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
            {crash, Type, Reason, erlang:get_stacktrace()}
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
                "Stacktrace: ~p",
                [Module, Function, Args, Type, Reason, Stacktrace]
            ),
            {error, {badrpc, Reason}};
        Result ->
            Result
    end.


%%--------------------------------------------------------------------
%% @doc
%% Creates a key/cert pair and a corresponding CSR for provider registration.
%% @end
%%--------------------------------------------------------------------
-spec generate_provider_cert_files() ->
    {KeyFile :: string(), CSRFile :: string(), CertFile :: string()}.
generate_provider_cert_files() ->
    Prefix = "provider" ++ integer_to_list(erlang:system_time(micro_seconds)),
    KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
    CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
    CertFile = filename:join(?TEMP_DIR, Prefix ++ "_cert.pem"),
    os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
    os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
    {KeyFile, CSRFile, CertFile}.


%%--------------------------------------------------------------------
%% @doc
%% Returns when effective graph has been fully recalculated.
%% @end
%%--------------------------------------------------------------------
-spec ensure_eff_graph_up_to_date(Config :: term()) -> true.
ensure_eff_graph_up_to_date(Config) ->
    ?assert(oz_test_utils:call_oz(Config, entity_graph, ensure_up_to_date, [])).


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
            {ok, 201, [{<<"location">>, <<"/test_location">>}], <<"">>}
        end),
    ok = test_utils:mock_expect(Nodes, handle_proxy_client, patch,
        fun(_, <<"/handle", _/binary>>, _, _) ->
            {ok, 204, [], <<"">>}
        end),
    ok = test_utils:mock_expect(Nodes, handle_proxy_client, delete,
        fun(_, <<"/handle", _/binary>>, _, _) ->
            {ok, 200, [], <<"">>}
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