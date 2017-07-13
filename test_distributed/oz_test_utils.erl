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
    call_oz/4
]).
% Operations corresponding to logic modules
-export([
    create_user/2,
    create_client_token/2,
    get_user/2,
    get_user_oz_privileges/2,
    get_user_eff_oz_privileges/2,
    get_user_groups/2,
    get_user_eff_groups/2,
    list_users/1,
    set_user_oz_privileges/4,
    set_user_default_space/3,
    unset_user_default_space/2,
    set_user_space_alias/4,
    unset_user_space_alias/3,
    set_user_default_provider/3,
    unset_user_default_provider/2,
    delete_user/2,

    user_leave_space/3
]).
-export([
    create_group/3,
    get_group/2,
    get_group_oz_privileges/2,
    get_group_eff_oz_privileges/2,
    list_groups/1,
    set_group_oz_privileges/4,
    delete_group/2,

    add_user_to_group/3,
    add_group_to_group/3,
    group_remove_user/3,
    group_leave_space/3,

    get_group_user_privileges/3,
    get_group_eff_user_privileges/3
]).
-export([
    create_space/3,
    get_space/2,
    list_spaces/1,
    update_space/3,
    delete_space/2,

    add_user_to_space/3,
    add_group_to_space/3,
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
    get_provider/2,
    list_providers/1,
    delete_provider/2,
    support_space/4,
    support_space/5
]).
-export([
    create_handle_service/5, create_handle_service/3,
    list_handle_services/1,
    delete_handle_service/2,
    add_user_to_handle_service/3,
    add_group_to_handle_service/3
]).
-export([
    create_handle/6, create_handle/3,
    get_handle/2,
    list_handles/1,
    update_handle/3, update_handle/5,
    delete_handle/2,
    add_user_to_handle/3,
    add_group_to_handle/3
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
-spec get_user_oz_privileges(Config :: term(), UserId :: od_user:id()) ->
    {ok, [privileges:oz_privilege()]}.
get_user_oz_privileges(Config, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, user_logic, get_oz_privileges, [
        ?ROOT, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns effective OZ privileges of a user.
%% @end
%%--------------------------------------------------------------------
-spec get_user_eff_oz_privileges(Config :: term(), UserId :: od_user:id()) ->
    {ok, [privileges:oz_privilege()]}.
get_user_eff_oz_privileges(Config, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, user_logic, get_eff_oz_privileges, [
        ?ROOT, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns groups of a user.
%% @end
%%--------------------------------------------------------------------
-spec get_user_groups(Config :: term(), UserId :: od_user:id()) ->
    {ok, [od_group:id()]}.
get_user_groups(Config, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, user_logic, get_groups, [
        ?ROOT, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns effective groups of a user.
%% @end
%%--------------------------------------------------------------------
-spec get_user_eff_groups(Config :: term(), UserId :: od_user:id()) ->
    {ok, [od_group:id()]}.
get_user_eff_groups(Config, UserId) ->
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
-spec set_user_oz_privileges(Config :: term(), UserId :: od_user:id(),
    Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:oz_privilege()]) -> ok.
set_user_oz_privileges(Config, UserId, Operation, Privileges) ->
    ?assertMatch(ok, call_oz(Config, user_logic, update_oz_privileges, [
        ?ROOT, UserId, Operation, Privileges
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets default space of a user.
%% @end
%%--------------------------------------------------------------------
-spec set_user_default_space(Config :: term(), UserId :: od_user:id(),
    SpaceId :: od_space:id()) -> ok.
set_user_default_space(Config, UserId, SpaceId) ->
    ?assertMatch(ok, call_oz(Config, user_logic, set_default_space, [
        ?ROOT, UserId, SpaceId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Unsets default space of a user.
%% @end
%%--------------------------------------------------------------------
-spec unset_user_default_space(Config :: term(), UserId :: od_user:id()) -> ok.
unset_user_default_space(Config, UserId) ->
    ?assertMatch(ok, call_oz(Config, user_logic, unset_default_space, [
        ?ROOT, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets alias for a space of a user.
%% @end
%%--------------------------------------------------------------------
-spec set_user_space_alias(Config :: term(), UserId :: od_user:id(),
    SpaceId :: od_space:id(), Alias :: binary()) -> ok.
set_user_space_alias(Config, UserId, SpaceId, Alias) ->
    ?assertMatch(ok, call_oz(Config, user_logic, set_space_alias, [
        ?ROOT, UserId, SpaceId, Alias
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets alias for a space of a user.
%% @end
%%--------------------------------------------------------------------
-spec unset_user_space_alias(Config :: term(), UserId :: od_user:id(),
    SpaceId :: od_space:id()) -> ok.
unset_user_space_alias(Config, UserId, SpaceId) ->
    ?assertMatch(ok, call_oz(Config, user_logic, delete_space_alias, [
        ?ROOT, UserId, SpaceId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Sets default provider of a user.
%% @end
%%--------------------------------------------------------------------
-spec set_user_default_provider(Config :: term(), UserId :: od_user:id(),
    ProviderId :: od_provider:id()) -> ok.
set_user_default_provider(Config, UserId, ProviderId) ->
    ?assertMatch(ok, call_oz(Config, user_logic, set_default_provider, [
        ?ROOT, UserId, ProviderId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Unsets default provider of a user.
%% @end
%%--------------------------------------------------------------------
-spec unset_user_default_provider(Config :: term(), UserId :: od_user:id()) ->
    ok.
unset_user_default_provider(Config, UserId) ->
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
%% Creates group in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Config :: term(), Client :: entity_logic:client(),
    Name :: od_group:name()) -> {ok, Id :: binary()}.
create_group(Config, Client, Name) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, create, [Client, Name]
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
%% Returns OZ privileges of a group.
%% @end
%%--------------------------------------------------------------------
-spec get_group_oz_privileges(Config :: term(), GroupId :: od_group:id()) ->
    {ok, [privileges:oz_privilege()]}.
get_group_oz_privileges(Config, GroupId) ->
    ?assertMatch({ok, _}, call_oz(Config, group_logic, get_oz_privileges, [
        ?ROOT, GroupId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns effective OZ privileges of a group.
%% @end
%%--------------------------------------------------------------------
-spec get_group_eff_oz_privileges(Config :: term(), GroupId :: od_group:id()) ->
    {ok, [privileges:oz_privilege()]}.
get_group_eff_oz_privileges(Config, GroupId) ->
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
-spec set_group_oz_privileges(Config :: term(), GroupId :: od_group:id(),
    Operation :: entity_graph:privileges_operation(),
    Privileges :: [privileges:oz_privilege()]) -> ok.
set_group_oz_privileges(Config, GroupId, Operation, Privileges) ->
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
-spec add_user_to_group(Config :: term(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> {ok, UserId :: od_user:id()}.
add_user_to_group(Config, GroupId, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, group_logic, add_user, [?ROOT, GroupId, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Adds a group a to group in onezone.
%% @end
%%--------------------------------------------------------------------
-spec add_group_to_group(Config :: term(), GroupId :: od_group:id(),
    ChildGroupId :: od_group:id()) -> {ok, ChildGroupId :: od_group:id()}.
add_group_to_group(Config, GroupId, ChildGroupId) ->
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
%% Returns privileges of a user in given group.
%% @end
%%--------------------------------------------------------------------
-spec get_group_user_privileges(Config :: term(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> {ok, [privileges:oz_privilege()]}.
get_group_user_privileges(Config, GroupId, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, group_logic, get_user_privileges, [
        ?ROOT, GroupId, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Returns effective privileges of a user in given group.
%% @end
%%--------------------------------------------------------------------
-spec get_group_eff_user_privileges(Config :: term(), GroupId :: od_group:id(),
    UserId :: od_user:id()) -> {ok, [privileges:oz_privilege()]}.
get_group_eff_user_privileges(Config, GroupId, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, group_logic, get_eff_user_privileges, [
        ?ROOT, GroupId, UserId
    ])).


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
%% Adds a user to a space.
%% @end
%%--------------------------------------------------------------------
-spec add_user_to_space(Config :: term(), SpaceId :: od_space:id(),
    UserId :: od_user:id()) -> {ok, od_user:id()}.
add_user_to_space(Config, SpaceId, UserId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, add_user, [?ROOT, SpaceId, UserId]
    )).


%%--------------------------------------------------------------------
%% @doc
%% Adds a group to a space.
%% @end
%%--------------------------------------------------------------------
-spec add_group_to_space(Config :: term(), SpaceId :: od_space:id(),
    GroupId :: od_group:id()) -> {ok, od_group:id()}.
add_group_to_space(Config, SpaceId, GroupId) ->
    ?assertMatch({ok, _}, call_oz(
        Config, space_logic, add_group, [?ROOT, SpaceId, GroupId]
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
%% Deletes given share from onezone.
%% @end
%%--------------------------------------------------------------------
-spec delete_share(Config :: term(), ShareId :: od_share:id()) -> ok.
delete_share(Config, ShareId) ->
    ?assertMatch(ok, call_oz(Config, share_logic, delete, [?ROOT, ShareId])).


%%--------------------------------------------------------------------
%% @doc
%% Creates a provider (automatically generates certificates).
%% @end
%%--------------------------------------------------------------------
-spec create_provider_and_certs(Config :: term(), Name :: od_provider:name()) ->
    {ok, {ProviderId :: binary(), KeyFile :: string(), CertFile :: string()}}.
create_provider_and_certs(Config, Name) ->
    {KeyFile, CSRFile, CertFile} = generate_provider_cert_files(),
    {ok, CSR} = file:read_file(CSRFile),
    {ok, {ProviderId, Certificate}} = ?assertMatch({ok, _}, call_oz(
        Config, provider_logic, create, [
            ?NOBODY,
            Name,
            [<<"127.0.0.1">>],
            <<"127.0.0.1">>,
            CSR
        ])),
    ok = file:write_file(CertFile, Certificate),
    {ok, {ProviderId, KeyFile, CertFile}}.


%%--------------------------------------------------------------------
%% @doc
%% Creates a provider.
%% @end
%%--------------------------------------------------------------------
-spec create_provider(Config :: term(), Data :: maps:map()) ->
    {ok, {ProviderId :: binary(), KeyFile :: string(), CertFile :: string()}}.
create_provider(Config, Data) ->
    ?assertMatch({ok, _}, call_oz(
        Config, provider_logic, create, [?NOBODY, Data]
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
-spec add_user_to_handle_service(Config :: term(),
    HServiceId :: od_handle_service:id(), UserId :: od_user:id()) ->
    {ok, od_user:id()}.
add_user_to_handle_service(Config, HServiceId, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, handle_service_logic, add_user, [
        ?ROOT, HServiceId, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Adds a group to a handle service.
%% @end
%%--------------------------------------------------------------------
-spec add_group_to_handle_service(Config :: term(),
    HServiceId :: od_handle_service:id(), GroupId :: od_group:id()) ->
    {ok, od_group:id()}.
add_group_to_handle_service(Config, HServiceId, GroupId) ->
    ?assertMatch({ok, _}, call_oz(Config, handle_service_logic, add_group, [
        ?ROOT, HServiceId, GroupId
    ])).


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
-spec add_user_to_handle(Config :: term(), HServiceId :: od_handle:id(),
    UserId :: od_user:id()) -> {ok, od_user:id()}.
add_user_to_handle(Config, HandleId, UserId) ->
    ?assertMatch({ok, _}, call_oz(Config, handle_logic, add_user, [
        ?ROOT, HandleId, UserId
    ])).


%%--------------------------------------------------------------------
%% @doc
%% Adds a group to a handle.
%% @end
%%--------------------------------------------------------------------
-spec add_group_to_handle(Config :: term(), HServiceId :: od_handle:id(),
    GroupId :: od_group:id()) -> {ok, od_group:id()}.
add_group_to_handle(Config, HandleId, GroupId) ->
    ?assertMatch({ok, _}, call_oz(Config, handle_logic, add_group, [
        ?ROOT, HandleId, GroupId
    ])).


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
    create_3_nested_groups(Config, TestUser, <<"gr">>, <<"gr">>, <<"gr">>).


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
    {ok, BottomGroup} = oz_test_utils:add_group_to_group(
        Config, MiddleGroup, BottomGroup
    ),
    {ok, MiddleGroup} = oz_test_utils:add_group_to_group(
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
    ?assert(call_oz(Config, entity_graph, ensure_up_to_date, [])).


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