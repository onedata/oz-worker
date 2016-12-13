%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2014 ACK CYFRONET AGH
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
-export([call_oz/4, generate_provider_cert_files/0, ensure_eff_graph_up_to_date/1]).

-export([create_user/2, get_user/2, get_client_token/2, remove_user/2]).
-export([set_user_oz_privileges/4]).

-export([create_group/3, get_group/2, remove_group/2]).
-export([add_user_to_group/4, add_group_to_group/4, group_remove_user/3]).
-export([set_group_oz_privileges/3]).

-export([
    create_space/3,
    add_user_to_space/4,
    add_group_to_space/4,
    space_invite_provider_token/3,
    space_invite_user_token/3,
    leave_space/3,
    remove_space/2]).
-export([modify_space/4, set_space_privileges/4]).
-export([space_has_effective_user/3]).

-export([create_share/5, remove_share/2]).

-export([
    create_provider_and_certs/2,
    create_provider/2,
    support_space/5,
    remove_provider/2
]).

-export([create_handle_service/5, remove_handle_service/2]).

-export([create_handle/6, remove_handle/2, modify_handle/5]).

-export([remove_all_entities/1, remove_all_entities/2]).

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
%% @doc Creates user in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_user(Config :: term(), User :: #od_user{}) ->
    {ok, Id :: binary()} | {error, Reason :: term()}.
create_user(Config, User) ->
    call_oz(Config, n_user_logic, create, [User]).


%%--------------------------------------------------------------------
%% @doc Retrieves user data from onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_user(Config :: term(), UserId :: binary()) ->
    {ok, #document{}} | {error, Reason :: term()}.
get_user(Config, UserId) ->
    call_oz(Config, od_user, get, [UserId]).


%%--------------------------------------------------------------------
%% @doc Creates a client token for given user.
%% @end
%%--------------------------------------------------------------------
-spec get_client_token(Config :: term(), UserId :: binary()) ->
    {ok, Id :: binary()} | {error, Reason :: term()}.
get_client_token(Config, UserId) ->
    call_oz(Config, auth_logic, gen_token, [UserId]).

%%--------------------------------------------------------------------
%% @doc Removes user from onezone.
%% @end
%%--------------------------------------------------------------------
-spec remove_user(Config :: term(), UserId :: binary()) ->
    boolean() | {error, Reason :: term()}.
remove_user(Config, UserId) ->
    call_oz(Config, user_logic, remove, [UserId]).

%%--------------------------------------------------------------------
%% @doc Sets OZ privileges of a user.
%% @end
%%--------------------------------------------------------------------
-spec set_user_oz_privileges(Config :: term(), UserId :: binary(),
    Operation :: set | grant | revoke, Privileges :: [privileges:oz_privilege()]) ->
    ok | {error, Reason :: term()}.
set_user_oz_privileges(Config, UserId, Operation, Privileges) ->
    call_oz(Config, n_user_logic, set_oz_privileges, [
        ?ROOT, UserId, Operation, Privileges
    ]).

%%--------------------------------------------------------------------
%% @doc Creates group in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Config :: term(), UserId :: binary(), Name :: binary()) ->
    {ok, Id :: binary()} | {error, Reason :: term()}.
create_group(Config, Client, Name) ->
    call_oz(Config, n_group_logic, create, [Client, Name]).


%%--------------------------------------------------------------------
%% @doc Retrieves user data from onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_group(Config :: term(), GroupId :: od_group:id()) ->
    {ok, #document{}} | {error, Reason :: term()}.
get_group(Config, GroupId) ->
    call_oz(Config, od_group, get, [GroupId]).


%%--------------------------------------------------------------------
%% @doc Adds a user or group to group in onezone.
%% @end
%%--------------------------------------------------------------------
-spec add_user_to_group(Config :: term(), Client :: n_entity_logic:client(),
    GroupId :: od_group:id(), UserId :: od_user:id()) ->
    {ok, GroupId :: od_group:id()} | {error, Reason :: term()}.
add_user_to_group(Config, Client, GroupId, UserId) ->
    call_oz(Config, n_group_logic, add_user, [Client, GroupId, UserId]).

add_group_to_group(Config, Client, ParentGroupId, ChildGroupId) ->
    call_oz(Config, n_group_logic, add_group, [Client, ParentGroupId, ChildGroupId]).


%%--------------------------------------------------------------------
%% @doc Removes user from a group from onezone.
%% @end
%%--------------------------------------------------------------------
-spec group_remove_user(Config :: term(), GroupId :: od_group:id(),
    UserId :: binary()) -> true | {error, Reason :: term()}.
group_remove_user(Config, GroupId, UserId) ->
    call_oz(Config, group_logic, remove_user, [GroupId, UserId]).


%%--------------------------------------------------------------------
%% @doc Removes group from onezone.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(Config :: term(), GroupId :: od_group:id()) ->
    boolean() | {error, Reason :: term()}.
remove_group(Config, GroupId) ->
    call_oz(Config, group_logic, remove, [GroupId]).

%%--------------------------------------------------------------------
%% @doc Sets OZ privileges of a group.
%% @end
%%--------------------------------------------------------------------
-spec set_group_oz_privileges(Config :: term(), GroupId :: od_group:id(),
    Privileges :: [privileges:oz_privilege()]) ->
    ok | {error, Reason :: term()}.
set_group_oz_privileges(Config, GroupId, Privileges) ->
    call_oz(Config, group_logic, set_oz_privileges, [GroupId, Privileges]).


%%--------------------------------------------------------------------
%% @doc Creates space in onezone.
%% @end
%%--------------------------------------------------------------------
-spec create_space(Config :: term(), Member :: {user | group, Id :: binary()},
    Name :: binary()) -> {ok, Id :: binary()} | {error, Reason :: term()}.
create_space(Config, Client, Name) ->
    call_oz(Config, n_space_logic, create, [Client, Name]).


%%--------------------------------------------------------------------
%% @doc Joins space as a user or a group.
%% @end
%%--------------------------------------------------------------------
-spec add_user_to_space(Config :: term(), Client :: n_entity_logic:client(),
    SpaceId :: od_space:id(), UserId :: od_user:id()) ->
    {ok, SpaceId :: od_space:id()} | {error, Reason :: term()}.
add_user_to_space(Config, Client, SpaceId, UserId) ->
    call_oz(Config, n_space_logic, add_user, [Client, SpaceId, UserId]).

add_group_to_space(Config, Client, SpaceId, GroupId) ->
    call_oz(Config, n_space_logic, add_group, [Client, SpaceId, GroupId]).



space_invite_provider_token(Config, Client, SpaceId) ->
    call_oz(Config, n_space_logic, create_invite_provider_token, [
        Client, SpaceId
    ]).


space_invite_user_token(Config, Client, SpaceId) ->
    call_oz(Config, n_space_logic, create_invite_user_token, [
        Client, SpaceId
    ]).





%%--------------------------------------------------------------------
%% @doc Leaves space as a user or a group.
%% @end
%%--------------------------------------------------------------------
-spec leave_space(Config :: term(), {user | group, Id :: binary()},
    SpaceId :: od_space:id()) -> boolean() | {error, Reason :: term()}.
leave_space(Config, {user, UserId}, SpaceId) ->
    call_oz(Config, space_logic, remove_user, [SpaceId, UserId]);

leave_space(Config, {group, GroupId}, SpaceId) ->
    call_oz(Config, space_logic, remove_group, [SpaceId, GroupId]).

%%--------------------------------------------------------------------
%% @doc Sets privileges in a space for a user or group.
%% @end
%%--------------------------------------------------------------------
-spec set_space_privileges(Config :: term(), Member :: {user, Id :: binary()},
    SpaceId :: od_space:id(), Privileges :: [privileges:space_privilege()]) ->
    ok | {error, Reason :: term()}.
set_space_privileges(Config, Member, SpaceId, Privileges) ->
    call_oz(Config, space_logic, set_privileges, [SpaceId, Member, Privileges]).


%%--------------------------------------------------------------------
%% @doc Checks if given space has given effective user.
%% @end
%%--------------------------------------------------------------------
-spec space_has_effective_user(Config :: term(), SpaceId :: od_space:id(),
    UserId :: binary()) -> boolean() | {error, Reason :: term()}.
space_has_effective_user(Config, SpaceId, UserId) ->
    call_oz(Config, space_logic, has_effective_user, [SpaceId, UserId]).


%%--------------------------------------------------------------------
%% @doc Modifies space name.
%% @end
%%--------------------------------------------------------------------
-spec modify_space(Config :: term(), SpaceId :: od_space:id(),
    Member :: {user, Id :: binary()} | provider, Name :: binary()) ->
    {ok, Id :: binary()} | {error, Reason :: term()}.
modify_space(Config, SpaceId, Member, Name) ->
    call_oz(Config, space_logic, modify, [SpaceId, Member, Name]).


%%--------------------------------------------------------------------
%% @doc Removes space.
%% @end
%%--------------------------------------------------------------------
-spec remove_space(Config :: term(), SpaceId :: od_space:id()) ->
    boolean() | {error, Reason :: term()}.
remove_space(Config, SpaceId) ->
    call_oz(Config, space_logic, remove, [SpaceId]).


%%--------------------------------------------------------------------
%% @doc Creates share.
%% @end
%%--------------------------------------------------------------------
-spec create_share(Config :: term(), ShareId :: binary(),
    Name :: binary(), RootFileId :: binary(), ParentSpaceId :: od_space:id()) ->
    {ok, ShareId :: binary()} | {error, Reason :: term()}.
create_share(Config, ShareId, Name, RootFileId, ParentSpaceId) ->
    call_oz(Config, share_logic, create, [ShareId, Name, RootFileId, ParentSpaceId]).


%%--------------------------------------------------------------------
%% @doc Removes share.
%% @end
%%--------------------------------------------------------------------
-spec remove_share(Config :: term(), ShareId :: binary()) ->
    boolean() | {error, Reason :: term()}.
remove_share(Config, ShareId) ->
    call_oz(Config, share_logic, remove, [ShareId]).


%%--------------------------------------------------------------------
%% @doc Creates a provider.
%% @end
%%--------------------------------------------------------------------
-spec create_provider_and_certs(Config :: term(), Name :: binary()) ->
    {ok, {ProviderId :: binary(), KeyFile :: string(), CertFile :: string()}}.
create_provider_and_certs(Config, Name) ->
    {KeyFile, CSRFile, CertFile} = generate_provider_cert_files(),
    {ok, CSR} = file:read_file(CSRFile),
    {ok, {ProviderId, Certificate}} = call_oz(Config, n_provider_logic, create, [
        ?NOBODY,
        Name,
        [<<"127.0.0.1">>],
        <<"127.0.0.1">>,
        CSR
    ]),
    ok = file:write_file(CertFile, Certificate),
    {ok, {ProviderId, KeyFile, CertFile}}.


create_provider(Config, Data) ->
    {ok, {ProviderId, Certificate}} = call_oz(
        Config, n_provider_logic, create, [?NOBODY, Data]),
    {ok, {ProviderId, Certificate}}.


%%--------------------------------------------------------------------
%% @doc Supports space by provider.
%% @end
%%--------------------------------------------------------------------
support_space(Config, Client, ProviderId, Token, Size) ->
    call_oz(Config, n_space_logic, support_space, [
        Client, ProviderId, Token, Size
    ]).


%%--------------------------------------------------------------------
%% @doc Removes a provider.
%% @end
%%--------------------------------------------------------------------
-spec remove_provider(Config :: term(), ProviderId :: binary()) ->
    boolean() | {error, Reason :: term()}.
remove_provider(Config, ProviderId) ->
    call_oz(Config, provider_logic, remove, [ProviderId]).


%%--------------------------------------------------------------------
%% @doc Creates a handle_service
%% @end
%%--------------------------------------------------------------------
-spec create_handle_service(Config :: term(), UserId :: od_user:id(),
    Name :: od_handle_service:name(),
    ProxyEndpoint :: od_handle_service:proxy_endpoint(),
    ServiceProperties :: od_handle_service:service_properties()) ->
    {ok, HandleServiceId :: od_handle_service:id()}.
create_handle_service(Config, UserId, Name, ProxyEndpoint, ServiceProperties) ->
    call_oz(Config, handle_service_logic, create,
        [UserId, Name, ProxyEndpoint, ServiceProperties]).

%%--------------------------------------------------------------------
%% @doc Removes handle_service
%% @end
%%--------------------------------------------------------------------
-spec remove_handle_service(Config :: term(), HandleServiceId :: od_handle_service:id()) -> boolean().
remove_handle_service(Config, HandleServiceId) ->
    call_oz(Config, handle_service_logic, remove, [HandleServiceId]).


%%--------------------------------------------------------------------
%% @doc Creates a handle.
%% @end
%%--------------------------------------------------------------------
-spec create_handle(Config :: term(), od_user:id(), od_handle_service:id(),
    od_handle:resource_type(), od_handle:resource_id(), od_handle:metadata()) ->
    {ok, od_handle:id()}.
create_handle(Config, UserId, HandleServiceId, ResourceType, ResourceId, Metadata) ->
    call_oz(Config, handle_logic, create,
        [UserId, HandleServiceId, ResourceType, ResourceId, Metadata]).

%%--------------------------------------------------------------------
%% @doc Removes handle
%% @end
%%--------------------------------------------------------------------
-spec remove_handle(Config :: term(), HandleId :: od_handle:id()) -> boolean().
remove_handle(Config, HandleId) ->
    call_oz(Config, handle_logic, remove, [HandleId]).


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
%% @doc Removes all entities from onezone
%% (users, groups, spaces, shares, providers).
%% NOTE: Does not remove predefined groups! Use remove_all_entities/2 for that.
%% @end
%%--------------------------------------------------------------------
-spec remove_all_entities(Config :: term()) -> ok | {error, Reason :: term()}.
remove_all_entities(Config) ->
    remove_all_entities(Config, false).


%%--------------------------------------------------------------------
%% @doc Removes all entities from onezone
%% (users, groups, spaces, shares, providers).
%% RemovePredefinedGroups decides if predefined groups should be removed too.
%% @end
%%--------------------------------------------------------------------
-spec remove_all_entities(Config :: term(),
    RemovePredefinedGroups :: boolean()) -> ok | {error, Reason :: term()}.
remove_all_entities(Config, RemovePredefinedGroups) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    % Delete all providers
    {ok, PrDocs} = call_oz(Config, od_provider, list, []),
    [true = remove_provider(Config, PId) || #document{key = PId} <- PrDocs],
    % Delete all shares
    {ok, ShareDocs} = call_oz(Config, od_share, list, []),
    [true = remove_share(Config, SId) || #document{key = SId} <- ShareDocs],
    % Delete all spaces
    {ok, SpaceDocs} = call_oz(Config, od_space, list, []),
    [true = remove_space(Config, SId) || #document{key = SId} <- SpaceDocs],
    {ok, HandleDocs} = call_oz(Config, od_handle, list, []),
    [true = remove_handle(Config, HId) || #document{key = HId} <- HandleDocs],
    {ok, HandleServiceDocs} = call_oz(Config, od_handle_service, list, []),
    [true = remove_handle_service(Config, HSId) || #document{key = HSId} <- HandleServiceDocs],
    % Delete all groups, excluding predefined groups
    {ok, GroupDocsAll} = call_oz(Config, od_group, list, []),
    % Check if predefined groups should be removed too.
    GroupDocs = case RemovePredefinedGroups of
        true ->
            GroupDocsAll;
        false ->
            % Filter out predefined groups
            {ok, PredefinedGroupsMapping} = test_utils:get_env(
                Node, oz_worker, predefined_groups
            ),
            PredefinedGroups = [Id || #{id := Id} <- PredefinedGroupsMapping],
            lists:filter(
                fun(#document{key = GroupId}) ->
                    not lists:member(GroupId, PredefinedGroups)
                end, GroupDocsAll)
    end,
    [true = remove_group(Config, GId) || #document{key = GId} <- GroupDocs],
    % Delete all users
    {ok, UserDocs} = call_oz(Config, od_user, list, []),
    [true = remove_user(Config, UId) || #document{key = UId} <- UserDocs],
    ok.


%%--------------------------------------------------------------------
%% @doc Creates a key/cert pair and a corresponding CSR for
%% provider registration.
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
%% @doc Returns when effective graph has been fully recalculated.
%% @end
%%--------------------------------------------------------------------
-spec ensure_eff_graph_up_to_date(Config :: proplists:proplist()) -> ok.
ensure_eff_graph_up_to_date(Config) ->
    oz_test_utils:call_oz(Config, entity_graph, ensure_up_to_date, []).