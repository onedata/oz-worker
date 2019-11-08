%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Module grouping all functions rpc called by Onepanel
%%% to simplify detecting their usage.
%%% @end
%%%-------------------------------------------------------------------
-module(rpc_api).
-author("Wojciech Geisler").

-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").

-export([apply/2]).
-export([
    check_token_auth/3,
    get_protected_provider_data/2, deploy_static_gui_package/4,
    update_cluster_version_info/4, set_user_password/3, create_user/2,
    add_user_to_group/3, list_users/1, user_exists/1, username_exists/1,
    get_user_details/1, get_user_details/2, migrate_onepanel_user_to_onezone/4,
    cluster_get_eff_user_privileges/3, get_protected_cluster_data/2,
    get_clusters_by_user_auth/1, cluster_logic_get_users/2,
    cluster_logic_get_eff_users/2, cluster_logic_get_groups/2,
    cluster_logic_get_eff_groups/2, cluster_logic_create_user_invite_token/2,
    reconcile_dns_config/0, dns_config_get_ns_hosts/0,
    gui_message_exists/1, get_gui_message_as_map/1, update_gui_message/3
]).


%%%===================================================================
%%% API entrypoint
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Wraps function invocation to wrap 'throw' exceptions in badrpc tuple
%% as if they were 'error' exceptions.
%% @end
%%--------------------------------------------------------------------
-spec apply(Function :: atom(), Args :: [term()]) ->
    Result :: term() | {badrpc, {'EXIT', {Reason, Stacktrace}}} when
    Reason :: term(), Stacktrace :: list().
apply(Function, Args) ->
    try
        erlang:apply(?MODULE, Function, Args)
    catch
        throw:Error ->
            Stacktrace = erlang:get_stacktrace(),
            {badrpc, {'EXIT', {Error, Stacktrace}}}
    end.


%%%===================================================================
%%% Exposed functions
%%%===================================================================

-spec check_token_auth(tokens:serialized() | tokens:token(),
    undefined | ip_utils:ip(), undefined | aai:audience()) ->
    {true, aai:auth()} | {error, term()}.
check_token_auth(Token, PeerIp, Audience)  ->
    token_auth:check_token_auth(Token, PeerIp, Audience).


-spec get_protected_provider_data(aai:auth(), od_provider:id()) ->
    {ok, map()} | {error, term()}.
get_protected_provider_data(Auth, ProviderId) ->
    provider_logic:get_protected_data(Auth, ProviderId).


-spec deploy_static_gui_package(onedata:gui(), onedata:release_version(),
    file:name_all(), VerifyGuiHash :: boolean()) ->
    {ok, onedata:gui_hash()} | ?ERROR_BAD_GUI_PACKAGE |
    ?ERROR_GUI_PACKAGE_TOO_LARGE | ?ERROR_GUI_PACKAGE_UNVERIFIED.
deploy_static_gui_package(GuiType, ReleaseVsn, PackagePath, VerifyGuiHash) ->
    gui_static:deploy_package(GuiType, ReleaseVsn, PackagePath, VerifyGuiHash).


-spec update_cluster_version_info(aai:auth(), od_cluster:id(),
    onedata:service_type(), od_cluster:version_info()) ->
    ok | {error, term()}.
update_cluster_version_info(Auth, ClusterId, ServiceType, VersionInfo) ->
    cluster_logic:update_version_info(Auth, ClusterId, ServiceType, VersionInfo).


-spec set_user_password(aai:auth(), od_user:id(),
    NewPassword :: basic_auth:password()) -> ok | {error, term()}.
set_user_password(Auth, UserId, NewPassword) ->
    user_logic:set_password(Auth, UserId, NewPassword).


-spec create_user(aai:auth(), Data :: map()) ->
    {ok, od_user:id()} | {error, term()}.
create_user(Auth, Data) ->
    user_logic:create(Auth, Data).


-spec add_user_to_group(aai:auth(), od_group:id(), od_user:id()) ->
    {ok, od_user:id()} | {error, term()}.
add_user_to_group(Auth, GroupId, UserId) ->
    group_logic:add_user(Auth, GroupId, UserId).


-spec list_users(aai:auth()) -> {ok, [od_user:id()]} | {error, term()}.
list_users(Auth) ->
    user_logic:list(Auth).


-spec user_exists(od_user:id()) -> boolean().
user_exists(UserId) ->
    user_logic:exists(UserId).


-spec username_exists(od_user:username()) -> boolean().
username_exists(Username) ->
    case od_user:get_by_username(Username) of
        {ok, _} -> true;
        _ -> false
    end.


-spec get_user_details(aai:auth()) -> {ok, #user_details{}} | {error, term()}.
get_user_details(#auth{subject = ?SUB(user, UserId)} = Auth) ->
    get_user_details(Auth, UserId).

-spec get_user_details(aai:auth(), od_user:id()) ->
    {ok, #user_details{}} | {error, term()}.
get_user_details(Auth, UserId) ->
    case user_logic:get_protected_data(Auth, UserId) of
        {ok, Map} ->
            #{
                <<"fullName">> := FullName, <<"username">> := Username,
                <<"linkedAccounts">> := Accounts, <<"emails">> := Emails
            } = Map,
            {ok, #user_details{
                id = UserId,
                full_name = FullName,
                username = Username,
                linked_accounts = Accounts,
                emails = Emails
            }};
        {error, _} = Error ->
            Error
    end.


-spec migrate_onepanel_user_to_onezone(OnepanelUserId :: binary(),
    OnepanelUsername :: binary(), basic_auth:password_hash(),
    Role :: regular | admin) -> {ok, od_user:id()}.
migrate_onepanel_user_to_onezone(OnepanelUserId, OnepanelUsername, PasswordHash, Role) ->
    basic_auth:migrate_onepanel_user_to_onezone(
        OnepanelUserId, OnepanelUsername, PasswordHash, Role).


-spec cluster_get_eff_user_privileges(aai:auth(), od_cluster:id(),
    od_user:id()) -> {ok, [privileges:cluster_privilege()]} | {error, term()}.
cluster_get_eff_user_privileges(Auth, ClusterId, UserId) ->
    cluster_logic:get_eff_user_privileges(Auth, ClusterId, UserId).


-spec get_protected_cluster_data(aai:auth(), od_cluster:id()) ->
    {ok, map()} | {error, term()}.
get_protected_cluster_data(Auth, ClusterId) ->
    cluster_logic:get_protected_data(Auth, ClusterId).


-spec get_clusters_by_user_auth(aai:auth()) ->
    {ok, [od_cluster:id()]} | {error, term()}.
get_clusters_by_user_auth(Auth) ->
    user_logic:get_clusters(Auth).


-spec cluster_logic_get_users(aai:auth(), od_cluster:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
cluster_logic_get_users(Auth, ClusterId) ->
    cluster_logic:get_users(Auth, ClusterId).


-spec cluster_logic_get_eff_users(aai:auth(), od_cluster:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
cluster_logic_get_eff_users(Auth, ClusterId) ->
    cluster_logic:get_eff_users(Auth, ClusterId).


-spec cluster_logic_get_groups(aai:auth(), od_cluster:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
cluster_logic_get_groups(Auth, ClusterId) ->
    cluster_logic:get_groups(Auth, ClusterId).


-spec cluster_logic_get_eff_groups(aai:auth(), od_cluster:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
cluster_logic_get_eff_groups(Auth, ClusterId) ->
    cluster_logic:get_eff_groups(Auth, ClusterId).


-spec cluster_logic_create_user_invite_token(aai:auth(), od_cluster:id()) ->
    {ok, tokens:token()} | {error, term()}.
cluster_logic_create_user_invite_token(Auth, ClusterId) ->
    cluster_logic:create_user_invite_token(Auth, ClusterId).


-spec reconcile_dns_config() -> ok.
reconcile_dns_config() ->
    node_manager_plugin:reconcile_dns_config().


-spec dns_config_get_ns_hosts() -> [{Name :: binary(), IP :: inet:ip4_address()}].
dns_config_get_ns_hosts() ->
    dns_config:get_ns_hosts().


-spec gui_message_exists(gui_message:id()) -> boolean().
gui_message_exists(MessageId) ->
    zone_logic:gui_message_exists(MessageId).


-spec get_gui_message_as_map(gui_message:id()) ->
    {ok, gui_message:map_repr()} | {error, term()}.
get_gui_message_as_map(MessageId) ->
    zone_logic:get_gui_message_as_map(MessageId).


-spec update_gui_message(aai:auth(), gui_message:id(), Data :: map()) ->
    ok | {error, term()}.
update_gui_message(Auth, MessageId, Data) ->
    zone_logic:update_gui_message(Auth, MessageId, Data).
