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
-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").

%% API
-export([
    authorize_by_oz_panel_gui_token/1, authorize_by_access_token/1,
    get_protected_provider_data/2, deploy_static_gui_package/4,
    update_cluster_version_info/4, set_user_password/3, create_user/2,
    add_user_to_group/3, list_users/1, user_exists/1, username_exists/1,
    get_user_details/1, get_user_details/2, migrate_onepanel_user_to_onezone/4,
    cluster_get_eff_user_privileges/3, get_protected_cluster_data/2,
    get_clusters_by_user_auth/1, cluster_logic_get_users/2,
    cluster_logic_get_eff_users/2, cluster_logic_get_groups/2,
    cluster_logic_get_eff_groups/2, cluster_logic_create_user_invite_token/2,
    reconcile_dns_config/0, dns_config_get_ns_hosts/0
]).


%%%===================================================================
%%% API functions
%%%===================================================================

-spec authorize_by_oz_panel_gui_token(tokens:token() | tokens:serialized()) ->
    {true, aai:auth()} | {error, term()}.
authorize_by_oz_panel_gui_token(Token) ->
    auth_logic:authorize_by_oz_panel_gui_token(Token).


-spec authorize_by_access_token(tokens:serialized() | tokens:token()) ->
    {true, aai:auth()} | false | {error, term()}.
authorize_by_access_token(Token)  ->
    auth_logic:authorize_by_access_token(Token).


-spec get_protected_provider_data(Auth :: aai:auth(), ProviderId :: od_provider:id()) ->
    {ok, map()} | {error, term()}.
get_protected_provider_data(Auth, ProviderId) ->
    provider_logic:get_protected_data(Auth, ProviderId).


-spec deploy_static_gui_package(onedata:gui(), onedata:release_version(), file:name_all(), VerifyGuiHash :: boolean()) ->
    {ok, onedata:gui_hash()} | ?ERROR_BAD_GUI_PACKAGE |
    ?ERROR_GUI_PACKAGE_TOO_LARGE | ?ERROR_GUI_PACKAGE_UNVERIFIED.
deploy_static_gui_package(GuiType, ReleaseVsn, PackagePath, VerifyGuiHash) ->
    gui_static:deploy_package(GuiType, ReleaseVsn, PackagePath, VerifyGuiHash).


-spec update_cluster_version_info(Auth :: aai:auth(), ClusterId :: od_cluster:id(),
    onedata:service_type(), VersionInfo :: od_cluster:version_info()) ->
    ok | {error, term()}.
update_cluster_version_info(Auth, ClusterId, ServiceType, VersionInfo) ->
    cluster_logic:update_version_info(Auth, ClusterId, ServiceType, VersionInfo).


-spec set_user_password(Auth :: aai:auth(), UserId :: od_user:id(),
    NewPassword :: basic_auth:password()) -> ok | {error, term()}.
set_user_password(Auth, UserId, NewPassword) ->
    user_logic:set_password(Auth, UserId, NewPassword).


-spec create_user(Auth :: aai:auth(), Data :: map()) ->
    {ok, od_user:id()} | {error, term()}.
create_user(Auth, Data) ->
    user_logic:create(Auth, Data).


-spec add_user_to_group(Auth :: aai:auth(),
    GroupId :: od_group:id(), UserId :: od_user:id()) ->
    {ok, od_user:id()} | {error, term()}.
add_user_to_group(Auth, GroupId, UserId) ->
    group_logic:add_user(Auth, GroupId, UserId).


-spec list_users(Auth :: aai:auth()) ->
    {ok, [od_user:id()]} | {error, term()}.
list_users(Auth) ->
    user_logic:list(Auth).


-spec user_exists(UserId :: od_user:id()) -> boolean().
user_exists(UserId) ->
    user_logic:exists(UserId).


-spec username_exists(od_user:username()) -> boolean().
username_exists(Username) ->
    case od_user:get_by_username(Username) of
        {ok, _} -> true;
        _ -> false
    end.


-spec get_user_details(Auth :: aai:auth()) ->
    {ok, #user_details{}} | {error, term()}.
get_user_details(#auth{subject = ?SUB(user, UserId)} = Auth) ->
    get_user_details(Auth, UserId).

-spec get_user_details(Auth :: aai:auth(), UserId :: od_user:id()) ->
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
    OnepanelUsername :: binary(), basic_auth:password_hash(), Role :: regular | admin) ->
    {ok, od_user:id()}.
migrate_onepanel_user_to_onezone(OnepanelUserId, OnepanelUsername, PasswordHash, Role) ->
    basic_auth:migrate_onepanel_user_to_onezone(
        OnepanelUserId, OnepanelUsername, PasswordHash, Role).


-spec cluster_get_eff_user_privileges(Auth :: aai:auth(), ClusterId :: od_cluster:id(),
    UserId :: od_user:id()) -> {ok, [privileges:cluster_privilege()]} | {error, term()}.
cluster_get_eff_user_privileges(Auth, ClusterId, UserId) ->
    cluster_logic:get_eff_user_privileges(Auth, ClusterId, UserId).


-spec get_protected_cluster_data(Auth :: aai:auth(), ClusterId :: od_cluster:id()) ->
    {ok, map()} | {error, term()}.
get_protected_cluster_data(Auth, ClusterId) ->
    cluster_logic:get_protected_data(Auth, ClusterId).


-spec get_clusters_by_user_auth(Auth :: aai:auth()) ->
    {ok, [od_cluster:id()]} | {error, term()}.
get_clusters_by_user_auth(Auth) ->
    user_logic:get_clusters(Auth).


-spec cluster_logic_get_users(Auth :: aai:auth(), ClusterId :: od_cluster:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
cluster_logic_get_users(Auth, ClusterId) ->
    cluster_logic:get_users(Auth, ClusterId).


-spec cluster_logic_get_eff_users(Auth :: aai:auth(), ClusterId :: od_cluster:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
cluster_logic_get_eff_users(Auth, ClusterId) ->
    cluster_logic:get_eff_users(Auth, ClusterId).


-spec cluster_logic_get_groups(Auth :: aai:auth(), ClusterId :: od_cluster:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
cluster_logic_get_groups(Auth, ClusterId) ->
    cluster_logic:get_groups(Auth, ClusterId).


-spec cluster_logic_get_eff_groups(Auth :: aai:auth(), ClusterId :: od_cluster:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
cluster_logic_get_eff_groups(Auth, ClusterId) ->
    cluster_logic:get_eff_groups(Auth, ClusterId).


-spec cluster_logic_create_user_invite_token(aai:auth(), od_cluster:id()) ->
    {ok, macaroon:macaroon()} | {error, term()}.
cluster_logic_create_user_invite_token(Auth, ClusterId) ->
    cluster_logic:create_user_invite_token(Auth, ClusterId).


-spec reconcile_dns_config() -> ok.
reconcile_dns_config() ->
    node_manager_plugin:reconcile_dns_config().


-spec dns_config_get_ns_hosts() -> [{Name :: binary(), IP :: inet:ip4_address()}].
dns_config_get_ns_hosts() ->
    dns_config:get_ns_hosts().


