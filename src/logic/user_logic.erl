%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all user logic functionality.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(user_logic).
-author("Lukasz Opiola").

-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").

% SSL Opts used to connect to onepanel.
% Onepanel is usually available under 127.0.0.1 or localhost, so hostname
% verification should be omitted here (the cert signature is still checked).
-define(ONEPANEL_CONNECT_OPTS, [
    {recv_timeout, 10000}, {ssl_options, [
        {secure, only_verify_peercert},
        {cacerts, https_listener:get_cert_chain_ders()}
    ]}
]).

-export([
    create/1, create/2, create/3,
    create_client_token/2
]).
-export([
    list/1,
    get/2,
    get_protected_data/2, get_protected_data/3,
    get_shared_data/2, get_shared_data/3,
    get_oz_privileges/2, get_eff_oz_privileges/2,
    list_client_tokens/2,
    get_space_alias/3,
    get_space_membership_requests/2,
    acquire_idp_access_token/3
]).
-export([
    update_full_name/3, update_username/3, update/3,
    change_password/3, change_password/4,
    toggle_basic_auth/3, set_password/3, update_basic_auth_config/3,
    toggle_access_block/3,
    update_oz_privileges/4, update_oz_privileges/3,
    set_space_alias/4
]).
-export([
    delete/2,
    delete_oz_privileges/2,
    delete_client_token/3,
    delete_space_alias/3
]).
-export([
    create_provider_registration_token/2,
    create_group/3, create_group/4,
    create_space/3,
    create_handle_service/5, create_handle_service/3,
    create_handle/7, create_handle/3,
    create_harvester/3, create_harvester/6,
    create_atm_inventory/3,

    join_group/3,
    join_space/3,
    join_harvester/3,
    join_cluster/3,
    join_atm_inventory/3,

    get_full_name/2,

    get_groups/2, get_eff_groups/2,
    get_group/3, get_eff_group/3,

    get_spaces/2, get_eff_spaces/2,
    get_space/3, get_eff_space/3,

    get_eff_providers/2, get_eff_provider/3,
    get_spaces_in_eff_provider/3,

    get_handle_services/2, get_eff_handle_services/2,
    get_handle_service/3, get_eff_handle_service/3,

    get_handles/2, get_eff_handles/2,
    get_handle/3, get_eff_handle/3,

    get_harvesters/2, get_eff_harvesters/2,
    get_harvester/3, get_eff_harvester/3,

    get_eff_clusters/1, get_clusters/2, get_eff_clusters/2,
    get_cluster/3, get_eff_cluster/3,

    get_atm_inventories/2, get_eff_atm_inventories/2,
    get_atm_inventory/3, get_eff_atm_inventory/3,

    leave_group/3,
    leave_space/3,
    leave_handle_service/3,
    leave_handle/3,
    leave_harvester/3,
    leave_cluster/3,
    leave_atm_inventory/3
]).
-export([
    exists/1,
    has_eff_oz_privilege/2,
    has_eff_group/2,
    has_eff_space/2,
    has_eff_provider/2,
    has_eff_handle_service/2,
    has_eff_handle/2,
    has_eff_harvester/2,
    has_eff_cluster/2,
    has_eff_atm_inventory/2
]).
-export([
    validate_full_name/1, normalize_full_name/1,
    validate_username/1, normalize_username/1,
    reset_entitlements/1
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new user document in database. No full_name/username is set.
%% @end
%%--------------------------------------------------------------------
-spec create(Auth :: aai:auth()) ->
    {ok, od_user:id()} | errors:error().
create(Auth) ->
    create(Auth, #{}).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new user document in database. full_name, username and password
%% can be provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create(Auth :: aai:auth(), Data :: map()) ->
    {ok, od_user:id()} | errors:error().
create(Auth, Data) ->
    create(Auth, undefined, Data).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new user document in database with proposed UserId.
%% full_name, username and password can be provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create(Auth :: aai:auth(), ProposedUserId :: undefined | od_user:id(), Data :: map()) ->
    {ok, od_user:id()} | errors:error().
create(Auth, ProposedUserId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_user, id = ProposedUserId, aspect = instance},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new client token for given user. Appends the token to the list
%% of user's tokens and returns the token.
%% @end
%%--------------------------------------------------------------------
-spec create_client_token(Auth :: aai:auth(),
    UserId :: od_user:id()) -> {ok, Token :: binary()} | errors:error().
create_client_token(Auth, UserId) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = client_tokens},
        data = #{}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Lists all users (their ids) in database.
%% @end
%%--------------------------------------------------------------------
-spec list(Auth :: aai:auth()) ->
    {ok, [od_user:id()]} | errors:error().
list(Auth) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = undefined, aspect = list}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a user record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, #od_user{}} | errors:error().
get(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves protected user data from database.
%% @end
%%--------------------------------------------------------------------
-spec get_protected_data(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, map()} | errors:error().
get_protected_data(Auth, UserId) ->
    get_protected_data(Auth, UserId, undefined).

get_protected_data(Auth, UserId, AuthHint) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = protected},
        auth_hint = AuthHint
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves shared user data from database.
%% @end
%%--------------------------------------------------------------------
-spec get_shared_data(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, map()} | errors:error().
get_shared_data(Auth, UserId) ->
    get_shared_data(Auth, UserId, undefined).


get_shared_data(Auth, UserId, AuthHint) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
        auth_hint = AuthHint
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves oz privileges of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_oz_privileges(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, [privileges:oz_privilege()]} | errors:error().
get_oz_privileges(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = oz_privileges}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves effective oz privileges of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_oz_privileges(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, [privileges:oz_privilege()]} | errors:error().
get_eff_oz_privileges(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = eff_oz_privileges}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves space alias for given space of a user. Returns ?ERROR_NOT_FOUND
%% if the user does not have a space alias for given space.
%% @end
%%--------------------------------------------------------------------
-spec get_space_alias(Auth :: aai:auth(), UserId :: od_user:id(),
    SpaceId :: od_space:id()) -> {ok, binary()} | errors:error().
get_space_alias(Auth, UserId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = {space_alias, SpaceId}}
    }).


-spec get_space_membership_requests(aai:auth(), od_user:id()) ->
    {ok, space_membership_requests:record()} | errors:error().
get_space_membership_requests(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = space_membership_requests}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Acquires an access token issued by given IdP for specific user. This
%% operation requires that the IdP is configured to support offline access.
%% @end
%%--------------------------------------------------------------------
-spec acquire_idp_access_token(aai:auth(), od_user:id(), auth_config:idp()) ->
    {ok, {idp_auth:access_token(), idp_auth:access_token_ttl()}} | errors:error().
acquire_idp_access_token(Auth, UserId, IdP) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = {idp_access_token, IdP}}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of client tokens of given user.
%% @end
%%--------------------------------------------------------------------
-spec list_client_tokens(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, [tokens:serialized()]} | errors:error().
list_client_tokens(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = client_tokens}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates the full_name of given user.
%% @end
%%--------------------------------------------------------------------
-spec update_full_name(Auth :: aai:auth(), UserId :: od_user:id(),
    NewName :: binary()) -> ok | errors:error().
update_full_name(Auth, UserId, NewName) ->
    update(Auth, UserId, #{<<"fullName">> => NewName}).


%%--------------------------------------------------------------------
%% @doc
%% Updates the username of given user.
%% @end
%%--------------------------------------------------------------------
-spec update_username(Auth :: aai:auth(), UserId :: od_user:id(),
    NewUsername :: od_user:username()) -> ok | errors:error().
update_username(Auth, UserId, NewUsername) ->
    update(Auth, UserId, #{<<"username">> => NewUsername}).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given user (full_name and username).
%% @end
%%--------------------------------------------------------------------
-spec update(Auth :: aai:auth(), UserId :: od_user:id(),
    Data :: #{}) -> ok | errors:error().
update(Auth, UserId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = instance},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Changes the password of given user (old password is required).
%% @end
%%--------------------------------------------------------------------
-spec change_password(Auth :: aai:auth(), UserId :: od_user:id(),
    OldPassword :: binary(), NewPassword :: basic_auth:password()) -> ok | errors:error().
change_password(Auth, UserId, OldPassword, NewPassword) ->
    change_password(Auth, UserId, #{
        <<"oldPassword">> => OldPassword,
        <<"newPassword">> => NewPassword
    }).

-spec change_password(Auth :: aai:auth(), UserId :: od_user:id(),
    Data :: #{}) -> ok | errors:error().
change_password(Auth, UserId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = password},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Toggles basic auth for given user
%% (if disabled, basic credentials cannot be used).
%% @end
%%--------------------------------------------------------------------
-spec toggle_basic_auth(Auth :: aai:auth(), UserId :: od_user:id(),
    BasicAuthEnabled :: boolean()) -> ok | errors:error().
toggle_basic_auth(Auth, UserId, BasicAuthEnabled) ->
    update_basic_auth_config(Auth, UserId, #{
        <<"basicAuthEnabled">> => BasicAuthEnabled
    }).


%%--------------------------------------------------------------------
%% @doc
%% Sets a new password for given user - can be used by admins only
%% (regular users must use change_password/4).
%% @end
%%--------------------------------------------------------------------
-spec set_password(Auth :: aai:auth(), UserId :: od_user:id(),
    NewPassword :: basic_auth:password()) -> ok | errors:error().
set_password(Auth, UserId, NewPassword) ->
    update_basic_auth_config(Auth, UserId, #{
        <<"newPassword">> => NewPassword
    }).


%%--------------------------------------------------------------------
%% @doc
%% Changes the basic auth config of given user: whether basic auth is enabled
%% for him and his password.
%% @end
%%--------------------------------------------------------------------
-spec update_basic_auth_config(Auth :: aai:auth(), UserId :: od_user:id(),
    Data :: map()) -> ok | errors:error().
update_basic_auth_config(Auth, UserId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = basic_auth},
        data = Data
    }).


-spec toggle_access_block(aai:auth(), od_user:id(), boolean() | entity_logic:data()) -> ok | errors:error().
toggle_access_block(Auth, UserId, Blocked) when is_boolean(Blocked) ->
    toggle_access_block(Auth, UserId, #{<<"blocked">> => Blocked});
toggle_access_block(Auth, UserId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = access_block},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates oz privileges of given user.
%% Allows to specify privileges to grant and to revoke.
%% @end
%%--------------------------------------------------------------------
-spec update_oz_privileges(Auth :: aai:auth(), UserId :: od_user:id(),
    PrivsToGrant :: [privileges:oz_privilege()],
    PrivsToRevoke :: [privileges:oz_privilege()]) -> ok | errors:error().
update_oz_privileges(Auth, UserId, PrivsToGrant, PrivsToRevoke) ->
    update_oz_privileges(Auth, UserId, #{
        <<"grant">> => PrivsToGrant,
        <<"revoke">> => PrivsToRevoke
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates oz privileges of given user.
%% Privileges to grant and revoke must be included in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_oz_privileges(Auth :: aai:auth(), UserId :: od_user:id(),
    Data :: #{}) -> ok | errors:error().
update_oz_privileges(Auth, UserId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = oz_privileges},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates the space alias for given space of a user. Has two variants:
%% 1) Alias is given explicitly
%% 2) Alias is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec set_space_alias(Auth :: aai:auth(), UserId :: od_user:id(),
    SpaceId :: od_space:id(), AliasOrData :: binary() | #{}) -> ok | errors:error().
set_space_alias(Auth, UserId, SpaceId, Alias) when is_binary(Alias) ->
    set_space_alias(Auth, UserId, SpaceId, #{<<"alias">> => Alias});
set_space_alias(Auth, UserId, SpaceId, Data) ->
    ?CREATE_RETURN_OK(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = {space_alias, SpaceId}},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given user from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(Auth :: aai:auth(), UserId :: od_user:id()) ->
    ok | errors:error().
delete(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Deletes (sets to empty list) oz privileges of given user.
%% @end
%%--------------------------------------------------------------------
-spec delete_oz_privileges(Auth :: aai:auth(), UserId :: od_user:id()) ->
    ok | errors:error().
delete_oz_privileges(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = oz_privileges}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given token of given user.
%% @end
%%--------------------------------------------------------------------
-spec delete_client_token(Auth :: aai:auth(), UserId :: od_user:id(),
    Serialized :: tokens:serialized()) -> ok | errors:error().
delete_client_token(Auth, UserId, Serialized) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = {client_token, Serialized}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Deletes the alias for a space of given user.
%% @end
%%--------------------------------------------------------------------
-spec delete_space_alias(Auth :: aai:auth(), UserId :: od_user:id(),
    SpaceId :: od_space:id()) -> ok | errors:error().
delete_space_alias(Auth, UserId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = {space_alias, SpaceId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a provider registration token for given user, which can be used to
%% register a new Oneprovider cluster in Onezone. The user is automatically
%% linked to the newly created Oneprovider cluster after successful
%% registration, which makes the user an admin of the cluster.
%% @end
%%--------------------------------------------------------------------
-spec create_provider_registration_token(Auth :: aai:auth(),
    UserId :: od_user:id()) -> {ok, tokens:token()} | errors:error().
create_provider_registration_token(Auth, UserId) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = provider_registration_token}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new group for given user.
%% Allows to specify group name and type.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Auth :: aai:auth(), UserId :: od_user:id(),
    Name :: binary(), Type :: od_group:type()) ->
    {ok, od_group:id()} | errors:error().
create_group(Auth, UserId, Name, Type) ->
    create_group(Auth, UserId, #{<<"name">> => Name, <<"type">> => Type}).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new group for given user. Has two variants:
%% 1) Group name is given explicitly (the new group will be of default type)
%% 2) Group name is provided in a proper Data object, group type is optional.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Auth :: aai:auth(), UserId :: od_user:id(),
    NameOrData :: binary() | #{}) -> {ok, od_group:id()} | errors:error().
create_group(Auth, UserId, Name) when is_binary(Name) ->
    create_group(Auth, UserId, #{<<"name">> => Name});
create_group(Auth, UserId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_group, id = undefined, aspect = instance},
        auth_hint = ?AS_USER(UserId),
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new space for given user. Has two variants:
%% 1) Space name is given explicitly
%% 2) Space name is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create_space(Auth :: aai:auth(), UserId :: od_user:id(),
    NameOrData :: binary() | #{}) -> {ok, od_space:id()} | errors:error().
create_space(Auth, UserId, Name) when is_binary(Name) ->
    create_space(Auth, UserId, #{<<"name">> => Name});
create_space(Auth, UserId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_space, id = undefined, aspect = instance},
        auth_hint = ?AS_USER(UserId),
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new handle service for given user.
%% Allows to specify the Name, ProxyEndpoint and ServiceProperties.
%% @end
%%--------------------------------------------------------------------
-spec create_handle_service(Auth :: aai:auth(), UserId :: od_user:id(),
    Name :: binary(), ProxyEndpoint :: od_handle_service:proxy_endpoint(),
    ServiceProperties :: od_handle_service:service_properties()) ->
    {ok, od_handle_service:id()} | errors:error().
create_handle_service(Auth, UserId, Name, ProxyEndpoint, ServiceProperties) ->
    create_handle_service(Auth, UserId, #{
        <<"name">> => Name,
        <<"proxyEndpoint">> => ProxyEndpoint,
        <<"serviceProperties">> => ServiceProperties
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new handle service for given user.
%% Name, ProxyEndpoint and ServiceProperties must be given in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create_handle_service(Auth :: aai:auth(), UserId :: od_user:id(),
    Data :: #{}) -> {ok, od_handle_service:id()} | errors:error().
create_handle_service(Auth, UserId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_handle_service, id = undefined, aspect = instance},
        auth_hint = ?AS_USER(UserId),
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new handle for given user.
%% Allows to specify the HServiceId, ResourceType, ResourceId and Metadata.
%% @end
%%--------------------------------------------------------------------
-spec create_handle(Auth :: aai:auth(), UserId :: od_user:id(),
    HServiceId :: od_handle_service:id(), ResourceType :: od_handle:resource_type(),
    ResourceId :: od_handle:resource_id(), MetadataPrefix :: od_handle:metadata_prefix(),
    Metadata :: od_handle:raw_metadata()) ->
    {ok, od_handle:id()} | errors:error().
create_handle(Auth, UserId, HServiceId, ResourceType, ResourceId, MetadataPrefix, Metadata) ->
    create_handle(Auth, UserId, #{
        <<"handleServiceId">> => HServiceId,
        <<"resourceType">> => ResourceType,
        <<"resourceId">> => ResourceId,
        <<"metadataPrefix">> => MetadataPrefix,
        <<"metadata">> => Metadata
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new handle for given user.
%% HServiceId, ResourceType, ResourceId and Metadata must be given in proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create_handle(Auth :: aai:auth(), UserId :: od_user:id(),
    Data :: #{}) -> {ok, od_handle:id()} | errors:error().
create_handle(Auth, UserId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_handle, id = undefined, aspect = instance},
        auth_hint = ?AS_USER(UserId),
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new harvester for given user. 
%% Harvester name, endpoint plugin and config are given explicitly.
%% @end
%%--------------------------------------------------------------------
-spec create_harvester(Auth :: aai:auth(), UserId :: od_user:id(), Name :: binary(),
    Endpoint :: binary(), Plugin :: binary(), Config :: map()) -> {ok, od_harvester:id()} | errors:error().
create_harvester(Auth, UserId, Name, Endpoint, Plugin, Config) ->
    create_harvester(Auth, UserId, #{
        <<"name">> => Name,
        <<"endpoint">> => Endpoint,
        <<"plugin">> => Plugin,
        <<"guiPluginConfig">> => Config
    }).



%%--------------------------------------------------------------------
%% @doc
%% Creates a new harvester for given user. 
%% Harvester name, endpoint plugin and config are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create_harvester(Auth :: aai:auth(), UserId :: od_user:id(),
    Data :: #{}) -> {ok, od_harvester:id()} | errors:error().
create_harvester(Auth, UserId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = undefined, aspect = instance},
        auth_hint = ?AS_USER(UserId),
        data = Data
    })).


-spec create_atm_inventory(aai:auth(), od_user:id(), binary() | entity_logic:data()) ->
    {ok, od_atm_inventory:id()} | errors:error().
create_atm_inventory(Auth, UserId, Name) when is_binary(Name) ->
    create_atm_inventory(Auth, UserId, #{<<"name">> => Name});
create_atm_inventory(Auth, UserId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = undefined, aspect = instance},
        auth_hint = ?AS_USER(UserId),
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Joins a group on behalf of given user based on group_invite_user token.
%% Has two variants:
%% 1) Token is given explicitly
%% 2) Token is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec join_group(Auth :: aai:auth(), UserId :: od_user:id(),
    TokenOrData :: tokens:serialized() | tokens:token() | map()) ->
    {ok, od_group:id()} | errors:error().
join_group(Auth, UserId, Data) when is_map(Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_group, id = undefined, aspect = join},
        auth_hint = ?AS_USER(UserId),
        data = Data
    }));
join_group(Auth, UserId, Token) ->
    join_group(Auth, UserId, #{<<"token">> => Token}).


%%--------------------------------------------------------------------
%% @doc
%% Joins a space on behalf of given user based on space_invite_user token.
%% Has two variants:
%% 1) Token is given explicitly
%% 2) Token is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec join_space(Auth :: aai:auth(), UserId :: od_user:id(),
    TokenOrData :: tokens:serialized() | tokens:token() | map()) ->
    {ok, od_space:id()} | errors:error().
join_space(Auth, UserId, Data) when is_map(Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_space, id = undefined, aspect = join},
        auth_hint = ?AS_USER(UserId),
        data = Data
    }));
join_space(Auth, UserId, Token) ->
    join_space(Auth, UserId, #{<<"token">> => Token}).


%%--------------------------------------------------------------------
%% @doc
%% Joins a harvester on behalf of given user based on harvester_invite_user token.
%% Has two variants:
%% 1) Token is given explicitly
%% 2) Token is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec join_harvester(Auth :: aai:auth(), UserId :: od_user:id(),
    TokenOrData :: tokens:serialized() | tokens:token() | map()) ->
    {ok, od_harvester:id()} | errors:error().
join_harvester(Auth, UserId, Data) when is_map(Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_harvester, id = undefined, aspect = join},
        auth_hint = ?AS_USER(UserId),
        data = Data
    }));
join_harvester(Auth, UserId, Token) ->
    join_harvester(Auth, UserId, #{<<"token">> => Token}).


%%--------------------------------------------------------------------
%% @doc
%% Joins a cluster on behalf of given user based on cluster_invite_user token.
%% Has two variants:
%% 1) Token is given explicitly
%% 2) Token is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec join_cluster(Auth :: aai:auth(), UserId :: od_user:id(),
    TokenOrData :: tokens:serialized() | tokens:token() | map()) ->
    {ok, od_cluster:id()} | errors:error().
join_cluster(Auth, UserId, Data) when is_map(Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_cluster, id = undefined, aspect = join},
        auth_hint = ?AS_USER(UserId),
        data = Data
    }));
join_cluster(Auth, UserId, Token) ->
    join_cluster(Auth, UserId, #{<<"token">> => Token}).


-spec join_atm_inventory(aai:auth(), od_user:id(), tokens:serialized() | tokens:token() | entity_logic:data()) ->
    {ok, od_atm_inventory:id()} | errors:error().
join_atm_inventory(Auth, UserId, Data) when is_map(Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = undefined, aspect = join},
        auth_hint = ?AS_USER(UserId),
        data = Data
    }));
join_atm_inventory(Auth, UserId, Token) ->
    join_atm_inventory(Auth, UserId, #{<<"token">> => Token}).


-spec get_full_name(aai:auth(), od_user:id()) ->
    {ok, od_user:full_name()} | errors:error().
get_full_name(Auth, UserId) ->
    case get(Auth, UserId) of
        {ok, #od_user{full_name = FullName}} -> {ok, FullName};
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of groups of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_groups(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, [od_group:id()]} | errors:error().
get_groups(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = groups}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective groups of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_groups(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, [od_group:id()]} | errors:error().
get_eff_groups(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = eff_groups}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific group among groups of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_group(Auth :: aai:auth(), UserId :: od_user:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | errors:error().
get_group(Auth, UserId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_USER(UserId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective group among
%% effective groups of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group(Auth :: aai:auth(), UserId :: od_user:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | errors:error().
get_eff_group(Auth, UserId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_group, id = GroupId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_USER(UserId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of spaces of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, [od_space:id()]} | errors:error().
get_spaces(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = spaces}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective spaces of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_spaces(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, [od_space:id()]} | errors:error().
get_eff_spaces(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = eff_spaces}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific space among spaces of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_space(Auth :: aai:auth(), UserId :: od_user:id(),
    SpaceId :: od_space:id()) -> {ok, #{}} | errors:error().
get_space(Auth, UserId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_USER(UserId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective space among
%% effective spaces of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_space(Auth :: aai:auth(), UserId :: od_user:id(),
    SpaceId :: od_space:id()) -> {ok, #{}} | errors:error().
get_eff_space(Auth, UserId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_space, id = SpaceId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_USER(UserId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective providers of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_providers(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, [od_provider:id()]} | errors:error().
get_eff_providers(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = eff_providers}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective provider among
%% effective providers of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_provider(Auth :: aai:auth(), UserId :: od_user:id(),
    ProviderId :: od_provider:id()) -> {ok, #{}} | errors:error().
get_eff_provider(Auth, UserId, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_provider, id = ProviderId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_USER(UserId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of spaces supported by specific effective provider among
%% effective providers of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces_in_eff_provider(Auth :: aai:auth(), UserId :: od_user:id(),
    ProviderId :: od_provider:id()) -> {ok, #{}} | errors:error().
get_spaces_in_eff_provider(Auth, UserId, ProviderId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_provider, id = ProviderId, aspect = {user_spaces, UserId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of handle_services of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_handle_services(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, [od_handle_service:id()]} | errors:error().
get_handle_services(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = handle_services}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective handle_services of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_handle_services(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, [od_handle_service:id()]} | errors:error().
get_eff_handle_services(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = eff_handle_services}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific handle_service among
%% handle_services of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_handle_service(Auth :: aai:auth(), UserId :: od_user:id(),
    HServiceId :: od_handle_service:id()) -> {ok, #{}} | errors:error().
get_handle_service(Auth, UserId, HServiceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle_service, id = HServiceId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_USER(UserId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective handle_service among
%% effective handle_services of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_handle_service(Auth :: aai:auth(), UserId :: od_user:id(),
    HServiceId :: od_handle_service:id()) -> {ok, #{}} | errors:error().
get_eff_handle_service(Auth, UserId, HServiceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle_service, id = HServiceId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_USER(UserId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of handles of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_handles(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, [od_handle:id()]} | errors:error().
get_handles(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = handles}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective handles of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_handles(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, [od_handle:id()]} | errors:error().
get_eff_handles(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = eff_handles}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific handle among
%% handles of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_handle(Auth :: aai:auth(), UserId :: od_user:id(),
    HandleId :: od_handle:id()) -> {ok, #{}} | errors:error().
get_handle(Auth, UserId, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_USER(UserId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective handle among
%% effective handles of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_handle(Auth :: aai:auth(), UserId :: od_user:id(),
    HandleId :: od_handle:id()) -> {ok, #{}} | errors:error().
get_eff_handle(Auth, UserId, HandleId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_handle, id = HandleId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_USER(UserId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of clusters of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_clusters(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, [od_cluster:id()]} | errors:error().
get_clusters(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = clusters}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective clusters of the authenticated user.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_clusters(Auth :: aai:auth()) ->
    {ok, [od_cluster:id()]} | errors:error().
get_eff_clusters(#auth{subject = ?SUB(user, UserId)} = Auth) ->
    get_eff_clusters(Auth, UserId).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective clusters of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_clusters(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, [od_cluster:id()]} | errors:error().
get_eff_clusters(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = eff_clusters}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific cluster among
%% clusters of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_cluster(Auth :: aai:auth(), UserId :: od_user:id(),
    ClusterId :: od_cluster:id()) -> {ok, #{}} | errors:error().
get_cluster(Auth, UserId, ClusterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_USER(UserId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective cluster among
%% effective clusters of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_cluster(Auth :: aai:auth(), UserId :: od_user:id(),
    ClusterId :: od_cluster:id()) -> {ok, #{}} | errors:error().
get_eff_cluster(Auth, UserId, ClusterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_cluster, id = ClusterId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_USER(UserId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of harvesters of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_harvesters(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, [od_harvester:id()]} | errors:error().
get_harvesters(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = harvesters}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective harvesters of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_harvesters(Auth :: aai:auth(), UserId :: od_user:id()) ->
    {ok, [od_harvester:id()]} | errors:error().
get_eff_harvesters(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = eff_harvesters}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific harvester among harvesters of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_harvester(Auth :: aai:auth(), UserId :: od_user:id(),
    HarvesterId :: od_harvester:id()) -> {ok, #{}} | errors:error().
get_harvester(Auth, UserId, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_USER(UserId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective harvester among
%% effective harvesters of given user.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_harvester(Auth :: aai:auth(), UserId :: od_user:id(),
    HarvesterId :: od_harvester:id()) -> {ok, #{}} | errors:error().
get_eff_harvester(Auth, UserId, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_harvester, id = HarvesterId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_USER(UserId)
    }).


-spec get_atm_inventories(aai:auth(), od_user:id()) ->
    {ok, [od_atm_inventory:id()]} | errors:error().
get_atm_inventories(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = atm_inventories}
    }).


-spec get_eff_atm_inventories(aai:auth(), od_user:id()) ->
    {ok, [od_atm_inventory:id()]} | errors:error().
get_eff_atm_inventories(Auth, UserId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = eff_atm_inventories}
    }).


-spec get_atm_inventory(aai:auth(), od_user:id(), od_atm_inventory:id()) ->
    {ok, #{}} | errors:error().
get_atm_inventory(Auth, UserId, AtmInventoryId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_USER(UserId)
    }).


-spec get_eff_atm_inventory(aai:auth(), od_user:id(), od_atm_inventory:id()) ->
    {ok, #{}} | errors:error().
get_eff_atm_inventory(Auth, UserId, AtmInventoryId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_inventory, id = AtmInventoryId, aspect = instance, scope = protected},
        auth_hint = ?THROUGH_USER(UserId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified group on behalf of given user.
%% @end
%%--------------------------------------------------------------------
-spec leave_group(Auth :: aai:auth(), UserId :: od_user:id(),
    GroupId :: od_group:id()) -> ok | errors:error().
leave_group(Auth, UserId, GroupId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = {group, GroupId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified space on behalf of given user.
%% @end
%%--------------------------------------------------------------------
-spec leave_space(Auth :: aai:auth(), UserId :: od_user:id(),
    SpaceId :: od_space:id()) -> ok | errors:error().
leave_space(Auth, UserId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = {space, SpaceId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified od_handle_service on behalf of given user.
%% @end
%%--------------------------------------------------------------------
-spec leave_handle_service(Auth :: aai:auth(), UserId :: od_user:id(),
    HServiceId :: od_handle_service:id()) -> ok | errors:error().
leave_handle_service(Auth, UserId, HServiceId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = {handle_service, HServiceId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified handle on behalf of given user.
%% @end
%%--------------------------------------------------------------------
-spec leave_handle(Auth :: aai:auth(), UserId :: od_user:id(),
    HandleId :: od_handle:id()) -> ok | errors:error().
leave_handle(Auth, UserId, HandleId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = {handle, HandleId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified harvester on behalf of given user.
%% @end
%%--------------------------------------------------------------------
-spec leave_harvester(Auth :: aai:auth(), UserId :: od_user:id(),
    HarvesterId :: od_harvester:id()) -> ok | errors:error().
leave_harvester(Auth, UserId, HarvesterId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = {harvester, HarvesterId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Leaves specified cluster on behalf of given user.
%% @end
%%--------------------------------------------------------------------
-spec leave_cluster(Auth :: aai:auth(), UserId :: od_user:id(),
    ClusterId :: od_cluster:id()) -> ok | errors:error().
leave_cluster(Auth, UserId, ClusterId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = {cluster, ClusterId}}
    }).


-spec leave_atm_inventory(aai:auth(), od_user:id(), od_atm_inventory:id()) ->
    ok | errors:error().
leave_atm_inventory(Auth, UserId, AtmInventoryId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_user, id = UserId, aspect = {atm_inventory, AtmInventoryId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether a user exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(UserId :: od_user:id()) -> boolean().
exists(UserId) ->
    {ok, Exists} = od_user:exists(UserId),
    Exists.


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether given user has specified effective oz privilege.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_oz_privilege(UserIdOrUser :: od_user:id() | #od_user{},
    Privilege :: privileges:oz_privilege()) -> boolean().
has_eff_oz_privilege(UserId, Privilege) when is_binary(UserId) ->
    entity_graph:has_oz_privilege(effective, Privilege, od_user, UserId);
has_eff_oz_privilege(User, Privilege) ->
    entity_graph:has_oz_privilege(effective, Privilege, User).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether given user belongs to specified effective group.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_group(UserIdOrUser :: od_user:id() | #od_user{},
    GroupId :: od_group:id()) -> boolean().
has_eff_group(UserId, GroupId) when is_binary(UserId) ->
    entity_graph:has_relation(effective, top_down, od_group, GroupId, od_user, UserId);
has_eff_group(User, GroupId) ->
    entity_graph:has_relation(effective, top_down, od_group, GroupId, User).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether given user belongs to specified effective space.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_space(UserIdOrUser :: od_user:id() | #od_user{},
    SpaceId :: od_space:id()) -> boolean().
has_eff_space(UserId, SpaceId) when is_binary(UserId) ->
    entity_graph:has_relation(effective, top_down, od_space, SpaceId, od_user, UserId);
has_eff_space(User, SpaceId) ->
    entity_graph:has_relation(effective, top_down, od_space, SpaceId, User).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether given user belongs to specified effective provider.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_provider(UserIdOrUser :: od_user:id() | #od_user{},
    ProviderId :: od_provider:id()) -> boolean().
has_eff_provider(UserId, ProviderId) when is_binary(UserId) ->
    entity_graph:has_relation(effective, top_down, od_provider, ProviderId, od_user, UserId);
has_eff_provider(User, ProviderId) ->
    entity_graph:has_relation(effective, top_down, od_provider, ProviderId, User).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether given user belongs to specified effective handle_service.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_handle_service(UserIdOrUser :: od_user:id() | #od_user{},
    HServiceId :: od_handle_service:id()) -> boolean().
has_eff_handle_service(UserId, HServiceId) when is_binary(UserId) ->
    entity_graph:has_relation(effective, top_down, od_handle_service, HServiceId, od_user, UserId);
has_eff_handle_service(User, HServiceId) ->
    entity_graph:has_relation(effective, top_down, od_handle_service, HServiceId, User).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether given user belongs to specified effective handle_service.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_handle(UserIdOrUser :: od_user:id() | #od_user{},
    HandleId :: od_handle:id()) -> boolean().
has_eff_handle(UserId, HandleId) when is_binary(UserId) ->
    entity_graph:has_relation(effective, top_down, od_handle, HandleId, od_user, UserId);
has_eff_handle(User, HandleId) ->
    entity_graph:has_relation(effective, top_down, od_handle, HandleId, User).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether given user belongs to specified effective harvester_service.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_harvester(UserIdOrUser :: od_user:id() | #od_user{},
    HarvesterId :: od_harvester:id()) -> boolean().
has_eff_harvester(UserId, HarvesterId) when is_binary(UserId) ->
    entity_graph:has_relation(effective, top_down, od_harvester, HarvesterId, od_user, UserId);
has_eff_harvester(User, HarvesterId) ->
    entity_graph:has_relation(effective, top_down, od_harvester, HarvesterId, User).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether given user belongs to specified effective cluster.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_cluster(UserIdOrUser :: od_user:id() | #od_user{},
    ClusterId :: od_cluster:id()) -> boolean().
has_eff_cluster(UserId, ClusterId) when is_binary(UserId) ->
    entity_graph:has_relation(effective, top_down, od_cluster, ClusterId, od_user, UserId);
has_eff_cluster(User, ClusterId) ->
    entity_graph:has_relation(effective, top_down, od_cluster, ClusterId, User).


-spec has_eff_atm_inventory(od_user:id() | od_user:record(), od_atm_inventory:id()) -> boolean().
has_eff_atm_inventory(UserId, AtmInventoryId) when is_binary(UserId) ->
    entity_graph:has_relation(effective, top_down, od_atm_inventory, AtmInventoryId, od_user, UserId);
has_eff_atm_inventory(User, AtmInventoryId) ->
    entity_graph:has_relation(effective, top_down, od_atm_inventory, AtmInventoryId, User).


%%--------------------------------------------------------------------
%% @doc
%% Validates user full_name against allowed format.
%% @end
%%--------------------------------------------------------------------
-spec validate_full_name(binary()) -> boolean().
validate_full_name(FullName) ->
    str_utils:validate_name(
        FullName, ?FULL_NAME_FIRST_CHARS_ALLOWED, ?FULL_NAME_MIDDLE_CHARS_ALLOWED,
        ?FULL_NAME_LAST_CHARS_ALLOWED, ?FULL_NAME_MAXIMUM_LENGTH
    ).


%%--------------------------------------------------------------------
%% @doc
%% @see entity_logic_sanitizer:normalize_name/9.
%% Normalizes user full_name to fit the allowed format.
%% @end
%%--------------------------------------------------------------------
-spec normalize_full_name(undefined | od_user:full_name()) -> od_user:full_name().
normalize_full_name(undefined) ->
    ?DEFAULT_FULL_NAME;
normalize_full_name(FullName) ->
    entity_logic_sanitizer:normalize_name(FullName,
        ?FULL_NAME_FIRST_CHARS_ALLOWED, <<"">>,
        ?FULL_NAME_MIDDLE_CHARS_ALLOWED, <<"-">>,
        ?FULL_NAME_LAST_CHARS_ALLOWED, <<"">>,
        ?FULL_NAME_MAXIMUM_LENGTH, ?DEFAULT_FULL_NAME
    ).


%%--------------------------------------------------------------------
%% @doc
%% Validates username against allowed format.
%% @end
%%--------------------------------------------------------------------
-spec validate_username(od_user:username()) -> boolean().
validate_username(Username) ->
    str_utils:validate_name(
        Username, ?USERNAME_FIRST_CHARS_ALLOWED, ?USERNAME_MIDDLE_CHARS_ALLOWED,
        ?USERNAME_LAST_CHARS_ALLOWED, ?USERNAME_MAXIMUM_LENGTH
    ).


%%--------------------------------------------------------------------
%% @doc
%% @see entity_logic_sanitizer:normalize_name/9.
%% Normalizes username to fit the allowed format.
%% @end
%%--------------------------------------------------------------------
-spec normalize_username(od_user:username()) -> od_user:username().
normalize_username(undefined) ->
    undefined;
normalize_username(Username) ->
    entity_logic_sanitizer:normalize_name(Username,
        ?USERNAME_FIRST_CHARS_ALLOWED, <<"">>,
        ?USERNAME_MIDDLE_CHARS_ALLOWED, <<"-">>,
        ?USERNAME_LAST_CHARS_ALLOWED, <<"">>,
        ?USERNAME_MAXIMUM_LENGTH, undefined
    ).


%%--------------------------------------------------------------------
%% @doc
%% Clears the list of entitlements of given user, which will cause full
%% refresh of all entitlements from all IdPs upon the next login.
%% Useful when the user was assigned new roles in the groups or the structure
%% changed in a way that cannot be corrected automatically.
%% @end
%%--------------------------------------------------------------------
-spec reset_entitlements(od_user:id()) -> ok | errors:error().
reset_entitlements(UserId) ->
    ResetEntitlements = fun(User) ->
        {ok, User#od_user{entitlements = []}}
    end,
    case od_user:update(UserId, ResetEntitlements) of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.
