%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour implements gs_translator_behaviour and is used to translate
%%% Graph Sync request results into format understood by Oneprovider.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_gs_translator).
-author("Lukasz Opiola").

-behaviour(gs_translator_behaviour).

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("cluster_worker/include/graph_sync/graph_sync.hrl").

%% API
-export([handshake_attributes/1, translate_create/3, translate_get/3]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns handshake response attributes for given client that has been
%% authorized.
%% @end
%%--------------------------------------------------------------------
-spec handshake_attributes(gs_protocol:client()) ->
    gs_protocol:handshake_attributes().
handshake_attributes(_) ->
    undefined.

%%--------------------------------------------------------------------
%% @doc
%% Translates CREATE result to the format understood by client. Will be called
%% only for requests that return {ok, {data, Data}}.
%% For other results, translate_get is called.
%% @end
%%--------------------------------------------------------------------
-spec translate_create(gs_protocol:protocol_version(), gs_protocol:gri(),
    Data :: term()) -> gs_protocol:data() | gs_protocol:error().
translate_create(1, #gri{aspect = invite_group_token}, Macaroon) ->
    translate_create(1, #gri{aspect = invite_user_token}, Macaroon);
translate_create(1, #gri{aspect = invite_provider_token}, Macaroon) ->
    translate_create(1, #gri{aspect = invite_user_token}, Macaroon);
translate_create(1, #gri{aspect = provider_registration_token}, Macaroon) ->
    translate_create(1, #gri{aspect = invite_user_token}, Macaroon);
translate_create(1, #gri{aspect = invite_user_token}, Macaroon) ->
    {ok, Token} = onedata_macaroons:serialize(Macaroon),
    Token;
translate_create(1, #gri{type = od_provider, aspect = map_idp_group}, Id) ->
    Id;

translate_create(ProtocolVersion, GRI, Data) ->
    ?error("Cannot translate graph sync create result for:~n
    ProtocolVersion: ~p~n
    GRI: ~p~n
    Data: ~p~n", [
        ProtocolVersion, GRI, Data
    ]),
    throw(?ERROR_INTERNAL_SERVER_ERROR).


%%--------------------------------------------------------------------
%% @doc
%% Translates GET result to the format understood by client. Should not include
%% "gri" in the resulting json map, as it is included automatically.
%% @end
%%--------------------------------------------------------------------
-spec translate_get(gs_protocol:protocol_version(), gs_protocol:gri(),
    Data :: term()) ->
    gs_protocol:data() | {gs_protocol:gri(), gs_protocol:data()} |
    gs_protocol:error().
translate_get(1, #gri{type = od_provider, aspect = current_time}, TimeMillis) ->
    #{<<"timeMillis">> => TimeMillis};

translate_get(1, #gri{type = od_user, aspect = instance, scope = private}, User) ->
    #od_user{
        name = Name,
        alias = Alias,
        email_list = EmailList,
        linked_accounts = LinkedAccounts,
        default_space = DefaultSpace,
        space_aliases = SpaceAliases,

        eff_groups = EffGroups,
        eff_spaces = EffSpaces,
        eff_handles = EffHandles,
        eff_handle_services = EffHandleServices
    } = User,
    #{
        <<"name">> => Name,
        <<"alias">> => gs_protocol:undefined_to_null(Alias),
        % TODO VFS-4506 deprecated, included for backward compatibility
        <<"login">> => gs_protocol:undefined_to_null(Alias),
        <<"emailList">> => EmailList,
        <<"linkedAccounts">> => user_logic:linked_accounts_to_maps(LinkedAccounts),
        <<"defaultSpaceId">> => gs_protocol:undefined_to_null(DefaultSpace),
        <<"spaceAliases">> => SpaceAliases,

        <<"effectiveGroups">> => maps:keys(EffGroups),
        <<"effectiveSpaces">> => maps:keys(EffSpaces),
        <<"effectiveHandles">> => maps:keys(EffHandles),
        <<"effectiveHandleServices">> => maps:keys(EffHandleServices)
    };

translate_get(1, #gri{type = od_user, aspect = instance, scope = protected}, User) ->
    #{
        <<"name">> := Name,
        <<"alias">> := Alias,
        <<"emailList">> := EmailList,
        <<"linkedAccounts">> := LinkedAccountMaps
    } = User,
    #{
        <<"name">> => Name,
        <<"alias">> => gs_protocol:undefined_to_null(Alias),
        % TODO VFS-4506 deprecated, included for backward compatibility
        <<"login">> => gs_protocol:undefined_to_null(Alias),
        <<"emailList">> => EmailList,
        <<"linkedAccounts">> => LinkedAccountMaps
    };

translate_get(1, #gri{type = od_user, aspect = instance, scope = shared}, User) ->
    #{
        <<"name">> := Name,
        <<"alias">> := Alias
    } = User,
    #{
        <<"name">> => Name,
        <<"alias">> => gs_protocol:undefined_to_null(Alias),
        % TODO VFS-4506 deprecated, included for backward compatibility
        <<"login">> => gs_protocol:undefined_to_null(Alias)
    };

translate_get(1, #gri{type = od_group, aspect = instance, scope = private}, Group) ->
    #od_group{
        name = Name,
        type = Type,

        children = Children,
        eff_children = EffChildren,
        parents = Parents,

        users = Users,
        eff_users = EffUsers,

        eff_spaces = EffSpaces
    } = Group,
    #{
        <<"name">> => Name,
        <<"type">> => Type,

        <<"children">> => Children,
        <<"effectiveChildren">> => strip_eff_relation_data(EffChildren),
        <<"parents">> => Parents,

        <<"users">> => Users,
        <<"effectiveUsers">> => strip_eff_relation_data(EffUsers),

        <<"spaces">> => maps:keys(EffSpaces)
    };

% shared and protected scopes carry the same data
translate_get(1, GRI = #gri{type = od_group, aspect = instance, scope = shared}, Group) ->
    translate_get(1, GRI#gri{scope = protected}, Group);
translate_get(1, #gri{type = od_group, aspect = instance, scope = protected}, GroupData) ->
    GroupData;

translate_get(1, #gri{type = od_space, aspect = instance, scope = private}, Space) ->
    #od_space{
        name = Name,

        users = Users,
        eff_users = EffUsers,

        groups = Groups,
        eff_groups = EffGroups,

        providers = Providers,
        shares = Shares
    } = Space,
    #{
        <<"name">> => Name,

        <<"users">> => Users,
        <<"effectiveUsers">> => strip_eff_relation_data(EffUsers),

        <<"groups">> => Groups,
        <<"effectiveGroups">> => strip_eff_relation_data(EffGroups),

        <<"providers">> => Providers,
        <<"shares">> => Shares
    };
translate_get(1, #gri{type = od_space, aspect = instance, scope = protected}, SpaceData) ->
    SpaceData;

translate_get(1, #gri{type = od_share, aspect = instance, scope = private}, Share) ->
    #od_share{
        name = Name,
        public_url = PublicUrl,
        space = SpaceId,
        handle = HandleId,
        root_file = RootFileId
    } = Share,
    #{
        <<"name">> => Name,
        <<"publicUrl">> => PublicUrl,
        <<"spaceId">> => SpaceId,
        <<"handleId">> => gs_protocol:undefined_to_null(HandleId),
        <<"rootFileId">> => RootFileId
    };

translate_get(1, #gri{type = od_share, aspect = instance, scope = protected}, ShareData) ->
    ShareData;

translate_get(1, #gri{type = od_share, aspect = instance, scope = public}, ShareData) ->
    ShareData;

translate_get(1, #gri{type = od_provider, id = Id, aspect = instance, scope = private}, Provider) ->
    #od_provider{
        name = Name,
        subdomain_delegation = SubdomainDelegation,
        domain = Domain,
        subdomain = Subdomain,
        admin_email = AdminEmail,
        latitude = Latitude,
        longitude = Longitude,

        spaces = Spaces,
        eff_users = EffUsers,
        eff_groups = EffGroups
    } = Provider,
    #{
        <<"name">> => Name,
        <<"subdomainDelegation">> => SubdomainDelegation,
        <<"domain">> => Domain,
        <<"subdomain">> => Subdomain,

        <<"adminEmail">> => AdminEmail,

        <<"latitude">> => Latitude,
        <<"longitude">> => Longitude,

        <<"online">> => provider_connection:is_online(Id),

        <<"spaces">> => Spaces,
        <<"effectiveUsers">> => maps:keys(EffUsers),
        <<"effectiveGroups">> => maps:keys(EffGroups)

    };


translate_get(1, #gri{type = od_provider, aspect = instance, scope = protected}, ProviderData) ->
    ProviderData;

translate_get(1, #gri{type = od_provider, aspect = domain_config}, Data) ->
    case maps:find(<<"ipList">>, Data) of
        {ok, IPList} ->
            IPBinaries = [list_to_binary(inet:ntoa(IP)) || IP <- IPList],
            Data#{<<"ipList">> := IPBinaries};
        error ->
            Data
    end;

translate_get(1, #gri{type = od_handle_service, aspect = instance, scope = private}, HService) ->
    #od_handle_service{
        name = Name,

        eff_groups = EffGroups,
        eff_users = EffUsers
    } = HService,
    #{
        <<"name">> => Name,

        <<"effectiveUsers">> => strip_eff_relation_data(EffUsers),
        <<"effectiveGroups">> => strip_eff_relation_data(EffGroups)
    };

translate_get(1, #gri{type = od_handle, aspect = instance, scope = private}, Handle) ->
    #od_handle{
        public_handle = PublicHandle,
        resource_type = ResourceType,
        resource_id = ResourceId,
        metadata = Metadata,
        timestamp = Timestamp,
        handle_service = HandleServiceId,

        eff_groups = EffGroups,
        eff_users = EffUsers
    } = Handle,
    #{
        <<"publicHandle">> => PublicHandle,
        <<"resourceType">> => ResourceType,
        <<"resourceId">> => ResourceId,
        <<"metadata">> => Metadata,
        <<"timestamp">> => time_utils:datetime_to_datestamp(Timestamp),
        <<"handleServiceId">> => HandleServiceId,

        <<"effectiveUsers">> => strip_eff_relation_data(EffUsers),
        <<"effectiveGroups">> => strip_eff_relation_data(EffGroups)
    };

translate_get(1, #gri{type = od_handle, aspect = instance, scope = public}, HandleData) ->
    HandleData;

translate_get(ProtocolVersion, GRI, Data) ->
    ?error("Cannot translate graph sync get result for:~n
    ProtocolVersion: ~p~n
    GRI: ~p~n
    Data: ~p~n", [
        ProtocolVersion, GRI, Data
    ]),
    throw(?ERROR_INTERNAL_SERVER_ERROR).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec strip_eff_relation_data(maps:map()) -> maps:map().
strip_eff_relation_data(Map) when is_map(Map) ->
    maps:map(
        fun(_, {Attributes, _EffRelationData}) ->
            Attributes
        end, Map).
