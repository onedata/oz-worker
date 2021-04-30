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
-include_lib("ctool/include/errors.hrl").
-include_lib("cluster_worker/include/graph_sync/graph_sync.hrl").

%% API
-export([handshake_attributes/1, translate_value/3, translate_resource/3]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback handshake_attributes/1.
%% @end
%%--------------------------------------------------------------------
-spec handshake_attributes(aai:auth()) ->
    gs_protocol:handshake_attributes().
handshake_attributes(_) ->
    undefined.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback translate_value/3.
%% @end
%%--------------------------------------------------------------------
-spec translate_value(gs_protocol:protocol_version(), gri:gri(),
    Value :: term()) -> Result | fun((aai:auth()) -> Result) when
    Result :: gs_protocol:data() | errors:error().
translate_value(_, #gri{type = od_provider, aspect = map_idp_group}, Id) ->
    Id;
translate_value(_, #gri{type = od_user, aspect = {idp_access_token, _}}, {AccessToken, Expires}) ->
    #{
        <<"token">> => AccessToken,
        <<"ttl">> => Expires
    };
translate_value(_, #gri{type = od_space, aspect = harvest_metadata}, Result) ->
    case Result of
        {error, _} = Error ->
            Error;
        _ ->
            maps:fold(fun
                (HarvesterId, #{<<"error">> := Error}, Acc) ->
                    Acc#{HarvesterId => #{<<"error">> => errors:to_json(Error)}};
                (HarvesterId, Indices, Acc) ->
                    Acc#{HarvesterId => Indices}
            end, #{}, Result)
    end;

translate_value(_, #gri{type = od_harvester, aspect = {submit_entry, _}}, FailedIndices) ->
    FailedIndices;
translate_value(_, #gri{type = od_harvester, aspect = {delete_entry, _}}, FailedIndices) ->
    FailedIndices;

translate_value(_, #gri{type = od_token, aspect = verify_access_token}, #{<<"subject">> := Sub, <<"ttl">> := TTL}) ->
    #{
        <<"subject">> => aai:serialize_subject(Sub),
        <<"ttl">> => utils:undefined_to_null(TTL)
    };
translate_value(_, #gri{type = od_token, aspect = verify_identity_token}, #{<<"subject">> := Sub, <<"ttl">> := TTL}) ->
    #{
        <<"subject">> => aai:serialize_subject(Sub),
        <<"ttl">> => utils:undefined_to_null(TTL)
    };
translate_value(_, #gri{type = od_token, id = undefined, aspect = {provider_temporary_token, _}}, Token) ->
    {ok, Serialized} = tokens:serialize(Token),
    Serialized;
translate_value(_, #gri{type = od_token, id = undefined, aspect = {offline_user_access_token, _}}, Token) ->
    {ok, Serialized} = tokens:serialize(Token),
    Serialized;

translate_value(ProtocolVersion, GRI, Data) ->
    ?error("Cannot translate graph sync create result for:~n
    ProtocolVersion: ~p~n
    GRI: ~p~n
    Data: ~p~n", [
        ProtocolVersion, GRI, Data
    ]),
    throw(?ERROR_INTERNAL_SERVER_ERROR).


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback translate_resource/3.
%% @end
%%--------------------------------------------------------------------
-spec translate_resource(gs_protocol:protocol_version(), gri:gri(),
    ResourceData :: term()) -> Result | fun((aai:auth()) -> Result) when
    Result :: gs_protocol:data() | errors:error().
translate_resource(_, #gri{type = od_provider, aspect = current_time}, TimeMillis) ->
    #{<<"timeMillis">> => TimeMillis};

translate_resource(_, #gri{type = od_user, aspect = instance, scope = private}, User) ->
    #od_user{
        full_name = FullName,
        username = Username,
        emails = Emails,
        linked_accounts = LinkedAccounts,
        space_aliases = SpaceAliases,
        blocked = Blocked
    } = User,
    #{
        <<"fullName">> => FullName,
        <<"username">> => utils:undefined_to_null(Username),
        <<"emails">> => Emails,
        <<"linkedAccounts">> => linked_accounts:to_maps(LinkedAccounts, luma_payload),

        <<"blocked">> => Blocked,
        <<"spaceAliases">> => SpaceAliases,

        <<"effectiveGroups">> => entity_graph:get_relations(effective, top_down, od_group, User),
        <<"effectiveSpaces">> => entity_graph:get_relations(effective, top_down, od_space, User),
        <<"effectiveHandles">> => entity_graph:get_relations(effective, top_down, od_handle, User),
        <<"effectiveHandleServices">> => entity_graph:get_relations(effective, top_down, od_handle_service, User),

        % TODO VFS-4506 deprecated fields, included for backward compatibility
        <<"defaultSpaceId">> => null,
        <<"name">> => FullName,
        <<"login">> => utils:undefined_to_null(Username),
        <<"alias">> => utils:undefined_to_null(Username),
        <<"emailList">> => Emails
    };

translate_resource(_, #gri{type = od_user, aspect = instance, scope = protected}, User) ->
    #{
        <<"fullName">> := FullName,
        <<"username">> := Username,
        <<"emails">> := Emails,
        <<"linkedAccounts">> := LinkedAccountMaps,
        <<"blocked">> := Blocked
    } = User,
    #{
        <<"fullName">> => FullName,
        <<"username">> => utils:undefined_to_null(Username),
        <<"emails">> => Emails,
        <<"linkedAccounts">> => LinkedAccountMaps,

        <<"blocked">> => Blocked,

        %% @TODO VFS-4506 deprecated fields, included for backward compatibility
        <<"name">> => FullName,
        <<"login">> => utils:undefined_to_null(Username),
        <<"alias">> => utils:undefined_to_null(Username),
        <<"emailList">> => Emails
    };

translate_resource(_, #gri{type = od_user, aspect = instance, scope = shared}, User) ->
    #{
        <<"fullName">> := FullName,
        <<"username">> := Username
    } = User,
    #{
        <<"fullName">> => FullName,
        <<"username">> => utils:undefined_to_null(Username),

        %% @TODO VFS-4506 deprecated field, included for backward compatibility
        <<"name">> => FullName,
        <<"login">> => utils:undefined_to_null(Username),
        <<"alias">> => utils:undefined_to_null(Username)
    };

translate_resource(_, #gri{type = od_group, aspect = instance, scope = private}, Group) ->
    #od_group{
        name = Name,
        type = Type,

        children = Children,
        parents = Parents,

        users = Users,

        eff_spaces = EffSpaces
    } = Group,
    #{
        <<"name">> => Name,
        <<"type">> => Type,

        <<"children">> => Children,
        <<"effectiveChildren">> => entity_graph:get_relations_with_attrs(effective, bottom_up, od_group, Group),
        <<"parents">> => Parents,

        <<"users">> => Users,
        <<"effectiveUsers">> => entity_graph:get_relations_with_attrs(effective, bottom_up, od_user, Group),

        <<"spaces">> => maps:keys(EffSpaces)
    };

% shared and protected scopes carry the same data
translate_resource(ProtoVersion, GRI = #gri{type = od_group, aspect = instance, scope = shared}, Group) ->
    translate_resource(ProtoVersion, GRI#gri{scope = protected}, Group);
translate_resource(_, #gri{type = od_group, aspect = instance, scope = protected}, GroupData) ->
    #{<<"name">> := Name, <<"type">> := Type} = GroupData,
    #{<<"name">> => Name, <<"type">> => Type};

translate_resource(_, #gri{type = od_space, aspect = instance, scope = private}, Space) ->
    #od_space{
        name = Name,

        owners = Owners,

        users = Users,

        groups = Groups,

        storages = Storages,
        shares = Shares,
        harvesters = Harvesters
    } = Space,
    #{
        <<"name">> => Name,

        <<"owners">> => Owners,

        <<"users">> => Users,
        <<"effectiveUsers">> => entity_graph:get_relations_with_attrs(effective, bottom_up, od_user, Space),

        <<"groups">> => Groups,
        <<"effectiveGroups">> => entity_graph:get_relations_with_attrs(effective, bottom_up, od_group, Space),

        <<"providers">> => entity_graph:get_relations_with_attrs(effective, top_down, od_provider, Space),
        <<"storages">> => Storages,
        <<"shares">> => Shares,
        <<"harvesters">> => Harvesters
    };
translate_resource(_, #gri{type = od_space, aspect = instance, scope = protected}, SpaceData) ->
    #{
        <<"name">> := Name,
        <<"providers">> := Providers
    } = SpaceData,
    #{
        <<"name">> => Name,
        <<"providers">> => Providers
    };

translate_resource(_, #gri{type = od_share, id = ShareId, aspect = instance, scope = private}, Share) ->
    #od_share{
        space = SpaceId,
        name = Name,
        description = Description,
        handle = HandleId,
        root_file = RootFileId,
        file_type = FileType
    } = Share,
    #{
        <<"spaceId">> => SpaceId,
        <<"name">> => Name,
        <<"description">> => Description,
        <<"publicUrl">> => share_logic:build_public_url(ShareId),
        <<"publicRestUrl">> => share_logic:build_public_rest_url(ShareId),
        <<"rootFileId">> => RootFileId,
        <<"fileType">> => FileType,
        <<"handleId">> => utils:undefined_to_null(HandleId)
    };

translate_resource(_, #gri{type = od_share, id = ShareId, aspect = instance, scope = public}, ShareData) ->
    #{
        <<"spaceId">> := SpaceId,
        <<"name">> := Name,
        <<"description">> := Description,
        <<"rootFileId">> := RootFileId,
        <<"fileType">> := FileType,
        <<"handleId">> := HandleId
    } = ShareData,
    #{
        <<"spaceId">> => SpaceId,
        <<"name">> => Name,
        <<"description">> => Description,
        <<"publicUrl">> => share_logic:build_public_url(ShareId),
        <<"publicRestUrl">> => share_logic:build_public_rest_url(ShareId),
        <<"rootFileId">> => RootFileId,
        <<"fileType">> => FileType,
        <<"handleId">> => utils:undefined_to_null(HandleId)
    };


translate_resource(_, #gri{type = od_provider, aspect = instance, scope = private}, {_Provider, RootToken}) ->
    % This covers provider creation via Graph Sync, in contrast to the get
    % request that does not return the root token
    {ok, Serialized} = tokens:serialize(RootToken),
    #{
        <<"providerRootToken">> => Serialized
    };
translate_resource(_, #gri{type = od_provider, id = Id, aspect = instance, scope = private}, Provider) ->
    #od_provider{
        name = Name,
        subdomain_delegation = SubdomainDelegation,
        domain = Domain,
        subdomain = Subdomain,
        admin_email = AdminEmail,
        latitude = Latitude,
        longitude = Longitude
    } = Provider,
    #{
        <<"name">> => Name,
        <<"subdomainDelegation">> => SubdomainDelegation,
        <<"domain">> => Domain,
        <<"subdomain">> => Subdomain,

        <<"adminEmail">> => AdminEmail,

        <<"latitude">> => Latitude,
        <<"longitude">> => Longitude,

        <<"online">> => provider_connections:is_online(Id),

        <<"storages">> => entity_graph:get_relations(direct, bottom_up, od_storage, Provider),
        %% @TODO VFS-5554 Deprecated, included for backward compatibility
        <<"spaces">> => entity_graph:get_relations_with_attrs(effective, bottom_up, od_space, Provider),
        <<"effectiveSpaces">> => entity_graph:get_relations_with_attrs(effective, bottom_up, od_space, Provider),
        <<"effectiveUsers">> => entity_graph:get_relations(effective, bottom_up, od_user, Provider),
        <<"effectiveGroups">> => entity_graph:get_relations(effective, bottom_up, od_group, Provider)
    };

translate_resource(_, #gri{type = od_provider, aspect = instance, scope = protected}, ProviderData) ->
    #{
        <<"name">> := Name, <<"domain">> := Domain,
        <<"latitude">> := Latitude, <<"longitude">> := Longitude,
        <<"online">> := Online
    } = ProviderData,
    #{
        <<"name">> => Name, <<"domain">> => Domain,
        <<"latitude">> => Latitude, <<"longitude">> => Longitude,
        <<"online">> => Online
    };

translate_resource(_, #gri{type = od_provider, aspect = domain_config}, Data) ->
    case maps:find(<<"ipList">>, Data) of
        {ok, IPList} ->
            IPBinaries = [list_to_binary(inet:ntoa(IP)) || IP <- IPList],
            Data#{<<"ipList">> := IPBinaries};
        error ->
            Data
    end;

translate_resource(_, #gri{type = od_handle_service, aspect = instance, scope = private}, HService) ->
    #{
        <<"name">> => HService#od_handle_service.name,

        <<"effectiveUsers">> => entity_graph:get_relations_with_attrs(effective, bottom_up, od_user, HService),
        <<"effectiveGroups">> => entity_graph:get_relations_with_attrs(effective, bottom_up, od_group, HService)
    };

translate_resource(_, #gri{type = od_handle, aspect = instance, scope = private}, Handle) ->
    #od_handle{
        public_handle = PublicHandle,
        resource_type = ResourceType,
        resource_id = ResourceId,
        metadata = Metadata,
        timestamp = Timestamp,
        handle_service = HandleServiceId
    } = Handle,
    #{
        <<"publicHandle">> => PublicHandle,
        <<"resourceType">> => ResourceType,
        <<"resourceId">> => ResourceId,
        <<"metadata">> => Metadata,
        <<"timestamp">> => time:seconds_to_iso8601(Timestamp),  % @TODO VFS-6309 to be removed in 21.02
        <<"handleServiceId">> => HandleServiceId,

        <<"effectiveUsers">> => entity_graph:get_relations_with_attrs(effective, bottom_up, od_user, Handle),
        <<"effectiveGroups">> => entity_graph:get_relations_with_attrs(effective, bottom_up, od_group, Handle)
    };

translate_resource(_, #gri{type = od_handle, aspect = instance, scope = public}, HandleData) ->
    #{
        <<"publicHandle">> := PublicHandle,
        <<"metadata">> := Metadata,
        <<"timestamp">> := Timestamp
    } = HandleData,
    #{
        <<"publicHandle">> => PublicHandle,
        <<"metadata">> => Metadata,
        <<"timestamp">> => time:seconds_to_iso8601(Timestamp)  % @TODO VFS-6309 to be removed in 21.02
    };

translate_resource(_, #gri{type = od_harvester, aspect = instance, scope = private}, Harvester) ->
    #od_harvester{indices = Indices} = Harvester,
    #{
        <<"indices">> => maps:keys(Indices),
        <<"spaces">> => entity_graph:get_relations(direct, top_down, od_space, Harvester)
    };

translate_resource(_, #gri{type = od_storage, aspect = instance, scope = private}, Storage) ->
    #od_storage{
        name = Name,
        provider = Provider,
        spaces = Spaces,
        qos_parameters = QosParams,
        imported = ImportedStorage,
        readonly = Readonly
    } = Storage,
    #{
        <<"name">> => Name,
        <<"provider">> => Provider,
        <<"spaces">> => maps:keys(Spaces),
        <<"qosParameters">> => QosParams,
        %% @TODO VFS-5856 deprecated, included for compatibility with 20.02.0-beta3
        <<"qos_parameters">> => QosParams,
        <<"imported">> => ImportedStorage,
        <<"readonly">> => Readonly
    };

translate_resource(_, #gri{type = od_storage, aspect = instance, scope = shared}, StorageDetails) ->
    #{
        <<"provider">> := Provider,
        <<"name">> := Name,
        <<"qosParameters">> := QosParams,
        <<"readonly">> := Readonly
    } = StorageDetails,
    #{
        <<"provider">> => Provider,
        <<"name">> => Name,
        <<"qosParameters">> => QosParams,
        <<"readonly">> => Readonly,
        %% @TODO VFS-5856 deprecated, included for compatibility with 20.02.0-beta3
        <<"qos_parameters">> => QosParams
    };

translate_resource(_, #gri{type = od_token, aspect = instance, scope = shared}, #{<<"revoked">> := Revoked}) ->
    #{<<"revoked">> => Revoked};

translate_resource(_, #gri{type = temporary_token_secret, scope = shared}, Generation) ->
    #{<<"generation">> => Generation};

translate_resource(_, #gri{type = od_atm_inventory, aspect = instance, scope = private}, AtmInventory) ->
    #{
        <<"name">> => AtmInventory#od_atm_inventory.name
    };

translate_resource(_, #gri{type = od_atm_lambda, aspect = instance, scope = private}, AtmLambda) ->
    #od_atm_lambda{
        name = Name,
        summary = Summary,
        description = Description,

        engine = Engine,
        operation_ref = OperationRef,

        execution_options = ExecutionOptions,
        argument_specs = ArgumentSpecs,
        result_specs = ResultSpecs
    } = AtmLambda,
    #{
        <<"name">> => Name,
        <<"summary">> => Summary,
        <<"description">> => Description,

        <<"engine">> => automation:lambda_engine_to_json(Engine),
        <<"operationRef">> => OperationRef,

        <<"executionOptions">> => atm_lambda_execution_options:to_json(ExecutionOptions),
        <<"argumentSpecs">> => [atm_lambda_argument_spec:to_json(S) || S <- ArgumentSpecs],
        <<"resultSpecs">> => [atm_lambda_result_spec:to_json(S) || S <- ResultSpecs]
    };

translate_resource(ProtocolVersion, GRI, Data) ->
    ?error("Cannot translate graph sync get result for:~n
    ProtocolVersion: ~p~n
    GRI: ~p~n
    Data: ~p~n", [
        ProtocolVersion, GRI, Data
    ]),
    throw(?ERROR_INTERNAL_SERVER_ERROR).
