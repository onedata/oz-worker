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
translate_value(ProtocolVersion, #gri{type = od_user, aspect = preauthorize}, {Subject, Caveats}) ->
    #{
        <<"subject">> => case ProtocolVersion >= 5 of
            true -> aai:serialize_subject(Subject);
            false -> deprecated_subject_to_json(Subject)
        end,
        <<"caveats">> => [caveats:serialize(C) || C <- Caveats]
    };
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

translate_value(_, #gri{type = od_token, aspect = preauthorize}, Subject) ->
    #{<<"subject">> => aai:serialize_subject(Subject)};
translate_value(_, #gri{type = od_token, aspect = verify_identity}, Subject) ->
    #{<<"subject">> => aai:serialize_subject(Subject)};

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
        default_space = DefaultSpace,
        space_aliases = SpaceAliases
    } = User,
    #{
        <<"fullName">> => FullName,
        <<"username">> => gs_protocol:undefined_to_null(Username),
        <<"emails">> => Emails,
        <<"linkedAccounts">> => linked_accounts:to_maps(LinkedAccounts),
        <<"defaultSpaceId">> => gs_protocol:undefined_to_null(DefaultSpace),
        <<"spaceAliases">> => SpaceAliases,

        <<"effectiveGroups">> => entity_graph:get_relations(effective, top_down, od_group, User),
        <<"effectiveSpaces">> => entity_graph:get_relations(effective, top_down, od_space, User),
        <<"effectiveHandles">> => entity_graph:get_relations(effective, top_down, od_handle, User),
        <<"effectiveHandleServices">> => entity_graph:get_relations(effective, top_down, od_handle_service, User),

        %% @TODO VFS-4506 deprecated fields, included for backward compatibility
        <<"name">> => FullName,
        <<"login">> => gs_protocol:undefined_to_null(Username),
        <<"alias">> => gs_protocol:undefined_to_null(Username),
        <<"emailList">> => Emails
    };

translate_resource(_, #gri{type = od_user, aspect = instance, scope = protected}, User) ->
    #{
        <<"fullName">> := FullName,
        <<"username">> := Username,
        <<"emails">> := Emails,
        <<"linkedAccounts">> := LinkedAccountMaps
    } = User,
    #{
        <<"fullName">> => FullName,
        <<"username">> => gs_protocol:undefined_to_null(Username),
        <<"emails">> => Emails,
        <<"linkedAccounts">> => LinkedAccountMaps,

        %% @TODO VFS-4506 deprecated fields, included for backward compatibility
        <<"name">> => FullName,
        <<"login">> => gs_protocol:undefined_to_null(Username),
        <<"alias">> => gs_protocol:undefined_to_null(Username),
        <<"emailList">> => Emails
    };

translate_resource(_, #gri{type = od_user, aspect = instance, scope = shared}, User) ->
    #{
        <<"fullName">> := FullName,
        <<"username">> := Username
    } = User,
    #{
        <<"fullName">> => FullName,
        <<"username">> => gs_protocol:undefined_to_null(Username),

        %% @TODO VFS-4506 deprecated field, included for backward compatibility
        <<"name">> => FullName,
        <<"login">> => gs_protocol:undefined_to_null(Username),
        <<"alias">> => gs_protocol:undefined_to_null(Username)
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

        users = Users,

        groups = Groups,

        storages = Storages,
        shares = Shares,
        harvesters = Harvesters
    } = Space,
    #{
        <<"name">> => Name,

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
    #{<<"name">> := Name, <<"providers">> := Providers} = SpaceData,
    #{<<"name">> => Name, <<"providers">> => Providers};

translate_resource(_, #gri{type = od_share, aspect = instance, scope = private}, Share) ->
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

translate_resource(_, #gri{type = od_share, aspect = instance, scope = public}, ShareData) ->
    #{
        <<"name">> := Name, <<"publicUrl">> := PublicUrl,
        <<"rootFileId">> := RootFileId, <<"handleId">> := HandleId
    } = ShareData,
    #{
        <<"name">> => Name, <<"publicUrl">> => PublicUrl,
        <<"rootFileId">> => RootFileId, <<"handleId">> => HandleId
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

        <<"online">> => provider_connection:is_online(Id),

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
        <<"timestamp">> => time_utils:datetime_to_datestamp(Timestamp),
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
        <<"timestamp">> => time_utils:datetime_to_datestamp(Timestamp)
    };

translate_resource(_, #gri{type = od_harvester, aspect = instance, scope = private}, Harvester) ->
    #od_harvester{indices = Indices} = Harvester,
    #{
        <<"indices">> => maps:keys(Indices),
        <<"spaces">> => entity_graph:get_relations(direct, top_down, od_space, Harvester)
    };

translate_resource(_, #gri{type = od_storage, aspect = instance, scope = private}, Storage) ->
    #od_storage{
        provider = Provider,
        qos_parameters = QosParams
    } = Storage,
    #{
        <<"provider">> => Provider,
        <<"qos_parameters">> => QosParams
    };

translate_resource(_, #gri{type = od_storage, aspect = instance, scope = shared}, StorageDetails) ->
    #{
        <<"qos_parameters">> := QosParams
    } = StorageDetails,
    #{
        <<"qos_parameters">> => QosParams
    };

translate_resource(ProtocolVersion, GRI, Data) ->
    ?error("Cannot translate graph sync get result for:~n
    ProtocolVersion: ~p~n
    GRI: ~p~n
    Data: ~p~n", [
        ProtocolVersion, GRI, Data
    ]),
    throw(?ERROR_INTERNAL_SERVER_ERROR).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deprecated - used for backward compatibility with 19.02.* (proto version < 5).
%% @end
%%--------------------------------------------------------------------
-spec deprecated_subject_to_json(aai:subject()) -> json_utils:json_term().
deprecated_subject_to_json(?SUB(user, UserId)) -> #{<<"user">> => UserId};
deprecated_subject_to_json(?SUB(?ONEPROVIDER, PrId)) -> #{<<"provider">> => PrId}.
