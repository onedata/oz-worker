%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour implements gs_translator_behaviour and is used to translate
%%% Graph Sync request results into format understood by GUI client.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_gs_translator).
-author("Lukasz Opiola").

-behaviour(gs_translator_behaviour).

-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/global_definitions.hrl").
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
handshake_attributes(_Client) ->
    BrandSubtitle = oz_worker:get_env(brand_subtitle, ""),
    DefaultHarvestingBackendType = oz_worker:get_env(default_harvesting_backend_type, undefined),
    DefaultHarvestingBackendEndpoint = oz_worker:get_env(default_harvesting_backend_endpoint, undefined),
    #{
        <<"globalTimeSeconds">> => global_clock:timestamp_seconds(),
        <<"zoneName">> => utils:undefined_to_null(oz_worker:get_name()),
        <<"zoneDomain">> => oz_worker:get_domain(),
        <<"serviceVersion">> => oz_worker:get_release_version(),
        <<"serviceBuildVersion">> => oz_worker:get_build_version(),
        <<"brandSubtitle">> => str_utils:unicode_list_to_binary(BrandSubtitle),
        <<"maxTemporaryTokenTtl">> => oz_worker:get_env(max_temporary_token_ttl, 604800), % 1 week
        <<"defaultHarvestingBackendType">> => utils:undefined_to_null(DefaultHarvestingBackendType),
        <<"defaultHarvestingBackendEndpoint">> => utils:undefined_to_null(DefaultHarvestingBackendEndpoint),
        <<"defaultAtmResourceSpec">> => oz_worker:get_env(default_atm_resource_spec),
        <<"availableMarketplaceTags">> => oz_worker:get_env(available_marketplace_tags)
    }.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback translate_value/3.
%% @end
%%--------------------------------------------------------------------
-spec translate_value(gs_protocol:protocol_version(), gri:gri(),
    Value :: term()) -> Result | fun((aai:auth()) -> Result) when
    Result :: gs_protocol:data() | errors:error().
translate_value(_, #gri{aspect = TokenType}, Token) when
    TokenType == invite_user_token;
    TokenType == invite_group_token;
    TokenType == invite_space_token;
    TokenType == space_support_token;
    TokenType == provider_registration_token ->

    serialize_token(Token);
translate_value(_, #gri{aspect = {user_temporary_token, _}}, Token) ->
    serialize_token(Token);
translate_value(_, #gri{type = od_harvester, aspect = {query, _}}, Response) ->
    Response;
translate_value(_, #gri{type = od_harvester, aspect = {gen_curl_query, _}}, Response) ->
    Response;
translate_value(_, #gri{type = od_token, aspect = examine}, Response) ->
    #{
        <<"onezoneDomain">> := OnezoneDomain,
        <<"id">> := Id,
        <<"persistence">> := Persistence,
        <<"subject">> := Subject,
        <<"type">> := Type,
        <<"caveats">> := Caveats
    } = Response,
    TokenTypeJson = case Type of
        ?INVITE_TOKEN(InviteType, EntityId) ->
            InviteTargetNameData = case {InviteType, EntityId} of
                {?USER_JOIN_GROUP, GroupId} ->
                    #{<<"groupName">> => lookup_name(group_logic, GroupId)};
                {?GROUP_JOIN_GROUP, GroupId} ->
                    #{<<"groupName">> => lookup_name(group_logic, GroupId)};
                {?USER_JOIN_SPACE, SpaceId} ->
                    #{<<"spaceName">> => lookup_name(space_logic, SpaceId)};
                {?GROUP_JOIN_SPACE, SpaceId} ->
                    #{<<"spaceName">> => lookup_name(space_logic, SpaceId)};
                {?SUPPORT_SPACE, SpaceId} ->
                    #{<<"spaceName">> => lookup_name(space_logic, SpaceId)};
                {?HARVESTER_JOIN_SPACE, SpaceId} ->
                    #{<<"spaceName">> => lookup_name(space_logic, SpaceId)};
                {?REGISTER_ONEPROVIDER, UserId} ->
                    #{<<"userName">> => lookup_name(user_logic, get_full_name, UserId)};
                {?USER_JOIN_CLUSTER, ClusterId} ->
                    #{<<"clusterName">> => lookup_name(cluster_logic, ClusterId)};
                {?GROUP_JOIN_CLUSTER, ClusterId} ->
                    #{<<"clusterName">> => lookup_name(cluster_logic, ClusterId)};
                {?USER_JOIN_HARVESTER, HarvesterId} ->
                    #{<<"harvesterName">> => lookup_name(harvester_logic, HarvesterId)};
                {?GROUP_JOIN_HARVESTER, HarvesterId} ->
                    #{<<"harvesterName">> => lookup_name(harvester_logic, HarvesterId)};
                {?SPACE_JOIN_HARVESTER, HarvesterId} ->
                    #{<<"harvesterName">> => lookup_name(harvester_logic, HarvesterId)};
                {?USER_JOIN_ATM_INVENTORY, AtmInventoryId} ->
                    #{<<"atmInventoryName">> => lookup_name(atm_inventory_logic, AtmInventoryId)};
                {?GROUP_JOIN_ATM_INVENTORY, AtmInventoryId} ->
                    #{<<"atmInventoryName">> => lookup_name(atm_inventory_logic, AtmInventoryId)}
            end,
            #{<<"inviteToken">> := Json} = token_type:to_json(Type),
            #{<<"inviteToken">> => maps:merge(Json, InviteTargetNameData)};
        _ ->
            token_type:to_json(Type)
    end,
    #{
        <<"onezoneDomain">> => OnezoneDomain,
        <<"id">> => Id,
        <<"persistence">> => Persistence,
        <<"subject">> => aai:subject_to_json(Subject),
        <<"type">> => TokenTypeJson,
        <<"caveats">> => [caveats:to_json(C) || C <- Caveats]
    };
translate_value(_, #gri{type = od_token, aspect = verify_invite_token}, #{<<"subject">> := Sub, <<"ttl">> := TTL}) ->
    #{
        <<"subject">> => aai:serialize_subject(Sub),
        <<"ttl">> => utils:undefined_to_null(TTL)
    };
translate_value(_, #gri{type = od_atm_lambda, aspect = dump}, JsonMap) ->
    JsonMap;
translate_value(_, #gri{type = od_atm_lambda, aspect = {dump_revision, _}}, JsonMap) ->
    JsonMap;
translate_value(_, #gri{type = od_atm_workflow_schema, aspect = dump}, JsonMap) ->
    JsonMap;
translate_value(_, #gri{type = od_atm_workflow_schema, aspect = {dump_revision, _}}, JsonMap) ->
    JsonMap;
translate_value(ProtocolVersion, GRI, Data) ->
    ?error("Cannot translate graph sync create result for:~n"
    "ProtocolVersion: ~p~n"
    "GRI: ~p~n"
    "Data: ~p~n", [
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
translate_resource(_, GRI = #gri{type = od_user}, Data) ->
    translate_user(GRI, Data);
translate_resource(_, GRI = #gri{type = od_group}, Data) ->
    translate_group(GRI, Data);
translate_resource(_, GRI = #gri{type = od_space}, Data) ->
    translate_space(GRI, Data);
translate_resource(_, GRI = #gri{type = od_share}, Data) ->
    translate_share(GRI, Data);
translate_resource(_, GRI = #gri{type = od_provider}, Data) ->
    translate_provider(GRI, Data);
translate_resource(_, GRI = #gri{type = od_token}, Data) ->
    translate_token(GRI, Data);
translate_resource(_, GRI = #gri{type = od_harvester}, Data) ->
    translate_harvester(GRI, Data);
translate_resource(_, GRI = #gri{type = od_cluster}, Data) ->
    translate_cluster(GRI, Data);
translate_resource(_, GRI = #gri{type = od_atm_inventory}, Data) ->
    translate_atm_inventory(GRI, Data);
translate_resource(_, GRI = #gri{type = od_atm_lambda}, Data) ->
    translate_atm_lambda(GRI, Data);
translate_resource(_, GRI = #gri{type = od_atm_workflow_schema}, Data) ->
    translate_atm_workflow_schema(GRI, Data);
translate_resource(_, GRI = #gri{type = oz_worker}, Data) ->
    translate_zone(GRI, Data);

translate_resource(ProtocolVersion, GRI, Data) ->
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

%% @private
-spec translate_user(gri:gri(), Data :: term()) ->
    gs_protocol:data() | fun((aai:auth()) -> gs_protocol:data()).
translate_user(GRI = #gri{type = od_user, id = UserId, aspect = instance, scope = private}, User) ->
    #od_user{
        basic_auth_enabled = BasicAuthEnabled,
        password_hash = PasswordHash,
        full_name = FullName,
        username = Username
    } = User,
    CanInviteProviders = open =:= oz_worker:get_env(provider_registration_policy, open) orelse
        user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_INVITE),
    #{
        <<"scope">> => <<"private">>,
        <<"basicAuthEnabled">> => BasicAuthEnabled,
        <<"hasPassword">> => PasswordHash /= undefined,
        <<"fullName">> => FullName,
        <<"username">> => utils:undefined_to_null(Username),
        <<"canInviteProviders">> => CanInviteProviders,
        <<"tokenList">> => gri:serialize(#gri{type = od_token, id = undefined, aspect = {user_named_tokens, UserId}}),
        <<"linkedAccountList">> => gri:serialize(GRI#gri{aspect = linked_accounts, scope = private}),
        <<"groupList">> => gri:serialize(GRI#gri{aspect = eff_groups, scope = private}),
        <<"spaceList">> => gri:serialize(GRI#gri{aspect = eff_spaces, scope = private}),
        <<"providerList">> => gri:serialize(GRI#gri{aspect = eff_providers, scope = private}),
        <<"harvesterList">> => gri:serialize(GRI#gri{aspect = eff_harvesters, scope = private}),
        <<"clusterList">> => gri:serialize(GRI#gri{aspect = eff_clusters, scope = private}),
        <<"atmInventoryList">> => gri:serialize(GRI#gri{aspect = eff_atm_inventories, scope = private}),
        <<"info">> => #{
            <<"creationTime">> => User#od_user.creation_time
        }
    };

translate_user(#gri{aspect = instance, scope = protected}, User) ->
    #{
        <<"fullName">> := FullName,
        <<"username">> := Username
    } = User,
    #{
        <<"scope">> => <<"protected">>,
        <<"fullName">> => FullName,
        <<"username">> => utils:undefined_to_null(Username)
    };

translate_user(#gri{aspect = instance, scope = shared}, User) ->
    #{
        <<"fullName">> := FullName,
        <<"username">> := Username
    } = User,
    #{
        <<"scope">> => <<"shared">>,
        <<"fullName">> => FullName,
        <<"username">> => utils:undefined_to_null(Username)
    };

translate_user(GRI = #gri{aspect = linked_accounts}, LinkedAccounts) ->
    #{
        <<"list">> => lists:map(
            fun(GeneratedUserId) ->
                gri:serialize(GRI#gri{aspect = {linked_account, GeneratedUserId}, scope = private})
            end, LinkedAccounts)
    };

translate_user(#gri{aspect = {linked_account, _}}, #linked_account{idp = IdP, emails = Emails}) ->
    #{
        <<"idp">> => IdP,
        <<"emails">> => Emails
    };

translate_user(#gri{aspect = eff_groups}, Groups) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_group, aspect = instance, scope = auto}, Groups)
    };

translate_user(#gri{aspect = eff_spaces}, Spaces) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_space, aspect = instance, scope = auto}, Spaces)
    };

translate_user(#gri{aspect = eff_providers}, Providers) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_provider, aspect = instance, scope = auto}, Providers)
    };

translate_user(#gri{aspect = eff_harvesters}, Harvesters) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_harvester, aspect = instance, scope = auto}, Harvesters)
    };

translate_user(#gri{aspect = eff_clusters}, Clusters) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_cluster, aspect = instance, scope = auto}, Clusters)
    };

translate_user(#gri{aspect = eff_atm_inventories}, AtmInventories) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_atm_inventory, aspect = instance, scope = auto}, AtmInventories)
    }.


%% @private
-spec translate_group(gri:gri(), Data :: term()) ->
    gs_protocol:data() | fun((aai:auth()) -> gs_protocol:data()).
translate_group(#gri{id = undefined, aspect = privileges, scope = private}, Privileges) ->
    Privileges;

translate_group(#gri{id = GroupId, aspect = instance, scope = private}, Group) ->
    fun(?USER(UserId)) -> #{
        <<"name">> => Group#od_group.name,
        <<"type">> => Group#od_group.type,
        <<"scope">> => <<"private">>,
        <<"canViewPrivileges">> => group_logic:has_eff_privilege(Group, UserId, ?GROUP_VIEW_PRIVILEGES),
        <<"directMembership">> => group_logic:has_direct_user(Group, UserId),
        <<"parentList">> => gri:serialize(#gri{type = od_group, id = GroupId, aspect = parents}),
        <<"childList">> => gri:serialize(#gri{type = od_group, id = GroupId, aspect = children}),
        <<"effChildList">> => gri:serialize(#gri{type = od_group, id = GroupId, aspect = eff_children}),
        <<"userList">> => gri:serialize(#gri{type = od_group, id = GroupId, aspect = users}),
        <<"effUserList">> => gri:serialize(#gri{type = od_group, id = GroupId, aspect = eff_users}),
        <<"spaceList">> => gri:serialize(#gri{type = od_group, id = GroupId, aspect = spaces}),
        <<"harvesterList">> => gri:serialize(#gri{type = od_group, id = GroupId, aspect = eff_harvesters}),
        <<"atmInventoryList">> => gri:serialize(#gri{type = od_group, id = GroupId, aspect = eff_atm_inventories}),
        <<"info">> => maps:merge(translate_creator(Group#od_group.creator), #{
            <<"creationTime">> => Group#od_group.creation_time
        })
    } end;

translate_group(#gri{id = GroupId, aspect = instance, scope = protected}, Group) ->
    #{
        <<"name">> := Name,
        <<"type">> := Type,
        <<"creator">> := Creator,
        <<"creationTime">> := CreationTime
    } = Group,
    fun(?USER(UserId)) -> #{
        <<"name">> => Name,
        <<"type">> => Type,
        <<"info">> => maps:merge(translate_creator(Creator), #{
            <<"creationTime">> => CreationTime
        }),
        <<"scope">> => <<"protected">>,
        <<"directMembership">> => group_logic:has_direct_user(GroupId, UserId)
    } end;

translate_group(#gri{aspect = instance, scope = shared}, Group) ->
    #{
        <<"name">> := Name,
        <<"type">> := Type,
        <<"creationTime">> := CreationTime
    } = Group,
    #{
        <<"name">> => Name,
        <<"type">> => Type,
        <<"info">> => #{
            <<"creationTime">> => CreationTime
        },
        <<"scope">> => <<"shared">>
    };

translate_group(#gri{aspect = As}, Groups) when As =:= parents; As =:= children; As =:= eff_children ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_group, aspect = instance, scope = auto}, Groups)
    };

translate_group(#gri{aspect = As}, Users) when As =:= users; As =:= eff_users ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_user, aspect = instance, scope = auto}, Users)
    };

translate_group(#gri{aspect = {user_privileges, _UserId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_group(#gri{aspect = {eff_user_privileges, _UserId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_group(#gri{aspect = {eff_user_membership, _UserId}}, Intermediaries) ->
    format_intermediaries(Intermediaries);

translate_group(#gri{aspect = {eff_child_membership, _UserId}}, Intermediaries) ->
    format_intermediaries(Intermediaries);

translate_group(#gri{aspect = {child_privileges, _ChildId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_group(#gri{aspect = {eff_child_privileges, _ChildId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_group(#gri{aspect = spaces}, Spaces) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_space, aspect = instance, scope = auto}, Spaces)
    };

translate_group(#gri{aspect = eff_harvesters}, Harvesters) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_harvester, aspect = instance, scope = auto}, Harvesters)
    };

translate_group(#gri{aspect = eff_atm_inventories}, AtmInventories) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_atm_inventory, aspect = instance, scope = auto}, AtmInventories)
    }.


%% @private
-spec translate_space(gri:gri(), Data :: term()) ->
    gs_protocol:data() | fun((aai:auth()) -> gs_protocol:data()).
translate_space(#gri{id = undefined, aspect = privileges, scope = private}, Privileges) ->
    Privileges;

translate_space(#gri{aspect = api_samples, scope = private}, ApiSamples) ->
    ApiSamples;

translate_space(#gri{id = SpaceId, aspect = instance, scope = private}, Space) ->
    #od_space{
        name = Name,
        shares = Shares,
        support_parameters_registry = SupportParametersRegistry
    } = Space,
    fun(?USER(UserId)) -> #{
        <<"name">> => Name,
        <<"scope">> => <<"private">>,
        <<"directMembership">> => space_logic:has_direct_user(Space, UserId),
        <<"currentUserIsOwner">> => space_logic:is_owner(Space, UserId),
        <<"currentUserEffPrivileges">> => entity_graph:get_relation_attrs(effective, bottom_up, od_user, UserId, Space),
        <<"ownerList">> => gri:serialize(#gri{type = od_space, id = SpaceId, aspect = owners}),
        <<"userList">> => gri:serialize(#gri{type = od_space, id = SpaceId, aspect = users}),
        <<"effUserList">> => gri:serialize(#gri{type = od_space, id = SpaceId, aspect = eff_users}),
        <<"groupList">> => gri:serialize(#gri{type = od_space, id = SpaceId, aspect = groups}),
        <<"effGroupList">> => gri:serialize(#gri{type = od_space, id = SpaceId, aspect = eff_groups}),
        <<"shareList">> => gri:serialize(#gri{type = od_space, id = SpaceId, aspect = shares}),
        <<"supportSizes">> => entity_graph:get_relations_with_attrs(effective, top_down, od_provider, Space),
        <<"providerList">> => gri:serialize(#gri{type = od_space, id = SpaceId, aspect = eff_providers}),
        <<"harvesterList">> => gri:serialize(#gri{type = od_space, id = SpaceId, aspect = harvesters}),
        <<"supportParametersRegistry">> => jsonable_record:to_json(SupportParametersRegistry, support_parameters_registry),
        <<"info">> => maps:merge(translate_creator(Space#od_space.creator), #{
            <<"creationTime">> => Space#od_space.creation_time,
            <<"sharesCount">> => length(Shares)
        })
    } end;

translate_space(#gri{id = SpaceId, aspect = instance, scope = protected}, SpaceData) ->
    #{
        <<"name">> := Name,
        <<"providers">> := SupportSizes,
        <<"creationTime">> := CreationTime,
        <<"creator">> := Creator,
        <<"sharesCount">> := SharesCount,
        <<"supportParametersRegistry">> := SupportParametersRegistry
    } = SpaceData,
    {ok, #document{value = Space}} = od_space:get(SpaceId),
    fun(?USER(UserId)) -> #{
        <<"name">> => Name,
        <<"scope">> => <<"protected">>,
        <<"directMembership">> => space_logic:has_direct_user(Space, UserId),
        <<"currentUserIsOwner">> => space_logic:is_owner(Space, UserId),
        <<"currentUserEffPrivileges">> => entity_graph:get_relation_attrs(effective, bottom_up, od_user, UserId, Space),
        <<"supportSizes">> => SupportSizes,
        <<"providerList">> => gri:serialize(#gri{type = od_space, id = SpaceId, aspect = eff_providers}),
        <<"supportParametersRegistry">> => jsonable_record:to_json(SupportParametersRegistry, support_parameters_registry),
        <<"info">> => maps:merge(translate_creator(Creator), #{
            <<"creationTime">> => CreationTime,
            <<"sharesCount">> => SharesCount
        })

    } end;

translate_space(#gri{aspect = As}, Users) when As =:= users; As =:= owners; As =:= eff_users ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_user, aspect = instance, scope = auto}, Users)
    };

translate_space(#gri{aspect = As}, Groups) when As =:= groups; As =:= eff_groups ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_group, aspect = instance, scope = auto}, Groups)
    };

translate_space(#gri{aspect = {user_privileges, _UserId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_space(#gri{aspect = {eff_user_privileges, _UserId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_space(#gri{aspect = {eff_user_membership, _UserId}}, Intermediaries) ->
    format_intermediaries(Intermediaries);

translate_space(#gri{aspect = {eff_group_membership, _UserId}}, Intermediaries) ->
    format_intermediaries(Intermediaries);

translate_space(#gri{aspect = {group_privileges, _GroupId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_space(#gri{aspect = {eff_group_privileges, _GroupId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_space(#gri{aspect = shares}, Shares) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_share, aspect = instance, scope = auto}, Shares)
    };

translate_space(#gri{aspect = eff_providers, scope = private}, Providers) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_provider, aspect = instance, scope = auto}, Providers)
    };

translate_space(#gri{aspect = harvesters}, Harvesters) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_harvester, aspect = instance, scope = auto}, Harvesters)
    }.


%% @private
-spec translate_share(gri:gri(), Data :: term()) ->
    gs_protocol:data() | fun((aai:auth()) -> gs_protocol:data()).
translate_share(#gri{aspect = instance, scope = private}, Share) ->
    #od_share{name = Name, file_type = FileType, space = SpaceId} = Share,
    #{
        <<"name">> => Name,
        <<"space">> => gri:serialize(#gri{type = od_space, id = SpaceId, aspect = instance, scope = auto}),
        <<"fileType">> => FileType
    };
translate_share(#gri{id = ShareId, aspect = instance, scope = public}, #{<<"name">> := Name}) ->
    {ok, {ChosenProviderId, ChosenProviderVersion}} = share_logic:choose_provider_for_public_share_handling(ShareId),
    #{
        <<"name">> => Name,
        <<"chosenProviderId">> => utils:undefined_to_null(ChosenProviderId),
        <<"chosenProviderVersion">> => utils:undefined_to_null(ChosenProviderVersion)
    }.


%% @private
-spec translate_provider(gri:gri(), Data :: term()) ->
    gs_protocol:data() | fun((aai:auth()) -> gs_protocol:data()).
translate_provider(#gri{type = od_provider, aspect = current_time}, TimeMillis) ->
    #{<<"timeMillis">> => TimeMillis};
translate_provider(#gri{type = od_provider, aspect = instance, scope = private}, {_Provider, RootToken}) ->
    % This covers provider creation via Graph Sync, in contrast to the get
    % request that does not return the root token
    #{
        <<"providerRootToken">> => serialize_token(RootToken)
    };
translate_provider(GRI = #gri{id = Id, aspect = instance, scope = private}, Provider) ->
    #od_provider{
        name = Name, domain = Domain,
        latitude = Latitude, longitude = Longitude,
        creation_time = CreationTime
    } = Provider,

    ClusterId = Id,
    fun(?USER(UserId)) -> #{
        <<"scope">> => <<"private">>,
        <<"name">> => Name,
        <<"domain">> => Domain,
        <<"latitude">> => Latitude,
        <<"longitude">> => Longitude,
        <<"cluster">> => gri:serialize(#gri{
            type = od_cluster, id = ClusterId, aspect = instance, scope = auto
        }),
        <<"online">> => provider_connections:is_online(Id),
        <<"spaceList">> => gri:serialize(GRI#gri{aspect = {user_spaces, UserId}, scope = private}),
        <<"info">> => #{
            <<"creationTime">> => CreationTime
        }
    } end;

translate_provider(GRI = #gri{id = Id, aspect = instance, scope = protected}, Provider) ->
    #{
        <<"name">> := Name, <<"domain">> := Domain,
        <<"latitude">> := Latitude, <<"longitude">> := Longitude,
        <<"online">> := Online,
        <<"creationTime">> := CreationTime
    } = Provider,

    ClusterId = Id,
    fun(?USER(UserId)) -> #{
        <<"scope">> => <<"protected">>,
        <<"name">> => Name,
        <<"domain">> => Domain,
        <<"latitude">> => Latitude,
        <<"longitude">> => Longitude,
        <<"cluster">> => gri:serialize(#gri{
            type = od_cluster, id = ClusterId, aspect = instance, scope = auto
        }),
        <<"online">> => Online,
        <<"spaceList">> => gri:serialize(GRI#gri{aspect = {user_spaces, UserId}, scope = private}),
        <<"info">> => #{
            <<"creationTime">> => CreationTime
        }
    } end;

translate_provider(#gri{aspect = instance, scope = shared}, Provider) ->
    #{
        <<"name">> := Name
    } = Provider,

    #{
        <<"scope">> => <<"shared">>,
        <<"name">> => Name
    };

translate_provider(#gri{aspect = {eff_user_membership, _UserId}}, Intermediaries) ->
    format_intermediaries(Intermediaries);

translate_provider(#gri{aspect = {eff_group_membership, _UserId}}, Intermediaries) ->
    format_intermediaries(Intermediaries);

translate_provider(#gri{aspect = {user_spaces, _UserId}}, Spaces) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_space, aspect = instance, scope = auto}, Spaces)
    };

translate_provider(#gri{aspect = eff_users}, Users) ->
    #{<<"list">> => Users};

translate_provider(#gri{aspect = eff_groups}, Groups) ->
    #{<<"list">> => Groups};

translate_provider(#gri{aspect = spaces}, Spaces) ->
    #{<<"list">> => Spaces}.


%% @private
-spec translate_token(gri:gri(), Data :: term()) ->
    gs_protocol:data() | fun((aai:auth()) -> gs_protocol:data()).
translate_token(#gri{id = undefined, aspect = {user_named_tokens, _}}, Tokens) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_token, aspect = instance, scope = auto}, Tokens)
    };
translate_token(#gri{aspect = instance, scope = private}, TokenData) ->
    #{
        <<"id">> := TokenId,
        <<"name">> := Name,
        <<"subject">> := Subject,
        <<"type">> := Type,
        <<"caveats">> := Caveats,
        <<"metadata">> := Metadata,
        <<"revoked">> := Revoked,
        <<"token">> := Token
    } = TokenData,
    #{
        <<"id">> => TokenId,
        <<"name">> => Name,
        <<"subject">> => aai:subject_to_json(Subject),
        <<"type">> => token_type:to_json(Type),
        <<"caveats">> => [caveats:to_json(C) || C <- Caveats],
        <<"metadata">> => Metadata,
        <<"revoked">> => Revoked,
        <<"token">> => serialize_token(Token)
    }.


%% @private
-spec translate_harvester(gri:gri(), Data :: term()) ->
    gs_protocol:data() | fun((aai:auth()) -> gs_protocol:data()).
translate_harvester(#gri{id = undefined, aspect = privileges, scope = private}, Privileges) ->
    Privileges;

translate_harvester(#gri{id = HarvesterId, aspect = instance, scope = private}, Harvester) ->
    #od_harvester{
        name = Name, endpoint = Endpoint,
        backend = BackendType, public = Public
    } = Harvester,
    fun(?USER(UserId)) -> #{
        <<"scope">> => <<"private">>,
        <<"name">> => Name,
        <<"public">> => Public,
        <<"harvestingBackendEndpoint">> => Endpoint,
        <<"harvestingBackendType">> => atom_to_binary(BackendType, utf8),
        <<"canViewPrivileges">> => harvester_logic:has_eff_privilege(Harvester, UserId, ?HARVESTER_VIEW_PRIVILEGES),
        <<"directMembership">> => harvester_logic:has_direct_user(Harvester, UserId),
        <<"guiPluginConfig">> => gri:serialize(#gri{type = od_harvester, id = HarvesterId, aspect = gui_plugin_config}),
        <<"indexList">> => gri:serialize(#gri{type = od_harvester, id = HarvesterId, aspect = indices}),
        <<"userList">> => gri:serialize(#gri{type = od_harvester, id = HarvesterId, aspect = users}),
        <<"effUserList">> => gri:serialize(#gri{type = od_harvester, id = HarvesterId, aspect = eff_users}),
        <<"groupList">> => gri:serialize(#gri{type = od_harvester, id = HarvesterId, aspect = groups}),
        <<"effGroupList">> => gri:serialize(#gri{type = od_harvester, id = HarvesterId, aspect = eff_groups}),
        <<"spaceList">> => gri:serialize(#gri{type = od_harvester, id = HarvesterId, aspect = spaces}),
        <<"effProviderList">> => gri:serialize(#gri{type = od_harvester, id = HarvesterId, aspect = eff_providers}),
        <<"info">> => maps:merge(translate_creator(Harvester#od_harvester.creator), #{
            <<"creationTime">> => Harvester#od_harvester.creation_time
        })
    } end;

translate_harvester(#gri{id = HarvesterId, aspect = instance, scope = protected}, HarvesterData) ->
    #{
        <<"name">> := Name,
        <<"public">> := Public,
        <<"harvestingBackendType">> := HarvestingBackendType,
        <<"harvestingBackendEndpoint">> := Endpoint,
        <<"creationTime">> := CreationTime,
        <<"creator">> := Creator
    } = HarvesterData,
    fun(?USER(UserId)) -> ProtectedData = #{
        <<"scope">> => <<"protected">>,
        <<"name">> => Name,
        <<"public">> => Public,
        <<"harvestingBackendType">> => HarvestingBackendType,
        <<"harvestingBackendEndpoint">> => Endpoint,
        <<"directMembership">> => harvester_logic:has_direct_user(HarvesterId, UserId),
        <<"info">> => maps:merge(translate_creator(Creator), #{
            <<"creationTime">> => CreationTime
        })},
        case Public of
            true ->
                ProtectedData#{
                    <<"guiPluginConfig">> => gri:serialize(#gri{type = od_harvester, id = HarvesterId, aspect = gui_plugin_config}),
                    <<"indexList">> => gri:serialize(#gri{type = od_harvester, id = HarvesterId, aspect = indices})
                };
            _ ->
                ProtectedData
        end
    end;

% used to display the list of harvesters in a space
translate_harvester(#gri{aspect = instance, scope = shared}, #{<<"name">> := Name}) ->
    #{
        <<"scope">> => <<"shared">>,
        <<"name">> => Name
    };

% used to display harvester's public GUI - if the public mode is enabled
translate_harvester(#gri{id = HarvesterId, aspect = instance, scope = public}, #{<<"name">> := Name}) ->
    #{
        <<"scope">> => <<"public">>,
        <<"name">> => Name,
        <<"guiPluginConfig">> => gri:serialize(#gri{type = od_harvester, id = HarvesterId, aspect = gui_plugin_config}),
        <<"indexList">> => gri:serialize(#gri{type = od_harvester, id = HarvesterId, aspect = indices})
    };

translate_harvester(#gri{aspect = gui_plugin_config}, Config) ->
    #{
        <<"guiPluginConfig">> => Config
    };

translate_harvester(#gri{aspect = As}, Users) when As =:= users; As =:= eff_users ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_user, aspect = instance, scope = auto}, Users)
    };

translate_harvester(#gri{aspect = As}, Groups) when As =:= groups; As =:= eff_groups ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_group, aspect = instance, scope = auto}, Groups)
    };

translate_harvester(#gri{aspect = spaces}, Spaces) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_space, aspect = instance, scope = auto}, Spaces)
    };

translate_harvester(#gri{aspect = eff_providers}, Providers) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_provider, aspect = instance, scope = auto}, Providers)
    };

translate_harvester(#gri{aspect = {user_privileges, _UserId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_harvester(#gri{aspect = {eff_user_privileges, _UserId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_harvester(#gri{aspect = {eff_user_membership, _UserId}}, Intermediaries) ->
    format_intermediaries(Intermediaries);

translate_harvester(#gri{aspect = {eff_group_membership, _UserId}}, Intermediaries) ->
    format_intermediaries(Intermediaries);

translate_harvester(#gri{aspect = {group_privileges, _GroupId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_harvester(#gri{aspect = {eff_group_privileges, _GroupId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_harvester(#gri{aspect = all_backend_types}, Plugins) ->
    #{
        <<"allBackendTypes">> => Plugins
    };

translate_harvester(#gri{aspect = {index, _}, scope = private}, IndexData) ->
    #harvester_index{
        name = Name,
        schema = Schema,
        gui_plugin_name = GuiPluginName,
        include_metadata = IncludeMetadata,
        include_file_details = IncludeFileDetails,
        include_rejection_reason = IncludeRejectionReason,
        retry_on_rejection = RetryOnRejection
    } = IndexData,
    #{
        <<"name">> => Name,
        <<"schema">> => utils:undefined_to_null(Schema),
        <<"guiPluginName">> => utils:undefined_to_null(GuiPluginName),
        <<"includeMetadata">> => IncludeMetadata,
        <<"includeFileDetails">> => IncludeFileDetails,
        <<"includeRejectionReason">> => IncludeRejectionReason,
        <<"retryOnRejection">> => RetryOnRejection
    };

translate_harvester(#gri{aspect = {index, _}, scope = public}, IndexData) ->
    #{
        <<"guiPluginName">> := GuiPluginName
    } = IndexData,
    #{
        <<"guiPluginName">> => utils:undefined_to_null(GuiPluginName)
    };

translate_harvester(#gri{aspect = indices, id = HarvesterId}, Indices) ->
    #{
        <<"list">> => lists:map(
            fun(IndexId) ->
                gri:serialize(#gri{type = od_harvester, id = HarvesterId, aspect = {index, IndexId}, scope = auto})
            end, Indices)
    };

translate_harvester(#gri{aspect = {index_stats, _}}, IndexStats) ->
    #{
        <<"indexStats">> => IndexStats
    }.


%% @private
-spec translate_cluster(gri:gri(), Data :: term()) ->
    gs_protocol:data() | fun((aai:auth()) -> gs_protocol:data()).
translate_cluster(#gri{id = undefined, aspect = privileges, scope = private}, Privileges) ->
    Privileges;

translate_cluster(#gri{id = ClusterId, aspect = instance, scope = private}, Cluster) ->
    #od_cluster{
        type = Type,
        worker_version = WorkerVersion,
        onepanel_version = OnepanelVersion,
        onepanel_proxy = OnepanelProxy,
        creation_time = CreationTime,
        creator = Creator
    } = Cluster,

    ProviderId = ClusterId,
    fun(?USER(UserId)) -> #{
        <<"type">> => Type,
        <<"provider">> => case Type of
            ?ONEZONE ->
                null;
            ?ONEPROVIDER ->
                gri:serialize(#gri{
                    type = od_provider, id = ProviderId, aspect = instance, scope = auto
                })
        end,
        <<"scope">> => <<"private">>,
        <<"canViewPrivileges">> => cluster_logic:has_eff_privilege(Cluster, UserId, ?CLUSTER_VIEW_PRIVILEGES),
        <<"directMembership">> => cluster_logic:has_direct_user(Cluster, UserId),
        <<"workerVersion">> => cluster_logic:version_info_to_json(WorkerVersion),
        <<"onepanelVersion">> => cluster_logic:version_info_to_json(OnepanelVersion),
        <<"onepanelProxy">> => OnepanelProxy,
        <<"info">> => maps:merge(translate_creator(Creator), #{
            <<"creationTime">> => CreationTime
        }),
        <<"userList">> => gri:serialize(#gri{type = od_cluster, id = ClusterId, aspect = users}),
        <<"effUserList">> => gri:serialize(#gri{type = od_cluster, id = ClusterId, aspect = eff_users}),
        <<"groupList">> => gri:serialize(#gri{type = od_cluster, id = ClusterId, aspect = groups}),
        <<"effGroupList">> => gri:serialize(#gri{type = od_cluster, id = ClusterId, aspect = eff_groups})
    } end;

translate_cluster(#gri{id = ClusterId, aspect = instance, scope = protected}, Cluster) ->
    #{
        <<"type">> := Type,
        <<"workerVersion">> := WorkerVersion,
        <<"onepanelVersion">> := OnepanelVersion,
        <<"onepanelProxy">> := OnepanelProxy,
        <<"creationTime">> := CreationTime,
        <<"creator">> := Creator
    } = Cluster,

    ProviderId = ClusterId,
    fun(?USER(UserId)) -> #{
        <<"scope">> => <<"protected">>,
        <<"directMembership">> => cluster_logic:has_direct_user(ClusterId, UserId),
        <<"type">> => Type,
        <<"provider">> => case Type of
            ?ONEZONE ->
                null;
            ?ONEPROVIDER ->
                gri:serialize(#gri{
                    type = od_provider, id = ProviderId, aspect = instance, scope = auto
                })
        end,
        <<"workerVersion">> => WorkerVersion,
        <<"onepanelVersion">> => OnepanelVersion,
        <<"onepanelProxy">> => OnepanelProxy,
        <<"info">> => maps:merge(translate_creator(Creator), #{
            <<"creationTime">> => CreationTime
        })
    } end;

translate_cluster(#gri{id = ClusterId, aspect = instance, scope = public}, Cluster) ->
    #{
        <<"type">> := Type,
        <<"workerVersion">> := WorkerVersion,
        <<"onepanelVersion">> := OnepanelVersion,
        <<"creationTime">> := CreationTime
    } = Cluster,

    ProviderId = ClusterId,
    fun(?USER(UserId)) -> #{
        <<"scope">> => <<"public">>,
        <<"directMembership">> => cluster_logic:has_direct_user(ClusterId, UserId),
        <<"type">> => Type,
        <<"provider">> => case Type of
            ?ONEZONE ->
                null;
            ?ONEPROVIDER ->
                gri:serialize(#gri{
                    type = od_provider, id = ProviderId, aspect = instance, scope = auto
                })
        end,
        <<"workerVersion">> => WorkerVersion,
        <<"onepanelVersion">> => OnepanelVersion,
        <<"info">> => #{
            <<"creationTime">> => CreationTime
        }
    } end;

translate_cluster(#gri{aspect = As}, Users) when As =:= users; As =:= eff_users ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_user, aspect = instance, scope = auto}, Users)
    };

translate_cluster(#gri{aspect = As}, Groups) when As =:= groups; As =:= eff_groups ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_group, aspect = instance, scope = auto}, Groups)
    };

translate_cluster(#gri{aspect = {user_privileges, _UserId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_cluster(#gri{aspect = {eff_user_privileges, _UserId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_cluster(#gri{aspect = {eff_user_membership, _UserId}}, Intermediaries) ->
    format_intermediaries(Intermediaries);

translate_cluster(#gri{aspect = {eff_group_membership, _UserId}}, Intermediaries) ->
    format_intermediaries(Intermediaries);

translate_cluster(#gri{aspect = {group_privileges, _GroupId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_cluster(#gri{aspect = {eff_group_privileges, _GroupId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    }.


%% @private
-spec translate_atm_inventory(gri:gri(), Data :: term()) ->
    gs_protocol:data() | fun((aai:auth()) -> gs_protocol:data()).
translate_atm_inventory(#gri{id = undefined, aspect = privileges, scope = private}, Privileges) ->
    Privileges;

translate_atm_inventory(#gri{id = AtmInventoryId, aspect = instance, scope = private}, AtmInventory) ->
    #od_atm_inventory{
        name = Name,
        creation_time = CreationTime,
        creator = Creator
    } = AtmInventory,

    fun(?USER(UserId)) -> #{
        <<"scope">> => <<"private">>,
        <<"name">> => Name,
        <<"canViewPrivileges">> => atm_inventory_logic:has_eff_privilege(AtmInventory, UserId, ?ATM_INVENTORY_VIEW_PRIVILEGES),
        <<"directMembership">> => atm_inventory_logic:has_direct_user(AtmInventory, UserId),
        <<"currentUserEffPrivileges">> => entity_graph:get_relation_attrs(effective, bottom_up, od_user, UserId, AtmInventory),
        <<"userList">> => gri:serialize(#gri{type = od_atm_inventory, id = AtmInventoryId, aspect = users}),
        <<"effUserList">> => gri:serialize(#gri{type = od_atm_inventory, id = AtmInventoryId, aspect = eff_users}),
        <<"groupList">> => gri:serialize(#gri{type = od_atm_inventory, id = AtmInventoryId, aspect = groups}),
        <<"effGroupList">> => gri:serialize(#gri{type = od_atm_inventory, id = AtmInventoryId, aspect = eff_groups}),
        <<"atmLambdaList">> => gri:serialize(#gri{type = od_atm_inventory, id = AtmInventoryId, aspect = atm_lambdas}),
        <<"atmWorkflowSchemaList">> => gri:serialize(#gri{type = od_atm_inventory, id = AtmInventoryId, aspect = atm_workflow_schemas}),
        <<"info">> => maps:merge(translate_creator(Creator), #{
            <<"creationTime">> => CreationTime
        })
    } end;

translate_atm_inventory(#gri{id = AtmInventoryId, aspect = instance, scope = protected}, AtmInventoryData) ->
    #{
        <<"name">> := Name,
        <<"creationTime">> := CreationTime,
        <<"creator">> := Creator
    } = AtmInventoryData,

    {ok, #document{value = AtmInventory}} = od_atm_inventory:get(AtmInventoryId),
    fun(?USER(UserId)) -> #{
        <<"scope">> => <<"protected">>,
        <<"name">> => Name,
        <<"directMembership">> => atm_inventory_logic:has_direct_user(AtmInventory, UserId),
        <<"currentUserEffPrivileges">> => entity_graph:get_relation_attrs(effective, bottom_up, od_user, UserId, AtmInventory),
        <<"info">> => maps:merge(translate_creator(Creator), #{
            <<"creationTime">> => CreationTime
        })
    } end;

translate_atm_inventory(#gri{aspect = As}, Users) when As =:= users; As =:= eff_users ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_user, aspect = instance, scope = auto}, Users)
    };

translate_atm_inventory(#gri{aspect = As}, Groups) when As =:= groups; As =:= eff_groups ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_group, aspect = instance, scope = auto}, Groups)
    };

translate_atm_inventory(#gri{aspect = {user_privileges, _UserId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_atm_inventory(#gri{aspect = {eff_user_privileges, _UserId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_atm_inventory(#gri{aspect = {eff_user_membership, _UserId}}, Intermediaries) ->
    format_intermediaries(Intermediaries);

translate_atm_inventory(#gri{aspect = {eff_group_membership, _UserId}}, Intermediaries) ->
    format_intermediaries(Intermediaries);

translate_atm_inventory(#gri{aspect = {group_privileges, _GroupId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_atm_inventory(#gri{aspect = {eff_group_privileges, _GroupId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_atm_inventory(#gri{aspect = atm_lambdas}, AtmLambdas) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_atm_lambda, aspect = instance, scope = auto}, AtmLambdas)
    };

translate_atm_inventory(#gri{aspect = atm_workflow_schemas}, AtmWorkflowSchemas) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_atm_workflow_schema, aspect = instance, scope = auto}, AtmWorkflowSchemas)
    }.


%% @private
-spec translate_atm_lambda(gri:gri(), Data :: term()) ->
    gs_protocol:data() | fun((aai:auth()) -> gs_protocol:data()).
translate_atm_lambda(GRI = #gri{aspect = instance, scope = private}, AtmLambda) ->
    #od_atm_lambda{
        revision_registry = RevisionRegistry,

        original_atm_lambda = OriginalAtmLambdaId,

        creation_time = CreationTime,
        creator = Creator
    } = AtmLambda,

    #{
        <<"scope">> => <<"private">>,

        <<"revisionRegistry">> => jsonable_record:to_json(RevisionRegistry, atm_lambda_revision_registry),

        <<"originalAtmLambda">> => case OriginalAtmLambdaId of
            undefined ->
                null;
            _ ->
                gri:serialize(#gri{
                    type = od_atm_lambda,
                    id = OriginalAtmLambdaId,
                    aspect = instance,
                    scope = auto
                })
        end,
        <<"atmInventoryList">> => gri:serialize(GRI#gri{aspect = atm_inventories, scope = private}),
        <<"atmWorkflowSchemaList">> => gri:serialize(GRI#gri{aspect = atm_workflow_schemas, scope = private}),

        <<"info">> => maps:merge(translate_creator(Creator), #{
            <<"creationTime">> => CreationTime
        })
    };

translate_atm_lambda(#gri{aspect = atm_inventories}, AtmInventories) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_atm_inventory, aspect = instance, scope = auto}, AtmInventories)
    };

translate_atm_lambda(#gri{aspect = atm_workflow_schemas}, AtmWorkflowSchemas) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_atm_workflow_schema, aspect = instance, scope = auto}, AtmWorkflowSchemas)
    }.


%% @private
-spec translate_atm_workflow_schema(gri:gri(), Data :: term()) ->
    gs_protocol:data() | fun((aai:auth()) -> gs_protocol:data()).
translate_atm_workflow_schema(#gri{id = Id, aspect = instance, scope = private}, AtmLambda) ->
    #od_atm_workflow_schema{
        name = Name,
        summary = Summary,

        revision_registry = RevisionRegistry,

        original_atm_workflow_schema = OriginalAtmWorkflowSchemaId,
        atm_inventory = AtmInventoryId,

        creation_time = CreationTime,
        creator = Creator
    } = AtmLambda,

    #{
        <<"scope">> => <<"private">>,
        <<"name">> => Name,
        <<"summary">> => Summary,

        <<"revisionRegistry">> => jsonable_record:to_json(RevisionRegistry, atm_workflow_schema_revision_registry),

        <<"originalAtmWorkflowSchema">> => case OriginalAtmWorkflowSchemaId of
            undefined ->
                null;
            _ ->
                gri:serialize(#gri{
                    type = od_atm_workflow_schema,
                    id = OriginalAtmWorkflowSchemaId,
                    aspect = instance,
                    scope = auto
                })
        end,
        <<"atmInventory">> => gri:serialize(#gri{type = od_atm_inventory, id = AtmInventoryId, aspect = instance, scope = auto}),
        <<"atmLambdaList">> => gri:serialize(#gri{type = od_atm_workflow_schema, id = Id, aspect = atm_lambdas}),

        <<"info">> => maps:merge(translate_creator(Creator), #{
            <<"creationTime">> => CreationTime
        })
    };

translate_atm_workflow_schema(#gri{aspect = atm_lambdas}, AtmLambdas) ->
    #{
        <<"list">> => ids_to_serialized_gris(#gri{type = od_atm_lambda, aspect = instance, scope = auto}, AtmLambdas)
    }.


%% @private
-spec translate_zone(gri:gri(), Data :: term()) ->
    gs_protocol:data() | fun((aai:auth()) -> gs_protocol:data()).
translate_zone(#gri{aspect = {gui_message, _MessageId}}, GuiMessage) ->
    #gui_message{enabled = Enabled, body = Body} = GuiMessage,
    #{
        <<"enabled">> => Enabled,
        <<"body">> => Body
    }.


%% @private
-spec ids_to_serialized_gris(gri:gri(), [binary()]) -> [binary()].
ids_to_serialized_gris(GriTemplate, Ids) ->
    lists:map(fun(Id) ->
        gri:serialize(GriTemplate#gri{id = Id})
    end, Ids).


%% @private
-spec format_intermediaries(entity_graph:intermediaries()) -> #{}.
format_intermediaries(Intermediaries) ->
    {GRIs, DirectMembership} = lists:foldl(fun
        ({_, ?SELF_INTERMEDIARY}, {AccGRIs, _AccDirectMembership}) ->
            {AccGRIs, true};
        ({Type, Id}, {AccGRIs, AccDirectMembership}) ->
            GRI = gri:serialize(#gri{
                type = Type, id = Id, aspect = instance, scope = auto
            }),
            {AccGRIs ++ [GRI], AccDirectMembership}
    end, {[], false}, Intermediaries),
    #{
        <<"intermediaries">> => GRIs,
        <<"directMembership">> => DirectMembership
    }.


%% @private
-spec translate_creator(undefined | aai:subject()) ->
    #{binary() => null | binary()}.
translate_creator(undefined) -> #{
    <<"creatorType">> => null,
    <<"creatorId">> => null
};
translate_creator(?SUB(Type, Id)) -> #{
    <<"creatorType">> => atom_to_binary(Type, utf8),
    <<"creatorId">> => utils:undefined_to_null(Id)
}.


%% @private
-spec serialize_token(tokens:token()) -> tokens:serialized().
serialize_token(Token) ->
    {ok, Serialized} = tokens:serialize(Token),
    Serialized.


%% @private
-spec lookup_name(module(), gri:entity_id()) -> binary() | null.
lookup_name(LogicModule, EntityId) ->
    lookup_name(LogicModule, get_name, EntityId).

%% @private
-spec lookup_name(module(), Function :: atom(), gri:entity_id()) -> binary() | null.
lookup_name(LogicModule, Function, EntityId) ->
    case LogicModule:Function(?ROOT, EntityId) of
        {ok, Name} -> Name;
        _ -> null
    end.