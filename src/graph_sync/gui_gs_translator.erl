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
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/api_errors.hrl").
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
-spec handshake_attributes(gs_protocol:client()) ->
    gs_protocol:handshake_attributes().
handshake_attributes(_) ->
    BrandSubtitle = oz_worker:get_env(brand_subtitle, ""),
    LoginNotification = oz_worker:get_env(login_notification, ""),

    #{
        <<"zoneName">> => oz_worker:get_name(),
        <<"serviceVersion">> => oz_worker:get_version(),
        <<"serviceBuildVersion">> => oz_worker:get_build_version(),
        <<"brandSubtitle">> => str_utils:unicode_list_to_binary(BrandSubtitle),
        <<"loginNotification">> => str_utils:unicode_list_to_binary(LoginNotification)
    }.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback translate_value/3.
%% @end
%%--------------------------------------------------------------------
-spec translate_value(gs_protocol:protocol_version(), gs_protocol:gri(),
    Value :: term()) -> Result | fun((gs_protocol:client()) -> Result) when
    Result :: gs_protocol:data() | gs_protocol:error().
translate_value(ProtoVersion, #gri{aspect = invite_group_token}, Macaroon) ->
    translate_value(ProtoVersion, #gri{aspect = invite_user_token}, Macaroon);
translate_value(ProtoVersion, #gri{aspect = invite_provider_token}, Macaroon) ->
    translate_value(ProtoVersion, #gri{aspect = invite_user_token}, Macaroon);
translate_value(ProtoVersion, #gri{aspect = invite_space_token}, Macaroon) ->
    translate_value(ProtoVersion, #gri{aspect = invite_user_token}, Macaroon);
translate_value(_, #gri{aspect = invite_user_token}, Macaroon) ->
    {ok, Token} = onedata_macaroons:serialize(Macaroon),
    Token;
translate_value(_, #gri{type = od_harvester, aspect = query}, Response) ->
    Response;
translate_value(_, #gri{type = od_harvester, aspect = index}, IndexId) ->
    #{<<"indexId">> => IndexId};

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
-spec translate_resource(gs_protocol:protocol_version(), gs_protocol:gri(),
    ResourceData :: term()) -> Result | fun((gs_protocol:client()) -> Result) when
    Result :: gs_protocol:data() | gs_protocol:error().
translate_resource(_, GRI = #gri{type = od_user}, Data) ->
    translate_user(GRI, Data);
translate_resource(_, GRI = #gri{type = od_group}, Data) ->
    translate_group(GRI, Data);
translate_resource(_, GRI = #gri{type = od_space}, Data) ->
    translate_space(GRI, Data);
translate_resource(_, GRI = #gri{type = od_provider}, Data) ->
    translate_provider(GRI, Data);
translate_resource(_, GRI = #gri{type = od_harvester}, Data) ->
    translate_harvester(GRI, Data);

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Translates GET result for user related aspects.
%% @end
%%--------------------------------------------------------------------
-spec translate_user(gs_protocol:gri(), Data :: term()) ->
    gs_protocol:data() | {gs_protocol:gri(), gs_protocol:data()}.
translate_user(GRI = #gri{type = od_user, aspect = instance, scope = private}, User) ->
    #od_user{
        name = Name,
        alias = Alias,
        default_space = DefaultSpace,
        default_provider = DefaultProvider
    } = User,
    #{
        <<"name">> => Name,
        <<"alias">> => Alias,
        <<"defaultSpaceId">> => gs_protocol:undefined_to_null(DefaultSpace),
        <<"defaultProviderId">> => gs_protocol:undefined_to_null(DefaultProvider),
        <<"clientTokenList">> => gs_protocol:gri_to_string(GRI#gri{aspect = client_tokens, scope = private}),
        <<"linkedAccountList">> => gs_protocol:gri_to_string(GRI#gri{aspect = linked_accounts, scope = private}),
        <<"groupList">> => gs_protocol:gri_to_string(GRI#gri{aspect = eff_groups, scope = private}),
        <<"spaceList">> => gs_protocol:gri_to_string(GRI#gri{aspect = eff_spaces, scope = private}),
        <<"providerList">> => gs_protocol:gri_to_string(GRI#gri{aspect = eff_providers, scope = private}),
        <<"harvesterList">> => gs_protocol:gri_to_string(GRI#gri{aspect = eff_harvesters, scope = private}),
        <<"info">> => #{
            <<"creationTime">> => User#od_user.creation_time
        }
    };

translate_user(#gri{aspect = instance, scope = shared}, User) ->
    #{
        <<"name">> := Name,
        <<"alias">> := Alias
    } = User,
    #{
        <<"name">> => Name,
        <<"alias">> => gs_protocol:undefined_to_null(Alias)
    };

translate_user(GRI = #gri{aspect = client_tokens}, ClientTokens) ->
    #{
        <<"list">> => lists:map(
            fun(ClientToken) ->
                gs_protocol:gri_to_string(GRI#gri{aspect = {client_token, ClientToken}, scope = private})
            end, ClientTokens)
    };

translate_user(#gri{aspect = {client_token, ClientToken}}, ClientToken) ->
    #{
        <<"id">> => ClientToken,
        <<"token">> => ClientToken
    };

translate_user(GRI = #gri{aspect = linked_accounts}, LinkedAccounts) ->
    #{
        <<"list">> => lists:map(
            fun(#linked_account{subject_id = UserId}) ->
                gs_protocol:gri_to_string(GRI#gri{aspect = {linked_account, UserId}, scope = private})
            end, LinkedAccounts)
    };

translate_user(#gri{aspect = {linked_account, _}}, #linked_account{idp = IdP, emails = Emails}) ->
    #{
        <<"idp">> => IdP,
        <<"emails">> => Emails
    };

translate_user(#gri{aspect = eff_groups}, Groups) ->
    #{
        <<"list">> => lists:map(
            fun(GroupId) ->
                gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = instance, scope = auto})
            end, Groups)
    };

translate_user(#gri{aspect = eff_spaces}, Spaces) ->
    #{
        <<"list">> => lists:map(
            fun(SpaceId) ->
                gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = instance, scope = auto})
            end, Spaces)
    };

translate_user(#gri{aspect = eff_providers}, Providers) ->
    #{
        <<"list">> => lists:map(
            fun(ProviderId) ->
                gs_protocol:gri_to_string(#gri{type = od_provider, id = ProviderId, aspect = instance, scope = auto})
            end, Providers)
    };

translate_user(#gri{aspect = eff_harvesters}, Harvesters) ->
    #{
        <<"list">> => lists:map(
            fun(HarvesterId) ->
                gs_protocol:gri_to_string(#gri{type = od_harvester, id = HarvesterId, aspect = instance, scope = auto})
            end, Harvesters)
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Translates GET result for group related aspects.
%% @end
%%--------------------------------------------------------------------
-spec translate_group(gs_protocol:gri(), Data :: term()) ->
    gs_protocol:data() | {gs_protocol:gri(), gs_protocol:data()}.
translate_group(#gri{id = GroupId, aspect = instance, scope = private}, Group) ->
    fun(?USER(UserId)) ->
        #{
            <<"name">> => Group#od_group.name,
            <<"type">> => Group#od_group.type,
            <<"scope">> => <<"private">>,
            <<"canViewPrivileges">> => group_logic:has_eff_privilege(Group, UserId, ?GROUP_VIEW_PRIVILEGES),
            <<"directMembership">> => group_logic:has_direct_user(Group, UserId),
            <<"parentList">> => gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = parents}),
            <<"childList">> => gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = children}),
            <<"effChildList">> => gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = eff_children}),
            <<"userList">> => gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = users}),
            <<"effUserList">> => gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = eff_users}),
            <<"spaceList">> => gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = spaces}),
            <<"harvesterList">> => gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = eff_harvesters}),
            <<"info">> => maps:merge(translate_creator(Group#od_group.creator), #{
                <<"creationTime">> => Group#od_group.creation_time
            })
        }
    end;

translate_group(#gri{aspect = instance, scope = protected}, Group) ->
    #{
        <<"name">> := Name,
        <<"type">> := Type,
        <<"creator">> := Creator,
        <<"creationTime">> := CreationTime
    } = Group,
    #{
        <<"name">> => Name,
        <<"type">> => Type,
        <<"info">> => maps:merge(translate_creator(Creator), #{
            <<"creationTime">> => CreationTime
        }),
        <<"scope">> => <<"protected">>
    };

translate_group(#gri{aspect = instance, scope = shared}, Group) ->
    #{
        <<"name">> := Name,
        <<"type">> := Type,
        <<"creationTime">> := CreationTime
    } = Group,
    #{
        <<"name">> => Name,
        <<"type">> => Type,
        <<"info">> =>  #{
            <<"creationTime">> => CreationTime
        },
        <<"scope">> => <<"shared">>
    };

translate_group(#gri{aspect = parents}, Parents) ->
    #{
        <<"list">> => lists:map(
            fun(ParentId) ->
                gs_protocol:gri_to_string(#gri{type = od_group, id = ParentId, aspect = instance, scope = auto})
            end, Parents)
    };

translate_group(#gri{aspect = children}, Children) ->
    #{
        <<"list">> => lists:map(
            fun(ChildId) ->
                gs_protocol:gri_to_string(#gri{type = od_group, id = ChildId, aspect = instance, scope = auto})
            end, Children)
    };

translate_group(#gri{aspect = eff_children}, Children) ->
    #{
        <<"list">> => lists:map(
            fun(ChildId) ->
                gs_protocol:gri_to_string(#gri{type = od_group, id = ChildId, aspect = instance, scope = auto})
            end, Children)
    };

translate_group(#gri{aspect = users}, Users) ->
    #{
        <<"list">> => lists:map(
            fun(UserId) ->
                gs_protocol:gri_to_string(#gri{type = od_user, id = UserId, aspect = instance, scope = auto})
            end, Users)
    };

translate_group(#gri{aspect = eff_users}, Users) ->
    #{
        <<"list">> => lists:map(
            fun(UserId) ->
                gs_protocol:gri_to_string(#gri{type = od_user, id = UserId, aspect = instance, scope = auto})
            end, Users)
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
        <<"list">> => lists:map(
            fun(SpaceId) ->
                gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = instance, scope = auto})
            end, Spaces)
    };

translate_group(#gri{aspect = eff_harvesters}, Harvesters) ->
    #{
        <<"list">> => lists:map(
            fun(HarvesterId) ->
                gs_protocol:gri_to_string(#gri{type = od_harvester, id = HarvesterId, aspect = instance, scope = auto})
            end, Harvesters)
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Translates GET result for space related aspects.
%% @end
%%--------------------------------------------------------------------
-spec translate_space(gs_protocol:gri(), Data :: term()) ->
    gs_protocol:data() | {gs_protocol:gri(), gs_protocol:data()}.
translate_space(#gri{id = SpaceId, aspect = instance, scope = private}, Space) ->
    #od_space{name = Name, providers = Providers, shares = Shares} = Space,
    fun(?USER(UserId)) ->
        #{
            <<"name">> => Name,
            <<"scope">> => <<"private">>,
            <<"canViewPrivileges">> => space_logic:has_eff_privilege(Space, UserId, ?SPACE_VIEW_PRIVILEGES),
            <<"directMembership">> => space_logic:has_direct_user(Space, UserId),
            <<"userList">> => gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = users}),
            <<"effUserList">> => gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = eff_users}),
            <<"groupList">> => gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = groups}),
            <<"effGroupList">> => gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = eff_groups}),
            <<"supportSizes">> => Providers,
            <<"providerList">> => gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = providers}),
            <<"harvesterList">> => gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = harvesters}),
            <<"info">> => maps:merge(translate_creator(Space#od_space.creator), #{
                <<"creationTime">> => Space#od_space.creation_time,
                <<"sharedDirectories">> => length(Shares)
            })
        }
    end;

translate_space(#gri{id = SpaceId, aspect = instance, scope = protected}, SpaceData) ->
    #{
        <<"name">> := Name,
        <<"providers">> := Providers,
        <<"creationTime">> := CreationTime,
        <<"creator">> := Creator,
        <<"sharedDirectories">> := SharedDirectories
    } = SpaceData,
    #{
        <<"name">> => Name,
        <<"scope">> => <<"protected">>,
        <<"supportSizes">> => Providers,
        <<"providerList">> => gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = providers}),
        <<"info">> => maps:merge(translate_creator(Creator), #{
            <<"creationTime">> => CreationTime,
            <<"sharedDirectories">> => SharedDirectories
        })
    };

translate_space(#gri{aspect = users}, Users) ->
    #{
        <<"list">> => lists:map(
            fun(UserId) ->
                gs_protocol:gri_to_string(#gri{type = od_user, id = UserId, aspect = instance, scope = auto})
            end, Users)
    };

translate_space(#gri{aspect = eff_users}, Users) ->
    #{
        <<"list">> => lists:map(
            fun(UserId) ->
                gs_protocol:gri_to_string(#gri{type = od_user, id = UserId, aspect = instance, scope = auto})
            end, Users)
    };

translate_space(#gri{aspect = groups}, Groups) ->
    #{
        <<"list">> => lists:map(
            fun(GroupId) ->
                gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = instance, scope = auto})
            end, Groups)
    };

translate_space(#gri{aspect = eff_groups}, Groups) ->
    #{
        <<"list">> => lists:map(
            fun(GroupId) ->
                gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = instance, scope = auto})
            end, Groups)
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

translate_space(#gri{aspect = providers, scope = private}, Providers) ->
    #{
        <<"list">> => lists:map(
            fun(ProviderId) ->
                gs_protocol:gri_to_string(#gri{type = od_provider, id = ProviderId, aspect = instance, scope = auto})
            end, Providers)
    };

translate_space(#gri{aspect = harvesters}, Harvesters) ->
    #{
        <<"list">> => lists:map(
            fun(HarvesterId) ->
                gs_protocol:gri_to_string(#gri{type = od_harvester, id = HarvesterId, aspect = instance, scope = auto})
            end, Harvesters)
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Translates GET result for provider related aspects.
%% @end
%%--------------------------------------------------------------------
-spec translate_provider(gs_protocol:gri(), Data :: term()) ->
    gs_protocol:data() | {gs_protocol:gri(), gs_protocol:data()}.
translate_provider(GRI = #gri{aspect = instance, scope = protected}, Provider) ->
    fun(?USER(UserId)) ->
        #{
            <<"name">> := Name, <<"domain">> := Domain,
            <<"latitude">> := Latitude, <<"longitude">> := Longitude,
            <<"online">> := Online,
            <<"creationTime">> := CreationTime
        } = Provider,
        #{
            <<"name">> => Name,
            <<"domain">> => Domain,
            <<"latitude">> => Latitude,
            <<"longitude">> => Longitude,
            <<"online">> => Online,
            <<"spaceList">> => gs_protocol:gri_to_string(GRI#gri{aspect = {user_spaces, UserId}, scope = private}),
            <<"info">> => #{
                <<"creationTime">> => CreationTime
            }
        }
    end;

translate_provider(#gri{aspect = {eff_user_membership, _UserId}}, Intermediaries) ->
    format_intermediaries(Intermediaries);

translate_provider(#gri{aspect = {eff_group_membership, _UserId}}, Intermediaries) ->
    format_intermediaries(Intermediaries);

translate_provider(#gri{aspect = {user_spaces, _UserId}}, Spaces) ->
    #{
        <<"list">> => lists:map(
            fun(SpaceId) ->
                gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = instance, scope = auto})
            end, Spaces)
    }.


translate_harvester(#gri{id = HarvesterId, aspect = instance, scope = private}, Harvester) ->
    #od_harvester{
        name = Name, endpoint = Endpoint, 
        plugin = Plugin
    } = Harvester,
    fun(?USER(UserId)) ->
        #{
            <<"name">> => Name,
            <<"scope">> => <<"private">>,
            <<"endpoint">> => Endpoint,
            <<"plugin">> => atom_to_binary(Plugin, utf8),
            <<"indexList">> => gs_protocol:gri_to_string(#gri{type = od_harvester, id = HarvesterId, aspect = indices}),
            <<"canViewPrivileges">> => harvester_logic:has_eff_privilege(Harvester, UserId, ?HARVESTER_VIEW_PRIVILEGES),
            <<"directMembership">> => harvester_logic:has_direct_user(Harvester, UserId),
            <<"userList">> => gs_protocol:gri_to_string(#gri{type = od_harvester, id = HarvesterId, aspect = users}),
            <<"effUserList">> => gs_protocol:gri_to_string(#gri{type = od_harvester, id = HarvesterId, aspect = eff_users}),
            <<"groupList">> => gs_protocol:gri_to_string(#gri{type = od_harvester, id = HarvesterId, aspect = groups}),
            <<"effGroupList">> => gs_protocol:gri_to_string(#gri{type = od_harvester, id = HarvesterId, aspect = eff_groups}),
            <<"spaceList">> => gs_protocol:gri_to_string(#gri{type = od_harvester, id = HarvesterId, aspect = spaces}),
            <<"info">> => maps:merge(translate_creator(Harvester#od_harvester.creator), #{
                <<"creationTime">> => Harvester#od_harvester.creation_time
            })
        }
    end;

translate_harvester(#gri{aspect = instance, scope = protected}, HarvesterData) ->
    #{
        <<"name">> := Name,
        <<"public">> := Public,
        <<"plugin">> := Plugin,
        <<"encpoint">> := Endpoint,
        <<"indices">> := Indices,
        <<"creationTime">> := CreationTime,
        <<"creator">> := Creator
    } = HarvesterData,
    #{
        <<"name">> => Name,
        <<"public">> => Public,
        <<"plugin">> => Plugin,
        <<"encpoint">> => Endpoint,
        <<"indices">> => Indices,
        <<"scope">> => <<"protected">>,
        <<"info">> => maps:merge(translate_creator(Creator), #{
            <<"creationTime">> => CreationTime
        })
    };

translate_harvester(#gri{aspect = config}, Config) ->
    #{
        <<"guiPluginConfig">> => Config
    };

translate_harvester(#gri{aspect = users}, Users) ->
    #{
        <<"list">> => lists:map(
            fun(UserId) ->
                gs_protocol:gri_to_string(#gri{type = od_user, id = UserId, aspect = instance, scope = auto})
            end, Users)
    };

translate_harvester(#gri{aspect = eff_users}, Users) ->
    #{
        <<"list">> => lists:map(
            fun(UserId) ->
                gs_protocol:gri_to_string(#gri{type = od_user, id = UserId, aspect = instance, scope = auto})
            end, Users)
    };

translate_harvester(#gri{aspect = groups}, Groups) ->
    #{
        <<"list">> => lists:map(
            fun(GroupId) ->
                gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = instance, scope = auto})
            end, Groups)
    };

translate_harvester(#gri{aspect = eff_groups}, Groups) ->
    #{
        <<"list">> => lists:map(
            fun(GroupId) ->
                gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = instance, scope = auto})
            end, Groups)
    };

translate_harvester(#gri{aspect = spaces}, Spaces) ->
    #{
        <<"list">> => lists:map(
            fun(SpaceId) ->
                gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = instance, scope = auto})
            end, Spaces)
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

translate_harvester(#gri{aspect = all_plugins}, Plugins) ->
    #{
        <<"allPlugins">> => Plugins
    };

translate_harvester(#gri{aspect = {index, _}}, IndexData) ->
    #{
        <<"name">> := Name,
        <<"schema">> := Schema,
        <<"guiPluginName">> := GuiPluginName
    } = IndexData,
    #{
        <<"name">> => Name,
        <<"schema">> => Schema,
        <<"guiPluginName">> => GuiPluginName
    };

translate_harvester(#gri{aspect = indices, id = HarvesterId}, Indices) ->
    #{
        <<"list">> => lists:map(
            fun(IndexId) ->
                gs_protocol:gri_to_string(#gri{type = od_harvester, id = HarvesterId, aspect = {index, IndexId}, scope = private})
            end, Indices)
    }.


-spec format_intermediaries(entity_graph:intermediaries()) -> #{}.
format_intermediaries(Intermediaries) ->
    {GRIs, DirectMembership} = lists:foldl(fun
        ({_, ?SELF_INTERMEDIARY}, {AccGRIs, _AccDirectMembership}) ->
            {AccGRIs, true};
        ({Type, Id}, {AccGRIs, AccDirectMembership}) ->
            GRI = gs_protocol:gri_to_string(#gri{
                type = Type, id = Id, aspect = instance, scope = auto
                    }),
                    {AccGRIs ++ [GRI], AccDirectMembership}
            end, {[], false}, Intermediaries),
    #{
        <<"intermediaries">> => GRIs,
        <<"directMembership">> => DirectMembership
    }.


-spec translate_creator(undefined | entity_logic:client()) ->
    #{binary() => null | binary()}.
translate_creator(undefined) -> #{
    <<"creatorType">> => null,
    <<"creatorId">> => null
};
translate_creator(#client{type = Type, id = Id}) -> #{
    <<"creatorType">> => atom_to_binary(Type, utf8),
    <<"creatorId">> => gs_protocol:undefined_to_null(Id)
}.
