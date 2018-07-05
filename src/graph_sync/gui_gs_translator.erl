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
%% {@link gs_translator_behaviour} callback handshake_attributes/1.
%% @end
%%--------------------------------------------------------------------
-spec handshake_attributes(gs_protocol:client()) ->
    gs_protocol:handshake_attributes().
handshake_attributes(_) ->
    Idps = case oz_worker:get_env(dev_mode) of
        {ok, true} ->
            % If dev mode is enabled, always return basic auth and just one
            % dummy provider which will redirect to /dev_login page.
            [<<"basicAuth">>, <<"plgrid">>];
        _ ->
            % Production mode, return providers from config
            ProvidersAtoms = auth_utils:get_all_idps(),
            [str_utils:to_binary(P) || P <- ProvidersAtoms]
    end,
    #{
        <<"zoneName">> => oz_worker:get_name(),
        <<"serviceVersion">> => oz_worker:get_version(),
        <<"serviceBuildVersion">> => oz_worker:get_build_version(),
        <<"idps">> => Idps
    }.


%%--------------------------------------------------------------------
%% @doc
%% {@link gs_translator_behaviour} callback translate_create/3.
%% @end
%%--------------------------------------------------------------------
-spec translate_create(gs_protocol:protocol_version(), gs_protocol:gri(),
    Data :: term()) -> Result | fun((gs_protocol:client()) -> Result) when
    Result :: gs_protocol:data() | gs_protocol:error().
translate_create(1, #gri{aspect = invite_group_token}, Macaroon) ->
    translate_create(1, #gri{aspect = invite_user_token}, Macaroon);
translate_create(1, #gri{aspect = invite_provider_token}, Macaroon) ->
    translate_create(1, #gri{aspect = invite_user_token}, Macaroon);
translate_create(1, #gri{aspect = invite_user_token}, Macaroon) ->
    {ok, Token} = onedata_macaroons:serialize(Macaroon),
    Token;

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
%% {@link gs_translator_behaviour} callback translate_get/3.
%% @end
%%--------------------------------------------------------------------
-spec translate_get(gs_protocol:protocol_version(), gs_protocol:gri(),
    Data :: term()) -> Result | fun((gs_protocol:client()) -> Result) when
    Result :: gs_protocol:data() | {gs_protocol:gri(), gs_protocol:data()} |
    gs_protocol:error().
translate_get(1, GRI = #gri{type = od_user}, Data) ->
    translate_user(GRI, Data);
translate_get(1, GRI = #gri{type = od_group}, Data) ->
    translate_group(GRI, Data);
translate_get(1, GRI = #gri{type = od_space}, Data) ->
    translate_space(GRI, Data);
translate_get(1, GRI = #gri{type = od_provider}, Data) ->
    translate_provider(GRI, Data);

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
        <<"providerList">> => gs_protocol:gri_to_string(GRI#gri{aspect = eff_providers, scope = private})
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

translate_user(#gri{aspect = {linked_account, _}}, #linked_account{idp = IdP, email_list = Emails}) ->
    #{
        <<"idp">> => IdP,
        <<"emailList">> => Emails
    };

translate_user(#gri{aspect = eff_groups}, Groups) ->
    #{
        <<"list">> => lists:map(
            fun(GroupId) ->
                gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = instance, scope = protected})
            end, Groups)
    };

translate_user(#gri{aspect = eff_spaces}, Spaces) ->
    #{
        <<"list">> => lists:map(
            fun(SpaceId) ->
                gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = instance, scope = protected})
            end, Spaces)
    };

translate_user(#gri{aspect = eff_providers}, Providers) ->
    #{
        <<"list">> => lists:map(
            fun(ProviderId) ->
                gs_protocol:gri_to_string(#gri{type = od_provider, id = ProviderId, aspect = instance, scope = protected})
            end, Providers)
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Translates GET result for group related aspects.
%% @end
%%--------------------------------------------------------------------
-spec translate_group(gs_protocol:gri(), Data :: term()) ->
    gs_protocol:data() | {gs_protocol:gri(), gs_protocol:data()}.
translate_group(GRI = #gri{aspect = instance, scope = private}, #od_group{name = Name, type = Type}) ->
    ProtectedGRI = GRI#gri{scope = protected},
    {ProtectedGRI, translate_group(ProtectedGRI, #{<<"name">> => Name, <<"type">> => Type})};

translate_group(#gri{id = GroupId, aspect = instance, scope = protected}, Group) ->
    Group#{
        <<"parentList">> => gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = parents}),
        <<"childList">> => gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = children}),
        <<"userList">> => gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = users}),
        <<"spaceList">> => gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = spaces})
    };

translate_group(#gri{aspect = instance, scope = shared}, Group) ->
    Group;

translate_group(#gri{aspect = parents}, Parents) ->
    #{
        <<"list">> => lists:map(
            fun(ParentId) ->
                gs_protocol:gri_to_string(#gri{type = od_group, id = ParentId, aspect = instance, scope = protected})
            end, Parents)
    };

translate_group(#gri{aspect = children}, Children) ->
    #{
        <<"list">> => lists:map(
            fun(ChildId) ->
                gs_protocol:gri_to_string(#gri{type = od_group, id = ChildId, aspect = instance, scope = shared})
            end, Children)
    };

translate_group(#gri{aspect = users}, Users) ->
    #{
        <<"list">> => lists:map(
            fun(UserId) ->
                gs_protocol:gri_to_string(#gri{type = od_user, id = UserId, aspect = instance, scope = shared})
            end, Users)
    };

translate_group(#gri{aspect = {user_privileges, _UserId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_group(#gri{aspect = {child_privileges, _ChildId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_group(#gri{aspect = spaces}, Spaces) ->
    #{
        <<"list">> => lists:map(
            fun(SpaceId) ->
                gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = instance, scope = private})
            end, Spaces)
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Translates GET result for space related aspects.
%% @end
%%--------------------------------------------------------------------
-spec translate_space(gs_protocol:gri(), Data :: term()) ->
    gs_protocol:data() | {gs_protocol:gri(), gs_protocol:data()}.
translate_space(GRI = #gri{aspect = instance, scope = private}, #od_space{name = Name, providers = Providers}) ->
    ProtectedGRI = GRI#gri{scope = protected},
    {ProtectedGRI, translate_space(ProtectedGRI, #{<<"name">> => Name, <<"providers">> => Providers})};

translate_space(#gri{id = SpaceId, aspect = instance, scope = protected}, SpaceData) ->
    #{
        <<"name">> := Name,
        <<"providers">> := Providers
    } = SpaceData,
    #{
        <<"name">> => Name,
        <<"userList">> => gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = users}),
        <<"groupList">> => gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = groups}),
        <<"supportSizes">> => Providers,
        <<"providerList">> => gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = providers})
    };

translate_space(#gri{aspect = users}, Users) ->
    #{
        <<"list">> => lists:map(
            fun(UserId) ->
                gs_protocol:gri_to_string(#gri{type = od_user, id = UserId, aspect = instance, scope = shared})
            end, Users)
    };

translate_space(#gri{aspect = groups}, Groups) ->
    #{
        <<"list">> => lists:map(
            fun(GroupId) ->
                gs_protocol:gri_to_string(#gri{type = od_group, id = GroupId, aspect = instance, scope = shared})
            end, Groups)
    };

translate_space(#gri{aspect = {user_privileges, _UserId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_space(#gri{aspect = {group_privileges, _GroupId}}, Privileges) ->
    #{
        <<"privileges">> => Privileges
    };

translate_space(#gri{aspect = providers, scope = private}, Providers) ->
    #{
        <<"list">> => lists:map(
            fun(ProviderId) ->
                gs_protocol:gri_to_string(#gri{type = od_provider, id = ProviderId, aspect = instance, scope = protected})
            end, Providers)
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
        Provider#{
            <<"spaceList">> => gs_protocol:gri_to_string(GRI#gri{aspect = {user_spaces, UserId}, scope = private})
        }
    end;

translate_provider(#gri{aspect = {user_spaces, _UserId}}, Spaces) ->
    #{
        <<"list">> => lists:map(
            fun(SpaceId) ->
                gs_protocol:gri_to_string(#gri{type = od_space, id = SpaceId, aspect = instance, scope = protected})
            end, Spaces)
    }.
