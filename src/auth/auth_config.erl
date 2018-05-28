%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This module handles configuration of OAuth providers.
%%% @end
%%%-------------------------------------------------------------------
-module(auth_config).

-include("auth_common.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

% Token prefix assumed when there is none specified in config
-define(DEFAULT_TOKEN_PREFIX(__ProviderId),
    <<(atom_to_binary(__ProviderId, utf8))/binary, ":">>
).

% Loading and managing auth config
-export([load_auth_config/0, get_auth_config/1, get_auth_providers/0]).
-export([get_provider_module/1, get_provider_app_id/1, get_provider_app_secret/1]).
-export([get_providers_with_auth_delegation/0]).
-export([get_group_mapping_config/1, has_group_mapping_enabled/1, get_super_group/1,
    normalize_membership_spec/2]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Loads auth config from predefined file.
%% @end
%%--------------------------------------------------------------------
-spec load_auth_config() -> ok | no_return().
load_auth_config() ->
    {ok, AuthConfigFile} = oz_worker:get_env(auth_config_file),
    Config = case file:consult(AuthConfigFile) of
        {ok, []} ->
            [];
        {ok, [Cfg]} when is_list(Cfg) ->
            % Cache configured providers supporting authority delegation
            AUthDelegationProviders = lists:filtermap(
                fun({ProviderId, ProviderCfg}) ->
                    AuthDelCfg = proplists:get_value(
                        authority_delegation, ProviderCfg, []
                    ),
                    case proplists:get_value(enabled, AuthDelCfg, false) of
                        false ->
                            false;
                        true ->
                            Prefix = proplists:get_value(
                                token_prefix, AuthDelCfg,
                                ?DEFAULT_TOKEN_PREFIX(ProviderId)
                            ),
                            {true, {ProviderId, Prefix}}
                    end
                end, Cfg),
            application:set_env(
                ?APP_NAME, auth_delegation_providers, AUthDelegationProviders
            ),
            Cfg;
        Other ->
            ?error("Cannot parse auth config: ~p.", [Other]),
            throw(cannot_parse_auth_config)
    end,
    application:set_env(?APP_NAME, auth_config, Config).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of configured OAuth providers.
%% @end
%%--------------------------------------------------------------------
-spec get_auth_providers() -> [term()].
get_auth_providers() ->
    {ok, Config} = oz_worker:get_env(auth_config),
    lists:map(
        fun({ProviderId, _}) ->
            ProviderId
        end, Config).


%%--------------------------------------------------------------------
%% @doc
%% Returns configuration for given provider, from config.
%% @end
%%--------------------------------------------------------------------
-spec get_auth_config(ProviderId :: atom()) -> [term()].
get_auth_config(ProviderId) ->
    {ok, Config} = oz_worker:get_env(auth_config),
    proplists:get_value(ProviderId, Config).


%%--------------------------------------------------------------------
%% @doc
%% Returns handler module for given provider, from config.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_module(ProviderId :: atom()) -> atom() | undefined.
get_provider_module(ProviderId) ->
    proplists:get_value(auth_module, get_auth_config(ProviderId)).


%%--------------------------------------------------------------------
%% @doc
%% Returns application ID for given provider, from config.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_app_id(ProviderId :: atom()) -> binary() | undefined.
get_provider_app_id(ProviderId) ->
    proplists:get_value(app_id, get_auth_config(ProviderId)).


%%--------------------------------------------------------------------
%% @doc
%% Returns application secret for given provider, from config.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_app_secret(ProviderId :: atom()) -> binary() | undefined.
get_provider_app_secret(ProviderId) ->
    proplists:get_value(app_secret, get_auth_config(ProviderId)).


%%--------------------------------------------------------------------
%% @doc
%% Returns a list of providers that support auth delegation,
%% with their corresponding prefix.
%% @end
%%--------------------------------------------------------------------
-spec get_providers_with_auth_delegation() ->
    [{auth_utils:idp(), Prefix :: binary()}].
get_providers_with_auth_delegation() ->
    oz_worker:get_env(auth_delegation_providers, []).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether given OIDC provider has group mapping enabled.
%% @end
%%--------------------------------------------------------------------
-spec get_group_mapping_config(ProviderId :: atom()) -> proplists:proplist().
get_group_mapping_config(ProviderId) ->
    proplists:get_value(group_mapping, get_auth_config(ProviderId), []).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether given OIDC provider has group mapping enabled.
%% @end
%%--------------------------------------------------------------------
-spec has_group_mapping_enabled(ProviderId :: atom()) -> boolean().
has_group_mapping_enabled(ProviderId) ->
    proplists:get_value(enabled, get_group_mapping_config(ProviderId), false).


%%--------------------------------------------------------------------
%% @doc
%% Returns the id of super group per given provider id, if specified,
%% undefined otherwise. Super group has admin rights in all groups belonging to
%% given virtual organization.
%% @end
%%--------------------------------------------------------------------
-spec get_super_group(ProviderId :: atom()) ->
    undefined | idp_group_mapping:group_spec().
get_super_group(ProviderId) ->
    proplists:get_value(super_group, get_group_mapping_config(ProviderId), undefined).

%%--------------------------------------------------------------------
%% @doc
%% Normalizes group membership specs for given provider.
%% @end
%%--------------------------------------------------------------------
-spec normalize_membership_spec(auth_utils:idp(), GroupId :: binary()) ->
    idp_group_mapping:membership_spec().
normalize_membership_spec(Idp, GroupId) ->
    HandlerModule = ?MODULE:get_provider_module(Idp),
    HandlerModule:normalized_membership_spec(Idp, GroupId).