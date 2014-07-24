%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module handles configuration of OAuth providers.
%% @end
%% ===================================================================
-module(auth_config).

-include("auth_common.hrl").

% Loading and managing auth config
-export([load_auth_config/0, get_auth_config/1, get_auth_providers/0]).
-export([get_provider_module/1, get_provider_app_id/1, get_provider_app_secret/1, get_provider_name/1]).
-export([get_provider_button_icon/1, get_provider_button_color/1]).


%% ====================================================================
%% API functions
%% ====================================================================


%% load_auth_config/0
%% ====================================================================
%% @doc Loads auth config from predefined file.
%% @end
%% ====================================================================
-spec load_auth_config() -> ok.
%% ====================================================================
load_auth_config() ->
    {ok, [Config]} = file:consult(?auth_config_file_path),
    application:set_env(veil_cluster_node, auth_config, Config).


%% get_auth_providers/0
%% ====================================================================
%% @doc Returns list of configured OAuth providers.
%% @end
%% ====================================================================
-spec get_auth_providers() -> ok.
%% ====================================================================
get_auth_providers() ->
    {ok, Config} = application:get_env(veil_cluster_node, auth_config),
    lists:map(
        fun({Provider, _}) ->
            Provider
        end, Config).


%% get_auth_config/1
%% ====================================================================
%% @doc Returns configuration for given provider, from config.
%% @end
%% ====================================================================
-spec get_auth_config(Provider :: atom()) -> ok.
%% ====================================================================
get_auth_config(Provider) ->
    {ok, Config} = application:get_env(veil_cluster_node, auth_config),
    proplists:get_value(Provider, Config).


%% get_provider_module/1
%% ====================================================================
%% @doc Returns handler module for given provider, from config.
%% @end
%% ====================================================================
-spec get_provider_module(Provider :: atom()) -> ok.
%% ====================================================================
get_provider_module(Provider) ->
    proplists:get_value(auth_module, get_auth_config(Provider)).


%% get_provider_app_id/1
%% ====================================================================
%% @doc Returns application ID for given provider, from config.
%% @end
%% ====================================================================
-spec get_provider_app_id(Provider :: atom()) -> ok.
%% ====================================================================
get_provider_app_id(Provider) ->
    proplists:get_value(app_id, get_auth_config(Provider)).


%% get_provider_app_secret/1
%% ====================================================================
%% @doc Returns application secret for given provider, from config.
%% @end
%% ====================================================================
-spec get_provider_app_secret(Provider :: atom()) -> ok.
%% ====================================================================
get_provider_app_secret(Provider) ->
    proplists:get_value(app_secret, get_auth_config(Provider)).


%% get_provider_name/1
%% ====================================================================
%% @doc Returns provider name for given provider, from config.
%% @end
%% ====================================================================
-spec get_provider_name(Provider :: atom()) -> ok.
%% ====================================================================
get_provider_name(Provider) ->
    proplists:get_value(name, get_auth_config(Provider)).


%% get_provider_button_icon/1
%% ====================================================================
%% @doc Returns button icon for given provider, from config.
%% @end
%% ====================================================================
-spec get_provider_button_icon(Provider :: atom()) -> ok.
%% ====================================================================
get_provider_button_icon(Provider) ->
    proplists:get_value(button_icon, get_auth_config(Provider)).


%% get_provider_button_color/1
%% ====================================================================
%% @doc Returns button color for given provider, from config.
%% @end
%% ====================================================================
-spec get_provider_button_color(Provider :: atom()) -> ok.
%% ====================================================================
get_provider_button_color(Provider) ->
    proplists:get_value(button_color, get_auth_config(Provider)).
