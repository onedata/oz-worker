%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2014 ACK CYFRONET AGH
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

% Loading and managing auth config
-export([load_auth_config/0, get_auth_config/1, get_auth_providers/0]).
-export([get_provider_module/1, get_provider_app_id/1, get_provider_app_secret/1, get_provider_name/1]).
-export([get_provider_button_icon/1, get_provider_button_color/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Loads auth config from predefined file.
%% @end
%%--------------------------------------------------------------------
-spec load_auth_config() -> ok | no_return().
load_auth_config() ->
    {ok, AuthConfigFile} = application:get_env(?APP_NAME, auth_config_file),
    Config = case file:consult(AuthConfigFile) of
                 {ok, []} ->
                     [];
                 {ok, [Cfg]} when is_list(Cfg) ->
                     Cfg;
                 Other ->
                     ?error("Cannot parse auth config: ~p.", [Other]),
                     throw(cannot_parse_auth_config)
             end,
    application:set_env(?APP_NAME, auth_config, Config).

%%--------------------------------------------------------------------
%% @doc Returns list of configured OAuth providers.
%% @end
%%--------------------------------------------------------------------
-spec get_auth_providers() -> [term()].
get_auth_providers() ->
    {ok, Config} = application:get_env(?APP_NAME, auth_config),
    lists:map(
        fun({Provider, _}) ->
            Provider
        end, Config).

%%--------------------------------------------------------------------
%% @doc Returns configuration for given provider, from config.
%% @end
%%--------------------------------------------------------------------
-spec get_auth_config(Provider :: atom()) -> [term()].
get_auth_config(Provider) ->
    {ok, Config} = application:get_env(?APP_NAME, auth_config),
    proplists:get_value(Provider, Config).

%%--------------------------------------------------------------------
%% @doc Returns handler module for given provider, from config.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_module(Provider :: atom()) -> atom() | undefined.
get_provider_module(Provider) ->
    proplists:get_value(auth_module, get_auth_config(Provider)).

%%--------------------------------------------------------------------
%% @doc Returns application ID for given provider, from config.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_app_id(Provider :: atom()) -> binary() | undefined.
get_provider_app_id(Provider) ->
    proplists:get_value(app_id, get_auth_config(Provider)).

%%--------------------------------------------------------------------
%% @doc Returns application secret for given provider, from config.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_app_secret(Provider :: atom()) -> binary() | undefined.
get_provider_app_secret(Provider) ->
    proplists:get_value(app_secret, get_auth_config(Provider)).

%%--------------------------------------------------------------------
%% @doc Returns provider name for given provider, from config.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_name(Provider :: atom()) -> binary() | undefined.
get_provider_name(Provider) ->
    proplists:get_value(name, get_auth_config(Provider)).

%%--------------------------------------------------------------------
%% @doc Returns button icon for given provider, from config.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_button_icon(Provider :: atom()) -> binary() | undefined.
get_provider_button_icon(Provider) ->
    proplists:get_value(button_icon, get_auth_config(Provider)).

%%--------------------------------------------------------------------
%% @doc Returns button color for given provider, from config.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_button_color(Provider :: atom()) -> binary() | undefined.
get_provider_button_color(Provider) ->
    proplists:get_value(button_color, get_auth_config(Provider)).
