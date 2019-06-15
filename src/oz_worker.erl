%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions related to the oz_worker app.
%%% @end
%%%-------------------------------------------------------------------
-module(oz_worker).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("entity_logic.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([get_env/1, get_env/2, set_env/2]).
-export([get_name/0]).
-export([get_domain/0, get_url/0, get_uri/1]).
-export([get_release_version/0, get_build_version/0]).
-export([get_config/0]).
-export([entity_logic_plugin/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Wrapper function to get oz_worker env variable.
%% @end
%%--------------------------------------------------------------------
-spec get_env(Key :: atom()) -> term() | no_return().
get_env(Key) ->
    case application:get_env(?APP_NAME, Key) of
        {ok, Value} ->
            Value;
        undefined ->
            ?alert("Could not find required env variable: ~p", [Key]),
            error({missing_env_variable, Key})
    end.


%%--------------------------------------------------------------------
%% @doc
%% Wrapper function to get oz_worker env variable or default.
%% @end
%%--------------------------------------------------------------------
-spec get_env(Key :: atom(), Default :: term()) -> term().
get_env(Key, Default) ->
    application:get_env(?APP_NAME, Key, Default).


%%--------------------------------------------------------------------
%% @doc
%% Wrapper function to set oz_worker env variable.
%% @end
%%--------------------------------------------------------------------
-spec set_env(Key :: atom(), Value :: term()) -> ok.
set_env(Key, Value) ->
    application:set_env(?APP_NAME, Key, Value).


%%--------------------------------------------------------------------
%% @doc
%% Returns Onezone name.
%% @end
%%--------------------------------------------------------------------
-spec get_name() -> undefined | binary().
get_name() ->
    case get_env(oz_name, undefined) of
        undefined -> undefined;
        Str when is_list(Str) -> list_to_binary(Str)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns Onezone domain.
%% @end
%%--------------------------------------------------------------------
-spec get_domain() -> binary().
get_domain() ->
    list_to_binary(get_env(http_domain)).


%%--------------------------------------------------------------------
%% @doc
%% Returns Onezone URL (with HTTPS scheme).
%% @end
%%--------------------------------------------------------------------
-spec get_url() -> binary().
get_url() ->
    <<"https://", (get_domain())/binary>>.


%%--------------------------------------------------------------------
%% @doc
%% Returns Onezone URI (with HTTPS scheme) and given path appended (the path
%% must start with a slash).
%% @end
%%--------------------------------------------------------------------
-spec get_uri(PathWithSlash :: binary() | string()) -> binary().
get_uri(PathWithSlash) when is_binary(PathWithSlash) ->
    <<(get_url())/binary, PathWithSlash/binary>>;
get_uri(PathWithSlash) when is_list(PathWithSlash) ->
    get_uri(list_to_binary(PathWithSlash)).


%%--------------------------------------------------------------------
%% @doc
%% Returns Onezone release version.
%% @end
%%--------------------------------------------------------------------
-spec get_release_version() -> binary().
get_release_version() ->
    {_AppId, _AppName, AppVersion} = lists:keyfind(
        ?APP_NAME, 1, application:loaded_applications()
    ),
    list_to_binary(AppVersion).


%%--------------------------------------------------------------------
%% @doc
%% Returns Onezone build version.
%% @end
%%--------------------------------------------------------------------
-spec get_build_version() -> undefined | binary().
get_build_version() ->
    case get_env(build_version, "unknown") of
        "" -> <<"unknown">>;
        Build -> list_to_binary(Build)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns an entity logic plugin module for type oz_worker.
%% Used for queries concerning the Onezone service, which
%% do not have a corresponding datastore model.
%% @end
%%--------------------------------------------------------------------
-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    zone_logic_plugin.


%%--------------------------------------------------------------------
%% @doc
%% Returns Onezone configuration details, as needed by the configuration
%% endpoint.
%% @end
%%--------------------------------------------------------------------
-spec get_config() -> {ok, #{atom() := term()}} | {error, Reason :: term()}.
get_config() ->
    entity_logic:handle(#el_req{
        operation = get,
        client = ?NOBODY,
        gri = #gri{type = oz_worker, id = undefined, aspect = configuration}
    }).
