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

%% API
-export([get_env/1, get_env/2]).
-export([get_name/0]).
-export([get_domain/0, get_url/0, get_uri/1]).
-export([get_version/0, get_build_version/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Wrapper function to get oz_worker env variable.
%% @end
%%--------------------------------------------------------------------
-spec get_env(Key :: atom()) -> undefined | {ok, term()}.
get_env(Key) ->
    application:get_env(?APP_NAME, Key).


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
%% Returns Onezone name.
%% @end
%%--------------------------------------------------------------------
-spec get_name() -> binary().
get_name() ->
    {ok, Name} = get_env(oz_name),
    list_to_binary(Name).


%%--------------------------------------------------------------------
%% @doc
%% Returns Onezone domain.
%% @end
%%--------------------------------------------------------------------
-spec get_domain() -> binary().
get_domain() ->
    {ok, Domain} = get_env(http_domain),
    list_to_binary(Domain).


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
-spec get_uri(PathWithSlash :: binary()) -> binary().
get_uri(PathWithSlash) when is_binary(PathWithSlash) ->
    <<(get_url())/binary, PathWithSlash/binary>>;
get_uri(PathWithSlash) when is_list(PathWithSlash) ->
    get_uri(list_to_binary(PathWithSlash)).


%%--------------------------------------------------------------------
%% @doc
%% Returns Onezone version.
%% @end
%%--------------------------------------------------------------------
-spec get_version() -> binary().
get_version() ->
    {_AppId, _AppName, AppVersion} = lists:keyfind(
        ?APP_NAME, 1, application:loaded_applications()
    ),
    list_to_binary(AppVersion).


%%--------------------------------------------------------------------
%% @doc
%% Returns Onezone build_version.
%% @end
%%--------------------------------------------------------------------
-spec get_build_version() -> binary().
get_build_version() ->
    get_env(build_version, <<"unknown">>).


