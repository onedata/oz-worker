%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements gui_route_plugin_behaviour. It decides on:
%%%   - mapping of URLs to pages (routes)
%%%   - logic and requirements on different routes
%%%   - what pages are used for login, logout, displaying errors
%%%   - what modules handles server logic of WebSocket connection with
%%%         the client (data and callback backends)
%%% @end
%%%-------------------------------------------------------------------
-module(gui_route_plugin).
-author("Lukasz Opiola").
-behaviour(gui_route_plugin_behaviour).

-include("gui/common.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("gui/include/gui.hrl").
-include_lib("ctool/include/logging.hrl").

-export([route/1, data_backend/2, private_rpc_backend/0, public_rpc_backend/0]).
-export([session_details/0]).
-export([login_page_path/0, default_page_path/0]).
-export([error_404_html_file/0, error_500_html_file/0]).

%% Convenience macros for defining routes.

-define(LOGOUT, #gui_route{
    requires_session = ?SESSION_LOGGED_IN,
    html_file = undefined,
    page_backend = logout_backend
}).

-define(VALIDATE_LOGIN, #gui_route{
    requires_session = ?SESSION_ANY,  % Can be used to log in or connect account
    html_file = undefined,
    page_backend = validate_login_backend
}).

-define(INDEX, #gui_route{
    requires_session = ?SESSION_ANY,
    websocket = ?SESSION_ANY,
    html_file = <<"index.html">>,
    page_backend = undefined
}).

-define(DEV_LOGIN, #gui_route{
    requires_session = ?SESSION_ANY,
    html_file = undefined,
    page_backend = dev_login_backend
}).

-define(VALIDATE_DEV_LOGIN, #gui_route{
    requires_session = ?SESSION_ANY,
    html_file = undefined,
    page_backend = validate_dev_login_backend
}).


%% ====================================================================
%% API
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc
%% @see gui_route_plugin_behaviour:route/1
%% @end
%%--------------------------------------------------------------------
-spec route(Path :: binary()) -> #gui_route{}.
route(<<"/do_logout">>) -> ?LOGOUT;
route(<<"/validate_login">>) -> ?VALIDATE_LOGIN;
route(<<"/dev_login">>) ->
    case application:get_env(?APP_Name, dev_mode) of
        {ok, true} ->
            ?DEV_LOGIN;
        _ ->
            ?INDEX
    end;
route(<<"/validate_dev_login">>) ->
    case application:get_env(?APP_Name, dev_mode) of
        {ok, true} ->
            ?VALIDATE_DEV_LOGIN;
        _ ->
            ?INDEX
    end;
route(<<"/">>) -> ?INDEX;
route(<<"/index.html">>) -> ?INDEX;
route(_) -> ?INDEX.


%%--------------------------------------------------------------------
%% @doc
%% @see gui_route_plugin_behaviour:data_backend/2
%% @end
%%--------------------------------------------------------------------
-spec data_backend(HasSession :: boolean(), Identifier :: binary()) -> HandlerModule :: module().
data_backend(true, <<"space">>) -> space_data_backend;
data_backend(true, <<"authorizer">>) -> authorizer_data_backend;
data_backend(true, <<"provider">>) -> provider_data_backend;
data_backend(true, <<"clienttoken">>) -> client_token_data_backend.


%%--------------------------------------------------------------------
%% @doc
%% @see gui_route_plugin_behaviour:private_rpc_backend/0
%% @end
%%--------------------------------------------------------------------
private_rpc_backend() -> private_rpc_backend.


%%--------------------------------------------------------------------
%% @doc
%% @see gui_route_plugin_behaviour:public_rpc_backend/0
%% @end
%%--------------------------------------------------------------------
public_rpc_backend() -> public_rpc_backend.


%%--------------------------------------------------------------------
%% @doc
%% @see gui_route_plugin_behaviour:get_session_details/0
%% @end
%%--------------------------------------------------------------------
-spec session_details() -> {ok, proplists:proplist()} | {error, term()}.
session_details() ->
    {ok, #document{value = #onedata_user{name = Name}}} =
        onedata_user:get(g_session:get_user_id()),
    FirstLogin = g_session:get_value(firstLogin, false),
    Res = [
        {<<"userName">>, Name},
        {<<"firstLogin">>, FirstLogin}
    ],
    {ok, Res}.


%%--------------------------------------------------------------------
%% @doc
%% @see gui_route_plugin_behaviour:login_page_path/0
%% @end
%%--------------------------------------------------------------------
-spec login_page_path() -> Path :: binary().
login_page_path() ->
    <<"/login">>.


%%--------------------------------------------------------------------
%% @doc
%% @see gui_route_plugin_behaviour:default_page_path/0
%% @end
%%--------------------------------------------------------------------
-spec default_page_path() -> Path :: binary().
default_page_path() ->
    <<"/">>.


%%--------------------------------------------------------------------
%% @doc
%% @see gui_route_plugin_behaviour:error_404_html_file/0
%% @end
%%--------------------------------------------------------------------
-spec error_404_html_file() -> FileName :: binary().
error_404_html_file() ->
    <<"page404.html">>.


%%--------------------------------------------------------------------
%% @doc
%% @see gui_route_plugin_behaviour:error_500_html_file/0
%% @end
%%--------------------------------------------------------------------
-spec error_500_html_file() -> FileName :: binary().
error_500_html_file() ->
    <<"page500.html">>.
