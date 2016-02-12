%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements session_logic_behaviour and
%%% is capable of persisting GUI sessions in datastore.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_session_plugin).
-author("Lukasz Opiola").
-behaviour(gui_session_plugin_behaviour).


-include_lib("gui/include/gui.hrl").
-include_lib("ctool/include/logging.hrl").


%% session_logic_behaviour API
-export([init/0, cleanup/0]).
-export([create_session/1, update_session/2, lookup_session/1]).
-export([delete_session/1]).
-export([get_cookie_ttl/0]).

% ETS name for cookies
-define(SESSION_ETS, cookies_ember).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback init/1.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    % Ets table needed for session storing.
    ets:new(?SESSION_ETS, [named_table, public, set, {read_concurrency, true}]),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback cleanup/1.
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok.
cleanup() ->
    ets:delete(?SESSION_ETS),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback create_session/1.
%% @end
%%--------------------------------------------------------------------
-spec create_session(CustomArgs) ->
    {ok, SessionId} | {error, term()} when
    CustomArgs :: [term()], SessionId :: binary().
create_session(_CustomArgs) ->
    {error, not_implemented}.



%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback update_session/2.
%% @end
%%--------------------------------------------------------------------
-spec update_session(SessionId, Memory) -> ok | {error, term()}
    when SessionId :: binary(),
    Memory :: [{Key :: binary(), Value :: binary}].
update_session(_SessionId, _Memory) ->
    {error, not_implemented}.



%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback lookup_session/1.
%% @end
%%--------------------------------------------------------------------
-spec lookup_session(SessionId :: binary()) -> {ok, Memory} | undefined
    when Memory :: [{Key :: binary(), Value :: binary}].
lookup_session(_SessionId) ->
    undefined.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback delete_session/1.
%% @end
%%--------------------------------------------------------------------
-spec delete_session(SessionId :: binary()) -> ok | {error, term()}.
delete_session(_SessionId) ->
    {error, not_implemented}.


%%--------------------------------------------------------------------
%% @doc
%% Should return cookies time to live in seconds.
%% @end
%%--------------------------------------------------------------------
-spec get_cookie_ttl() -> integer() | {error, term()}.
get_cookie_ttl() ->
    {ok, 1}.
