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
-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").


%% session_logic_behaviour API
-export([init/0, cleanup/0]).
-export([create_session/2, update_session/2, lookup_session/1]).
-export([delete_session/1]).
-export([get_session_details/1]).
-export([get_cookie_ttl/0]).


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
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback cleanup/1.
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok.
cleanup() ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback create_session/1.
%% @end
%%--------------------------------------------------------------------
-spec create_session(UserId :: term(), CustomArgs :: [term()]) ->
    {ok, SessionId :: binary()} | {error, term()}.
create_session(UserId, _CustomArgs) ->
    SessId = datastore_utils:gen_uuid(),
    Sess = #session{user_id = UserId},
    case session:create(#document{key = SessId, value = Sess}) of
        {ok, SessId} ->
            {ok, SessId};
        {error, Reason} ->
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback update_session/2.
%% @end
%%--------------------------------------------------------------------
-spec update_session(SessionId, Memory) -> ok | {error, term()}
    when SessionId :: binary(),
    Memory :: [{Key :: binary(), Value :: binary}].
update_session(SessionId, Memory) ->
    case session:update(SessionId, #{memory => Memory}) of
        {ok, _} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback lookup_session/1.
%% @end
%%--------------------------------------------------------------------
-spec lookup_session(SessionId :: binary() | undefined) -> {ok, Memory} | undefined
    when Memory :: [{Key :: binary(), Value :: binary}].
lookup_session(SessionId) ->
    case SessionId of
        undefined ->
            undefined;
        _ ->
            case session:get(SessionId) of
                {ok, #document{value = #session{memory = Memory}}} ->
                    {ok, Memory};
                _ ->
                    undefined
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link gui_session_plugin_behaviour} callback delete_session/1.
%% @end
%%--------------------------------------------------------------------
-spec delete_session(SessionId :: binary()) -> ok | {error, term()}.
delete_session(SessionId) ->
    case SessionId of
        undefined ->
            ok;
        _ ->
            case session:delete(SessionId) of
                ok -> ok;
                {error, _} = Error -> Error
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Should return cookies time to live in seconds.
%% @end
%%--------------------------------------------------------------------
-spec get_cookie_ttl() -> integer() | {error, term()}.
get_cookie_ttl() ->
    case application:get_env(?APP_Name, gui_cookie_ttl) of
        {ok, Val} when is_integer(Val) ->
            Val;
        _ ->
            {error, missing_env}
    end.
