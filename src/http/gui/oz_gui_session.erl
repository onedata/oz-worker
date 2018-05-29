%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles GUI session manipulation.
%%% @end
%%%-------------------------------------------------------------------
-module(oz_gui_session).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

-export([log_in/2, log_out/1, get/1, get_user_id/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Logs given user in, creating new session and setting the session cookie in
%% http response.
%% @end
%%--------------------------------------------------------------------
-spec log_in(od_user:id(), cowboy_req:req()) ->    cowboy_req:req().
log_in(UserId, Req) ->
    {ok, SessionId} = session:create(UserId),
    new_gui:set_session_cookie(SessionId, session:session_ttl(), Req).


%%--------------------------------------------------------------------
%% @doc
%% Logs out the user that performed given request, deletes the session and
%% clears the session cookie.
%% @end
%%--------------------------------------------------------------------
-spec log_out(cowboy_req:req()) -> cowboy_req:req().
log_out(Req) ->
    case new_gui:get_session_cookie(Req) of
        undefined ->
            Req;
        SessionId ->
            session:delete(SessionId),
            new_gui:unset_session_cookie(Req)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the session by given session id or http request.
%% @end
%%--------------------------------------------------------------------
-spec get(session:id() | cowboy_req:req()) -> {ok, session:doc()} | {error, term()}.
get(SessionId) when is_binary(SessionId) ->
    session:get(SessionId);
get(Req) ->
    case new_gui:get_session_cookie(Req) of
        undefined ->
            {error, not_found};
        SessionId ->
            ?MODULE:get(SessionId)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the user id of currents session owner, or error if there is no
%% valid session.
%% @end
%%--------------------------------------------------------------------
-spec get_user_id(session:id() | cowboy_req:req()) -> {ok, od_user:id()} |  {error, term()}.
get_user_id(ReqOrSessionId) ->
    case ?MODULE:get(ReqOrSessionId) of
        {error, not_found} ->
            {error, not_logged_in};
        {ok, #document{value = #session{user_id = UserId}}} ->
            {ok, UserId}
    end.
