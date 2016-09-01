%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles requests with URL in form alias.gr.domain and
%%% performs a HTTP redirect to proper provider.
%%% @end
%%%-------------------------------------------------------------------
-module(public_share_handler).

-include_lib("ctool/include/logging.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

-export([init/3, handle/2, terminate/3]).

%%--------------------------------------------------------------------
%% @doc Cowboy handler callback, no state is required
%% @end
%%--------------------------------------------------------------------
-spec init(any(), term(), any()) -> {ok, term(), []}.
init(_Type, Req, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% @doc Handles a request returning a HTTP Redirect (307 - Moved temporarily).
%% @end
%%--------------------------------------------------------------------
-spec handle(term(), term()) -> {ok, term(), term()}.
handle(Req, State) ->
    try
        % Get query string from URL
        % Get the alias from URL
        {ShareId, _} = cowboy_req:binding(share_id, Req),
        URL = space_logic:share_id_to_redirect_url(ShareId),
        {ok, Req2} = cowboy_req:reply(307,
            [
                {<<"location">>, URL},
                {<<"content-type">>, <<"text/html">>}
            ], <<"">>, Req),
        {ok, Req2, State}
    catch T:M ->
        ?debug_stacktrace("Error while redirecting to public share - ~p:~p",
            [T, M]),
        {ok, Req3} = cowboy_req:reply(404, [], <<"">>, Req),
        {ok, Req3, State}
    end.

%%--------------------------------------------------------------------
%% @doc Cowboy handler callback, no cleanup needed
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), term(), term()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
