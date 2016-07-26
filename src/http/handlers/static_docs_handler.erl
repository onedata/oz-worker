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
-module(static_docs_handler).

-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

-export([init/3, handle/2, terminate/3]).

%%--------------------------------------------------------------------
%% @doc Cowboy handler callback, no state is required
%% @end
%%--------------------------------------------------------------------
-spec init(any(), term(), any()) -> {ok, term(), []}.
init(_Type, Req, _Args) ->
    {ok, Req, []}.

%%--------------------------------------------------------------------
%% @doc Handles a request returning a HTTP Redirect (307 - Moved temporarily).
%% @end
%%--------------------------------------------------------------------
-spec handle(term(), term()) -> {ok, term(), term()}.
handle(Req, State) ->
    try
        % Get path to static docs files
        {ok, DocsRootSt} = application:get_env(?APP_Name, gui_docs_static_root),
        DocsRoot = str_utils:to_binary(DocsRootSt),
        L = byte_size(DocsRoot),
        NewReq = case cowboy_req:path(Req) of
            % If the request URL is in form '/docs' rather than '/docs/',
            % redirect the user so that gitbook works correctly.
            {DocsRoot, _} ->
                {ok, Req2} = cowboy_req:reply(301, [
                    {<<"location">>, <<DocsRoot/binary, "/">>},
                    {<<"content-type">>, <<"text/html">>}
                ], <<"">>, Req),
                Req2;
            % All paths start with that root path, strip it out
            {<<DocsRoot:L/binary, FilePath/binary>>, _} ->
                % Download the file from docs server and serve it to the client
                % The request is followed as is (with all the headers) to the
                % server and its unmodified answer is followed back.
                {ok, DocsServerStr} =
                    application:get_env(?APP_Name, gui_docs_server),
                DocsServer = str_utils:to_binary(DocsServerStr),
                FileURL = <<DocsServer/binary, FilePath/binary>>,
                {ReqHeaders, _} = cowboy_req:headers(Req),
                {ok, Code, RespHeaders, Result} =
                    http_client:get(FileURL, ReqHeaders),
                {ok, Req2} = cowboy_req:reply(Code, RespHeaders, Result, Req),
                Req2
        end,
        {ok, NewReq, State}
    catch T:M ->
        ?info_stacktrace("Error while serving static docs file - ~p:~p",
            [T, M]),
        {ok, Req3} = cowboy_req:reply(500, [], <<"">>, Req),
        {ok, Req3, State}
    end.

%%--------------------------------------------------------------------
%% @doc Cowboy handler callback, no cleanup needed
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), term(), term()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
