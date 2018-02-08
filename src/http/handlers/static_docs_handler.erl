%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Cowboy handler that acts as a proxy for documentation static files -
%%% all requests are followed to a third party documentation server and
%%% responses are fed back to the client.
%%% @end
%%%-------------------------------------------------------------------
-module(static_docs_handler).
-author("Lukasz Opiola").

-behaviour(cowboy_handler).

-include("datastore/oz_datastore_models.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

-export([init/2]).


%%--------------------------------------------------------------------
%% @doc Cowboy handler callback.
%% Handles a request - serves a file downloaded from docs server
%% or returns a HTTP redirect.
%% @end
%%--------------------------------------------------------------------
-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(#{method := <<"GET">>, path := Path} = Req, State) ->
    NewReq = try
        % Get path to static docs files
        {ok, DocsRootSt} = application:get_env(?APP_NAME, gui_docs_static_root),
        DocsRoot = str_utils:to_binary(DocsRootSt),
        Len = byte_size(DocsRoot),
        case Path of
            % If the request URL is in form '/docs' rather than '/docs/',
            % redirect the user so that gitbook works correctly.
            DocsRoot ->
                cowboy_req:reply(301, #{
                    <<"location">> => <<DocsRoot/binary, "/">>,
                    <<"content-type">> => <<"text/html">>
                }, Req);
            % All paths start with that root path, strip it out
            <<DocsRoot:Len/binary, FilePath/binary>> ->
                % Download the file from docs server and serve it to the client
                % The request is followed as is (with all the headers) to the
                % server and its unmodified answer is followed back.
                {ok, DocsServerStr} =
                    application:get_env(?APP_NAME, gui_docs_server),
                DocsServer = str_utils:to_binary(DocsServerStr),
                FileURL = <<DocsServer/binary, FilePath/binary>>,
                {ok, Code, Headers, Result} =
                    http_client:get(FileURL, cowboy_req:headers(Req)),
                ConvertFun = fun(Val) ->
                    list_to_binary(string:to_lower(binary_to_list(Val)))
                end,
                RespHeaders = maps:from_list(
                    [{ConvertFun(K), V} || {K, V} <- maps:to_list(Headers)]
                ),
                cowboy_req:reply(Code, RespHeaders, Result, Req)
        end
    catch T:M ->
        ?info_stacktrace("Error while serving static docs file - ~p:~p",
            [T, M]),
        cowboy_req:reply(500, Req)
    end,
    {ok, NewReq, State};
init(Req, State) ->
    NewReq = cowboy_req:reply(405, #{<<"allow">> => <<"GET">>}, Req),
    {ok, NewReq, State}.
