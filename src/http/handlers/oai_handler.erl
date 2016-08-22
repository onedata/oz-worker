%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(oai_handler).
-author("Jakub Kudzia").

-include_lib("ctool/include/logging.hrl").

%% API
-export([init/3, handle/2, terminate/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Cowboy handler callback, no state is required
%% @end
%%--------------------------------------------------------------------
-spec init(term(), term(), term()) -> {ok, term(), []}.
init(_Type, Req, _Opts) ->
    {ok, Req, []}.

%%--------------------------------------------------------------------
%% @doc
%% WRITEME
%% @end
%%--------------------------------------------------------------------
-spec handle(term(), term()) -> {ok, term(), term()}.
handle(Req, State) ->
    {FullHostname, _} = cowboy_req:header(<<"host">>, Req),
    {QS, _} = cowboy_req:qs(Req),
    io:format("FullHostname: ~p~n", [FullHostname]),
    io:format("QS: ~p~n", [QS]),
    io:format("Req: ~p~n", [Req]),
    io:format("State: ~p~n", [State]),
    {Method, Req} = cowboy_req:method(Req),
    io:format("Method: ~p~n", [Method]),
    io:format("Parsed QS: ~p~n", [cowboy_req:parse_qs(Req)]),


    case Method of
        <<"GET">> -> handle_get_request(Req, State);
        _ -> handle_post_request(Req, State)
    end.

%%    % Remove the leading 'www.' if present
%%    Hostname = case FullHostname of
%%        <<"www.", Rest/binary>> -> Rest;
%%        _ -> FullHostname
%%    end,
%%    {Path, _} = cowboy_req:path(Req),
%%    {ok, Req2} = cowboy_req:reply(301, [
%%        {<<"location">>, <<"https://", Hostname/binary, Path/binary, "?", QS/binary>>},
%%        {<<"content-type">>, <<"text/html">>}
%%    ], <<"">>, Req),
%%    {ok, Req2, State}.

%%--------------------------------------------------------------------
%% @doc
%% Cowboy handler callback, no cleanup needed
%%--------------------------------------------------------------------
-spec terminate(term(), term(), term()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.


handle_get_request(Req, State) ->
    error(not_implemented).

handle_post_request(Req, State) ->
    error(not_implemented).