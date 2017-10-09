%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles requests for public CA certificate endpoint.
%%% @end
%%%-------------------------------------------------------------------
-module(public_ca_handler).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-export([init/3, handle/2, terminate/3]).

%%--------------------------------------------------------------------
%% @doc
%% Cowboy handler callback, no state is required
%% @end
%%--------------------------------------------------------------------
-spec init(any(), term(), any()) -> {ok, term(), []}.
init(_Type, Req, State) ->
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handles a request returning CA certificate in PEM format.
%% @end
%%--------------------------------------------------------------------
-spec handle(term(), term()) -> {ok, term(), term()}.
handle(Req, State) ->
    try
        {ok, Req2} = cowboy_req:reply(
            200,
            [{<<"content-type">>, <<"application/x-pem-file">>}],
            ozpca:oz_ca_pem(),
            Req
        ),
        {ok, Req2, State}
    catch Type:Reason ->
        ?error_stacktrace("Cannot serve public CA cert - ~p:~p", [Type, Reason]),
        {ok, Req3} = cowboy_req:reply(500, [], <<"">>, Req),
        {ok, Req3, State}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Cowboy handler callback, no cleanup needed
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), term(), term()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
