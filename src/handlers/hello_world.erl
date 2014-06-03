%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Rest handler example with hello world api
%% @end
%% ===================================================================
-module(hello_world).
-author("Tomasz Lichon").

-export([init/3]).
-export([content_types_provided/2]).
-export([hello_to_html/2, hello_to_json/2, hello_to_text/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[
		{<<"text/html">>, hello_to_html},
		{<<"application/json">>, hello_to_json},
		{<<"text/plain">>, hello_to_text}
	], Req, State}.

hello_to_html(Req, State) ->
	Body = <<"<html>REST Hello World as HTML!</html>">>,
	{Body, Req, State}.

hello_to_json(Req, State) ->
	Body = <<"{\"rest\": \"Hello World!\"}">>,
	{Body, Req, State}.

hello_to_text(Req, State) ->
	{<<"REST Hello World as text!">>, Req, State}.