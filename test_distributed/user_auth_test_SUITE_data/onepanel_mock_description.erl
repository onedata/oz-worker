%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This is an OZ mock description used by appmock.
%%% @end
%%%-------------------------------------------------------------------
-module(onepanel_mock_description).
-author("Lukasz Opiola").

-behaviour(mock_app_description_behaviour).

-include_lib("appmock/include/appmock.hrl").
-include_lib("ctool/include/logging.hrl").

-export([rest_mocks/0, tcp_server_mocks/0]).

rest_mocks() ->
    ExpectedMacaroon = macaroon:create("a", "b", "c"),
    ExpectedBasicAuth = <<"Basic ", (base64:encode(<<"user:password">>))/binary>>,
    {ok, ExpectedToken} = token_utils:serialize62(ExpectedMacaroon),
    CheckAuth = fun(Req) ->
        case req:header(<<"macaroon">>, Req) of
            ExpectedToken ->
                true;
            _ ->
                case req:header(<<"authorization">>, Req) of
                    ExpectedBasicAuth ->
                        true;
                    _ ->
                        false
                end
        end
    end,
    [
        #rest_mock{port = 8443, path = <<"/api/v3/onezone/user">>,
            response = fun(Req, State) ->
                case CheckAuth(Req) of
                    true ->
                        ResponseBody = json_utils:encode([
                            {<<"userId">>, <<"test_id">>},
                            {<<"name">>, <<"test_name">>}
                        ]),
                        {#rest_response{code = 200, body = ResponseBody, content_type = <<"application/json">>}, State};
                    _ ->
                        {#rest_response{code = 403}, State}
                end
            end,
            initial_state = undefined},
        #rest_mock{port = 8443, path = <<"/api/v3/onezone/user/spaces">>,
            response = fun(Req, State) ->
                case CheckAuth(Req) of
                    true ->
                        ResponseBody = json_utils:encode([
                            {<<"default">>, <<"space0">>},
                            {<<"spaces">>, [<<"space0">>]}
                        ]),
                        {#rest_response{code = 200, body = ResponseBody, content_type = <<"application/json">>}, State};
                    _ ->
                        {#rest_response{code = 403}, State}
                end
            end,
            initial_state = undefined},
        #rest_mock{port = 8443, path = <<"/api/v3/onezone/user/groups">>,
            response = fun(Req, State) ->
                case CheckAuth(Req) of
                    true ->
                        ResponseBody = json_utils:encode([
                            {<<"groups">>, []}
                        ]),
                        {#rest_response{code = 200, body = ResponseBody, content_type = <<"application/json">>}, State};
                    _ ->
                        {#rest_response{code = 403}, State}
                end
            end,
            initial_state = undefined},
        #rest_mock{port = 8443, path = <<"/api/v3/onezone/user/effective_groups">>,
            response = fun(Req, State) ->
                case CheckAuth(Req) of
                    true ->
                        ResponseBody = json_utils:encode([
                            {<<"effective_groups">>, []}
                        ]),
                        {#rest_response{code = 200, body = ResponseBody, content_type = <<"application/json">>}, State};
                    _ ->
                        {#rest_response{code = 403}, State}
                end
            end,
            initial_state = undefined}
    ].

tcp_server_mocks() -> [].