%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module tests provider logic
%%% @end
%%%-------------------------------------------------------------------
-module(provider_logic_tests).
-author("Tomasz Lichon").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests description
%%%===================================================================


provider_logic_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"test_connection function", fun test_connection_test/0}
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    ok.

teardown(_) ->
    ok.

%%%===================================================================
%%% Tests functions
%%%===================================================================

test_connection_test() ->
    meck:new(http_client),
    meck:expect(http_client, get,
        fun
            (<<"https://172.16.67.194:443/test">>, [], <<>>, [insecure]) ->
                {ok, 200, nothing_important, <<"gui">>};
            (<<"https://172.16.67.194:8443/rest/latest/test">>, [], <<>>,
                [insecure]) ->
                {ok, 200, nothing_important, <<"rest">>};
            (<<"https://172.16.67.194:123/wrong_url">>, [], <<>>, [insecure]) ->
                {error, {conn_failed, {error, econnrefused}}}
        end),
    Arg = [
        {<<"gui">>, <<"https://172.16.67.194:443/test">>},
        {<<"rest">>, <<"https://172.16.67.194:8443/rest/latest/test">>},
        {<<"unknown_service">>, <<"https://172.16.67.194:123/wrong_url">>}
    ],

    Ans = provider_logic:test_connection(Arg),
    Expected = [
        {<<"https://172.16.67.194:443/test">>, ok},
        {<<"https://172.16.67.194:8443/rest/latest/test">>, ok},
        {<<"https://172.16.67.194:123/wrong_url">>, error}
    ],

    ?assertEqual(Expected, Ans),
    meck:unload(http_client).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-endif.
