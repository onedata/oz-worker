%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc This module tests provider logic
%%% @end
%%% Created : 15. May 2014 12:11
%%%-------------------------------------------------------------------
-module(provider_logic_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(TEST).

%% ===================================================================
%% Tests description
%% ===================================================================

provider_logic_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"test_connection function", fun test_connection_test/0}
        ]
    }.

%% ===================================================================
%% Setup/teardown functions
%% ===================================================================

setup() ->
    ok.

teardown(_) ->
    ok.

%% ===================================================================
%% Tests functions
%% ===================================================================

test_connection_test() ->
    meck:new(ibrowse),
    meck:expect(ibrowse,send_req,
        fun
            ("https://172.16.67.194:443/test",[],get) -> {ok, "200", nothing_important, "gui"};
            ("https://172.16.67.194:8443/rest/latest/test",[],get) -> {ok, "200", nothing_important, "rest"};
            ("https://172.16.67.194:123/wrong_url",[],get) -> {error,{conn_failed,{error,econnrefused}}}
        end),
    Arg= [
        {<<"gui">>,<<"https://172.16.67.194:443/test">>},
        {<<"rest">>,<<"https://172.16.67.194:8443/rest/latest/test">>},
        {<<"unknown_service">>,<<"https://172.16.67.194:123/wrong_url">>}
    ],

    Ans = provider_logic:test_connection(Arg),
    Expected = [
        {<<"https://172.16.67.194:443/test">>,<<"ok">>},
        {<<"https://172.16.67.194:8443/rest/latest/test">>,<<"ok">>},
        {<<"https://172.16.67.194:123/wrong_url">>,<<"error">>}
    ],

    ?assertEqual(Expected, Ans),
    meck:unload(ibrowse).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-endif.