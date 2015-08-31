%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Basic tests that check connection to main parts of application
%%% @end
%%%-------------------------------------------------------------------
-module(rest_modules_test_SUITE).
-author("Jakub Kudzia").


-include("registered_names.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("annotations/include/annotations.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

-define(PRINT(ARG),
    begin
        io:format("DEBUG: ~p~n",[ARG])
    end).

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([provider_rest_module_test/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-performance({test_cases, []}).
all() -> [provider_rest_module_test].

provider_rest_module_test(Config) ->
    ibrowse:start(),
    ssl:start(),
    [Node] = ?config(gr_nodes, Config),
    IP = get_node_ip(Node),
%%     ?PRINT(IP),
    ?PRINT("https://" ++ get_node_ip(Node) ++ "/provider"),

    URLS = [<<"127.0.0.1">>],
    CSR = generate_csr(),
    RedirectionPoint = <<"https://127.0.0.1:443">>,
    ClientName = <<"provider">>,
    Body = mochijson2:encode([{urls,URLS}, {csr, CSR}, {redirectionPoint, RedirectionPoint},{clientName, ClientName}]),

    ?PRINT(Body),

    Ans1 = ibrowse:send_req("https://" ++ IP ++ ":8443/provider", [], post, Body),
    Ans2 = ibrowse:send_req("https://" ++ IP ++ ":8443/provider", [], get),

    ?PRINT(Ans1),
    ?PRINT(Ans2),

    ?assertMatch({ok, _, _, _}, Ans1),
    ?assertMatch({ok, _, _, _}, Ans2),

    ssl:stop(),
    ibrowse:stop().


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
%%     timer:sleep(60000), % TODO add nagios to GR and delete sleep
    NewConfig.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% returns ip (as a string) of given node
get_node_ip(Node) ->
    CMD = "docker inspect --format '{{ .NetworkSettings.IPAddress }}'" ++ " " ++ utils:get_host(Node),
    re:replace(os:cmd(CMD), "\\s+", "", [global,{return,list}]).


generate_csr() ->
    {MegaSec, Sec, MiliSec} = erlang:now(),
    Prefix = lists:foldl(fun(Int, Acc) ->
        Acc ++ integer_to_list(Int) end, "provider", [MegaSec, Sec, MiliSec]),
    KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
    CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
    os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
    os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
    {ok, CSR} = file:read_file(CSRFile),
    CSR.