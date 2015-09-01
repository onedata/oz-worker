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
%%     CSR = generate_csr(),
    RedirectionPoint = <<"https://127.0.0.1:443">>,
    ClientName = <<"provider">>,

    {MegaSec, Sec, MiliSec} = erlang:now(),
    Prefix = lists:foldl(fun(Int, Acc) ->
        Acc ++ integer_to_list(Int) end, "provider", [MegaSec, Sec, MiliSec]),
    KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
    CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
    CertFile = filename:join(?TEMP_DIR, Prefix ++ "_cert.pem"),
    os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
    os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
    {ok, CSR} = file:read_file(CSRFile),

    Body = jiffy:encode({[{urls,URLS}, {csr, CSR}, {redirectionPoint, RedirectionPoint},{clientName, ClientName}]}),

    ?PRINT(Body),
    Headers = [{"Content-Type","application/json"}],
%%     Options = [
%%         {response_format, list},
%%         {trace, true},
%%         {ssl_options, [{verify, verify_none}]},
%%         {content_type, "application/json"}
%%     ],

    Resp1 = ibrowse:send_req("https://" ++ IP ++ ":8443/provider", Headers, post, Body),
    {ok, _Status, _ResponseHeaders, ResponseBody} = Resp1,

    ?PRINT(ResponseBody),

    Cert = proplists:get_value(<<"certificate">>, jiffy:decode((ResponseBody))),

    ?PRINT(Cert),

    file:write_file(CertFile, Cert),

    Options = [{ssl_options, [{keyfile, KeyFile}, {certfile, CertFile }]}],

    Resp2 = ibrowse:send_req("https://" ++ IP ++ ":8443/provider", [], get,[], Options),

    ?PRINT(Resp1),
    ?PRINT(Resp2),

    ?assertMatch({ok, _, _, _}, Resp1),
    ?assertMatch({ok, _, _, _}, Resp2),

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


%% generate_csr() ->
%%     {MegaSec, Sec, MiliSec} = erlang:now(),
%%     Prefix = lists:foldl(fun(Int, Acc) ->
%%         Acc ++ integer_to_list(Int) end, "provider", [MegaSec, Sec, MiliSec]),
%%     KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
%%     CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
%%     os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
%%     os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
%%     {ok, CSR} = file:read_file(CSRFile),
%%     CSR.