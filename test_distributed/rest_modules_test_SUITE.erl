%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Integration tests of rest_modules
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
-export([provider_test/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-performance({test_cases, []}).
all() -> [provider_rest_module_test].

%% Testing endpoint: /provider

provider_test(Config) ->
    ibrowse:start(),
    ssl:start(),
    [Node] = ?config(gr_nodes, Config),
    GR_IP = get_node_ip(Node),

    URLS1 = [<<"127.0.0.1">>],
    RedirectionPoint1 = <<"https://127.0.0.1:443">>,
    ClientName1 = <<"provider1">>,

    {KeyFile, CSRFile, CertFile} = generate_cert_files(),
    {ok, CSR} = file:read_file(CSRFile),
    Body1 = jiffy:encode({[
        {urls,URLS1},
        {csr, CSR},
        {redirectionPoint, RedirectionPoint1},
        {clientName, ClientName1}
    ]}),

    Headers = [{"Content-Type","application/json"}],
%%     Options = [
%%         {response_format, list},
%%         {trace, true},
%%         {ssl_options, [{verify, verify_none}]},
%%         {content_type, "application/json"}
%%     ],

%%     register new provider
    {ok, _Status1, _ResponseHeaders1, ResponseBody1} =
        ibrowse:send_req("https://" ++ GR_IP ++ ":8443/provider", Headers, post, Body1),

    {JSONOutput1} = jiffy:decode(ResponseBody1),
    Cert = proplists:get_value(<<"certificate">>, JSONOutput1),
    ProviderId = proplists:get_value(<<"providerId">>, JSONOutput1),

    file:write_file(CertFile, Cert),

    Options = [{ssl_options, [{keyfile, KeyFile}, {certfile, CertFile }]}],

%% get info about provider
    {ok, _Status2, _ResponseHeaders2, ResponseBody2} =
        ibrowse:send_req("https://" ++ GR_IP ++ ":8443/provider", [], get,[], Options),

    {JSONOutput2} = jiffy:decode(ResponseBody2),

    ?assertMatch(ClientName1, proplists:get_value(<<"clientName">>, JSONOutput2)),
    ?assertMatch(URLS1, proplists:get_value(<<"urls">>, JSONOutput2)),
    ?assertMatch(RedirectionPoint1, proplists:get_value(<<"redirectionPoint">>, JSONOutput2)),
    ?assertMatch(ProviderId, proplists:get_value(<<"providerId">>, JSONOutput2)),

    URLS2 = [<<"127.0.0.2">>],
    RedirectionPoint2 = <<"https://127.0.0.2:443">>,
    ClientName2 = <<"provider2">>,

    Body2 = jiffy:encode({[
        {urls,URLS2},
        {redirectionPoint, RedirectionPoint2},
        {clientName, ClientName2}
    ]}),

%% modify provider info
    ?assertMatch({ok, _, _, _},
                 ibrowse:send_req("https://" ++ GR_IP ++ ":8443/provider", Headers, patch, Body2, Options)),

%% get modified provider info
    {ok, _Status3, _ResponseHeaders3, ResponseBody3} =
        ibrowse:send_req("https://" ++ GR_IP ++ ":8443/provider", [], get,[], Options),

    {JSONOutput3} = jiffy:decode(ResponseBody3),

    ?assertMatch(ClientName2, proplists:get_value(<<"clientName">>, JSONOutput3)),
    ?assertMatch(URLS2, proplists:get_value(<<"urls">>, JSONOutput3)),
    ?assertMatch(RedirectionPoint2, proplists:get_value(<<"redirectionPoint">>, JSONOutput3)),


    {KeyFile2, _CSRFile2, CertFile2} = generate_cert_files(),
    file:write_file(CertFile, "wrong_cert"),
    Options2 = [{ssl_options, [{keyfile, KeyFile2}, {certfile, CertFile2 }]}],

%% check if response to request with wrong certificate has status 401
    ?assertMatch({ok, "401", _, _},
                 ibrowse:send_req("https://" ++ GR_IP ++ ":8443/provider", [], get,[], Options2)),

%% delete provider
    ?assertMatch({ok, "202", _, _},
                 ibrowse:send_req("https://" ++ GR_IP ++ ":8443/provider", [], delete,[], Options)),

%% check if provider was deleted
    ?assertMatch({ok, "401", _, _},
        ibrowse:send_req("https://" ++ GR_IP ++ ":8443/provider", [], get,[], Options)),

    ssl:stop(),
    ibrowse:stop().

%%%===================================================================

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


generate_cert_files() ->
    {MegaSec, Sec, MiliSec} = erlang:now(),
    Prefix = lists:foldl(fun(Int, Acc) ->
        Acc ++ integer_to_list(Int) end, "provider", [MegaSec, Sec, MiliSec]),
    KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
    CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
    CertFile = filename:join(?TEMP_DIR, Prefix ++ "_cert.pem"),
    os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
    os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
    {KeyFile, CSRFile, CertFile}.