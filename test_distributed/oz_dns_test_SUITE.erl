%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Basic tests that check dns answers.
%%% @end
%%%-------------------------------------------------------------------
-module(oz_dns_test_SUITE).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("kernel/src/inet_dns.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([get_all_ips_test/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([get_all_ips_test]).

get_all_ips_test(Config) ->
    [Node1, Node2] = ?config(oz_worker_nodes, Config),
    DnsPort = get_dns_port(Node1),
    Node1Ip = get_dns_ip(Node1),
    Node2Ip = get_dns_ip(Node2),
    Domain = get_domain(Node1),
    Query = inet_dns:encode(
        #dns_rec{
            header = #dns_header{
                id = crypto:rand_uniform(1, 16#FFFF),
                opcode = 'query',
                rd = true
            },
            qdlist = [#dns_query{
                domain = Domain,
                type = a,
                class = in
            }],
            arlist = [{dns_rr_opt, ".", opt, 1280, 0, 0, 0, <<>>}]
        }),
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    gen_udp:send(Socket, Node1Ip, DnsPort, Query),

    {ok, {_, _, Packet}} = gen_udp:recv(Socket, 65535, 50000),
    {ok, #dns_rec{anlist = Answers}} = inet_dns:decode(Packet),

    IpsFromDNS = lists:map(fun(#dns_rr{data = IpTuple})-> inet_parse:ntoa(IpTuple) end, Answers),
    Expected = lists:sort([Node1Ip, Node2Ip]),
    ?assertMatch(Expected, lists:sort(IpsFromDNS)).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    NewConfig.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).

%%%===================================================================
%%% Helper functions
%%%===================================================================

get_dns_port(Node) ->
    {ok, DNSPort} = rpc:call(Node, application, get_env, [?CLUSTER_WORKER_NAME, dns_port]),
    DNSPort.

get_dns_ip(Node) ->
    {ok, ExternalIP} = rpc:call(Node, application, get_env, [?APP_Name, external_ip]),
    str_utils:to_list(ExternalIP).

get_domain(Node) ->
    string:join(lists:nthtail(1, string:tokens(atom_to_list(Node), ".")), ".").