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
-module(core_mechanism_test_SUITE).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("kernel/src/inet_dns.hrl").
-include_lib("cluster_worker/include/modules/datastore/datastore_common_internal.hrl").
-include_lib("cluster_worker/include/modules/datastore/datastore_models_def.hrl").

-define(TIMEOUT, timer:minutes(5)).
-define(call_store(N, F, A), ?call(N, datastore, F, A)).
-define(call(N, M, F, A), ?call(N, M, F, A, ?TIMEOUT)).
-define(call(N, M, F, A, T), rpc:call(N, M, F, A, T)).

%% API
-export([all/0]).
-export([dns_get_all_ips_test/1, test_models/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([dns_get_all_ips_test, test_models]).

test_models(Config) ->
    [Worker | _] = ?config(oz_worker_nodes, Config),
    Models = ?call(Worker, datastore_config, models, []),

    lists:foreach(fun(ModelName) ->
%%        ct:print("Module ~p", [ModelName]),

        #model_config{store_level = SL} = MC = ?call(Worker, ModelName, model_init, []),
        Cache = case SL of
            ?GLOBALLY_CACHED_LEVEL -> true;
            ?LOCALLY_CACHED_LEVEL -> true;
            _ -> false
        end,

        Key = list_to_binary("key_tm_" ++ atom_to_list(ModelName)),
        Doc =  #document{
            key = Key,
            value = MC#model_config.defaults
        },
        ?assertMatch({ok, _}, ?call_store(Worker, save, [SL, Doc])),
        ?assertMatch({ok, true}, ?call_store(Worker, exists, [SL, ModelName, Key])),

%%        ct:print("Module ok ~p", [ModelName]),

        case Cache of
            true ->
                PModule = ?call_store(Worker, driver_to_module, [persistence_driver_module]),
                ?assertMatch({ok, true}, ?call(Worker, PModule, exists, [MC, Key]), 10);
%%                ct:print("Module caching ok ~p", [ModelName]);
            _ ->
                ok
        end
    end, Models).

dns_get_all_ips_test(Config) ->
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