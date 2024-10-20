%%% @author Wojciech Geisler
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning DNS server shipped with OneZone
%%% and OneProvider subdomain delegation.
%%% @end
%%%-------------------------------------------------------------------
-module(dns_test_SUITE).
-author("Wojciech Geisler").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("kernel/src/inet_dns.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/global_definitions.hrl").

-type config() :: [{atom(), term()}].

%% API
-export([all/0, init_per_suite/1, end_per_suite/1, end_per_testcase/2]).
-export([
    dns_server_resolves_oz_domain_test/1,
    dns_state_stores_provider_data_test/1,
    dns_server_resolves_delegated_subdomain_test/1,
    dns_server_resolves_changed_subdomain_test/1,
    update_fails_on_duplicated_subdomain_test/1,
    dns_server_resolves_ns_records_test/1,
    dns_server_duplicates_ns_records_test/1,
    dns_server_resolves_static_records/1,
    static_subdomain_does_not_shadow_provider_subdomain_test/1,
    dns_server_does_not_resolve_removed_subdomain_test/1,
    dns_resolves_txt_record/1,
    txt_record_forbidden_without_subdomain_delegation/1,
    dns_does_not_resolve_removed_txt_record_test/1,
    removing_nonexistent_txt_does_nothing/1,
    dns_config_update_increases_soa_serial/1
]).


all() -> ?ALL([
    dns_server_resolves_oz_domain_test,
    dns_state_stores_provider_data_test,
    dns_server_resolves_delegated_subdomain_test,
    dns_server_resolves_changed_subdomain_test,
    update_fails_on_duplicated_subdomain_test,
    dns_server_resolves_ns_records_test,
    dns_server_duplicates_ns_records_test,
    dns_server_resolves_static_records,
    static_subdomain_does_not_shadow_provider_subdomain_test,
    dns_server_does_not_resolve_removed_subdomain_test,
    dns_resolves_txt_record,
    txt_record_forbidden_without_subdomain_delegation,
    dns_does_not_resolve_removed_txt_record_test,
    removing_nonexistent_txt_does_nothing,
    dns_config_update_increases_soa_serial
]).

-define(DNS_ASSERT_RETRY_COUNT, 7).
-define(DNS_ASSERT_RETRY_DELAY, timer:seconds(5)).

-define(DNS_STATE_KEY, <<"dns_state_singleton">>).
-define(DATASTORE_CTX, #{model => dns_state}).

%%%===================================================================
%%% Example data
%%%===================================================================
-define(PROVIDER_NAME1, <<"test_provider">>).
-define(PROVIDER_NAME2, <<"second_provider">>).
-define(PROVIDER_IPS1, lists:sort([{240, 1, 1, 0}, {240, 1, 1, 1}, {240, 1, 1, 2}])).
-define(PROVIDER_IPS2, lists:sort([{241, 1, 1, 0}, {241, 1, 1, 1}, {241, 1, 1, 2}])).
-define(STATIC_SUBDOMAIN_IPS1, lists:sort([{1, 2, 3, 4}, {5, 6, 7, 8}])).
-define(STATIC_SUBDOMAIN_IPS2, lists:sort([{122, 255, 255, 32}])).
-define(PROVIDER_SUBDOMAIN1, "provsub").
-define(PROVIDER_SUBDOMAIN2, "other-provsub").
-define(EXTERNAL_DOMAIN1, "domain.org").


%%%===================================================================
%%% Setup/Teardown functions
%%%===================================================================

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    Posthook = fun(NewConfig) ->
        Nodes = ?config(oz_worker_nodes, NewConfig),
        IPstrings = lists:map(fun binary_to_list/1,
            lists:map(fun test_utils:get_docker_ip/1,
                Nodes)),
        IPs = lists:map(fun(IPstring) ->
            {ok, IP} = inet:parse_ipv4strict_address(IPstring),
            IP
        end, IPstrings),

        ZoneDomain = binary_to_list(oz_test_utils:oz_domain(NewConfig)),
        [{oz_domain, ZoneDomain}, {oz_ips, lists:sort(IPs)} | NewConfig]
    end,
    [{env_up_posthook, Posthook}, {?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(_Config) ->
    ok.

end_per_testcase(static_subdomain_does_not_shadow_provider_subdomain_test, Config) ->
    lists:foreach(fun(Env) ->
        oz_test_utils:set_env(Config, Env, [])
    end, [dns_static_a_records, dns_static_ns_records, dns_static_mx_records,
        dns_static_txt_records, dns_static_cname_records]),

    oz_test_utils:delete_all_entities(Config),
    ok;

end_per_testcase(_, Config) ->
    % prevent "subdomain occupied" errors
    oz_test_utils:delete_all_entities(Config),
    ok.


%%%===================================================================
%%% API functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% OneZone dns, working on every node, should respond with IPs of all OneZone
%% nodes.
%% @end
%%--------------------------------------------------------------------
dns_server_resolves_oz_domain_test(Config) ->
    OZ_IPs = ?config(oz_ips, Config),
    OZ_Domain = ?config(oz_domain, Config),

    assert_dns_answer(OZ_IPs, OZ_Domain, a, OZ_IPs).


%%--------------------------------------------------------------------
%% @doc
%% When subdomain delegation is enabled for a provider, dns_state provides
%% information about its subdomain.
%% @end
%%--------------------------------------------------------------------
dns_state_stores_provider_data_test(Config) ->
    %% given
    ProviderName = ?PROVIDER_NAME1,
    SubdomainBin = <<?PROVIDER_SUBDOMAIN1>>,
    ProviderIPs = ?PROVIDER_IPS1,

    %% when
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(
        Config, ProviderName),
    oz_test_utils:enable_subdomain_delegation(
        Config, ProviderId, SubdomainBin, ProviderIPs),

    %% then
    ?assertEqual({ok, SubdomainBin, ProviderIPs}, oz_test_utils:call_oz(Config,
        dns_state, get_delegation_config, [ProviderId])),

    StIP = oz_test_utils:call_oz(Config,
        dns_state, get_subdomains_to_ips, []),
    ?assertEqual(ProviderIPs, lists:sort(maps:get(SubdomainBin, StIP))).


%%--------------------------------------------------------------------
%% @doc
%% DNS on all OZ nodes should resolve provider domain built from subdomain
%% and oz domain
%% @end
%%--------------------------------------------------------------------
dns_server_resolves_delegated_subdomain_test(Config) ->
    %% given
    Name = ?PROVIDER_NAME1,
    ProviderIPs = ?PROVIDER_IPS1,
    OZIPs = ?config(oz_ips, Config),

    Subdomain = ?PROVIDER_SUBDOMAIN1,
    SubdomainBin = <<?PROVIDER_SUBDOMAIN1>>,
    OZDomain = ?config(oz_domain, Config),
    FullDomain = Subdomain ++ "." ++ OZDomain,

    %% when
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, Name),
    oz_test_utils:enable_subdomain_delegation(
        Config, ProviderId, SubdomainBin, ProviderIPs),

    %% then
    {ok, ProviderDoc} = oz_test_utils:get_provider(Config, ProviderId),
    ?assertEqual(list_to_binary(FullDomain), ProviderDoc#od_provider.domain),
    assert_dns_answer(OZIPs, FullDomain, a, ProviderIPs).


%%--------------------------------------------------------------------
%% @doc
%% When subdomain delegation is set with a different subdomain, dns server
%% should stop resolving old subdomain and start resolving new.
%% @end
%%--------------------------------------------------------------------
dns_server_resolves_changed_subdomain_test(Config) ->
    %% given
    Name = ?PROVIDER_NAME1,
    ProviderIPs = ?PROVIDER_IPS1,
    OZIPs = ?config(oz_ips, Config),

    OZDomain = ?config(oz_domain, Config),
    Subdomain1 = ?PROVIDER_SUBDOMAIN1,
    SubdomainBin1 = <<?PROVIDER_SUBDOMAIN1>>,
    FullDomain1 = Subdomain1 ++ "." ++ OZDomain,

    Subdomain2 = ?PROVIDER_SUBDOMAIN2,
    SubdomainBin2 = <<?PROVIDER_SUBDOMAIN2>>,
    FullDomain2 = Subdomain2 ++ "." ++ OZDomain,

    %% when
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, Name),
    oz_test_utils:enable_subdomain_delegation(
        Config, ProviderId, SubdomainBin1, ProviderIPs),

    assert_dns_answer(OZIPs, FullDomain1, a, ProviderIPs),

    oz_test_utils:enable_subdomain_delegation(
        Config, ProviderId, SubdomainBin2, ProviderIPs),

    %% then
    assert_dns_answer(OZIPs, FullDomain1, a, []),
    assert_dns_answer(OZIPs, FullDomain2, a, ProviderIPs).


%%--------------------------------------------------------------------
%% @doc
%% DNS zone should have a number of NS records pointing to nsX subdomains.
%% Those subdomains should be resolved to OZ nodes ips.
%% Their number is limited by config.
%% @end
%%--------------------------------------------------------------------
dns_server_resolves_ns_records_test(Config) ->
    OZIPs = ?config(oz_ips, Config),
    OZDomain = ?config(oz_domain, Config),

    Maximum = 2,
    oz_test_utils:set_env(Config, dns_ns_max_entries, Maximum),
    oz_test_utils:set_env(Config, dns_ns_min_entries, 1), % the basic case

    % force dns update
    ?assertEqual(ok, oz_test_utils:call_oz(Config,
        node_manager_plugin, reconcile_dns_config, [])),

    % number of nodes based on env_desc
    [IP1, IP2, _IP3] = lists:sort(OZIPs),
    NSDomainsIPs = [{"ns1." ++ OZDomain, [IP1]}, {"ns2." ++ OZDomain, [IP2]}],
    {NSDomains, _} = lists:unzip(NSDomainsIPs),

    assert_dns_answer(OZIPs, OZDomain, ns, NSDomains),
    % all NS records have associated A records
    lists:foreach(fun({Domain, IPs}) ->
        assert_dns_answer(OZIPs, Domain, a, IPs)
    end, NSDomainsIPs).


%%--------------------------------------------------------------------
%% @doc
%% Configuration variable can be used to force resolving more
%% nsX domains than there are nodes.
%% @end
%%--------------------------------------------------------------------
dns_server_duplicates_ns_records_test(Config) ->
    OZIPs = ?config(oz_ips, Config),
    OZDomain = ?config(oz_domain, Config),

    Minimum = 4,
    Maximum = 5,
    oz_test_utils:set_env(Config, dns_ns_max_entries, Maximum),
    oz_test_utils:set_env(Config, dns_ns_min_entries, Minimum),

    % force dns update
    ?assertEqual(ok, oz_test_utils:call_oz(Config,
        node_manager_plugin, reconcile_dns_config, [])),

    % number of nodes based on env_desc
    [IP1, IP2, IP3] = lists:sort(OZIPs),
    NSDomainsIPs = [{"ns1." ++ OZDomain, [IP1]}, {"ns2." ++ OZDomain, [IP2]},
        {"ns3." ++ OZDomain, [IP3]}, {"ns4." ++ OZDomain, [IP1]}],
    {NSDomains, _} = lists:unzip(NSDomainsIPs),

    assert_dns_answer(OZIPs, OZDomain, ns, NSDomains),
    % all NS records have associated A records
    lists:foreach(fun({Domain, IPs}) ->
        assert_dns_answer(OZIPs, Domain, a, IPs)
    end, NSDomainsIPs).


%%--------------------------------------------------------------------
%% @doc
%% A subdomain must not be set for a provider if the subdomain is
%% already in use elsewhere.
%% @end
%%--------------------------------------------------------------------
update_fails_on_duplicated_subdomain_test(Config) ->
    Name1 = ?PROVIDER_NAME1,
    Name2 = ?PROVIDER_NAME2,
    SubdomainBin = <<?PROVIDER_SUBDOMAIN1>>,
    StaticSubdomain = <<"test">>,
    StaticNSSubdomain = <<"test">>,

    oz_test_utils:set_env(Config, dns_static_a_records, [{StaticSubdomain, [{1, 1, 1, 1}]}]),
    % ns records should also block setting subdomain
    oz_test_utils:set_env(Config, dns_static_ns_records, [{StaticNSSubdomain, [StaticNSSubdomain]}]),
    {ok, {P1, _}} = oz_test_utils:create_provider(Config, Name1),
    {ok, {P2, _}} = oz_test_utils:create_provider(Config, Name2),


    oz_test_utils:enable_subdomain_delegation(Config, P1, SubdomainBin, []),

    Data = #{
        <<"subdomainDelegation">> => true,
        <<"subdomain">> => SubdomainBin,
        <<"ipList">> => []},

    % subdomain used by another provider
    ?assertMatch(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"subdomain">>),
        oz_test_utils:call_oz(Config,
            provider_logic, update_domain_config, [?ROOT, P2, Data])
    ),

    % subdomain reserved for nameserver
    Data2 = Data#{<<"subdomain">> := <<"ns19">>},
    ?assertMatch(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"subdomain">>),
        oz_test_utils:call_oz(Config,
            provider_logic, update_domain_config, [?ROOT, P2, Data2])
    ),

    % subdomain configured in app config
    Data3 = Data#{<<"subdomain">> := StaticSubdomain},
    ?assertMatch(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"subdomain">>),
        oz_test_utils:call_oz(Config,
            provider_logic, update_domain_config, [?ROOT, P2, Data3])
    ),

    % subdomain configured in app config for ns server
    Data4 = Data#{<<"subdomain">> := StaticNSSubdomain},
    ?assertMatch(?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"subdomain">>),
        oz_test_utils:call_oz(Config,
            provider_logic, update_domain_config, [?ROOT, P2, Data4])
    ).



dns_server_resolves_static_records(Config) ->
    OZIPs = ?config(oz_ips, Config),
    OZDomain = ?config(oz_domain, Config),

    % app.config entries can use upper/mixed case and should be lowercased
    % for use by the DNS server.
    % The DNS server should understand queries regardless of case.
    Records = [
        {
            txt,
            dns_static_txt_records,
            [{<<"tXt">>, <<"txt-value">>}],
            {"txt." ++ OZDomain, [["txt-value"]]}},
        {
            a,
            dns_static_a_records,
            [{<<"A">>, ?PROVIDER_IPS1}],
            {"a." ++ OZDomain, ?PROVIDER_IPS1}},
        {
            mx,
            dns_static_mx_records,
            [{<<"mX">>, <<"mx-value">>, 10}],
            {"mx." ++ OZDomain, [{10, "mx-value"}]}
        },
        {
            cname,
            dns_static_cname_records,
            [{<<"cName">>, <<"cname-value">>}],
            {"cname." ++ OZDomain, ["cname-value"]}
        },
        {
            ns,
            dns_static_ns_records,
            [{<<"nS">>, [<<"ns1-value">>, <<"ns2-value">>]}],
            {"ns." ++ OZDomain, ["ns1-value", "ns2-value"]}}
    ],

    lists:foreach(fun({_, Env, Entries, _}) ->
        oz_test_utils:set_env(Config, Env, Entries)
    end, Records),
    ?assertEqual(ok, oz_test_utils:call_oz(Config,
        node_manager_plugin, reconcile_dns_config, [])),

    lists:foreach(fun({Type, _, _, {Query, Expected}}) ->
        assert_dns_answer(OZIPs, Query, Type, Expected),
        assert_dns_answer(OZIPs, string:uppercase(Query), Type, Expected)
    end, Records).



%%--------------------------------------------------------------------
%% @doc
%% When a static subdomain entry is set after a provider has registered
%% with the same subdomain the provider subdomain should take precedence.
%% This detection of duplicates should be case insensitive.
%% @end
%%--------------------------------------------------------------------
static_subdomain_does_not_shadow_provider_subdomain_test(Config) ->
    %% given
    ProviderName = ?PROVIDER_NAME1,
    ProviderIPs1 = ?PROVIDER_IPS1,
    StaticIPs = ?STATIC_SUBDOMAIN_IPS1,

    OZIPs = ?config(oz_ips, Config),

    Subdomain = ?PROVIDER_SUBDOMAIN1,
    SubdomainBin = <<?PROVIDER_SUBDOMAIN1>>,
    UpperSubdomainBin = string:uppercase(<<?PROVIDER_SUBDOMAIN1>>),
    OZDomain = ?config(oz_domain, Config),
    FullDomain = Subdomain ++ "." ++ OZDomain,
    UpperFullDomain = binary_to_list(UpperSubdomainBin) ++ "." ++ OZDomain,

    % provider uses a subdomain
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config,
        ProviderName),
    oz_test_utils:enable_subdomain_delegation(
        Config, ProviderId, SubdomainBin, ProviderIPs1),

    % subdomain is set as static entry statically
    oz_test_utils:set_env(Config, dns_static_a_records,
        [{SubdomainBin, StaticIPs}, {UpperSubdomainBin, StaticIPs}]),

    % DNS update is sent
    ?assertEqual(ok, oz_test_utils:call_oz(Config,
        node_manager_plugin, reconcile_dns_config, [])),

    % provider IPs are still resolved
    assert_dns_answer(OZIPs, FullDomain, a, ProviderIPs1),
    assert_dns_answer(OZIPs, UpperFullDomain, a, ProviderIPs1).


%%--------------------------------------------------------------------
%% @doc
%% When subdomain delegation is disabled, dns server should stop resolving old
%% subdomain.
%% @end
%%--------------------------------------------------------------------
dns_server_does_not_resolve_removed_subdomain_test(Config) ->
    %% given
    Name = ?PROVIDER_NAME1,
    ProviderIPs = ?PROVIDER_IPS1,
    OZIPs = ?config(oz_ips, Config),

    OZDomain = ?config(oz_domain, Config),
    Subdomain = ?PROVIDER_SUBDOMAIN1,
    SubdomainBin = <<?PROVIDER_SUBDOMAIN1>>,
    FullDomain = Subdomain ++ "." ++ OZDomain,

    Domain = ?EXTERNAL_DOMAIN1,
    DomainBin = list_to_binary(Domain),

    %% when
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, Name),
    oz_test_utils:enable_subdomain_delegation(
        Config, ProviderId, SubdomainBin, ProviderIPs),

    assert_dns_answer(OZIPs, FullDomain, a, ProviderIPs),

    % disable subdomain delegation
    oz_test_utils:set_provider_domain(Config, ProviderId, DomainBin),

    %% then
    assert_dns_answer(OZIPs, FullDomain, a, []),
    % this domain should not be handled by OZ dns
    assert_dns_answer(OZIPs, DomainBin, a, []).


dns_resolves_txt_record(Config) ->
    OZIPs = ?config(oz_ips, Config),
    OZDomain = ?config(oz_domain, Config),

    Name = ?PROVIDER_NAME1,
    Subdomain = ?PROVIDER_SUBDOMAIN1,
    SubdomainBin = <<?PROVIDER_SUBDOMAIN1>>,
    FullDomain = Subdomain ++ "." ++ OZDomain,

    RecordContent = <<"special_letsencrypt_token">>,
    RecordContent2 = <<"changed ttl">>,
    RecordName = <<"acme_validation">>,
    RecordName2 = <<"custom_ttl">>,
    RecordFQDN = binary_to_list(RecordName) ++ "." ++ FullDomain,

    {ok, {P1, _}} = oz_test_utils:create_provider(Config, Name),

    oz_test_utils:enable_subdomain_delegation(Config, P1, SubdomainBin, []),

    ?assertMatch(ok,
        oz_test_utils:call_oz(Config,
            provider_logic, set_dns_txt_record, [?ROOT, P1, RecordName, RecordContent])
    ),
    ?assertMatch(ok,
        oz_test_utils:call_oz(Config,
            provider_logic, set_dns_txt_record, [?ROOT, P1, RecordName2,
                RecordContent2, 10])
    ),

    assert_dns_answer(OZIPs, RecordFQDN, txt, [[binary_to_list(RecordContent)]]).


txt_record_forbidden_without_subdomain_delegation(Config) ->
    Name = ?PROVIDER_NAME1,

    RecordContent = <<"special_letsencrypt_token">>,
    RecordName = <<"acme_validation">>,

    {ok, {P1, _}} = oz_test_utils:create_provider(Config, Name),


    ?assertMatch(?ERROR_SUBDOMAIN_DELEGATION_DISABLED,
        oz_test_utils:call_oz(Config,
            provider_logic, set_dns_txt_record, [?ROOT, P1, RecordName, RecordContent])
    ).


dns_does_not_resolve_removed_txt_record_test(Config) ->
    OZIPs = ?config(oz_ips, Config),
    OZDomain = ?config(oz_domain, Config),

    Name = ?PROVIDER_NAME1,
    Subdomain = ?PROVIDER_SUBDOMAIN1,
    SubdomainBin = <<?PROVIDER_SUBDOMAIN1>>,
    FullDomain = Subdomain ++ "." ++ OZDomain,

    RecordContent = <<"special_letsencrypt_token">>,
    RecordName = <<"acme_validation">>,
    RecordFQDN = binary_to_list(RecordName) ++ "." ++ FullDomain,

    {ok, {P1, _}} = oz_test_utils:create_provider(Config, Name),

    oz_test_utils:enable_subdomain_delegation(Config, P1, SubdomainBin, []),

    ?assertMatch(ok, oz_test_utils:call_oz(Config,
            provider_logic, set_dns_txt_record, [?ROOT, P1, RecordName, RecordContent])
    ),

    assert_dns_answer(OZIPs, RecordFQDN, txt, [[binary_to_list(RecordContent)]]),

    ?assertMatch(ok, oz_test_utils:call_oz(Config,
            provider_logic, remove_dns_txt_record, [?ROOT, P1, RecordName])
    ),

    assert_dns_answer(OZIPs, RecordFQDN, txt, []).


removing_nonexistent_txt_does_nothing(Config) ->
    {ok, #document{value = DnsStateBefore}} = ?assertMatch({ok, _},
        oz_test_utils:call_oz(Config, datastore_model, get, [?DATASTORE_CTX, ?DNS_STATE_KEY])),

    ?assertMatch(ok, oz_test_utils:call_oz(Config,
        dns_state, remove_txt_record, [<<"nonexistentProvider">>, <<"sometxt">>])
    ),

    {ok, #document{value = DnsStateAfter}} = ?assertMatch({ok, _},
        oz_test_utils:call_oz(Config, datastore_model, get, [?DATASTORE_CTX, ?DNS_STATE_KEY])),
    ?assertEqual(DnsStateBefore, DnsStateAfter).


dns_config_update_increases_soa_serial(Config) ->
    OZIPs = ?config(oz_ips, Config),
    OZDomain = ?config(oz_domain, Config),
    [PreviousSerial | _] = PreviousSerials = lists:map(fun(Server) ->
        get_soa_serial(OZDomain, Server)
    end, OZIPs),
    assert_all_equal(PreviousSerials),

    % wait for unix timestamp, used as the serial, to change
    timer:sleep(timer:seconds(1)),

    % force dns update
    ?assertEqual(ok, oz_test_utils:call_oz(Config,
        node_manager_plugin, reconcile_dns_config, [])),

    wait_for_soa_serial_to_change(OZIPs, OZDomain, PreviousSerial, ?DNS_ASSERT_RETRY_COUNT),

    [NewSerial | _] = NewSerials = lists:map(fun(Server) ->
        get_soa_serial(OZDomain, Server)
    end, OZIPs),
    assert_all_equal(NewSerials),
    ?assert(NewSerial > PreviousSerial).


%%%===================================================================
%%% Utils
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Verifies that all provided dns servers respond with expected
%% set of values. Does not verify order of received data.
%% @end
%%--------------------------------------------------------------------
-spec assert_dns_answer(Servers :: [inet:ip4_address()],
    Query :: string(), Type :: inet_res:rr_type(),
    Expected :: [inet_res:dns_data()]) ->
    ok | no_return().
assert_dns_answer(Servers, Query, Type, Expected) ->
    assert_dns_answer(Servers, Query, Type, Expected, ?DNS_ASSERT_RETRY_COUNT).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Verifies that all provided dns servers respond with expected
%% set of values. Does not verify order of received data.
%% Allows custom retries count.
%% @end
%%--------------------------------------------------------------------
-spec assert_dns_answer(Servers :: [inet:ip4_address()],
    Query :: string(), Type :: inet_res:r_type(),
    Expected :: [inet_res:dns_data()], Retries :: integer()) ->
    ok | no_return().
assert_dns_answer(Servers, Query, Type, Expected, Attempts) ->
    SortedExpected = lists:sort(Expected),
    lists:foreach(fun(Server) ->
        Opts = [{nameservers, [{Server, 53}]}],

        % there are multiple, delayed attempts because inet_res:lookup
        % displays ~20 seconds delay before returning updated results
        try
            ?assertEqual(SortedExpected,
                filter_response(Type, inet_res:resolve(Query, any, Type, Opts)),
                Attempts, ?DNS_ASSERT_RETRY_DELAY)
        catch error:{Reason, _} = Error
            when Reason =:= assertEqual_failed orelse Reason =:= assertMatch_failed ->
            ct:pal("DNS query type ~tp to server ~tp for name ~tp "
            "returned incorrect results in ~tp attempts.",
                [Type, Server, Query, Attempts]),
            erlang:error(Error)
        end
    % restrict to one server, to save time in tests ensuring NOT existence
    % of a record, as each server would wait for the timeout.
    end, [hd(Servers)]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Waits for DNS lookup of SOA record to return a changed serial number.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_soa_serial_to_change(Servers :: [inet:ip4_address()],
    Query :: string(), Previous :: integer(), Attempts :: non_neg_integer()) ->
    ok | no_return().
wait_for_soa_serial_to_change(Servers, Query, OldSerial, Attempts) ->
    lists:foreach(fun(Server) ->
        % there are multiple, delayed attempts because inet_res:lookup
        % displays ~20 seconds delay before returning updated results
        try
            NewSerial = get_soa_serial(Query, Server),
            ?assertNotEqual(OldSerial, NewSerial)
        catch _:_ when Attempts > 0 ->
            timer:sleep(?DNS_ASSERT_RETRY_DELAY),
            wait_for_soa_serial_to_change(Servers, Query, OldSerial, Attempts - 1)
        end
    end, Servers).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Filters results of inet_res:resolve by record type and returns it sorted.
%% @end
%%--------------------------------------------------------------------
-spec filter_response(Type :: atom(), Response :: {ok, #dns_rec{}} | {error, _}) ->
    [inet_res:dns_data()].
filter_response(_, {error, _}) -> [];
filter_response(Type, {ok, Response}) ->
    #dns_rec{anlist = Anlist, arlist = Arlist, nslist = Nslist} = Response,
    Filtered = lists:filtermap(fun
        (Record) when Record#dns_rr.type =:= Type -> {true, Record#dns_rr.data};
        (_) -> false
        end, Anlist ++ Arlist ++ Nslist),
    lists:sort(Filtered).


-spec get_soa_serial(Domain :: string(), Server :: inet:ip4_address()) ->
    Serial :: integer().
get_soa_serial(Domain, Server) ->
    Opts = [{nameservers, [{Server, 53}]}],
    [SoaRecord] = ?assertMatch([{_,_,_,_,_,_,_}],
        inet_res:lookup(Domain, in, soa, Opts)),
    element(3, SoaRecord).


-spec assert_all_equal(list()) -> ok | no_return().
assert_all_equal([]) ->
    ok;
assert_all_equal([Head | _] = List) ->
    ?assertEqual(lists:duplicate(length(List), Head), List).
