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

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("kernel/src/inet_dns.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").


%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

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

-define(TXT_RECORD_JSON(__NAME, __CONTENT), #{
    <<"name">> => __NAME, <<"content">> => __CONTENT
}).
-define(TXT_RECORD_JSON(__NAME, __CONTENT, __TTL), #{
    <<"name">> => __NAME, <<"content">> => __CONTENT, <<"ttl">> => __TTL
}).

%%%===================================================================
%%% Example data
%%%===================================================================


-define(PROVIDER_NAME1, <<"test_provider">>).
-define(PROVIDER_NAME2, <<"second_provider">>).
-define(OP_WORKER_IPS1, lists:sort([{240, 1, 1, 0}, {240, 1, 1, 1}, {240, 1, 1, 2}])).
-define(ONES3_IPS1, lists:sort([{240, 1, 1, 2}, {240, 1, 1, 3}])).
-define(OP_WORKER_IPS2, lists:sort([{241, 1, 1, 0}, {241, 1, 1, 1}, {241, 1, 1, 2}])).
-define(ONES3_PORT, 9999).
-define(STATIC_SUBDOMAIN_IPS1, lists:sort([{1, 2, 3, 4}, {5, 6, 7, 8}])).
-define(STATIC_SUBDOMAIN_IPS2, lists:sort([{122, 255, 255, 32}])).
-define(PROVIDER_SUBDOMAIN1, "provsub").
-define(PROVIDER_SUBDOMAIN2, "other-provsub").
-define(EXTERNAL_DOMAIN1, "domain.org").


%%%===================================================================
%%% Setup/Teardown functions
%%%===================================================================


-spec init_per_suite(test_config:config()) -> test_config:config().
init_per_suite(Config) ->
    oct_background:init_per_suite([{?LOAD_MODULES, [oz_test_utils]} | Config], #onenv_test_config{
        onenv_scenario = "1oz_3nodes",
        posthook = fun(NewConfig) ->
            oz_test_utils:set_env(NewConfig, subdomain_delegation_supported, true),

            OzDomain = str_utils:to_list(oct_background:get_zone_domain()),
            OzIps = get_oz_ips(),
            [{oz_domain, OzDomain}, {oz_ips, lists:sort(OzIps)} | NewConfig]
        end
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


init_per_testcase(_Testcase, Config) ->
    Config.


end_per_testcase(Testcase = static_subdomain_does_not_shadow_provider_subdomain_test, Config) ->
    lists:foreach(fun(Env) ->
        oz_test_utils:set_env(Config, Env, [])
    end, [
        dns_static_a_records,
        dns_static_ns_records,
        dns_static_mx_records,
        dns_static_txt_records,
        dns_static_cname_records
    ]),
    end_per_testcase(?DEFAULT_CASE(Testcase), Config);

end_per_testcase(_, Config) ->
    % prevent "subdomain occupied" errors
    oz_test_utils:delete_all_entities(Config),
    ok.


%%%===================================================================
%%% Tests
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% OneZone dns, working on every node, should respond with Ips of all OneZone
%% nodes.
%% @end
%%--------------------------------------------------------------------
dns_server_resolves_oz_domain_test(Config) ->
    OzDomain = ?config(oz_domain, Config),
    OzIps = ?config(oz_ips, Config),

    assert_dns_answer(OzIps, OzDomain, a, OzIps).


%%--------------------------------------------------------------------
%% @doc
%% When subdomain delegation is enabled for a provider, dns_state provides
%% information about its subdomain.
%% @end
%%--------------------------------------------------------------------
dns_state_stores_provider_data_test(Config) ->
    %% given
    OpName = ?PROVIDER_NAME1,
    OpWorkerIps = ?OP_WORKER_IPS1,
    OneS3Ips = ?ONES3_IPS1,
    OpSubdomainLabelBin = <<?PROVIDER_SUBDOMAIN1>>,

    %% when
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, OpName),
    oz_test_utils:enable_subdomain_delegation(
        Config, ProviderId, OpSubdomainLabelBin, OpWorkerIps, {OneS3Ips, ?ONES3_PORT}
    ),

    %% then
    ?assertEqual(
        {ok, OpSubdomainLabelBin, #{op_worker => OpWorkerIps, ones3 => OneS3Ips}},
        oz_test_utils:call_oz(Config, dns_state, get_delegation_config, [ProviderId])
    ),

    ?assertEqual(
        #{OpSubdomainLabelBin => OpWorkerIps, <<"s3.", OpSubdomainLabelBin/binary>> => OneS3Ips},
        oz_test_utils:call_oz(Config, dns_state, get_provider_relative_domain_names_to_ips, [])
    ).


%%--------------------------------------------------------------------
%% @doc
%% DNS on all Oz nodes should resolve provider domain built from subdomain
%% and oz domain
%% @end
%%--------------------------------------------------------------------
dns_server_resolves_delegated_subdomain_test(Config) ->
    %% given
    OzDomain = ?config(oz_domain, Config),
    OzIps = ?config(oz_ips, Config),

    OpName = ?PROVIDER_NAME1,
    OpSubdomainLabel = ?PROVIDER_SUBDOMAIN1,

    OpWorkerDomain = OpSubdomainLabel ++ "." ++ OzDomain,
    OpWorkerIps = ?OP_WORKER_IPS1,

    OneS3Domain = "s3." ++ OpWorkerDomain,
    OneS3Ips = ?ONES3_IPS1,

    %% when
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, OpName),
    oz_test_utils:enable_subdomain_delegation(
        Config, ProviderId, OpSubdomainLabel, OpWorkerIps, {OneS3Ips, ?ONES3_PORT}
    ),

    %% then
    {ok, ProviderDoc} = oz_test_utils:get_provider(Config, ProviderId),
    ?assertEqual(list_to_binary(OpWorkerDomain), ProviderDoc#od_provider.domain),

    assert_dns_answer(OzIps, OpWorkerDomain, a, OpWorkerIps),
    assert_dns_answer(OzIps, OneS3Domain, a, OneS3Ips).


%%--------------------------------------------------------------------
%% @doc
%% When subdomain delegation is set with a different subdomain, dns server
%% should stop resolving old subdomain and start resolving new.
%% @end
%%--------------------------------------------------------------------
dns_server_resolves_changed_subdomain_test(Config) ->
    %% given
    OzDomain = ?config(oz_domain, Config),
    OzIps = ?config(oz_ips, Config),

    OpName = ?PROVIDER_NAME1,
    OpSubdomainLabel1 = ?PROVIDER_SUBDOMAIN1,
    OpSubdomainLabel2 = ?PROVIDER_SUBDOMAIN2,

    OpWorkerDomain1 = OpSubdomainLabel1 ++ "." ++ OzDomain,
    OpWorkerDomain2 = OpSubdomainLabel2 ++ "." ++ OzDomain,
    OpWorkerIps = ?OP_WORKER_IPS1,

    OneS3Domain1 = "s3." ++ OpWorkerDomain1,
    OneS3Domain2 = "s3." ++ OpWorkerDomain2,
    OneS3Ips = ?ONES3_IPS1,

    %% when
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, OpName),
    oz_test_utils:enable_subdomain_delegation(
        Config, ProviderId, OpSubdomainLabel1, OpWorkerIps, {OneS3Ips, ?ONES3_PORT}
    ),

    assert_dns_answer(OzIps, OpWorkerDomain1, a, OpWorkerIps),
    assert_dns_answer(OzIps, OneS3Domain1, a, OneS3Ips),

    oz_test_utils:enable_subdomain_delegation(
        Config, ProviderId, OpSubdomainLabel2, OpWorkerIps, {OneS3Ips, ?ONES3_PORT}
    ),

    %% then
    assert_dns_answer(OzIps, OpWorkerDomain1, a, []),
    assert_dns_answer(OzIps, OneS3Domain1, a, []),
    assert_dns_answer(OzIps, OpWorkerDomain2, a, OpWorkerIps),
    assert_dns_answer(OzIps, OneS3Domain2, a, OneS3Ips).


%%--------------------------------------------------------------------
%% @doc
%% DNS zone should have a number of NS records pointing to nsX subdomains.
%% Those subdomains should be resolved to Oz nodes ips.
%% Their number is limited by config.
%% @end
%%--------------------------------------------------------------------
dns_server_resolves_ns_records_test(Config) ->
    OzDomain = ?config(oz_domain, Config),
    OzIps = ?config(oz_ips, Config),

    Maximum = 2,
    oz_test_utils:set_env(Config, dns_ns_max_entries, Maximum),
    oz_test_utils:set_env(Config, dns_ns_min_entries, 1), % the basic case

    % force dns update
    reconcile_dns_config(Config),

    % number of nodes based on env_desc
    [IP1, IP2, _IP3] = OzIps,
    NSDomainsIps = [{"ns1." ++ OzDomain, [IP1]}, {"ns2." ++ OzDomain, [IP2]}],
    {NSDomains, _} = lists:unzip(NSDomainsIps),

    assert_dns_answer(OzIps, OzDomain, ns, NSDomains),

    % all NS records have associated A records
    lists:foreach(fun({Domain, Ips}) ->
        assert_dns_answer(OzIps, Domain, a, Ips)
    end, NSDomainsIps).


%%--------------------------------------------------------------------
%% @doc
%% Configuration variable can be used to force resolving more
%% nsX domains than there are nodes.
%% @end
%%--------------------------------------------------------------------
dns_server_duplicates_ns_records_test(Config) ->
    OzDomain = ?config(oz_domain, Config),
    OzIps = ?config(oz_ips, Config),

    Minimum = 4,
    Maximum = 5,
    oz_test_utils:set_env(Config, dns_ns_max_entries, Maximum),
    oz_test_utils:set_env(Config, dns_ns_min_entries, Minimum),

    % force dns update
    reconcile_dns_config(Config),

    % number of nodes based on env_desc
    [IP1, IP2, IP3] = OzIps,
    NSDomainsIps = [
        {"ns1." ++ OzDomain, [IP1]},
        {"ns2." ++ OzDomain, [IP2]},
        {"ns3." ++ OzDomain, [IP3]},
        {"ns4." ++ OzDomain, [IP1]}
    ],
    {NSDomains, _} = lists:unzip(NSDomainsIps),

    assert_dns_answer(OzIps, OzDomain, ns, NSDomains),

    % all NS records have associated A records
    lists:foreach(fun({Domain, Ips}) ->
        assert_dns_answer(OzIps, Domain, a, Ips)
    end, NSDomainsIps).


%%--------------------------------------------------------------------
%% @doc
%% A subdomain must not be set for a provider if the subdomain is
%% already in use elsewhere.
%% @end
%%--------------------------------------------------------------------
update_fails_on_duplicated_subdomain_test(Config) ->
    OpName1 = ?PROVIDER_NAME1,
    OpName2 = ?PROVIDER_NAME2,
    SubdomainBin = <<?PROVIDER_SUBDOMAIN1>>,
    StaticSubdomain = <<"static-subdomain">>,
    NestedStaticSubdomainTopElement = <<"test-subdomain">>,
    NestedStaticSubdomain = <<"nested.static.", NestedStaticSubdomainTopElement/binary>>,
    StaticNSSubdomain = <<"ns-subdomain">>,

    oz_test_utils:set_env(Config, dns_static_a_records, [
        {StaticSubdomain, [{1, 1, 1, 1}]},
        {NestedStaticSubdomain, [{1, 1, 1, 1}]}
    ]),
    % ns records should also block setting subdomain
    oz_test_utils:set_env(Config, dns_static_ns_records, [{StaticNSSubdomain, [StaticNSSubdomain]}]),
    {ok, {P1, _}} = oz_test_utils:create_provider(Config, OpName1),
    {ok, {P2, _}} = oz_test_utils:create_provider(Config, OpName2),

    oz_test_utils:enable_subdomain_delegation(Config, P1, SubdomainBin, []),

    Data = #{
        <<"subdomainDelegation">> => true,
        <<"subdomain">> => SubdomainBin,
        <<"ipList">> => []
    },

    % subdomain used by another provider
    ?assertMatch(
        ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"subdomain">>),
        oz_test_utils:call_oz(Config, provider_logic, update_domain_config, [?ROOT, P2, Data])
    ),

    % subdomain reserved for nameserver
    Data2 = Data#{<<"subdomain">> := <<"ns19">>},
    ?assertMatch(
        ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"subdomain">>),
        oz_test_utils:call_oz(Config, provider_logic, update_domain_config, [?ROOT, P2, Data2])
    ),

    % static subdomain configured in app config
    Data3 = Data#{<<"subdomain">> := StaticSubdomain},
    ?assertMatch(
        ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"subdomain">>),
        oz_test_utils:call_oz(Config, provider_logic, update_domain_config, [?ROOT, P2, Data3])
    ),

    % nested static subdomain configured in app config
    Data4 = Data#{<<"subdomain">> := NestedStaticSubdomainTopElement},
    ?assertMatch(
        ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"subdomain">>),
        oz_test_utils:call_oz(Config, provider_logic, update_domain_config, [?ROOT, P2, Data4])
    ),

    % subdomain configured in app config for ns server
    Data6 = Data#{<<"subdomain">> := StaticNSSubdomain},
    ?assertMatch(
        ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"subdomain">>),
        oz_test_utils:call_oz(Config, provider_logic, update_domain_config, [?ROOT, P2, Data6])
    ).


dns_server_resolves_static_records(Config) ->
    OzDomain = ?config(oz_domain, Config),
    OzIps = ?config(oz_ips, Config),

    % app.config entries can use upper/mixed case and should be lowercased
    % for use by the DNS server.
    % The DNS server should understand queries regardless of case.
    Records = [
        {
            txt,
            dns_static_txt_records,
            [{<<"tXt">>, <<"txt-value">>}],
            {"txt." ++ OzDomain, [["txt-value"]]}},
        {
            a,
            dns_static_a_records,
            [{<<"A">>, ?OP_WORKER_IPS1}],
            {"a." ++ OzDomain, ?OP_WORKER_IPS1}},
        {
            mx,
            dns_static_mx_records,
            [{<<"mX">>, <<"mx-value">>, 10}],
            {"mx." ++ OzDomain, [{10, "mx-value"}]}
        },
        {
            cname,
            dns_static_cname_records,
            [{<<"cName">>, <<"cname-value">>}],
            {"cname." ++ OzDomain, ["cname-value"]}
        },
        {
            ns,
            dns_static_ns_records,
            [{<<"nS">>, [<<"ns1-value">>, <<"ns2-value">>]}],
            {"ns." ++ OzDomain, ["ns1-value", "ns2-value"]}}
    ],

    lists:foreach(fun({_, Env, Entries, _}) ->
        oz_test_utils:set_env(Config, Env, Entries)
    end, Records),

    reconcile_dns_config(Config),

    lists:foreach(fun({Type, _, _, {Query, Expected}}) ->
        assert_dns_answer(OzIps, Query, Type, Expected),
        assert_dns_answer(OzIps, string:uppercase(Query), Type, Expected)
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
    OzDomain = ?config(oz_domain, Config),
    OzIps = ?config(oz_ips, Config),

    OpName = ?PROVIDER_NAME1,
    OpSubdomainLabel = ?PROVIDER_SUBDOMAIN1,
    OpSubdomainLabelBin = <<?PROVIDER_SUBDOMAIN1>>,
    OpWorkerDomain = OpSubdomainLabel ++ "." ++ OzDomain,
    OpWorkerIps1 = ?OP_WORKER_IPS1,

    NestedSubdomainBin = <<"nested.", OpSubdomainLabelBin/binary>>,

    UpperOpSubdomainLabelBin = string:uppercase(<<?PROVIDER_SUBDOMAIN1>>),
    UpperOpDomain = binary_to_list(UpperOpSubdomainLabelBin) ++ "." ++ OzDomain,

    StaticIps = ?STATIC_SUBDOMAIN_IPS1,

    % provider uses a subdomain
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, OpName),
    oz_test_utils:enable_subdomain_delegation(Config, ProviderId, OpSubdomainLabelBin, OpWorkerIps1),

    % subdomain is set as static entry statically
    oz_test_utils:set_env(Config, dns_static_a_records, [
        {OpSubdomainLabelBin, StaticIps},
        {NestedSubdomainBin, StaticIps},
        {UpperOpSubdomainLabelBin, StaticIps}
    ]),

    % DNS update is sent
    reconcile_dns_config(Config),

    % provider Ips are still resolved
    assert_dns_answer(OzIps, OpWorkerDomain, a, OpWorkerIps1),
    assert_dns_answer(OzIps, UpperOpDomain, a, OpWorkerIps1),

    % and nested static subdomain is not
    assert_dns_answer(OzIps, NestedSubdomainBin, a, []).


%%--------------------------------------------------------------------
%% @doc
%% When subdomain delegation is disabled, dns server should stop resolving old
%% subdomain.
%% @end
%%--------------------------------------------------------------------
dns_server_does_not_resolve_removed_subdomain_test(Config) ->
    %% given
    OzDomain = ?config(oz_domain, Config),
    OzIps = ?config(oz_ips, Config),

    OpName = ?PROVIDER_NAME1,
    OpWorkerIps = ?OP_WORKER_IPS1,
    OneS3Ips = ?ONES3_IPS1,

    OpSubdomainLabel = ?PROVIDER_SUBDOMAIN1,
    OpWorkerDomain = OpSubdomainLabel ++ "." ++ OzDomain,
    OneS3Domain = "s3." ++ OpWorkerDomain,

    OpDomain = ?EXTERNAL_DOMAIN1,
    OpDomainBin = list_to_binary(OpDomain),

    %% when
    {ok, {ProviderId, _}} = oz_test_utils:create_provider(Config, OpName),
    oz_test_utils:enable_subdomain_delegation(
        Config, ProviderId, OpSubdomainLabel, OpWorkerIps, {OneS3Ips, ?ONES3_PORT}
    ),

    assert_dns_answer(OzIps, OpWorkerDomain, a, OpWorkerIps),
    assert_dns_answer(OzIps, OneS3Domain, a, OneS3Ips),

    % disable subdomain delegation
    oz_test_utils:set_provider_domain(Config, ProviderId, OpDomainBin),

    %% then
    assert_dns_answer(OzIps, OpWorkerDomain, a, []),
    assert_dns_answer(OzIps, OneS3Domain, a, []),
    % this domain should not be handled by Oz dns
    assert_dns_answer(OzIps, OpDomainBin, a, []).


dns_resolves_txt_record(Config) ->
    OzDomain = ?config(oz_domain, Config),
    OzIps = ?config(oz_ips, Config),

    OpName = ?PROVIDER_NAME1,
    OpSubdomainLabel = ?PROVIDER_SUBDOMAIN1,

    Service = ?RAND_ELEMENT([op_worker, ones3]),
    FullDomain = case Service of
        op_worker -> OpSubdomainLabel ++ "." ++ OzDomain;
        ones3 -> "s3." ++ OpSubdomainLabel ++ "." ++ OzDomain
    end,

    RecordName = <<"acme_validation">>,
    RecordContent = <<"special_letsencrypt_token">>,
    RecordFQDN = binary_to_list(RecordName) ++ "." ++ FullDomain,
    RecordName2 = <<"custom_ttl">>,
    RecordContent2 = <<"changed ttl">>,
    RecordFQDN2 = binary_to_list(RecordName2) ++ "." ++ FullDomain,

    {ok, {P1, _}} = oz_test_utils:create_provider(Config, OpName),

    oz_test_utils:enable_subdomain_delegation(Config, P1, OpSubdomainLabel, []),

    ?assertMatch(ok, set_dns_txt_record(Config, Service, P1, RecordName, RecordContent, 5)),
    ?assertMatch(ok, set_dns_txt_record(Config, Service, P1, RecordName2, RecordContent2, 5)),

    assert_dns_answer(OzIps, RecordFQDN, txt, [[binary_to_list(RecordContent)]]),
    assert_dns_answer(OzIps, RecordFQDN2, txt, [[binary_to_list(RecordContent2)]]).


txt_record_forbidden_without_subdomain_delegation(Config) ->
    OpName = ?PROVIDER_NAME1,

    RecordContent = <<"special_letsencrypt_token">>,
    RecordName = <<"acme_validation">>,

    {ok, {P1, _}} = oz_test_utils:create_provider(Config, OpName),

    ?assertMatch(
        ?ERROR_SUBDOMAIN_DELEGATION_DISABLED,
        set_dns_txt_record(Config, ?RAND_ELEMENT([op_worker, ones3]), P1, RecordName, RecordContent, 5)
    ).


dns_does_not_resolve_removed_txt_record_test(Config) ->
    OzDomain = ?config(oz_domain, Config),
    OzIps = ?config(oz_ips, Config),

    OpName = ?PROVIDER_NAME1,
    OpSubdomainLabel = ?PROVIDER_SUBDOMAIN1,

    Service = ?RAND_ELEMENT([op_worker, ones3]),
    FullDomain = case Service of
        op_worker -> OpSubdomainLabel ++ "." ++ OzDomain;
        ones3 -> "s3." ++ OpSubdomainLabel ++ "." ++ OzDomain
    end,

    RecordContent = <<"special_letsencrypt_token">>,
    RecordName = <<"acme_validation">>,
    RecordFQDN = binary_to_list(RecordName) ++ "." ++ FullDomain,

    {ok, {P1, _}} = oz_test_utils:create_provider(Config, OpName),

    oz_test_utils:enable_subdomain_delegation(Config, P1, OpSubdomainLabel, []),

    ?assertMatch(ok, set_dns_txt_record(Config, Service, P1, RecordName, RecordContent, 5)),

    assert_dns_answer(OzIps, RecordFQDN, txt, [[binary_to_list(RecordContent)]]),

    ?assertMatch(ok, unset_dns_txt_record(Config, Service, P1, RecordName)),

    assert_dns_answer(OzIps, RecordFQDN, txt, []).


removing_nonexistent_txt_does_nothing(Config) ->
    {ok, #document{value = DnsStateBefore}} = ?assertMatch({ok, _},
        oz_test_utils:call_oz(Config, datastore_model, get, [?DATASTORE_CTX, ?DNS_STATE_KEY])),

    ?assertMatch({error, no_subdomain}, oz_test_utils:call_oz(Config, dns_state, update_txt_records, [
        <<"nonexistentProvider">>, #{op_worker => #{unset => [<<"sometxt">>]}}
    ])),

    {ok, #document{value = DnsStateAfter}} = ?assertMatch({ok, _},
        oz_test_utils:call_oz(Config, datastore_model, get, [?DATASTORE_CTX, ?DNS_STATE_KEY])),
    ?assertEqual(DnsStateBefore, DnsStateAfter).


dns_config_update_increases_soa_serial(Config) ->
    OzDomain = ?config(oz_domain, Config),
    OzIps = ?config(oz_ips, Config),

    [PreviousSerial | _] = PreviousSerials = lists:map(fun(Server) ->
        get_soa_serial(OzDomain, Server)
    end, OzIps),
    assert_all_equal(PreviousSerials),

    % wait for unix timestamp, used as the serial, to change
    timer:sleep(timer:seconds(1)),

    % force dns update
    reconcile_dns_config(Config),

    wait_for_soa_serial_to_change(OzIps, OzDomain, PreviousSerial, ?DNS_ASSERT_RETRY_COUNT),

    [NewSerial | _] = NewSerials = lists:map(fun(Server) ->
        get_soa_serial(OzDomain, Server)
    end, OzIps),
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


%% @private
reconcile_dns_config(Config) ->
    ?assertEqual(ok, oz_test_utils:call_oz(
        Config, node_manager_plugin, reconcile_dns_config, []
    )).


%% @private
-spec get_oz_ips() -> [inet:ip_address()].
get_oz_ips() ->
    lists:sort(lists:map(fun get_node_ip/1, oct_background:get_zone_panels())).


%% @private
-spec get_node_ip(node()) -> inet:ip_address().
get_node_ip(Node) ->
    panel_test_rpc:insecure_call(Node, fun() ->
        {ok, IpAddresses} = inet:getifaddrs(),
        hd([
            Addr || {_, Opts} <- IpAddresses, {addr, Addr} <- Opts,
            size(Addr) == 4, Addr =/= {127, 0, 0, 1}
        ])
    end).


%% @private
set_dns_txt_record(Config, ones3, ProviderId, Name, Content, TTL) ->
    oz_test_utils:call_oz(Config, provider_logic, update_dns_txt_record, [
        ?ROOT, ProviderId, #{<<"setOneS3TxtRecord">> => ?TXT_RECORD_JSON(Name, Content, TTL)}
    ]);
set_dns_txt_record(Config, op_worker, ProviderId, Name, Content, TTL) ->
    case ?RAND_BOOL() of
        true ->
            oz_test_utils:call_oz(Config, provider_logic, update_dns_txt_record, [
                ?ROOT, ProviderId, #{<<"setOpWorkerTxtRecord">> => ?TXT_RECORD_JSON(Name, Content, TTL)}
            ]);
        false ->
            oz_test_utils:call_oz(Config, provider_logic, set_dns_txt_record, [
                ?ROOT, ProviderId, Name, Content, TTL
            ])
    end.


%% @private
unset_dns_txt_record(Config, ones3, ProviderId, Name) ->
    oz_test_utils:call_oz(Config, provider_logic, update_dns_txt_record, [
        ?ROOT, ProviderId, #{<<"unsetOneS3TxtRecordName">> => Name}
    ]);
unset_dns_txt_record(Config, op_worker, ProviderId, Name) ->
    case ?RAND_BOOL() of
        true ->
            oz_test_utils:call_oz(Config, provider_logic, update_dns_txt_record, [
                ?ROOT, ProviderId, #{<<"unsetOpWorkerTxtRecordName">> => Name}
            ]);
        false ->
            oz_test_utils:call_oz(Config, provider_logic, remove_dns_txt_record, [
                ?ROOT, ProviderId, Name
            ])
    end.
