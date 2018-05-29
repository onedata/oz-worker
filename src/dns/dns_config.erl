%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains functions for updating erldns.
%%% @end
%%%-------------------------------------------------------------------
-module(dns_config).
-author("Wojciech Geisler").

-include("registered_names.hrl").
-include_lib("dns/include/dns.hrl").
-include_lib("ctool/include/logging.hrl").

-export([build_config/0, insert_config/1]).
-export([build_domain/2, build_fqdn_from_subdomain/1]).

-type domain() :: binary().
-type domain_entry() :: {domain(), [inet:ip4_address()]}.
-type dns_config() :: {Name :: domain(), _Version :: <<>>, Records :: [#dns_rr{}]}.
-export_type([domain/0, dns_config/0]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Joins subdomain with domain to build fully qualified domain.
%% @end
%%--------------------------------------------------------------------
-spec build_domain(Subdomain :: domain(), Domain :: domain()) -> domain().
build_domain(<<>>, Domain) ->
    Domain;
build_domain(Subdomain, Domain) ->
    <<Subdomain/binary, ".", Domain/binary>>.


%%--------------------------------------------------------------------
%% @doc
%% Joins provided subdomain with onezone domain.
%% @end
%%--------------------------------------------------------------------
-spec build_fqdn_from_subdomain(Subdomain :: domain()) -> domain().
build_fqdn_from_subdomain(Subdomain) ->
    build_domain(Subdomain, oz_worker:get_domain()).


%%--------------------------------------------------------------------
%% @doc
%% Inserts prebuilt dns config into dns server.
%% @end
%%--------------------------------------------------------------------
-spec insert_config(dns_config()) -> ok.
insert_config(Config) ->
    ok = erldns_zone_cache:put_zone(Config).


%%--------------------------------------------------------------------
%% @doc
%% Builds dns config with up to date data, using provided IPs
%% for OneZone.
%% @end
%%--------------------------------------------------------------------
-spec build_config() -> dns_config().
build_config() ->
    OneZoneIPs = node_manager:get_cluster_ips(),
    OneZoneDomain = oz_worker:get_domain(),
    AdminEmail = get_env(soa_admin_mailbox),

    OnezoneNS = build_onezone_ns_entries(OneZoneIPs),

    {PrimaryNS, _IPs} = hd(OnezoneNS),
    SOARecord = build_record_soa(OneZoneDomain, AdminEmail, PrimaryNS),

    {OneZoneDomain, <<>>, [
        SOARecord |
        build_a_records(OnezoneNS, OneZoneIPs) ++
        build_ns_records(OnezoneNS) ++
        build_txt_records() ++
        build_mx_records() ++
        build_cname_records()]}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns A records based on provider subdomains and entries in app config.
%% @end
%%--------------------------------------------------------------------
-spec build_a_records(NSDomains :: [domain_entry()], OneZoneIPs :: [inet:ip4_address()]) ->
    [#dns_rr{}].
build_a_records(NSDomains, OneZoneIPs) ->
    OneZoneDomain = oz_worker:get_domain(),

    ProviderSubdomains = maps:to_list(dns_state:get_subdomains_to_ips()),

    % check if there are any overlapping records
    StaticSubdomains = filter_shadowed_entries(get_env(static_a_records, [])),

    ProviderDomains = [{build_domain(Sub, OneZoneDomain), IPs}
        || {Sub, IPs} <- ProviderSubdomains ++ StaticSubdomains],

    Entries = [{OneZoneDomain, OneZoneIPs} | ProviderDomains ++ NSDomains],

    lists:flatmap(fun({Domain, IPs}) ->
        [build_record_a(Domain, IP) || IP <- IPs]
    end, Entries).



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates NS records based on onezone nodes
%% and static entries in app config.
%% @end
%%--------------------------------------------------------------------
-spec build_ns_records([domain_entry()]) -> [#dns_rr{}].
build_ns_records(OneZoneNS) ->
    OneZoneDomain = oz_worker:get_domain(),

    OnezoneRecords =
        [build_record_ns(OneZoneDomain, NSHost) || {NSHost, _} <- OneZoneNS],

    StaticEntries = filter_shadowed_entries(get_env(static_ns_records, [])),
    StaticRecords = lists:flatmap(fun({Subdomain, Nameservers}) ->
        NSs = case Nameservers of
            _ when is_list(Nameservers) -> Nameservers;
            _ -> [Nameservers]
        end,

        Domain = build_domain(Subdomain, OneZoneDomain),
        [build_record_ns(Domain, NS) || NS <- NSs]
    end, StaticEntries),

    OnezoneRecords ++ StaticRecords.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates domains for subdomains "ns1", "ns2" etc. in the onezone
%% domain to be used as nameserver addresses.
%% @end
%%--------------------------------------------------------------------
-spec build_onezone_ns_entries([inet:ip4_address()]) ->
    [domain_entry()].
build_onezone_ns_entries(OneZoneIPs) ->
    OneZoneDomain = oz_worker:get_domain(),
    NSIPs = lists:sort(OneZoneIPs),

    % ensure minimum number of NS subdomains is met
    Minimum = get_env(ns_min_entries, 1),
    Maximum = get_env(ns_max_entries, 10),
    TargetNum = min(Maximum, max(Minimum, length(NSIPs))),

    RepeatNum = utils:ceil(Minimum / length(NSIPs)),
    NSIPsRepeated =
        lists:sublist(lists:append(lists:duplicate(RepeatNum, NSIPs)),
            TargetNum),

    {NSDomainsIPs, _} = lists:foldl(fun(IP, {DomainsIPs, Count}) ->
        Index = integer_to_binary(Count),
        Domain = build_domain(<<"ns", Index/binary>>, OneZoneDomain),
        {[{Domain, [IP]} | DomainsIPs], Count + 1}
    end, {[], 1}, NSIPsRepeated),

    % preserve ascending order
    lists:reverse(NSDomainsIPs).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates TXT dns records.
%% @end
%%--------------------------------------------------------------------
-spec build_txt_records() -> [#dns_rr{}].
build_txt_records() ->
    OneZoneDomain = oz_worker:get_domain(),
    ProviderEntries = dns_state:get_txt_records(),
    StaticEntries = get_env(static_txt_records, []),

    lists:map(fun
        ({Name, {Content, undefined}}) ->
            Domain = build_domain(Name, OneZoneDomain),
            build_record_txt(Domain, Content);
        ({Name, {Value, TTL}}) ->
            Domain = build_domain(Name, OneZoneDomain),
            build_record_txt(Domain, Value, TTL);
        ({Name, Value}) ->
            Domain = build_domain(Name, OneZoneDomain),
            build_record_txt(Domain, Value)
    end, ProviderEntries ++ StaticEntries).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates MX dns records.
%% @end
%%--------------------------------------------------------------------
-spec build_mx_records() -> [#dns_rr{}].
build_mx_records() ->
    OneZoneDomain = oz_worker:get_domain(),
    StaticEntries = get_env(static_mx_records, []),

    lists:map(fun({Subdomain, Mailserver, Preference}) ->
        Domain = build_domain(Subdomain, OneZoneDomain),
        build_record_mx(Domain, Mailserver, Preference)
    end, StaticEntries).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates CNAME dns records.
%% @end
%%--------------------------------------------------------------------
-spec build_cname_records() -> [#dns_rr{}].
build_cname_records() ->
    OneZoneDomain = oz_worker:get_domain(),
    StaticEntries = filter_shadowed_entries(get_env(static_cname_records, [])),

    lists:map(fun({Subdomain, Target}) ->
        Domain = build_domain(Subdomain, OneZoneDomain),
        build_record_cname(Domain, Target)
    end, StaticEntries).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Filters out static entries for subdomains which are already used
%% by a provider, logging the fact.
%% @end
%%--------------------------------------------------------------------
filter_shadowed_entries(StaticEntries) ->
    ProviderEntries = maps:to_list(dns_state:get_subdomains_to_ips()),
    % check if there are any overlapping records
    lists:filter(fun(Entry) ->
        Subdomain = element(1, Entry), % not all tuples are 2-element, eg. MX entries
        case proplists:is_defined(Subdomain, ProviderEntries) of
            false -> true;
            _ ->
                ?warning("Ignoring static entry for subdomain \"~s\" "
                "as the subdomain is already used by a provider.", [Subdomain]),
                false
        end
    end, StaticEntries).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a dns A record for erldns.
%% @end
%%--------------------------------------------------------------------
-spec build_record_a(domain(), inet:ip4_address()) -> #dns_rr{}.
build_record_a(Domain, IP) ->
    #dns_rr{
        name = Domain,
        type = ?DNS_TYPE_A,
        ttl = get_env(a_ttl),
        data = #dns_rrdata_a{ip = IP}
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a dns SOA record for erldns.
%% @end
%%--------------------------------------------------------------------
-spec build_record_soa(Name :: domain(), MainName :: domain(),
    Admin :: binary()) -> #dns_rr{}.
build_record_soa(Name, MainName, Admin) ->
    #dns_rr{
        name = Name,
        type = ?DNS_TYPE_SOA,
        ttl = get_env(soa_ttl, 120),
        data = #dns_rrdata_soa{
            mname = MainName,
            rname = Admin,
            serial = get_env(soa_serial),
            refresh = get_env(soa_refresh),
            retry = get_env(soa_retry),
            expire = get_env(soa_expire),
            minimum = get_env(soa_minimum)
       }
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a NS record for erldns.
%% @end
%%--------------------------------------------------------------------
-spec build_record_ns(Name :: domain(), Nameserver :: domain()) -> #dns_rr{}.
build_record_ns(Name, Nameserver) ->
    #dns_rr{
        name = Name,
        type = ?DNS_TYPE_NS,
        ttl = get_env(ns_ttl, 120),
        data = #dns_rrdata_ns{dname = Nameserver}
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a TXT record for erldns. Binary content will be converted
%% to string (list).
%% @end
%%--------------------------------------------------------------------
-spec build_record_txt(domain(), binary() | string()) -> #dns_rr{}.
build_record_txt(Domain, Value) ->
    build_record_txt(Domain, Value, get_env(txt_ttl, 120)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a TXT record for erldns with provided TTL.
%% Binary content will be converted to string (list) as required
%% by erl_dns.
%% @end
%%--------------------------------------------------------------------
-spec build_record_txt(Domain :: domain(), Value :: binary() | string(),
    TTL :: non_neg_integer()) -> #dns_rr{}.
build_record_txt(Domain, Value, TTL) when is_binary(Value) ->
    build_record_txt(Domain, binary:bin_to_list(Value), TTL);
build_record_txt(Domain, Value, TTL) ->
    #dns_rr{
        name = Domain,
        type = ?DNS_TYPE_TXT,
        ttl = TTL,
        data = #dns_rrdata_txt{txt = Value}
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a MX record for erldns.
%% @end
%%--------------------------------------------------------------------
-spec build_record_mx(domain(), domain(), integer()) -> #dns_rr{}.
build_record_mx(Domain, Address, Preference) ->
    #dns_rr{
        name = Domain,
        type = ?DNS_TYPE_MX,
        ttl = get_env(mx_ttl, 120),
        data = #dns_rrdata_mx{exchange = Address, preference = Preference}
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a CNAME record for erldns.
%% @end
%%--------------------------------------------------------------------
-spec build_record_cname(domain(), domain()) -> #dns_rr{}.
build_record_cname(Name, Target) ->
    #dns_rr{
        name = Name,
        type = ?DNS_TYPE_CNAME,
        ttl = get_env(cname_ttl, 120),
        data = #dns_rrdata_cname{dname = Target}
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves value from dns config. Throw error if config is not set.
%% @end
%%--------------------------------------------------------------------
-spec get_env(Key :: atom()) -> term().
get_env(Key) ->
    Props = oz_worker:get_env(dns, []),
    case proplists:get_value(Key, Props) of
        undefined ->
            ?error("Failed to fetch property \"~p\" from dns config.", [Key]),
            throw({error, {missing_config, dns, Key}});
        Value -> Value
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves value from dns config or return Default if not set.
%% @end
%%--------------------------------------------------------------------
-spec get_env(Key :: atom(), Default :: term()) -> term().
get_env(Key, Default) ->
    Props = oz_worker:get_env(dns, []),
    proplists:get_value(Key, Props, Default).
