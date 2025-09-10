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
-include_lib("dns_erlang/include/dns.hrl").
-include_lib("ctool/include/logging.hrl").

-export([build_config/0, insert_config/1]).
-export([get_ns_hosts/0]).

-type domain_entry() :: {dns_utils:domain_name(), [inet:ip4_address()]}.
-type dns_config() :: {Name :: dns_utils:domain_name(), _Version :: <<>>, Records :: [#dns_rr{}]}.
-type serial() :: -2147483648..2147483647.
-export_type([dns_config/0]).


%%%===================================================================
%%% API
%%%===================================================================


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
    AdminEmail = get_soa_admin(OneZoneDomain),
    DnsZoneSerial = generate_zone_serial(),

    OnezoneNS = build_onezone_ns_entries(OneZoneIPs),

    {PrimaryNS, _IPs} = hd(OnezoneNS),
    SOARecord = build_record_soa(OneZoneDomain, PrimaryNS,
        AdminEmail, DnsZoneSerial),

    {OneZoneDomain, <<>>, [
        SOARecord |
        build_a_records(OnezoneNS, OneZoneIPs) ++
        build_ns_records(OnezoneNS) ++
        build_txt_records() ++
        build_mx_records() ++
        build_cname_records()]}.


%%--------------------------------------------------------------------
%% @doc
%% Returns full names of the nsX. subdomains and corresponding IPs.
%% Used by Onepanel to determine recommended DNS config.
%% @end
%%--------------------------------------------------------------------
-spec get_ns_hosts() -> [{Name :: binary(), IP :: inet:ip4_address()}].
get_ns_hosts() ->
    OneZoneIPs = node_manager:get_cluster_ips(),
    OnezoneNS = build_onezone_ns_entries(OneZoneIPs),
    [{Name, IP} || {Name, [IP]} <- OnezoneNS].


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns admin email to be given in SOA record.
%% Uses value from app config if specified, otherwise generates
%% the name by prepending "admin" to onezone domain.
%% @end
%%--------------------------------------------------------------------
-spec get_soa_admin(OneZoneDomain :: binary()) -> binary().
get_soa_admin(OneZoneDomain) ->
    case oz_worker:get_env(dns_soa_admin_mailbox, undefined) of
        undefined -> <<"admin.", OneZoneDomain/binary>>;
        Admin -> str_utils:to_binary(Admin)
    end.


%%--------------------------------------------------------------------
%% @doc @private
%% Generates serial number for the SOA record. The number must change
%% every time the zone contents change, which is why it is set
%% to current unix timestamp every time the zone is generated.
%% The serial is ensured to be representable as a 32 bit signed integer
%% (negative SOA serials may be treated as larger in cases defined
%% by "serial number arithmetic", avoiding the Year 2038 Problem).
%% @end
%%--------------------------------------------------------------------
-spec generate_zone_serial() -> serial().
generate_zone_serial() ->
    wrap_like_signed_32bit(global_clock:timestamp_seconds()).


%%--------------------------------------------------------------------
%% @doc @private
%% Wraps an integer like a signed 32bit arithmetic would.
%% @end
%%--------------------------------------------------------------------
-spec wrap_like_signed_32bit(integer()) -> serial().
wrap_like_signed_32bit(Integer) when Integer < (1 bsl 31) -> Integer;
wrap_like_signed_32bit(Integer) -> wrap_like_signed_32bit(Integer - (1 bsl 32)).


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

    ProviderRelativeDomainNamesToIps = dns_state:get_provider_relative_domain_names_to_ips(),

    % check if there are any overlapping records
    StaticSubdomainsToIps = filter_shadowed_entries(oz_worker:get_env(dns_static_a_records, [])),

    ProviderDomainsToIps = [
        {dns_utils:build_domain(Sub, OneZoneDomain), IPs}
        || {Sub, IPs} <- StaticSubdomainsToIps ++ maps:to_list(ProviderRelativeDomainNamesToIps)
    ],

    Entries = [{OneZoneDomain, OneZoneIPs} | ProviderDomainsToIps ++ NSDomains],

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

    StaticEntries = filter_shadowed_entries(oz_worker:get_env(dns_static_ns_records, [])),
    StaticRecords = lists:flatmap(fun({Subdomain, Nameservers}) ->
        NSs = case Nameservers of
            _ when is_list(Nameservers) -> Nameservers;
            _ -> [Nameservers]
        end,

        Domain = dns_utils:build_domain(Subdomain, OneZoneDomain),
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
    Minimum = oz_worker:get_env(dns_ns_min_entries, 1),
    Maximum = oz_worker:get_env(dns_ns_max_entries, 10),
    TargetNum = min(Maximum, max(Minimum, length(NSIPs))),

    RepeatNum = utils:ceil(Minimum / length(NSIPs)),
    NSIPsRepeated =
        lists:sublist(lists:append(lists:duplicate(RepeatNum, NSIPs)),
            TargetNum),

    {NSDomainsIPs, _} = lists:foldl(fun(IP, {DomainsIPs, Count}) ->
        Index = integer_to_binary(Count),
        Domain = dns_utils:build_domain(<<"ns", Index/binary>>, OneZoneDomain),
        {
            [{Domain, [IP]} | DomainsIPs],
            Count + 1
        }
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
    StaticEntries = oz_worker:get_env(dns_static_txt_records, []),

    lists:map(fun
        ({Name, {Content, undefined}}) ->
            Domain = dns_utils:build_domain(Name, OneZoneDomain),
            build_record_txt(Domain, Content);
        ({Name, {Value, TTL}}) ->
            Domain = dns_utils:build_domain(Name, OneZoneDomain),
            build_record_txt(Domain, Value, TTL);
        ({Name, Value}) ->
            Domain = dns_utils:build_domain(Name, OneZoneDomain),
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
    StaticEntries = oz_worker:get_env(dns_static_mx_records, []),

    lists:map(fun({Subdomain, Mailserver, Preference}) ->
        Domain = dns_utils:build_domain(Subdomain, OneZoneDomain),
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
    StaticEntries = filter_shadowed_entries(oz_worker:get_env(dns_static_cname_records, [])),

    lists:map(fun({Subdomain, Target}) ->
        Domain = dns_utils:build_domain(Subdomain, OneZoneDomain),
        build_record_cname(Domain, Target)
    end, StaticEntries).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Filters out static entries for subdomains which are already used
%% by a provider, logging the fact.
%% @end
%%--------------------------------------------------------------------
-spec filter_shadowed_entries([T]) -> [T] when T :: tuple().
filter_shadowed_entries(StaticEntries) ->
    ProviderSubdomainLabels = dns_state:get_provider_subdomain_labels(),

    % check if there are any overlapping records
    lists:filter(fun(Entry) ->
        SubdomainLabel = element(1, Entry), % not all tuples are 2-element, eg. MX entries
        NormalizedSubdomainLabel = string:lowercase(SubdomainLabel),
        IsSubdomain = fun(Domain) -> dns_utils:is_equal_or_subdomain(NormalizedSubdomainLabel, Domain) end,

        case lists:any(IsSubdomain, ProviderSubdomainLabels) of
            false ->
                true;
            _ ->
                ?warning("Ignoring static entry for subdomain \"~ts\" "
                "as the domain is already used by a provider.", [SubdomainLabel]),
                false
        end
    end, StaticEntries).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a dns A record for erldns.
%% @end
%%--------------------------------------------------------------------
-spec build_record_a(dns_utils:domain_name(), inet:ip4_address()) -> #dns_rr{}.
build_record_a(Domain, IP) ->
    #dns_rr{
        name = Domain,
        type = ?DNS_TYPE_A,
        ttl = oz_worker:get_env(dns_a_ttl, 120),
        data = #dns_rrdata_a{ip = IP}
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a dns SOA record for erldns.
%% @end
%%--------------------------------------------------------------------
-spec build_record_soa(Name :: dns_utils:domain_name(), MainName :: dns_utils:domain_name(),
    Admin :: binary(), serial()) -> #dns_rr{}.
build_record_soa(Name, MainName, Admin, Serial) ->
    #dns_rr{
        name = Name,
        type = ?DNS_TYPE_SOA,
        ttl = oz_worker:get_env(dns_soa_ttl, 120),
        data = #dns_rrdata_soa{
            mname = MainName,
            rname = Admin,
            serial = Serial,
            refresh = oz_worker:get_env(dns_soa_refresh, 7200),
            retry = oz_worker:get_env(dns_soa_retry, 1800),
            expire = oz_worker:get_env(dns_soa_expire, 1209600),
            minimum = oz_worker:get_env(dns_soa_minimum, 120)
       }
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a NS record for erldns.
%% @end
%%--------------------------------------------------------------------
-spec build_record_ns(Name :: dns_utils:domain_name(), Nameserver :: dns_utils:domain_name()) -> #dns_rr{}.
build_record_ns(Name, Nameserver) ->
    #dns_rr{
        name = Name,
        type = ?DNS_TYPE_NS,
        ttl = oz_worker:get_env(dns_ns_ttl, 120),
        data = #dns_rrdata_ns{dname = Nameserver}
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a TXT record for erldns. Binary content will be converted
%% to string (list).
%% @end
%%--------------------------------------------------------------------
-spec build_record_txt(dns_utils:domain_name(), binary() | string()) -> #dns_rr{}.
build_record_txt(Domain, Value) ->
    build_record_txt(Domain, Value, oz_worker:get_env(dns_txt_ttl, 120)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a TXT record for erldns with provided TTL.
%% Binary content will be converted to string (list) as required
%% by erl_dns.
%% @end
%%--------------------------------------------------------------------
-spec build_record_txt(Domain :: dns_utils:domain_name(), Value :: binary() | string(),
    TTL :: time:seconds()) -> #dns_rr{}.
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
-spec build_record_mx(dns_utils:domain_name(), dns_utils:domain_name(), integer()) -> #dns_rr{}.
build_record_mx(Domain, Address, Preference) ->
    #dns_rr{
        name = Domain,
        type = ?DNS_TYPE_MX,
        ttl = oz_worker:get_env(dns_mx_ttl, 120),
        data = #dns_rrdata_mx{exchange = Address, preference = Preference}
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a CNAME record for erldns.
%% @end
%%--------------------------------------------------------------------
-spec build_record_cname(dns_utils:domain_name(), dns_utils:domain_name()) -> #dns_rr{}.
build_record_cname(Name, Target) ->
    #dns_rr{
        name = Name,
        type = ?DNS_TYPE_CNAME,
        ttl = oz_worker:get_env(dns_cname_ttl, 120),
        data = #dns_rrdata_cname{dname = Target}
    }.
