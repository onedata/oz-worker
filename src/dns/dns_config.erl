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

-export([build_config/1, build_config/0, insert_config/1]).
-export([build_domain/2, build_fqdn_from_subdomain/1]).

-type domain() :: binary().
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
build_domain(Subdomain, Domain) ->
    <<Subdomain/binary, ".", Domain/binary>>.


%%--------------------------------------------------------------------
%% @doc
%% Joins provided subdomain with onezone domain.
%% @end
%%--------------------------------------------------------------------
-spec build_fqdn_from_subdomain(Subdomain :: domain()) -> domain().
build_fqdn_from_subdomain(Subdomain) ->
    build_domain(Subdomain, get_onezone_domain()).


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
%% Builds dns config with up to date data.
%% @end
%%--------------------------------------------------------------------
-spec build_config() -> dns_config().
build_config() ->
    build_config(get_onezone_ips()).


%%--------------------------------------------------------------------
%% @doc
%% Builds dns config with up to date data, using provided IPs
%% for OneZone.
%% @end
%%--------------------------------------------------------------------
-spec build_config(OneZoneIPs :: [inet:ip4_address()]) -> dns_config().
build_config(OneZoneIPs) ->
    OneZoneDomain = get_onezone_domain(),
    AdminEmail = get_app_config(soa_admin_mailbox),

    NSDomainsIPs = build_nameserver_domains(OneZoneDomain, OneZoneIPs),
    {NSDomains, _} = lists:unzip(NSDomainsIPs),
    PrimaryNS = hd(NSDomains),

    ProviderDomains = get_provider_domains(OneZoneDomain),

    % check if there are any overlapping records
    StaticDomains = lists:filter(fun({Domain, IP}) ->
        case lists:keyfind(Domain, 1, ProviderDomains) of
            false -> true;
            _ ->
                ?warning("Skipping static subdomain entry for domain ~s "
                "as the subdomain is already used by a provider.", [Domain]),
                false
        end
    end, build_static_domains(OneZoneDomain)),

    DomainsToIPS = [{OneZoneDomain, OneZoneIPs}
                   | StaticDomains
                   ++ NSDomainsIPs
                   ++ ProviderDomains],

    ARecords = lists:flatmap(fun({Domain, IPs}) ->
        [build_record_a(Domain, IP) || IP <- IPs]
    end, DomainsToIPS),
    NSRecords = [build_record_ns(OneZoneDomain, NSHost) || NSHost <- NSDomains],
    SOARecord = build_record_soa(OneZoneDomain, AdminEmail, PrimaryNS),

    {OneZoneDomain, <<>>, [SOARecord | NSRecords ++ ARecords]}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns IPs of all OneZone nodes.
%% @end
%%--------------------------------------------------------------------
-spec get_onezone_ips() -> [inet:ip4_address()].
get_onezone_ips() ->
    {ok, NodesIPs} = node_manager:get_cluster_nodes_ips(),
    {_, IPs} = lists:unzip(NodesIPs),
    IPs.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns OneZone domain.
%% @end
%%--------------------------------------------------------------------
-spec get_onezone_domain() -> domain().
get_onezone_domain() ->
    {ok, Host} = application:get_env(?APP_NAME, http_domain),
    list_to_binary(Host).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns all domains for providers who requested a subdomain from onezone.
%% Each provider's domain is mapped to a list of its IPs.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_domains(OneZoneDomain :: domain()) ->
    [{domain(), [inet:ip4_address()]}].
get_provider_domains(OneZoneDomain) ->
    SubdomainsIPs = dns_state:get_subdomains_to_ips(),
    [{build_domain(Subdomain, OneZoneDomain), IPs}
     || {Subdomain, IPs} <- SubdomainsIPs].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generate domains for subdomains "ns1", "ns2" etc. in the onezone
%% domain to be used as nameserver addresses.
%% @end
%%--------------------------------------------------------------------
-spec build_nameserver_domains(OneZoneDomain :: domain(),
    OneZoneIPs :: [inet:ip4_address()]) ->
    [{domain(), [inet:ip4_address()]}].
build_nameserver_domains(OneZoneDomain, OneZoneIPs) ->
    NSIPs = lists:sublist(lists:sort(OneZoneIPs), get_app_config(ns_limit, 10)),

    {NSDomainsIPs, _} = lists:foldl(fun(IP, {DomainsIPs, Count}) ->
        Index = integer_to_binary(Count),
        Domain = build_domain(<<"ns", Index/binary>>, OneZoneDomain),
        {[{Domain, [IP]} | DomainsIPs], Count + 1}
    end, {[], 1}, NSIPs),

    % preserve ascending order
    lists:reverse(NSDomainsIPs).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates domains based on subdomains given in the configuration file.
%% @end
%%--------------------------------------------------------------------
-spec build_static_domains(OneZoneDomain :: domain()) ->
    [{domain(), [inet:ip4_address()]}].
build_static_domains(OneZoneDomain) ->
    Subdomains = get_app_config(static_entries, []),
    [{build_domain(Sub, OneZoneDomain), IPs} || {Sub, IPs} <- Subdomains].


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
        ttl = get_app_config(a_ttl),
        data = #dns_rrdata_a{ip = IP}
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a dns SOA record for erldns.
%% @end
%%--------------------------------------------------------------------
-spec build_record_soa(Domain :: domain(), MainName :: domain(),
    Admin :: binary()) -> #dns_rr{}.
build_record_soa(Domain, MainName, Admin) ->
    #dns_rr{
        name = Domain,
        type = ?DNS_TYPE_SOA,
        ttl = get_app_config(soa_ttl, 120),
        data = #dns_rrdata_soa{
            mname = MainName,
            rname = Admin,
            serial = get_app_config(soa_serial),
            refresh = get_app_config(soa_refresh),
            retry = get_app_config(soa_retry),
            expire = get_app_config(soa_expire),
            minimum = get_app_config(soa_minimum)
       }
    }.


-spec build_record_ns(domain(), domain()) -> #dns_rr{}.
build_record_ns(Domain, NSDomain) ->
    #dns_rr{
        name = Domain,
        type = ?DNS_TYPE_NS,
        ttl = get_app_config(ns_ttl, 120),
        data = #dns_rrdata_ns{dname = NSDomain}
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves value from dns config. Throw error if config is not set.
%% @end
%%--------------------------------------------------------------------
-spec get_app_config(Key :: atom()) -> term().
get_app_config(Key) ->
    Props = application:get_env(?APP_NAME, dns, []),
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
-spec get_app_config(Key :: atom(), Default :: term()) -> term().
get_app_config(Key, Default) ->
    Props = application:get_env(?APP_NAME, dns, []),
    proplists:get_value(Key, Props, Default).
