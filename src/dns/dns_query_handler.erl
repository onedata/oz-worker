%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This module is responsible for handling DNS queries.
%%% @end
%%%-------------------------------------------------------------------
-module(dns_query_handler).

-include_lib("kernel/src/inet_dns.hrl").
-include_lib("ctool/include/logging.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("registered_names.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

%% DNS config handling
-export([load_config/0, load_config/1, get_canonical_hostname/0]).

%% dns_handler_behaviour API
-export([handle_a/2, handle_ns/2, handle_cname/2, handle_soa/2, handle_wks/2,
    handle_ptr/2, handle_hinfo/2, handle_minfo/2, handle_mx/2, handle_txt/2]).

%% Alias of all available GR workers
-define(ALL_WORKERS, "ALL").

% Record holding zone config. Config file is parsed into list of such records.
-record(dns_zone, {
    % Canonical domain name
    cname = "" :: string(),
    % IP addresses of servers in the domain. Must include NS and MX servers.
    ip_addresses = [] :: [{Hostname :: string(), Prefix :: string(),
        {A :: byte(), B :: byte(), C :: byte(), D :: byte()}}],
    % Hostnames of domain's name servers
    ns_servers = [] :: [string()],
    % Hosts handling mail exchange for the domain
    % Values are {<preference>, <hostname>}, lowest preference is preferred
    mail_exchange = [] :: [{Preference :: integer(), Hostname :: string()}],
    % Parameters used to generate SOA responses
    authority :: {MName :: string(), RName :: string(), Serial :: integer(),
        Refresh :: integer(), Retry :: integer(), Expiry :: integer(), Minimum :: integer()},
    % TTLs of different answers
    ttl_a = 0 :: integer(),
    ttl_ns = 0 :: integer(),
    ttl_soa = 0 :: integer(),
    ttl_mx = 0 :: integer(),
    ttl_oneprovider_ns = 0 :: integer(),
    ttl_oneprovider_a = 0 :: integer()
}).

%%%===================================================================
%%% DNS config handling
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Loads (or reloads) DNS config from default file.
%% @end
%%--------------------------------------------------------------------
-spec load_config() -> ok | no_return().
load_config() ->
    {ok, ConfigFile} = application:get_env(cluster_worker, dns_config_file),
    load_config(ConfigFile).


%%--------------------------------------------------------------------
%% @doc Loads (or reloads) DNS config from given file.
%% @end
%%--------------------------------------------------------------------
-spec load_config(ConfigFile :: string()) -> ok | no_return().
load_config(ConfigFile) ->
    {ok, Config} = file:consult(ConfigFile),
    GetProp = fun(Param, Proplist) ->
        case proplists:get_value(Param, Proplist, undefined) of
            undefined -> throw({missing_param, Param});
            Val -> Val
        end
    end,
    DNSConfig = lists:map(
        fun({zone, ZoneProplist}) ->
            CName = GetProp(cname, ZoneProplist),
            % Extract prefixes from hostnames ("ns.onedata.org" -> "ns")
            % and convert addresses to DNS-compatible terms
            IPAddresses = lists:map(
                fun({Hostname, AddressesString}) ->
                    Addresses = lists:map(
                        fun
                            (?ALL_WORKERS) -> ?ALL_WORKERS;
                            (AddrString) ->
                                {ok, AddrAtoms} = inet_parse:ipv4_address(AddrString),
                                AddrAtoms
                        end, AddressesString),
                    Index = string:str(Hostname, CName),
                    % All hostnames must end with cname
                    case Index > 0 andalso Index + length(CName) - 1 =:= length(Hostname) of
                        false -> throw(wrong_hostname_in_ip_addresses);
                        true -> ok
                    end,
                    {Hostname, Addresses}
                end, GetProp(ip_addresses, ZoneProplist)),
            AuthorityProplist = GetProp(authority, ZoneProplist),
            SOATuple = {
                GetProp(primary_ns, AuthorityProplist),
                GetProp(admin_mailbox, AuthorityProplist),
                GetProp(serial, AuthorityProplist),
                GetProp(refresh, AuthorityProplist),
                GetProp(retry, AuthorityProplist),
                GetProp(expiry, AuthorityProplist),
                GetProp(nxdomain_ttl, AuthorityProplist)
            },
            TTLProplist = GetProp(ttl, ZoneProplist),
            #dns_zone{
                cname = CName,
                ip_addresses = IPAddresses,
                ns_servers = GetProp(ns_servers, ZoneProplist),
                mail_exchange = GetProp(mail_exchange, ZoneProplist),
                authority = SOATuple,
                ttl_a = GetProp(a, TTLProplist),
                ttl_ns = GetProp(ns, TTLProplist),
                ttl_soa = GetProp(soa, TTLProplist),
                ttl_mx = GetProp(mx, TTLProplist),
                ttl_oneprovider_ns = GetProp(oneprovider_ns, TTLProplist),
                ttl_oneprovider_a = GetProp(oneprovider_a, TTLProplist)
            }
        end, Config),
    application:set_env(?APP_NAME, dns_zones, DNSConfig).


%%--------------------------------------------------------------------
%% @doc Returns canonical hostname as specified in dns.config.
%% @end
%%--------------------------------------------------------------------
-spec get_canonical_hostname() -> string().
get_canonical_hostname() ->
    {ok, [#dns_zone{cname = Hostname} | _]} = application:get_env(?APP_NAME, dns_zones),
    Hostname.


%%%===================================================================
%%% dns_handler_behaviour API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Handles DNS queries of type A.
%% See {@link dns_handler_behaviour} for reference.
%% @end
%%--------------------------------------------------------------------
-spec handle_a(DomainNotNormalized :: string(), LBAdvice :: term()) -> dns_worker_plugin_behaviour:handler_reply().
handle_a(DomainNotNormalized, LBAdvice) ->
    case parse_domain(DomainNotNormalized) of
        unknown_domain ->
            refused;
        {Domain, Prefix, #dns_zone{ip_addresses = IPAddresses, ttl_a = TTL} = DNSZone} ->
            case proplists:get_value(Domain, IPAddresses, undefined) of
                undefined ->
                    handle_unknown_subdomain(?S_A, DomainNotNormalized, Prefix, DNSZone, LBAdvice);
                IPAddrList ->
                    FinalIPAddrList = account_lb(IPAddrList, LBAdvice),
                    {ok,
                            [dns_server:answer_record(DomainNotNormalized, TTL, ?S_A, IPAddress) || IPAddress <- FinalIPAddrList] ++
                            [dns_server:authoritative_answer_flag(true)]
                    }
            end
    end.


%%--------------------------------------------------------------------
%% @doc Handles DNS queries of type NS.
%% See {@link dns_handler_behaviour} for reference.
%% @end
%%--------------------------------------------------------------------
-spec handle_ns(DomainNotNormalized :: string(), LBAdvice :: term()) -> dns_worker_plugin_behaviour:handler_reply().
handle_ns(DomainNotNormalized, LBAdvice) ->
    case parse_domain(DomainNotNormalized) of
        unknown_domain ->
            refused;
        {Domain, Prefix, #dns_zone{cname = CName, ip_addresses = IPAddresses, ns_servers = NSServers, ttl_ns = TTLNS, ttl_a = TTLA} = DNSZone} ->
            case Domain of
                CName ->
                    {ok,
                            [dns_server:answer_record(DomainNotNormalized, TTLNS, ?S_NS, NSHostname) || NSHostname <- NSServers] ++
                            lists:flatten([begin
                                IPAddrList = proplists:get_value(NSHostname, IPAddresses, []),
                                FinalIPAddrList = account_lb(IPAddrList, LBAdvice),
                                [dns_server:additional_record(NSHostname, TTLA, ?S_A, IPAddress) || IPAddress <- FinalIPAddrList]
                            end || NSHostname <- NSServers]) ++
                            [dns_server:authoritative_answer_flag(true)]
                    };
                _ ->
                    handle_unknown_subdomain(?S_NS, DomainNotNormalized, Prefix, DNSZone, LBAdvice)
            end
    end.


%%--------------------------------------------------------------------
%% @doc Handles DNS queries of type CNAME.
%% See {@link dns_handler_behaviour} for reference.
%% @end
%%--------------------------------------------------------------------
-spec handle_cname(DomainNotNormalized :: string(), LBAdvice :: term()) -> dns_worker_plugin_behaviour:handler_reply().
handle_cname(DomainNotNormalized, _) ->
    case parse_domain(DomainNotNormalized) of
        unknown_domain ->
            refused;
        {Domain, _Prefix, DNSZone} ->
            answer_with_soa(Domain, DNSZone)
    end.


%%--------------------------------------------------------------------
%% @doc Handles DNS queries of type MX.
%% See {@link dns_handler_behaviour} for reference.
%% @end
%%--------------------------------------------------------------------
-spec handle_mx(DomainNotNormalized :: string(), LBAdvice :: term()) -> dns_worker_plugin_behaviour:handler_reply().
handle_mx(DomainNotNormalized, LBAdvice) ->
    case parse_domain(DomainNotNormalized) of
        unknown_domain ->
            refused;
        {Domain, _Prefix, #dns_zone{cname = CName, ip_addresses = IPAddresses, mail_exchange = MXServers, ttl_ns = TTLNS, ttl_a = TTLA} = DNSZone} ->
            case Domain of
                CName ->
                    {ok,
                            [dns_server:answer_record(DomainNotNormalized, TTLNS, ?S_MX, {MXPriority, MXHostname}) || {MXPriority, MXHostname} <- MXServers] ++
                            lists:flatten([begin
                                IPAddrList = proplists:get_value(MXHostname, IPAddresses, []),
                                FinalIPAddrList = account_lb(IPAddrList, LBAdvice),
                                [dns_server:additional_record(MXHostname, TTLA, ?S_A, IPAddress) || IPAddress <- FinalIPAddrList]
                            end || {_MXPriority, MXHostname} <- MXServers]) ++
                            [dns_server:authoritative_answer_flag(true)]
                    };
                _ ->
                    answer_with_soa(Domain, DNSZone)
            end
    end.


%%--------------------------------------------------------------------
%% @doc Handles DNS queries of type SOA.
%% See {@link dns_handler_behaviour} for reference.
%% @end
%%--------------------------------------------------------------------
-spec handle_soa(DomainNotNormalized :: string(), LBAdvice :: term()) -> dns_worker_plugin_behaviour:handler_reply().
handle_soa(DomainNotNormalized, LBAdvice) ->
    case parse_domain(DomainNotNormalized) of
        unknown_domain ->
            refused;
        {Domain, _Prefix, #dns_zone{cname = CName, ip_addresses = IPAddresses, ns_servers = NSServers, authority = Authority,
            ttl_a = TTLA, ttl_ns = TTLNS, ttl_soa = TTLSOA} = DNSZone} ->
            case Domain of
                CName ->
                    {ok,
                            [dns_server:answer_record(DomainNotNormalized, TTLSOA, ?S_SOA, Authority)] ++
                            [dns_server:authority_record(DomainNotNormalized, TTLNS, ?S_NS, NSHostname) || NSHostname <- NSServers] ++
                            lists:flatten([begin
                                IPAddrList = proplists:get_value(NSHostname, IPAddresses, []),
                                FinalIPAddrList = account_lb(IPAddrList, LBAdvice),
                                [dns_server:additional_record(NSHostname, TTLA, ?S_A, IPAddress) || IPAddress <- FinalIPAddrList]
                            end || NSHostname <- NSServers]) ++
                            [dns_server:authoritative_answer_flag(true)]
                    };
                _ ->
                    answer_with_soa(Domain, DNSZone)
            end
    end.


%%--------------------------------------------------------------------
%% @doc Handles DNS queries of type WKS.
%% See {@link dns_handler_behaviour} for reference.
%% @end
%%--------------------------------------------------------------------
-spec handle_wks(DomainNotNormalized :: string(), LBAdvice :: term()) -> dns_worker_plugin_behaviour:handler_reply().
handle_wks(DomainNotNormalized, _) ->
    case parse_domain(DomainNotNormalized) of
        unknown_domain ->
            refused;
        {Domain, _Prefix, DNSZone} ->
            answer_with_soa(Domain, DNSZone)
    end.


%%--------------------------------------------------------------------
%% @doc Handles DNS queries of type PTR.
%% See {@link dns_handler_behaviour} for reference.
%% @end
%%--------------------------------------------------------------------
-spec handle_ptr(DomainNotNormalized :: string(), LBAdvice :: term()) -> dns_worker_plugin_behaviour:handler_reply().
handle_ptr(DomainNotNormalized, _LBAdvice) ->
    case parse_domain(DomainNotNormalized) of
        unknown_domain ->
            refused;
        {Domain, _Prefix, DNSZone} ->
            answer_with_soa(Domain, DNSZone)
    end.


%%--------------------------------------------------------------------
%% @doc Handles DNS queries of type HINFO.
%% See {@link dns_handler_behaviour} for reference.
%% @end
%%--------------------------------------------------------------------
-spec handle_hinfo(DomainNotNormalized :: string(), LBAdvice :: term()) -> dns_worker_plugin_behaviour:handler_reply().
handle_hinfo(DomainNotNormalized, _) ->
    case parse_domain(DomainNotNormalized) of
        unknown_domain ->
            refused;
        {Domain, _Prefix, DNSZone} ->
            answer_with_soa(Domain, DNSZone)
    end.


%%--------------------------------------------------------------------
%% @doc Handles DNS queries of type MINFO.
%% See {@link dns_handler_behaviour} for reference.
%% @end
%%--------------------------------------------------------------------
-spec handle_minfo(DomainNotNormalized :: string(), LBAdvice :: term()) -> dns_worker_plugin_behaviour:handler_reply().
handle_minfo(DomainNotNormalized, _) ->
    case parse_domain(DomainNotNormalized) of
        unknown_domain ->
            refused;
        {Domain, _Prefix, DNSZone} ->
            answer_with_soa(Domain, DNSZone)
    end.


%%--------------------------------------------------------------------
%% @doc Handles DNS queries of type TXT.
%% See {@link dns_handler_behaviour} for reference.
%% @end
%%--------------------------------------------------------------------
-spec handle_txt(DomainNotNormalized :: string(), LBAdvice :: term()) -> dns_worker_plugin_behaviour:handler_reply().
handle_txt(DomainNotNormalized, _) ->
    case parse_domain(DomainNotNormalized) of
        unknown_domain ->
            refused;
        {Domain, _Prefix, DNSZone} ->
            answer_with_soa(Domain, DNSZone)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Analyzes a domain name - normalizes it (deletes leading www), extracts subdomain prefix
%% and finds a matching zone from config, if existent.
%% @end
%%--------------------------------------------------------------------
-spec parse_domain(Domain :: string()) -> {NormalizedDomain :: string(),
    Prefix :: string(), Suffix :: string(), DNSZone :: #dns_zone{}} | unknown_domain.
parse_domain(DomainArg) ->
    % If requested domain starts with 'www.', ignore it
    Domain = case DomainArg of
        [$w, $w, $w, $. | Rest] -> Rest;
        Other -> Other
    end,
    {ok, DNSZones} = application:get_env(?APP_NAME, dns_zones),
    % Find first matching zone
    MatchingZone = lists:foldl(
        fun(#dns_zone{cname = CName} = CurrentZone, Acc) ->
            case Acc of
                #dns_zone{} = Zone ->
                    Zone;
                undefined ->
                    case string:str(Domain, CName) of
                        0 -> undefined;
                        _ -> CurrentZone
                    end
            end
        end, undefined, DNSZones),
    case MatchingZone of
        undefined ->
            unknown_domain;
        #dns_zone{cname = ProviderHostnameWithoutDot} ->
            case ProviderHostnameWithoutDot =:= Domain of
                true ->
                    {Domain, "", MatchingZone};
                false ->
                    ProviderHostname = "." ++ ProviderHostnameWithoutDot,
                    HostNamePos = string:rstr(Domain, ProviderHostname),
                    % If hostname is at this position, it's a suffix (the string ends with it)
                    ValidHostNamePos = length(Domain) - length(ProviderHostname) + 1,
                    case (HostNamePos =:= ValidHostNamePos) andalso HostNamePos > 0 of
                        false ->
                            unknown_domain;
                        true ->
                            {Domain, string:sub_string(Domain, 1, HostNamePos - 1), MatchingZone}
                    end
            end

    end.


%%--------------------------------------------------------------------
%% @private
%% @doc This function is called when a subdomain is not recognizable according to dns.config.
%% This happens when:
%% 1) The requested subdomain does not exist or shouldn't have occured in the request -> return SOA record
%% 2) The subdomain is in form of alias.onedata.org -> return NS servers of supporting provider
%% @end
%%--------------------------------------------------------------------
-spec handle_unknown_subdomain(QueryType :: dns_worker_plugin_behaviour:query_type(),
    Domain :: string(), Prefix :: string(), DNSZone :: #dns_zone{}, LBAdvice :: term()) ->
    dns_worker_plugin_behaviour:handler_reply().
handle_unknown_subdomain(QueryType, DomainNotNormalized, PrefixStr, DNSZone, LBAdvice) ->
    try
        Prefix = list_to_binary(PrefixStr),
        case get_redirection_point_by_alias(Prefix) of
            {ok, RedPointBin} ->
                GRDomain = get_canonical_hostname(),
                RedPoint = binary_to_list(RedPointBin),
                % Check if provider is in the same domain - then return NS
                % It not, return A or NS records of GR servers and then make a HTTP
                % redirect.
                case string:rstr(RedPoint, GRDomain) of
                    0 ->
                        % Depending if asking about A or NS, return OZ nameservers or workers
                        case QueryType of
                            ?S_NS ->
                                return_oz_nameservers(DomainNotNormalized, DNSZone, LBAdvice);
                            ?S_A ->
                                #dns_zone{ip_addresses = IPAddresses, ttl_a = TTL} = DNSZone,
                                IPAddrList = proplists:get_value(GRDomain, IPAddresses),
                                FinalIPAddrList = account_lb(IPAddrList, LBAdvice),
                                {ok,
                                        [dns_server:answer_record(DomainNotNormalized, TTL, ?S_A, IPAddress) || IPAddress <- FinalIPAddrList] ++
                                        [dns_server:authoritative_answer_flag(true)]
                                }
                        end;
                    _ ->
                        #dns_zone{ttl_oneprovider_ns = TTL} = DNSZone,
                        #{host := Host} = url_utils:parse(RedPoint),
                        HostStr = str_utils:to_list(Host),
                        {ok,
                            [dns_server:answer_record(DomainNotNormalized, TTL, ?S_NS, HostStr)]
                        }
                end;
            _ ->
                answer_with_soa(DomainNotNormalized, DNSZone)
        end
    catch _:_ ->
        answer_with_soa(DomainNotNormalized, DNSZone)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Returns records that will be evaluated as authoritative SOA record
%% in authority section of DNS response.
%% @end
%%--------------------------------------------------------------------
-spec answer_with_soa(Domain :: string(), DNSZone :: #dns_zone{}) -> dns_worker_plugin_behaviour:handler_reply().
answer_with_soa(Domain, #dns_zone{cname = CName, ip_addresses = IPAddresses, authority = Authority, ttl_soa = TTL}) ->
    ReplyType = case proplists:get_value(Domain, IPAddresses, undefined) of
        undefined ->
            nx_domain;
        _ ->
            ok
    end,
    {ReplyType, [
        dns_server:authority_record(CName, TTL, ?S_SOA, Authority),
        dns_server:authoritative_answer_flag(true)
    ]}.


%%--------------------------------------------------------------------
%% @private
%% @doc Returns IP list unchanged. If "ALL" nodes are requested, then
%% load balancing comes into play and IPs are returned according to it.
%% @end
%%--------------------------------------------------------------------
-spec account_lb(IPAddrList :: [string()]|[{A :: byte(), B :: byte(), C :: byte(), D :: byte()}], DSNAdvice :: term())
        -> [{A :: byte(), B :: byte(), C :: byte(), D :: byte()}].
account_lb(IPAddrList, LBAdvice) ->
    case IPAddrList of
        [?ALL_WORKERS] ->
            load_balancing:choose_nodes_for_dns(LBAdvice);
        _ ->
            IPAddrList
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Retrieves a provider for user by given alias and returns the
%% provider data or returns error when it is not possible.
%% @end
%%--------------------------------------------------------------------
-spec get_redirection_point_by_alias(Alias :: binary()) -> {ok, binary()} | error.
get_redirection_point_by_alias(Alias) ->
    GetUserResult = case Alias of
        <<?NO_ALIAS_UUID_PREFIX, UUID/binary>> ->
            od_user:get(UUID);
        _ ->
            case od_user:get_by_criterion({alias, Alias}) of
                {ok, Ans} ->
                    {ok, Ans};
                _ ->
                    od_user:get(Alias)
            end
    end,
    case GetUserResult of
        {ok, #document{key = UserId, value = #od_user{chosen_provider = ChosenProvider}}} ->
            % If default provider is not known, set it.
            RedirectionPoint =
                try
                    {ok, #od_provider{
                        redirection_point = RedPoint
                    }} = provider_logic:get(?ROOT, ChosenProvider),
                    RedPoint
                catch _:_ ->
                    {ok, NewChosenProv} =
                        provider_logic:choose_provider_for_user(UserId),
                    {ok, _} = od_user:update(UserId, fun(User = #od_user{}) ->
                        {ok, User#od_user{chosen_provider = NewChosenProv}}
                    end),
                    {ok, #od_provider{
                        redirection_point = RedPoint2
                    }} = provider_logic:get(?ROOT, NewChosenProv),
                    RedPoint2
                end,
            {ok, RedirectionPoint};
        _ ->
            error
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Returns all GR nameservers and their A records in additional section.
%% @end
%%--------------------------------------------------------------------
-spec return_oz_nameservers(Domain :: string(), DNSZone :: #dns_zone{}, LBAdvice :: term()) -> {ok, term()}.
return_oz_nameservers(DomainNotNormalized, #dns_zone{ip_addresses = IPAddresses, ns_servers = NSServers, ttl_ns = TTLNS, ttl_a = TTLA}, LBAdvice) ->
    {ok,
            [dns_server:answer_record(DomainNotNormalized, TTLNS, ?S_NS, NSHostname) || NSHostname <- NSServers] ++
            lists:flatten([begin
                IPAddrList = proplists:get_value(NSHostname, IPAddresses, []),
                FinalIPAddrList = account_lb(IPAddrList, LBAdvice),
                [dns_server:additional_record(NSHostname, TTLA, ?S_A, IPAddress) || IPAddress <- FinalIPAddrList]
            end || NSHostname <- NSServers]) ++
            [dns_server:authoritative_answer_flag(true)]
    }.