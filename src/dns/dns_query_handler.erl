%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This module is responsible for handling DNS queries.
%%% @end
%%%-------------------------------------------------------------------
-module(dns_query_handler).

%%-include_lib("kernel/src/inet_dns.hrl").

%%
%% Currently defined opcodes
%%
-define(QUERY,    16#0).          %% standard query
-define(IQUERY,   16#1).	      %% inverse query
-define(STATUS,   16#2).	      %% nameserver status query
%% -define(xxx,   16#3)  %% 16#3 reserved
%%  non standard
-define(UPDATEA,  16#9).	       %% add resource record
-define(UPDATED,  16#a).	       %% delete a specific resource record
-define(UPDATEDA, 16#b).	       %% delete all nemed resource record
-define(UPDATEM,  16#c).	       %% modify a specific resource record
-define(UPDATEMA, 16#d).	       %% modify all named resource record

-define(ZONEINIT, 16#e).	       %% initial zone transfer
-define(ZONEREF,  16#f).	       %% incremental zone referesh


%%
%% Currently defined response codes
%%
-define(NOERROR,  0).		%% no error
-define(FORMERR,  1).		%% format error
-define(SERVFAIL, 2).		%% server failure
-define(NXDOMAIN, 3).		%% non existent domain
-define(NOTIMP,	  4).		%% not implemented
-define(REFUSED,  5).		%% query refused
%%	non standard
-define(NOCHANGE, 16#f).		%% update failed to change db
-define(BADVERS,  16).

%%
%% Type values for resources and queries
%%
-define(T_A,		1).		%% host address
-define(T_NS,		2).		%% authoritative server
-define(T_MD,		3).		%% mail destination
-define(T_MF,		4).		%% mail forwarder
-define(T_CNAME,	5).		%% connonical name
-define(T_SOA,		6).		%% start of authority zone
-define(T_MB,		7).		%% mailbox domain name
-define(T_MG,		8).		%% mail group member
-define(T_MR,		9).		%% mail rename name
-define(T_NULL,		10).		%% null resource record
-define(T_WKS,		11).		%% well known service
-define(T_PTR,		12).		%% domain name pointer
-define(T_HINFO,	13).		%% host information
-define(T_MINFO,	14).		%% mailbox information
-define(T_MX,		15).		%% mail routing information
-define(T_TXT,		16).		%% text strings
-define(T_AAAA,         28).            %% ipv6 address
%% SRV (RFC 2052)
-define(T_SRV,          33).            %% services
%% NAPTR (RFC 2915)
-define(T_NAPTR,        35).            %% naming authority pointer
-define(T_OPT,          41).            %% EDNS pseudo-rr RFC2671(7)
%% SPF (RFC 4408)
-define(T_SPF,          99).            %% server policy framework
%%      non standard
-define(T_UINFO,	100).		%% user (finger) information
-define(T_UID,		101).		%% user ID
-define(T_GID,		102).		%% group ID
-define(T_UNSPEC,	103).		%% Unspecified format (binary data)
%%	Query type values which do not appear in resource records
-define(T_AXFR,		252).		%% transfer zone of authority
-define(T_MAILB,	253).		%% transfer mailbox records
-define(T_MAILA,	254).		%% transfer mail agent records
-define(T_ANY,		255).		%% wildcard match

%%
%% Symbolic Type values for resources and queries
%%
-define(S_A,		a).		%% host address
-define(S_NS,		ns).		%% authoritative server
-define(S_MD,		md).		%% mail destination
-define(S_MF,		mf).		%% mail forwarder
-define(S_CNAME,	cname).		%% connonical name
-define(S_SOA,		soa).		%% start of authority zone
-define(S_MB,		mb).		%% mailbox domain name
-define(S_MG,		mg).		%% mail group member
-define(S_MR,		mr).		%% mail rename name
-define(S_NULL,		null).		%% null resource record
-define(S_WKS,		wks).		%% well known service
-define(S_PTR,		ptr).		%% domain name pointer
-define(S_HINFO,	hinfo).		%% host information
-define(S_MINFO,	minfo).		%% mailbox information
-define(S_MX,		mx).		%% mail routing information
-define(S_TXT,		txt).		%% text strings
-define(S_AAAA,         aaaa).          %% ipv6 address
%% SRV (RFC 2052)
-define(S_SRV,          srv).           %% services
%% NAPTR (RFC 2915)
-define(S_NAPTR,        naptr).         %% naming authority pointer
-define(S_OPT,          opt).           %% EDNS pseudo-rr RFC2671(7)
%% SPF (RFC 4408)
-define(S_SPF,          spf).           %% server policy framework
%%      non standard
-define(S_UINFO,	uinfo).		%% user (finger) information
-define(S_UID,		uid).		%% user ID
-define(S_GID,		gid).		%% group ID
-define(S_UNSPEC,	unspec).        %% Unspecified format (binary data)
%%	Query type values which do not appear in resource records
-define(S_AXFR,		axfr).		%% transfer zone of authority
-define(S_MAILB,	mailb).		%% transfer mailbox records
-define(S_MAILA,	maila).		%% transfer mail agent records
-define(S_ANY,		any).		%% wildcard match

%%
%% Values for class field
%%

-define(C_IN,		1).      	%% the arpa internet
-define(C_CHAOS,	3).		%% for chaos net at MIT
-define(C_HS,		4).		%% for Hesiod name server at MIT
%%  Query class values which do not appear in resource records
-define(C_ANY,		255).		%% wildcard match


%% indirection mask for compressed domain names
-define(INDIR_MASK, 16#c0).

%%
%% Structure for query header, the order of the fields is machine and
%% compiler dependent, in our case, the bits within a byte are assignd
%% least significant first, while the order of transmition is most
%% significant first.  This requires a somewhat confusing rearrangement.
%%
-record(dns_header,
{
    id = 0,       %% ushort query identification number
    %% byte F0
    qr = 0,       %% :1   response flag
    opcode = 0,   %% :4   purpose of message
    aa = 0,       %% :1   authoritive answer
    tc = 0,       %% :1   truncated message
    rd = 0,       %% :1   recursion desired
    %% byte F1
    ra = 0,       %% :1   recursion available
    pr = 0,       %% :1   primary server required (non standard)
    %% :2   unused bits
    rcode = 0     %% :4   response code
}).

-record(dns_rec,
{
    header,       %% dns_header record
    qdlist = [],  %% list of question entries
    anlist = [],  %% list of answer entries
    nslist = [],  %% list of authority entries
    arlist = []   %% list of resource entries
}).

%% DNS resource record
-record(dns_rr,
{
    domain = "",   %% resource domain
    type = any,    %% resource type
    class = in,    %% reource class
    cnt = 0,       %% access count
    ttl = 0,       %% time to live
    data = [],     %% raw data
    %%
    tm,            %% creation time
    bm = [],       %% Bitmap storing domain character case information.
    func = false   %% Optional function calculating the data field.
}).

-define(DNS_UDP_PAYLOAD_SIZE, 1280).

-record(dns_rr_opt,           %% EDNS RR OPT (RFC2671), dns_rr{type=opt}
{
    domain = "",        %% should be the root domain
    type = opt,
    udp_payload_size = ?DNS_UDP_PAYLOAD_SIZE, %% RFC2671(4.5 CLASS)
    ext_rcode = 0,      %% RFC2671(4.6 EXTENDED-RCODE)
    version = 0,        %% RFC2671(4.6 VERSION)
    z = 0,              %% RFC2671(4.6 Z)
    data = []           %% RFC2671(4.4)
}).

-record(dns_query,
{
    domain,     %% query domain
    type,        %% query type
    class      %% query class
}).


































-include_lib("ctool/include/logging.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").

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
    application:set_env(?APP_Name, dns_zones, DNSConfig).


%%--------------------------------------------------------------------
%% @doc Returns canonical hostname as specified in dns.config.
%% @end
%%--------------------------------------------------------------------
-spec get_canonical_hostname() -> string().
get_canonical_hostname() ->
    {ok, [#dns_zone{cname = Hostname} | _]} = application:get_env(?APP_Name, dns_zones),
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
                    handle_unknown_subdomain(DomainNotNormalized, Prefix, DNSZone, LBAdvice);
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
                    handle_unknown_subdomain(DomainNotNormalized, Prefix, DNSZone, LBAdvice)
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
    {ok, DNSZones} = application:get_env(?APP_Name, dns_zones),
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
-spec handle_unknown_subdomain(Domain :: string(), Prefix :: string(), DNSZone :: #dns_zone{}, LBAdvice :: term()) ->
    dns_worker_plugin_behaviour:handler_reply().
handle_unknown_subdomain(Domain, PrefixStr, DNSZone, LBAdvice) ->
    try
        Prefix = list_to_binary(PrefixStr),
        case get_provider_data_by_alias(Prefix) of
            {ok, DataProplist} ->
                GRDomain = get_canonical_hostname(),
                RedPoint = binary_to_list(proplists:get_value(redirectionPoint, DataProplist)),
                % Check if provider is in the same domain - then return NS
                % It not, return A records of GR servers and then make a HTTP
                % redirect.
                case string:rstr(RedPoint, GRDomain) of
                    0 ->
                        #dns_zone{ip_addresses = IPAddresses, ttl_a = TTL} = DNSZone,
                        IPAddrList = proplists:get_value(GRDomain, IPAddresses),
                        FinalIPAddrList = account_lb(IPAddrList, LBAdvice),
                        ?dump({ok,
                                [dns_server:answer_record(Domain, TTL, ?S_A, IPAddress) || IPAddress <- FinalIPAddrList] ++
                                [dns_server:authoritative_answer_flag(true)]
                        }),
                        {ok,
                                [dns_server:answer_record(Domain, TTL, ?S_A, IPAddress) || IPAddress <- FinalIPAddrList] ++
                                [dns_server:authoritative_answer_flag(true)]
                        };
                    _ ->
                        #dns_zone{ttl_oneprovider_ns = TTL} = DNSZone,
                        {ok, {_Scheme, _UserInfo, HostStr, _Port, _Path, _Query}} = http_uri:parse(str_utils:to_list(RedPoint)),
                        {ok,
                            [dns_server:answer_record(Domain, TTL, ?S_NS, HostStr)]
                        }
                end;
            _ ->
                answer_with_soa(Domain, DNSZone)
        end
    catch _:_ ->
        answer_with_soa(Domain, DNSZone)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Returns records that will be evaluated as uthoritative SOA record
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


get_provider_data_by_alias(Alias) ->
    GetUserResult = case Alias of
        <<?NO_ALIAS_UUID_PREFIX, UUID/binary>> ->
            user_logic:get_user_doc(UUID);
        _ ->
            case user_logic:get_user_doc({alias, Alias}) of
                {ok, Ans} ->
                    {ok, Ans};
                _ ->
                    user_logic:get_user_doc(Alias)
            end
    end,
    case GetUserResult of
        {ok, #document{key = UserID, value = #onedata_user{chosen_provider = ChosenProvider}}} ->
            % If default provider is not known, set it.
            DataProplist =
                try
                    {ok, Data} = provider_logic:get_data(ChosenProvider),
                    Data
                catch _:_ ->
                    {ok, NewChosenProv} =
                        provider_logic:choose_provider_for_user(UserID),
                    ok = user_logic:modify(UserID, [{chosen_provider, NewChosenProv}]),
                    {ok, Data2} = provider_logic:get_data(NewChosenProv),
                    Data2
                end,
            {ok, DataProplist};
        _ ->
            error
    end.




%% % TODO this is a temporary solution to always return A records of provider when
%% % the DNS is asked for address of alias.onedata.org
%% return_providers_a_records(Domain, PrefixStr, #dns_zone{ttl_oneprovider_ns = TTL} = DNSZone) ->
%%     try
%%         Prefix = list_to_binary(PrefixStr),
%%         GetUserResult = case Prefix of
%%                             <<?NO_ALIAS_UUID_PREFIX, UUID/binary>> ->
%%                                 user_logic:get_user(UUID);
%%                             _ ->
%%                                 case user_logic:get_user({alias, Prefix}) of
%%                                     {ok, Ans} ->
%%                                         {ok, Ans};
%%                                     _ ->
%%                                         user_logic:get_user(Prefix)
%%                                 end
%%                         end,
%%         case GetUserResult of
%%             {ok, #user{chosen_provider = DefaulfProvider}} ->
%%                 {ok, DataProplist} = provider_logic:get_data(DefaulfProvider),
%%                 URLs = proplists:get_value(urls, DataProplist),
%%                 IPAddrList = [begin {ok, IP} = inet_parse:ipv4_address(binary_to_list(IPBin)), IP end || IPBin <- URLs],
%%                 {ok,
%%                         [dns_server:answer_record(Domain, TTL, ?S_A, IPAddress) || IPAddress <- IPAddrList] ++
%%                         [dns_server:authoritative_answer_flag(true)]
%%                 };
%%             _ ->
%%                 answer_with_soa(Domain, DNSZone)
%%         end
%%     catch _:_ ->
%%         answer_with_soa(Domain, DNSZone)
%%     end.
%%
%%
%% % TODO this is a temporary solution, returns GR's NS addresses
%% return_oz_nameservers(DomainNotNormalized, #dns_zone{ip_addresses = IPAddresses, ns_servers = NSServers, ttl_ns = TTLNS, ttl_a = TTLA}) ->
%%     {ok,
%%             [dns_server:answer_record(DomainNotNormalized, TTLNS, ?S_NS, NSHostname) || NSHostname <- NSServers] ++
%%             lists:flatten([begin
%%                                IPAddrList = proplists:get_value(NSHostname, IPAddresses, []),
%%                                [dns_server:additional_record(NSHostname, TTLA, ?S_A, IPAddress) || IPAddress <- IPAddrList]
%%                            end || NSHostname <- NSServers]) ++
%%             [dns_server:authoritative_answer_flag(true)]
%%     }.