%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module implements dns_query_handler_behaviour and is responsible
%% for handling DNS queries.
%% @end
%% ===================================================================
-module(dns_query_handler).
-behaviour(dns_query_handler_behaviour).

-include_lib("ctool/include/dns/dns.hrl").
-include_lib("ctool/include/logging.hrl").
-include("dao/dao_types.hrl").
-include("registered_names.hrl").


%% DNS config handling
-export([load_config/0, load_config/1, parse_domain/1]).

%% dns_query_handler_behaviour API
-export([handle_a/1, handle_ns/1, handle_cname/1, handle_soa/1, handle_wks/1, handle_ptr/1, handle_hinfo/1, handle_minfo/1, handle_mx/1, handle_txt/1]).

-define(DEFAULT_DNS_CONFIG_LOCATION, "resources/dns.config").

% Record holding zone config. Config file is parsed into list of such records.
-record(dns_zone, {
    % Canonical domain name
    cname = "" :: string(),
    % IP addresses of servers in the domain. Must include NS and MX servers.
    ip_addresses = [] :: [{Hostname :: string(), Prefix :: string(), {A :: byte(), B :: byte(), C :: byte(), D :: byte()}}],
    % Hostnames of domain's name servers
    ns_servers = [] :: [string()],
    % Hosts handling mail exchange for the domain
    % Values are {<preference>, <hostname>}, lowest preference is preferred
    mail_exchange = [] :: [{Preference :: integer(), Hostname :: string()}],
    % Parameters used to generate SOA responses
    authority :: {MName :: string(), RName :: string(), Serial :: integer(),
        Refresh :: integer(), Retry :: integer(), Expiry :: integer(), Minimum :: integer()}
}).

%% load_config/0
%% ====================================================================
%% @doc Loads (or reloads) DNS config from default file.
%% @end
%% ====================================================================
-spec load_config() -> ok | no_return().
%% ====================================================================
load_config() ->
    load_config(?DEFAULT_DNS_CONFIG_LOCATION).


%% load_config/1
%% ====================================================================
%% @doc Loads (or reloads) DNS config from given file.
%% @end
%% ====================================================================
-spec load_config(ConfigFile :: string()) -> ok | no_return().
%% ====================================================================
load_config(ConfigFile) ->
    {ok, Config} = file:consult(ConfigFile),
    GetParam = fun(Param, Proplist) ->
        case proplists:get_value(Param, Proplist, undefined) of
            undefined -> throw({missing_param, Param});
            Val -> Val
        end
    end,
    DNSConfig = lists:map(
        fun({zone, ZoneProplist}) ->
            CName = GetParam(cname, ZoneProplist),
            % Extract prefixes from hostnames ("mx.onedata.org" -> "mx")
            % and convert addresses to DNS-compatible terms
            IPAddresses = lists:map(
                fun({Hostname, AddressesString}) ->
                    Addresses = lists:map(
                        fun(AddrString) ->
                            {ok, AddrAtoms} = inet_parse:ipv4_address(AddrString),
                            AddrAtoms
                        end, AddressesString),
                    Index = string:str(Hostname, CName),
                    % All hostnames must end with cname
                    case Index + length(CName) - 1 =:= length(Hostname) of
                        true ->
                            ok;
                        false ->
                            throw(wrong_hostname_in_ip_addresses)
                    end,
                    StrippedHostname = case Index of
                                           0 ->
                                               % All hostnames must end with cname
                                               throw(wrong_hostname_in_ip_addresses);
                                           1 ->
                                               % Hostname == cname
                                               "";
                                           _ ->
                                               string:sub_string(Hostname, 1, Index - 2)
                                       end,
                    {Hostname, StrippedHostname, Addresses}
                end, GetParam(ip_addresses, ZoneProplist)),
            AuthorityProplist = GetParam(authority, ZoneProplist),
            SOATuple = {
                GetParam(primary_ns, AuthorityProplist),
                GetParam(admin_mailbox, AuthorityProplist),
                GetParam(serial, AuthorityProplist),
                GetParam(refresh, AuthorityProplist),
                GetParam(retry, AuthorityProplist),
                GetParam(expiry, AuthorityProplist),
                GetParam(nxdomain_ttl, AuthorityProplist)
            },
            #dns_zone{
                cname = CName,
                ip_addresses = IPAddresses,
                ns_servers = GetParam(ns_servers, ZoneProplist),
                mail_exchange = GetParam(mail_exchange, ZoneProplist),
                authority = SOATuple
            }
        end, Config),
    application:set_env(?APP_Name, dns_zones, DNSConfig).


%% ===================================================================
%% dns_query_handler_behaviour API
%% ===================================================================

%% handle_a/1
%% ====================================================================
%% @doc Handles DNS queries of type A.
%% See {@link dns_query_handler_behaviour} for reference.
%% @end
%% ====================================================================
-spec handle_a(Domain :: string()) -> {ok, [Response]} | serv_fail | nx_domain | not_impl | refused
    when Response :: {A :: byte(), B :: byte(), C :: byte(), D :: byte()}.
%% ====================================================================
handle_a(Domain) ->
    case parse_domain(Domain) of
        nx_domain ->
            nx_domain;
        {Prefix, _Suffix, #dns_zone{ip_addresses = IPAddresses} = DNSZone} ->
            case lists:keyfind(Prefix, 2, IPAddresses) of
                {Domain, Prefix, IPAddrList} ->
                    {ok,
                        [dns_server:answer_record(Domain, ?S_A, IPAddress) || IPAddress <- IPAddrList] ++
                        [dns_server:authoritative_answer_flag(true)]
                    };
                _ ->
                    answer_with_soa(Domain, DNSZone)
            end
    end.


answer_with_soa(Domain, #dns_zone{authority = Authority}) ->
    {ok, [dns_server:authority_record(Domain, ?S_SOA, Authority)]}.


%% handle_ns/1
%% ====================================================================
%% @doc Handles DNS queries of type NS.
%% See {@link dns_query_handler_behaviour} for reference.
%% @end
%% ====================================================================
-spec handle_ns(Domain :: string()) -> {ok, [Response]} | serv_fail | nx_domain | not_impl | refused
    when Response :: string().
%% ====================================================================
handle_ns(Domain) ->
    case parse_domain(Domain) of
        nx_domain ->
            nx_domain;
        {Prefix, Suffix} ->
            if
                Prefix =:= "" ->
                    {ok, [
                        dns_server:answer_record(Domain, ?S_NS, Suffix),
                        dns_server:authoritative_answer_flag(true)
                    ]};
                Prefix =:= "www" ->
                    {ok, [
                        dns_server:answer_record(Domain, ?S_NS, Suffix),
                        dns_server:authoritative_answer_flag(true)
                    ]};
                true ->
                    maybe_answer_with_authority(Domain, Prefix)
            end
    end.


%% handle_cname/1
%% ====================================================================
%% @doc Handles DNS queries of type CNAME.
%% See {@link dns_query_handler_behaviour} for reference.
%% @end
%% ====================================================================
-spec handle_cname(Domain :: string()) -> {ok, [Response]} | serv_fail | nx_domain | not_impl | refused
    when Response :: string().
%% ====================================================================
handle_cname(Domain) ->
    case parse_domain(Domain) of
        nx_domain ->
            nx_domain;
        {Prefix, Suffix} ->
            if
                Prefix =:= "" ->
                    {ok, [
                        dns_server:answer_record(Domain, ?S_CNAME, Suffix),
                        dns_server:authoritative_answer_flag(true)
                    ]};
                Prefix =:= "www" ->
                    {ok, [
                        dns_server:answer_record(Domain, ?S_CNAME, Suffix),
                        dns_server:authoritative_answer_flag(true)
                    ]};
                true ->
                    maybe_answer_with_authority(Domain, Prefix)
            end
    end.


%% handle_mx/1
%% ====================================================================
%% @doc Handles DNS queries of type MX.
%% See {@link dns_query_handler_behaviour} for reference.
%% @end
%% ====================================================================
-spec handle_mx(Domain :: string()) -> {ok, [Response]} | serv_fail | nx_domain | not_impl | refused
    when Response :: {Pref :: integer(), Exch :: string()}.
%% ====================================================================
handle_mx(Domain) ->
    case parse_domain(Domain) of
        nx_domain ->
            nx_domain;
        {Prefix, Suffix} ->
            if
                Prefix =:= "" ->
                    {ok, [
                        dns_server:answer_record(Domain, ?S_MX, {0, Suffix}),
                        dns_server:authoritative_answer_flag(true)
                    ]};
                Prefix =:= "www" ->
                    {ok, [
                        dns_server:answer_record(Domain, ?S_MX, {0, Suffix}),
                        dns_server:authoritative_answer_flag(true)
                    ]};
                true ->
                    maybe_answer_with_authority(Domain, Prefix)
            end
    end.


%% handle_soa/1
%% ====================================================================
%% @doc Handles DNS queries of type SOA.
%% See {@link dns_query_handler_behaviour} for reference.
%% @end
%% ====================================================================
-spec handle_soa(Domain :: string()) -> {ok, [Response]} | serv_fail | nx_domain | not_impl | refused
    when Response :: {MName :: string(), RName :: string(), Serial :: integer(), Refresh :: integer(),
    Retry :: integer(), Expiry :: integer(), Minimum :: integer()}.
%% ====================================================================
handle_soa(_Domain) -> not_impl.


%% handle_wks/1
%% ====================================================================
%% @doc Handles DNS queries of type WKS.
%% See {@link dns_query_handler_behaviour} for reference.
%% @end
%% ====================================================================
-spec handle_wks(Domain :: string()) -> {ok, [Response]} | serv_fail | nx_domain | not_impl | refused
    when Response :: {{A :: byte(), B :: byte(), C :: byte(), D :: byte()}, Proto :: string(), BitMap :: string()}.
%% ====================================================================
handle_wks(_Domain) -> not_impl.


%% handle_ptr1
%% ====================================================================
%% @doc Handles DNS queries of type PTR.
%% See {@link dns_query_handler_behaviour} for reference.
%% @end
%% ====================================================================
-spec handle_ptr(Domain :: string()) -> {ok, [Response]} | serv_fail | nx_domain | not_impl | refused
    when Response :: string().
%% ====================================================================
handle_ptr(_Domain) -> not_impl.


%% handle_hinfo/1
%% ====================================================================
%% @doc Handles DNS queries of type HINFO.
%% See {@link dns_query_handler_behaviour} for reference.
%% @end
%% ====================================================================
-spec handle_hinfo(Domain :: string()) -> {ok, [Response]} | serv_fail | nx_domain | not_impl | refused
    when Response :: {CPU :: string(), OS :: string()}.
%% ====================================================================
handle_hinfo(_Domain) -> not_impl.


%% handle_minfo/1
%% ====================================================================
%% @doc Handles DNS queries of type MINFO.
%% See {@link dns_query_handler_behaviour} for reference.
%% @end
%% ====================================================================
-spec handle_minfo(Domain :: string()) -> {ok, [Response]} | serv_fail | nx_domain | not_impl | refused
    when Response :: {RM :: string(), EM :: string()}.
%% ====================================================================
handle_minfo(_Domain) -> not_impl.


%% handle_txt/1
%% ====================================================================
%% @doc Handles DNS queries of type TXT.
%% See {@link dns_query_handler_behaviour} for reference.
%% @end
%% ====================================================================
-spec handle_txt(Domain :: string()) -> {ok, [Response]} | serv_fail | nx_domain | not_impl | refused
    when Response :: [string()].
%% ====================================================================
handle_txt(_Domain) -> not_impl.


%% ===================================================================
%% Internal functions
%% ===================================================================

%% parse_domain/1
%% ====================================================================
%% @doc Split the domain name into prefix and suffix, where suffix matches the
%% canonical provider's hostname (retrieved from env). The split is made on the dot between prefix and suffix.
%% If that's not possible, returns nx_domain.
%% @end
%% ====================================================================
-spec parse_domain(Domain :: string()) -> {Prefix :: string(), Suffix :: string(), DNSZone :: #dns_zone{}} | nx_domain.
%% ====================================================================
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
            nx_domain;
        #dns_zone{cname = ProviderHostnameWithoutDot} ->
            case ProviderHostnameWithoutDot =:= Domain of
                true ->
                    {"", ProviderHostnameWithoutDot, MatchingZone};
                false ->
                    ProviderHostname = "." ++ ProviderHostnameWithoutDot,
                    HostNamePos = string:rstr(Domain, ProviderHostname),
                    % If hostname is at this position, it's a suffix (the string ends with it)
                    ValidHostNamePos = length(Domain) - length(ProviderHostname) + 1,
                    case (HostNamePos =:= ValidHostNamePos) andalso HostNamePos > 0 of
                        false ->
                            nx_domain;
                        true ->
                            {string:sub_string(Domain, 1, HostNamePos - 1), ProviderHostnameWithoutDot, MatchingZone}
                    end
            end

    end.


maybe_answer_with_authority(Domain, DomainPrefix) ->
    case user_logic:get_user_doc({alias, list_to_binary(DomainPrefix)}) of
        {ok, #db_document{uuid = UserIdString}} ->
            case get_provider_for_user(list_to_binary(UserIdString)) of
                {ok, ProviderHostname, _URL} ->
                    {ok, {_, _, IPString, _, _, _}} = http_uri:parse(binary_to_list(ProviderHostname)),
                    {ok, [
                        dns_server:authority_record(Domain, ?S_NS, IPString)
                    ]};
                _ ->
                    nx_domain
            end;
        _ ->
            nx_domain
    end.


get_provider_for_user(UserID) ->
    {ok, [{spaces, Spaces}, {default, DefaultSpace}]} = user_logic:get_spaces(UserID),
    {ok, [{providers, DSProviders}]} = case DefaultSpace of
                                           undefined -> {ok, [{providers, []}]};
                                           _ -> space_logic:get_providers(DefaultSpace, user) end,
    case DSProviders of
        List when length(List) > 0 ->
            % Default space has got some providers, random one
            {ProviderHostname, RedirectURL} = auth_logic:get_redirection_uri(
                UserID, lists:nth(crypto:rand_uniform(1, length(DSProviders) + 1), DSProviders)),
            {ok, ProviderHostname, RedirectURL};
        _ ->
            % Default space does not have a provider, look in other spaces
            ProviderIDs = lists:foldl(
                fun(Space, Acc) ->
                    {ok, [{providers, Providers}]} = space_logic:get_providers(Space, user),
                    Providers ++ Acc
                end, [], Spaces),

            case ProviderIDs of
                [] ->
                    % No provider for other spaces = nowhere to redirect
                    {error, no_provider};
                _ ->
                    % There are some providers for other spaces, redirect to a random provider
                    {ProviderHostname, RedirectURL} = auth_logic:get_redirection_uri(
                        UserID, lists:nth(crypto:rand_uniform(1, length(ProviderIDs) + 1), ProviderIDs)),
                    {ok, ProviderHostname, RedirectURL}
            end
    end.