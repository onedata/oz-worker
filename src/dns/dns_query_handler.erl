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
-export([load_config/0, load_config/1]).

%% dns_query_handler_behaviour API
-export([handle_a/1, handle_ns/1, handle_cname/1, handle_soa/1, handle_wks/1, handle_ptr/1, handle_hinfo/1, handle_minfo/1, handle_mx/1, handle_txt/1]).

-define(DEFAULT_DNS_CONFIG_LOCATION, "resources/dns.config").


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
    application:set_env(?APP_Name, dns_config, Config).


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
    {ok, [{IPv4, _NetworkIP, _Mask} | _]} = inet:getif(),
    case parse_domain(Domain) of
        nx_domain ->
            nx_domain;
        {Prefix, _Suffix} ->
            if
                Prefix =:= "" ->
                    {ok, [
                        dns_server:answer_record(Domain, ?S_A, IPv4),
                        dns_server:authoritative_answer_flag(true)
                    ]};
                Prefix =:= "www" ->
                    {ok, [
                        dns_server:answer_record(Domain, ?S_A, IPv4),
                        dns_server:authoritative_answer_flag(true)
                    ]};
                true ->
                    maybe_answer_with_authority(Domain, Prefix)
            end
    end.


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
-spec parse_domain(Domain :: string()) -> {Prefix :: string(), Suffix :: string()} | nx_domain.
%% ====================================================================
parse_domain(Domain) ->
    {ok, ProviderHostnameWithoutDot} = application:get_env(?APP_Name, hostname),
    case ProviderHostnameWithoutDot =:= Domain of
        true ->
            {"", ProviderHostnameWithoutDot};
        false ->
            ProviderHostname = "." ++ ProviderHostnameWithoutDot,
            HostNamePos = string:rstr(Domain, ProviderHostname),
            % If hostname is at this position, it's a suffix (the string ends with it)
            ValidHostNamePos = length(Domain) - length(ProviderHostname) + 1,
            case (HostNamePos =:= ValidHostNamePos) andalso HostNamePos > 0 of
                false ->
                    nx_domain;
                true ->
                    {string:sub_string(Domain, 1, HostNamePos - 1), ProviderHostnameWithoutDot}
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