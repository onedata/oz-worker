%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Stores all information related to providers required to build up to
%%% date DNS configuration - provider IPs and subdomains.
%%% @end
%%%-------------------------------------------------------------------
-module(dns_state).
-author("Wojciech Geisler").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([set_delegation_config/3, get_delegation_config/1,
    remove_delegation_config/1, get_subdomains_to_ips/0]).
-export([set_txt_record/3, set_txt_record/4, get_txt_records/0, remove_txt_record/2]).

-export([get_dns_state/0]).


%% datastore_model callbacks
-export([get_record_version/0, upgrade_record/2, get_record_struct/1]).

-type id() :: binary().
-type record() :: #dns_state{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).

-type subdomain() :: binary().
-type ttl() :: non_neg_integer() | undefined.
-export_type([id/0, record/0, doc/0]).
-export_type([subdomain/0, ttl/0]).

-define(CTX, #{model => ?MODULE}).

-define(DNS_STATE_KEY, <<"dns_state_singleton">>).
-define(DNS_STATE_LOCK, dns_state).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Updates subdomain delegation config of given provider.
%% @end
%%--------------------------------------------------------------------
-spec set_delegation_config(ProviderId :: od_provider:id(),
    Subdomain :: subdomain(), IPs :: [inet:ip4_address()]) ->
    ok | {error, subdomain_exists}.
set_delegation_config(ProviderId, Subdomain, IPs) ->
    Result = case is_subdomain_reserved(Subdomain) of
        true ->
            ?info("Refusing to register provider subdomain ~s as it is reserved", [Subdomain]),
            {error, subdomain_exists};
        false ->
            Diff = fun(DnsState) ->
                StateOrError = case get_provider_by_subdomain(DnsState, Subdomain) of
                    {true, ProviderId} ->
                        DnsState; % subdomain is already set
                    {true, OtherProvider} ->
                        ?debug("Refusing to set provider's ~s subdomain to ~s as it is used by provider ~s",
                            [ProviderId, Subdomain, OtherProvider]),
                        {error, subdomain_exists};
                    false ->
                        % remove old subdomain of provider begin updated before setting new
                        DnsState2 = unset_subdomain(DnsState, ProviderId),
                        set_subdomain(DnsState2, ProviderId, Subdomain)
                end,
                case StateOrError of
                    {error, subdomain_exists} -> {error, subdomain_exists};
                    NewState -> {ok, set_ips(NewState, ProviderId, IPs)}
                end
            end,
            Default = set_ips(set_subdomain(#dns_state{}, ProviderId, Subdomain),
                ProviderId, IPs),
            update(Diff, Default)
    end,
    case Result of
        {ok, _} ->
            node_manager_plugin:reconcile_dns_config(),
            ok;
        {error, subdomain_exists} ->
            {error, subdomain_exists}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns information related to subdomain delegation of given provider.
%% @end
%%--------------------------------------------------------------------
-spec get_delegation_config(od_provider:id()) ->
    {ok, Subdomain :: subdomain(), IPs :: [inet:ip4_address()]} |
    {error, not_found}.
get_delegation_config(ProviderId) ->
    {ok, DnsState} = get_dns_state(),
    #dns_state{
        provider_to_subdomain = PtS,
        provider_to_ips = PtIPs} = DnsState,
    case maps:find(ProviderId, PtS) of
        {ok, Subdomain} ->
            {ok, Subdomain, maps:get(ProviderId, PtIPs, [])};
        error ->
            {error, not_found}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Sets txt record under given name in provider's subdomain.
%% Given provider mus have an associated subdomain, otherwise
%% error is returned.
%% @end
%%--------------------------------------------------------------------
-spec set_txt_record(od_provider:id(), Name :: binary(), Content :: binary()) ->
    ok | {error, no_subdomain}.
set_txt_record(ProviderId, Name, Content) ->
    set_txt_record(ProviderId, Name, Content, undefined).

-spec set_txt_record(od_provider:id(), Name :: binary(), Content :: binary(), TTL :: ttl()) ->
    ok | {error, no_subdomain}.
set_txt_record(ProviderId, Name, Content, TTL) ->
    Result = update(fun(DnsState) ->
        #dns_state{provider_to_subdomain = PtS} = DnsState,
        case maps:find(ProviderId, PtS) of
            {ok, _} ->
                {ok, set_txt_record(DnsState, ProviderId, Name, Content, TTL)};
            error ->
                {error, not_found}
        end
    end, set_txt_record(#dns_state{}, ProviderId, Name, Content, TTL)),

    case Result of
        {ok, _} ->
            node_manager_plugin:reconcile_dns_config(),
            ok;
        {error, not_found} ->
            % dns_state record does not exist or provider subdomain not found
            {error, no_subdomain}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Removes TXT record identified by a provider and record name.
%% @end
%%--------------------------------------------------------------------
-spec remove_txt_record(ProviderId :: od_provider:id(), Name :: binary()) -> ok.
remove_txt_record(ProviderId, Name) ->
    {ok, _} = update(fun(DnsState) ->
        #dns_state{provider_to_txt_records = PtTR} = DnsState,
        case maps:find(ProviderId, PtTR) of
            {ok, ProviderPtTR} -> {ok, DnsState#dns_state{
                provider_to_txt_records = PtTR#{
                    ProviderId => proplists:delete(Name, ProviderPtTR)
                }}};
            error -> ok
        end
    end, #dns_state{}),
    node_manager_plugin:reconcile_dns_config(),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Deletes all information about given provider.
%% @end
%%--------------------------------------------------------------------
-spec remove_delegation_config(od_provider:id()) -> ok.
remove_delegation_config(ProviderId) ->
    {ok, _} = update(fun(DnsState) ->
        DnsState2 = unset_subdomain(DnsState, ProviderId),
        DnsState3 = remove_txt_by_provider(DnsState2, ProviderId),
        {ok, unset_ips(DnsState3, ProviderId)}
    end, #dns_state{}),
    node_manager_plugin:reconcile_dns_config(),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Returns all provider subdomains and associated IPs.
%% @end
%%--------------------------------------------------------------------
-spec get_subdomains_to_ips() -> #{subdomain() => [inet:ip4_address()]}.
get_subdomains_to_ips() ->
    {ok, DnsState} = get_dns_state(),
    #dns_state{
        provider_to_ips = PtIPs,
        subdomain_to_provider = StP} = DnsState,
    maps:map(fun(_Subdomain, ProviderId) ->
        maps:get(ProviderId, PtIPs, [])
    end, StP).


%%--------------------------------------------------------------------
%% @doc
%% Returns all txt records, building their names using provider subdomains
%% @end
%%--------------------------------------------------------------------
-spec get_txt_records() ->
    [{Subdomain :: binary(), {Content :: binary(), TTL :: ttl()}}].
get_txt_records() ->
    {ok, DnsState} = get_dns_state(),
    #dns_state{
        provider_to_subdomain = PtS,
        provider_to_txt_records = PtTR} = DnsState,
    lists:flatmap(fun({ProviderId, Records}) ->
        ProviderSubdomain = maps:get(ProviderId, PtS),
        [{<<Name/binary, $., ProviderSubdomain/binary>>, {Content, TTL}}
            || {Name, Content, TTL} <- Records]
    end, maps:to_list(PtTR)).

%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Returns model's record version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_version() -> datastore_model:record_version().
get_record_version() ->
    2.


%%--------------------------------------------------------------------
%% @doc
%% Returns model's record structure in provided version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_struct(datastore_model:record_version()) ->
    datastore_model:record_struct().
get_record_struct(1) ->
    {record, [
        {subdomain_to_provider, #{string => string}},
        {provider_to_subdomain, #{string => string}},
        {provider_to_ips, #{string => [{integer, integer, integer, integer}]}},
        {provider_to_txt_records, #{string => [{string, string}]}}
    ]};
get_record_struct(2) ->
    {record, [
        {subdomain_to_provider, #{string => string}},
        {provider_to_subdomain, #{string => string}},
        {provider_to_ips, #{string => [{integer, integer, integer, integer}]}},
        {provider_to_txt_records, #{string => [{string, string, integer}]}}
    ]}.


%%--------------------------------------------------------------------
%% @doc
%% Upgrades model's record from provided version to the next one.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_record(datastore_model:record_version(), datastore_model:record()) ->
    {datastore_model:record_version(), datastore_model:record()}.
upgrade_record(1, DnsState) ->
    {
        dns_state,
        SubdomainToProvider,
        ProviderToSubdomain,
        ProviderToIPS,
        ProviderToTxt
    } = DnsState,
    {2, {
        dns_state,
        SubdomainToProvider,
        ProviderToSubdomain,
        ProviderToIPS,
        maps:map(fun(_Provider, TxtRecords) ->
            [{Name, Content, undefined} || {Name, Content} <- TxtRecords]
        end, ProviderToTxt)
    }}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if subdomain is reserved for a static entry or nameserver.
%% @end
%%--------------------------------------------------------------------
-spec is_subdomain_reserved(subdomain()) -> boolean().
is_subdomain_reserved(Subdomain) ->
    {ok, DnsConf} = oz_worker:get_env(dns),

    % Get all reserved values
    Static = lists:flatmap(fun(Config) ->
        proplists:get_keys(proplists:get_value(Config, DnsConf, []))
    end, [static_a_records, static_ns_records, static_cname_records]),
    Static2 = lists:map(fun({_Name, Value, _Preference}) ->
        Value
    end, proplists:get_value(static_mx_records, DnsConf, [])) ++ Static,

    % subdomains "ns" or "nsX" where X is a number are reserved for nameserver.
    lists:member(Subdomain, Static2) orelse
        match == re:run(Subdomain, <<"^ns[0-9]*$">>, [{capture, none}]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sets provider subdomain in the dns state record.
%% @end
%%--------------------------------------------------------------------
-spec set_subdomain(record(), od_provider:id(), subdomain()) -> record().
set_subdomain(DnsState, ProviderId, Subdomain) ->
    #dns_state{
        provider_to_subdomain = PtS,
        subdomain_to_provider = StP} = DnsState,
    NewStP = case maps:find(ProviderId, PtS) of
        {ok, OldSubdomain} -> maps:remove(OldSubdomain, StP);
        error -> StP
    end,
    DnsState#dns_state{
        provider_to_subdomain = PtS#{ProviderId => Subdomain},
        subdomain_to_provider = NewStP#{Subdomain => ProviderId}
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sets provider IPs in the dns state record.
%% @end
%%--------------------------------------------------------------------
-spec set_ips(record(), od_provider:id(), [inet:ip4_address()]) ->
    record().
set_ips(#dns_state{provider_to_ips = PtIPs} = DnsState, ProviderId, IPs) ->
    DnsState#dns_state{provider_to_ips = PtIPs#{ProviderId => IPs}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes txt records of given provider from dns state document.
%% @end
%%--------------------------------------------------------------------
-spec remove_txt_by_provider(#dns_state{}, od_provider:id()) ->
    #dns_state{}.
remove_txt_by_provider(#dns_state{provider_to_txt_records = PtTR} = DnsState, ProviderId) ->
    DnsState#dns_state{
        provider_to_txt_records = maps:remove(ProviderId, PtTR)
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes subdomain of given provider from dns state document.
%% @end
%%--------------------------------------------------------------------
-spec unset_subdomain(record(), od_provider:id()) ->
    record().
unset_subdomain(DnsState, ProviderId) ->
    #dns_state{
        provider_to_subdomain = PtS,
        subdomain_to_provider = StP} = DnsState,
    Subdomain = maps:get(ProviderId, PtS, undefined),
    DnsState#dns_state{
        provider_to_subdomain = maps:remove(ProviderId, PtS),
        subdomain_to_provider = maps:remove(Subdomain, StP)
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes IPs of given provider from dns state document.
%% @end
%%--------------------------------------------------------------------
-spec unset_ips(record(), od_provider:id()) ->
    record().
unset_ips(#dns_state{provider_to_ips = PtIPs} = DnsState, ProviderId) ->
    DnsState#dns_state{
        provider_to_ips = maps:remove(ProviderId, PtIPs)
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sets TXT record content for given provider and record name.
%% Overwrites existing content for given name if any.
%% @end
%%--------------------------------------------------------------------
-spec set_txt_record(#dns_state{}, od_provider:id(),
    Name :: binary(), Content :: binary(), TTL :: ttl()) -> #dns_state{}.
set_txt_record(#dns_state{provider_to_txt_records = PtTR} = DnsState,
    ProviderId, Name, Content, TTL) ->
    TxtRecords = maps:get(ProviderId, PtTR, []),
    TxtRecords2 = lists:keystore(Name, 1, TxtRecords, {Name, Content, TTL}),
    DnsState#dns_state{provider_to_txt_records =
    PtTR#{ProviderId => TxtRecords2}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Finds provider associated with given subdomain.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_by_subdomain(record(), subdomain()) ->
    {true, od_provider:id()} | false.
get_provider_by_subdomain(#dns_state{subdomain_to_provider = StP}, Subdomain) ->
    case maps:find(Subdomain, StP) of
        error ->
            false;
        {ok, Found} ->
            {true, Found}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns dns state document.
%% @end
%%--------------------------------------------------------------------
-spec get_dns_state() -> {ok, record()} | {error, term()}.
get_dns_state() ->
    case datastore_model:get(?CTX, ?DNS_STATE_KEY) of
        {ok, #document{value = #dns_state{} = DnsState}} -> {ok, DnsState};
        {error, not_found} ->
            {ok, #dns_state{}};
        Error -> Error
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates dns state. If record does not exist, inserts Default.
%% @end
%%--------------------------------------------------------------------
-spec update(diff(), record()) -> {ok, record()} | {error, term()}.
update(Diff, Default) ->
    case datastore_model:update(?CTX, ?DNS_STATE_KEY, Diff, Default) of
        {ok, #document{key = ?DNS_STATE_KEY, value = DnsState}} ->
            {ok, DnsState};
        {error, _} = Error ->
            Error
    end.
