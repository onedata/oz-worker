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
-export([
    set_delegation_config/3,
    get_delegation_config/1,
    remove_delegation_config/1,

    get_provider_subdomain_labels/0,
    get_provider_relative_domain_names_to_ips/0
]).
-export([
    update_txt_records/2,
    get_txt_records/0
]).

-export([get_dns_state/0]).


%% datastore_model callbacks
-export([get_record_version/0, upgrade_record/2, get_record_struct/1]).

-type id() :: binary().
-type record() :: #dns_state{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).

-type provider_service() :: op_worker | ones3.

%% Mapping between provider service and its external ips
-type provider_ips() :: #{provider_service() => [inet:ip4_address()]}.
-type ttl() :: time:seconds() | undefined.
-type txt_records_diff() :: #{
    provider_service() => #{
        set => #{binary() => {binary(), ttl()}},
        unset => [binary()]
    }
}.

-export_type([id/0, record/0, doc/0]).
-export_type([provider_service/0, provider_ips/0, ttl/0, txt_records_diff/0]).

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
-spec set_delegation_config(od_provider:id(), dns_utils:domain_label(), provider_ips()) ->
    ok | {error, subdomain_exists}.
set_delegation_config(ProviderId, ProviderSubdomainLabel, ProviderIps) ->
    Result = case is_subdomain_label_reserved(ProviderSubdomainLabel) of
        true ->
            ?info("Refusing to register provider subdomain '~ts' as it is reserved", [ProviderSubdomainLabel]),
            {error, subdomain_exists};
        false ->
            Diff = fun(DnsState) ->
                StateOrError = case find_provider_by_subdomain_label(DnsState, ProviderSubdomainLabel) of
                    {true, ProviderId} ->
                        DnsState; % subdomain is already set
                    {true, OtherProvider} ->
                        ?debug("Refusing to set provider's ~ts subdomain to ~ts as it is used by provider ~ts",
                            [ProviderId, ProviderSubdomainLabel, OtherProvider]),
                        {error, subdomain_exists};
                    false ->
                        % remove old subdomain of provider begin updated before setting new
                        DnsState2 = unset_subdomain_label(DnsState, ProviderId),
                        set_subdomain_label(DnsState2, ProviderId, ProviderSubdomainLabel)
                end,
                case StateOrError of
                    {error, subdomain_exists} -> {error, subdomain_exists};
                    NewState -> {ok, set_ips(NewState, ProviderId, ProviderIps)}
                end
            end,
            Default = set_ips(
                set_subdomain_label(#dns_state{}, ProviderId, ProviderSubdomainLabel),
                ProviderId, ProviderIps
            ),
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
    {ok, dns_utils:domain_label(), provider_ips()} | {error, not_found}.
get_delegation_config(ProviderId) ->
    {ok, DnsState} = get_dns_state(),
    #dns_state{
        provider_to_subdomain = PtS,
        provider_to_ips = PtIPs} = DnsState,
    case maps:find(ProviderId, PtS) of
        {ok, Subdomain} ->
            {ok, Subdomain, maps:get(ProviderId, PtIPs)};
        error ->
            {error, not_found}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Deletes all information about given provider.
%% @end
%%--------------------------------------------------------------------
-spec remove_delegation_config(od_provider:id()) -> ok.
remove_delegation_config(ProviderId) ->
    {ok, _} = update(fun(DnsState) ->
        DnsState2 = unset_subdomain_label(DnsState, ProviderId),
        DnsState3 = remove_txt_records(DnsState2, ProviderId),
        {ok, unset_ips(DnsState3, ProviderId)}
    end, #dns_state{}),
    node_manager_plugin:reconcile_dns_config(),
    ok.


-spec get_provider_subdomain_labels() -> [dns_utils:domain_label()].
get_provider_subdomain_labels() ->
    {ok, #dns_state{provider_to_subdomain = PtS}} = get_dns_state(),
    maps:values(PtS).


-spec get_provider_relative_domain_names_to_ips() ->
    #{dns_utils:domain_name() => [inet:ip4_address()]}.
get_provider_relative_domain_names_to_ips() ->
    {ok, DnsState = #dns_state{provider_to_ips = PtIPs}} = get_dns_state(),

    maps:fold(fun(ProviderSubdomainLabel, ProviderId, OuterAcc) ->

        maps:fold(fun
            (_ProviderService, [], InnerAcc) ->
                InnerAcc;
            (ProviderService, ServiceIPs, InnerAcc) ->
                ProviderServiceRelDomain = build_provider_service_rel_domain(
                    ProviderService, ProviderSubdomainLabel
                ),
                InnerAcc#{ProviderServiceRelDomain => ServiceIPs}
        end, OuterAcc, maps:get(ProviderId, PtIPs))

    end, #{}, DnsState#dns_state.subdomain_to_provider).


%%--------------------------------------------------------------------
%% @doc
%% Updates txt records in provider's subdomain.
%% Given provider mus have an associated subdomain, otherwise
%% error is returned.
%% @end
%%--------------------------------------------------------------------
-spec update_txt_records(od_provider:id(), txt_records_diff()) ->
    ok | {error, no_subdomain}.
update_txt_records(ProviderId, Diff) ->
    Result = update(fun(DnsState = #dns_state{provider_to_subdomain = PtS}) ->
        case maps:find(ProviderId, PtS) of
            {ok, _} ->
                {ok, update_txt_records(DnsState, ProviderId, Diff)};
            error ->
                {error, not_found}
        end
    end, update_txt_records(#dns_state{}, ProviderId, Diff)),

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
%% Returns all txt records, building their names using provider subdomains
%% @end
%%--------------------------------------------------------------------
-spec get_txt_records() ->
    [{Subdomain :: binary(), {Content :: binary(), TTL :: ttl()}}].
get_txt_records() ->
    {ok, DnsState} = get_dns_state(),
    #dns_state{provider_to_subdomain = PtS} = DnsState,

    maps:fold(fun(ProviderId, ProviderTxtRecords, OuterAcc) ->
        ProviderSubdomainLabel = maps:get(ProviderId, PtS),

        maps:fold(fun(ProviderService, ProviderServiceTxtRecords, MiddleAcc) ->
            ProviderServiceRelDomain = build_provider_service_rel_domain(
                ProviderService, ProviderSubdomainLabel
            ),

            maps:fold(fun(Name, {Content, TTL}, InnerAcc) ->
                Entry = {<<Name/binary, $., ProviderServiceRelDomain/binary>>, {Content, TTL}},
                [Entry | InnerAcc]
            end, MiddleAcc, ProviderServiceTxtRecords)
        end, OuterAcc, ProviderTxtRecords)

    end, [], DnsState#dns_state.provider_to_txt_records).


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
    3.


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
    ]};

get_record_struct(3) ->
    {record, [
        {subdomain_to_provider, #{string => string}},
        {provider_to_subdomain, #{string => string}},
        {provider_to_ips, #{string => #{atom => [{integer, integer, integer, integer}]}}},
        {provider_to_txt_records, #{string => #{atom => #{string => {string, integer}}}}}
    ]}.


%%--------------------------------------------------------------------
%% @doc
%% Upgrades model's record from provided version to the next one.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_record(datastore_model:record_version(), datastore_model:record()) ->
    {datastore_model:record_version(), datastore_model:record()}.
upgrade_record(1, {
    ?MODULE,
    SubdomainToProvider,
    ProviderToSubdomain,
    ProviderToIPS,
    ProviderToTxt
}) ->
    {2, {
        ?MODULE,
        SubdomainToProvider,
        ProviderToSubdomain,
        ProviderToIPS,
        maps:map(fun(_ProviderId, TxtRecords) ->
            [{Name, Content, undefined} || {Name, Content} <- TxtRecords]
        end, ProviderToTxt)
    }};

upgrade_record(2, {
    ?MODULE,
    SubdomainToProvider,
    ProviderToSubdomain,
    ProviderToIPS,
    ProviderToTxt
}) ->
    NewProviderToIPS = maps:map(fun(_ProviderId, Ips) ->
        #{op_worker => Ips}
    end, ProviderToIPS),

    NewProviderToTxt = maps:map(fun(_ProviderId, TxtRecords) ->
        #{op_worker => lists:foldl(
            fun({Name, Content, TTL}, Acc) -> Acc#{Name => {Content, TTL}} end,
            #{},
            TxtRecords
        )}
    end, ProviderToTxt),

    {3, {
        ?MODULE,
        SubdomainToProvider,
        ProviderToSubdomain,
        NewProviderToIPS,
        NewProviderToTxt
    }}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec build_provider_service_rel_domain(provider_service(), dns_utils:domain_label()) ->
    dns_utils:domain_name().
build_provider_service_rel_domain(ProviderService, ProviderSubdomainLabel) ->
    dns_utils:build_domain(
        provider_service_to_subdomain_label(ProviderService),
        ProviderSubdomainLabel
    ).


%% @private
-spec provider_service_to_subdomain_label(provider_service()) ->
    dns_utils:domain_label().
provider_service_to_subdomain_label(op_worker) -> <<>>;
provider_service_to_subdomain_label(ones3) -> <<"s3">>.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if subdomain is reserved for a static entry or nameserver.
%% @end
%%--------------------------------------------------------------------
-spec is_subdomain_label_reserved(dns_utils:domain_label()) -> boolean().
is_subdomain_label_reserved(ProviderSubdomainLabel) ->
    % Get all reserved values
    Static = lists:flatmap(fun(Env) ->
        proplists:get_keys(oz_worker:get_env(Env, []))
    end, [dns_static_a_records, dns_static_ns_records, dns_static_cname_records]),
    Static2 = lists:foldl(fun({_Name, Value, _Preference}, Acc) ->
        [Value | Acc]
    end, Static, oz_worker:get_env(dns_static_mx_records, [])),

    IsReservedByStaticEntry = lists:any(fun(StaticSubdomain) ->
        dns_utils:is_equal_or_subdomain(StaticSubdomain, ProviderSubdomainLabel)
    end, Static2),

    % subdomains "ns" or "nsX" where X is a number are reserved for nameserver.
    IsReservedByStaticEntry orelse match == re:run(
        ProviderSubdomainLabel, <<"^ns[0-9]*$">>, [{capture, none}]
    ).


%% @private
-spec set_subdomain_label(record(), od_provider:id(), dns_utils:domain_label()) -> record().
set_subdomain_label(DnsState, ProviderId, SubdomainLabel) ->
    #dns_state{
        provider_to_subdomain = PtS,
        subdomain_to_provider = StP} = DnsState,
    NewStP = case maps:find(ProviderId, PtS) of
        {ok, OldSubdomain} -> maps:remove(OldSubdomain, StP);
        error -> StP
    end,
    DnsState#dns_state{
        provider_to_subdomain = PtS#{ProviderId => SubdomainLabel},
        subdomain_to_provider = NewStP#{SubdomainLabel => ProviderId}
    }.


%% @private
-spec unset_subdomain_label(record(), od_provider:id()) -> record().
unset_subdomain_label(DnsState, ProviderId) ->
    #dns_state{
        provider_to_subdomain = PtS,
        subdomain_to_provider = StP} = DnsState,
    Subdomain = maps:get(ProviderId, PtS, undefined),
    DnsState#dns_state{
        provider_to_subdomain = maps:remove(ProviderId, PtS),
        subdomain_to_provider = maps:remove(Subdomain, StP)
    }.


%% @private
-spec set_ips(record(), od_provider:id(), provider_ips()) -> record().
set_ips(#dns_state{provider_to_ips = PtIPs} = DnsState, ProviderId, IPs) ->
    DnsState#dns_state{provider_to_ips = PtIPs#{ProviderId => IPs}}.


%% @private
-spec unset_ips(record(), od_provider:id()) -> record().
unset_ips(#dns_state{provider_to_ips = PtIPs} = DnsState, ProviderId) ->
    DnsState#dns_state{provider_to_ips = maps:remove(ProviderId, PtIPs)}.


%% @private
-spec update_txt_records(record(), od_provider:id(), txt_records_diff()) -> record().
update_txt_records(#dns_state{provider_to_txt_records = PtTR} = DnsState, ProviderId, Diff) ->
    TxtRecords = maps:get(ProviderId, PtTR, #{}),

    NewTxtRecords = maps:fold(fun(ProviderService, ProviderServiceTxtRecordsDiff, Acc) ->
        CurrentRecords = maps:get(ProviderService, TxtRecords, #{}),
        RecordsToSet = maps:get(set, ProviderServiceTxtRecordsDiff, #{}),
        RecordsToUnset = maps:get(unset, ProviderServiceTxtRecordsDiff, []),

        Acc#{ProviderService => maps:without(RecordsToUnset, maps:merge(CurrentRecords, RecordsToSet))}
    end, TxtRecords, Diff),

    DnsState#dns_state{provider_to_txt_records = PtTR#{ProviderId => NewTxtRecords}}.


%% @private
-spec remove_txt_records(record(), od_provider:id()) -> record().
remove_txt_records(#dns_state{provider_to_txt_records = PtTR} = DnsState, ProviderId) ->
    DnsState#dns_state{provider_to_txt_records = maps:remove(ProviderId, PtTR)}.


%% @private
-spec find_provider_by_subdomain_label(record(), dns_utils:domain_label()) ->
    {true, od_provider:id()} | false.
find_provider_by_subdomain_label(#dns_state{subdomain_to_provider = StP}, SubdomainLabel) ->
    case maps:find(SubdomainLabel, StP) of
        error -> false;
        {ok, Found} -> {true, Found}
    end.


%% @private
-spec get_dns_state() -> {ok, record()} | {error, term()}.
get_dns_state() ->
    case datastore_model:get(?CTX, ?DNS_STATE_KEY) of
        {ok, #document{value = #dns_state{} = DnsState}} ->
            {ok, DnsState};
        {error, not_found} ->
            {ok, #dns_state{}};
        Error ->
            Error
    end.


%% @private
-spec update(diff(), record()) -> {ok, record()} | {error, term()}.
update(Diff, Default) ->
    case datastore_model:update(?CTX, ?DNS_STATE_KEY, Diff, Default) of
        {ok, #document{key = ?DNS_STATE_KEY, value = DnsState}} ->
            {ok, DnsState};
        {error, _} = Error ->
            Error
    end.
