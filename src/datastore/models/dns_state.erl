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
o%% @end
%%%-------------------------------------------------------------------
-module(dns_state).
-author("Wojciech Geisler").

-include("datastore/oz_datastore_models.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([set_delegation_config/3, get_delegation_config/1,
         remove_delegation_config/1, get_subdomains_to_ips/0]).

%% datastore_model callbacks
-export([get_record_struct/1]).

-type id() :: binary().
-type record() :: #dns_state{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).

-type subdomain() :: binary().
-export_type([id/0, record/0]).
-export_type([subdomain/0]).

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
            update(fun(DnsState) ->
                StateOrError = case get_provider_by_subdomain(DnsState, Subdomain) of
                    {ok, ProviderId} ->
                        DnsState; % subdomain is already set
                    {ok, OtherProvider} ->
                        ?info("Refusing to register provider subdomain ~s as it is used by provider ~s", [Subdomain, OtherProvider]),
                        {error, subdomain_exists};
                    {error, not_found} ->
                        % remove old subdomain of given provider before setting new
                        DnsState2 = unset_subdomain(DnsState, ProviderId),
                        set_subdomain(DnsState2, ProviderId, Subdomain)
                end,
                case StateOrError of
                    {error, subdomain_exists} -> {error, subdomain_exists};
                    NewState -> {ok, set_ips(NewState, ProviderId, IPs)}
                end
            end, set_ips(set_subdomain(#dns_state{}, ProviderId, Subdomain),
                         ProviderId, IPs))
   end,
   case Result of
       {ok, _} ->
           node_manager_plugin:trigger_broadcast_dns_config(),
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
    {ok, DnsState} = get_or_create(),
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
%% Deletes all information about given provider.
%% @end
%%--------------------------------------------------------------------
-spec remove_delegation_config(od_provider:id()) -> ok.
remove_delegation_config(ProviderId) ->
    {ok, _} = update(fun(DnsState) ->
        DnsState2 = unset_subdomain(DnsState, ProviderId),
        {ok, unset_ips(DnsState2, ProviderId)}
    end, #dns_state{}),
    node_manager_plugin:trigger_broadcast_dns_config(),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Returns all provider subdomains and associated IPs.
%% @end
%%--------------------------------------------------------------------
-spec get_subdomains_to_ips() -> [{subdomain(), [inet:ip4_address()]}].
get_subdomains_to_ips() ->
    {ok, DnsState} = get_or_create(),
    #dns_state{
       provider_to_ips = PtIPs,
       provider_to_subdomain = PtS} = DnsState,
    maps:fold(fun(Provider, Subdomain, SubdomainsToIps) ->
        [{Subdomain, maps:get(Provider, PtIPs, [])} | SubdomainsToIps]
    end, [], PtS).


%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================

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
        {provider_to_ips, #{string => [{integer, integer, integer, integer}]}}
    ]}.


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
    {ok, DnsConf} = application:get_env(?APP_NAME, dns),
    {Static, _} = lists:unzip(proplists:get_value(static_entries, DnsConf, [])),

    % subdomains "ns" or "nsX" where X is a number are reserved for nameserver.
    lists:member(Subdomain, Static) orelse
        match == re:run(Subdomain, <<"^ns[0-9]*$">>, [{capture, none}]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates provider subdomain in dns state document.
%% @end
%%--------------------------------------------------------------------
-spec set_subdomain(#dns_state{}, od_provider:id(), subdomain()) -> #dns_state{}.
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
%% Updates provider ips in dns state document.
%% @end
%%--------------------------------------------------------------------
-spec set_ips(#dns_state{}, od_provider:id(), [inet:ip4_address()]) ->
    #dns_state{}.
set_ips(#dns_state{provider_to_ips = PtIPs} = DnsState, ProviderId, IPs) ->
    DnsState#dns_state{provider_to_ips = PtIPs#{ProviderId => IPs}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Removes subdomain of given provider from dns state document.
%% @end
%%--------------------------------------------------------------------
-spec unset_subdomain(#dns_state{}, od_provider:id()) ->
    #dns_state{}.
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
-spec unset_ips(#dns_state{}, od_provider:id()) ->
    #dns_state{}.
unset_ips(#dns_state{provider_to_ips = PtIPs} = DnsState, ProviderId) ->
    DnsState#dns_state{
        provider_to_ips = maps:remove(ProviderId, PtIPs)
    }.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Finds provider associated with given subdomain.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_by_subdomain(#dns_state{}, subdomain()) ->
    {ok, od_provider:id()} | {error, not_found}.
get_provider_by_subdomain(#dns_state{subdomain_to_provider = StP}, Subdomain) ->
    case maps:find(Subdomain, StP) of
        error ->
            {error, not_found};
        {ok, Found} ->
            {ok, Found}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns dns state document.
%% Creates default document if it doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_or_create() -> {ok, #dns_state{}} | {error, term()}.
get_or_create() ->
    Result = case datastore_model:get(?CTX, ?DNS_STATE_KEY) of
        {ok, Doc} -> {ok, Doc};
        {error, not_found} ->
            % use update instead of insert to prevent data races
            datastore_model:update(?CTX, ?DNS_STATE_KEY,
                fun(DnsState) -> {ok, DnsState} end,
                #document{key = ?DNS_STATE_KEY, value=#dns_state{}});
        Error -> Error
    end,
    case Result of
        {ok, #document{key = ?DNS_STATE_KEY, value=DnsState}} ->
            {ok, DnsState};
        {error, _} = E ->
            E
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates dns state. If record does not exist, inserts Default.
%% @end
%%--------------------------------------------------------------------
-spec update(diff(), #dns_state{}) -> {ok, #dns_state{}} | {error, term()}.
update(Diff, Default) ->
    case datastore_model:update(?CTX, ?DNS_STATE_KEY, Diff, Default) of
        {ok, #document{key = ?DNS_STATE_KEY, value = DnsState}} ->
            {ok, DnsState};
        {error, _} = Error ->
            Error
    end.
