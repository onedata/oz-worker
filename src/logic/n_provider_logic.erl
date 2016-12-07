%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc

%%% @end
%%%-------------------------------------------------------------------
-module(n_provider_logic).
-author("Lukasz Opiola").
-behaviour(data_logic_behaviour).

-include("gui/common.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

-define(PLUGIN, n_provider_logic_plugin).

-export([
    create/5, create/7, create/2
]).
-export([
    support_space/4, support_space/3
]).
-export([
    get/2,
    get_spaces/2, get_space/3,
    get_eff_users/2, get_eff_user/3,
    get_eff_groups/2, get_eff_group/3
]).
-export([
    update/3
]).
-export([
    delete/2
]).
-export([
    check_my_ports/2,
    check_my_ip/1
]).
-export([
    exists/1,
    list/0
]).
-export([
    get_url/1,
    choose_provider_for_user/1,
    check_provider_connectivity/1
]).


create(Issuer, Name, URLs, RedirectionPoint, CSR) ->
    create(Issuer, #{
        <<"name">> => Name,
        <<"urls">> => URLs,
        <<"redirectionPoint">> => RedirectionPoint,
        <<"csr">> => CSR
    }).
create(Issuer, Name, URLs, RedirectionPoint, CSR, Latitude, Longitude) ->
    create(Issuer, #{
        <<"name">> => Name,
        <<"urls">> => URLs,
        <<"redirectionPoint">> => RedirectionPoint,
        <<"csr">> => CSR,
        <<"latitude">> => Latitude,
        <<"longitude">> => Longitude
    }).
create(Issuer, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, undefined, entity, Data).


support_space(Issuer, ProviderId, Token, SupportSize) ->
    support_space(Issuer, ProviderId, #{
        <<"token">> => Token, <<"size">> => SupportSize
    }).
support_space(Issuer, ProviderId, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, ProviderId, spaces, Data).


get(Issuer, ProviderId) ->
    n_entity_logic:get(Issuer, ?PLUGIN, ProviderId, entity).
get_spaces(Issuer, ProviderId) ->
    n_entity_logic:get(Issuer, ?PLUGIN, ProviderId, spaces).
get_space(Issuer, ProviderId, SpaceId) ->
    n_entity_logic:get(Issuer, ?PLUGIN, ProviderId, {space, SpaceId}).
get_eff_users(Issuer, ProviderId) ->
    n_entity_logic:get(Issuer, ?PLUGIN, ProviderId, eff_users).
get_eff_user(Issuer, ProviderId, UserId) ->
    n_entity_logic:get(Issuer, ?PLUGIN, ProviderId, {eff_user, UserId}).
get_eff_groups(Issuer, ProviderId) ->
    n_entity_logic:get(Issuer, ?PLUGIN, ProviderId, eff_groups).
get_eff_group(Issuer, ProviderId, GroupId) ->
    n_entity_logic:get(Issuer, ?PLUGIN, ProviderId, {eff_group, GroupId}).




update(Issuer, ProviderId, Data) ->
    n_entity_logic:update(Issuer, ?PLUGIN, ProviderId, entity, Data).

delete(Issuer, ProviderId) ->
    n_entity_logic:delete(Issuer, ?PLUGIN, ProviderId, entity).


check_my_ports(Issuer, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, undefined, check_my_ports, Data).


check_my_ip(Issuer) ->
    n_entity_logic:get(Issuer, ?PLUGIN, undefined, check_my_ip).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a provider exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(ProviderId :: od_provider:id()) -> boolean().
exists(ProviderId) ->
    od_provider:exists(ProviderId).


%%--------------------------------------------------------------------
%% @doc Returns full provider URL.
%% @end
%%--------------------------------------------------------------------
-spec get_url(ProviderId :: binary()) ->
    {ok, ProviderURL :: binary()}.
get_url(ProviderId) ->
    {ok, #document{
        value = #od_provider{
            redirection_point = RedPoint
        }}} = provider_logic:get_data(ProviderId),
    #hackney_url{host = Host, port = Port} = hackney_url:parse_url(RedPoint),
    URL = str_utils:format_bin("https://~s:~B", [Host, Port]),
    {ok, URL}.


%%--------------------------------------------------------------------
%% @doc Returns provider id of provider that has been chosen
%% as default for given user, or {error, no_provider} otherwise.
%% If the user has a default spaces and it is supported by some providers,
%% one of them will be chosen randomly.
%% Otherwise, if any of user spaces is supported by any provider,
%% one of them will be chosen randomly.
%% @end
%%--------------------------------------------------------------------
-spec choose_provider_for_user(Referer :: binary() | undefined) ->
    {ok, ProviderId :: binary()} | {error, no_provider}.
choose_provider_for_user(UserId) ->
    error(unimplemented),
    % Check if the user has a default space and if it is supported.
    {ok, [{spaces, Spaces}, {default, DefaultSpace}]} =
        user_logic:get_spaces(UserId),
    {ok, [{providers, DSProviders}]} =
        case DefaultSpace of
            undefined ->
                {ok, [{providers, []}]};
            _ ->
                space_logic:get_providers(DefaultSpace, user)
        end,
    case DSProviders of
        List when length(List) > 0 ->
            % Default space has got some providers, random one
            {ok, lists:nth(crypto:rand_uniform(1, length(DSProviders) + 1), DSProviders)};
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
                    % There are some providers for other spaces, random one
                    {ok, lists:nth(crypto:rand_uniform(1, length(ProviderIDs) + 1), ProviderIDs)}
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns a list of all providers in the system (their ids).
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [od_provider:id()]}.
list() ->
    {ok, ProviderDocs} = od_provider:list(),
    {ok, [ProviderId || #document{key = ProviderId} <- ProviderDocs]}.


%%--------------------------------------------------------------------
%% @doc
% Checks if given provider (by Id) is alive and responding.
%% @end
%%--------------------------------------------------------------------
-spec check_provider_connectivity(ProviderId :: od_provider:id()) -> boolean().
check_provider_connectivity(ProviderId) ->
    case subscriptions:any_connection_active(ProviderId) of
        true ->
            true;
        false ->
            try
                % Sometimes it may happen that there is no websocket connection
                % but the worker is fully operational. For example, when the
                % connection has timed out and provider hasn't reconnected yet.
                % In such case, make sure it is really inoperable by making
                % a http request.
                {ok, #od_provider{
                    redirection_point = RedPoint
                }} = get(?ROOT, ProviderId),
                #hackney_url{host = Host} = hackney_url:parse_url(RedPoint),
                ConnCheckEndpoint = str_utils:format_bin("https://~s~s", [
                    Host, ?provider_id_endpoint
                ]),
                {ok, _, _, ProviderId} =
                    http_client:get(ConnCheckEndpoint, [], <<>>, [insecure]),
                true
            catch _:_ ->
                false
            end
    end.
