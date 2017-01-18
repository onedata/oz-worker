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

-include("entity_logic.hrl").
-include("gui/common.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

-define(PLUGIN, n_provider_logic_plugin).

-export([
    create/5, create/7, create/2, create_dev/2
]).
-export([
    get/2,
    get_data/2,
    list/1
]).
-export([
    update/3
]).
-export([
    delete/2
]).
-export([
    get_eff_users/2, get_eff_user/3,
    get_eff_groups/2, get_eff_group/3,
    get_spaces/2, get_space/3,
    support_space/4, support_space/3,
    update_support_size/4,
    revoke_support/3
]).
-export([
    check_my_ports/2,
    check_my_ip/2
]).
-export([
    exists/1
]).
-export([
    get_url/1,
    choose_provider_for_user/1,
    check_provider_connectivity/1
]).


create(Client, Name, URLs, RedirectionPoint, CSR) ->
    create(Client, #{
        <<"name">> => Name,
        <<"urls">> => URLs,
        <<"redirectionPoint">> => RedirectionPoint,
        <<"csr">> => CSR
    }).
create(Client, Name, URLs, RedirectionPoint, CSR, Latitude, Longitude) ->
    create(Client, #{
        <<"name">> => Name,
        <<"urls">> => URLs,
        <<"redirectionPoint">> => RedirectionPoint,
        <<"csr">> => CSR,
        <<"latitude">> => Latitude,
        <<"longitude">> => Longitude
    }).
create(Client, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, undefined, entity, Data).
create_dev(Client, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, undefined, entity_dev, Data).


get(Client, ProviderId) ->
    n_entity_logic:get(Client, ?PLUGIN, ProviderId, entity).


get_data(Client, ProviderId) ->
    n_entity_logic:get(Client, ?PLUGIN, ProviderId, data).


list(Client) ->
    n_entity_logic:get(Client, ?PLUGIN, undefined, list).
get_eff_users(Client, ProviderId) ->
    n_entity_logic:get(Client, ?PLUGIN, ProviderId, eff_users).
get_eff_user(Client, ProviderId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, ProviderId, {eff_user, UserId}).
get_eff_groups(Client, ProviderId) ->
    n_entity_logic:get(Client, ?PLUGIN, ProviderId, eff_groups).
get_eff_group(Client, ProviderId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, ProviderId, {eff_group, GroupId}).
get_spaces(Client, ProviderId) ->
    n_entity_logic:get(Client, ?PLUGIN, ProviderId, spaces).
get_space(Client, ProviderId, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, ProviderId, {space, SpaceId}).



update(Client, ProviderId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, ProviderId, entity, Data).


delete(Client, ProviderId) ->
    n_entity_logic:delete(Client, ?PLUGIN, ProviderId, entity).


support_space(Client, ProviderId, Token, SupportSize) ->
    support_space(Client, ProviderId, #{
        <<"token">> => Token, <<"size">> => SupportSize
    }).
support_space(Client, ProviderId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, ProviderId, support, Data).


update_support_size(Client, ProviderId, SpaceId, SupSize) when is_integer(SupSize) ->
    update_support_size(Client, ProviderId, SpaceId, #{
        <<"size">> => SupSize
    });
update_support_size(Client, ProviderId, SpaceId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, ProviderId, {space, SpaceId}, Data).


revoke_support(Client, ProviderId, SpaceId) ->
    n_entity_logic:delete(Client, ?PLUGIN, ProviderId, {space, SpaceId}).


check_my_ports(Client, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, undefined, check_my_ports, Data).


check_my_ip(Client, CowboyReq) ->
    n_entity_logic:get(Client, ?PLUGIN, undefined, {check_my_ip, CowboyReq}).


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
-spec get_url(ProviderId :: od_provider:id()) -> {ok, ProviderURL :: binary()}.
get_url(ProviderId) ->
    {ok, #od_provider{redirection_point = RedPoint}} = get(?ROOT, ProviderId),
    #{host := Host, port := Port} = url_utils:parse(RedPoint),
    {ok, str_utils:format_bin("https://~s:~B", [Host, Port])}.


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
    {ok, ProviderId :: od_provider:id()} | {error, no_provider}.
choose_provider_for_user(UserId) ->
    % Check if the user has a default space and if it is supported.
    #od_user{
        spaces = Spaces, default_space = DefaultSpace
    } = n_user_logic:get(?ROOT, UserId),
    {ok, DSProviders} =
        case DefaultSpace of
            undefined ->
                {ok, [{providers, []}]};
            _ ->
                n_space_logic:get_providers(?ROOT, DefaultSpace)
        end,
    case DSProviders of
        List when length(List) > 0 ->
            % Default space has got some providers, random one
            {ok, lists:nth(crypto:rand_uniform(1, length(DSProviders) + 1), DSProviders)};
        _ ->
            % Default space does not have a provider, look in other spaces
            ProviderIds = lists:foldl(
                fun(Space, Acc) ->
                    {ok, Providers} = n_space_logic:get_providers(?ROOT, Space),
                    Providers ++ Acc
                end, [], Spaces),

            case ProviderIds of
                [] ->
                    % No provider for other spaces = nowhere to redirect
                    {error, no_provider};
                _ ->
                    % There are some providers for other spaces, random one
                    {ok, lists:nth(crypto:rand_uniform(1, length(ProviderIds) + 1), ProviderIds)}
            end
    end.


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
                #{host := Host} = url_utils:parse(RedPoint),
                ConnCheckEndpoint = str_utils:format_bin("https://~s~s", [
                    Host, ?PROVIDER_ID_ENDPOINT
                ]),
                {ok, _, _, ProviderId} =
                    http_client:get(ConnCheckEndpoint, [], <<>>, [insecure]),
                true
            catch _:_ ->
                false
            end
    end.
