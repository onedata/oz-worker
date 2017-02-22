%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all provider logic functionalities.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(n_provider_logic).
-author("Lukasz Opiola").

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
    exists/1,
    has_eff_user/2
]).
-export([
    get_url/1,
    choose_provider_for_user/1,
    is_provider_connected/1
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new provider document in database based on Name, URLs,
%% RedirectionPoint and CSR (Certificate Signing Request).
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: n_entity_logic:client(), Name :: binary(),
    URLs :: [binary()], RedirectionPoint :: binary(), CSR :: binary()) ->
    {ok, od_provider:id()} | {error, term()}.
create(Client, Name, URLs, RedirectionPoint, CSR) ->
    create(Client, #{
        <<"name">> => Name,
        <<"urls">> => URLs,
        <<"redirectionPoint">> => RedirectionPoint,
        <<"csr">> => CSR
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new provider document in database based on Name, URLs,
%% RedirectionPoint, CSR (Certificate Signing Request), Latitude and Longitude.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: n_entity_logic:client(), Name :: binary(),
    URLs :: [binary()], RedirectionPoint :: binary(), CSR :: binary(),
    Latitude :: float(), Longitude :: float()) ->
    {ok, od_provider:id()} | {error, term()}.
create(Client, Name, URLs, RedirectionPoint, CSR, Latitude, Longitude) ->
    create(Client, #{
        <<"name">> => Name,
        <<"urls">> => URLs,
        <<"redirectionPoint">> => RedirectionPoint,
        <<"csr">> => CSR,
        <<"latitude">> => Latitude,
        <<"longitude">> => Longitude
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new provider document in database. Name, URLs, RedirectionPoint and
%% CSR (Certificate Signing Request) are provided in a
%% proper Data object, Latitude and Longitude are optional.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: n_entity_logic:client(), Data :: #{}) ->
    {ok, od_provider:id()} | {error, term()}.
create(Client, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, undefined, entity, Data).


%%--------------------------------------------------------------------
%% @doc
%% TODO This is a developer functionality and should be removed when
%% TODO VFS-2550 is ready.
%% Creates a new provider document in database. UUID, Name, URLs,
%% RedirectionPoint and CSR (Certificate Signing Request) are provided in a
%% proper Data object, Latitude and Longitude are optional.
%% @end
%%--------------------------------------------------------------------
-spec create_dev(Client :: n_entity_logic:client(), Data :: #{}) ->
    {ok, od_provider:id()} | {error, term()}.
create_dev(Client, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, undefined, entity_dev, Data).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a provider record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(Client :: n_entity_logic:client(), ProviderId :: od_provider:id()) ->
    {ok, #od_provider{}} | {error, term()}.
get(Client, ProviderId) ->
    n_entity_logic:get(Client, ?PLUGIN, ProviderId, entity).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves information about a provider record from database.
%% @end
%%--------------------------------------------------------------------
-spec get_data(Client :: n_entity_logic:client(), ProviderId :: od_provider:id()) ->
    {ok, #{}} | {error, term()}.
get_data(Client, ProviderId) ->
    n_entity_logic:get(Client, ?PLUGIN, ProviderId, data).


%%--------------------------------------------------------------------
%% @doc
%% Lists all providers (their ids) in database.
%% @end
%%--------------------------------------------------------------------
-spec list(Client :: n_entity_logic:client()) ->
    {ok, [od_provider:id()]} | {error, term()}.
list(Client) ->
    n_entity_logic:get(Client, ?PLUGIN, undefined, list).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given provider. Supports updating Name, URLs,
%% RedirectionPoint, Latitude and Longitude.
%% @end
%%--------------------------------------------------------------------
-spec update(Client :: n_entity_logic:client(), ProviderId :: od_provider:id(),
    Data :: #{}) -> ok | {error, term()}.
update(Client, ProviderId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, ProviderId, entity, Data).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given provider from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(Client :: n_entity_logic:client(), ProviderId :: od_provider:id()) ->
    ok | {error, term()}.
delete(Client, ProviderId) ->
    n_entity_logic:delete(Client, ?PLUGIN, ProviderId, entity).


%%--------------------------------------------------------------------
%% @doc
%% Supports a space based on support_space_token and support size.
%% @end
%%--------------------------------------------------------------------
-spec support_space(Client :: n_entity_logic:client(), ProviderId :: od_provider:id(),
    Token :: token:id() | macaroon:macaroon(), SupportSize :: integer()) ->
    {ok, od_space:id()} | {error, term()}.
support_space(Client, ProviderId, Token, SupportSize) ->
    support_space(Client, ProviderId, #{
        <<"token">> => Token, <<"size">> => SupportSize
    }).


%%--------------------------------------------------------------------
%% @doc
%% Supports a space. Token (support_space_token) and SupportSize
%% are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec support_space(Client :: n_entity_logic:client(), ProviderId :: od_provider:id(),
    Data :: #{}) -> {ok, od_space:id()} | {error, term()}.
support_space(Client, ProviderId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, ProviderId, support, Data).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective users of given provider.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_users(Client :: n_entity_logic:client(), ProviderId :: od_provider:id()) ->
    {ok, [od_user:id()]} | {error, term()}.
get_eff_users(Client, ProviderId) ->
    n_entity_logic:get(Client, ?PLUGIN, ProviderId, eff_users).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective user among
%% effective users of given provider.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_user(Client :: n_entity_logic:client(), ProviderId :: od_provider:id(),
    UserId :: od_user:id()) -> {ok, #{}} | {error, term()}.
get_eff_user(Client, ProviderId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, ProviderId, {eff_user, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of effective groups of given provider.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_groups(Client :: n_entity_logic:client(), ProviderId :: od_provider:id()) ->
    {ok, [od_group:id()]} | {error, term()}.
get_eff_groups(Client, ProviderId) ->
    n_entity_logic:get(Client, ?PLUGIN, ProviderId, eff_groups).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific effective group among
%% effective groups of given provider.
%% @end
%%--------------------------------------------------------------------
-spec get_eff_group(Client :: n_entity_logic:client(), ProviderId :: od_provider:id(),
    GroupId :: od_group:id()) -> {ok, #{}} | {error, term()}.
get_eff_group(Client, ProviderId, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, ProviderId, {eff_group, GroupId}).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the list of spaces of given provider.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(Client :: n_entity_logic:client(), ProviderId :: od_provider:id()) ->
    {ok, [od_space:id()]} | {error, term()}.
get_spaces(Client, ProviderId) ->
    n_entity_logic:get(Client, ?PLUGIN, ProviderId, spaces).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the information about specific space among spaces of given provider.
%% @end
%%--------------------------------------------------------------------
-spec get_space(Client :: n_entity_logic:client(), ProviderId :: od_provider:id(),
    SpaceId :: od_space:id()) -> {ok, #{}} | {error, term()}.
get_space(Client, ProviderId, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, ProviderId, {space, SpaceId}).


%%--------------------------------------------------------------------
%% @doc
%% Updates support size for specified space of given provider. Has two variants:
%% 1) New support size is given explicitly
%% 2) New support size is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_support_size(Client :: n_entity_logic:client(), ProviderId :: od_provider:id(),
    SpaceId :: od_space:id(), SupSizeOrData :: integer() | #{}) -> ok | {error, term()}.
update_support_size(Client, ProviderId, SpaceId, SupSize) when is_integer(SupSize) ->
    update_support_size(Client, ProviderId, SpaceId, #{
        <<"size">> => SupSize
    });
update_support_size(Client, ProviderId, SpaceId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, ProviderId, {space, SpaceId}, Data).


%%--------------------------------------------------------------------
%% @doc
%% Revokes support for specified space on behalf of given provider.
%% @end
%%--------------------------------------------------------------------
-spec revoke_support(Client :: n_entity_logic:client(), ProviderId :: od_provider:id(),
    SpaceId :: od_space:id()) -> ok | {error, term()}.
revoke_support(Client, ProviderId, SpaceId) ->
    n_entity_logic:delete(Client, ?PLUGIN, ProviderId, {space, SpaceId}).


%%--------------------------------------------------------------------
%% @doc
%% Performs port check operation by requesting all specified URLs and returning
%% whether the requests succeeded.
%% @end
%%--------------------------------------------------------------------
-spec check_my_ports(Client :: n_entity_logic:client(), Data :: #{}) ->
    ok | {error, term()}.
check_my_ports(Client, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, undefined, check_my_ports, Data).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the IP of requesting client based on cowboy req.
%% @end
%%--------------------------------------------------------------------
-spec check_my_ip(Client :: n_entity_logic:client(),
    CowboyReq :: cowboy_req:req()) -> {ok, IP :: binary()} | {error, term()}.
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
%% @doc
%% Predicate saying whether specified user is an effective user in given provider.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_user(ProviderOrId :: od_provider:id() | #od_provider{},
    UserId :: od_user:id()) -> boolean().
has_eff_user(ProviderId, UserId) when is_binary(ProviderId) ->
    case od_provider:get(ProviderId) of
        {ok, #document{value = Provider}} ->
            has_eff_user(Provider, UserId);
        _ ->
            false
    end;
has_eff_user(#od_provider{eff_users = EffUsers}, UserId) ->
    maps:is_key(UserId, EffUsers).


%%--------------------------------------------------------------------
%% @doc
%% Returns full provider URL.
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
    {ok, #od_user{
        spaces = Spaces, default_space = DefaultSpace
    }} = n_user_logic:get(?ROOT, UserId),
    {ok, DSProviders} =
        case DefaultSpace of
            undefined ->
                {ok, []};
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
-spec is_provider_connected(ProviderId :: od_provider:id()) -> boolean().
is_provider_connected(ProviderId) ->
    case subscriptions:any_connection_active(ProviderId) of
        true ->
            true;
        false ->
            try
                % Sometimes it may happen that there is no websocket connection
                % but the worker is fully operational. For example, when the
                % connection has timed out and provider hasn't reconnected yet.
                % In such case, make sure it is really inoperable by making
                % a http request. Use low connection timeout so as not to delay
                % the calling process to much.
                {ok, #od_provider{
                    redirection_point = RedPoint
                }} = get(?ROOT, ProviderId),
                #{host := Host} = url_utils:parse(RedPoint),
                ConnCheckEndpoint = str_utils:format_bin("https://~s~s", [
                    Host, ?PROVIDER_ID_ENDPOINT
                ]),
                {ok, _, _, ProviderId} = http_client:get(
                    ConnCheckEndpoint, #{}, <<>>, [
                        insecure, {connect_timeout, 2000}
                    ]
                ),
                true
            catch _:_ ->
                false
            end
    end.
