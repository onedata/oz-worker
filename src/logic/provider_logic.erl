%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module implementing the business logic for space providers.
%%% This module serves as a buffer between the database and the REST API.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_logic).
-author("Konrad Zemek").

-include("gui/common.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

%% API
-export([create/4, create/5, modify/2, exists/1]).
-export([has_user/2, has_group/2]).
-export([get_effective_users/1, get_effective_groups/1]).
-export([get_data/1, get_spaces/1, get_url/1]).
-export([remove/1]).
-export([test_connection/1, check_provider_connectivity/1]).
-export([choose_provider_for_user/1]).
-export([list/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Create a provider's account.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec create(ClientName :: binary(), URLs :: [binary()],
    RedirectionPoint :: binary(), CSR :: binary()) ->
    {ok, ProviderId :: binary(), ProviderCertPem :: binary()}.
create(ClientName, URLs, RedirectionPoint, CSRBin) ->
    create(ClientName, URLs, RedirectionPoint, CSRBin, #{}).


%%--------------------------------------------------------------------
%% @doc Create a provider's account.
%% Throws exception when call to the datastore fails.
%% Accepts optional arguments map (which currently supports 'latitude' and
%% 'longitude' keys)
%% @end
%%--------------------------------------------------------------------
-spec create(ClientName :: binary(), URLs :: [binary()],
    RedirectionPoint :: binary(), CSR :: binary(),
    OptionalArgs :: #{atom() => term()}) ->
    {ok, ProviderId :: binary(), ProviderCertPem :: binary()}.
create(ClientName, URLs, RedirectionPoint, CSRBin, OptionalArgs) ->
    ProviderId = datastore_utils:gen_uuid(),
    {ok, {ProviderCertPem, Serial}} = worker_proxy:call(ozpca_worker,
        {sign_provider_req, ProviderId, CSRBin}),

    Latitude = maps:get(latitude, OptionalArgs, undefined),
    Longitude = maps:get(longitude, OptionalArgs, undefined),

    Provider = #od_provider{client_name = ClientName, urls = URLs,
        redirection_point = RedirectionPoint, serial = Serial,
        latitude = Latitude, longitude = Longitude},
    od_provider:save(#document{key = ProviderId, value = Provider}),

    {ok, ProviderId, ProviderCertPem}.

%%--------------------------------------------------------------------
%% @doc Modify provider's details.
%% Throws exception when call to the datastore fails, or provider doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec modify(ProviderId :: binary(), Data :: [proplists:property()]) ->
    ok.
modify(ProviderId, Data) ->
    {ok, _} = od_provider:update(ProviderId, fun(Provider) ->
        URLs = proplists:get_value(<<"urls">>, Data, Provider#od_provider.urls),
        RedirectionPoint = proplists:get_value(<<"redirectionPoint">>, Data, Provider#od_provider.redirection_point),
        ClientName = proplists:get_value(<<"clientName">>, Data, Provider#od_provider.client_name),
        Latitude = proplists:get_value(<<"latitude">>, Data, Provider#od_provider.latitude),
        Longitude = proplists:get_value(<<"longitude">>, Data, Provider#od_provider.longitude),

        {ok, Provider#od_provider{
            urls = URLs,
            redirection_point = RedirectionPoint,
            client_name = ClientName,
            latitude = Latitude,
            longitude = Longitude
        }}
    end),
    ok.

%%--------------------------------------------------------------------
%% @doc Returns whether a Provider exists.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec exists(ProviderId :: binary()) ->
    boolean().
exists(ProviderId) ->
    od_provider:exists(ProviderId).

%%--------------------------------------------------------------------
%% @doc Get provider's details.
%% Throws exception when call to the datastore fails, or provider doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_data(ProviderId :: binary()) ->
    {ok, Data :: [proplists:property()]}.
get_data(ProviderId) ->
    {ok, #document{value = #od_provider{
        client_name = ClientName,
        urls = URLs,
        redirection_point = RedirectionPoint,
        latitude = Latitude,
        longitude = Longitude
    }}} = od_provider:get(ProviderId),

    {ok, [
        {clientName, ClientName},
        {providerId, ProviderId},
        {urls, URLs},
        {redirectionPoint, RedirectionPoint},
        {latitude, Latitude},
        {longitude, Longitude}
    ]}.

%%--------------------------------------------------------------------
%% @doc Get Spaces supported by the provider.
%% Throws exception when call to the datastore fails, or provider doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(ProviderId :: binary()) ->
    {ok, Data :: [proplists:property()]}.
get_spaces(ProviderId) ->
    {ok, #document{
        value = #od_provider{
            spaces = Spaces
        }}} = od_provider:get(ProviderId),
    {ok, [{spaces, Spaces}]}.

%%--------------------------------------------------------------------
%% @doc Get Users effectively supported by the provider (sum of all users of
%% spaces that it supports).
%% Throws exception when call to the datastore fails, or provider doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_users(ProviderId :: binary()) ->
    {ok, Data :: [proplists:property()]}.
get_effective_users(ProviderId) ->
    {ok, #document{
        value = #od_provider{
            spaces = Spaces
        }}} = od_provider:get(ProviderId),
    Users = lists:foldl(
        fun(SpaceId, Acc) ->
            {ok, [{users, SpUsers}]} = space_logic:get_effective_users(SpaceId),
            ordsets:union([Acc, ordsets:from_list(SpUsers)])
        end, [], Spaces),
    {ok, [{users, Users}]}.

%%--------------------------------------------------------------------
%% @doc Get Users effectively supported by the provider (sum of all users of
%% spaces that it supports).
%% Throws exception when call to the datastore fails, or provider doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_effective_groups(ProviderId :: binary()) ->
    {ok, Data :: [proplists:property()]}.
get_effective_groups(ProviderId) ->
    {ok, #document{
        value = #od_provider{
            spaces = Spaces
        }}} = od_provider:get(ProviderId),
    Groups = lists:foldl(
        fun(SpaceId, Acc) ->
            {ok, [{groups, SpaceGroups}]} = space_logic:get_effective_groups(
                SpaceId
            ),
            ordsets:union([Acc, ordsets:from_list(SpaceGroups)])
        end, [], Spaces),
    {ok, [{groups, Groups}]}.

%%--------------------------------------------------------------------
%% @doc Returns full provider URL.
%% @end
%%--------------------------------------------------------------------
-spec get_url(ProviderId :: binary()) ->
    {ok, ProviderURL :: binary()}.
get_url(ProviderId) ->
    {ok, PData} = provider_logic:get_data(ProviderId),
    RedirectionPoint = proplists:get_value(redirectionPoint, PData),
    #hackney_url{host = Host, port = Port} =
        hackney_url:parse_url(RedirectionPoint),
    URL = str_utils:format_bin("https://~s:~B", [Host, Port]),
    {ok, URL}.


%%--------------------------------------------------------------------
%% @doc Remove provider's account.
%% Throws exception when call to the datastore fails, or provider is already removed.
%% @end
%%--------------------------------------------------------------------
-spec remove(ProviderId :: binary()) ->
    true.
remove(ProviderId) ->
    {ok, #document{value = #od_provider{spaces = Spaces, serial = Serial}}} = od_provider:get(ProviderId),

    lists:foreach(fun(SpaceId) ->
        {ok, _} = od_space:update(SpaceId, fun(Space) ->
            #od_space{providers_supports = Supports} = Space,
            {ok, Space#od_space{
                providers_supports = proplists:delete(ProviderId, Supports)
            }}
        end)
    end, Spaces),

    worker_proxy:call(ozpca_worker, {revoke, Serial}),
    case (od_provider:delete(ProviderId)) of
        ok -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Tests connection to given url.
%% @end
%%--------------------------------------------------------------------
-spec test_connection(ToCheck :: any()) ->
    {ok, [{ServiceName :: binary(), Status :: ok | error}]} | {error, bad_data}.
test_connection(ToCheck) ->
    test_connection(ToCheck, []).

%%--------------------------------------------------------------------
%% @private
%% @doc Tests connection to given url.
%% @end
%%--------------------------------------------------------------------
-spec test_connection(ToCheck :: any(),
    Acc :: [{ServiceName :: binary(), Status :: ok | error}]) ->
    {ok, [{ServiceName :: binary(), Status :: ok | error}]} | {error, bad_data}.
test_connection([], Acc) ->
    {ok, lists:reverse(Acc)};
test_connection([{<<"undefined">>, <<Url/binary>>} | Rest], Acc) ->
    ConnStatus = case http_client:get(Url, [], <<>>, [insecure]) of
        {ok, 200, _, _} -> ok;
        _ -> error
    end,
    test_connection(Rest, [{Url, ConnStatus} | Acc]);
test_connection([{<<ServiceName/binary>>, <<Url/binary>>} | Rest], Acc) ->
    ConnStatus =
        case http_client:get(Url, [], <<>>, [insecure]) of
            {ok, 200, _, ServiceName} ->
                ok;
            Error ->
                ?debug("Checking connection to ~p failed with error: ~n~p",
                    [Url, Error]),
                error
        end,
    test_connection(Rest, [{Url, ConnStatus} | Acc]);
test_connection(_, _) ->
    {error, bad_data}.


%%--------------------------------------------------------------------
%% @doc Returns whether the user identified by UserId is supported by this
%% provider, i.e. the provider supports a space where this user belongs.
%% Shall return false in any other case (provider doesn't exist, etc).
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec has_user(ProviderId :: od_provider:id(), UserId :: od_user:id()) ->
    boolean().
has_user(ProviderId, UserId) ->
    case od_provider:get(ProviderId) of
        {error, {not_found, _}} ->
            false;
        {ok, #document{value = #od_provider{spaces = Spaces}}} ->
            lists:any(
                fun(SpaceId) ->
                    space_logic:has_effective_user(SpaceId, UserId)
                end, Spaces)
    end.


%%--------------------------------------------------------------------
%% @doc Returns whether the group identified by GroupId is supported by this
%% provider, i.e. the provider supports a space where this group belongs.
%% Shall return false in any other case (provider doesn't exist, etc).
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec has_group(ProviderId :: od_provider:id(), GroupId :: od_group:id()) ->
    boolean().
has_group(ProviderId, GroupId) ->
    case od_provider:get(ProviderId) of
        {error, {not_found, _}} ->
            false;
        {ok, #document{value = #od_provider{spaces = Spaces}}} ->
            lists:any(
                fun(SpaceId) ->
                    space_logic:has_effective_group(SpaceId, GroupId)
                end, Spaces)
    end.


% Checks if given provider (by ID) is alive and responding.
-spec check_provider_connectivity(ProviderId :: binary()) -> boolean().
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
                {ok, Data} = provider_logic:get_data(ProviderId),
                RedirectionPoint = proplists:get_value(redirectionPoint, Data),
                #hackney_url{
                    host = Host
                } = hackney_url:parse_url(RedirectionPoint),
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
    {ok, ProviderID :: binary()} | {error, no_provider}.
choose_provider_for_user(UserID) ->
    % Check if the user has a default space and if it is supported.
    {ok, [{spaces, Spaces}, {default, DefaultSpace}]} =
        user_logic:get_spaces(UserID),
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
