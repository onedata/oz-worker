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

-include("dao/dao_types.hrl").
-include("registered_names.hrl").

-include_lib("dao/include/common.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([create/4, modify/2, exists/1]).
-export([get_data/1, get_spaces/1]).
-export([remove/1]).
-export([test_connection/1]).
-export([get_default_provider_for_user/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Create a provider's account.
%% Throws exception when call to dao fails.
%% @end
%%--------------------------------------------------------------------
-spec create(ClientName :: binary(), URLs :: [binary()],
    RedirectionPoint :: binary(), CSR :: binary()) ->
    {ok, ProviderId :: binary(), ProviderCertPem :: binary()}.
create(ClientName, URLs, RedirectionPoint, CSRBin) ->
    ProviderId = dao_helper:gen_uuid(),
    BinProviderId = str_utils:to_binary(ProviderId),
    {ok, ProviderCertPem, Serial} = grpca:sign_provider_req(BinProviderId, CSRBin),
    dao_adapter:save(#db_document{uuid = ProviderId, record =
    #provider{client_name = ClientName, urls = URLs,
        redirection_point = RedirectionPoint, serial = Serial}}),

    {ok, BinProviderId, ProviderCertPem}.

%%--------------------------------------------------------------------
%% @doc Modify provider's details.
%% Throws exception when call to dao fails, or provider doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec modify(ProviderId :: binary(), Data :: [proplists:property()]) ->
    ok.
modify(ProviderId, Data) ->
    Doc = dao_adapter:provider_doc(ProviderId),
    #db_document{record = Provider} = Doc,
    ?alert("ProviderId: ~p~n", [ProviderId]),
    ?alert("Doc: ~p~n", [Doc]),

    URLs = proplists:get_value(<<"urls">>, Data, Provider#provider.urls),
    RedirectionPoint = proplists:get_value(<<"redirectionPoint">>, Data, Provider#provider.redirection_point),
    ClientName = proplists:get_value(<<"clientName">>, Data, Provider#provider.client_name),

    ProviderNew = Provider#provider{urls = URLs, redirection_point = RedirectionPoint, client_name = ClientName},
    ?alert("ProviderNew: ~p~n", [ProviderNew]),
    Res = dao_adapter:save(Doc#db_document{record = ProviderNew}),
    ?alert("Modify: ~p~n", [Res]),
    ok.

%%--------------------------------------------------------------------
%% @doc Returns whether a Provider exists.
%% Throws exception when call to dao fails.
%% @end
%%--------------------------------------------------------------------
-spec exists(ProviderId :: binary()) ->
    boolean().
exists(ProviderId) ->
    dao_adapter:provider_exists(ProviderId).

%%--------------------------------------------------------------------
%% @doc Get provider's details.
%% Throws exception when call to dao fails, or provider doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_data(ProviderId :: binary()) ->
    {ok, Data :: [proplists:property()]}.
get_data(ProviderId) ->
    #provider{
        client_name = ClientName,
        urls = URLs,
        redirection_point = RedirectionPoint} = dao_adapter:provider(ProviderId),

    {ok, [
        {clientName, ClientName},
        {providerId, ProviderId},
        {urls, URLs},
        {redirectionPoint, RedirectionPoint}
    ]}.

%%--------------------------------------------------------------------
%% @doc Get Spaces supported by the provider.
%% Throws exception when call to dao fails, or provider doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(ProviderId :: binary()) ->
    {ok, Data :: [proplists:property()]}.
get_spaces(ProviderId) ->
    #provider{spaces = Spaces} = dao_adapter:provider(ProviderId),
    {ok, [{spaces, Spaces}]}.

%%--------------------------------------------------------------------
%% @doc Remove provider's account.
%% Throws exception when call to dao fails, or provider is already removed.
%% @end
%%--------------------------------------------------------------------
-spec remove(ProviderId :: binary()) ->
    true.
remove(ProviderId) ->
    #provider{spaces = Spaces, serial = Serial} = dao_adapter:provider(ProviderId),

    lists:foreach(fun(SpaceId) ->
        SpaceDoc = dao_adapter:space_doc(SpaceId),
        #db_document{record = #space{providers = Providers, size = Size} = Space} = SpaceDoc,
        SpaceNew = Space#space{providers = lists:delete(ProviderId, Providers), size = proplists:delete(ProviderId, Size)},
        dao_adapter:save(SpaceDoc#db_document{record = SpaceNew}),
        op_channel_logic:space_modified(SpaceNew#space.providers, SpaceId, SpaceNew),
        op_channel_logic:space_removed([ProviderId], SpaceId)
    end, Spaces),

    grpca:revoke(Serial),
    dao_adapter:provider_remove(ProviderId).

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
%% @doc Returns provider id of provider that has been chosen
%% as default for given user, or {error, no_provider} otherwise.
%% If the user has a default spaces and it is supported by some providers,
%% one of them will be chosen randomly.
%% Otherwise, if any of user spaces is supported by any provider,
%% one of them will be chosen randomly.
%% @end
%%--------------------------------------------------------------------
-spec get_default_provider_for_user(Referer :: binary() | undefined) ->
    {ok, ProviderID :: binary()} | {error, no_provider}.
get_default_provider_for_user(UserID) ->
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
