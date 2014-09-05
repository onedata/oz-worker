%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The module implementing the business logic for space providers.
%% This module serves as a buffer between the database and the REST API.
%% @end
%% ===================================================================
-module(provider_logic).
-author("Konrad Zemek").

-include("dao/dao_types.hrl").
-include("registered_names.hrl").

-include_lib("dao/include/common.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([create/3, modify/2, exists/1]).
-export([get_data/1, get_spaces/1]).
-export([remove/1]).
-export([test_connection/1]).


%% create/1
%% ====================================================================
%% @doc Create a provider's account.
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec create(URLs :: [binary()], RedirectionPoint :: binary(), CSR :: binary()) ->
    {ok, ProviderId :: binary(), ProviderCertPem :: binary()} | no_return().
%% ====================================================================
create(URLs, RedirectionPoint, CSRBin) ->
    ProviderId = dao_adapter:save(#provider{urls = URLs, redirection_point = RedirectionPoint}),
    {ok, ProviderCertPem} = grpca:sign_provider_req(ProviderId, CSRBin),
    {ok, ProviderId, ProviderCertPem}.


%% modify/2
%% ====================================================================
%% @doc Modify provider's details.
%% Throws exception when call to dao fails, or provider doesn't exist.
%% @end
%% ====================================================================
-spec modify(ProviderId :: binary(), Data :: [proplists:property()]) ->
    ok | no_return().
%% ====================================================================
modify(ProviderId, Data) ->
    Doc = dao_adapter:provider_doc(ProviderId),
    #veil_document{record = Provider} = Doc,

    URLs = proplists:get_value(<<"urls">>, Data, Provider#provider.urls),
    RedirectionPoint = proplists:get_value(<<"redirectionPoint">>, Data, Provider#provider.redirection_point),

    ProviderNew = Provider#provider{urls = URLs, redirection_point = RedirectionPoint},
    dao_adapter:save(Doc#veil_document{record = ProviderNew}),
    ok.


%% exists/1
%% ====================================================================
%% @doc Returns whether a Provider exists.
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec exists(ProviderId :: binary()) -> boolean().
%% ====================================================================
exists(ProviderId) ->
    dao_adapter:provider_exists(ProviderId).


%% get_data/1
%% ====================================================================
%% @doc Get provider's details.
%% Throws exception when call to dao fails, or provider doesn't exist.
%% @end
%% ====================================================================
-spec get_data(ProviderId :: binary()) ->
    {ok, Data :: [proplists:property()]} | no_return().
%% ====================================================================
get_data(ProviderId) ->
    #provider{urls = URLs, redirection_point = RedirectionPoint} = dao_adapter:provider(ProviderId),
    {ok, [
        {providerId, ProviderId},
        {urls, URLs},
        {redirectionPoint, RedirectionPoint}
    ]}.


%% get_spaces/1
%% ====================================================================
%% @doc Get Spaces supported by the provider.
%% Throws exception when call to dao fails, or provider doesn't exist.
%% @end
%% ====================================================================
-spec get_spaces(ProviderId :: binary()) ->
    {ok, Data :: [proplists:property()]} | no_return().
%% ====================================================================
get_spaces(ProviderId) ->
    #provider{spaces = Spaces} = dao_adapter:provider(ProviderId),
    {ok, [{spaces, Spaces}]}.


%% remove/1
%% ====================================================================
%% @doc Remove provider's account.
%% Throws exception when call to dao fails, or provider is already removed.
%% @end
%% ====================================================================
-spec remove(ProviderId :: binary()) -> true | no_return().
%% ====================================================================
remove(ProviderId) ->
    #provider{spaces = Spaces} = dao_adapter:provider(ProviderId),

    lists:foreach(fun(SpaceId) ->
        SpaceDoc = dao_adapter:space_doc(SpaceId),
        #veil_document{record = #space{providers = Providers} = Space} = SpaceDoc,
        SpaceNew = Space#space{providers = lists:delete(ProviderId, Providers)},
        dao_adapter:save(SpaceDoc#veil_document{record = SpaceNew})
    end, Spaces),

    dao_adapter:provider_remove(ProviderId).

%% test_connection/1
%% ====================================================================
%% @doc Tests connection to given url, returns <<"ok">> or <<"error">> status for each element
%% ====================================================================
-spec test_connection(ToCheck :: list({ServiceName :: binary(),Url :: binary()})) -> list(ConnStatus) when
    ConnStatus :: {ServiceName :: binary(), Status :: binary()}.
%% ====================================================================
test_connection([]) ->
    [];
test_connection([ {<<"undefined">>,Url} | Rest]) ->
    UrlString = binary_to_list(Url),
    ConnStatus = case ibrowse:send_req(UrlString,[],get) of
                     {ok, "200", _, _} ->
                         <<"ok">>;
                     _ ->
                         <<"error">>
                 end,
    [{Url,ConnStatus} | test_connection(Rest)];
test_connection([ {ServiceName,Url} | Rest]) ->
    UrlString = binary_to_list(Url),
    ServiceNameString = binary_to_list(ServiceName),
    ConnStatus = case ibrowse:send_req(UrlString,[],get) of
        {ok, "200", _, ServiceNameString} ->
            <<"ok">>;
        Error ->
            ?debug("Checking connection to ~p failed with error: ~n~p",[Url,Error]),
            <<"error">>
    end,
    [{Url,ConnStatus} | test_connection(Rest)].
