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

-include_lib("public_key/include/public_key.hrl").

%% API
-export([create/3, modify/2]).
-export([get_data/1, get_spaces/1]).
-export([remove/1]).
-export([test_connection/1]).


%% create/1
%% ====================================================================
%% @doc Create a provider's account.
%% ====================================================================
-spec create(URLs :: [binary()], RedirectionPoint :: binary(), CSR :: binary()) ->
    {ok, ProviderId :: binary(), ProviderCertPem :: binary()} | no_return().
%% ====================================================================
create(URLs, RedirectionPoint, CSRBin) ->
    ProviderId = logic_helper:save(#provider{urls = URLs, redirection_point = RedirectionPoint}),
    {ok, ProviderCertPem} = grpca:sign_provider_req(ProviderId, CSRBin),
    {ok, ProviderId, ProviderCertPem}.


%% modify/2
%% ====================================================================
%% @doc Modify provider's details.
%% ====================================================================
-spec modify(ProviderId :: binary(), Data :: [proplists:property()]) ->
    ok | no_return().
%% ====================================================================
modify(ProviderId, Data) ->
    Doc = logic_helper:provider_doc(ProviderId),
    #veil_document{record = Provider} = Doc,

    URLs = proplists:get_value(<<"urls">>, Data, Provider#provider.urls),
    RedirectionPoint = proplists:get_value(<<"redirectionPoint">>, Data, Provider#provider.redirection_point),

    ProviderNew = Provider#provider{urls = URLs, redirection_point = RedirectionPoint},
    logic_helper:save(Doc#veil_document{record = ProviderNew}),
    ok.


%% get_data/1
%% ====================================================================
%% @doc Get provider's details.
%% ====================================================================
-spec get_data(ProviderId :: binary()) ->
    {ok, Data :: [proplists:property()]} | no_return().
%% ====================================================================
get_data(ProviderId) ->
    #provider{urls = URLs, redirection_point = RedirectionPoint} = logic_helper:provider(ProviderId),
    {ok, [
        {providerId, ProviderId},
        {urls, URLs},
        {redirectionPoint, RedirectionPoint}
    ]}.


%% get_spaces/1
%% ====================================================================
%% @doc Get Spaces supported by the provider.
%% ====================================================================
-spec get_spaces(ProviderId :: binary()) ->
    {ok, Data :: [proplists:property()]} | no_return().
%% ====================================================================
get_spaces(ProviderId) ->
    #provider{spaces = Spaces} = logic_helper:provider(ProviderId),
    {ok, [{spaces, Spaces}]}.


%% remove/1
%% ====================================================================
%% @doc Remove provider's account.
%% ====================================================================
-spec remove(ProviderId :: binary()) -> boolean().
%% ====================================================================
remove(ProviderId) ->
    #provider{spaces = Spaces} = logic_helper:provider(ProviderId),

    lists:foreach(fun(SpaceId) ->
        SpaceDoc = logic_helper:space_doc(SpaceId),
        #veil_document{record = #space{providers = Providers} = Space} = SpaceDoc,
        SpaceNew = Space#space{providers = lists:delete(ProviderId, Providers)},
        logic_helper:save(SpaceDoc#veil_document{record = SpaceNew})
    end, Spaces),

    logic_helper:provider_remove(ProviderId).

%% test_connection/1
%% ====================================================================
%% @doc Tests connection to given url, returns <<"ok">> or <<"error">> status for each element
%% ====================================================================
-spec test_connection(ToCheck :: list({ServiceName :: binary(),Url :: binary()})) -> list(ConnStatus) when
    ConnStatus :: {ServiceName :: binary(), Status :: binary()}.
%% ====================================================================
test_connection([]) ->
    [];
test_connection([ {ServiceName,Url} | Rest]) ->
    UrlString = binary_to_list(Url),
    ServiceNameString = binary_to_list(ServiceName),
    ConnStatus = case ibrowse:send_req(UrlString,[],get) of
        {ok, "200", _, ServiceNameString} ->
            <<"ok">>;
        Error ->
            lager:info("Checking connection to ~p failed with error: ~n~p",[Url,Error]),
            <<"error">>
    end,
    [{ServiceName,ConnStatus} | test_connection(Rest)].
