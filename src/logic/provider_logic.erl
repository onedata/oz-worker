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
-include_lib("dao/include/common.hrl").


%% API
-export([create/1, modify/2]).
-export([get_data/1, get_spaces/1]).
-export([remove/1]).


%% create/1
%% ====================================================================
%% @doc Create a provider's account.
%% Throws exception when call to dao fails.
%% @end
%% ====================================================================
-spec create(URL :: binary()) ->
    {ok, ProviderId :: binary()} | no_return().
%% ====================================================================
create(URL) ->
    ProviderId = dao_adapter:save(#provider{url = URL}),
    {ok, ProviderId}.


%% modify/2
%% ====================================================================
%% @doc Modify provider's details.
%% Throws exception when call to dao fails, or provider doesn't exist.
%% @end
%% ====================================================================
-spec modify(ProviderId :: binary(), URL :: binary()) ->
    ok | no_return().
%% ====================================================================
modify(ProviderId, URL) ->
    Doc = dao_adapter:provider_doc(ProviderId),
    #veil_document{record = Provider} = Doc,
    ProviderNew = Provider#provider{url = URL},
    dao_adapter:save(Doc#veil_document{record = ProviderNew}),
    ok.


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
    #provider{url = URL} = dao_adapter:provider(ProviderId),
    {ok, [
        {providerId, ProviderId},
        {url, URL}
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
