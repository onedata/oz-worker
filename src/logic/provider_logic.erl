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


%% API
-export([create/1, modify/2]).
-export([get_data/1, get_spaces/1]).
-export([remove/1]).


%% create/1
%% ====================================================================
%% @doc Create a provider's account.
%% ====================================================================
-spec create(URL :: binary()) ->
    {ok, ProviderId :: binary()} | no_return().
%% ====================================================================
create(URL) ->
    ProviderId = logic_helper:save(#provider{url = URL}),
    {ok, ProviderId}.


%% modify/2
%% ====================================================================
%% @doc Modify provider's details.
%% ====================================================================
-spec modify(ProviderId :: binary(), URL :: binary()) ->
    ok | no_return().
%% ====================================================================
modify(ProviderId, URL) ->
    Doc = logic_helper:provider_doc(ProviderId),
    #veil_document{record = Provider} = Doc,
    ProviderNew = Provider#provider{url = URL},
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
    #provider{url = URL} = logic_helper:provider(ProviderId),
    {ok, [
        {providerId, ProviderId},
        {url, URL}
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
