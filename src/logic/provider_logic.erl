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


%% API
-export([create/1, modify/2, get_data/1, get_spaces/1, remove/1]).


%% create/1
%% ====================================================================
%% @doc Create a provider's account.
%% ====================================================================
-spec create(URL :: binary()) ->
    {ok, ProviderId :: binary()} | {error, Reason :: any()}.
%% ====================================================================
create(URL) ->
    {ok, <<"ProviderId">>}.


%% modify/2
%% ====================================================================
%% @doc Modify provider's details.
%% ====================================================================
-spec modify(ProviderId :: binary(), URL :: binary()) ->
    ok | {error, Reason :: any()}.
%% ====================================================================
modify(ProviderId, URL) ->
    ok.


%% get_data/1
%% ====================================================================
%% @doc Get provider's details.
%% ====================================================================
-spec get_data(ProviderId :: binary()) ->
    {ok, Data :: [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_data(ProviderId) ->
    {ok, [{url, <<"providerurl">>}]].


%% get_spaces/1
%% ====================================================================
%% @doc Get Spaces supported by the provider.
%% ====================================================================
-spec get_spaces(ProviderId :: binary()) ->
    {ok, Data :: [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_spaces(ProviderId) ->
    {ok, [{spaces, [<<"space">>]}]}.


%% remove/1
%% ====================================================================
%% @doc Remove provider's account.
%% ====================================================================
-spec remove(ProviderId :: binary()) -> boolean().
%% ====================================================================
remove(ProviderId) ->
    true.
