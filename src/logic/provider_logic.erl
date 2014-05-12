%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: The module implementing the business logic for space providers.
%% This module serves as a buffer between the database and the REST API.
%% @end
%% ===================================================================
-module(provider_logic).
-author("Konrad Zemek").

%% API
-export([register_provider/1, get_data/1, modify_data/2, support_space/2,
    unregister_provider/1]).


-spec register_provider(URL :: binary()) -> {ok, binary()} | {error, any()}.
register_provider(URL) ->
    {ok, <<"providerid">>}.


-spec get_data(ProviderId :: binary()) -> {ok, [proplists:property()]} | {error, any()}.
get_data(ProviderId) ->
    {ok, [{<<"name">>, <<"lolprovider">>}]}.


-spec modify_data(ProviderId :: binary(), Modifications :: [proplists:property()]) -> ok | {error, any()}.
modify_data(ProviderId, Modifications) ->
    ok.


-spec support_space(ProviderId :: binary(), Token :: binary()) -> {ok, binary()} | {error, any()}.
support_space(ProviderId, Token) ->
    {ok, <<"spaceid">>}.


-spec unregister_provider(ProviderId :: binary()) -> ok | {error, any()}.
unregister_provider(ProviderId) ->
    ok.
