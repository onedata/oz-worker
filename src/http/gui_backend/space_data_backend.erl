%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements data_backend_behaviour and is used to synchronize
%%% the `space` model used in Ember application.
%%% @end
%%%-------------------------------------------------------------------
-module(space_data_backend).
-author("Lukasz Opiola").
-behaviour(data_backend_behaviour).

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([init/0]).
-export([find/2, find_all/1, find_query/2]).
-export([create_record/2, update_record/3, delete_record/2]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback init/0.
%% @end
%%--------------------------------------------------------------------
init() ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback find/2.
%% @end
%%--------------------------------------------------------------------
find(<<"space">>, SpaceIds) ->
    UserId = g_session:get_user_id(),
    {ok, GetSpaces} = user_logic:get_spaces(UserId),
    Default = proplists:get_value(default, GetSpaces),
    Res = lists:map(
        fun(SpaceId) ->
            {ok, SpaceData} = space_logic:get_data(SpaceId, provider),
            Name = proplists:get_value(name, SpaceData),
            {ok, [{providers, Providers}]} =
                space_logic:get_providers(SpaceId, provider),
            [
                {<<"id">>, SpaceId},
                {<<"name">>, Name},
                {<<"isDefault">>, SpaceId =:= Default},
                {<<"providers">>, Providers}
            ]
        end, SpaceIds),
    {ok, Res}.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback find_query/2.
%% @end
%%--------------------------------------------------------------------
find_query(<<"space">>, _Data) ->
    {error, <<"Not implemented">>}.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback find_all/1.
%% @end
%%--------------------------------------------------------------------
find_all(<<"space">>) ->
    UserId = g_session:get_user_id(),
    {ok, GetSpaces} = user_logic:get_spaces(UserId),
    SpaceIds = proplists:get_value(spaces, GetSpaces),
    {ok, _Res} = find(<<"space">>, SpaceIds).


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback create_record/2.
%% @end
%%--------------------------------------------------------------------
create_record(<<"space">>, Data) ->
    Name = proplists:get_value(<<"name">>, Data),
    {ok, SpaceId} = space_logic:create({user, g_session:get_user_id()}, Name),
    NewSpaceData = [
        {<<"id">>, SpaceId},
        {<<"name">>, Name},
        {<<"isDefault">>, false},
        {<<"providers">>, []}
    ],
    {ok, NewSpaceData}.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback update_record/3.
%% @end
%%--------------------------------------------------------------------
update_record(<<"space">>, SpaceId, Data) ->
    UserId = g_session:get_user_id(),
    IsDefault = proplists:get_value(<<"isDefault">>, Data),
    case IsDefault of
        true ->
            user_logic:set_default_space(UserId, SpaceId);
        false ->
            ok
    end,
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback delete_record/2.
%% @end
%%--------------------------------------------------------------------
delete_record(<<"space">>, _Id) ->
    {error, <<"Not implemented">>}.
