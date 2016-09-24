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

%% data_backend_behaviour callbacks
-export([init/0]).
-export([find/2, find_all/1, find_query/2]).
-export([create_record/2, update_record/3, delete_record/2]).
%% API
-export([space_record/4, space_record/5]).


%%%===================================================================
%%% data_backend_behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback init/0.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback find/2.
%% @end
%%--------------------------------------------------------------------
-spec find(ResourceType :: binary(), Id :: binary()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
find(<<"space">>, SpaceId) ->
    UserId = g_session:get_user_id(),
    % Check if the user belongs to this space
    case space_logic:has_effective_user(SpaceId, UserId) of
        false ->
            gui_error:unauthorized();
        true ->
            {ok, [{providers, UserProviders}]} = user_logic:get_providers(
                UserId
            ),
            {ok, #document{
                value = #onedata_user{
                    space_names = SpaceNamesMap,
                    default_space = DefaultSpaceId
                }}} = onedata_user:get(UserId),
            Res = space_record(
                SpaceId, SpaceNamesMap, DefaultSpaceId, UserProviders
            ),
            {ok, Res}
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback find_all/1.
%% @end
%%--------------------------------------------------------------------
-spec find_all(ResourceType :: binary()) ->
    {ok, [proplists:proplist()]} | gui_error:error_result().
find_all(<<"space">>) ->
    UserId = g_session:get_user_id(),
    {ok, UserSpaces} = user_logic:get_spaces(UserId),
    SpaceIds = proplists:get_value(spaces, UserSpaces),
    {ok, [{providers, UserProviders}]} = user_logic:get_providers(UserId),
    {ok, #document{
        value = #onedata_user{
            space_names = SpaceNamesMap,
            default_space = DefaultSpaceId
        }}} = onedata_user:get(UserId),
    Res = lists:map(
        fun(SpaceId) ->
            space_record(
                SpaceId,
                SpaceNamesMap,
                DefaultSpaceId,
                UserProviders
            )
        end, SpaceIds),
    {ok, Res}.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback find_query/2.
%% @end
%%--------------------------------------------------------------------
-spec find_query(ResourceType :: binary(), Data :: proplists:proplist()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
find_query(<<"space">>, _Data) ->
    gui_error:report_error(<<"Not iplemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback create_record/2.
%% @end
%%--------------------------------------------------------------------
-spec create_record(RsrcType :: binary(), Data :: proplists:proplist()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
create_record(<<"space">>, Data) ->
    Name = proplists:get_value(<<"name">>, Data),
    {ok, SpaceId} = space_logic:create({user, g_session:get_user_id()}, Name),
    NewSpaceData = [
        {<<"id">>, SpaceId},
        {<<"name">>, Name},
        {<<"isDefault">>, false},
        {<<"hasViewPrivilege">>, true},
        {<<"providers">>, []}
    ],
    {ok, NewSpaceData}.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback update_record/3.
%% @end
%%--------------------------------------------------------------------
-spec update_record(RsrcType :: binary(), Id :: binary(),
    Data :: proplists:proplist()) ->
    ok | gui_error:error_result().
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
%% {@link data_backend_behaviour} callback delete_record/2.
%% @end
%%--------------------------------------------------------------------
-spec delete_record(RsrcType :: binary(), Id :: binary()) ->
    ok | gui_error:error_result().
delete_record(<<"space">>, _Id) ->
    gui_error:report_error(<<"Not iplemented">>).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns a client-compliant space record.
%% @end
%%--------------------------------------------------------------------
-spec space_record(SpaceId :: binary(), SpaceNamesMap :: #{},
    DefaultSpaceId :: binary(), UserProviders :: [binary()],
    HasViewPrivileges :: boolean()) -> proplists:proplist().
space_record(SpaceId, SpaceNamesMap, DefaultSpaceId, UserProviders) ->
    % Check if that user has view privileges in that space
    HasViewPrivs = space_logic:has_effective_privilege(
        SpaceId, g_session:get_user_id(), space_view_data
    ),
    space_record(SpaceId, SpaceNamesMap, DefaultSpaceId, UserProviders,
        HasViewPrivs).

%%--------------------------------------------------------------------
%% @doc
%% Returns a client-compliant space record.
%% @end
%%--------------------------------------------------------------------
-spec space_record(SpaceId :: binary(), SpaceNamesMap :: #{},
    DefaultSpaceId :: binary(), UserProviders :: [binary()],
    HasViewPrivileges :: boolean()) -> proplists:proplist().
space_record(SpaceId, SpaceNamesMap, DefaultSpaceId, UserProviders,
    HasViewPrivileges) ->
    {ok, #document{value = #space{
        name = DefaultName,
        providers_supports = ProvidersSupports
    }}} = space:get(SpaceId),
    % Try to get space name from personal user's mapping, if not use its
    % default name.
    Name = maps:get(SpaceId, SpaceNamesMap, DefaultName),
    case HasViewPrivileges of
        false ->
            [
                {<<"id">>, SpaceId},
                {<<"name">>, Name},
                {<<"isDefault">>, SpaceId =:= DefaultSpaceId},
                {<<"hasViewPrivilege">>, false},
                {<<"providers">>, []}
            ];
        true ->
            {Providers, _} = lists:unzip(ProvidersSupports),
            ProvidersToDisplay = lists:filter(
                fun(Provider) ->
                    lists:member(Provider, UserProviders)
                end, Providers),
            [
                {<<"id">>, SpaceId},
                {<<"name">>, Name},
                {<<"isDefault">>, SpaceId =:= DefaultSpaceId},
                {<<"hasViewPrivilege">>, true},
                {<<"providers">>, ProvidersToDisplay}
            ]
    end.