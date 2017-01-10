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
-export([init/0, terminate/0]).
-export([find/2, find_all/1, find_query/2]).
-export([create_record/2, update_record/3, delete_record/2]).
%% API
-export([space_record/2, space_record/3]).


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
%% {@link data_backend_behaviour} callback terminate/0.
%% @end
%%--------------------------------------------------------------------
-spec terminate() -> ok.
terminate() ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback find/2.
%% @end
%%--------------------------------------------------------------------
-spec find(ResourceType :: binary(), Id :: binary()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
find(<<"space">>, SpaceId) ->
    UserId = gui_session:get_user_id(),
    % Check if the user belongs to this space
    case space_logic:has_effective_user(SpaceId, UserId) of
        false ->
            gui_error:unauthorized();
        true ->
            {ok, space_record(SpaceId, UserId)}
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback find_all/1.
%% @end
%%--------------------------------------------------------------------
-spec find_all(ResourceType :: binary()) ->
    {ok, [proplists:proplist()]} | gui_error:error_result().
find_all(<<"space">>) ->
    gui_error:report_error(<<"Not implemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback find_query/2.
%% @end
%%--------------------------------------------------------------------
-spec find_query(ResourceType :: binary(), Data :: proplists:proplist()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
find_query(<<"space">>, _Data) ->
    gui_error:report_error(<<"Not implemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback create_record/2.
%% @end
%%--------------------------------------------------------------------
-spec create_record(RsrcType :: binary(), Data :: proplists:proplist()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
create_record(<<"space">>, Data) ->
    Name = proplists:get_value(<<"name">>, Data),
    case Name of
        <<"">> ->
            gui_error:report_error(<<"Empty space names are not allowed">>);
        Bin when is_binary(Bin) ->
            UserId = gui_session:get_user_id(),
            {ok, SpaceId} = space_logic:create({user, UserId}, Name),
            % Push user record with a new space list.
            gui_async:push_updated(
                <<"user">>, user_data_backend:user_record(UserId)
            ),
            {ok, space_record(SpaceId, UserId)};
        _ ->
            gui_error:report_error(<<"Invalid space name">>)
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback update_record/3.
%% @end
%%--------------------------------------------------------------------
-spec update_record(RsrcType :: binary(), Id :: binary(),
    Data :: proplists:proplist()) ->
    ok | gui_error:error_result().
update_record(<<"space">>, SpaceId, [{<<"name">>, NewName}]) ->
    UserId = gui_session:get_user_id(),
    space_logic:modify(SpaceId, {user, UserId}, NewName);
update_record(<<"space">>, _SpaceId, _Data) ->
    gui_error:report_error(<<"Not implemented">>).



%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback delete_record/2.
%% @end
%%--------------------------------------------------------------------
-spec delete_record(RsrcType :: binary(), Id :: binary()) ->
    ok | gui_error:error_result().
delete_record(<<"space">>, _Id) ->
    gui_error:report_error(<<"Not implemented">>).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns a client-compliant space record based on space id. Automatically
%% check if the user has view privileges in that space and returns proper data.
%% @end
%%--------------------------------------------------------------------
-spec space_record(SpaceId :: binary(), UserId :: binary()) ->
    proplists:proplist().
space_record(SpaceId, UserId) ->
    % Check if that user has view privileges in that space
    HasViewPrivs = space_logic:has_effective_privilege(
        SpaceId, UserId, space_view_data
    ),
    space_record(SpaceId, UserId, HasViewPrivs).


%%--------------------------------------------------------------------
%% @doc
%% Returns a client-compliant space record based on space id. Allows to
%% override HasViewPrivileges.
%% @end
%%--------------------------------------------------------------------
-spec space_record(SpaceId :: binary(), UserId :: od_user:id(),
    HasViewPrivileges :: boolean()) -> proplists:proplist().
space_record(SpaceId, UserId, HasViewPrivileges) ->
    {ok, #document{value = #od_space{
        name = DefaultName,
        providers_supports = ProvidersSupports
    }}} = od_space:get(SpaceId),
    {ok, #document{value = #od_user{
        space_aliases = SpaceNamesMap
    }}} = od_user:get(UserId),
    % Try to get space name from personal user's mapping, if not use its
    % default name.
    Name = maps:get(SpaceId, SpaceNamesMap, DefaultName),
    {Providers, SupportSizes} = lists:unzip(ProvidersSupports),
    TotalSize = lists:sum(SupportSizes),
    [
        {<<"id">>, SpaceId},
        {<<"name">>, Name},
        {<<"hasViewPrivilege">>, HasViewPrivileges},
        {<<"totalSize">>, TotalSize},
        {<<"supportSizes">>, ProvidersSupports},
        {<<"providers">>, Providers}
    ].
