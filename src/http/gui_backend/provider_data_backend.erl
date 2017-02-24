%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements data_backend_behaviour and is used to synchronize
%%% the `provider` model used in Ember application.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_data_backend).
-author("Lukasz Opiola").
-behaviour(data_backend_behaviour).

-include("datastore/oz_datastore_models_def.hrl").
-include("gui/common.hrl").
-include_lib("ctool/include/logging.hrl").

%% data_backend_behaviour callbacks
-export([init/0, terminate/0]).
-export([find_record/2, find_all/1, query/2, query_record/2]).
-export([create_record/2, update_record/3, delete_record/2]).
%% API
-export([provider_record/2]).


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
%% {@link data_backend_behaviour} callback find_record/2.
%% @end
%%--------------------------------------------------------------------
-spec find_record(ResourceType :: binary(), Id :: binary()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
find_record(<<"provider">>, ProviderId) ->
    UserId = gui_session:get_user_id(),
    Res = provider_record(ProviderId, UserId),
    {ok, Res}.


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback find_all/1.
%% @end
%%--------------------------------------------------------------------
-spec find_all(ResourceType :: binary()) ->
    {ok, [proplists:proplist()]} | gui_error:error_result().
find_all(<<"provider">>) ->
    gui_error:report_error(<<"Not implemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback query/2.
%% @end
%%--------------------------------------------------------------------
-spec query(ResourceType :: binary(), Data :: proplists:proplist()) ->
    {ok, [proplists:proplist()]} | gui_error:error_result().
query(<<"provider">>, _Data) ->
    gui_error:report_error(<<"Not implemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback query_record/2.
%% @end
%%--------------------------------------------------------------------
-spec query_record(ResourceType :: binary(), Data :: proplists:proplist()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
query_record(<<"provider">>, _Data) ->
    gui_error:report_error(<<"Not implemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback create_record/2.
%% @end
%%--------------------------------------------------------------------
-spec create_record(RsrcType :: binary(), Data :: proplists:proplist()) ->
    {ok, proplists:proplist()} | gui_error:error_result().
create_record(<<"provider">>, _Data) ->
    gui_error:report_error(<<"Not implemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback update_record/3.
%% @end
%%--------------------------------------------------------------------
-spec update_record(RsrcType :: binary(), Id :: binary(),
    Data :: proplists:proplist()) ->
    ok | gui_error:error_result().
update_record(<<"provider">>, _ProviderId, _Data) ->
    gui_error:report_error(<<"Not implemented">>).


%%--------------------------------------------------------------------
%% @doc
%% {@link data_backend_behaviour} callback delete_record/2.
%% @end
%%--------------------------------------------------------------------
-spec delete_record(RsrcType :: binary(), Id :: binary()) ->
    ok | gui_error:error_result().
delete_record(<<"provider">>, _Id) ->
    gui_error:report_error(<<"Not implemented">>).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns a client-compliant provider record.
%% @end
%%--------------------------------------------------------------------
-spec provider_record(ProviderId :: od_provider:id(), UserId :: od_user:id()) ->
    proplists:proplist().
provider_record(ProviderId, UserId) ->
    Client = ?USER(gui_session:get_user_id()),
    {ok, #od_user{eff_spaces = EffUserSpaces}} = user_logic:get(Client, UserId),
    {ok, #od_provider{
        name = Name,
        redirection_point = RedPoint,
        latitude = Latitude,
        longitude = Longitude,
        spaces = SpacesWithSupports
    }} = provider_logic:get(?ROOT, ProviderId),
    Spaces = maps:keys(SpacesWithSupports),

    #{host := Host} = url_utils:parse(RedPoint),
    Status = case false of
        true ->
            <<"online">>;
        false ->
            % Sometimes it may happen that there is no websocket connection
            % but the worker is fully operational. For example, when the
            % connection has timed out and provider hasn't reconnected yet.
            % In such case, make sure it is really inoperable and send the
            % result asynchronously.
            gui_async:spawn(true, fun() ->
                check_provider_async(ProviderId)
            end),
            <<"pending">>
    end,

    SpacesToDisplay = lists:filter(
        fun(Space) ->
            lists:member(Space, maps:keys(EffUserSpaces))
        end, Spaces),
    [
        {<<"id">>, ProviderId},
        {<<"name">>, Name},
        {<<"status">>, Status},
        {<<"host">>, Host},
        {<<"spaces">>, SpacesToDisplay},
        {<<"latitude">>, Latitude},
        {<<"longitude">>, Longitude},
        {<<"user">>, UserId}
    ].


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Asynchronously tries to connect to a provider via HTTP and pushes the
%% information whether it was successful to client. This is used when
%% subscriptions channel report no connection, but the provider might still be
%% online.
%% @end
%%--------------------------------------------------------------------
-spec check_provider_async(ProviderId :: od_provider:id()) -> ok.
check_provider_async(ProviderId) ->
    Status = try
        {ok, ProviderUrl} = provider_logic:get_url(ProviderId),
        ConnCheckEndpoint = <<ProviderUrl/binary, ?PROVIDER_ID_ENDPOINT>>,
        case http_client:get(ConnCheckEndpoint, #{}, <<>>, [insecure]) of
            {ok, _, _, ProviderId} -> <<"online">>;
            _ -> <<"offline">>
        end
    catch _:_ ->
        <<"offline">>
    end,
    gui_async:push_updated(<<"provider">>, [
        {<<"id">>, ProviderId}, {<<"status">>, Status}
    ]).