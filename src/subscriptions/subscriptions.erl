%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Manages subscriptions of providers.
%%% @end
%%%-------------------------------------------------------------------
-module(subscriptions).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([add_connection/2, remove_connection/2, get_doc/1, update_users/2,
    update_missing_seq/3, seen/2, remove_expired_connections/1,
    all/0]).


%%--------------------------------------------------------------------
%% @doc
%% Adds connection to the working provider.
%% @end
%%--------------------------------------------------------------------
-spec add_connection(ProviderID :: binary(), Connection :: pid())
        -> no_return().
add_connection(ProviderID, Connection) ->
    provider_subscription:create_or_update(#document{
        key = ProviderID,
        value = #provider_subscription{
            provider = ProviderID,
            connections = [Connection]
        }
    }, fun(Subscription) ->
        Extended = [Connection | Subscription#provider_subscription.connections],
        {ok, Subscription#provider_subscription{connections = Extended}}
    end).


%%--------------------------------------------------------------------
%% @doc
%% Removes expired connections from provider connections.
%% @end
%%--------------------------------------------------------------------
-spec remove_expired_connections(ProviderID :: binary()) -> no_return().
remove_expired_connections(ProviderID) ->
    provider_subscription:update(ProviderID, fun(Subscription) ->
        Filtered = lists:filter(fun(Pid) ->
            process_info(Pid) =/= undefined
        end, Subscription#provider_subscription.connections),
        Subscription#provider_subscription{connections = Filtered}
    end).

%%--------------------------------------------------------------------
%% @doc
%% Removes connection from provider connections.
%% @end
%%--------------------------------------------------------------------
-spec remove_connection(ProviderID :: binary(), Connection :: pid())
        -> no_return().
remove_connection(ProviderID, Connection) ->
    provider_subscription:create_or_update(#document{
        key = ProviderID,
        value = #provider_subscription{
            provider = ProviderID,
            connections = []
        }
    }, fun(Subscription) ->
        Connections = Subscription#provider_subscription.connections,
        Filtered = [E || E <- Connections, E =/= Connection],
        {ok, Subscription#provider_subscription{connections = Filtered}}
    end).

%%--------------------------------------------------------------------
%% @doc
%% Fetches subscription of the provider with given ID
%% @end
%%--------------------------------------------------------------------
-spec get_doc(ProviderID :: binary())
        -> {ok, datastore:document()} | {error, Reason :: term()}.
get_doc(ProviderID) ->
    provider_subscription:get(ProviderID).

%%--------------------------------------------------------------------
%% @doc
%% Fetches all existing subscription docs.
%% @end
%%--------------------------------------------------------------------
-spec all() -> [datastore:document()].
all() ->
    {ok, Subscriptions} = provider_subscription:all(),
    Subscriptions.

%%--------------------------------------------------------------------
%% @doc
%% Updates users declared in subscription.
%% Returns user IDs added by this operation.
%% @end
%%--------------------------------------------------------------------
-spec update_users(ProviderID :: binary(), UserIDs :: [binary()])
        -> NewUserIDs :: [binary()].
update_users(ProviderID, Users) ->
    {ok, #document{value = #provider_subscription{users = Current}}} =
        provider_subscription:get(ProviderID),

    {ok, _} = provider_subscription:update(ProviderID, fun(Subscription) ->
        {ok, Subscription#provider_subscription{users = ordsets:from_list(Users)}}
    end),

    ordsets:subtract(Users, Current).


%%--------------------------------------------------------------------
%% @doc
%% Updates subscription tracking status. Accepts newest sequence number
%% (according to the provider) and sequence numbers he didn't receive.
%% @end
%%--------------------------------------------------------------------
-spec update_missing_seq(ProviderID :: binary(), ResumeAt :: seq(),
    Missing :: [seq()]) -> no_return().

update_missing_seq(ProviderID, ResumeAt, Missing) ->
    {ok, _} = provider_subscription:create_or_update(#document{
        key = ProviderID,
        value = #provider_subscription{
            missing = Missing,
            resume_at = ResumeAt
        }
    }, fun(Subscription) -> {ok, Subscription#provider_subscription{
        missing = Missing,
        resume_at = ResumeAt
    }} end).


%%--------------------------------------------------------------------
%% @doc
%% Returns if provider already received update with given sequence number.
%% Special updates (with -1 sequence number) are always qualified as not seen.
%% @end
%%--------------------------------------------------------------------
-spec seen(Subscription :: #provider_subscription{}, Seq :: seq())
        -> boolean().
seen(_, -1) -> false;
seen(Subscription, Seq) ->
    #provider_subscription{missing = Missing, resume_at = ResumeAt} = Subscription,
    case Seq > ResumeAt of
        true -> false;
        false -> not lists:member(Seq, Missing)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================