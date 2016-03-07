%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Manages subscriptions of providers and users.
%%% @end
%%%-------------------------------------------------------------------
-module(subscriptions).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([add_connection/2, remove_connection/2, subscription/1, update_users/2,
    update_missing_seq/3, subscribed/2, remove_expired_connections/1]).

add_connection(ProviderID, HandlerPid) ->
    provider_subscription:create_or_update(#document{
        key = ProviderID,
        value = #provider_subscription{
            provider = ProviderID,
            connections = [HandlerPid]
        }
    }, fun(Subscription) ->
        Extended = [HandlerPid | Subscription#provider_subscription.connections],
        Subscription#provider_subscription{connections = Extended}
    end).

remove_expired_connections(ProviderID) ->
    provider_subscription:update(ProviderID, fun(Subscription) ->
        Filtered = lists:filter(fun(Pid) ->
            process_info(Pid) =/= undefined
        end, Subscription#provider_subscription.connections),
        Subscription#provider_subscription{connections = Filtered}
    end).

remove_connection(ProviderID, HandlerPid) ->
    provider_subscription:create_or_update(#document{
        key = ProviderID,
        value = #provider_subscription{
            provider = ProviderID,
            connections = []
        }
    }, fun(Subscription) ->
        Connections = Subscription#provider_subscription.connections,
        Filtered = [E || E <- Connections, E =/= HandlerPid],
        Subscription#provider_subscription{connections = Filtered}
    end).

subscription(ProviderID) ->
    provider_subscription:get(ProviderID).

update_users(ProviderID, Users) ->
    {ok, #document{value = #provider_subscription{users = Current}}} =
        provider_subscription:get(ProviderID),

    {ok, _} = provider_subscription:update(ProviderID, fun(Subscription) ->
        Subscription#provider_subscription{users = ordsets:from_list(Users)}
    end),

    ordsets:subtract(Users, Current).

update_missing_seq(ProviderID, ResumeAt, Missing) ->
    {ok, _} = provider_subscription:create_or_update(#document{
        key = ProviderID,
        value = #provider_subscription{
            missing = Missing,
            resume_at = ResumeAt
        }
    }, fun(Subscription) -> Subscription#provider_subscription{
        missing = Missing,
        resume_at = ResumeAt
    } end).

subscribed(ProviderID, Seq) ->
    {ok, #document{value = #provider_subscription{
        missing = Missing,
        resume_at = ResumeAt
    }}} = provider_subscription:get(ProviderID),

    case Seq > ResumeAt of
        true -> true;
        false -> lists:member(Seq, Missing)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================