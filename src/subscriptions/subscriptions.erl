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
    update_missing_seq/3, seen/2, all/0, any_connection_active/1]).

-type(seq() :: non_neg_integer() | -1).
-type(model() :: onedata_user | user_group | space).
-export_type([seq/0, model/0]).

%%--------------------------------------------------------------------
%% @doc
%% Adds connection to the working provider.
%% @end
%%--------------------------------------------------------------------
-spec add_connection(ProviderID :: binary(), Connection :: pid()) -> ok.
add_connection(ProviderID, Connection) ->
    {ok, ProviderID} = provider_subscription:create_or_update(#document{
        key = ProviderID,
        value = #provider_subscription{
            provider = ProviderID,
            connections = [Connection]
        }
    }, fun(Subscription) ->
        Extended = [Connection | Subscription#provider_subscription.connections],
        {ok, Subscription#provider_subscription{connections = Extended}}
    end), ok.

%%--------------------------------------------------------------------
%% @doc
%% Removes connection from provider connections.
%% @end
%%--------------------------------------------------------------------
-spec remove_connection(ProviderID :: binary(), Connection :: pid()) -> ok.
remove_connection(ProviderID, Connection) ->
    {ok, _} = provider_subscription:create_or_update(#document{
        key = ProviderID,
        value = #provider_subscription{
            provider = ProviderID,
            connections = []
        }
    }, fun(Subscription) ->
        Connections = Subscription#provider_subscription.connections,
        Filtered = [E || E <- Connections, E =/= Connection],
        {ok, Subscription#provider_subscription{connections = Filtered}}
    end), ok.

%%--------------------------------------------------------------------
%% @doc
%% Fetches subscription of the provider with given ID
%% @end
%%--------------------------------------------------------------------
-spec get_doc(ProviderID :: binary()) ->
    {ok, datastore:document()} | {error, Reason :: term()}.
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
    Missing :: [seq()]) -> ok.

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
    }} end), ok.


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

%%--------------------------------------------------------------------
%% @doc
%% Checks if provider has an alive connection.
%% @end
%%--------------------------------------------------------------------
-spec any_connection_active(ProviderID :: binary()) -> boolean().
any_connection_active(ProviderID) ->
    case subscriptions:get_doc(ProviderID) of
        {ok, #document{value = #provider_subscription{connections = PidList}}} ->
            lists:any(fun(Pid) ->
                node(Pid) =:= node() andalso process_info(Pid) =/= undefined
            end, PidList
            ) orelse lists:any(fun(Pid) ->
                Node = node(Pid),
                Node =/= node() andalso
                    rpc:call(Node, erlang, process_info, [Pid]) =/= undefined
            end, PidList);
        _ -> false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================