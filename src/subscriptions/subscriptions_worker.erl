%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This worker resolves recipients for given updates, and sends updates.
%%% @end
%%%-------------------------------------------------------------------
-module(subscriptions_worker).
-author("Michal Zmuda").

-behaviour(worker_plugin_behaviour).

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

%% worker_plugin_behaviour callbacks
-export([init/1, handle/1, cleanup/0]).

%% API
-export([subscribe_user/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server but does not registers it yet.
%% @end
%%--------------------------------------------------------------------

-spec subscribe_user(UserID :: term(), ProviderID :: term()) -> no_return().
subscribe_user(UserID, ProviderID) ->
    TTL = application:get_env(?APP_Name, client_subscription_ttl_seconds, 300),
    worker_proxy:cast(?MODULE, {subscribe_user, UserID, ProviderID, TTL}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Initialises the worker and ensures cache is available.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: worker_host:plugin_state()} | {error, Reason :: term()}.
init(_Args) ->
    process_flag(trap_exit, true),
    changes_cache:ensure_initialised(),
    {ok, #{}}.

%%--------------------------------------------------------------------
%% @doc
%% Initialises the worker and ensures cache is available.
%% @end
%%--------------------------------------------------------------------
-spec handle(Request :: term()) -> ok | {error, Reason :: term()} | no_return().
handle(healthcheck) ->
    case couchdb_datastore_driver:db_run(couchbeam_changes, follow_once, [], 30) of
        {ok, _, _} -> ok;
        _ -> {error, couchbeam_not_reachable}
    end;

handle({send_update, ProviderSubscriptions, Message}) ->
    ?info("Sending ~p", [[ProviderSubscriptions, Message]]),
    lists:foreach(fun(Subscription) ->
        Provider = Subscription#provider_subscription.provider,
        Endpoint = Subscription#provider_subscription.endpoint,

        ?info("Putting ~p", [[Provider, Endpoint, Message]]), %todo
        outbox:put(Provider, Endpoint, Message)
    end, ProviderSubscriptions);

handle({handle_change, Seq, Doc, Type}) ->
    changes_cache:put(Seq, Doc, Type),
    handle_change(Seq, Doc, Type, fun(_) -> true end);

handle({subscribe_provider, ProviderID, Endpoint, LastSeenSeq}) ->
    subscriptions:put(ProviderID, Endpoint, LastSeenSeq),
    fetch_history(ProviderID, LastSeenSeq);

handle({subscribe_user, UserID, ProviderID, TTL}) ->
    subscriptions:put_user(UserID, ProviderID, TTL);

handle(_Request) ->
    ?log_bad_request(_Request).

%%--------------------------------------------------------------------
%% @doc
%% Cleans up the worker.
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok | {error, Reason :: term()}.
cleanup() ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

fetch_history(ProviderID, LastSeenSeq) ->
    Filter = fun(P) -> P =:= ProviderID end,
    case {changes_cache:newest_seq(), changes_cache:oldest_seq()} of
        {{ok, _Newest}, {ok, _Oldest}} when LastSeenSeq >= _Oldest ->
            fetch_from_cache(LastSeenSeq, _Newest, Filter);
        {{ok, _Newest}, {ok, _Oldest}} ->
            fetch_from_cache(LastSeenSeq, _Newest, Filter),
            fetch_from_db(LastSeenSeq, _Oldest - 1, Filter);
        _ -> fetch_from_db(LastSeenSeq, last_seq(), Filter)
    end.


fetch_from_cache(From, To, Filter) ->
    lists:foreach(fun({Seq, {Doc, Type}}) ->
        handle_change(Seq, Doc, Type, Filter)
    end, changes_cache:slice(From, To)).

fetch_from_db(From, To, Filter) ->
    spawn(fun() ->
        couchdb_datastore_driver:changes_start_link(fun
            (_Seq, stream_ended, _Type) -> ok;
            (Seq, Doc, Type) ->
                handle_change(Seq, Doc, Type, Filter)
        end, From, To)
    end).

handle_change(Seq, Doc, Type, Filter) ->
    Message = translator:get_msg(Seq, Doc, Type),
    Entitled = get_entitled_subscriptions(Doc, Type, Filter),

    NodeToSubscriptions = lists:foldr(fun(Subscription, Acc) ->
        Node = Subscription#provider_subscription.node,
        dict:append(Node, Subscription, Acc)
    end, dict:new(), Entitled),

    lists:foreach(fun({Node, Subscriptions}) ->
        worker_proxy:cast({?MODULE, Node}, {send_update, Subscriptions, Message})
    end, dict:to_list(NodeToSubscriptions)).

get_entitled_subscriptions(Doc, Type, Filter) ->
    Providers = allowed:providers(Doc, Type, Filter),
    Now = erlang:system_time(seconds),

    Subscriptions = lists:map(fun(Provider) ->
        case subscriptions:find(Provider) of
            {ok, #document{value = Val = #provider_subscription{expires = E}}}
                when E > Now -> Val;
            _ -> no_active_subscription
        end
    end, Providers),

    lists:filter(fun(X) -> X =/= no_active_subscription end, Subscriptions).


-spec last_seq() -> non_neg_integer().
last_seq() ->
    try
        {ok, LastSeq, _} = couchdb_datastore_driver:db_run(couchbeam_changes, follow_once, [], 30),
        binary_to_integer(LastSeq)
    catch
        E:R ->
            ?error("Last sequence number unknown (assuming 1) due to ~p:~p", [E, R]),
            1
    end.