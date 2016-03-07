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


%%%===================================================================
%%% API
%%%===================================================================

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

handle({handle_change, Seq, Doc, Type}) ->
    changes_cache:put(Seq, Doc, Type),
    handle_change(Seq, Doc, Type, fun(_) -> true end);

handle({add_connection, ProviderID, Connection}) ->
    subscriptions:add_connection(ProviderID, Connection);

handle({remove_connection, ProviderID, Connection}) ->
    subscriptions:remove_connection(ProviderID, Connection);

handle({update_missing_seq, ProviderID, ResumeAt, Missing}) ->
    subscriptions:update_missing_seq(ProviderID, ResumeAt, Missing),
    fetch_history(ProviderID, ResumeAt, Missing);

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

fetch_history(ProviderID, ResumeAt, Missing) ->
    [SmallestMissing | _] = lists:append(Missing, [ResumeAt]),
    Filter = fun(P) -> P =:= ProviderID end,
    case {changes_cache:newest_seq(), changes_cache:oldest_seq()} of
        {{ok, _Newest}, {ok, _Oldest}} when SmallestMissing >= _Oldest ->
            fetch_from_cache(SmallestMissing, _Newest, Filter);
        {{ok, _Newest}, {ok, _Oldest}} ->
            fetch_from_cache(SmallestMissing, _Newest, Filter),
            fetch_from_db(SmallestMissing, _Oldest - 1, Filter);
        _ -> fetch_from_db(SmallestMissing, last_seq(), Filter)
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

    lists:foreach(fun(Subscription) ->
        [Conn | _] = Subscription#provider_subscription.connections,
        Messages = json_utils:encode({array, [Message]}),
        ?info("Pushing ~p ~p", [Conn, Messages]),
        Conn ! {push, Messages}
    end, Entitled).

get_entitled_subscriptions(Doc, Type, Filter) ->
    Providers = allowed:providers(Doc, Type, Filter),

    lists:filtermap(fun(Provider) ->
        case subscriptions:subscription(Provider) of
            {ok, #document{value = Val}} -> {true, Val};
            _ -> false
        end
    end, Providers).


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