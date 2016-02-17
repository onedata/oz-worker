%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(subscriptions).
-author("Michal Zmuda").

-behaviour(worker_plugin_behaviour).

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([init/1, handle/1, cleanup/0, common_change_callback/3]).

-spec init(Args :: term()) ->
    {ok, State :: worker_host:plugin_state()} | {error, Reason :: term()}.
init(_Args) ->
    timer:sleep(timer:seconds(2)),% todo wait for couchdb
    process_flag(trap_exit, true),
    LastSeq = get_last_seq(),
    start_changes_stream(LastSeq),
    {ok, #{provider_callbacks => #{}, last_seq => LastSeq}}.

handle(healthcheck) ->
    ok;
handle({provider_subscribe, ProviderID, Callback, From}) ->
    ?info("Request ~p", [{provider_subscribe, ProviderID, Callback, From}]),
    add_subscription(ProviderID, Callback),
    fetch_history(ProviderID, Callback, From);
handle(_Request) ->
    ?log_bad_request(_Request).

-spec cleanup() -> ok | {error, Reason :: term()}.
cleanup() ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_subscription(ID, Callback) ->
    TTL = application:get_env(?APP_Name, subscription_ttl_seconds, 120),
    ExpiresAt = now_seconds() + TTL,
    ?info("Adding subscription ~p", [{ID, Callback, ExpiresAt, TTL}]),
    worker_host:state_update(?MODULE, provider_callbacks, fun(Callbacks) ->
        maps:put(ID, {Callback, ExpiresAt}, Callbacks)
    end),
    ok.

fetch_history(ProviderID, Callback, From) ->
    To = worker_host:state_get(?MODULE, last_seq),
    ?info("History fetch ~p", [[ProviderID, Callback, From, To]]),
    couchdb_datastore_driver:changes_start_link(fun
        (_Seq, stream_ended, _Type) -> ok;
        (Seq, Doc, Type) -> handle_change(Seq, Doc, Type, #{ProviderID => {Callback, infinity}})
    end, From, To).

-spec common_change_callback(Seq :: non_neg_integer(), datastore:document() | stream_ended, model_behaviour:model_type() | undefined) -> ok.
common_change_callback(_Seq, stream_ended, _Type) ->
    Seq = worker_host:state_get(?MODULE, last_seq),
    ?error("Changes stream broken - restarting at ~p", [Seq]),
    start_changes_stream(Seq),
    ok;
common_change_callback(Seq, Doc, Type) ->
    cleanup_expired(),
    Callbacks = worker_host:state_get(?MODULE, provider_callbacks),
    update_last_common_seq(Seq),
    handle_change(Seq, Doc, Type, Callbacks).

update_last_common_seq(Seq) ->
    worker_host:state_update(?MODULE, last_seq, fun(Val) ->
        case Val < Seq of
            true -> Seq;
            false -> Val
        end
    end).

handle_change(Seq, Doc, Type, Callbacks) ->
    NoOp = fun(_Seq, _Doc, _Type) -> ok end,
    Providers = get_providers(Seq, Doc, Type),
    ?info("Dispatching {seq=~p, doc=~p, type=~p} to ~p having ~p", [Seq, Doc, Type, Providers, Callbacks]),
    lists:foreach(fun(Provider) ->
        {Callback, _} = maps:get(Provider, Callbacks, NoOp),
        spawn(fun() -> Callback(Seq, Doc, Type) end)
    end, Providers).

cleanup_expired() ->
    worker_host:state_update(?MODULE, provider_callbacks, fun(Callbacks) ->
        Now = now_seconds(),
        maps:filter(fun(_ID, {_Callback, ExpiresAt}) ->
            Now < ExpiresAt
        end, Callbacks)
    end),
    ok.

now_seconds() ->
    erlang:system_time(seconds).

-spec start_changes_stream(Seq :: non_neg_integer()) -> no_return().
start_changes_stream(StartSeq) ->
    couchdb_datastore_driver:changes_start_link(fun common_change_callback/3, StartSeq, infinity).

-spec get_last_seq() -> non_neg_integer().
get_last_seq() ->
    try
        {ok, LastSeq, _} = couchdb_datastore_driver:db_run(couchbeam_changes, follow_once, [], 30),
        binary_to_integer(LastSeq)
    catch
        E:R -> ?error("Last sequence number unknown (assuming 1) due to ~p:~p", [E, R]), 1
    end.

get_providers(_Seq, Doc, space) ->
    #document{value = Value} = Doc,
    #space{providers = SpaceProviders} = Value,
    SpaceProviders;

get_providers(_Seq, Doc, user_group) ->
    #document{key = ID} = Doc,
    {ok, [{providers, GroupProviders}]} = group_logic:get_providers(ID),
    GroupProviders;

get_providers(_Seq, Doc, onedata_user) ->
    #document{key = ID} = Doc,
    {ok, [{providers, UserProviders}]} = user_logic:get_providers(ID),
    UserProviders.
