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

-export([init/1, handle/1, cleanup/0]).

-spec init(Args :: term()) ->
    {ok, State :: worker_host:plugin_state()} | {error, Reason :: term()}.
init(_Args) ->
    LastSeq = get_last_seq(),
    start_changes_stream(LastSeq),
    #{provider_callbacks => #{}}.

handle({provider_subscribe, ProviderID, Callback, From}) ->
    add_subscription(ProviderID, Callback),
    fetch_history(ProviderID, Callback, From);
handle(Req) ->
    error("Unsupported request ~p", [Req]).

-spec cleanup() -> ok | {error, Reason :: term()}.
cleanup() ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_subscription(ID, Callback) ->
    TTL = get_ttl(),
    ExpiresAt = erlang:system_time() + TTL,
    worker_host:state_update(?MODULE, provider_callbacks, fun(Callbacks) ->
        maps:put(ID, {Callback, ExpiresAt}, Callbacks)
    end).

get_ttl() ->
    HeartbeatInterval = application:get_env(?APP_Name, hartbeat_interval, timer:minutes(2)),
    HeartbeatInterval * 2.

fetch_history(ProviderID, Callback, From) ->
    couchdb_datastore_driver:changes_start_link(fun(Seq, Doc, Type) ->
        handle_change(Seq, Doc, Type, #{ProviderID => Callback})
    end, From, get_last_seq()).

-spec common_change_callback(Seq :: non_neg_integer(), datastore:document() | stream_ended, model_behaviour:model_type() | undefined) -> ok.
common_change_callback(Seq, stream_ended, _Type) ->
    ?error("Changes stream broken - restarting at ~p", [Seq]),
    start_changes_stream(Seq),
    ok;
common_change_callback(Seq, Doc, Type) ->
    cleanup_expired(),
    Callbacks = worker_host:state_get(?MODULE, provider_callbacks),
    handle_change(Seq, Doc, Type, Callbacks).

handle_change(Seq, Doc, Type, Callbacks) ->
    Providers = get_providers(Seq, Doc, Type),
    ?debug("Dispatching {seq=~p, doc=~p, type=~p} to ~p", [Seq, Doc, Type, Providers]),
    lists:foreach(fun(Provider) ->
        Callback = maps:get(Provider, Callbacks, fun(_Seq, _Doc, _Type) -> ok end),
        Callback(Seq, Doc, Type)
    end, Providers).

cleanup_expired() ->
    worker_host:state_update(?MODULE, provider_callbacks, fun(Callbacks) ->
        Now = erlang:system_time(),
        maps:filter(fun(_ID, {_Callback, ExpiresAt}) ->
            Now < ExpiresAt
        end, Callbacks)
    end).

-spec start_changes_stream(Seq :: non_neg_integer()) -> no_return().
start_changes_stream(StartSeq) ->
    couchdb_datastore_driver:changes_start_link(fun common_change_callback/3, StartSeq, infinity).

-spec get_last_seq() -> non_neg_integer().
get_last_seq() ->
    try
        LastSeq = couchdb_datastore_driver:db_run(couchbeam_changes, follow_once, [], 3),
        binary_to_integer(LastSeq)
    catch
        E:R -> ?error_stacktrace("Last sequence number unknown (assuming 0) due to ~p:~p", [E, R]), 0
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
