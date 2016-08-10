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
-module(locations).
-author("Michal Zmuda").

-include_lib("ctool/include/logging.hrl").

-define(GENERATED_ID_RETRIES, 3).
-define(GIVEN_ID_RETRIES, 2).

%% API
-export([claim_id/1, claim_id/0, resolve_id/1, claim_model/2, resolve_model/2, claim_model/3]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Resolves (via Location Service) what address is assigned to given key.
%% @end
%%--------------------------------------------------------------------
-spec resolve_id(ID :: binary()) -> {ok, Value :: [{binary(), term()}]} | {error, term()}.
resolve_id(ID) ->
    get_value(ID).

%%--------------------------------------------------------------------
%% @doc
%% Reserves (via Location Service) given key (assigns it to own address).
%% @end
%%--------------------------------------------------------------------
-spec resolve_model(Model :: atom(), ID :: binary()) -> {ok, [{binary(), term()}]} | {error, term()}.
resolve_model(Model, ID) ->
    get_value(model_id(Model, ID)).

%%--------------------------------------------------------------------
%% @doc
%% Reserves (via Location Service) given key (assigns it to own address).
%% @end
%%--------------------------------------------------------------------
-spec claim_model(Model :: atom(), ID :: binary()) -> {ok, ID :: binary()} | {error, term()}.
claim_model(Model, ID) ->
    claim_model(Model, ID, []).
claim_model(Model, ID, Data) ->
    set_value_under_id(model_id(Model, ID), [{location, get_address()} | Data], ?GIVEN_ID_RETRIES).

%%--------------------------------------------------------------------
%% @doc
%% Reserves (via Location Service) given key (assigns it to own address).
%% @end
%%--------------------------------------------------------------------
-spec claim_id(ID :: binary()) -> {ok, ID :: binary()} | {error, term()}.
claim_id(ID) ->
    set_value_under_id(ID, get_address(), ?GIVEN_ID_RETRIES).

%%--------------------------------------------------------------------
%% @doc
%% Reserves (via Location Service) generated key (assigns it to own address).
%% @end
%%--------------------------------------------------------------------
-spec claim_id() -> {ok, ID :: binary()} | {error, term()}.
claim_id() ->
    set_value(get_address(), ?GENERATED_ID_RETRIES).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @private
%% Returns ID in location service.
%% @end
%%--------------------------------------------------------------------
-spec model_id(Model :: atom(), ID :: binary()) -> binary().
model_id(Model, ID) ->
    list_to_binary(atom_to_list(Model) ++ "::" ++ binary_to_list(ID)).

%%--------------------------------------------------------------------
%% @doc @private
%% Returns own address of the OZ.
%% @end
%%--------------------------------------------------------------------
-spec get_address() -> binary().
get_address() ->
    {ok, Host} = application:get_env(oz_worker, http_domain),
    list_to_binary(Host).

%%--------------------------------------------------------------------
%% @doc @private
%% Saves the value in the DHT. Retries are attempted.
%% @end
%%--------------------------------------------------------------------
-spec get_value(ID :: binary()) -> {ok, Value :: [{binary(), term()}]} | {error, term()}.
get_value(ID) ->
    case dht_worker:get_value(ID) of
        {error, Reason} ->
            ?warning("DHT get failed with reason ~p", [Reason]),
            {error, dht_get_failed};
        {ok, 'OK', Value, _} -> {ok, Value};
        {ok, Error, _, Message} ->
            ?warning("DHT reported error ~p - ~p", [Error, Message]),
            {error, dht_get_failed}
    end.

%%--------------------------------------------------------------------
%% @doc @private
%% Saves the value in the DHT under given key. Retries are attempted.
%% @end
%%--------------------------------------------------------------------
-spec set_value_under_id(ID :: binary(), Value :: term()) ->
    {ok, ID :: binary()} | {error, term()}.
set_value_under_id(ID, Value) ->
    case dht_worker:set_value(ID, Value) of
        {error, Reason} ->
            ?warning("DHT put failed with reason ~p", [Reason]),
            {error, dht_put_failed};
        {ok, 'OK', _} -> {ok, ID};
        {ok, Error, Message} ->
            ?warning("DHT reported error ~p - ~p", [Error, Message]),
            {error, dht_put_failed}
    end.

-spec set_value_under_id(ID :: binary(), Value :: term(), Retries :: non_neg_integer()) ->
    {ok, ID :: binary()} | {error, term()}.
set_value_under_id(ID, Value, 1) ->
    set_value_under_id(ID, Value);
set_value_under_id(ID, Value, Retries) ->
    case set_value_under_id(ID, Value) of
        {ok, _} -> {ok, ID};
        {error, _} ->
            ?warning("Failed to save under given ID ~p - retrying", [ID]),
            set_value_under_id(ID, Value, Retries - 1)
    end.

-spec set_value(Value :: term(), Retries :: non_neg_integer()) ->
    {ok, ID :: binary()} | {error, term()}.
set_value(Value, 0) ->
    set_value_under_id(datastore_utils:gen_uuid(), Value);
set_value(Value, Retries) ->
    ID = datastore_utils:gen_uuid(),
    case set_value_under_id(ID, Value) of
        {ok, _} -> {ok, ID};
        {error, _} ->
            ?warning("Failed to save under generated ID ~p - retrying", [ID]),
            set_value(Value, Retries - 1)
    end.