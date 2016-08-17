%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module manages identity data in DHT.
%%% @end
%%%-------------------------------------------------------------------
-module(location_service_identity_repository).
-author("Michal Zmuda").

-behaviour(identity_repository_behaviour).

-include_lib("ctool/include/logging.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(DHT_DATA_KEY, <<"public_key">>).
-define(TYPE, identity).


-export([publish/2, get/1]).

%%--------------------------------------------------------------------
%% @doc
%% Publishes public key under given ID.
%% @end
%%--------------------------------------------------------------------
-spec publish(identity:id(), identity:encoded_public_key()) ->
    ok | {error, Reason :: term()}.
publish(ID, EncodedPublicKey) ->
    case locations:claim_model(?TYPE, ID, [{?DHT_DATA_KEY, EncodedPublicKey}]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Determines public key for given ID.
%% @end
%%--------------------------------------------------------------------
-spec get(identity:id()) ->
    {ok, identity:encoded_public_key()} | {error, Reason :: term()}.
get(ID) ->
    case locations:resolve_model(?TYPE, ID) of
        {error, Reason} ->
            ?error("DHT query failed due to: ~p", [Reason]),
            {error, {dht_query_failed, Reason}};
        {ok, Props} ->
            EncodedPublicKey = proplists:get_value(?DHT_DATA_KEY, Props),
            {ok, EncodedPublicKey}
    end.