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
-module(owned_identity_repository).
-author("Michal Zmuda").

-behaviour(identity_repository_behaviour).

-include_lib("ctool/include/logging.hrl").
-include_lib("datastore/oz_datastore_models_def.hrl").

-define(ACTUAL_REPOSITORY, dht_identity_repository).

-export([publish/2, get/1]).

%%--------------------------------------------------------------------
%% @doc
%% Publishes public key under given ID.
%% @end
%%--------------------------------------------------------------------
-spec publish(identity:id(), identity:public_key()) ->
    ok | {error, Reason :: term()}.
publish(ID, EncodedPublicKey) ->
    case ?ACTUAL_REPOSITORY:publish(ID, EncodedPublicKey) of
        ok ->
            DbResult = owned_identity:save(#document{key = ID, value =
            #owned_identity{id = ID, encoded_public_key = EncodedPublicKey}}),
            case DbResult of
                {ok, _} -> ok;
                {error, Reason} -> {error, {db_failed, Reason}}
            end;
        {error, Reason} -> {error, {repo_failed, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Determines public key for given ID.
%% @end
%%--------------------------------------------------------------------
-spec get(identity:id()) ->
    {ok, identity:encoded_public_key()} | {error, Reason :: term()}.
get(ID) ->
    case owned_identity:get(ID) of
        {ok, #document{value = #owned_identity{encoded_public_key = Encoded}}} ->
            {ok, Encoded};
        {error, {not_found, _}} ->
            ?ACTUAL_REPOSITORY:get(ID);
        {error, Reason} ->
            {error, {db_failed, Reason}}
    end.