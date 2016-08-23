%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is used to manage global location data.
%%% @end
%%%-------------------------------------------------------------------
-module(locations).
-author("Michal Zmuda").

-include_lib("ctool/include/logging.hrl").

-export([resolve/2, claim/2]).

-type(id() :: binary()).
-type(location() :: binary()).
-type(namespace() :: atom()).
-type(additional_data() :: [{atom() | binary(), term()}]).
-export_type([id/0, location/0, namespace/0, additional_data/0]).

-define(RESOLVE_RETRIES, 3).
-define(CLAIM_RETRIES, 3).
-define(KEY, <<"location">>).

%%%===================================================================

%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Resolves (via Location Service) what address is assigned to given key.
%% @end
%%--------------------------------------------------------------------
-spec resolve(namespace(), id()) -> {ok, location()} | {error, term()}.
resolve(Namespace, ID) ->
    get_location(id(Namespace, ID), ?RESOLVE_RETRIES).

%%--------------------------------------------------------------------
%% @doc
%% Reserves (via Location Service) given key (assigns it to own address).
%% @end
%%--------------------------------------------------------------------
-spec claim(namespace(), id()) -> ok | {error, term()}.
claim(Namespace, ID) ->
    set_location(id(Namespace, ID), ?CLAIM_RETRIES).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @private
%% Returns ID prefixed with namespace.
%% @end
%%--------------------------------------------------------------------
-spec id(namespace(), id()) -> id().
id(Namespace, ID) ->
    NamespaceBin = atom_to_binary(Namespace, latin1),
    <<NamespaceBin/binary, "::", ID/binary>>.

%%--------------------------------------------------------------------
%% @doc @private
%% Returns own address of the OZ.
%% @end
%%--------------------------------------------------------------------
-spec get_domain() -> binary().
get_domain() ->
    {ok, Host} = application:get_env(oz_worker, http_domain),
    list_to_binary(Host).

%%--------------------------------------------------------------------
%% @doc @private
%% Saves the value in the DHT. Retries are attempted.
%% @end
%%--------------------------------------------------------------------
-spec get_location(id(), Retries :: non_neg_integer()) ->
    {ok, location()} | {error, term()}.
get_location(ID, Retries) ->
    case location_service_worker:get(ID) of
        {error, {'CONFLICT', _}} = Err -> Err;
        {error, _} when Retries > 0 -> get_location(ID, Retries - 1);
        {error, Reason} -> {error, Reason};
        {ok, Data} ->
            Location = proplists:get_value(?KEY, Data),
            case is_binary(Location) of
                true -> {ok, Location};
                false -> {error, malformed_location}
            end
    end.

%%--------------------------------------------------------------------
%% @doc @private
%% Saves the value in the DHT under given key. Retries are attempted.
%% @end
%%--------------------------------------------------------------------
-spec set_location(id(), Retries :: non_neg_integer()) ->
    {ok, ID :: binary()} | {error, term()}.
set_location(ID, Retries) ->
    Data = [{?KEY, get_domain()}],
    case location_service_worker:set(ID, Data) of
        {error, {'CONFLICT', _}} = Err -> Err;
        {error, _} when Retries > 0 -> set_location(ID, Retries - 1);
        {error, Reason} -> {error, Reason};
        ok -> ok
    end.
