%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module manages identity data in DHT.
%%% @end
%%%-------------------------------------------------------------------
-module(located_rest_identity_repository).
-author("Michal Zmuda").

-behaviour(identity_repository_behaviour).

-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("datastore/oz_datastore_models.hrl").

-define(LS_NAMESPACE, identity_location).

-export([publish/2, get/1]).

%%--------------------------------------------------------------------
%% @doc
%% Publishes public key under given ID.
%% @end
%%--------------------------------------------------------------------
-spec publish(identity:id(), identity:encoded_public_key()) ->
    ok | {error, Reason :: term()}.
publish(ID, EncodedPublicKey) ->
    case locations:claim(?LS_NAMESPACE, ID) of
        ok ->
            DbResult = owned_identity:save(#document{key = ID, value =
            #owned_identity{id = ID, encoded_public_key = EncodedPublicKey}}),
            case DbResult of
                {ok, _} -> ok;
                {error, Reason} -> {error, {datastore_failed, Reason}}
            end;
        {error, Reason} -> {error, {location_service_failed, Reason}}
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
        {error, not_found} ->
            get_public_key_by_location(ID);
        {error, Reason} ->
            {error, {datastore_failed, Reason}}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @private
%% Determines public key for given ID -  first it uses location service
%% to locate the owner of given ID (OZ, who published this key) and then
%% it uses rest API of OZ to obtain actual public key assigned to given ID.
%% @end
%%--------------------------------------------------------------------
-spec get_public_key_by_location(ID :: identity:id()) ->
    {ok, identity:encoded_public_key()} | {error, Reason :: term()}.
get_public_key_by_location(ID) ->
    case locations:resolve(?LS_NAMESPACE, ID) of
        {error, Reason} ->
            {error, {location_service_failed, Reason}};
        {ok, Location} ->
            EncodedPublicKey = get_public_key_via_rest(Location, ID),
            {ok, EncodedPublicKey}
    end.

%%--------------------------------------------------------------------
%% @doc @private
%% Uses rest API of OZ to obtain actual public key assigned to given ID.
%% @end
%%--------------------------------------------------------------------
-spec get_public_key_via_rest(OzDomain :: binary(), ID :: identity:id()) ->
    identity:encoded_public_key().
get_public_key_via_rest(OzDomain, ID) ->
    {ok, RESTAPIPrefixStr} = application:get_env(?APP_NAME, rest_api_prefix),
    {ok, RestPort} = application:get_env(?APP_NAME, rest_port),
    URL = str_utils:format_bin("https://~s:~B~s/publickey/~s", [
        OzDomain, RestPort, RESTAPIPrefixStr, http_utils:url_encode(ID)]),
    Opts = [{ssl_options, [{secure, false}]}],
    {ok, 200, _, Body} = http_client:get(URL, #{}, <<>>, Opts),
    proplists:get_value(<<"publicKey">>, json_utils:decode(Body)).