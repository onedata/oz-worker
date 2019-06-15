%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for macaroon_auth record - used to store information about authorization
%%% carried by macaroons. Record id is used as macaroon identifier.
%%% @end
%%%-------------------------------------------------------------------
-module(macaroon_auth).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([create/2, get/1, delete/1]).

%% datastore_model callbacks
-export([get_record_struct/1]).

-define(CTX, #{
    model => ?MODULE
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a macaroon_auth record in database.
%% @end
%%--------------------------------------------------------------------
-spec create(macaroon_logic:secret(), macaroon_logic:issuer()) ->
    {ok, macaroon_logic:id()}.
create(Secret, Issuer) ->
    {ok, #document{key = Id}} = datastore_model:save(?CTX, #document{
        value = #macaroon_auth{
            secret = Secret,
            type = authorization,
            issuer = Issuer
        }
    }),
    {ok, Id}.

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the secret and issuer of given macaroon from database.
%% @end
%%--------------------------------------------------------------------
-spec get(macaroon_logic:id()) ->
    {ok, macaroon_logic:secret(), macaroon_logic:issuer()} | {error, term()}.
get(Id) ->
    case datastore_model:get(?CTX, Id) of
        {ok, #document{value = #macaroon_auth{secret = Secret, type = authorization, issuer = Issuer}}} ->
            {ok, Secret, Issuer};
        Error = {error, _} ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Deletes a macaroon_auth record from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(macaroon_logic:id()) -> ok | {error, term()}.
delete(Id) ->
    datastore_model:delete(?CTX, Id).

%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns model's record structure in provided version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_struct(datastore_model:record_version()) ->
    datastore_model:record_struct().
get_record_struct(1) ->
    {record, [
        {secret, string},
        {type, atom},
        {issuer, {record, [
            {type, atom},
            {id, string}
        ]}}
    ]}.