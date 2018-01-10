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
-export([create/3, get/1, delete/1]).

%% datastore_model callbacks
-export([get_record_struct/1]).

-type id() :: binary().
-type record() :: #macaroon_auth{}.
-type doc() :: datastore_doc:doc(record()).

-type secret() :: binary().
-type type() :: authorization.
-type issuer() :: entity_logic:client().

-export_type([id/0, record/0, doc/0]).
-export_type([secret/0, type/0, issuer/0]).

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
-spec create(secret(), type(), issuer()) -> {ok, id()}.
create(Secret, Type, Issuer) ->
    {ok, #document{key = Id}} = datastore_model:save(?CTX, #document{
        value = #macaroon_auth{
            secret = Secret,
            type = Type,
            issuer = Issuer
        }
    }),
    {ok, Id}.

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a macaroon_auth record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, record()} | {error, term()}.
get(Id) ->
    case datastore_model:get(?CTX, Id) of
        {ok, #document{value = MacaroonAuth}} ->
            {ok, MacaroonAuth};
        Error = {error, _} ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Deletes a macaroon_auth record from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(id()) -> ok | {error, term()}.
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