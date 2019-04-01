%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for volatile_macaroon record - used to store information about the
%%% issuer of a macaroon. Record id serves as macaroon identifier.
%%% Stored in memory only.
%%% @end
%%%-------------------------------------------------------------------
-module(volatile_macaroon).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([create/2, get/1, delete/1]).

-define(CTX, #{
    model => ?MODULE,
    disc_driver => undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a volatile_macaroon record in database.
%% @end
%%--------------------------------------------------------------------
-spec create(macaroon_logic:secret(), macaroon_logic:issuer()) ->
    {ok, macaroon_logic:id()}.
create(Secret, Issuer) ->
    {ok, #document{key = Id}} = datastore_model:save(?CTX, #document{
        value = #volatile_macaroon{
            secret = Secret,
            issuer = Issuer
        }
    }),
    {ok, Id}.

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a volatile_macaroon record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(macaroon_logic:id()) ->
    {ok, macaroon_logic:secret(), macaroon_logic:issuer()} | {error, term()}.
get(Id) ->
    case datastore_model:get(?CTX, Id) of
        {ok, #document{value = #volatile_macaroon{secret = Secret, issuer = Issuer}}} ->
            {ok, Secret, Issuer};
        Error = {error, _} ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Deletes a volatile_macaroon record from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(macaroon_logic:id()) -> ok | {error, term()}.
delete(Id) ->
    datastore_model:delete(?CTX, Id).
