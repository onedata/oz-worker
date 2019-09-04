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
%%% @todo VFS-5554 This module is deprecated, kept for backward compatibility
%%% @end
%%%-------------------------------------------------------------------
-module(macaroon_auth).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([create/2, get/1, delete/1]).

%% datastore_model callbacks
-export([get_record_version/0, get_record_struct/1, upgrade_record/2]).

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
-spec create(tokens:secret(), aai:auth()) -> {ok, tokens:nonce()}.
create(Secret, Auth) ->
    {ok, #document{key = Id}} = datastore_model:save(?CTX, #document{
        value = #macaroon_auth{
            secret = Secret,
            type = authorization,
            issuer = Auth#auth.subject
        }
    }),
    {ok, Id}.

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the secret and issuer of given macaroon from database.
%% @end
%%--------------------------------------------------------------------
-spec get(tokens:nonce()) -> {ok, tokens:secret(), aai:auth()} | {error, term()}.
get(Id) ->
    case datastore_model:get(?CTX, Id) of
        {ok, #document{value = #macaroon_auth{secret = Secret, type = authorization, issuer = Issuer}}} ->
            {ok, Secret, #auth{subject = Issuer}};
        Error = {error, _} ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Deletes a macaroon_auth record from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(tokens:nonce()) -> ok | {error, term()}.
delete(Id) ->
    datastore_model:delete(?CTX, Id).

%%%===================================================================
%%% datastore_model callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns model's record version.
%% @end
%%--------------------------------------------------------------------
-spec get_record_version() -> datastore_model:record_version().
get_record_version() ->
    2.

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
    ]};
get_record_struct(2) ->
    {record, [
        {secret, string},
        {type, atom},
        {issuer, {record, [ % nested record changed from #client{} to #subject{}
            {type, atom},
            {id, string}
        ]}}
    ]}.


%%--------------------------------------------------------------------
%% @doc
%% Upgrades model's record from provided version to the next one.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_record(datastore_model:record_version(), datastore_model:record()) ->
    {datastore_model:record_version(), datastore_model:record()}.
upgrade_record(1, MacaroonAuth) ->
    {
        macaroon_auth,
        Secret,
        Type,
        Creator
    } = MacaroonAuth,

    {2, #macaroon_auth{
        secret = Secret,
        type = Type,
        issuer = upgrade_common:client_to_subject(Creator)
    }}.