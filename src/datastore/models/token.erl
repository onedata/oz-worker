%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for token record - used to store token data.
%%% @end
%%%-------------------------------------------------------------------
-module(token).
-author("Michal Zmuda").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([save/1, get/1, delete/1, update/2]).

%% datastore_model callbacks
-export([get_record_struct/1, get_record_version/0, upgrade_record/2]).

-type id() :: binary().
-type record() :: #token{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-export_type([id/0, record/0]).

-define(CTX, #{model => ?MODULE}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates token.
%% @end
%%--------------------------------------------------------------------
-spec save(doc()) -> {ok, doc()} | {error, term()}.
save(Doc) ->
    datastore_model:create(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Returns token by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(TokenId) ->
    datastore_model:get(?CTX, TokenId).

%%--------------------------------------------------------------------
%% @doc
%% Deletes token by ID.
%% @end
%%--------------------------------------------------------------------
-spec delete(id()) -> ok | {error, term()}.
delete(TokenId) ->
    datastore_model:delete(?CTX, TokenId).


%%--------------------------------------------------------------------
%% @doc
%% Updates token by ID.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(TokenId, Diff) ->
    datastore_model:update(?CTX, TokenId, Diff).


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
        {secret, binary},
        {resource, atom},
        {resource_id, string},
        {issuer, {record, [
            {type, atom},
            {id, string}
        ]}}
    ]};
get_record_struct(2) ->
    {record, [
        {secret, binary},
        {resource, atom},
        {resource_id, string},
        {issuer, {record, [
            {type, atom},
            {id, string}
        ]}},
        {locked, boolean}
    ]}.

%%--------------------------------------------------------------------
%% @doc
%% Upgrades model's record from provided version to the next one.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_record(datastore_model:record_version(), datastore_model:record()) ->
    {datastore_model:record_version(), datastore_model:record()}.
upgrade_record(1, Token) ->
    {
        token,
        Secret,
        Resource,
        ResourceId,
        Issuer
    } = Token,
    
    {2, #token{
        secret = Secret,
        resource = Resource,
        resource_id = ResourceId,
        issuer = Issuer,
        locked = false
    }}.
