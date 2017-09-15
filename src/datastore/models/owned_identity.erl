%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Contains cached identity data.
%%% @end
%%%-------------------------------------------------------------------
-module(owned_identity).
-author("Michal Zmuda").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([save/1, get/1, list/0]).

%% datastore_model callbacks
-export([get_record_struct/1]).

-type id() :: binary().
-type record() :: #owned_identity{}.
-type doc() :: datastore_doc:doc(record()).
-export_type([id/0, record/0]).

-define(CTX, #{
    model => ?MODULE,
    fold_enabled => true
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Saves owned identity.
%% @end
%%--------------------------------------------------------------------
-spec save(doc()) -> {ok, doc()} | {error, term()}.
save(Doc) ->
    datastore_model:save(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Returns owned identity by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(Id) ->
    datastore_model:get(?CTX, Id).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all owned identities.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [doc()]} | {error, term()}.
list() ->
    datastore_model:fold(?CTX, fun(Doc, Acc) -> {ok, [Doc | Acc]} end, []).

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
        {id, string},
        {encoded_public_key, binary}
    ]}.