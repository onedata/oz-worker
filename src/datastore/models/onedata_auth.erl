%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for onedata_auth record - used to store macaroon's secrets.
%%% @todo VFS-5554 This module is deprecated, kept for backward compatibility
%%% @end
%%%-------------------------------------------------------------------
-module(onedata_auth).
-author("Michal Zmuda").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([save/1, get/1, delete/1]).
-export([get_by_user_id/1]).

%% datastore_model callbacks
-export([get_record_struct/1]).

-type id() :: binary().
-type record() :: #onedata_auth{}.
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
%% Saves onedata auth.
%% @end
%%--------------------------------------------------------------------
-spec save(doc()) -> {ok, doc()} | {error, term()}.
save(Doc) ->
    datastore_model:save(?CTX, Doc).

%%--------------------------------------------------------------------
%% @doc
%% Returns onedata auth by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(Id) ->
    datastore_model:get(?CTX, Id).

%%--------------------------------------------------------------------
%% @doc
%% Deletes onedata auth.
%% @end
%%--------------------------------------------------------------------
-spec delete(id()) -> ok | {error, term()}.
delete(Id) ->
    datastore_model:delete(?CTX, Id).

%%--------------------------------------------------------------------
%% @doc
%% Gets auth from DB for a given user id.
%% @end
%%--------------------------------------------------------------------
-spec get_by_user_id(od_user:id()) -> {ok, [doc()]}.
get_by_user_id(UserId) ->
    datastore_model:fold(?CTX, fun
        (#document{value = #onedata_auth{user_id = UID}} = Doc, Acc) ->
            case UID of
                UserId -> {ok, [Doc | Acc]};
                _ -> {ok, Acc}
            end
    end, []).

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
        {secret, binary},
        {user_id, string}
    ]}.