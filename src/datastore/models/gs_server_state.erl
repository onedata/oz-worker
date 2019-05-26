%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Persistent state of Graph Sync server worker. Holds the last change seq
%%% that was successfully processed, so that the server know where to resume in
%%% case of a crash.
%%% @end
%%%-------------------------------------------------------------------
-module(gs_server_state).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").

-define(GLOBAL_STATE_KEY, <<"global_state">>).

%% API
-export([get_seq/0, set_seq/1]).

%% datastore_model callbacks
-export([get_record_struct/1]).

-type id() :: binary().
-type record() :: #gs_server_state{}.
-type doc() :: datastore_doc:doc(record()).

-export_type([id/0, record/0]).

-define(CTX, #{model => ?MODULE}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns last successfully processed sequence number.
%% @end
%%--------------------------------------------------------------------
-spec get_seq() -> couchbase_changes:seq().
get_seq() ->
    case datastore_model:get(?CTX, ?GLOBAL_STATE_KEY) of
        {ok, #document{value = #gs_server_state{seq = Seq}}} ->
            Seq;
        {error, not_found} ->
            1
    end.

%%--------------------------------------------------------------------
%% @doc
%% Sets last successfully processed sequence number to given value.
%% @end
%%--------------------------------------------------------------------
-spec set_seq(couchbase_changes:seq()) -> {ok, doc()} | {error, term()}.
set_seq(Seq) ->
    datastore_model:save(?CTX, #document{
        key = ?GLOBAL_STATE_KEY, value = #gs_server_state{seq = Seq}
    }).


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
        {seq, integer}
    ]}.

