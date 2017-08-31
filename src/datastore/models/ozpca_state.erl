%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Stores CA dedicated node
%%% todo: implement distributed CA properly (connected with VFS-1499)
%%% @end
%%%-------------------------------------------------------------------
-module(ozpca_state).
-author("Michal Zmuda").

-include("datastore/oz_datastore_models.hrl").

%% API
-export([get/1, update/3]).

%% datastore_model callbacks
-export([get_record_struct/1]).

-type id() :: binary().
-type record() :: #ozpca_state{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-export_type([id/0, record/0]).

-define(CTX, #{model => ?MODULE}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns ozpca state by ID.
%% @end
%%--------------------------------------------------------------------
-spec get(id()) -> {ok, doc()} | {error, term()}.
get(Id) ->
    datastore_model:get(?CTX, Id).

%%--------------------------------------------------------------------
%% @doc
%% Updates ozpca by ID or creates default one.
%% @end
%%--------------------------------------------------------------------
-spec update(id(), diff(), record()) -> {ok, doc()} | {error, term()}.
update(Id, Diff, Default) ->
    datastore_model:update(?CTX, Id, Diff, Default).

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
        {dedicated_node, {atom, term}}
    ]}.