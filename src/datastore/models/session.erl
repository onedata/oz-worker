%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for session record - used to store session details.
%%% @end
%%%-------------------------------------------------------------------
-module(session).
-author("Michal Zmuda").
-behaviour(model_behaviour).

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("cluster_worker/include/modules/datastore/datastore_model.hrl").

-type doc() :: datastore:document().
-type info() :: #session{}.
-type id() :: binary().
-export_type([doc/0, info/0, id/0]).


%% model_behaviour callbacks
-export([save/1, get/1, exists/1, delete/1, update/2, create/1,
    model_init/0, 'after'/5, before/4]).


-type memory() :: maps:map().
-export_type([memory/0]).


%%%===================================================================
%%% model_behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback save/1.
%% @end
%%--------------------------------------------------------------------
-spec save(datastore:document()) -> {ok, datastore:key()} | datastore:generic_error().
save(#document{value = Sess} = Document) ->
    Timestamp = os:timestamp(),
    model:execute_with_default_context(?MODULE, save, [
        Document#document{value = Sess#session{
            accessed = Timestamp
        }}
    ]).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback update/2.
%% @end
%%--------------------------------------------------------------------
-spec update(datastore:key(), Diff :: datastore:document_diff()) ->
    {ok, datastore:key()} | datastore:update_error().
update(Key, Diff) when is_map(Diff) ->
    model:execute_with_default_context(?MODULE, update, [Key,
        Diff#{accessed => os:timestamp()}]);
update(Key, Diff) when is_function(Diff) ->
    NewDiff = fun(Sess) ->
        case Diff(Sess) of
            {ok, NewSess} -> {ok, NewSess#session{accessed = os:timestamp()}};
            {error, Reason} -> {error, Reason}
        end
              end,
    model:execute_with_default_context(?MODULE, update, [Key, NewDiff]).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback create/1.
%% @end
%%--------------------------------------------------------------------
-spec create(datastore:document()) -> {ok, datastore:key()} | datastore:create_error().
create(#document{value = Sess} = Document) ->
    Timestamp = os:timestamp(),
    model:execute_with_default_context(?MODULE, create, [
        Document#document{value = Sess#session{
            accessed = Timestamp
        }}
    ]).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback get/1.
%% Sets access time to current time for user session and returns old value.
%% @end
%%--------------------------------------------------------------------
-spec get(datastore:key()) -> {ok, datastore:document()} | datastore:get_error().
get(Key) ->
    case model:execute_with_default_context(?MODULE, get, [Key]) of
        {ok, Doc} ->
            session:update(Key, #{}),
            {ok, Doc};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback delete/1.
%% @end
%%--------------------------------------------------------------------
-spec delete(datastore:key()) -> ok | datastore:generic_error().
delete(Key) ->
    model:execute_with_default_context(?MODULE, delete, [Key]).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback exists/1.
%% @end
%%--------------------------------------------------------------------
-spec exists(datastore:key()) -> datastore:exists_return().
exists(Key) ->
    case ?RESPONSE(model:execute_with_default_context(?MODULE, exists, [Key])) of
        true ->
            update(Key, #{}),
            true;
        false ->
            false
    end.

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback model_init/0.
%% @end
%%--------------------------------------------------------------------
-spec model_init() -> model_behaviour:model_config().
model_init() ->
    ?MODEL_CONFIG(session_bucket, [], ?GLOBAL_ONLY_LEVEL).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback 'after'/5.
%% @end
%%--------------------------------------------------------------------
-spec 'after'(ModelName :: model_behaviour:model_type(), Method :: model_behaviour:model_action(),
    Level :: datastore:store_level(), Context :: term(),
    ReturnValue :: term()) -> ok.
'after'(_ModelName, _Method, _Level, _Context, _ReturnValue) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback before/4.
%% @end
%%--------------------------------------------------------------------
-spec before(ModelName :: model_behaviour:model_type(), Method :: model_behaviour:model_action(),
    Level :: datastore:store_level(), Context :: term()) -> ok | datastore:generic_error().
before(_ModelName, _Method, _Level, _Context) ->
    ok.
