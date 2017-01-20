%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Database model representing file handles.
%%% @end
%%%-------------------------------------------------------------------
-module(od_handle).
-author("Tomasz Lichon").
-behaviour(model_behaviour).

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("cluster_worker/include/modules/datastore/datastore_model.hrl").

-type doc() :: datastore:document().
-type info() :: #od_handle{}.
-type id() :: binary().
-type resource_type() :: binary().
-type resource_id() :: binary().
-type public_handle() :: binary().
-type metadata() :: binary().
-type timestamp() :: calendar:datetime().

-export_type([doc/0, info/0, id/0]).
-export_type([resource_type/0, resource_id/0, public_handle/0, metadata/0,
    timestamp/0]).

%% API
-export([actual_timestamp/0]).

%% model_behaviour callbacks
-export([save/1, get/1, list/0, exists/1, delete/1, update/2, create/1,
    model_init/0, 'after'/5, before/4]).
-export([record_struct/1]).
-export([to_string/1]).

%%--------------------------------------------------------------------
%% @doc
%% Returns structure of the record in specified version.
%% @end
%%--------------------------------------------------------------------
-spec record_struct(datastore_json:record_version()) -> datastore_json:record_struct().
record_struct(1) ->
    {record, [
        {public_handle, string},
        {resource_type, string},
        {metadata, string},
        {timestamp, {{integer, integer, integer}, {integer, integer, integer}}},
        {resource_id, string},
        {handle_service, string},
        {users, #{string => [atom]}},
        {groups, #{string => [atom]}},
        {eff_users, #{string => {[atom], [{atom, string}]}}},
        {eff_groups, #{string => {[atom], [{atom, string}]}}},
        {bottom_up_dirty, boolean}
    ]}.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @equiv erlang:universaltime().
%%--------------------------------------------------------------------
-spec actual_timestamp() -> timestamp().
actual_timestamp() ->
    erlang:universaltime().

%%%===================================================================
%%% model_behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback save/1.
%% @end
%%--------------------------------------------------------------------
-spec save(datastore:document()) -> {ok, datastore:ext_key()} | datastore:generic_error().
save(Document = #document{value = Handle}) ->
    datastore:save(?STORE_LEVEL, Document#document{value = Handle#od_handle{timestamp = od_handle:actual_timestamp()}}).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback update/2.
%% @end
%%--------------------------------------------------------------------
-spec update(datastore:ext_key(), Diff :: datastore:document_diff()) ->
    {ok, datastore:ext_key()} | datastore:update_error().
update(Key, Diff) ->
    datastore:update(?STORE_LEVEL, ?MODULE, Key, Diff).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback create/1.
%% @end
%%--------------------------------------------------------------------
-spec create(datastore:document()) -> {ok, datastore:ext_key()} | datastore:create_error().
create(Document = #document{value = Handle}) ->
    datastore:create(?STORE_LEVEL, Document#document{value = Handle#od_handle{timestamp = od_handle:actual_timestamp()}}).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback get/1.
%% @end
%%--------------------------------------------------------------------
-spec get(datastore:ext_key()) -> {ok, datastore:document()} | datastore:get_error().
get(Key) ->
    datastore:get(?STORE_LEVEL, ?MODULE, Key).

%%--------------------------------------------------------------------
%% @doc
%% Returns list of all records.
%% @end
%%--------------------------------------------------------------------
-spec list() -> {ok, [datastore:document()]} | datastore:generic_error() | no_return().
list() ->
    datastore:list(?STORE_LEVEL, ?MODEL_NAME, ?GET_ALL, []).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback delete/1.
%% @end
%%--------------------------------------------------------------------
-spec delete(datastore:ext_key()) -> ok | datastore:generic_error().
delete(Key) ->
    datastore:delete(?STORE_LEVEL, ?MODULE, Key).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback exists/1.
%% @end
%%--------------------------------------------------------------------
-spec exists(datastore:ext_key()) -> datastore:exists_return().
exists(Key) ->
    ?RESPONSE(datastore:exists(?STORE_LEVEL, ?MODULE, Key)).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback model_init/0.
%% @end
%%--------------------------------------------------------------------
-spec model_init() -> model_behaviour:model_config().
model_init() ->
    StoreLevel = ?DISK_ONLY_LEVEL,
    ?MODEL_CONFIG(handle_bucket, [], StoreLevel).

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


%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the handle with given id.
%% @end
%%--------------------------------------------------------------------
-spec to_string(HandleId :: id()) -> binary().
to_string(HandleId) ->
    <<"handle:", HandleId/binary>>.

