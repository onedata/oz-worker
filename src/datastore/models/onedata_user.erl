%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for onedata_user record - representing a user in the system.
%%% @end
%%%-------------------------------------------------------------------
-module(onedata_user).
-author("Michal Zmuda").
-behaviour(model_behaviour).

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("cluster_worker/include/modules/datastore/datastore_model.hrl").

%% model_behaviour callbacks
-export([save/1, get/1, list/0, exists/1, delete/1, update/2, create/1,
    model_init/0, 'after'/5, before/4]).

%% API
-export([get_all_ids/0, get_by_criterion/1]).


-type id() :: binary().
-export_type([id/0]).

%%%===================================================================
%%% model_behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback save/1.
%% @end
%%--------------------------------------------------------------------
-spec save(datastore:document()) -> {ok, datastore:ext_key()} | datastore:generic_error().
save(Document) ->
    datastore:save(?STORE_LEVEL, Document).

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
create(Document) ->
    datastore:create(?STORE_LEVEL, Document).

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
%% todo: change level once list is supported by the datastore (couchbase)
%% @end
%%--------------------------------------------------------------------
-spec model_init() -> model_behaviour:model_config().
model_init() ->
    % TODO migrate to GLOBALLY_CACHED_LEVEL
    StoreLevel = application:get_env(?APP_Name, user_store_level, ?DISK_ONLY_LEVEL),
    Hooks = record_location_hooks:get_hooks(),
    ?MODEL_CONFIG(onedata_user_bucket, Hooks, StoreLevel).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback 'after'/5.
%% @end
%%--------------------------------------------------------------------
-spec 'after'(ModelName :: model_behaviour:model_type(), Method :: model_behaviour:model_action(),
    Level :: datastore:store_level(), Context :: term(),
    ReturnValue :: term()) -> ok.
'after'(ModelName, Method, _Level, Context, ReturnValue) ->
    record_location_hooks:handle_after(ModelName, Method, Context, ReturnValue).

%%--------------------------------------------------------------------
%% @doc
%% {@link model_behaviour} callback before/4.
%% @end
%%--------------------------------------------------------------------
-spec before(ModelName :: model_behaviour:model_type(), Method :: model_behaviour:model_action(),
    Level :: datastore:store_level(), Context :: term()) -> ok | datastore:generic_error().
before(ModelName, Method, _Level, Context) ->
    record_location_hooks:handle_before(ModelName, Method, Context).

%%%===================================================================
%%% API callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 
%% @private
%% Gets all users from DB (that have at least one email address set).
%% This function is used for development purposes, there appears to be no production use case.
%% @end
%%--------------------------------------------------------------------
-spec get_all_ids() -> {ok, [binary()]}.
get_all_ids() ->
    Filter = fun
        ('$end_of_table', Acc) ->
            {abort, Acc};
        (#document{value = #onedata_user{}, key = Id}, Acc) ->
            {next, [Id | Acc]};
        (_, Acc) ->
            {next, Acc}
    end,
    datastore:list(?STORE_LEVEL, ?MODEL_NAME, Filter, []).

%%--------------------------------------------------------------------
%% @doc 
%% @private
%% Gets first user matching given criterion.
%% todo: change implementation to something fast (connected with VFS-1498)
%% @end
%%--------------------------------------------------------------------

-spec get_by_criterion(Criterion :: {connected_account_user_id, {ProviderID :: atom(), UserID :: binary()}} |
{email, binary()} | {alias, binary()}) ->
    {ok, #document{}} | {error, any()}.

get_by_criterion({email, Value}) ->
    Filter = fun
        ('$end_of_table', Acc) ->
            {abort, Acc};
        (#document{value = #onedata_user{email_list = EmailList}} = Doc, Acc) ->
            case lists:member(Value, EmailList) of
                true -> {abort, [Doc | Acc]};
                false -> {next, Acc}
            end;
        (_, Acc) ->
            {next, Acc}
    end,
    {ok, [Result | _]} = datastore:list(?STORE_LEVEL, ?MODEL_NAME, Filter, []),
    {ok, Result};

get_by_criterion({alias, Value}) ->
    Filter = fun
        ('$end_of_table', Acc) ->
            {abort, Acc};
        (#document{value = #onedata_user{alias = Alias}} = Doc, Acc) ->
            case Alias of
                Value -> {abort, [Doc | Acc]};
                _ -> {next, Acc}
            end;
        (_, Acc) ->
            {next, Acc}
    end,
    {ok, [Result | _]} = datastore:list(?STORE_LEVEL, ?MODEL_NAME, Filter, []),
    {ok, Result};

get_by_criterion({connected_account_user_id, {ProviderID, UserID}}) ->
    Filter = fun
        ('$end_of_table', Acc) ->
            {abort, Acc};
        (#document{value = #onedata_user{connected_accounts = Accounts}} = Doc, Acc) ->
            Found = lists:any(fun
                (#oauth_account{provider_id = PID, user_id = UID}) ->
                    case {PID, UID} of
                        {ProviderID, UserID} -> true;
                        _ -> false
                    end;
                (_) -> false
            end, Accounts),
            case Found of
                true -> {abort, [Doc | Acc]};
                _ -> {next, Acc}
            end;
        (_, Acc) ->
            {next, Acc}
    end,
    {ok, [Result | _]} = datastore:list(?STORE_LEVEL, ?MODEL_NAME, Filter, []),
    {ok, Result}.
