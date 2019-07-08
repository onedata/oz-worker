%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for singleton record that stores a global, shared token secret for
%%% temporary tokens. The secret can be regenerated, in such case all existing
%%% temporary tokens become invalid.
%%% @end
%%%-------------------------------------------------------------------
-module(shared_token_secret).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([init/0, get/0, regenerate/0]).

%% datastore_model callbacks
-export([get_record_struct/1]).

-define(CTX, #{
    model => ?MODULE
}).

-define(SINGLETON_DB_KEY, <<"secret">>).
-define(CACHE_KEY, shared_token_secret).

-define(CLUSTER_NODES, element(2, {ok, _} = node_manager:get_cluster_nodes())).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Ensures that shared token secret exists - creates a new one if there is none.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    case datastore_model:get(?CTX, ?SINGLETON_DB_KEY) of
        {ok, _} -> ok;
        {error, not_found} -> regenerate()
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the shared token secret.
%% @end
%%--------------------------------------------------------------------
-spec get() -> tokens:secret().
get() ->
    {ok, Secret} = simple_cache:get(?CACHE_KEY, fun() ->
        {ok, Doc} = datastore_model:get(?CTX, ?SINGLETON_DB_KEY),
        {true, Doc#document.value#shared_token_secret.secret}
    end),
    Secret.


%%--------------------------------------------------------------------
%% @doc
%% Regenerates the shared token secret. Causes all existing temporary tokens
%% to be invalidated.
%% @end
%%--------------------------------------------------------------------
-spec regenerate() -> ok.
regenerate() ->
    datastore_model:save(?CTX, #document{
        key = ?SINGLETON_DB_KEY,
        value = #shared_token_secret{secret = tokens:generate_secret()}
    }),
    lists:foreach(fun(Node) ->
        ok = rpc:call(Node, simple_cache, clear, [?CACHE_KEY])
    end, ?CLUSTER_NODES),
    ?warning(
        "Generated a new shared token secret. "
        "All existing temporary tokens have been invalidated."
    ),
    ok.

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
        {secret, binary}
    ]}.
