%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% API for records that store information about named tokens. Named tokens can
%%% be viewed through REST API / GUI and revoked at will (as opposed to
%%% temporary tokens). They can be created in the name of a user or provider,
%%% which is called token subject. Each token must have a unique name
%%% (per given subject, i.e. a user cannot have two tokens with the same name,
%%% but two different users can). Named tokens can be referenced by their name -
%%% link tree is used for underlying implementation.
%%% @end
%%%-------------------------------------------------------------------
-module(od_token).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

-type id() :: tokens:nonce().
-type record() :: #od_token{}.
-type doc() :: datastore_doc:doc(record()).
-type diff() :: datastore_doc:diff(record()).
-type name() :: binary().
-type metadata() :: json_utils:json_term().
-export_type([record/0, name/0, metadata/0]).

%% API
-export([create/1, get/1, exists/1, update/2, delete/1]).
-export([entity_logic_plugin/0]).

%% datastore_model callbacks
-export([get_record_struct/1]).


-define(CTX, #{
    model => ?MODULE,
    sync_enabled => true
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec create(doc()) -> {ok, doc()} | {error, term()}.
create(Doc) ->
    datastore_model:create(?CTX, Doc).

-spec get(id()) -> {ok, doc()} | {error, term()}.
get(Id) ->
    datastore_model:get(?CTX, Id).

-spec exists(id()) -> {ok, boolean()} | {error, term()}.
exists(Id) ->
    datastore_model:exists(?CTX, Id).

-spec update(id(), diff()) -> {ok, doc()} | {error, term()}.
update(Id, Diff) ->
    datastore_model:update(?CTX, Id, Diff).

-spec delete(id()) -> ok | {error, term()}.
delete(Id) ->
    datastore_model:delete(?CTX, Id).

%%--------------------------------------------------------------------
%% @doc
%% Returns the entity logic plugin module that handles model logic.
%% @end
%%--------------------------------------------------------------------
-spec entity_logic_plugin() -> module().
entity_logic_plugin() ->
    token_logic_plugin.

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
        {name, string},
        {version, integer},
        {subject, {custom, string, {aai, serialize_subject, deserialize_subject}}},
        {type, {custom, string, {tokens, serialize_type, deserialize_type}}},
        {caveats, [string]},
        {secret, string},
        {metadata, {custom, json, {json_utils, encode, decode}}},
        {revoked, boolean}
    ]}.
