%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% identity records into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(identity_rest_translator).
-author("Lukasz Opiola").

-include("rest.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-export([response/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Translates given entity logic result into REST response
%% expressed by #rest_resp{} record.
%% @end
%%--------------------------------------------------------------------
-spec response(Operation :: n_entity_logic:operation(),
    EntityId :: n_entity_logic:entity_id(), Resource :: n_entity_logic:resource(),
    Result :: n_entity_logic:result()) -> #rest_resp{}.
response(create, undefined, {provider, _Id}, ok) ->
    n_rest_handler:ok_no_content_reply();

response(get, undefined, {publickey, _Id}, {ok, EncodedPublicKey}) ->
    n_rest_handler:ok_body_reply(#{<<"publicKey">> => EncodedPublicKey});

response(update, undefined, _, ok) ->
    n_rest_handler:updated_reply();

response(delete, undefined, _, ok) ->
    n_rest_handler:deleted_reply().

