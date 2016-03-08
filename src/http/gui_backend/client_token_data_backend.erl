%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements data_backend_behaviour and is used to synchronize
%%% the file model used in Ember application.
%%% THIS IS A PROTOTYPE AND AN EXAMPLE OF IMPLEMENTATION.
%%% @end
%%%-------------------------------------------------------------------
-module(client_token_data_backend).
-author("Lukasz Opiola").

-compile([export_all]).

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([init/0]).
-export([find/2, find_all/1, find_query/2]).
-export([create_record/2, update_record/3, delete_record/2]).

%% Convenience macro to log a debug level log dumping given variable.
-define(log_debug(_Arg),
    ?alert("~s", [str_utils:format("CLIENT_TOKEN_DATA_BACKEND: ~s: ~p", [??_Arg, _Arg])])
).


init() ->
    ?log_debug({websocket_init, g_session:get_session_id()}),
    ok.


find(<<"clienttoken">>, _Ids) ->
    {error, not_iplemented}.

find_query(<<"clienttoken">>, _Data) ->
    {error, not_iplemented}.

%% Called when ember calls findAll
find_all(<<"clienttoken">>) ->
    UserId = g_session:get_user_id(),
    {ok, ClientTokens} = user_logic:get_client_tokens(UserId),
    Res = lists:map(
        fun(Id) ->
            [{<<"id">>, Id}]
        end, ClientTokens),
    {ok, Res}.

%% Called when ember asks to create a record
create_record(<<"clienttoken">>, _Data) ->
    UserId = g_session:get_user_id(),
    Token = auth_logic:gen_token(UserId),
    user_logic:add_client_token(UserId, Token),
    {ok, [
        {<<"id">>, Token}
    ]}.

%% Called when ember asks to update a record
update_record(<<"clienttoken">>, _TokenId, _Data) ->
    {error, not_iplemented}.

%% Called when ember asks to delete a record
delete_record(<<"clienttoken">>, Token) ->
    UserId = g_session:get_user_id(),
    {ok, Macaroon} = macaroon:deserialize(Token),
    Identifier = macaroon:identifier(Macaroon),
    onedata_auth:delete(Identifier),
    user_logic:delete_client_token(UserId, Token).
