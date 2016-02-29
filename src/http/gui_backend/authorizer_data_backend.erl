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
-module(authorizer_data_backend).
-author("Lukasz Opiola").

-compile([export_all]).

-include_lib("ctool/include/logging.hrl").

%% API
-export([init/0]).
-export([find/2, find_all/1, find_query/2]).
-export([create_record/2, update_record/3, delete_record/2]).

%% Convenience macro to log a debug level log dumping given variable.
-define(log_debug(_Arg),
    ?debug("~s", [str_utils:format("AUTHORIZER_DATA_BACKEND: ~s: ~p", [??_Arg, _Arg])])
).


init() ->
    ?log_debug({websocket_init, g_session:get_session_id()}),
%%    {ok, _Pid} = data_backend:async_process(fun() -> async_loop() end),
    ok.


find(<<"authorizer">>, [_AuthorizerId]) ->
    {error, not_iplemented}.

find_query(<<"authorizer">>, _Data) ->
    {error, not_iplemented}.

%% Called when ember asks for all files
find_all(<<"authorizer">>) ->
    Res = [
        [
            {<<"id">>, <<"a1">>},
            {<<"type">>, <<"google">>},
            {<<"email">>, <<"liput@gmail.com">>}
        ],
        [
            {<<"id">>, <<"a2">>},
            {<<"type">>, <<"dropbox">>},
            {<<"email">>, <<"liputdb@gmail.com">>}
        ],
        [
            {<<"id">>, <<"a3">>},
            {<<"type">>, <<"facebook">>},
            {<<"email">>, <<"liputfb@gmail.com">>}
        ]
    ],
    {ok, Res}.


%% Called when ember asks to create a record
create_record(<<"authorizer">>, _Data) ->
    {error, not_iplemented}.

%% Called when ember asks to update a record
update_record(<<"authorizer">>, _Id, _Data) ->
    {error, not_iplemented}.

%% Called when ember asks to delete a record
delete_record(<<"authorizer">>, _Id) ->
    {error, not_iplemented}.
