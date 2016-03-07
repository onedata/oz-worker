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
    ?debug("~s", [str_utils:format("CLIENT_TOKEN_DATA_BACKEND: ~s: ~p", [??_Arg, _Arg])])
).


init() ->
    ?log_debug({websocket_init, g_session:get_session_id()}),
%%    {ok, _Pid} = data_backend:async_process(fun() -> async_loop() end),
    ok.


find(<<"clienttoken">>, ProviderIds) ->
    {error, not_iplemented}.
    % UserId = g_session:get_user_id(),
    % Res = lists:map(
    %     fun(ProviderId) ->
    %         {ok, ProviderData} = provider_logic:get_data(ProviderId),
    %         Name = proplists:get_value(clientName, ProviderData),
    %         IsWorking = provider_logic:check_provider_connectivity(ProviderId),
    %         {ok, [{spaces, Spaces}]} = provider_logic:get_spaces(ProviderId),
    %         {ok, #document{
    %             value = #onedata_user{
    %                 default_provider = DefaultProvider
    %             }
    %         }} = user_logic:get_user_doc(UserId),
    %         [
    %             {<<"id">>, ProviderId},
    %             {<<"name">>, Name},
    %             {<<"isDefault">>, ProviderId =:= DefaultProvider},
    %             {<<"isWorking">>, IsWorking},
    %             {<<"spaces">>, Spaces}
    %         ]
    %     end, ProviderIds),
    % {ok, Res}.

find_query(<<"clienttoken">>, _Data) ->
    {error, not_iplemented}.

%% Called when ember asks for all files
find_all(<<"clienttoken">>) ->
   {ok, [
      [{<<"id">>, <<"fsdsfaADSFhAERtaDSFgadgaeQRtEQRtErtfsdsfaADSFhAERtaDSFgadgaeQRtEQRtErtfsdsfaADSFhAERtaDSFgadgaeQRtEQRtErt">>}],
      [{<<"id">>, <<"MAdfmASDMFAmsdfmasdFasjdfiouasdMAdfmASDMFAmsdfmasdFasjdfiouasdMAdfmASDMFAmsdfmasdFasjdfiouasdMAdfmASDMFAmsdfmasdFasjdfiouasd">>}]
   ]}.
    % UserId = g_session:get_user_id(),
    % {ok, [{providers, ProviderIds}]} = user_logic:get_providers(UserId),
    % {ok, _Res} = find(<<"clienttoken">>, ProviderIds).


%% Called when ember asks to create a record
create_record(<<"clienttoken">>, _Data) ->
    {ok, [
      {<<"id">>, <<"nowynowynowynowynowynowynowynowynowynowynowynowynowynowynowynowynowynowynowynowynowynowynowynowynowynowynowynowynowynowy">>}
    ]}.

%% Called when ember asks to update a record
update_record(<<"clienttoken">>, _TokenId, Data) ->
    {error, not_iplemented}.

%% Called when ember asks to delete a record
delete_record(<<"clienttoken">>, _Id) ->
    ok.
