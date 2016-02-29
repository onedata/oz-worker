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

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([init/0]).
-export([find/2, find_all/1, find_query/2]).
-export([create_record/2, update_record/3, delete_record/2]).

%% Convenience macro to log a debug level log dumping given variable.
-define(log_debug(_Arg),
    ?alert("~s", [str_utils:format("AUTHORIZER_DATA_BACKEND: ~s: ~p", [??_Arg, _Arg])])
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
    {ok, #document{value = #onedata_user{connected_accounts = OAuthAccounts}}} =
        onedata_user:get(g_session:get_user_id()),
    % One connected account can have multiple email addresses - create
    % a separate authorizer record for every
    Res = lists:foldl(
        fun(OAuthAccount, Acc) ->
            #oauth_account{
                provider_id = Provider,
                email_list = Emails,
                user_id = UserId} = OAuthAccount,
            ProviderBin = str_utils:to_binary(Provider),
            AccId = <<ProviderBin/binary, "#", UserId/binary>>,
            Accounts = lists:map(
                fun(Email) ->
                    [
                        {<<"id">>, AccId},
                        {<<"type">>, ProviderBin},
                        {<<"email">>, Email}
                    ]
                end, Emails),
            Accounts ++ Acc
        end, [], OAuthAccounts),
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
