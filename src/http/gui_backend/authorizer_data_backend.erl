%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements data_backend_behaviour and is used to synchronize
%%% the `authorizer` model used in Ember application.
%%% @end
%%%-------------------------------------------------------------------
-module(authorizer_data_backend).
-author("Lukasz Opiola").
-behaviour(data_backend_behaviour).

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([init/0]).
-export([find/2, find_all/1, find_query/2]).
-export([create_record/2, update_record/3, delete_record/2]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback init/0.
%% @end
%%--------------------------------------------------------------------
init() ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback find/2.
%% @end
%%--------------------------------------------------------------------
find(<<"authorizer">>, [_AuthorizerId]) ->
    {error, <<"Not implemented">>}.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback find_query/2.
%% @end
%%--------------------------------------------------------------------
find_query(<<"authorizer">>, _Data) ->
    {error, <<"Not implemented">>}.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback find_all/1.
%% @end
%%--------------------------------------------------------------------
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
            UserIdBin = str_utils:to_binary(UserId),
            AccId = <<ProviderBin/binary, "#", UserIdBin/binary>>,
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


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback create_record/2.
%% @end
%%--------------------------------------------------------------------
create_record(<<"authorizer">>, _Data) ->
    {error, <<"Not implemented">>}.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback update_record/3.
%% @end
%%--------------------------------------------------------------------
update_record(<<"authorizer">>, _Id, _Data) ->
    {error, <<"Not implemented">>}.


%%--------------------------------------------------------------------
%% @doc
%% {@link authorizer_data_backend} callback delete_record/2.
%% @end
%%--------------------------------------------------------------------
delete_record(<<"authorizer">>, _Id) ->
    {error, <<"Not implemented">>}.
