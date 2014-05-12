%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: The module implementing the business logic for registry's users.
%% This module serves as a buffer between the database and the REST API.
%% @end
%% ===================================================================
-module(user_logic).
-author("Konrad Zemek").

%% API
-export([register_user/1, modify_data/2, new_accounts_merge_token/1,
    new_space_create_token/1, merge_accounts/2, join_space/2, join_group/2,
    unregister_user/1, get_data/1]).


-spec register_user(Name :: binary()) -> {ok, binary()} | {error, any()}.
register_user(Name) ->
    {ok, <<"userid">>}.


-spec get_data(UserId :: binary()) -> {ok, [proplists:property()]} | {error, any()}.
get_data(UserId) ->
    {ok, [{<<"name">>, <<"loluser">>}]}.


-spec modify_data(UserId :: binary(), Modifications :: [proplists:property()]) -> ok | {error, any()}.
modify_data(UserId, Modifications) ->
    ok.


-spec new_accounts_merge_token(UserId :: binary()) -> {ok, binary()} | {error, any()}.
new_accounts_merge_token(UserId) ->
    {ok, <<"accountsmergetoken">>}.


-spec new_space_create_token(UserId :: binary()) -> {ok, binary()} | {error, any()}.
new_space_create_token(UserId) ->
    {ok, <<"spacecreatetoken">>}.


-spec merge_accounts(UserId :: binary(), Token :: binary()) -> ok | {error, any()}.
merge_accounts(UserId, Token) ->
    ok.


-spec join_space(UserId :: binary(), Token :: binary()) -> {ok, binary()} | {error, any()}.
join_space(UserId, Token) ->
    {ok, <<"spaceid">>}.


-spec join_group(UserId :: binary(), Token :: binary()) -> {ok, binary()} | {error, any()}.
join_group(UserId, Token) ->
    {ok, <<"groupid">>}.


-spec unregister_user(UserId :: binary()) -> ok | {error, any()}.
unregister_user(UserId) ->
    ok.
