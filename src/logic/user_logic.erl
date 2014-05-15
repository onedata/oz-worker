%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The module implementing the business logic for registry's users.
%% This module serves as a buffer between the database and the REST API.
%% @end
%% ===================================================================
-module(user_logic).
-author("Konrad Zemek").


%% API
-export([create/1, modify/2, merge/2, get_data/1, get_spaces/1, get_groups/1,
    remove/1]).


%% create/1
%% ====================================================================
%% @doc Creates a user account.
%% ====================================================================
-spec create(Name :: binary()) ->
    {ok, UserId :: binary()} | {error, Reason :: any()}.
%% ====================================================================
create(Name) ->
    {ok, <<"userid">>}.


%% modify/2
%% ====================================================================
%% @doc Modifies user details.
%% ====================================================================
-spec modify(UserId :: binary(), Name :: binary()) ->
    ok | {error, Reason :: any()}.
%% ====================================================================
modify(UserId, Name) ->
    ok.


%% merge/2
%% ====================================================================
%% @doc Merges an account identified by token into current user's account.
%% ====================================================================
-spec merge(UserId :: binary(), Token :: binary()) ->
    ok | {error, Reason :: any()}.
%% ====================================================================
merge(UserId, Token) ->
    ok.


%% get_data/1
%% ====================================================================
%% @doc Returns user details.
%% ====================================================================
-spec get_data(UserId :: binary()) ->
    {ok, [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_data(UserId) ->
    {ok, [{name, <<"username">>}]}.


%% get_spaces/1
%% ====================================================================
%% @doc Returns user's spaces.
%% ====================================================================
-spec get_spaces(UserId :: binary()) ->
    {ok, [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_spaces(UserId) ->
    {ok, [{spaces, <<"spaces">>}]}.


%% get_groups/1
%% ====================================================================
%% @doc Returns user's groups.
%% ====================================================================
-spec get_groups(UserId :: binary()) ->
    {ok, [proplists:property()]} | {error, Reason :: any()}.
%% ====================================================================
get_groups(UserId) ->
    {ok, [{groups, <<"groups">>}]}.


%% remove/1
%% ====================================================================
%% @doc Remove user's account.
%% ====================================================================
-spec remove(UserId :: binary()) -> boolean().
%% ====================================================================
remove(UserId) ->
    true.
