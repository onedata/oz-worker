%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Module exposing oz-worker functions, that are used in tests.
%%% @end
%%%-------------------------------------------------------------------
-module(test_rpc_api).
-author("Piotr Duleba").

-include("entity_logic.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").

-export([
    get_configuration/0,
    timestamp_seconds/0,
    get_env/1,

    list_users/1,
    create_user/1,
    create_user/2,
    get_user_protected_data/2,
    create_user_temporary_token/3,
    authenticate/2,

    create_group/2,
    get_group_users/2,

    create_space/3,
    create_space_support_token/2,
    list_spaces/1,
    get_space_protected_data/2,
    delete_space/2
]).


%%%===================================================================
%%% API
%%%===================================================================


-spec get_configuration() -> {ok, #{atom() := term()}} | {error, Reason :: term()}.
get_configuration() ->
    zone_logic:get_configuration().


-spec timestamp_seconds() -> time:seconds().
timestamp_seconds() ->
    global_clock:timestamp_seconds().


-spec get_env(Key :: atom()) -> term() | no_return().
get_env(Env) ->
    oz_worker:get_env(Env).


-spec list_users(aai:auth()) -> {ok, [od_user:id()]} | {error, term()}.
list_users(Auth) ->
    rpc_api:list_users(Auth).


-spec create_user(aai:auth()) -> {ok, od_user:id()} | errors:error().
create_user(Auth) ->
    user_logic:create(Auth).


-spec create_user(aai:auth(), Data :: map()) -> {ok, od_user:id()} | errors:error().
create_user(Auth, Data) ->
    rpc_api:create_user(Auth, Data).


-spec get_user_protected_data(aai:auth(), od_user:id()) -> {ok, #user_details{}} | {error, term()}.
get_user_protected_data(Auth, UserId) ->
    user_logic:get_protected_data(Auth, UserId).


-spec create_user_temporary_token(aai:auth(), od_user:id(), map()) ->
    {ok, tokens:token()} | no_return().
create_user_temporary_token(Auth, UserId, Data) ->
    token_logic:create_user_temporary_token(Auth, UserId, Data).


-spec authenticate(od_user:username(), binary()) ->
    {true, aai:auth()} | errors:unauthorized_error().
authenticate(UserName, Password) ->
    basic_auth:authenticate(UserName, Password).


-spec create_group(aai:auth(), Data :: map()) -> {ok, od_user:id()} | errors:error().
create_group(Auth, GroupName) ->
    group_logic:create(Auth, GroupName).


-spec get_group_users(aai:auth(), od_group:id()) ->
    {ok, [od_user:id()]} | errors:error().
get_group_users(Auth, GroupId) ->
    group_logic:get_users(Auth, GroupId).


-spec create_space(aai:auth(), od_user:id(),
    NameOrData :: binary() | map()) -> {ok, od_space:id()} | errors:error().
create_space(Config, UserId, NameOrData) ->
    user_logic:create_space(Config, UserId, NameOrData).


-spec create_space_support_token(aai:auth(), od_space:id()) ->
    {ok, tokens:token()} | errors:error().
create_space_support_token(Auth, SpaceId) ->
    space_logic:create_space_support_token(Auth, SpaceId).


-spec list_spaces(aai:auth()) ->
    {ok, [od_space:id()]} | errors:error().
list_spaces(Auth) ->
    space_logic:list(Auth).


-spec get_space_protected_data(aai:auth(), od_space:id()) -> {ok, map()} | no_return().
get_space_protected_data(Auth, SpaceId) ->
    space_logic:get_protected_data(Auth, SpaceId).


-spec delete_space(aai:auth(), od_space:id()) -> ok | errors:error().
delete_space(Auth, SpaceId) ->
    space_logic:delete(Auth, SpaceId).
