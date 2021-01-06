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
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").

-export([
    get_zone_configuration/0,
    get_env/1,

    list_users/1,
    create_user/1,
    create_user/2,
    get_user_details/2,
    authenticate/2,

    create_group/2,
    get_group_users/2,

    create_space/3,
    create_space_support_token/2,
    get_space_ids/1,
    delete_space/2,

    get_token_logic_required_admin_priviliges/1
]).


%%%===================================================================
%%% Exposed functions
%%%===================================================================
-spec get_zone_configuration() -> {ok, #{atom() := term()}} | {error, Reason :: term()}.
get_zone_configuration() ->
    rpc_api:get_zone_configuration().


-spec get_env(Key :: atom()) -> term() | no_return().
get_env(Env) ->
    rpc_api:get_env(Env).


-spec list_users(aai:auth()) -> {ok, [od_user:id()]} | {error, term()}.
list_users(Auth) ->
    rpc_api:list_users(Auth).


-spec create_user(aai:auth()) -> {ok, od_user:id()} | errors:error().
create_user(Auth) ->
    rpc_api:create_user(Auth).


-spec create_user(aai:auth(), Data :: map()) -> {ok, od_user:id()} | errors:error().
create_user(Auth, Data) ->
    rpc_api:create_user(Auth, Data).


-spec get_user_details(aai:auth(), od_user:id()) -> {ok, #user_details{}} | {error, term()}.
get_user_details(Auth, UserId) ->
    rpc_api:get_user_protected_data(Auth, UserId).


-spec authenticate(od_user:username(), binary()) ->
    {true, aai:auth()} | errors:unauthorized_error().
authenticate(UserName, Password) ->
    rpc_api:authenticate(UserName, Password).


-spec create_group(Auth :: aai:auth(), Data :: map()) -> {ok, od_user:id()} | errors:error().
create_group(Auth, GroupName) ->
    rpc_api:create_group(Auth, GroupName).


-spec get_group_users(Auth :: aai:auth(), GroupId :: od_group:id()) ->
    {ok, [od_user:id()]} | errors:error().
get_group_users(Auth, GroupId) ->
    rpc_api:get_group_users(Auth, GroupId).


-spec create_space(Auth :: aai:auth(), UserId :: od_user:id(),
    NameOrData :: binary() | #{}) -> {ok, od_space:id()} | errors:error().
create_space(Config, UserId, NameOrData) ->
    rpc_api:create_space(Config, UserId, NameOrData).


-spec create_space_support_token(Auth :: aai:auth(), SpaceId :: od_space:id()) ->
    {ok, tokens:token()} | errors:error().
create_space_support_token(Auth, SpaceId) ->
    rpc_api:create_space_support_token(Auth, SpaceId).


-spec get_space_ids(Auth :: aai:auth()) ->
    {ok, [od_space:id()]} | errors:error().
get_space_ids(Auth) ->
    rpc_api:list_spaces(Auth).


-spec delete_space(Auth :: aai:auth(), SpaceId :: od_space:id()) -> ok | errors:error().
delete_space(Auth, SpaceId) ->
    rpc_api:delete_space(Auth, SpaceId).


-spec get_token_logic_required_admin_priviliges(entity_logic:req()) -> [privileges:oz_privilege()] | forbidden.
get_token_logic_required_admin_priviliges(Arg) ->
    rpc_api:get_token_logic_required_admin_priviliges(Arg).
