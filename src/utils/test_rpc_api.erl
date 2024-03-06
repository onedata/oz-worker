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
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").

-export([
    simulate_downtime/1,
    timestamp_seconds/0,
    get_env/1,
    set_env/2,
    get_domain/0,

    add_user_to_cluster/4,

    list_users/1,
    create_user/1,
    create_user/2,
    get_user_protected_data/2,
    create_user_temporary_token/3,
    create_user_temporary_access_token/1,
    are_basic_credentials_valid/2,

    create_group/2,
    get_group_users/2,

    list_spaces/1,
    create_space/3,
    add_user_to_space/3,
    create_space_support_token/2,
    get_space_protected_data/2,
    space_set_user_privileges/4,
    update_space_name/3,
    delete_space/2,

    create_inventory_for_user/3,
    add_user_to_inventory/4,
    create_lambda/2,
    create_workflow_schema/2,
    update_workflow_schema/3,
    get_workflow_schema/2
]).

-define(DEFAULT_TEMP_CAVEAT_TTL, 360000).


%%%===================================================================
%%% API
%%%===================================================================


-spec simulate_downtime(time:seconds()) -> ok | {error, term()}.
simulate_downtime(Seconds) ->
    https_listener:stop(),
    timer:sleep(timer:seconds(Seconds)),
    https_listener:start().


-spec timestamp_seconds() -> time:seconds().
timestamp_seconds() ->
    global_clock:timestamp_seconds().


-spec get_env(Key :: atom()) -> term() | no_return().
get_env(Env) ->
    oz_worker:get_env(Env).


-spec set_env(Key :: atom(), term()) -> ok.
set_env(Env, Value) ->
    oz_worker:set_env(Env, Value).


-spec get_domain() -> binary().
get_domain() ->
    oz_worker:get_domain().


-spec add_user_to_cluster(aai:auth(), od_cluster:id(), od_user:id(), [privileges:cluster_privilege()]) -> {ok, od_user:id()} | errors:error().
add_user_to_cluster(Auth, ClusterId, UserId, Privileges) ->
    cluster_logic:add_user(Auth, ClusterId, UserId, Privileges).


-spec list_users(aai:auth()) -> {ok, [od_user:id()]} | {error, term()}.
list_users(Auth) ->
    rpc_api:list_users(Auth).


-spec create_user(aai:auth()) -> {ok, od_user:id()} | errors:error().
create_user(Auth) ->
    user_logic:create(Auth).


-spec create_user(aai:auth(), Data :: map()) -> {ok, od_user:id()} | errors:error().
create_user(Auth, Data) ->
    rpc_api:create_user(Auth, Data).


-spec get_user_protected_data(aai:auth(), od_user:id()) -> {ok, map()} | {error, term()}.
get_user_protected_data(Auth, UserId) ->
    user_logic:get_protected_data(Auth, UserId).


-spec create_user_temporary_token(aai:auth(), od_user:id(), map()) ->
    {ok, tokens:token()} | no_return().
create_user_temporary_token(Auth, UserId, Data) ->
    token_logic:create_user_temporary_token(Auth, UserId, Data).


-spec create_user_temporary_access_token(od_user:id()) ->
    {ok, binary()} | no_return().
create_user_temporary_access_token(UserId) ->
    Auth = ?USER(UserId),
    Now = timestamp_seconds(),
    {ok, Token} = create_user_temporary_token(Auth, UserId,#{
        <<"type">> => ?ACCESS_TOKEN,
        <<"caveats">> => [#cv_time{valid_until = Now + ?DEFAULT_TEMP_CAVEAT_TTL}]
    }),
    tokens:serialize(Token).


-spec are_basic_credentials_valid(od_user:username(), binary()) -> boolean().
are_basic_credentials_valid(UserName, Password) ->
    case basic_auth:authenticate(UserName, Password) of
        {true, _} -> true;
        ?ERROR_UNAUTHORIZED(?ERROR_BAD_BASIC_CREDENTIALS) -> false
    end.


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


-spec add_user_to_space(aai:auth(), od_space:id(), od_user:id()) ->
    {ok, od_user:id()} | errors:error().
add_user_to_space(Auth, SpaceId, UserId) ->
    space_logic:add_user(Auth, SpaceId, UserId).


-spec create_space_support_token(aai:auth(), od_space:id()) ->
    {ok, tokens:token()} | errors:error().
create_space_support_token(Auth = ?USER(UserId), SpaceId) ->
    token_logic:create_user_temporary_token(Auth, UserId, #{
        <<"type">> => ?INVITE_TOKEN(?SUPPORT_SPACE, SpaceId),
        <<"caveats">> => [#cv_time{valid_until = global_clock:timestamp_seconds() + 3600}]
    }).


-spec list_spaces(aai:auth()) ->
    {ok, [od_space:id()]} | errors:error().
list_spaces(Auth) ->
    space_logic:list(Auth).


-spec get_space_protected_data(aai:auth(), od_space:id()) -> {ok, map()} | no_return().
get_space_protected_data(Auth, SpaceId) ->
    space_logic:get_protected_data(Auth, SpaceId).


-spec space_set_user_privileges(
    aai:auth(), od_space:id(), od_user:id(), [privileges:space_privilege()]
) ->
    ok | errors:error().
space_set_user_privileges(Auth, SpaceId, UserId, Privileges) ->
    space_logic:update_user_privileges(
        Auth, SpaceId, UserId, Privileges, lists_utils:subtract(privileges:space_admin(), Privileges)
    ).


-spec update_space_name(aai:auth(), od_space:id(), od_space:name()) ->
    {ok, od_user:id()} | errors:error().
update_space_name(Auth, SpaceId, NewName) ->
    space_logic:update(Auth, SpaceId, NewName).


-spec delete_space(aai:auth(), od_space:id()) -> ok | errors:error().
delete_space(Auth, SpaceId) ->
    space_logic:delete(Auth, SpaceId).


-spec create_inventory_for_user(aai:auth(), od_user:id(), od_atm_inventory:name() | entity_logic:data()) ->
    {ok, od_atm_inventory:id()} | errors:error().
create_inventory_for_user(Auth, UserId, NameOrData) ->
    user_logic:create_atm_inventory(Auth, UserId, NameOrData).


-spec add_user_to_inventory(
    aai:auth(),
    od_atm_inventory:id(),
    od_user:id(),
    [privileges:atm_inventory_privilege()] | entity_logic:data()
) ->
    {ok, od_user:id()} | errors:error().
add_user_to_inventory(Auth, AtmInventoryId, UserId, Privileges) ->
    atm_inventory_logic:add_user(Auth, AtmInventoryId, UserId, Privileges).


-spec create_lambda(aai:auth(), od_atm_lambda:name() | entity_logic:data()) ->
    {ok, od_atm_lambda:id()} | errors:error().
create_lambda(Auth, Data) ->
    atm_lambda_logic:create(Auth, Data).


-spec create_workflow_schema(aai:auth(), od_atm_workflow_schema:name() | entity_logic:data()) ->
    {ok, od_atm_workflow_schema:id()} | errors:error().
create_workflow_schema(Auth, NameOrData) ->
    atm_workflow_schema_logic:create(Auth, NameOrData).


-spec update_workflow_schema(aai:auth(), od_atm_workflow_schema:id(), entity_logic:data()) -> ok.
update_workflow_schema(Auth, AtmWorkflowSchemaId, Data) ->
    atm_workflow_schema_logic:update(Auth, AtmWorkflowSchemaId, Data).


-spec get_workflow_schema(aai:auth(), od_atm_workflow_schema:id()) ->
    {ok, od_atm_workflow_schema:record()} | errors:error().
get_workflow_schema(Auth, AtmWorkflowSchemaId) ->
    atm_workflow_schema_logic:get(Auth, AtmWorkflowSchemaId).
