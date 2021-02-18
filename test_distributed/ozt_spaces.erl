%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for manipulating spaces of oz-worker service in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_spaces).
-author("Lukasz Opiola").

-include("ozt.hrl").
-include_lib("ctool/include/space_support/support_stage.hrl").
-include_lib("ctool/include/space_support/provider_sync_progress.hrl").
-include_lib("ctool/include/space_support/provider_capacity_usage.hrl").

%% API
-export([create/0, create/1]).
-export([get/1, exists/1]).
-export([add_owner/2, remove_owner/2]).
-export([add_user/2, add_user/3]).
-export([remove_user/2]).
-export([add_group/2, add_group/3]).
-export([remove_group/2]).
-export([create_user_invite_token/2]).
-export([create_group_invite_token/2]).
-export([create_support_token/2, create_support_token/3]).
-export([create_share/2]).
-export([get_user_privileges/2, get_group_privileges/2]).
-export([set_user_privileges/3]).
-export([delete/1]).
-export([get_support_parameters_registry/1]).
-export([get_support_stage_registry/1]).
-export([get_capacity_usage_registry/1]).
-export([extract_sync_progress_registry_matrix/1]).
-export([minimum_support_size/0]).
-export([has_default_support_parameters/2, has_legacy_support_stages/2]).
-export([has_initial_capacity_usage/3, has_initial_sync_progress/3]).

-compile([{no_auto_import, [get/1]}]).

%%%===================================================================
%%% API
%%%===================================================================

-spec create() -> od_space:id().
create() ->
    create(?UNIQUE_STRING).


-spec create(od_space:name()) -> od_space:id().
create(Name) ->
    {ok, SpaceId} = ?assertMatch({ok, _}, ozt:rpc(space_logic, create, [?ROOT, #{<<"name">> => Name}])),
    SpaceId.


-spec get(od_space:id()) -> od_space:record().
get(SpaceId) ->
    {ok, Space} = ?assertMatch({ok, _}, ozt:rpc(space_logic, get, [?ROOT, SpaceId])),
    Space.


-spec exists(od_space:id()) -> od_space:record().
exists(SpaceId) ->
    ozt:rpc(space_logic, exists, [SpaceId]).


-spec add_owner(od_space:id(), od_user:id()) -> ok.
add_owner(SpaceId, UserId) ->
    ?assertMatch(ok, ozt:rpc(space_logic, add_owner, [?ROOT, SpaceId, UserId])).


-spec remove_owner(od_space:id(), od_user:id()) -> ok.
remove_owner(SpaceId, UserId) ->
    ?assertMatch(ok, ozt:rpc(space_logic, remove_owner, [?ROOT, SpaceId, UserId])).


-spec add_user(od_space:id(), od_user:id()) -> ok.
add_user(SpaceId, UserId) ->
    add_user(SpaceId, UserId, privileges:space_member()).

-spec add_user(od_space:id(), od_user:id(), [privileges:space_privilege()]) -> ok.
add_user(SpaceId, UserId, Privileges) ->
    ?assertMatch({ok, _}, ozt:rpc(space_logic, add_user, [?ROOT, SpaceId, UserId, Privileges])),
    ok.


-spec remove_user(od_space:id(), od_user:id()) -> ok.
remove_user(SpaceId, UserId) ->
    ?assertMatch(ok, ozt:rpc(space_logic, remove_user, [?ROOT, SpaceId, UserId])).


-spec add_group(od_space:id(), od_group:id()) -> ok.
add_group(SpaceId, GroupId) ->
    add_group(SpaceId, GroupId, privileges:space_member()).

-spec add_group(od_space:id(), od_group:id(), [privileges:space_privilege()]) -> ok.
add_group(SpaceId, GroupId, Privileges) ->
    ?assertMatch({ok, _}, ozt:rpc(space_logic, add_group, [?ROOT, SpaceId, GroupId, Privileges])),
    ok.


-spec remove_group(od_space:id(), od_group:id()) -> ok.
remove_group(SpaceId, GroupId) ->
    ?assertMatch(ok, ozt:rpc(space_logic, remove_group, [?ROOT, SpaceId, GroupId])).


-spec create_user_invite_token(od_space:id(), od_user:id()) -> tokens:token().
create_user_invite_token(SpaceId, UserId) ->
    ozt_tokens:create(temporary, ?SUB(user, UserId), ?INVITE_TOKEN(?USER_JOIN_SPACE, SpaceId)).


-spec create_group_invite_token(od_space:id(), od_user:id()) -> tokens:token().
create_group_invite_token(SpaceId, UserId) ->
    ozt_tokens:create(temporary, ?SUB(user, UserId), ?INVITE_TOKEN(?GROUP_JOIN_SPACE, SpaceId)).


-spec create_support_token(od_space:id(), od_user:id()) -> tokens:token().
create_support_token(SpaceId, UserId) ->
    create_support_token(SpaceId, UserId, support_parameters:build(global, eager)).

-spec create_support_token(od_space:id(), od_user:id(), support_parameters:parameters()) -> tokens:token().
create_support_token(SpaceId, UserId, SupportParameters) ->
    ozt_tokens:create(temporary, ?SUB(user, UserId), ?INVITE_TOKEN(?SUPPORT_SPACE, SpaceId, SupportParameters)).


-spec create_share(od_space:id(), od_share:name()) -> od_share:id().
create_share(SpaceId, Name) ->
    {ok, ShareId} = ?assertMatch({ok, _}, ozt:rpc(share_logic, create, [
        ?ROOT, str_utils:rand_hex(16), Name, ?ROOT_FILE_ID, SpaceId
    ])),
    ShareId.


-spec get_user_privileges(od_space:id(), od_user:id()) -> [privileges:space_privilege()].
get_user_privileges(SpaceId, UserId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(space_logic, get_user_privileges, [?ROOT, SpaceId, UserId])),
    Privs.


-spec get_group_privileges(od_space:id(), od_group:id()) -> [privileges:space_privilege()].
get_group_privileges(SpaceId, GroupId) ->
    {ok, Privs} = ?assertMatch({ok, _}, ozt:rpc(space_logic, get_group_privileges, [?ROOT, SpaceId, GroupId])),
    Privs.


-spec set_user_privileges(od_space:id(), od_user:id(), [privileges:space_privilege()]) -> ok.
set_user_privileges(SpaceId, UserId, Privileges) ->
    ?assertMatch(ok, ozt:rpc(space_logic, update_user_privileges, [?ROOT, SpaceId, UserId, #{
        <<"grant">> => Privileges,
        <<"revoke">> => lists_utils:subtract(privileges:space_admin(), Privileges)
    }])).


-spec delete(od_space:id()) -> ok.
delete(SpaceId) ->
    ?assertMatch(ok, ozt:rpc(space_logic, delete, [?ROOT, SpaceId])).


-spec get_support_parameters_registry(od_space:id()) -> support_parameters:registry().
get_support_parameters_registry(SpaceId) ->
    Space = get(SpaceId),
    Space#od_space.support_parameters_registry.


-spec get_support_stage_registry(od_space:id()) -> support_stage:registry().
get_support_stage_registry(SpaceId) ->
    Space = get(SpaceId),
    Space#od_space.support_stage_registry.


-spec get_capacity_usage_registry(od_space:id()) -> provider_capacity_usage:registry().
get_capacity_usage_registry(SpaceId) ->
    {ok, SpaceStats} = ozt:rpc(space_logic, get_stats, [?ROOT, SpaceId]),
    SpaceStats#space_stats.capacity_usage_registry.


%%--------------------------------------------------------------------
%% @doc
%% Extracts only the information about seen seqs and timestamps from the space's
%% provider sync progress registry, returns a map of maps with the values as
%% tuples containing the seen seq and timestamp.
%% @end
%%--------------------------------------------------------------------
-spec extract_sync_progress_registry_matrix(od_space:id()) ->
    #{od_provider:id() => #{od_provider:id() => {provider_sync_progress:seen_seq(), provider_sync_progress:seq_timestamp()}}}.
extract_sync_progress_registry_matrix(SpaceId) ->
    {ok, SpaceStats} = ozt:rpc(space_logic, get_stats, [?ROOT, SpaceId]),
    Registry = SpaceStats#space_stats.sync_progress_registry#sync_progress_registry.per_provider,
    maps:map(fun(_ProviderId, #provider_sync_progress{per_peer = PerPeer}) ->
        maps:map(fun(_PeerId, #sync_progress_with_peer{seen_seq = SeenSeq, seq_timestamp = SeqTimestamp}) ->
            {SeenSeq, SeqTimestamp}
        end, PerPeer)
    end, Registry).


-spec minimum_support_size() -> od_space:support_size().
minimum_support_size() ->
    ozt:get_env(minimum_space_support_size).


%% @doc Support stages that should be set for legacy supports
-spec has_default_support_parameters(od_space:id(), [od_provider:id()]) -> boolean().
has_default_support_parameters(SpaceId, Providers) ->
    ExpectedRegistry =  maps:from_list(lists:map(fun(ProviderId) ->
        {ProviderId, support_parameters:build(global, eager)}
    end, Providers)),
    ExpectedRegistry =:= get_support_parameters_registry(SpaceId).


%% @doc Support stages that should be set for legacy supports
-spec has_legacy_support_stages(od_space:id(), #{od_provider:id() => [od_storage:id()]}) -> boolean().
has_legacy_support_stages(SpaceId, StoragesPerProvider) ->
    ExpectedRegistry = maps:map(fun(_ProviderId, Storages) ->
        #support_stage_details{provider_stage = legacy, per_storage = maps:from_list(lists:map(fun(StorageId) ->
            {StorageId, legacy}
        end, Storages))}
    end, StoragesPerProvider),
    ExpectedRegistry =:= get_support_stage_registry(SpaceId).


%% @doc Initial capacity usage that should be set for new supports or legacy supports after Onezone upgrade.
-spec has_initial_capacity_usage(od_space:id(), od_space:support_size(), #{od_provider:id() => [od_storage:id()]}) ->
    boolean().
has_initial_capacity_usage(SpaceId, SupportSize, StoragesPerProvider) ->
    ExpectedRegistry = maps:map(fun(_ProviderId, Storages) ->
        #provider_capacity_usage{overfull = false, per_storage = maps:from_list(lists:map(fun(StorageId) ->
            {StorageId, #storage_capacity_usage{
                overfull = false, used = ?UNKNOWN_USAGE_VALUE, granted = SupportSize
            }}
        end, Storages))}
    end, StoragesPerProvider),
    ExpectedRegistry =:= get_capacity_usage_registry(SpaceId).


%% @doc Initial sync progress that should be set for new supports or legacy supports after Onezone upgrade.
-spec has_initial_sync_progress(od_space:id(), time:seconds(), [od_provider:id()]) -> boolean().
has_initial_sync_progress(SpaceId, SpaceCreationTime, Providers) ->
    PerPeer = maps:from_list(lists:map(fun(ProviderId) ->
        {ProviderId, {1, SpaceCreationTime}}
    end, Providers)),
    ExpectedRegistry = maps:from_list(lists:map(fun(ProviderId) ->
        {ProviderId, PerPeer}
    end, Providers)),
    ExpectedRegistry =:= extract_sync_progress_registry_matrix(SpaceId).
