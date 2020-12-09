%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for manipulating providers of oz-worker service in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_providers).
-author("Lukasz Opiola").

-include("ozt.hrl").

-define(PROVIDER_DATA(Name, RegistrationToken), #{
    <<"name">> => Name,
    <<"adminEmail">> => <<"admin@oneprovider.example.com">>,
    <<"domain">> => <<"oneprovider.example.com">>,
    <<"subdomainDelegation">> => false,
    <<"latitude">> => 0.0,
    <<"longitude">> => 0.0,
    <<"token">> => RegistrationToken
}).

%% API
-export([create/0]).
-export([create_for_admin_user/1, create_for_admin_user/2]).
-export([create_as_support_for_user/1]).
-export([create_as_support_for_space/1]).
-export([simulate_version/2]).
-export([set_up_support_for_user/2]).
-export([get_root_token/1]).
-export([create_storage/1, create_storage/3, ensure_storage/2]).
-export([support_space/2, support_space/3, support_space/4]).
-export([support_space_using_token/3, support_space_using_token/4]).
-export([simulate_preexisting_19_02_space_support/2]).
-export([delete/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec create() -> od_provider:id().
create() ->
    ProviderAdmin = ozt_users:create_admin([?OZ_PROVIDERS_INVITE]),
    create_for_admin_user(ProviderAdmin).


-spec create_for_admin_user(od_user:id()) -> od_provider:id().
create_for_admin_user(UserId) ->
    create_for_admin_user(UserId, <<"of-user-", UserId/binary>>).


-spec create_for_admin_user(od_user:id(), od_provider:name()) -> od_provider:id().
create_for_admin_user(UserId, Name) ->
    RegToken = ozt_tokens:create(temporary, ?SUB(user, UserId), ?INVITE_TOKEN(?REGISTER_ONEPROVIDER, UserId)),
    {ok, {ProviderId, _}} = ?assertMatch({ok, _}, ozt:rpc(provider_logic, create, [
        ?NOBODY, ?PROVIDER_DATA(Name, RegToken)
    ])),
    % make the Onezone perceive the Oneprovider as up-to-date and conforming to new space support mechanisms
    ozt:rpc(cluster_logic, update_version_info, [
        ?PROVIDER(ProviderId), ProviderId, ?WORKER, {<<"21.02.1">>, <<"build">>, ?EMPTY_GUI_HASH}
    ]),
    ProviderId.


-spec create_as_support_for_user(od_user:id()) -> od_provider:id().
create_as_support_for_user(UserId) ->
    ProviderId = create(),
    SpaceId = ozt_users:create_space_for(UserId),
    support_space(ProviderId, SpaceId),
    ozt:reconcile_entity_graph(),
    ProviderId.


-spec create_as_support_for_space(od_space:id()) -> od_provider:id().
create_as_support_for_space(SpaceId) ->
    ProviderId = create(),
    support_space(ProviderId, SpaceId),
    ozt:reconcile_entity_graph(),
    ProviderId.


-spec simulate_version(od_provider:id(), onedata:release_version()) -> ok.
simulate_version(ProviderId, ReleaseVersion) ->
    % this should finish with an error (bad GUI hash was given), but nevertheless set the release version
    ?assertMatch(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"workerVersion.gui">>), ozt:rpc(cluster_logic, update_version_info, [
        ?PROVIDER(ProviderId), ProviderId, ?WORKER,
        {ReleaseVersion, ?DEFAULT_BUILD_VERSION, ?EMPTY_GUI_HASH}
    ])),
    ok.


-spec set_up_support_for_user(od_provider:id(), od_user:id()) -> ok.
set_up_support_for_user(ProviderId, UserId) ->
    SpaceId = ozt_users:create_space_for(UserId),
    support_space(ProviderId, SpaceId).


-spec get_root_token(od_provider:id()) -> tokens:token().
get_root_token(ProviderId) ->
    {ok, #{<<"token">> := Token}} = ?assertMatch({ok, _}, ozt:rpc(token_logic, get_provider_named_token_by_name, [
        ?ROOT, ProviderId, ?PROVIDER_ROOT_TOKEN_NAME
    ])),
    Token.


-spec create_storage(od_provider:id()) -> od_storage:id().
create_storage(ProviderId) ->
    create_storage(ProviderId, <<"of-provider-", ProviderId/binary>>, #{}).


-spec create_storage(od_provider:id(), od_storage:name(), od_storage:qos_parameters()) -> od_storage:id().
create_storage(ProviderId, Name, QosParameters) ->
    {ok, StorageId} = ?assertMatch({ok, _}, ozt:rpc(storage_logic, create, [
        ?PROVIDER(ProviderId), datastore_key:new(), #{<<"name">> => Name, <<"qosParameters">> => QosParameters}
    ])),
    StorageId.


-spec ensure_storage(od_provider:id(), od_storage:id()) -> ok.
ensure_storage(ProviderId, StorageId) ->
    ?assertSuccessOrAlreadyExists(ozt:rpc(storage_logic, create, [
        ?PROVIDER(ProviderId), StorageId, <<"storage-of-", ProviderId/binary>>
    ])).


-spec support_space(od_provider:id(), od_space:id()) -> ok.
support_space(ProviderId, SpaceId) ->
    StorageId = datastore_key:new_from_digest([ProviderId]),
    ensure_storage(ProviderId, StorageId),
    support_space(ProviderId, StorageId, SpaceId).

-spec support_space(od_provider:id(), od_space:id(), od_storage:id()) -> ok.
support_space(ProviderId, StorageId, SpaceId) ->
    support_space(ProviderId, StorageId, SpaceId, ozt_spaces:minimum_support_size()).

-spec support_space(od_provider:id(), od_storage:id(), od_space:id(), od_space:support_size()) ->
    ok.
support_space(ProviderId, StorageId, SpaceId, Size) ->
    % Create a temporary user for inviting a provider, as invite tokens cannot
    % be created as root.
    TmpUser = ozt_users:create(),
    ozt_spaces:add_user(SpaceId, TmpUser, [?SPACE_ADD_SUPPORT]),
    Token = ozt_spaces:create_support_token(SpaceId, TmpUser),
    support_space_using_token(ProviderId, StorageId, Token, Size),
    ozt_users:delete(TmpUser).


-spec support_space_using_token(od_provider:id(), od_storage:id(), tokens:token()) ->
    ok.
support_space_using_token(ProviderId, StorageId, Token) ->
    support_space_using_token(ProviderId, StorageId, Token, ozt_spaces:minimum_support_size()).

-spec support_space_using_token(od_provider:id(), od_storage:id(), tokens:token(), od_space:support_size()) ->
    ok.
support_space_using_token(ProviderId, StorageId, Token, Size) ->
    ?assertMatch({ok, _}, ozt:rpc(storage_logic, support_space, [
        ?PROVIDER(ProviderId), StorageId, Token, Size
    ])),
    ok.


-spec simulate_preexisting_19_02_space_support(od_provider:id(), od_space:id()) -> ok.
simulate_preexisting_19_02_space_support(ProviderId, SpaceId) ->
    % Providers in 19.02 did not have storages in Onezone, but a virtual storage
    % the same id as the provider was created for them upon Onezone's upgrade to
    % 20.02 and all the supports were transferred there. After Onezone's upgrade
    % to 21.02
    VirtualStorageId = ProviderId,
    ensure_storage(ProviderId, VirtualStorageId),
    support_space(ProviderId, VirtualStorageId, SpaceId).


-spec delete(od_provider:id()) -> ok.
delete(ProviderId) ->
    ?assertMatch(ok, ozt:rpc(provider_logic, delete, [?ROOT, ProviderId])).
