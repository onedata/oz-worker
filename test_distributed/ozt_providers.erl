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

-define(PROVIDER_DATA(UserId, RegistrationToken), #{
    <<"name">> => <<"of-user-", UserId/binary>>,
    <<"adminEmail">> => <<"admin@oneprovider.example.com">>,
    <<"domain">> => <<"oneprovider.example.com">>,
    <<"subdomainDelegation">> => false,
    <<"latitude">> => 0.0,
    <<"longitude">> => 0.0,
    <<"token">> => RegistrationToken
}).

%% API
-export([create/0]).
-export([create_for_admin_user/1, create_as_support_for_user/1]).
-export([create_as_support_for_space/1]).
-export([get_root_token/1]).
-export([ensure_storage/2]).
-export([support_space/2, support_space_using_token/4]).
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
    RegToken = ozt_tokens:create(temporary, ?SUB(user, UserId), ?INVITE_TOKEN(?REGISTER_ONEPROVIDER, UserId)),
    {ok, {ProviderId, _}} = ?assertMatch({ok, _}, ozt:rpc(provider_logic, create, [
        ?NOBODY, ?PROVIDER_DATA(UserId, RegToken)
    ])),
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


-spec get_root_token(od_provider:id()) -> tokens:token().
get_root_token(ProviderId) ->
    {ok, #{<<"token">> := Token}} = ?assertMatch({ok, _}, ozt:rpc(token_logic, get_provider_named_token_by_name, [
        ?ROOT, ProviderId, ?PROVIDER_ROOT_TOKEN_NAME
    ])),
    Token.


-spec ensure_storage(od_provider:id(), od_storage:id()) -> ok.
ensure_storage(ProviderId, StorageId) ->
    ozt:rpc(storage_logic, create, [?PROVIDER(ProviderId), StorageId, <<"storage-of-", ProviderId/binary>>]),
    ok.


-spec support_space(od_provider:id(), od_space:id()) -> ok.
support_space(ProviderId, SpaceId) ->
    StorageId = ProviderId, % Use a storage with the same id as provider for easier test code
    ensure_storage(ProviderId, StorageId),
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


-spec support_space_using_token(od_provider:id(), od_storage:id(), tokens:token(), od_space:support_size()) ->
    ok.
support_space_using_token(ProviderId, StorageId, Token, Size) ->
    ?assertMatch({ok, _}, ozt:rpc(storage_logic, support_space, [
        ?PROVIDER(ProviderId), StorageId, Token, Size
    ])),
    ok.


-spec delete(od_provider:id()) -> ok.
delete(ProviderId) ->
    ?assertMatch(ok, ozt:rpc(provider_logic, delete, [?ROOT, ProviderId])).
