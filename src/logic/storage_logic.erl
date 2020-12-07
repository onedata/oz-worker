%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all storage logic functionality.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(storage_logic).
-author("Michal Stanisz").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").

-export([
    create/2, create/3
]).
-export([
    get/2,
    get_shared_data/3
]).
-export([
    update/3
]).
-export([
    delete/2
]).
-export([
    support_space/4, support_space/3,
    init_support/4, init_support/3,
    init_unsupport/3,
    complete_unsupport_resize/3,
    complete_unsupport_purge/3,
    finalize_unsupport/3,
    update_support_size/4,
    revoke_support/3,
    get_spaces/2,
    upgrade_support_to_20_02/3,
    upgrade_support_to_21_02/3
]).
-export([
    exists/1,
    has_eff_user/2,
    has_eff_group/2,
    belongs_to_provider/2,
    supports_space/2
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new storage document in database. Has two variants:
%% 1) Storage Name is given explicitly
%% 2) Storage name is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create(aai:auth(), NameOrData :: binary() | map()) ->
    {ok, od_storage:id()} | errors:error().
create(Auth, NameOrData) ->
    create(Auth, datastore_key:new(), NameOrData).

-spec create(aai:auth(), Id :: od_storage:id(), NameOrData :: binary() | map()) ->
    {ok, od_storage:id()} | errors:error().
create(Auth, Id, Name) when is_binary(Name) ->
    create(Auth, Id, #{<<"name">> => Name, <<"imported">> => false});
create(Auth, Id, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_storage, id = Id, aspect = instance},
        data = Data
    })).


%% @TODO VFS-6977 deprecated - to be removed in the future, currently an alias for init_support
-spec support_space(aai:auth(), od_storage:id(), tokens:token(), od_space:support_size()) ->
    {ok, od_space:id()} | errors:error().
support_space(Auth, StorageId, Token, SupportSize) ->
    init_support(Auth, StorageId, Token, SupportSize).


%% @TODO VFS-6977 deprecated - to be removed in the future, currently an alias for init_support
-spec support_space(aai:auth(), od_storage:id(), Data :: map()) ->
    {ok, od_space:id()} | errors:error().
support_space(Auth, StorageId, Data) ->
    init_support(Auth, StorageId, Data).


-spec init_support(aai:auth(), od_storage:id(), tokens:token(), od_space:support_size()) ->
    {ok, od_space:id()} | errors:error().
init_support(Auth, StorageId, Token, SupportSize) ->
    init_support(Auth, StorageId, #{
        <<"token">> => Token, <<"size">> => SupportSize
    }).


-spec init_support(aai:auth(), od_storage:id(), Data :: map()) ->
    {ok, od_space:id()} | errors:error().
init_support(Auth, StorageId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = init_support},
        data = Data
    })).


-spec init_unsupport(aai:auth(), od_storage:id(), od_space:id()) -> ok | errors:error().
init_unsupport(Auth, StorageId, SpaceId) ->
    ?CREATE_RETURN_OK(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = {init_unsupport, SpaceId}}
    })).


-spec complete_unsupport_resize(aai:auth(), od_storage:id(), od_space:id()) -> ok | errors:error().
complete_unsupport_resize(Auth, StorageId, SpaceId) ->
    ?CREATE_RETURN_OK(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = {complete_unsupport_resize, SpaceId}}
    })).


-spec complete_unsupport_purge(aai:auth(), od_storage:id(), od_space:id()) -> ok | errors:error().
complete_unsupport_purge(Auth, StorageId, SpaceId) ->
    ?CREATE_RETURN_OK(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = {complete_unsupport_purge, SpaceId}}
    })).


-spec finalize_unsupport(aai:auth(), od_storage:id(), od_space:id()) -> ok | errors:error().
finalize_unsupport(Auth, StorageId, SpaceId) ->
    ?CREATE_RETURN_OK(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = {finalize_unsupport, SpaceId}}
    })).


%%--------------------------------------------------------------------
%% @doc
%% Updates support size for specified space of given storage. Has two variants:
%% 1) New support size is given explicitly
%% 2) New support size is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update_support_size(aai:auth(), od_storage:id(),
    od_space:id(), SupSizeOrData :: integer() | map()) -> ok | errors:error().
update_support_size(Auth, StorageId, SpaceId, SupSize) when is_integer(SupSize) ->
    update_support_size(Auth, StorageId, SpaceId, #{
        <<"size">> => SupSize
    });
update_support_size(Auth, StorageId, SpaceId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = {space, SpaceId}},
        data = Data
    }).


%% @TODO VFS-6977 deprecated - to be removed in the future - forcefully removes the support
-spec revoke_support(aai:auth(), od_storage:id(), od_space:id()) ->
    ok | errors:error().
revoke_support(Auth, StorageId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = {space, SpaceId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a storage record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(aai:auth(), od_storage:id()) -> {ok, #od_storage{}} | errors:error().
get(Auth, StorageId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves storage data shared through space from database.
%% @end
%%--------------------------------------------------------------------
-spec get_shared_data(aai:auth(), od_storage:id(), od_space:id()) ->
    {ok, map()} | errors:error().
get_shared_data(Auth, StorageId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = instance, scope = shared},
        auth_hint = ?THROUGH_SPACE(SpaceId)
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given storage.
%% @end
%%--------------------------------------------------------------------
-spec update(aai:auth(), od_storage:id(), Data :: map()) -> ok | errors:error().
update(Auth, StorageId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = instance},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given storage from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(aai:auth(), od_storage:id()) -> ok | errors:error().
delete(Auth, StorageId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Returns list of spaces supported by given storage.
%% @end
%%--------------------------------------------------------------------
-spec get_spaces(aai:auth(), od_storage:id()) ->
    {ok, [od_space:id()]} | errors:error().
get_spaces(Auth, StorageId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = spaces}
    }).


-spec upgrade_support_to_20_02(aai:auth(), od_storage:id(), od_space:id()) ->
    {ok, od_space:id()} | errors:error().
upgrade_support_to_20_02(Auth, StorageId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = {upgrade_legacy_support, SpaceId}}
    }).


-spec upgrade_support_to_21_02(aai:auth(), od_storage:id(), od_space:id()) ->
    {ok, od_space:id()} | errors:error().
upgrade_support_to_21_02(Auth, StorageId, SpaceId) ->
    entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = {upgrade_support_to_21_02, SpaceId}}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a storage exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(od_storage:id()) -> boolean().
exists(StorageId) ->
    {ok, Exists} = od_storage:exists(StorageId),
    Exists.


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified user is an effective user
%% of spaces supported by the storage.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_user(od_storage:id() | #od_storage{}, od_user:id()) -> boolean().
has_eff_user(StorageId, UserId) when is_binary(StorageId) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, od_storage, StorageId);
has_eff_user(Storage, UserId) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Storage).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified group is an effective group of given storage.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_group(od_storage:id() | #od_storage{}, od_group:id()) -> boolean().
has_eff_group(StorageId, GroupId) when is_binary(StorageId) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, od_storage, StorageId);
has_eff_group(Storage, GroupId) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, Storage).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified provider is parent of given storage.
%% @end
%%--------------------------------------------------------------------
-spec belongs_to_provider(od_storage:id() | #od_storage{}, od_provider:id()) -> boolean().
belongs_to_provider(StorageId, ProviderId) when is_binary(StorageId) ->
    entity_graph:has_relation(direct, top_down, od_provider, ProviderId, od_storage, StorageId);
belongs_to_provider(Storage, ProviderId) ->
    entity_graph:has_relation(direct, top_down, od_provider, ProviderId, Storage).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified space is supported by given storage.
%% @end
%%--------------------------------------------------------------------
-spec supports_space(od_storage:id() | #od_storage{}, od_space:id()) -> boolean().
supports_space(StorageId, SpaceId) when is_binary(StorageId) ->
    entity_graph:has_relation(direct, bottom_up, od_space, SpaceId, od_storage, StorageId);
supports_space(Storage, SpaceId) ->
    entity_graph:has_relation(direct, bottom_up, od_space, SpaceId, Storage).
