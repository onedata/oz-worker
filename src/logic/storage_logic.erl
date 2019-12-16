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
    update_support_size/4,
    revoke_support/3,
    get_spaces/2
]).
-export([
    exists/1,
    has_eff_user/2,
    has_eff_group/2,
    belongs_to_provider/2,
    supports_space/2
]).
-export([
    migrate_legacy_supports/0
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
    create(Auth, datastore_utils:gen_key(), NameOrData).

-spec create(aai:auth(), Id :: od_storage:id(), NameOrData :: binary() | map()) ->
    {ok, od_storage:id()} | errors:error().
create(Auth, Id, Name) when is_binary(Name) ->
    create(Auth, Id, #{<<"name">> => Name});
create(Auth, Id, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_storage, id = Id, aspect = instance},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Supports a space based on support_space_token and support size.
%% @end
%%--------------------------------------------------------------------
-spec support_space(aai:auth(), od_storage:id(),
    tokens:token() | macaroon:macaroon(), SupportSize :: integer()) ->
    {ok, od_space:id()} | errors:error().
support_space(Auth, StorageId, Token, SupportSize) ->
    support_space(Auth, StorageId, #{
        <<"token">> => Token, <<"size">> => SupportSize
    }).


%%--------------------------------------------------------------------
%% @doc
%% Supports a space. Token (support_space_token) and SupportSize
%% are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec support_space(aai:auth(), od_storage:id(), Data :: map()) ->
    {ok, od_space:id()} | errors:error().
support_space(Auth, StorageId, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = support},
        data = Data
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


%%--------------------------------------------------------------------
%% @doc
%% Revokes support for specified space on behalf of given storage.
%% @end
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% @doc
%% Migrates legacy space supports to new model with storages.
%% This is done by creating virtual storage record for each provider
%% and supporting space with this storage.
%% This storage record does not represent actual storage, it is provider
%% that keeps knowledge of storages as long as it is in previous version.
%% Provider removes this virtual storage record during its upgrade procedure.
%%
%% Dedicated for upgrading Onezone from 19.02.* to 19.09.*.
%% @end
%%--------------------------------------------------------------------
-spec migrate_legacy_supports() -> ok.
migrate_legacy_supports() ->
    entity_graph:init_state(),
    ?info("Creating virtual storages for providers..."),
    {ok, Providers} = od_provider:list(),
    lists:foreach(fun(#document{key = ProviderId} = Provider) ->
        case provider_logic:has_storage(ProviderId, ProviderId) of
            true -> ok;
            false ->
                create(?PROVIDER(ProviderId), ProviderId, ?STORAGE_DEFAULT_NAME)
        end,
        {ok, Spaces} = provider_logic:get_legacy_spaces(Provider),
        ?info("  Migrating space supports for provider: ~p", [ProviderId]),
        maps:map(fun(SpaceId, SupportSize) ->
            try
                entity_graph:add_relation(od_space, SpaceId, od_storage, ProviderId, SupportSize)
            catch
                _:(?ERROR_RELATION_ALREADY_EXISTS(_,_,_,_)) -> ok
            end,
            {ok, _} = provider_logic:remove_legacy_space(ProviderId, SpaceId)
        end, Spaces),
        ?notice(" Successfully migrated space supports for provider: ~p", [ProviderId])
    end, Providers),
    ?notice("Successfully migrated legacy supports").
