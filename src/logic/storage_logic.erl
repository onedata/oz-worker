%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all storage logic functionalities.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(storage_logic).
-author("Michal Stanisz").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

-export([
    create/2, create/3
]).
-export([
    get/2,
    get_protected_data/2
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
    has_provider/2,
    supports_space/2
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new space document in database. Has two variants:
%% 1) Storage Name is given explicitly
%% 2) Storage name is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create(Auth :: aai:auth(), NameOrData :: binary() | #{}) ->
    {ok, od_storage:id()} | {error, term()}.
create(Auth, NameOrData) ->
    create(Auth, datastore_utils:gen_key(), NameOrData).

-spec create(Auth :: aai:auth(), Id :: od_storage:id(), NameOrData :: binary() | #{}) ->
    {ok, od_storage:id()} | {error, term()}.
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
-spec support_space(Auth :: aai:auth(), StorageId :: od_provider:id(),
    Token :: tokens:token() | macaroon:macaroon(), SupportSize :: integer()) ->
    {ok, od_space:id()} | {error, term()}.
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
-spec support_space(Auth :: aai:auth(), StorageId :: od_provider:id(),
    Data :: #{}) -> {ok, od_space:id()} | {error, term()}.
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
-spec update_support_size(Auth :: aai:auth(), ProviderId :: od_provider:id(),
    SpaceId :: od_space:id(), SupSizeOrData :: integer() | #{}) -> ok | {error, term()}.
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
-spec revoke_support(Auth :: aai:auth(), StorageId :: od_storage:id(),
    SpaceId :: od_space:id()) -> ok | {error, term()}.
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
-spec get(Auth :: aai:auth(), StorageId :: od_storage:id()) ->
    {ok, #od_storage{}} | {error, term()}.
get(Auth, StorageId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves protected storage data from database.
%% @end
%%--------------------------------------------------------------------
-spec get_protected_data(Auth :: aai:auth(), StorageId :: od_storage:id()) ->
    {ok, map()} | {error, term()}.
get_protected_data(Auth, StorageId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_storage, id = StorageId, aspect = instance, scope = protected}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given storage.
%% @end
%%--------------------------------------------------------------------
-spec update(Auth :: aai:auth(), StorageId :: od_storage:id(),
    Data :: #{}) -> ok | {error, term()}.
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
-spec delete(Auth :: aai:auth(), StorageId :: od_storage:id()) ->
    ok | {error, term()}.
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
-spec get_spaces(Auth :: aai:auth(), StorageId :: od_storage:id()) ->
    {ok, [od_space:id()]} | {error, term()}.
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
-spec exists(StorageId :: od_storage:id()) -> boolean().
exists(StorageId) ->
    {ok, Exists} = od_storage:exists(StorageId),
    Exists.


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified user is an effective user of given storage.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_user(StorageOrId :: od_storage:id() | #od_storage{},
    UserId :: od_user:id()) -> boolean().
has_eff_user(StorageId, UserId) when is_binary(StorageId) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, od_storage, StorageId);
has_eff_user(Storage, UserId) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Storage).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified group is an effective group of given storage.
%% @end
%%--------------------------------------------------------------------
-spec has_eff_group(StorageOrId :: od_storage:id() | #od_storage{},
    GroupId :: od_group:id()) -> boolean().
has_eff_group(StorageId, GroupId) when is_binary(StorageId) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, od_storage, StorageId);
has_eff_group(Storage, GroupId) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, Storage).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified provider is parent of given storage.
%% @end
%%--------------------------------------------------------------------
-spec has_provider(StorageOrId :: od_storage:id() | #od_storage{},
    ProviderId :: od_provider:id()) -> boolean().
has_provider(StorageId, ProviderId) when is_binary(StorageId) ->
    entity_graph:has_relation(direct, top_down, od_provider, ProviderId, od_storage, StorageId);
has_provider(Storage, ProviderId) ->
    entity_graph:has_relation(direct, top_down, od_provider, ProviderId, Storage).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying whether specified space is supported by given storage.
%% @end
%%--------------------------------------------------------------------
-spec supports_space(StorageOrId :: od_storage:id() | #od_storage{},
    SpaceId :: od_space:id()) -> boolean().
supports_space(StorageId, SpaceId) when is_binary(StorageId) ->
    entity_graph:has_relation(direct, bottom_up, od_space, SpaceId, od_storage, StorageId);
supports_space(Storage, SpaceId) ->
    entity_graph:has_relation(direct, bottom_up, od_space, SpaceId, Storage).
