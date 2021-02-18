%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_storage model.
%%% @end
%%%-------------------------------------------------------------------
-module(storage_logic_plugin).
-author("Michal Stanisz").
-behaviour(entity_logic_plugin_behaviour).

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/errors.hrl").

-define(MINIMUM_SUPPORT_SIZE, oz_worker:get_env(minimum_space_support_size, 1000000)).

-export([fetch_entity/1, operation_supported/3, is_subscribable/2]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, required_admin_privileges/1, validate/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity and its revision from datastore, if applicable.
%% Should return:
%%  * {true, entity_logic:versioned_entity()}
%%      if the fetch was successful
%%  * false
%%      if fetch is not applicable for this operation
%%  * {error, _}
%%      if there was an error, such as ?ERROR_NOT_FOUND
%% @end
%%--------------------------------------------------------------------
-spec fetch_entity(gri:gri()) ->
    {true, entity_logic:versioned_entity()} | false | errors:error().
fetch_entity(#gri{id = StorageId}) ->
    case od_storage:get(StorageId) of
        {ok, #document{value = Storage, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_rev:parse(DbRev),
            {true, {Storage, Revision}};
        _ ->
            ?ERROR_NOT_FOUND
    end.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given operation is supported based on operation, aspect and
%% scope (entity type is known based on the plugin itself).
%% @end
%%--------------------------------------------------------------------
-spec operation_supported(entity_logic:operation(), entity_logic:aspect(),
    entity_logic:scope()) -> boolean().
operation_supported(create, instance, private) -> true;
operation_supported(create, support, private) -> true;
operation_supported(create, {upgrade_legacy_support, _}, private) -> true;
operation_supported(create, {upgrade_support_to_22_02, _}, private) -> true;

operation_supported(create, init_support, private) -> true;

operation_supported(create, {init_unsupport, _}, private) -> true;
operation_supported(create, {complete_unsupport_resize, _}, private) -> true;
operation_supported(create, {complete_unsupport_purge, _}, private) -> true;
operation_supported(create, {finalize_unsupport, _}, private) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, instance, shared) -> true;

operation_supported(get, spaces, private) -> true;

operation_supported(update, instance, private) -> true;
operation_supported(update, {space, _}, private) -> true;

operation_supported(delete, instance, private) -> true;
operation_supported(delete, {space, _}, private) -> true;

operation_supported(_, _, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given {Aspect, Scope} pair is subscribable, i.e. clients can
%% subscribe to receive updates concerning the aspect of entity.
%% @end
%%--------------------------------------------------------------------
-spec is_subscribable(entity_logic:aspect(), entity_logic:scope()) ->
    boolean().
is_subscribable(instance, _) -> true;
is_subscribable(_, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(#el_req{gri = #gri{id = ProposedId, aspect = instance} = GRI, auth = ?PROVIDER(ProviderId) = Auth, data = Data}) ->
    Name = maps:get(<<"name">>, Data),
    QosParameters = maps:get(<<"qosParameters">>, Data, #{}),
    % Only legacy providers do not send imported value
    % set it to `unknown` so provider could change it during its upgrade procedure
    ImportedStorage = maps:get(<<"imported">>, Data, unknown),
    Readonly = maps:get(<<"readonly">>, Data, false),
    case Readonly andalso ImportedStorage =:= false of
        true -> throw(?ERROR_REQUIRES_IMPORTED_STORAGE(<<"'newly created storage'">>));
        _ -> ok
    end,
    StorageId = ensure_id(ProposedId),
    ExtendedQosParameters = add_implicit_qos_parameters(StorageId, ProviderId, QosParameters),
    StorageDoc = #document{
        key = StorageId,
        value = #od_storage{
            name = Name,
            qos_parameters = ExtendedQosParameters,
            imported = ImportedStorage,
            readonly = Readonly,
            provider = ProviderId,
            creator = aai:normalize_subject(Auth#auth.subject),
            creation_time = global_clock:timestamp_seconds()
        }
    },
    case od_storage:create(StorageDoc) of
        {error, already_exists} ->
            throw(?ERROR_ALREADY_EXISTS);
        {ok, #document{key = StorageId}} ->
            entity_graph:add_relation(
                od_storage, StorageId,
                od_provider, ProviderId
            ),
            {true, {Storage, Rev}} = fetch_entity(#gri{id = StorageId}),
            {ok, resource, {GRI#gri{id = StorageId}, {Storage, Rev}}}
    end;

% This endpoint is dedicated for providers upgrading from version 19.02.* to 20.02.*.
%% @TODO VFS-5856 remove deprecated space support functionalities
create(#el_req{gri = #gri{id = StorageId, aspect = {upgrade_legacy_support, SpaceId}}}) ->
    fun(#od_storage{provider = ProviderId}) ->
        space_support:upgrade_support_to_20_02(ProviderId, StorageId, SpaceId)
    end;

% This endpoint is dedicated for providers upgrading from version 21.02.* to 22.02.*.
create(#el_req{gri = #gri{id = StorageId, aspect = {upgrade_support_to_22_02, SpaceId}}}) ->
    fun(#od_storage{provider = ProviderId}) ->
        space_support:upgrade_support_to_22_02(ProviderId, StorageId, SpaceId)
    end;

% @TODO VFS-6977 The 'support' procedure is deprecated and used by legacy providers (<= 20.02).
% This is an alias for the modern procedure, which internally checks the version and
% properly handles supports of legacy providers.
create(Req = #el_req{gri = GRI = #gri{aspect = support}}) ->
    create(Req#el_req{gri = GRI#gri{aspect = init_support}});

create(#el_req{auth = Auth, gri = #gri{id = StorageId, aspect = init_support}, data = Data}) ->
    fun(#od_storage{provider = ProviderId}) ->
        SupportSize = maps:get(<<"size">>, Data),
        Token = maps:get(<<"token">>, Data),
        invite_tokens:consume(Auth, Token, ?SUPPORT_SPACE, fun(SpaceId, TokenParameters, _) ->
            space_support:init_support(ProviderId, StorageId, SpaceId, SupportSize, TokenParameters)
        end)
    end;

create(#el_req{gri = #gri{id = StorageId, aspect = {init_unsupport, SpaceId}}}) ->
    fun(#od_storage{provider = ProviderId}) ->
        space_support:init_unsupport(ProviderId, StorageId, SpaceId)
    end;

create(#el_req{gri = #gri{id = StorageId, aspect = {complete_unsupport_resize, SpaceId}}}) ->
    fun(#od_storage{provider = ProviderId}) ->
        space_support:complete_unsupport_resize(ProviderId, StorageId, SpaceId)
    end;

create(#el_req{gri = #gri{id = StorageId, aspect = {complete_unsupport_purge, SpaceId}}}) ->
    fun(#od_storage{provider = ProviderId}) ->
        space_support:complete_unsupport_purge(ProviderId, StorageId, SpaceId)
    end;

create(#el_req{gri = #gri{id = StorageId, aspect = {finalize_unsupport, SpaceId}}}) ->
    fun(#od_storage{provider = ProviderId}) ->
        space_support:finalize_unsupport(ProviderId, StorageId, SpaceId)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = instance, scope = private}}, Storage) ->
    {ok, Storage};
get(#el_req{gri = #gri{aspect = instance, scope = shared}}, Storage) ->
    #od_storage{
        provider = Provider,
        name = Name,
        qos_parameters = QosParameters,
        creation_time = CreationTime,
        creator = Creator,
        readonly = Readonly
    } = Storage,
    {ok, #{
        <<"provider">> => Provider,
        <<"name">> => Name, %% @TODO VFS-6520 Check if still needed after storage api is implemented
        <<"qosParameters">> => QosParameters,
        <<"creationTime">> => CreationTime,
        <<"creator">> => Creator,
        <<"readonly">> => Readonly
    }};

get(#el_req{gri = #gri{aspect = spaces}}, #od_storage{spaces = Spaces}) ->
    {ok, maps:keys(Spaces)}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = StorageId, aspect = instance}, data = Data}) ->
    % critical section to avoid race condition with space support
    space_support:lock_on_storage(StorageId, fun() ->
        ?extract_ok(od_storage:update(StorageId, fun(Storage) ->
            #od_storage{
                name = Name,
                provider = ProviderId,
                qos_parameters = QosParameters,
                imported = ImportedStorage,
                readonly = Readonly
            } = Storage,

            NewName = maps:get(<<"name">>, Data, Name),
            NewQosParameters = maps:get(<<"qosParameters">>, Data, QosParameters),
            NewImportedStorage = maps:get(<<"imported">>, Data, ImportedStorage),
            NewReadonly = maps:get(<<"readonly">>, Data, Readonly),
            try
                SupportsAnySpace = supports_any_space(Storage),
                check_imported_storage_value(ImportedStorage, NewImportedStorage, SupportsAnySpace),
                check_readonly_value(NewReadonly, NewImportedStorage, StorageId),
                ExtendedNewQosParameters = add_implicit_qos_parameters(StorageId, ProviderId, NewQosParameters),
                {ok, Storage#od_storage{
                    name = NewName,
                    qos_parameters = ExtendedNewQosParameters,
                    imported = NewImportedStorage,
                    readonly = NewReadonly
                }}
            catch
                throw:{error, Error} ->
                    {error, Error}
            end
        end))
    end);

update(Req = #el_req{gri = #gri{id = StorageId, aspect = {space, SpaceId}}}) ->
    NewSupportSize = maps:get(<<"size">>, Req#el_req.data),
    space_support:resize(StorageId, SpaceId, NewSupportSize).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = StorageId, aspect = instance}}) ->
    entity_graph:delete_with_relations(od_storage, StorageId);

delete(#el_req{gri = #gri{id = StorageId, aspect = {space, SpaceId}}}) ->
    fun(#od_storage{provider = ProviderId}) ->
        space_support:force_unsupport(ProviderId, StorageId, SpaceId),
        ok
    end.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(#el_req{gri = #gri{aspect = {space, SpaceId}}}, Storage) ->
    entity_graph:has_relation(direct, bottom_up, od_space, SpaceId, Storage);

% All other aspects exist if storage record exists.
exists(#el_req{gri = #gri{id = Id}}, #od_storage{}) ->
    Id =/= undefined.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(Req = #el_req{operation = create, gri = #gri{aspect = instance}}, _) ->
    case Req#el_req.auth of
        ?PROVIDER(_) -> true;
        _ -> false
    end;

authorize(#el_req{operation = create, auth = ?PROVIDER(PrId), gri = #gri{aspect = support}}, Storage) ->
    storage_logic:belongs_to_provider(Storage, PrId);

authorize(#el_req{operation = create, auth = ?PROVIDER(PrId), gri = #gri{aspect = init_support}}, Storage) ->
    storage_logic:belongs_to_provider(Storage, PrId);

authorize(#el_req{operation = create, auth = ?PROVIDER(PrId), gri = #gri{aspect = {init_unsupport, SpaceId}}}, Storage) ->
    auth_by_support(PrId, Storage, SpaceId);
authorize(#el_req{operation = create, auth = ?PROVIDER(PrId), gri = #gri{aspect = {complete_unsupport_resize, SpaceId}}}, Storage) ->
    auth_by_support(PrId, Storage, SpaceId);
authorize(#el_req{operation = create, auth = ?PROVIDER(PrId), gri = #gri{aspect = {complete_unsupport_purge, SpaceId}}}, Storage) ->
    auth_by_support(PrId, Storage, SpaceId);
authorize(#el_req{operation = create, auth = ?PROVIDER(PrId), gri = #gri{aspect = {finalize_unsupport, SpaceId}}}, Storage) ->
    auth_by_support(PrId, Storage, SpaceId);

authorize(#el_req{operation = create, auth = ?PROVIDER(PrId), gri = #gri{aspect = {upgrade_legacy_support, SpaceId}}}, Storage) ->
    % to ensure this operation is idempotent, check if the provider supports the space with any
    % storage (not necessarily the virtual storage)
    storage_logic:belongs_to_provider(Storage, PrId) andalso
        provider_logic:supports_space(PrId, SpaceId);
authorize(#el_req{operation = create, auth = ?PROVIDER(PrId), gri = #gri{aspect = {upgrade_support_to_22_02, SpaceId}}}, Storage) ->
    auth_by_support(PrId, Storage, SpaceId);

authorize(#el_req{operation = get, auth = ?PROVIDER(PrId), gri = #gri{aspect = instance, scope = private}}, Storage) ->
    storage_logic:belongs_to_provider(Storage, PrId);

authorize(Req = #el_req{operation = get, gri = GRI = #gri{aspect = instance, scope = shared}}, Storage) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?PROVIDER(PrId), ?THROUGH_SPACE(SpaceId)} ->
            storage_logic:supports_space(Storage, SpaceId) andalso
                provider_logic:supports_space(PrId, SpaceId);
        _ ->
            % Access to private data also allows access to shared data
            authorize(Req#el_req{gri = GRI#gri{scope = private}}, Storage)
    end;

authorize(#el_req{operation = get, auth = ?PROVIDER(PrId), gri = #gri{aspect = spaces}}, Storage) ->
    storage_logic:belongs_to_provider(Storage, PrId);

authorize(#el_req{operation = update, auth = ?PROVIDER(PrId), gri = #gri{aspect = instance}}, Storage) ->
    storage_logic:belongs_to_provider(Storage, PrId);

authorize(#el_req{operation = update, auth = ?PROVIDER(PrId), gri = #gri{aspect = {space, _}}}, Storage) ->
    storage_logic:belongs_to_provider(Storage, PrId);

authorize(#el_req{operation = delete, auth = ?PROVIDER(PrId), gri = #gri{aspect = instance}}, Storage) ->
    storage_logic:belongs_to_provider(Storage, PrId);

authorize(#el_req{operation = delete, auth = ?PROVIDER(PrId), gri = #gri{aspect = {space, _}}}, Storage) ->
    storage_logic:belongs_to_provider(Storage, PrId);

authorize(_, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc
%% Returns list of admin privileges needed to perform given operation.
%% @end
%%--------------------------------------------------------------------
-spec required_admin_privileges(entity_logic:req()) -> [privileges:oz_privilege()] | forbidden.
required_admin_privileges(_) ->
    forbidden.

%%--------------------------------------------------------------------
%% @doc
%% Returns validity verificators for given request.
%% Returns a map with 'required', 'optional' and 'at_least_one' keys.
%% Under each of them, there is a map:
%%      Key => {type_verificator, value_verificator}
%% Which means how value of given Key should be validated.
%% @end
%%--------------------------------------------------------------------
-spec validate(entity_logic:req()) -> entity_logic:validity_verificator().
validate(#el_req{operation = create, gri = #gri{aspect = instance}}) -> #{
    required => #{
        <<"name">> => {binary, name}
    },
    optional => #{
        <<"qosParameters">> => {json, qos_parameters},
        <<"imported">> => {boolean, any},
        <<"readonly">> => {boolean, any}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = support}}) -> #{
    required => #{
        <<"token">> => {invite_token, ?SUPPORT_SPACE},
        <<"size">> => {integer, {not_lower_than, ?MINIMUM_SUPPORT_SIZE}}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = init_support}}) -> #{
    required => #{
        <<"token">> => {invite_token, ?SUPPORT_SPACE},
        <<"size">> => {integer, {not_lower_than, ?MINIMUM_SUPPORT_SIZE}}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {init_unsupport, _}}}) -> #{
};
validate(#el_req{operation = create, gri = #gri{aspect = {complete_unsupport_resize, _}}}) -> #{
};
validate(#el_req{operation = create, gri = #gri{aspect = {complete_unsupport_purge, _}}}) -> #{
};
validate(#el_req{operation = create, gri = #gri{aspect = {finalize_unsupport, _}}}) -> #{
};

validate(#el_req{operation = create, gri = #gri{aspect = {upgrade_legacy_support, _}}}) -> #{
};
validate(#el_req{operation = create, gri = #gri{aspect = {upgrade_support_to_22_02, _}}}) -> #{
};

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    at_least_one => #{
        <<"name">> => {binary, name},
        <<"qosParameters">> => {json, qos_parameters},
        <<"imported">> => {boolean, any},
        <<"readonly">> => {boolean, any}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = {space, _}}}) -> #{
    required => #{
        <<"size">> => {integer, {not_lower_than, ?MINIMUM_SUPPORT_SIZE}}
    }
}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec ensure_id(undefined | od_storage:id()) -> od_storage:id().
ensure_id(undefined) -> datastore_key:new();
ensure_id(StorageId) -> StorageId.


%% @private
-spec supports_any_space(od_storage:record() | od_storage:id()) -> boolean().
supports_any_space(#od_storage{spaces = Spaces}) ->
    map_size(Spaces) > 0.


%% @private
-spec auth_by_support(od_provider:id(), od_storage:record() | od_storage:id(), od_space:id()) ->
    boolean().
auth_by_support(ProviderId, StorageOrId, SpaceId) ->
    storage_logic:belongs_to_provider(StorageOrId, ProviderId) andalso
        storage_logic:supports_space(StorageOrId, SpaceId).


%%--------------------------------------------------------------------
%% @doc
%% @private
%% Modification of imported value should be blocked if storage supports any space
%% unless it was previously `unknown` meaning that storage was created by legacy provider.
%% @end
%%--------------------------------------------------------------------
-spec check_imported_storage_value(PreviousValue :: boolean() | unknown, NewValue :: boolean(),
    SupportsAnySpace :: boolean()) -> ok | no_return().
check_imported_storage_value(unknown = _PreviousValue, _NewValue, _SupportsAnySpace) -> ok;
check_imported_storage_value(_PreviousValue, _NewValue, false = _SupportsAnySpace) -> ok;
check_imported_storage_value(PreviousValue, PreviousValue = _NewValue, _SupportsAnySpace) -> ok;
check_imported_storage_value(_PreviousValue, _NewValue, _SupportsAnySpace) -> throw(?ERROR_STORAGE_IN_USE).


%% @private
-spec check_readonly_value(ReadonlyValue :: boolean(), IsImportedStorage :: boolean(), od_storage:id()) ->
    ok | no_return().
check_readonly_value(true = _ReadonlyValue, false = _IsImportedStorage, StorageId) ->
    throw(?ERROR_REQUIRES_IMPORTED_STORAGE(StorageId));
check_readonly_value(_ReadonlyValue, _IsImportedStorage, _StorageId) -> ok.


%% @private
-spec add_implicit_qos_parameters(od_storage:id(), od_provider:id(), od_storage:qos_parameters()) ->
    od_storage:qos_parameters() | no_return().
add_implicit_qos_parameters(_StorageId, ProviderId, #{<<"providerId">> := OtherProvider}) when ProviderId =/= OtherProvider ->
    throw(?ERROR_BAD_VALUE_NOT_ALLOWED(<<"qosParameters.providerId">>, [ProviderId]));
add_implicit_qos_parameters(StorageId, _ProviderId, #{<<"storageId">> := OtherStorage}) when StorageId =/= OtherStorage ->
    throw(?ERROR_BAD_VALUE_NOT_ALLOWED(<<"qosParameters.storageId">>, [StorageId]));
add_implicit_qos_parameters(StorageId, ProviderId, QosParameters) ->
    QosParameters#{
        <<"storageId">> => StorageId,
        <<"providerId">> => ProviderId
    }.
