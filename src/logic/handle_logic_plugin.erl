%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_handle model.
%%% @end
%%%-------------------------------------------------------------------
-module(handle_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/errors.hrl").
-include("http/handlers/oai.hrl").

-export([fetch_entity/1, operation_supported/3, is_subscribable/2]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, required_admin_privileges/1, validate/1]).

-define(METADATA_SIZE_LIMIT, 100000).
-define(AVAILABLE_METADATA_FORMATS, oai_metadata:supported_formats()).
-define(DEFAULT_METADATA_PREFIX, ?OAI_DC_METADATA_PREFIX).

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
fetch_entity(#gri{id = HandleId}) ->
    case od_handle:get(HandleId) of
        {ok, #document{value = Handle, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_rev:parse(DbRev),
            {true, {Handle, Revision}};
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
operation_supported(create, {user, _}, private) -> true;
operation_supported(create, {group, _}, private) -> true;

operation_supported(get, list, private) -> true;
operation_supported(get, privileges, _) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, instance, protected) -> true;
operation_supported(get, instance, public) -> true;

operation_supported(get, users, private) -> true;
operation_supported(get, eff_users, private) -> true;
operation_supported(get, {user_privileges, _}, private) -> true;
operation_supported(get, {eff_user_privileges, _}, private) -> true;

operation_supported(get, groups, private) -> true;
operation_supported(get, eff_groups, private) -> true;
operation_supported(get, {group_privileges, _}, private) -> true;
operation_supported(get, {eff_group_privileges, _}, private) -> true;

operation_supported(update, instance, private) -> true;
operation_supported(update, {user_privileges, _}, private) -> true;
operation_supported(update, {group_privileges, _}, private) -> true;

operation_supported(delete, instance, private) -> true;
operation_supported(delete, {user, _}, private) -> true;
operation_supported(delete, {group, _}, private) -> true;

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
create(Req = #el_req{gri = #gri{id = undefined, aspect = instance} = GRI, auth = Auth}) ->
    HandleServiceId = maps:get(<<"handleServiceId">>, Req#el_req.data),
    ResourceType = <<"Share">> = maps:get(<<"resourceType">>, Req#el_req.data),
    ShareId = maps:get(<<"resourceId">>, Req#el_req.data),
    RawMetadata = maps:get(<<"metadata">>, Req#el_req.data),
    MetadataPrefix = maps:get(<<"metadataPrefix">>, Req#el_req.data, ?DEFAULT_METADATA_PREFIX),
    CreationTime = od_handle:current_timestamp(),

    % ensure no race conditions when creating a handle for a share (only one may be created)
    od_share:critical_section_for(ShareId, fun() ->
        {ok, #document{value = ShareRecord}} = od_share:get(ShareId),

        ShareRecord#od_share.handle =:= undefined orelse throw(?ERROR_ALREADY_EXISTS),

        RevisedMetadata = raw_metadata_to_revised_for_publication(MetadataPrefix, RawMetadata, ShareId, ShareRecord),

        {ok, PublicHandle} = handle_proxy:register_handle(
            HandleServiceId, ResourceType, ShareId, oai_metadata:encode_xml(MetadataPrefix, RevisedMetadata)
        ),

        FinalMetadata = oai_metadata:insert_public_handle(MetadataPrefix, RevisedMetadata, PublicHandle),

        {ok, #document{key = HandleId}} = od_handle:create(#document{value = #od_handle{
            public_handle = PublicHandle,
            resource_type = ResourceType,
            metadata_prefix = MetadataPrefix,
            metadata = oai_metadata:encode_xml(MetadataPrefix, FinalMetadata),
            timestamp = CreationTime,

            resource_id = ShareId,
            handle_service = HandleServiceId,

            creator = aai:normalize_subject(Auth#auth.subject),
            creation_time = CreationTime
        }}),

        case Req#el_req.auth_hint of
            ?AS_USER(UserId) ->
                entity_graph:add_relation(
                    od_user, UserId,
                    od_handle, HandleId,
                    privileges:handle_admin()
                );
            ?AS_GROUP(GroupId) ->
                entity_graph:add_relation(
                    od_group, GroupId,
                    od_handle, HandleId,
                    privileges:handle_admin()
                );
            _ ->
                ok
        end,
        entity_graph:add_relation(
            od_handle, HandleId,
            od_share, ShareId
        ),

        handle_registry:report_created(MetadataPrefix, HandleServiceId, HandleId, CreationTime),
        {true, {FetchedHandle, Rev}} = fetch_entity(#gri{aspect = instance, id = HandleId}),
        {ok, resource, {GRI#gri{id = HandleId}, {FetchedHandle, Rev}}}
    end);

create(#el_req{gri = #gri{id = HandleId, aspect = {user, UserId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:handle_member()),
    entity_graph:add_relation(
        od_user, UserId,
        od_handle, HandleId,
        Privileges
    ),
    NewGRI = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
    {true, {User, Rev}} = user_logic_plugin:fetch_entity(#gri{id = UserId}),
    {ok, UserData} = user_logic_plugin:get(#el_req{gri = NewGRI}, User),
    {ok, resource, {NewGRI, ?THROUGH_HANDLE(HandleId), {UserData, Rev}}};

create(#el_req{gri = #gri{id = HandleId, aspect = {group, GroupId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:handle_member()),
    entity_graph:add_relation(
        od_group, GroupId,
        od_handle, HandleId,
        Privileges
    ),
    NewGRI = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
    {true, {Group, Rev}} = group_logic_plugin:fetch_entity(#gri{id = GroupId}),
    {ok, GroupData} = group_logic_plugin:get(#el_req{gri = NewGRI}, Group),
    {ok, resource, {NewGRI, ?THROUGH_HANDLE(HandleId), {GroupData, Rev}}}.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = list}}, _) ->
    % NOTE: od_handle:list() still uses the datastore's secure fold listing
    % (which should be reworked at some point), but the handle registry provides
    % a lighter way to list (nevertheless, at some point we should never list whole
    % collections, but do this in batches).
    {ok, [H#handle_listing_entry.handle_id || H <- handle_registry:gather_by_all_prefixes()]};

get(#el_req{gri = #gri{aspect = privileges}}, _) ->
    {ok, #{
        <<"member">> => privileges:handle_member(),
        <<"admin">> => privileges:handle_admin()
    }};

get(#el_req{gri = #gri{aspect = instance, scope = private}}, Handle) ->
    {ok, Handle};
get(#el_req{gri = #gri{aspect = instance, scope = protected}}, Handle) ->
    #od_handle{handle_service = HandleService, public_handle = PublicHandle,
        resource_type = ResourceType, resource_id = ResourceId,
        metadata = Metadata, metadata_prefix = MetadataPrefix,
        timestamp = Timestamp, creation_time = CreationTime, creator = Creator
    } = Handle,
    {ok, #{
        <<"handleServiceId">> => HandleService,
        <<"publicHandle">> => PublicHandle,
        <<"resourceType">> => ResourceType,
        <<"resourceId">> => ResourceId,
        <<"metadata">> => Metadata,
        <<"metadataPrefix">> => MetadataPrefix,
        <<"timestamp">> => Timestamp,
        <<"creationTime">> => CreationTime,
        <<"creator">> => Creator
    }};
get(#el_req{gri = #gri{aspect = instance, scope = public}}, Handle) ->
    #od_handle{
        handle_service = HandleServiceId, public_handle = PublicHandle,
        resource_type = ResourceType, resource_id = ResourceId,
        metadata = Metadata, metadata_prefix = MetadataPrefix,
        timestamp = Timestamp, creation_time = CreationTime
    } = Handle,
    {ok, #{
        <<"handleServiceId">> => HandleServiceId,
        <<"publicHandle">> => PublicHandle,
        <<"resourceType">> => ResourceType,
        <<"resourceId">> => ResourceId,
        <<"metadata">> => Metadata,
        <<"metadataPrefix">> => MetadataPrefix,
        <<"timestamp">> => Timestamp,
        <<"creationTime">> => CreationTime
    }};

get(#el_req{gri = #gri{aspect = users}}, Handle) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_user, Handle)};
get(#el_req{gri = #gri{aspect = eff_users}}, Handle) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_user, Handle)};
get(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, Handle) ->
    {ok, entity_graph:get_relation_attrs(direct, bottom_up, od_user, UserId, Handle)};
get(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, Handle) ->
    {ok, entity_graph:get_relation_attrs(effective, bottom_up, od_user, UserId, Handle)};

get(#el_req{gri = #gri{aspect = groups}}, Handle) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_group, Handle)};
get(#el_req{gri = #gri{aspect = eff_groups}}, Handle) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_group, Handle)};
get(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, Handle) ->
    {ok, entity_graph:get_relation_attrs(direct, bottom_up, od_group, GroupId, Handle)};
get(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, Handle) ->
    {ok, entity_graph:get_relation_attrs(effective, bottom_up, od_group, GroupId, Handle)}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = HandleId, aspect = instance}, data = Data}) ->
    od_handle:critical_section_for(HandleId, fun() ->
        {ok, #document{value = #od_handle{
            handle_service = HandleService,
            timestamp = PreviousTimestamp,
            resource_id = ShareId,
            metadata_prefix = MetadataPrefix,
            public_handle = PublicHandle
        }}} = od_handle:get(HandleId),
        {ok, #document{value = ShareRecord}} = od_share:get(ShareId),

        % only the metadata field can be updated
        InputRawMetadata = maps:get(<<"metadata">>, Data),
        RevisedMetadata = raw_metadata_to_revised_for_publication(MetadataPrefix, InputRawMetadata, ShareId, ShareRecord),
        FinalMetadata = oai_metadata:insert_public_handle(MetadataPrefix, RevisedMetadata, PublicHandle),
        FinalRawMetadata = oai_metadata:encode_xml(MetadataPrefix, FinalMetadata),

        CurrentTimestamp = od_handle:current_timestamp(),
        {ok, _} = od_handle:update(HandleId, fun(Handle = #od_handle{}) ->
            {ok, Handle#od_handle{
                timestamp = CurrentTimestamp,
                metadata = FinalRawMetadata
            }}
        end),
        % every handle modification must be reflected in the handle registry
        handle_registry:report_updated(MetadataPrefix, HandleService, HandleId, PreviousTimestamp, CurrentTimestamp)
        % TODO VFS-7454 currently not supported by the handle proxy implementation
        % handle_proxy:modify_handle(HandleId, FinalRawMetadata)
    end);


update(Req = #el_req{gri = #gri{id = HandleId, aspect = {user_privileges, UserId}}}) ->
    PrivsToGrant = maps:get(<<"grant">>, Req#el_req.data, []),
    PrivsToRevoke = maps:get(<<"revoke">>, Req#el_req.data, []),
    entity_graph:update_relation(
        od_user, UserId,
        od_handle, HandleId,
        {PrivsToGrant, PrivsToRevoke}
    );

update(Req = #el_req{gri = #gri{id = HandleId, aspect = {group_privileges, GroupId}}}) ->
    PrivsToGrant = maps:get(<<"grant">>, Req#el_req.data, []),
    PrivsToRevoke = maps:get(<<"revoke">>, Req#el_req.data, []),
    entity_graph:update_relation(
        od_group, GroupId,
        od_handle, HandleId,
        {PrivsToGrant, PrivsToRevoke}
    ).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = HandleId, aspect = instance}}) ->
    od_handle:critical_section_for(HandleId, fun() ->
        {ok, #document{value = #od_handle{
            handle_service = HandleService,
            timestamp = PreviousTimestamp,
            metadata_prefix = MetadataPrefix,
            public_handle = PublicHandle
        }}} = od_handle:get(HandleId),
        try
            handle_proxy:unregister_handle(HandleId)
        catch Class:Reason:Stacktrace ->
            ?warning_exception(?autoformat_with_msg(
                "Handle was removed but it failed to be unregistered from its handle service",
                [HandleId, PublicHandle, HandleService]
            ), Class, Reason, Stacktrace)
        end,
        DeletionTimestamp = od_handle:current_timestamp(),
        handle_registry:report_deleted(MetadataPrefix, HandleService, HandleId, PreviousTimestamp, DeletionTimestamp),
        entity_graph:delete_with_relations(od_handle, HandleId)
    end);

delete(#el_req{gri = #gri{id = HandleId, aspect = {user, UserId}}}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_handle, HandleId
    );

delete(#el_req{gri = #gri{id = HandleId, aspect = {group, GroupId}}}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_handle, HandleId
    ).


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(Req = #el_req{gri = #gri{aspect = instance, scope = protected}}, Handle = #od_handle{}) ->
    case Req#el_req.auth_hint of
        ?THROUGH_USER(UserId) ->
            handle_logic:has_eff_user(Handle, UserId);
        ?THROUGH_GROUP(GroupId) ->
            handle_logic:has_eff_group(Handle, GroupId);
        ?THROUGH_HANDLE_SERVICE(HServiceId) ->
            handle_logic:has_handle_service(Handle, HServiceId);
        undefined ->
            true
    end;

exists(#el_req{gri = #gri{aspect = {user, UserId}}}, Handle) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, Handle);

exists(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, Handle) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, Handle);

exists(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, Handle) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Handle);

exists(#el_req{gri = #gri{aspect = {group, GroupId}}}, Handle) ->
    entity_graph:has_relation(direct, bottom_up, od_group, GroupId, Handle);

exists(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, Handle) ->
    entity_graph:has_relation(direct, bottom_up, od_group, GroupId, Handle);

exists(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, Handle) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, Handle);

% All other aspects exist if handle record exists.
exists(#el_req{gri = #gri{id = Id}}, #od_handle{}) ->
    Id =/= undefined.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(Req = #el_req{operation = create, gri = #gri{id = undefined, aspect = instance}}, _) ->
    HServiceId = maps:get(<<"handleServiceId">>, Req#el_req.data, <<"">>),
    ShareId = maps:get(<<"resourceId">>, Req#el_req.data, <<"">>),
    SpaceId = try share_logic_plugin:fetch_entity(#gri{id = ShareId}) of
        {error, _} ->
            throw(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"resourceId">>));
        {true, {#od_share{space = SpId}, _}} ->
            SpId
    catch _:_ ->
        throw(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"resourceId">>))
    end,
    HandleService = try handle_service_logic_plugin:fetch_entity(#gri{id = HServiceId}) of
        {error, _} ->
            throw(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"handleServiceId">>));
        {true, {HService, _}} ->
            HService
    catch _:_ ->
        throw(?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"handleServiceId">>))
    end,
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            space_logic:has_eff_privilege(SpaceId, UserId, ?SPACE_MANAGE_SHARES) andalso
                handle_service_logic:has_eff_privilege(HandleService, UserId, ?HANDLE_SERVICE_REGISTER_HANDLE);

        {?USER(UserId), ?AS_GROUP(GroupId)} ->
            handle_service_logic:has_eff_group(HandleService, GroupId) andalso
                space_logic:has_eff_privilege(SpaceId, UserId, ?SPACE_MANAGE_SHARES) andalso
                group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_CREATE_HANDLE) andalso
                handle_service_logic:has_eff_privilege(HandleService, UserId, ?HANDLE_SERVICE_REGISTER_HANDLE);

        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, _}}}, Handle) ->
    auth_by_privilege(Req, Handle, ?HANDLE_UPDATE);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {group, _}}}, Handle) ->
    auth_by_privilege(Req, Handle, ?HANDLE_UPDATE);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = instance, scope = private}}, Handle) ->
    auth_by_privilege(Req, Handle, ?HANDLE_VIEW);

authorize(#el_req{operation = get, gri = #gri{aspect = privileges}}, _) ->
    true;

authorize(Req = #el_req{operation = get, gri = GRI = #gri{aspect = instance, scope = protected}}, Handle) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?THROUGH_USER(UserId)} ->
            % User's membership in this handle_service is checked in 'exists'
            true;

        {?USER(_UserId), ?THROUGH_USER(_OtherUserId)} ->
            false;

        {?USER(ClientUserId), ?THROUGH_GROUP(GroupId)} ->
            % Groups's membership in this handle_service is checked in 'exists'
            group_logic:has_eff_privilege(GroupId, ClientUserId, ?GROUP_VIEW);

        {?USER(ClientUserId), ?THROUGH_HANDLE_SERVICE(HServiceId)} ->
            % Handle belonging to handle_service is checked in 'exists'
            handle_service_logic:has_eff_privilege(
                HServiceId, ClientUserId, ?HANDLE_SERVICE_VIEW
            );

        {?USER(ClientUserId), _} ->
            handle_logic:has_eff_user(Handle, ClientUserId);

        _ ->
            % Access to private data also allows access to protected data
            authorize(Req#el_req{gri = GRI#gri{scope = private}}, Handle)
    end;

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {user_privileges, UserId}}}, _) ->
    true;

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {eff_user_privileges, UserId}}}, _) ->
    true;

authorize(#el_req{operation = get, gri = #gri{aspect = instance, scope = public}}, _) ->
    true;

authorize(Req = #el_req{operation = get, auth = ?USER}, Handle) ->
    % All other resources can be accessed with view privileges
    auth_by_privilege(Req, Handle, ?HANDLE_VIEW);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = instance}}, Handle) ->
    auth_by_privilege(Req, Handle, ?HANDLE_UPDATE) orelse
        auth_by_hservice_privilege(Req, Handle, ?HANDLE_SERVICE_MANAGE_HANDLES);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}, Handle) ->
    auth_by_privilege(Req, Handle, ?HANDLE_UPDATE);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {group_privileges, _}}}, Handle) ->
    auth_by_privilege(Req, Handle, ?HANDLE_UPDATE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = instance}}, Handle) ->
    auth_by_privilege(Req, Handle, ?HANDLE_DELETE) orelse
        auth_by_hservice_privilege(Req, Handle, ?HANDLE_SERVICE_MANAGE_HANDLES);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {user, _}}}, Handle) ->
    auth_by_privilege(Req, Handle, ?HANDLE_UPDATE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {group, _}}}, Handle) ->
    auth_by_privilege(Req, Handle, ?HANDLE_UPDATE);

authorize(_, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc
%% Returns list of admin privileges needed to perform given operation.
%% @end
%%--------------------------------------------------------------------
-spec required_admin_privileges(entity_logic:req()) -> [privileges:oz_privilege()] | forbidden.
required_admin_privileges(Req = #el_req{operation = create, gri = #gri{aspect = instance}}) ->
    case Req#el_req.auth_hint of
        ?AS_USER(_) -> [?OZ_HANDLES_CREATE, ?OZ_USERS_ADD_RELATIONSHIPS];
        ?AS_GROUP(_) -> [?OZ_HANDLES_CREATE, ?OZ_GROUPS_ADD_RELATIONSHIPS];
        _ -> [?OZ_HANDLES_CREATE]
    end;

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {user, _}}}) ->
    [?OZ_HANDLES_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {group, _}}}) ->
    [?OZ_HANDLES_ADD_RELATIONSHIPS, ?OZ_GROUPS_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = list}}) ->
    [?OZ_HANDLES_LIST];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance, scope = protected}}) ->
    [?OZ_HANDLES_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance, scope = public}}) ->
    [?OZ_HANDLES_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = users}}) ->
    [?OZ_HANDLES_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_users}}) ->
    [?OZ_HANDLES_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {user_privileges, _}}}) ->
    [?OZ_HANDLES_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_user_privileges, _}}}) ->
    [?OZ_HANDLES_VIEW_PRIVILEGES];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = groups}}) ->
    [?OZ_HANDLES_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_groups}}) ->
    [?OZ_HANDLES_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {group_privileges, _}}}) ->
    [?OZ_HANDLES_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_group_privileges, _}}}) ->
    [?OZ_HANDLES_VIEW_PRIVILEGES];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = instance}}) ->
    [?OZ_HANDLES_UPDATE];
required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) ->
    [?OZ_HANDLES_SET_PRIVILEGES];
required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {group_privileges, _}}}) ->
    [?OZ_HANDLES_SET_PRIVILEGES];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = instance}}) ->
    [?OZ_HANDLES_DELETE];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {user, _}}}) ->
    [?OZ_HANDLES_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {group, _}}}) ->
    [?OZ_HANDLES_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS];

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
-spec validate(entity_logic:req()) -> entity_logic_sanitizer:sanitizer_spec().
validate(#el_req{operation = create, gri = #gri{aspect = instance}}) -> #{
    required => #{
        <<"handleServiceId">> => {any, {exists, fun(Value) ->
            handle_service_logic:exists(Value)
        end}},
        <<"resourceType">> => {binary, [<<"Share">>]},
        <<"resourceId">> => {any, {exists, fun(Value) ->
            share_logic:exists(Value) end
        }},
        <<"metadataPrefix">> => {binary, ?AVAILABLE_METADATA_FORMATS},
        <<"metadata">> => {binary, {text_length_limit, ?METADATA_SIZE_LIMIT}}

    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {user, _}}}) -> #{
    required => #{
        {aspect, <<"userId">>} => {any, {exists, fun(UserId) ->
            user_logic:exists(UserId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:handle_privileges()}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {group, _}}}) -> #{
    required => #{
        {aspect, <<"groupId">>} => {any, {exists, fun(GroupId) ->
            group_logic:exists(GroupId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:handle_privileges()}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    required => #{
        <<"metadata">> => {binary, {text_length_limit, ?METADATA_SIZE_LIMIT}}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) ->
    #{
        at_least_one => #{
            <<"grant">> => {list_of_atoms, privileges:handle_privileges()},
            <<"revoke">> => {list_of_atoms, privileges:handle_privileges()}
        }
    };

validate(#el_req{operation = update, gri = #gri{aspect = {group_privileges, Id}}}) ->
    validate(#el_req{operation = update, gri = #gri{aspect = {user_privileges, Id}}}).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec raw_metadata_to_revised_for_publication(
    od_handle:metadata_prefix(), od_handle:raw_metadata(), od_share:id(), od_share:record()
) ->
    od_handle:parsed_metadata().
raw_metadata_to_revised_for_publication(MetadataPrefix, RawMetadata, ShareId, ShareRecord) ->
    ParsedMetadata = case oai_xml:parse(RawMetadata) of
        {ok, Parsed} -> Parsed;
        error -> throw(?ERROR_BAD_VALUE_XML(<<"metadata">>))
    end,
    case oai_metadata:revise_for_publication(MetadataPrefix, ParsedMetadata, ShareId, ShareRecord) of
        {ok, Revised} -> Revised;
        error -> throw(?ERROR_BAD_VALUE_XML(<<"metadata">>))
    end.


-spec auth_by_privilege(
    entity_logic:req() | od_user:id(),
    od_handle:id() | od_handle:record(),
    privileges:handle_privilege()
) ->
    boolean().
auth_by_privilege(#el_req{auth = ?USER(UserId)}, HandleOrId, Privilege) ->
    auth_by_privilege(UserId, HandleOrId, Privilege);
auth_by_privilege(#el_req{auth = _OtherAuth}, _HandleOrId, _Privilege) ->
    false;
auth_by_privilege(UserId, HandleOrId, Privilege) ->
    handle_logic:has_eff_privilege(HandleOrId, UserId, Privilege).


-spec auth_by_hservice_privilege(
    entity_logic:req() | od_user:id(),
    od_handle:record(),
    privileges:handle_service_privilege()
) ->
    boolean().
auth_by_hservice_privilege(#el_req{auth = ?USER(UserId)}, Handle, Privilege) ->
    auth_by_hservice_privilege(UserId, Handle, Privilege);
auth_by_hservice_privilege(#el_req{auth = _OtherAuth}, _Handle, _Privilege) ->
    false;
auth_by_hservice_privilege(UserId, #od_handle{handle_service = HServiceId}, Privilege) ->
    handle_service_logic:has_eff_privilege(HServiceId, UserId, Privilege).
