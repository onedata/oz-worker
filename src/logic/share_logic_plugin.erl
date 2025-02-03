%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_share model.
%%% @end
%%%-------------------------------------------------------------------
-module(share_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata_file.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/errors.hrl").

-export([fetch_entity/1, operation_supported/3, is_subscribable/2]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, required_admin_privileges/1, validate/1]).

-define(SHARE_DESCRIPTION_SIZE_LIMIT, 100000).


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
fetch_entity(#gri{id = ShareId}) ->
    case od_share:get(ShareId) of
        {ok, #document{value = Share, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_rev:parse(DbRev),
            {true, {Share, Revision}};
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

operation_supported(get, list, private) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, instance, public) -> true;

operation_supported(update, instance, private) -> true;

operation_supported(delete, instance, private) -> true;

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
    ShareId = maps:get(<<"shareId">>, Req#el_req.data),
    Name = maps:get(<<"name">>, Req#el_req.data),
    Description = maps:get(<<"description">>, Req#el_req.data, <<"">>),
    SpaceId = maps:get(<<"spaceId">>, Req#el_req.data),
    RootFileId = maps:get(<<"rootFileId">>, Req#el_req.data),
    % TODO VFS-VFS-12490 [file, dir] deprecated, left for BC, can be removed in 23.02.*
    FileType = case maps:get(<<"fileType">>, Req#el_req.data, ?DIRECTORY_TYPE) of
        file -> ?REGULAR_FILE_TYPE;
        dir -> ?DIRECTORY_TYPE;
        ModernType -> ModernType
    end,

    RootFileUuid = try
        {FileUuid, SpaceId, ShareId} = file_id:unpack_share_guid(RootFileId),
        true = is_binary(FileUuid) andalso byte_size(FileUuid) > 0,
        FileUuid
    catch _:_ ->
        throw(?ERR_BAD_DATA(?err_ctx(), <<"rootFileId">>, undefined))
    end,

    ShareDoc = #document{key = ShareId, value = #od_share{
        name = Name,
        description = Description,
        root_file_uuid = RootFileUuid,
        file_type = FileType,
        space = SpaceId,
        creator = aai:normalize_subject(Auth#auth.subject),
        creation_time = global_clock:timestamp_seconds()
    }},
    case od_share:create(ShareDoc) of
        {ok, #document{value = ShareRecord, revs = [DbRev | _]}} ->
            share_registry:report_created(ShareId, ShareRecord),
            {Rev, _Hash} = datastore_rev:parse(DbRev),
            {ok, resource, {GRI#gri{id = ShareId}, {ShareRecord, Rev}}};
        {error, already_exists} ->
            % This can potentially happen if a share with given share id
            % has been created between data verification and create
            ?ERROR_ALREADY_EXISTS
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = list}}, _) ->
    {ok, ShareDocs} = od_share:list(),
    {ok, [ShareId || #document{key = ShareId} <- ShareDocs]};

get(#el_req{gri = #gri{aspect = instance, scope = private}}, ShareRecord) ->
    {ok, ShareRecord};
get(#el_req{gri = #gri{id = ShareId, aspect = instance, scope = public}}, ShareRecord) ->
    #od_share{
        space = SpaceId,
        name = Name,
        description = Description,
        handle = HandleId,
        file_type = FileType,
        creation_time = CreationTime
    } = ShareRecord,
    {ok, #{
        <<"spaceId">> => SpaceId,
        <<"name">> => Name,
        <<"description">> => Description,
        <<"rootFileObjectId">> => od_share:build_root_file(objectid, ShareId, ShareRecord),
        <<"fileType">> => FileType,
        <<"handleId">> => HandleId,
        <<"creationTime">> => CreationTime
    }}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = ShareId, aspect = instance}, data = Data}) ->
    od_share:critical_section_for(ShareId, fun() ->
        #document{value = #od_share{name = OldName} = PreviousShareRecord} = ?check(od_share:get(ShareId)),
        NewName = maps:get(<<"name">>, Data, OldName),

        {ok, _} = od_share:update(ShareId, fun(Share) ->
            {ok, Share#od_share{
                name = NewName,
                description = maps:get(<<"description">>, Data, Share#od_share.description)
            }}
        end),

        NewName /= OldName andalso share_registry:report_name_updated(ShareId, PreviousShareRecord, NewName),
        ok
    end).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{auth = Auth, gri = #gri{id = ShareId, aspect = instance}}) ->
    delete_internal(Auth, ShareId).


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(Req = #el_req{gri = #gri{aspect = instance, scope = private}}, Share) ->
    case Req#el_req.auth_hint of
        ?THROUGH_SPACE(SpaceId) ->
            Share#od_share.space =:= SpaceId;
        undefined ->
            true
    end;

exists(#el_req{gri = #gri{id = Id}}, #od_share{}) ->
    % All aspects exist if share record exists.
    Id =/= undefined.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(Req = #el_req{operation = create, gri = #gri{id = undefined, aspect = instance}}, _) ->
    SpaceId = maps:get(<<"spaceId">>, Req#el_req.data, <<"">>),
    auth_by_space_privilege(Req, SpaceId, ?SPACE_MANAGE_SHARES);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = instance, scope = private}}, Share) ->
    case Req#el_req.auth of
        ?USER(UserId) ->
            % In case of auth_hint = ?THROUGH_SPACE(SpaceId),
            % share's membership in space is checked in 'exists'.
            auth_by_space_privilege(UserId, Share, ?SPACE_VIEW);

        ?PROVIDER(ProviderId) ->
            auth_by_space_support(ProviderId, Share)

    end;

authorize(#el_req{operation = get, gri = #gri{aspect = instance, scope = public}}, _) ->
    true;

authorize(Req = #el_req{operation = update, gri = #gri{aspect = instance}}, Share) ->
    auth_by_space_privilege(Req, Share, ?SPACE_MANAGE_SHARES);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = instance}}, Share) ->
    auth_by_space_privilege(Req, Share, ?SPACE_MANAGE_SHARES);

authorize(_, _) ->
    false.


%%--------------------------------------------------------------------
%% @doc
%% Returns list of admin privileges needed to perform given operation.
%% @end
%%--------------------------------------------------------------------
-spec required_admin_privileges(entity_logic:req()) ->
    [privileges:oz_privilege()] | forbidden | fun((entity_logic:entity()) -> [privileges:oz_privilege()] | forbidden).
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = instance}}) ->
    [?OZ_SHARES_CREATE];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = list}}) ->
    [?OZ_SHARES_LIST];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance}}) ->
    [?OZ_SHARES_VIEW];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = instance}}) ->
    [?OZ_SHARES_UPDATE];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = instance}}) ->
    fun
        (#od_share{handle = undefined}) -> [?OZ_SHARES_DELETE];
        (#od_share{handle = _}) -> [?OZ_SHARES_DELETE, ?OZ_HANDLES_DELETE]
    end;

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
validate(#el_req{operation = create, gri = #gri{aspect = instance}, data = Data}) ->
    % TODO VFS-VFS-12490 [file, dir] deprecated, left for BC, can be removed in 23.02.*
    DeprecatedFileTypeValues = case Data of
        #{<<"fileType">> := <<"file">>} -> [file];
        #{<<"fileType">> := <<"dir">>} -> [dir];
        _ -> []
    end,
    #{
        required => #{
            <<"shareId">> => {binary, {not_exists, fun(Value) ->
                not share_logic:exists(Value)
            end}},
            <<"name">> => {binary, fun validate_name/1},
            <<"rootFileId">> => {binary, non_empty},  % file_id:share_root_file_guid()
            <<"spaceId">> => {any, {exists, fun(Value) ->
                space_logic:exists(Value)
            end}}
        },
        optional => #{
            <<"fileType">> => {atom, [?REGULAR_FILE_TYPE, ?DIRECTORY_TYPE] ++ DeprecatedFileTypeValues},
            <<"description">> => {binary, {text_length_limit, ?SHARE_DESCRIPTION_SIZE_LIMIT}}
        }
    };

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    at_least_one => #{
        <<"name">> => {binary, fun validate_name/1},
        <<"description">> => {binary, {text_length_limit, ?SHARE_DESCRIPTION_SIZE_LIMIT}}
    }
}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec delete_internal(aai:auth(), od_share:id()) -> ok.
delete_internal(Auth, ShareId) ->
    % ensure no race conditions with share update or with handle creation/deletion
    % (@see handle_logic_plugin)
    Result = od_share:critical_section_for(ShareId, fun() ->
        delete_share_unsafe(ShareId)
    end),
    % If the handle needs to be deleted first, it must be done outside of the
    % critical section to avoid a deadlock. Then, deletion is retried. Theoretically,
    % in case of parallel requests to add/remove a handle from the share, this
    % function may run a lot of times - but it will finally succeed.
    case Result of
        done ->
            ok;
        {handle_must_be_deleted_first, HandleId} ->
            case handle_logic:delete(Auth, HandleId) of
                ok -> ok;
                ?ERROR_NOT_FOUND -> ok;
                {error, _} = Error -> throw(Error)
            end,
            delete_internal(Auth, ShareId)
    end.


%% @private
-spec delete_share_unsafe(od_share:id()) -> done | {handle_must_be_deleted_first, od_handle:id()}.
delete_share_unsafe(ShareId) ->
    % race condition: in case of the share being already deleted, this will throw ?ERROR_NOT_FOUND
    #document{value = #od_share{handle = HandleId} = PreviousShareRecord} = ?check(od_share:get(ShareId)),
    % in case of DB inconsistencies (share points to a handle that does not exist) we
    % must not try to delete the handle first, otherwise we get infinite recursion
    case od_handle:exists(utils:ensure_defined(HandleId, <<"undefinedId">>)) of
        {ok, true} ->
            {handle_must_be_deleted_first, HandleId};
        {ok, false} ->
            share_registry:report_deleted(ShareId, PreviousShareRecord#od_share{handle = undefined}),
            od_share:force_delete(ShareId),
            done
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns if given user has specific effective privilege in space to which this
%% share belongs. UserId and SpaceId is either given explicitly or derived from
%% request or share record. Auths of type other than user are discarded.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_space_privilege(entity_logic:req() | od_user:id(),
    od_share:record() | od_space:id(), privileges:space_privilege()) ->
    boolean().
auth_by_space_privilege(#el_req{auth = ?USER(UserId)}, Share, Privilege) ->
    auth_by_space_privilege(UserId, Share, Privilege);
auth_by_space_privilege(#el_req{auth = _OtherAuth}, _Share, _Privilege) ->
    false;
auth_by_space_privilege(UserId, Share = #od_share{}, Privilege) ->
    auth_by_space_privilege(UserId, Share#od_share.space, Privilege);
auth_by_space_privilege(UserId, SpaceId, Privilege) ->
    space_logic:has_eff_privilege(SpaceId, UserId, Privilege).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns if given provider supports the space to which share represented
%% by entity belongs.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_space_support(od_provider:id(), od_share:record()) ->
    boolean().
auth_by_space_support(ProviderId, Share) ->
    space_logic:is_supported_by_provider(Share#od_share.space, ProviderId).


%% @private
-spec validate_name(od_share:name()) -> true | no_return().
validate_name(ShareName) ->
    case onedata_file:is_valid_filename(ShareName) of
        true when byte_size(ShareName) =< ?NAME_MAXIMUM_LENGTH ->
            true;
        _ ->
            % TODO VFS-12486 Rework all ERROR_BAD_DATA errors with hint into individual ones (waiting for od_error)
            throw(?ERR_BAD_DATA(?err_ctx(), <<"name">>, <<"Bad value: ", (?SHARE_NAME_REQUIREMENTS_DESCRIPTION)/binary>>))
    end.