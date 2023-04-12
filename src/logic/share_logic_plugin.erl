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
    FileType = maps:get(<<"fileType">>, Req#el_req.data, dir),
    ShareDoc = #document{key = ShareId, value = #od_share{
        name = Name,
        description = Description,
        root_file = RootFileId,
        file_type = FileType,
        creator = aai:normalize_subject(Auth#auth.subject),
        creation_time = global_clock:timestamp_seconds()
    }},
    case od_share:create(ShareDoc) of
        {ok, _} ->
            entity_graph:add_relation(
                od_share, ShareId,
                od_space, SpaceId
            ),
            {true, {Share, Rev}} = fetch_entity(#gri{aspect = instance, id = ShareId}),
            {ok, resource, {GRI#gri{id = ShareId}, {Share, Rev}}};
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

get(#el_req{gri = #gri{aspect = instance, scope = private}}, Share) ->
    {ok, Share};
get(#el_req{gri = #gri{aspect = instance, scope = public}}, Share) ->
    #od_share{
        space = SpaceId,
        name = Name, description = Description,
        handle = HandleId,
        root_file = RootFileId, file_type = FileType,
        creation_time = CreationTime
    } = Share,
    {ok, #{
        <<"spaceId">> => SpaceId,
        <<"name">> => Name, <<"description">> => Description,
        <<"rootFileId">> => RootFileId,
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
    {ok, _} = od_share:update(ShareId, fun(Share) ->
        #od_share{name = OldName, description = OldDescription} = Share,
        {ok, Share#od_share{
            name = maps:get(<<"name">>, Data, OldName),
            description = maps:get(<<"description">>, Data, OldDescription)
        }}
    end),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = ShareId, aspect = instance}}) ->
    entity_graph:delete_with_relations(od_share, ShareId).


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
-spec required_admin_privileges(entity_logic:req()) -> [privileges:oz_privilege()] | forbidden.
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = instance}}) ->
    [?OZ_SHARES_CREATE];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = list}}) ->
    [?OZ_SHARES_LIST];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance}}) ->
    [?OZ_SHARES_VIEW];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = instance}}) ->
    [?OZ_SHARES_UPDATE];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = instance}}) ->
    [?OZ_SHARES_DELETE];

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
        <<"shareId">> => {binary, {not_exists, fun(Value) ->
            not share_logic:exists(Value)
        end}},
        <<"name">> => {binary, name},
        <<"rootFileId">> => {binary, non_empty},
        <<"spaceId">> => {any, {exists, fun(Value) ->
            space_logic:exists(Value)
        end}}
    },
    optional => #{
        <<"fileType">> => {atom, [file, dir]},
        <<"description">> => {binary, {text_length_limit, ?SHARE_DESCRIPTION_SIZE_LIMIT}}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    at_least_one => #{
        <<"name">> => {binary, name},
        <<"description">> => {binary, {text_length_limit, ?SHARE_DESCRIPTION_SIZE_LIMIT}}
    }
}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

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

