%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_harvester model.
%%% @end
%%%-------------------------------------------------------------------
-module(harvester_logic_plugin).
-author("Michal Stanisz").
-behaviour(entity_logic_plugin_behaviour).

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/http/headers.hrl").

-export([fetch_entity/1, operation_supported/3, is_subscribable/2]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, required_admin_privileges/1, validate/1]).


-define(HARVESTING_BACKENDS, onezone_plugins:get_plugins(harvesting_backend) ++ [
    elasticsearch_harvesting_backend
]).

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
fetch_entity(#gri{id = HarvesterId}) ->
    case od_harvester:get(HarvesterId) of
        {ok, #document{value = Harvester, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_rev:parse(DbRev),
            {true, {Harvester, Revision}};
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

operation_supported(create, invite_user_token, private) -> true;
operation_supported(create, invite_group_token, private) -> true;
operation_supported(create, invite_space_token, private) -> true;

operation_supported(create, join, private) -> true;
operation_supported(create, instance, private) -> true;

operation_supported(create, {user, _}, private) -> true;
operation_supported(create, {group, _}, private) -> true;
operation_supported(create, {space, _}, private) -> true;
operation_supported(create, group, private) -> true;

operation_supported(create, index, private) -> true;
operation_supported(create, {submit_batch, _}, private) -> true;
operation_supported(create, {query, _}, private) -> true;
operation_supported(create, {gen_curl_query, _}, public) -> true;
operation_supported(create, {gen_curl_query, _}, private) -> true;

operation_supported(get, list, private) -> true;
operation_supported(get, all_backend_types, private) -> true;
operation_supported(get, privileges, _) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, instance, protected) -> true;
operation_supported(get, instance, shared) -> true;
operation_supported(get, instance, public) -> true;

operation_supported(get, gui_plugin_config, private) -> true;

operation_supported(get, {index, _}, private) -> true;
operation_supported(get, {index, _}, public) -> true;

operation_supported(get, users, private) -> true;
operation_supported(get, eff_users, private) -> true;
operation_supported(get, {user_privileges, _}, private) -> true;
operation_supported(get, {eff_user_privileges, _}, private) -> true;
operation_supported(get, {eff_user_membership, _}, private) -> true;

operation_supported(get, groups, private) -> true;
operation_supported(get, eff_groups, private) -> true;
operation_supported(get, {group_privileges, _}, private) -> true;
operation_supported(get, {eff_group_privileges, _}, private) -> true;
operation_supported(get, {eff_group_membership, _}, private) -> true;

operation_supported(get, spaces, private) -> true;

operation_supported(get, eff_providers, private) -> true;

operation_supported(get, indices, private) -> true;
operation_supported(get, {index_stats, _}, private) -> true;

operation_supported(update, instance, private) -> true;
operation_supported(update, gui_plugin_config, private) -> true;
operation_supported(update, {index, _}, private) -> true;
operation_supported(update, {user_privileges, _}, private) -> true;
operation_supported(update, {group_privileges, _}, private) -> true;

operation_supported(delete, instance, private) -> true;
operation_supported(delete, {user, _}, private) -> true;
operation_supported(delete, {group, _}, private) -> true;
operation_supported(delete, {space, _}, private) -> true;
operation_supported(delete, {index, _}, private) -> true;
operation_supported(delete, metadata, private) -> true;
operation_supported(delete, {index_metadata, _}, private) -> true;

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
is_subscribable(users, private) -> true;
is_subscribable(eff_users, private) -> true;
is_subscribable(groups, private) -> true;
is_subscribable(eff_groups, private) -> true;
is_subscribable(group, private) -> true;
is_subscribable({group, _}, private) -> true;
is_subscribable({user_privileges, _}, private) -> true;
is_subscribable({eff_user_privileges, _}, private) -> true;
is_subscribable({eff_user_membership, _}, private) -> true;
is_subscribable({group_privileges, _}, private) -> true;
is_subscribable({eff_group_privileges, _}, private) -> true;
is_subscribable({eff_group_membership, _}, private) -> true;
is_subscribable(spaces, private) -> true;
is_subscribable({space, _}, private) -> true;
is_subscribable(eff_providers, private) -> true;
is_subscribable(gui_plugin_config, private) -> true;
is_subscribable(index, _) -> true;
is_subscribable(indices, private) -> true;
is_subscribable({index, _}, _) -> true;
is_subscribable({index_stats, _}, private) -> true;
is_subscribable(_, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(#el_req{gri = #gri{aspect = instance} = GRI, auth = Auth,
    auth_hint = AuthHint, data = #{<<"name">> := Name} = Data}
) ->
    % Default value in get_env is needed so error is not raised when env is not defined.
    % If any env is not defined, all values concerning harvesting backend are in Data (see validate/1)
    BackendType = maps:get(<<"harvestingBackendType">>, Data,
        oz_worker:get_env(default_harvesting_backend_type, undefined)),
    BackendEndpoint = maps:get(<<"harvestingBackendEndpoint">>, Data,
        oz_worker:get_env(default_harvesting_backend_endpoint, undefined)),
    Config = maps:get(<<"guiPluginConfig">>, Data, #{}),

    NormalizedEndpoint = case normalize_endpoint_and_check_connectivity(BackendEndpoint, BackendType) of
        {ok, NewEndpoint} -> NewEndpoint;
        {error, _} = Error -> throw(Error)
    end,

    {ok, #document{key = HarvesterId}} = od_harvester:create(#document{
        value = #od_harvester{
            name = Name,
            endpoint = NormalizedEndpoint,
            backend = BackendType,
            gui_plugin_config = Config,
            creator = aai:normalize_subject(Auth#auth.subject),
            creation_time = global_clock:timestamp_seconds()
        }
    }),

    case AuthHint of
        ?AS_USER(UserId) ->
            entity_graph:add_relation(
                od_user, UserId,
                od_harvester, HarvesterId,
                privileges:harvester_admin()
            );
        ?AS_GROUP(GroupId) ->
            entity_graph:add_relation(
                od_group, GroupId,
                od_harvester, HarvesterId,
                privileges:harvester_admin()
            );
        _ ->
            ok
    end,

    ok = gui_static:link_default_harvester_gui(HarvesterId),

    {true, {Harvester, Rev}} = fetch_entity(#gri{aspect = instance, id = HarvesterId}),
    {ok, resource, {GRI#gri{id = HarvesterId}, {Harvester, Rev}}};

create(Req = #el_req{auth = Auth, gri = #gri{id = undefined, aspect = join}}) ->
    Token = maps:get(<<"token">>, Req#el_req.data),
    ExpType = case Req#el_req.auth_hint of
        ?AS_USER(_) -> ?USER_JOIN_HARVESTER;
        ?AS_GROUP(_) -> ?GROUP_JOIN_HARVESTER;
        ?AS_SPACE(_) -> ?SPACE_JOIN_HARVESTER
    end,

    invite_tokens:consume(Auth, Token, ExpType, fun(HarvesterId, _, Privileges) ->
        case Req#el_req.auth_hint of
            ?AS_USER(UserId) ->
                entity_graph:add_relation(
                    od_user, UserId,
                    od_harvester, HarvesterId,
                    Privileges
                );
            ?AS_GROUP(GroupId) ->
                entity_graph:add_relation(
                    od_group, GroupId,
                    od_harvester, HarvesterId,
                    Privileges
                );
            ?AS_SPACE(SpaceId) ->
                entity_graph:add_relation(
                    od_harvester, HarvesterId,
                    od_space, SpaceId
                ),
                harvester_indices:update_stats(HarvesterId, all, fun(ExistingStats) ->
                    harvester_indices:coalesce_index_stats(ExistingStats, SpaceId, false)
                end);
            _ ->
                ok
        end,
        {NewScope, NewAuthHint} = case Req#el_req.auth_hint of
            ?AS_SPACE(SpId) ->
                {shared, ?THROUGH_SPACE(SpId)};
            _ ->  % ?AS_USER or ?AS_GROUP
                Scope = case lists:member(?HARVESTER_VIEW, Privileges) of
                    true -> private;
                    false -> protected
                end,
                {Scope, undefined}

        end,
        NewGRI = #gri{type = od_harvester, id = HarvesterId, aspect = instance, scope = NewScope},
        {true, {Harvester, Rev}} = fetch_entity(#gri{aspect = instance, id = HarvesterId}),
        {ok, HarvesterData} = get(#el_req{gri = NewGRI}, Harvester),
        case NewAuthHint of
            undefined -> {ok, resource, {NewGRI, {HarvesterData, Rev}}};
            _ -> {ok, resource, {NewGRI, NewAuthHint, {HarvesterData, Rev}}}
        end
    end);

create(#el_req{auth = Auth, gri = #gri{id = HarvesterId, aspect = invite_user_token}}) ->
    %% @TODO VFS-5815 deprecated, should be removed in the next major version AFTER 20.02.*
    token_logic:create_legacy_invite_token(Auth, ?INVITE_TOKEN(?USER_JOIN_HARVESTER, HarvesterId));

create(#el_req{auth = Auth, gri = #gri{id = HarvesterId, aspect = invite_group_token}}) ->
    %% @TODO VFS-5815 deprecated, should be removed in the next major version AFTER 20.02.*
    token_logic:create_legacy_invite_token(Auth, ?INVITE_TOKEN(?GROUP_JOIN_HARVESTER, HarvesterId));

create(#el_req{auth = Auth, gri = #gri{id = HarvesterId, aspect = invite_space_token}}) ->
    %% @TODO VFS-5815 deprecated, should be removed in the next major version AFTER 20.02.*
    token_logic:create_legacy_invite_token(Auth, ?INVITE_TOKEN(?SPACE_JOIN_HARVESTER, HarvesterId));

create(#el_req{gri = #gri{id = HarvesterId, aspect = {user, UserId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:harvester_member()),
    entity_graph:add_relation(
        od_user, UserId,
        od_harvester, HarvesterId,
        Privileges
    ),
    NewGRI = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
    {true, {User, Rev}} = user_logic_plugin:fetch_entity(#gri{id = UserId}),
    {ok, UserData} = user_logic_plugin:get(#el_req{gri = NewGRI}, User),
    {ok, resource, {NewGRI, ?THROUGH_HARVESTER(HarvesterId), {UserData, Rev}}};

create(#el_req{gri = #gri{id = HarvesterId, aspect = {group, GroupId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:harvester_member()),
    entity_graph:add_relation(
        od_group, GroupId,
        od_harvester, HarvesterId,
        Privileges
    ),
    NewGRI = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
    {true, {Group, Rev}} = group_logic_plugin:fetch_entity(#gri{id = GroupId}),
    {ok, GroupData} = group_logic_plugin:get(#el_req{gri = NewGRI}, Group),
    {ok, resource, {NewGRI, ?THROUGH_GROUP(HarvesterId), {GroupData, Rev}}};

create(#el_req{gri = #gri{id = HarvesterId, aspect = {space, SpaceId}}}) ->
    entity_graph:add_relation(
        od_harvester, HarvesterId,
        od_space, SpaceId
    ),
    NewGRI = #gri{type = od_space, id = SpaceId, aspect = instance, scope = protected},
    {true, {Space, Rev}} = space_logic_plugin:fetch_entity(#gri{id = SpaceId}),
    {ok, SpaceData} = space_logic_plugin:get(#el_req{gri = NewGRI}, Space),
    harvester_indices:update_stats(HarvesterId, all,
        fun(ExistingStats) -> harvester_indices:coalesce_index_stats(ExistingStats, SpaceId, false) end),
    {ok, resource, {NewGRI, ?THROUGH_SPACE(HarvesterId), {SpaceData, Rev}}};

create(Req = #el_req{gri = GRI = #gri{id = HarvesterId, aspect = group}}) ->
    % Create a new group for a user and add the group as a member of this harvester.
    {ok, resource, {NewGRI = #gri{id = GroupId}, _}} = group_logic_plugin:create(
        Req#el_req{gri = GRI#gri{type = od_group, id = undefined, aspect = instance}}
    ),
    Privileges = privileges:harvester_member(),
    entity_graph:add_relation(
        od_group, GroupId,
        od_harvester, HarvesterId,
        Privileges
    ),
    {true, {Group, Rev}} = group_logic_plugin:fetch_entity(#gri{id = GroupId}),
    {ok, resource, {NewGRI, {Group, Rev}}};

create(#el_req{gri = Gri = #gri{aspect = index, id = HarvesterId}, data = Data}) ->
    IndexId = datastore_key:new(),

    Name = maps:get(<<"name">>, Data),
    Schema = maps:get(<<"schema">>, Data, undefined),
    GuiPluginName = utils:null_to_undefined(maps:get(<<"guiPluginName">>, Data, undefined)),

    Index = #harvester_index{
        name = Name,
        schema = Schema,
        gui_plugin_name = GuiPluginName
    },

    UpdateFun = fun(#od_harvester{indices = Indices, backend = HarvestingBackend, endpoint = Endpoint, spaces = Spaces} = Harvester) ->
        IndexStats = lists:foldl(fun(SpaceId, ExistingStats) ->
            harvester_indices:coalesce_index_stats(ExistingStats, SpaceId, false) end, #{}, Spaces),
        IndexInfo = Index#harvester_index{
            stats = IndexStats,
            include_metadata = maps:get(<<"includeMetadata">>, Data, Index#harvester_index.include_metadata),
            include_file_details = maps:get(<<"includeFileDetails">>, Data, Index#harvester_index.include_file_details),
            include_rejection_reason = maps:get(<<"includeRejectionReason">>, Data, Index#harvester_index.include_rejection_reason),
            retry_on_rejection = maps:get(<<"retryOnRejection">>, Data, Index#harvester_index.retry_on_rejection)
        },
        case HarvestingBackend:create_index(Endpoint, IndexId, IndexInfo, Schema) of
            ok ->
                {ok, Harvester#od_harvester{indices = Indices#{IndexId => IndexInfo}}};
            {error, _} = Error ->
                Error
        end
    end,
    NewGri = Gri#gri{aspect = {index, IndexId}, scope = private},
    case od_harvester:update(HarvesterId, UpdateFun) of
        {ok, #document{value = #od_harvester{indices = UpdatedIndices}}} ->
            UpdatedIndex = maps:get(IndexId, UpdatedIndices),
            {ok, resource, {NewGri, {UpdatedIndex, inherit_rev}}};
        {error, _} = Error ->
            Error
    end;

create(#el_req{gri = #gri{aspect = {query, IndexId}}, data = Data}) ->
    fun(#od_harvester{backend = HarvestingBackend, endpoint = Endpoint}) ->
        case HarvestingBackend:query_index(Endpoint, IndexId, Data) of
            {ok, Value} -> {ok, value, Value};
            {error, _} = Error -> Error
        end
    end;

create(#el_req{gri = #gri{id = HarvesterId, aspect = {gen_curl_query, IndexId}, scope = Scope}, data = Data}) ->
    Uri = oz_worker:get_rest_uri(
        <<"/harvesters/", HarvesterId/binary, "/indices/", IndexId/binary, "/query">>
    ),
    AuthHeaderOpt = case Scope of
        private -> str_utils:format_bin("-H \"~ts: $TOKEN\"", [?HDR_X_AUTH_TOKEN]);
        public -> <<"">>
    end,
    ContentTypeHeaderOpt = str_utils:format_bin("-H \"~ts: application/json\"", [?HDR_CONTENT_TYPE]),
    PrefixWithHeaders = str_utils:format_bin("curl -X POST ~ts ~ts ", [AuthHeaderOpt, ContentTypeHeaderOpt]),
    % escape all single quotation marks (') with backslashes (\)
    EncodedData = re:replace(json_utils:encode(Data), <<"'">>, <<"\\\\'">>, [{return, binary}, global]),
    {ok, value, <<PrefixWithHeaders/binary, Uri/binary, " -d '", EncodedData/binary, "'">>};

create(#el_req{auth = ?PROVIDER(ProviderId), gri = #gri{aspect = {submit_batch, SpaceId}, id = HarvesterId}, data = Data}) ->
    #{
        <<"indices">> := Indices,
        <<"maxSeq">> := MaxSeq,
        <<"maxStreamSeq">> := MaxStreamSeq,
        <<"batch">> := Batch
    } = Data,
    fun(#od_harvester{endpoint = Endpoint, backend = HarvestingBackend, indices = ExistingIndices}) ->
        IndicesToUpdate = [X || X <- Indices, Y <- maps:keys(ExistingIndices), X == Y],
        case Indices -- IndicesToUpdate of
            [] -> ok;
            Diff -> ?debug("Ignoring not known indices ~tp in harvester ~tp", [Diff, HarvesterId])
        end,
        {ok, Res} = case Batch of
            [] -> {ok, lists:map(fun(IndexId) -> {IndexId, ok} end, IndicesToUpdate)};
            _ -> HarvestingBackend:submit_batch(Endpoint, HarvesterId, maps:with(IndicesToUpdate, ExistingIndices), Batch)
        end,
        harvester_indices:update_stats(HarvesterId, Res, fun
            (PreviousStats, ok) ->
                harvester_indices:update_seqs(PreviousStats, SpaceId, ProviderId, MaxStreamSeq, MaxSeq, undefined);
            (PreviousStats, {error, NewCurrentSeq, _, ErrorMsg}) ->
                harvester_indices:update_seqs(PreviousStats, SpaceId, ProviderId, NewCurrentSeq, MaxSeq, ErrorMsg)
        end
        ),
        FailedIndices = lists:filtermap(
            fun({_IndexId, ok}) -> false;
                ({IndexId, {error, _, FailedSeq, _}}) -> {true, {IndexId, FailedSeq}}
            end, Res
        ),
        {ok, value, maps:from_list(FailedIndices)}
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
    {ok, HarvesterDocs} = od_harvester:list(),
    {ok, [HarvesterId || #document{key = HarvesterId} <- HarvesterDocs]};

get(#el_req{gri = #gri{aspect = privileges}}, _) ->
    {ok, #{
        <<"member">> => privileges:harvester_member(),
        <<"manager">> => privileges:harvester_manager(),
        <<"admin">> => privileges:harvester_admin()
    }};

get(#el_req{gri = #gri{aspect = instance, scope = private}}, Harvester) ->
    {ok, Harvester};
get(#el_req{gri = #gri{aspect = instance, scope = protected}}, Harvester) ->
    #od_harvester{
        name = Name,
        backend = HarvestingBackendType,
        endpoint = Endpoint,
        public = Public,
        creation_time = CreationTime,
        creator = Creator,
        bottom_up_dirty = BottomUpDirty
    } = Harvester,
    {ok, #{
        <<"name">> => Name,
        <<"public">> => Public,
        <<"harvestingBackendType">> => HarvestingBackendType,
        <<"harvestingBackendEndpoint">> => Endpoint,
        <<"areEffPrivilegesRecalculated">> => not BottomUpDirty,
        <<"creator">> => Creator,
        <<"creationTime">> => CreationTime
    }};
get(#el_req{gri = #gri{aspect = instance, scope = _}}, Harvester) ->  % covers shared and public scopes
    {ok, #{
        <<"name">> => Harvester#od_harvester.name
    }};

get(#el_req{gri = #gri{aspect = all_backend_types}}, _) ->
    {ok, lists:map(fun(HarvestingBackend) ->
        #{
            <<"id">> => HarvestingBackend,
            <<"name">> => HarvestingBackend:get_name()
        }
    end, ?HARVESTING_BACKENDS)};
get(#el_req{gri = #gri{aspect = gui_plugin_config}}, #od_harvester{gui_plugin_config = Config}) ->
    {ok, Config};

get(#el_req{gri = #gri{aspect = users}}, Harvester) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_user, Harvester)};
get(#el_req{gri = #gri{aspect = eff_users}}, Harvester) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_user, Harvester)};
get(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, Harvester) ->
    {ok, entity_graph:get_relation_attrs(direct, bottom_up, od_user, UserId, Harvester)};
get(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, Harvester) ->
    {ok, entity_graph:get_relation_attrs(effective, bottom_up, od_user, UserId, Harvester)};
get(#el_req{gri = #gri{aspect = {eff_user_membership, UserId}}}, Harvester) ->
    {ok, entity_graph:get_intermediaries(bottom_up, od_user, UserId, Harvester)};

get(#el_req{gri = #gri{aspect = groups}}, Harvester) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_group, Harvester)};
get(#el_req{gri = #gri{aspect = eff_groups}}, Harvester) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_group, Harvester)};
get(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, Harvester) ->
    {ok, entity_graph:get_relation_attrs(direct, bottom_up, od_group, GroupId, Harvester)};
get(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, Harvester) ->
    {ok, entity_graph:get_relation_attrs(effective, bottom_up, od_group, GroupId, Harvester)};
get(#el_req{gri = #gri{aspect = {eff_group_membership, GroupId}}}, Harvester) ->
    {ok, entity_graph:get_intermediaries(bottom_up, od_group, GroupId, Harvester)};

get(#el_req{gri = #gri{aspect = spaces}}, Harvester) ->
    {ok, entity_graph:get_relations(direct, top_down, od_space, Harvester)};

get(#el_req{gri = #gri{aspect = eff_providers}}, Harvester) ->
    {ok, entity_graph:get_relations(effective, top_down, od_provider, Harvester)};

get(#el_req{gri = #gri{aspect = indices}}, Harvester) ->
    {ok, maps:keys(Harvester#od_harvester.indices)};

get(#el_req{gri = #gri{aspect = {index, IndexId}, scope = private}}, Harvester) ->
    {ok, maps:get(IndexId, Harvester#od_harvester.indices)};

get(#el_req{gri = #gri{aspect = {index, IndexId}, scope = public}}, Harvester) ->
    #harvester_index{
        gui_plugin_name = GuiPluginName
    } = maps:get(IndexId, Harvester#od_harvester.indices),
    {ok, #{
        <<"guiPluginName">> => GuiPluginName
    }};

get(#el_req{gri = #gri{aspect = {index_stats, IndexId}}}, Harvester) ->
    #harvester_index{
        stats = Stats
    } = maps:get(IndexId, Harvester#od_harvester.indices),
    {ok, maps:map(fun(_SpaceId, Providers) ->
        maps:map(fun(_ProviderId, IndexStats) ->
            #index_stats{
                current_seq = CurrentSeq,
                max_seq = MaxSeq,
                last_update = LastUpdate,
                error = Error,
                archival = Archival
            } = IndexStats,
            #{
                <<"currentSeq">> => CurrentSeq,
                <<"maxSeq">> => MaxSeq,
                <<"lastUpdate">> => utils:undefined_to_null(LastUpdate),
                <<"error">> => utils:undefined_to_null(Error),
                <<"archival">> => Archival
            }
        end, Providers)
    end, Stats)}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().

update(#el_req{gri = #gri{id = HarvesterId, aspect = instance}, data = Data}) ->
    UpdateFun = fun(Harvester) ->
        #od_harvester{
            name = Name, endpoint = Endpoint,
            backend = HarvestingBackend, public = Public
        } = Harvester,

        NewName = maps:get(<<"name">>, Data, Name),
        NewEndpoint = maps:get(<<"harvestingBackendEndpoint">>, Data, Endpoint),
        NewHarvestingBackend = maps:get(<<"harvestingBackendType">>, Data, HarvestingBackend),
        NewPublic = maps:get(<<"public">>, Data, Public),

        NewHarvester = Harvester#od_harvester{
            name = NewName,
            backend = NewHarvestingBackend,
            public = NewPublic
        },

        case normalize_endpoint_and_check_connectivity(NewEndpoint, NewHarvestingBackend) of
            {ok, NormalizedEndpoint} ->
                {ok, NewHarvester#od_harvester{endpoint = NormalizedEndpoint}};
            {error, _} = Error ->
                Error
        end
    end,
    case od_harvester:update(HarvesterId, UpdateFun) of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end;

update(#el_req{gri = #gri{id = HarvesterId, aspect = gui_plugin_config}, data = Data}) ->
    {ok, _} = od_harvester:update(HarvesterId, fun(#od_harvester{gui_plugin_config = Config} = Harvester) ->
        NewConfig = maps:get(<<"guiPluginConfig">>, Data, Config),
        {ok, Harvester#od_harvester{gui_plugin_config = NewConfig}}
    end),
    ok;

update(#el_req{gri = #gri{id = HarvesterId, aspect = {index, IndexId}}, data = Data}) ->
    {ok, _} = od_harvester:update(HarvesterId, fun(#od_harvester{indices = Indices} = Harvester) ->
        Index = #harvester_index{
            name = Name,
            gui_plugin_name = GuiPluginName
        } = maps:get(IndexId, Indices),
        NewName = maps:get(<<"name">>, Data, Name),
        NewGuiPluginName = utils:null_to_undefined(maps:get(<<"guiPluginName">>, Data, GuiPluginName)),
        {ok, Harvester#od_harvester{indices = Indices#{IndexId => Index#harvester_index{
            name = NewName,
            gui_plugin_name = NewGuiPluginName
        }}}}
    end),
    ok;

update(Req = #el_req{gri = #gri{id = HarvesterId, aspect = {user_privileges, UserId}}}) ->
    PrivsToGrant = maps:get(<<"grant">>, Req#el_req.data, []),
    PrivsToRevoke = maps:get(<<"revoke">>, Req#el_req.data, []),
    entity_graph:update_relation(
        od_user, UserId,
        od_harvester, HarvesterId,
        {PrivsToGrant, PrivsToRevoke}
    );

update(Req = #el_req{gri = #gri{id = HarvesterId, aspect = {group_privileges, GroupId}}}) ->
    PrivsToGrant = maps:get(<<"grant">>, Req#el_req.data, []),
    PrivsToRevoke = maps:get(<<"revoke">>, Req#el_req.data, []),
    entity_graph:update_relation(
        od_group, GroupId,
        od_harvester, HarvesterId,
        {PrivsToGrant, PrivsToRevoke}
    ).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = HarvesterId, aspect = instance}}) ->
    entity_graph:delete_with_relations(od_harvester, HarvesterId);

delete(#el_req{gri = #gri{id = HarvesterId, aspect = {user, UserId}}}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_harvester, HarvesterId
    );

delete(#el_req{gri = #gri{id = HarvesterId, aspect = {group, GroupId}}}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_harvester, HarvesterId
    );

delete(#el_req{gri = #gri{id = HarvesterId, aspect = {space, SpaceId}}}) ->
    harvester_indices:update_stats(HarvesterId, all, fun(ExistingStats) ->
        harvester_indices:coalesce_index_stats(ExistingStats, SpaceId, true)
    end),
    entity_graph:remove_relation(
        od_harvester, HarvesterId,
        od_space, SpaceId
    );

delete(#el_req{gri = #gri{id = HarvesterId, aspect = {index, IndexId}}}) ->
    {ok, _} = od_harvester:update(HarvesterId, fun(#od_harvester{indices = Indices} = Harvester) ->
        {ok, Harvester#od_harvester{indices = maps:remove(IndexId, Indices)}}
    end),
    ok;

delete(#el_req{gri = #gri{aspect = metadata}}) ->
    fun(#od_harvester{indices = ExistingIndices, backend = HarvestingBackend, endpoint = Endpoint}) ->
        IndicesIds = maps:keys(ExistingIndices),
        lists:foreach(fun(IndexId) ->
            HarvestingBackend:delete_index(Endpoint, IndexId)
        end, IndicesIds)
    end;

delete(#el_req{gri = #gri{aspect = {index_metadata, IndexId}}}) ->
    fun(#od_harvester{backend = HarvestingBackend, endpoint = Endpoint}) ->
        HarvestingBackend:delete_index(Endpoint, IndexId)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(Req = #el_req{gri = #gri{aspect = Aspect}}, Harvester) when
    Aspect =:= instance;
    Aspect =:= data ->
    case Req#el_req.auth_hint of
        ?THROUGH_USER(UserId) ->
            harvester_logic:has_eff_user(Harvester, UserId);
        ?THROUGH_GROUP(GroupId) ->
            harvester_logic:has_eff_group(Harvester, GroupId);
        ?THROUGH_SPACE(SpaceId) ->
            harvester_logic:has_space(Harvester, SpaceId);
        undefined ->
            true
    end;

exists(#el_req{gri = #gri{aspect = {user, UserId}}}, Harvester) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, Harvester);

exists(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, Harvester) ->
    entity_graph:has_relation(direct, bottom_up, od_user, UserId, Harvester);

exists(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, Harvester) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Harvester);

exists(#el_req{gri = #gri{aspect = {eff_user_membership, UserId}}}, Harvester) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Harvester);

exists(#el_req{gri = #gri{aspect = {group, GroupId}}}, Harvester) ->
    entity_graph:has_relation(direct, bottom_up, od_group, GroupId, Harvester);

exists(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, Harvester) ->
    entity_graph:has_relation(direct, bottom_up, od_group, GroupId, Harvester);

exists(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, Harvester) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, Harvester);

exists(#el_req{gri = #gri{aspect = {eff_group_membership, GroupId}}}, Harvester) ->
    entity_graph:has_relation(effective, bottom_up, od_group, GroupId, Harvester);

exists(#el_req{gri = #gri{aspect = {space, SpaceId}}}, Harvester) ->
    entity_graph:has_relation(direct, top_down, od_space, SpaceId, Harvester);

exists(#el_req{gri = #gri{aspect = {index, IndexId}}}, Harvester) ->
    maps:is_key(IndexId, Harvester#od_harvester.indices);

exists(#el_req{gri = #gri{aspect = {index_stats, IndexId}}}, Harvester) ->
    maps:is_key(IndexId, Harvester#od_harvester.indices);

% All other aspects exist if harvester record exists.
exists(#el_req{gri = #gri{id = Id}}, #od_harvester{}) ->
    Id =/= undefined.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(Req = #el_req{operation = create, gri = #gri{id = undefined, aspect = instance, scope = private}}, _) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            user_logic_plugin:auth_by_oz_privilege(UserId, ?OZ_HARVESTERS_CREATE);

        {?USER(UserId), ?AS_GROUP(GroupId)} ->
            group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_HARVESTER) andalso
                user_logic_plugin:auth_by_oz_privilege(UserId, ?OZ_HARVESTERS_CREATE);

        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = join}}, _) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            true;
        {?USER(UserId), ?AS_GROUP(GroupId)} ->
            group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_HARVESTER);
        {?USER(UserId), ?AS_SPACE(SpaceId)} ->
            space_logic:has_eff_privilege(SpaceId, UserId, ?SPACE_ADD_HARVESTER);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, UserId}}, auth = ?USER(UserId), data = #{<<"privileges">> := _}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_USER) andalso auth_by_privilege(Req, Harvester, ?HARVESTER_SET_PRIVILEGES);
authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, UserId}}, auth = ?USER(UserId), data = _}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_USER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {group, GroupId}}, auth = ?USER(UserId), data = #{<<"privileges">> := _}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_GROUP) andalso
        auth_by_privilege(Req, Harvester, ?HARVESTER_SET_PRIVILEGES) andalso
        group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_HARVESTER);
authorize(Req = #el_req{operation = create, gri = #gri{aspect = {group, GroupId}}, auth = ?USER(UserId), data = _}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_GROUP) andalso
        group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_HARVESTER);
authorize(Req = #el_req{operation = create, gri = #gri{aspect = {space, SpaceId}}, auth = ?USER(UserId), data = _}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_SPACE) andalso
        space_logic:has_eff_privilege(SpaceId, UserId, ?SPACE_ADD_HARVESTER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_user_token}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_USER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_group_token}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_GROUP);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_space_token}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_SPACE);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = group}}, Harvester) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_GROUP);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = index}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_UPDATE);

authorize(#el_req{operation = create, gri = #gri{aspect = {submit_batch, SpaceId}}, auth = Auth}, Harvester) ->
    case Auth of
        ?PROVIDER(ProviderId) ->
            provider_logic:supports_space(ProviderId, SpaceId) andalso harvester_logic:has_space(Harvester, SpaceId);
        _Other ->
            false
    end;

authorize(#el_req{operation = create, gri = #gri{aspect = {query, _}}, auth = Auth}, Harvester) ->
    case Auth of
        ?USER(UserId) -> Harvester#od_harvester.public orelse
            entity_graph:has_relation(effective, bottom_up, od_user, UserId, Harvester);
        _ ->
            % client can be nobody
            Harvester#od_harvester.public
    end;

authorize(#el_req{operation = create, gri = #gri{aspect = {gen_curl_query, _}, scope = public}}, Harvester) ->
    Harvester#od_harvester.public;
authorize(#el_req{operation = create, gri = #gri{aspect = {gen_curl_query, _}, scope = private}, auth = ?USER(UserId)}, Harvester) ->
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Harvester);

authorize(#el_req{operation = get, gri = #gri{aspect = privileges}}, _) ->
    true;

authorize(#el_req{operation = get, auth = Auth, gri = #gri{aspect = instance, scope = private}}, Harvester) ->
    case Auth of
        ?USER(UserId) ->
            auth_by_privilege(UserId, Harvester, ?HARVESTER_VIEW);
        ?PROVIDER(ProviderId) ->
            lists:any(fun(SpaceId) -> provider_logic:supports_space(ProviderId, SpaceId) end,
                Harvester#od_harvester.spaces)
    end;

authorize(Req = #el_req{operation = get, gri = GRI = #gri{aspect = instance, scope = protected}}, Harvester) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(UserId), ?THROUGH_USER(UserId)} ->
            % User's membership in this harvester is checked in 'exists'
            true;

        {?USER(_UserId), ?THROUGH_USER(_OtherUserId)} ->
            false;

        {?USER(ClientUserId), ?THROUGH_GROUP(GroupId)} ->
            % Groups's membership in this harvester is checked in 'exists'
            group_logic:has_eff_privilege(GroupId, ClientUserId, ?GROUP_VIEW);

        {?USER(ClientUserId), _} ->
            harvester_logic:has_eff_user(Harvester, ClientUserId);

        _ ->
            % Access to private data also allows access to protected data
            authorize(Req#el_req{gri = GRI#gri{scope = private}}, Harvester)
    end;

authorize(Req = #el_req{operation = get, gri = GRI = #gri{aspect = instance, scope = shared}}, Harvester) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(ClientUserId), ?THROUGH_SPACE(SpaceId)} ->
            % Space's membership in this harvester is checked in 'exists'
            space_logic:has_eff_privilege(SpaceId, ClientUserId, ?SPACE_VIEW);

        _ ->
            % Access to private data also allows access to shared data
            authorize(Req#el_req{gri = GRI#gri{scope = protected}}, Harvester)
    end;

authorize(#el_req{operation = get, gri = #gri{aspect = instance, scope = public}}, Harvester) ->
    Harvester#od_harvester.public;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = gui_plugin_config}}, Harvester) ->
    Harvester#od_harvester.public orelse auth_by_privilege(Req, Harvester, ?HARVESTER_VIEW);

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {user_privileges, UserId}}}, _) ->
    true;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {user_privileges, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_VIEW_PRIVILEGES);

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {eff_user_privileges, UserId}}}, _) ->
    true;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_user_privileges, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_VIEW_PRIVILEGES);

authorize(#el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {eff_user_membership, UserId}}}, _) ->
    true;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_user_membership, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_VIEW);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {group_privileges, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_VIEW_PRIVILEGES);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_group_privileges, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_VIEW_PRIVILEGES);

authorize(Req = #el_req{operation = get, auth = ?USER(UserId), gri = #gri{aspect = {eff_group_membership, GroupId}}}, Harvester) ->
    group_logic:has_eff_user(GroupId, UserId) orelse auth_by_privilege(Req, Harvester, ?HARVESTER_VIEW);

authorize(#el_req{operation = get, gri = #gri{aspect = {index, _}, scope = public}}, Harvester) ->
    Harvester#od_harvester.public;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = indices}}, Harvester) ->
    Harvester#od_harvester.public orelse auth_by_privilege(Req, Harvester, ?HARVESTER_VIEW);

authorize(#el_req{operation = get, gri = #gri{aspect = all_backend_types}}, _) ->
    true;

authorize(Req = #el_req{operation = get, auth = ?USER}, Harvester) ->
    % All other resources can be accessed with view privileges
    auth_by_privilege(Req, Harvester, ?HARVESTER_VIEW);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = instance}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_UPDATE);
authorize(Req = #el_req{operation = update, gri = #gri{aspect = gui_plugin_config}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_UPDATE);
authorize(Req = #el_req{operation = update, gri = #gri{aspect = {index, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_UPDATE);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_SET_PRIVILEGES);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {group_privileges, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_SET_PRIVILEGES);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = instance}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_DELETE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {user, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_REMOVE_USER);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {group, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_REMOVE_GROUP);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {space, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_REMOVE_SPACE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {index, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_UPDATE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = metadata}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_DELETE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {index_metadata, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_DELETE);

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
        ?AS_USER(_) -> [?OZ_HARVESTERS_CREATE, ?OZ_USERS_ADD_RELATIONSHIPS];
        ?AS_GROUP(_) -> [?OZ_HARVESTERS_CREATE, ?OZ_GROUPS_ADD_RELATIONSHIPS];
        _ -> [?OZ_HARVESTERS_CREATE]
    end;

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = invite_user_token}}) ->
    [?OZ_HARVESTERS_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = invite_group_token}}) ->
    [?OZ_HARVESTERS_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = invite_space_token}}) ->
    [?OZ_HARVESTERS_ADD_RELATIONSHIPS];

required_admin_privileges(Req = #el_req{operation = create, gri = #gri{aspect = join}}) ->
    case Req#el_req.auth_hint of
        ?AS_USER(_) -> [?OZ_USERS_ADD_RELATIONSHIPS];
        ?AS_GROUP(_) -> [?OZ_GROUPS_ADD_RELATIONSHIPS];
        ?AS_SPACE(_) -> [?OZ_SPACES_ADD_RELATIONSHIPS]
    end;

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {user, _}}, data = #{<<"privileges">> := _}}) ->
    [?OZ_HARVESTERS_ADD_RELATIONSHIPS, ?OZ_HARVESTERS_SET_PRIVILEGES, ?OZ_USERS_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {user, _}}, data = _}) ->
    [?OZ_HARVESTERS_ADD_RELATIONSHIPS, ?OZ_USERS_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {group, _}}, data = #{<<"privileges">> := _}}) ->
    [?OZ_HARVESTERS_ADD_RELATIONSHIPS, ?OZ_HARVESTERS_SET_PRIVILEGES, ?OZ_GROUPS_ADD_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {group, _}}, data = _}) ->
    [?OZ_HARVESTERS_ADD_RELATIONSHIPS, ?OZ_GROUPS_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {space, _}}, data = _}) ->
    [?OZ_HARVESTERS_ADD_RELATIONSHIPS, ?OZ_SPACES_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = group}}) ->
    [?OZ_GROUPS_CREATE, ?OZ_HARVESTERS_ADD_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = index}}) ->
    [?OZ_HARVESTERS_UPDATE];

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = {query, _}}}) ->
    [?OZ_HARVESTERS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = list}}) ->
    [?OZ_HARVESTERS_LIST];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance, scope = protected}}) ->
    [?OZ_HARVESTERS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance, scope = shared}}) ->
    [?OZ_HARVESTERS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance, scope = public}}) ->
    [?OZ_HARVESTERS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = gui_plugin_config}}) ->
    [?OZ_HARVESTERS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {user_privileges, _}}}) ->
    [?OZ_HARVESTERS_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_user_privileges, _}}}) ->
    [?OZ_HARVESTERS_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_user_membership, _}}}) ->
    [?OZ_HARVESTERS_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {group_privileges, _}}}) ->
    [?OZ_HARVESTERS_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_group_privileges, _}}}) ->
    [?OZ_HARVESTERS_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {eff_group_membership, _}}}) ->
    [?OZ_HARVESTERS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = users}}) ->
    [?OZ_HARVESTERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_users}}) ->
    [?OZ_HARVESTERS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = groups}}) ->
    [?OZ_HARVESTERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_groups}}) ->
    [?OZ_HARVESTERS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = spaces}}) ->
    [?OZ_HARVESTERS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_providers}}) ->
    [?OZ_HARVESTERS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = indices}}) ->
    [?OZ_HARVESTERS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {index, _}, scope = private}}) ->
    [?OZ_HARVESTERS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {index_stats, _}}}) ->
    [?OZ_HARVESTERS_VIEW];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = instance}}) ->
    [?OZ_HARVESTERS_UPDATE];
required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = gui_plugin_config}}) ->
    [?OZ_HARVESTERS_UPDATE];
required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {index, _}}}) ->
    [?OZ_HARVESTERS_UPDATE];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) ->
    [?OZ_HARVESTERS_SET_PRIVILEGES];
required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = {group_privileges, _}}}) ->
    [?OZ_HARVESTERS_SET_PRIVILEGES];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = instance}}) ->
    [?OZ_HARVESTERS_DELETE];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {user, _}}}) ->
    [?OZ_HARVESTERS_REMOVE_RELATIONSHIPS, ?OZ_USERS_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {group, _}}}) ->
    [?OZ_HARVESTERS_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {space, _}}}) ->
    [?OZ_HARVESTERS_REMOVE_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {index, _}}}) ->
    [?OZ_HARVESTERS_UPDATE];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = metadata}}) ->
    [?OZ_HARVESTERS_DELETE];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {index_metadata, _}}}) ->
    [?OZ_HARVESTERS_DELETE];

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
validate(#el_req{operation = create, gri = #gri{aspect = instance}}) ->
    BackendValidityVerificator = #{
        <<"harvestingBackendType">> => {atom, ?HARVESTING_BACKENDS},
        <<"harvestingBackendEndpoint">> => {binary, non_empty}
    },
    {Required, Optional} = case lists:member(undefined, [
        oz_worker:get_env(default_harvesting_backend_type, undefined),
        oz_worker:get_env(default_harvesting_backend_endpoint, undefined)
    ]) of
        true -> {BackendValidityVerificator, #{}};
        false -> {#{}, BackendValidityVerificator}
    end,
    #{
        required => Required#{
            <<"name">> => {binary, name}
        },
        optional => Optional#{
            <<"guiPluginConfig">> => {json, any}
        }
    };

validate(#el_req{operation = create, gri = #gri{aspect = {submit_batch, _}}}) -> #{
    required => #{
        <<"maxSeq">> => {integer, {not_lower_than, 0}},
        <<"maxStreamSeq">> => {integer, {not_lower_than, 0}},
        % no need to check index membership - incorrect indices are later ignored
        <<"indices">> => {list_of_binaries, non_empty},
        <<"batch">> => {any, any}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = index}}) -> #{
    required => #{
        <<"name">> => {binary, name}
    },
    optional => #{
        <<"guiPluginName">> => {binary, any},
        <<"schema">> => {binary, non_empty},
        <<"includeMetadata">> => {list_of_atoms, fun(Vals) ->
            Key = <<"includeMetadata">>,
            Vals == [] andalso throw(?ERROR_BAD_VALUE_EMPTY(Key)),
            lists:all(fun(Val) -> lists:member(Val, od_harvester:all_metadata_types()) end, Vals)
                orelse throw(?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(Key, od_harvester:all_metadata_types())),
            true
        end},
        <<"includeFileDetails">> => {list_of_atoms, od_harvester:all_file_details()},
        <<"includeRejectionReason">> => {boolean, any},
        <<"retryOnRejection">> => {boolean, any}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {query, _}}}) ->
    fun(#od_harvester{backend = HarvestingBackend}) ->
        HarvestingBackend:query_validator()
    end;
validate(Req = #el_req{operation = create, gri = GRI = #gri{aspect = {gen_curl_query, IndexId}}}) ->
    validate(Req#el_req{gri = GRI#gri{aspect = {query, IndexId}}});

validate(Req = #el_req{operation = create, gri = #gri{aspect = join}}) ->
    #{
        required => #{
            <<"token">> => {invite_token, case Req#el_req.auth_hint of
                ?AS_USER(_) -> ?USER_JOIN_HARVESTER;
                ?AS_GROUP(_) -> ?GROUP_JOIN_HARVESTER;
                ?AS_SPACE(_) -> ?SPACE_JOIN_HARVESTER
            end}
        }
    };

validate(#el_req{operation = create, gri = #gri{aspect = invite_user_token}}) ->
    #{
    };

validate(#el_req{operation = create, gri = #gri{aspect = invite_group_token}}) ->
    #{
    };

validate(#el_req{operation = create, gri = #gri{aspect = invite_space_token}}) ->
    #{
    };

validate(#el_req{operation = create, gri = #gri{aspect = {user, _}}}) -> #{
    required => #{
        {aspect, <<"userId">>} => {any, {exists, fun(UserId) ->
            user_logic:exists(UserId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:harvester_privileges()}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {group, _}}}) -> #{
    required => #{
        {aspect, <<"groupId">>} => {any, {exists, fun(GroupId) ->
            group_logic:exists(GroupId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:harvester_privileges()}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {space, _}}}) -> #{
    required => #{
        {aspect, <<"spaceId">>} => {any, {exists, fun(SpaceId) ->
            space_logic:exists(SpaceId) end}
        }
    }
};

validate(Req = #el_req{operation = create, gri = #gri{aspect = group}}) ->
    group_logic_plugin:validate(Req#el_req{gri = #gri{
        type = od_group, id = undefined, aspect = instance
    }});

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    at_least_one => #{
        <<"name">> => {binary, name},
        <<"harvestingBackendType">> => {atom, ?HARVESTING_BACKENDS},
        <<"harvestingBackendEndpoint">> => {binary, non_empty},
        <<"public">> => {boolean, any}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = gui_plugin_config}}) -> #{
    required => #{
        <<"guiPluginConfig">> => {json, any}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = {index, _}}}) -> #{
    at_least_one => #{
        <<"name">> => {binary, name},
        <<"guiPluginName">> => {binary, any}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) ->
    #{
        at_least_one => #{
            <<"grant">> => {list_of_atoms, privileges:harvester_privileges()},
            <<"revoke">> => {list_of_atoms, privileges:harvester_privileges()}
        }
    };

validate(#el_req{operation = update, gri = #gri{aspect = {group_privileges, Id}}}) ->
    validate(#el_req{operation = update, gri = #gri{aspect = {user_privileges, Id}}}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns if given user has specific effective privilege in the harvester.
%% UserId is either given explicitly or derived from entity logic request.
%% Auths of type other than user are discarded.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_privilege(entity_logic:req() | od_user:id(),
    od_harvester:id() | od_harvester:record(), privileges:harvester_privilege()) -> boolean().
auth_by_privilege(#el_req{auth = ?USER(UserId)}, HarvesterOrId, Privilege) ->
    auth_by_privilege(UserId, HarvesterOrId, Privilege);
auth_by_privilege(#el_req{auth = _OtherAuth}, _HarvesterOrId, _Privilege) ->
    false;
auth_by_privilege(UserId, HarvesterOrId, Privilege) ->
    harvester_logic:has_eff_privilege(HarvesterOrId, UserId, Privilege).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes endpoint by removing trailing slash.
%% Checks whether resulting endpoint is responding.
%% @end
%%--------------------------------------------------------------------
-spec normalize_endpoint_and_check_connectivity(od_harvester:endpoint(), od_harvester:backend()) ->
    {ok, od_harvester:endpoint()} | {error, term()}.
normalize_endpoint_and_check_connectivity(Endpoint, HarvestingBackend) ->
    NormalizedEndpoint = re:replace(Endpoint, <<"\s|/$">>, <<"">>, [{return, binary}]),
    case HarvestingBackend:ping(NormalizedEndpoint) of
        ok -> {ok, NormalizedEndpoint};
        {error, _} = Error -> Error
    end.
