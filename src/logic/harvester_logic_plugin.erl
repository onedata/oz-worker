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

-include("tokens.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/api_errors.hrl").

-export([fetch_entity/1, operation_supported/3, is_subscribable/2]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, required_admin_privileges/1, validate/1]).

% Internal record used for calling harvester plugin.
-record(state, {
    harvester_id :: od_harvester:id() | undefined,
    index_id :: binary() | undefined,
    endpoint :: binary() | undefined,
    plugin :: atom() | undefined,
    file_id :: binary() | undefined,
    data = #{} :: maps:map()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity from datastore based on its EntityId.
%% Should return ?ERROR_NOT_FOUND if the entity does not exist.
%% @end
%%--------------------------------------------------------------------
-spec fetch_entity(entity_logic:entity_id()) ->
    {ok, entity_logic:entity()} | entity_logic:error().
fetch_entity(HarvesterId) ->
    case od_harvester:get(HarvesterId) of
        {ok, #document{value = Harvester}} ->
            {ok, Harvester};
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
operation_supported(create, {submit_entry, _}, private) -> true;
operation_supported(create, {delete_entry, _}, private) -> true;
operation_supported(create, {query, _}, private) -> true;

operation_supported(get, list, private) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, instance, protected) -> true;

operation_supported(get, all_plugins, private) -> true;
operation_supported(get, gui_plugin_config, private) -> true;

operation_supported(get, {index, _}, private) -> true;

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

operation_supported(get, indices, private) -> true;

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
is_subscribable(gui_plugin_config, private) -> true;
is_subscribable({index, _}, private) -> true;
is_subscribable(_, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(#el_req{gri = #gri{aspect = instance} = GRI, client = Client,
    auth_hint = AuthHint, data = Data}) ->
    #{
        <<"name">> := Name,
        <<"endpoint">> := Endpoint,
        <<"plugin">> := Plugin
    } = Data,
    Config = maps:get(<<"guiPluginConfig">>, Data, #{}),
    
    case call_plugin(ping, #state{plugin = Plugin, endpoint = Endpoint}) of
        ok -> ok;
        Error -> throw(Error)
    end,

    {ok, #document{key = HarvesterId}} = od_harvester:create(#document{
        value = #od_harvester{
            name = Name, 
            endpoint = Endpoint, 
            plugin = Plugin, 
            gui_plugin_config = Config,
            creator = Client
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
    {ok, Harvester} = fetch_entity(HarvesterId),
    {ok, resource, {GRI#gri{id = HarvesterId}, Harvester}};

create(Req = #el_req{gri = #gri{id = undefined, aspect = join}}) ->
    Macaroon = maps:get(<<"token">>, Req#el_req.data),
    % In the future, privileges can be included in token
    Privileges = privileges:harvester_user(),
    JoinHarvesterFun = fun(od_harvester, HarvesterId) ->
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
                    od_space, SpaceId,
                    od_harvester, HarvesterId
                );
            _ ->
                ok
        end,
        HarvesterId
    end,
    HarvesterId = token_logic:consume(Macaroon, JoinHarvesterFun),

    NewGRI = #gri{type = od_harvester, id = HarvesterId, aspect = instance,
        scope = case lists:member(?HARVESTER_VIEW, Privileges) of
            true -> private;
            false -> protected
        end
    },
    {ok, Harvester} = fetch_entity(HarvesterId),
    {ok, HarvesterData} = get(#el_req{gri = NewGRI}, Harvester),
    {ok, resource, {NewGRI, HarvesterData}};

create(Req = #el_req{gri = #gri{id = HarvesterId, aspect = invite_user_token}}) ->
    {ok, Macaroon} = token_logic:create(
        Req#el_req.client,
        ?HARVESTER_INVITE_USER_TOKEN,
        {od_harvester, HarvesterId}
    ),
    {ok, value, Macaroon};

create(Req = #el_req{gri = #gri{id = HarvesterId, aspect = invite_group_token}}) ->
    {ok, Macaroon} = token_logic:create(
        Req#el_req.client,
        ?HARVESTER_INVITE_GROUP_TOKEN,
        {od_harvester, HarvesterId}
    ),
    {ok, value, Macaroon};

create(Req = #el_req{gri = #gri{id = HarvesterId, aspect = invite_space_token}}) ->
    {ok, Macaroon} = token_logic:create(
        Req#el_req.client,
        ?HARVESTER_INVITE_SPACE_TOKEN,
        {od_harvester, HarvesterId}
    ),
    {ok, value, Macaroon};

create(#el_req{gri = #gri{id = HarvesterId, aspect = {user, UserId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:harvester_user()),
    entity_graph:add_relation(
        od_user, UserId,
        od_harvester, HarvesterId,
        Privileges
    ),
    NewGRI = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
    {ok, User} = user_logic_plugin:fetch_entity(UserId),
    {ok, UserData} = user_logic_plugin:get(#el_req{gri = NewGRI}, User),
    {ok, resource, {NewGRI, ?THROUGH_HARVESTER(HarvesterId), UserData}};

create(#el_req{gri = #gri{id = HarvesterId, aspect = {group, GroupId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:harvester_user()),
    entity_graph:add_relation(
        od_group, GroupId,
        od_harvester, HarvesterId,
        Privileges
    ),
    NewGRI = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
    {ok, Group} = group_logic_plugin:fetch_entity(GroupId),
    {ok, GroupData} = group_logic_plugin:get(#el_req{gri = NewGRI}, Group),
    {ok, resource, {NewGRI, ?THROUGH_GROUP(HarvesterId), GroupData}};

create(#el_req{gri = #gri{id = HarvesterId, aspect = {space, SpaceId}}}) ->
    entity_graph:add_relation(
        od_space, SpaceId,
        od_harvester, HarvesterId
    ),
    NewGRI = #gri{type = od_space, id = SpaceId, aspect = instance, scope = protected},
    {ok, Space} = space_logic_plugin:fetch_entity(SpaceId),
    {ok, SpaceData} = space_logic_plugin:get(#el_req{gri = NewGRI}, Space),
    {ok, resource, {NewGRI, ?THROUGH_GROUP(HarvesterId), SpaceData}};

create(Req = #el_req{gri = GRI = #gri{id = HarvesterId, aspect = group}}) ->
    % Create a new group for a user and add the group as a member of this harvester.
    {ok, resource, {NewGRI = #gri{id = GroupId}, _}} = group_logic_plugin:create(
        Req#el_req{gri = GRI#gri{type = od_group, id = undefined, aspect = instance}}
    ),
    Privileges = privileges:harvester_user(),
    entity_graph:add_relation(
        od_group, GroupId,
        od_harvester, HarvesterId,
        Privileges
    ),
    {ok, Group} = group_logic_plugin:fetch_entity(GroupId),
    {ok, resource, {NewGRI, Group}};

create(#el_req{gri = #gri{aspect = index, id = HarvesterId}, data = Data}) ->
    IndexId = datastore_utils:gen_key(),
    State = #state{
        harvester_id = HarvesterId,
        index_id = IndexId,
        data = Data
    },
    case perform_index_operation(create_index, State) of
        {ok, _} -> {ok, value, IndexId};
        {error, _} = Error -> Error
    end;
    
create(#el_req{client = ?PROVIDER(ProviderId), gri = #gri{aspect = {submit_entry, FileId}, id = HarvesterId}, data = Data}) ->
    #{
        <<"seq">> := Seq,
        <<"maxSeq">> := MaxSeq,
        <<"indices">> := SubmitIndices,
        <<"json">> := Json
    } = Data,
    fun(#od_harvester{plugin = Plugin, endpoint = Endpoint, indices = Indices}) ->
        State = #state{
            plugin = Plugin, endpoint = Endpoint,
            harvester_id = HarvesterId, file_id = FileId, 
            data = Json
        },
        {ok, FailedIndices} = perform_entry_operation(submit_entry, Indices, SubmitIndices, ProviderId, {Seq, MaxSeq}, State),
        {ok, value, #{<<"failedIndices">> => FailedIndices}}
    end;
    
create(#el_req{client = ?PROVIDER(ProviderId), gri = #gri{aspect = {delete_entry, FileId}, id = HarvesterId}, data = Data}) ->
    #{
        <<"seq">> := Seq,
        <<"maxSeq">> := MaxSeq,
        <<"indices">> := SubmitIndices
    } = Data,
    fun(#od_harvester{plugin = Plugin, endpoint = Endpoint, indices = Indices}) ->
        State = #state{
            plugin = Plugin, endpoint = Endpoint,
            harvester_id = HarvesterId, file_id = FileId
        },
        {ok, FailedIndices} = perform_entry_operation(delete_entry, Indices, SubmitIndices, ProviderId, {Seq, MaxSeq}, State),
        {ok, value, #{<<"failedIndices">> => FailedIndices}}
    end;

create(#el_req{gri = #gri{aspect = {query, IndexId}, id = HarvesterId}, data = Data}) ->
    fun(#od_harvester{plugin = Plugin, endpoint = Endpoint}) ->
        State = #state{
            harvester_id = HarvesterId, index_id = IndexId, 
            plugin = Plugin, endpoint = Endpoint, data = Data
        },
        case call_plugin(query, State) of
            {ok, Value} -> {ok, value, Value};
            {error, _} = Error -> Error
        end
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

get(#el_req{gri = #gri{aspect = instance, scope = private}}, Harvester) ->
    {ok, Harvester};
get(#el_req{gri = #gri{aspect = instance, scope = protected}}, Harvester) ->
    #od_harvester{
        name = Name, plugin = Plugin, endpoint = Endpoint, public = Public,
        indices = Indices, creator = Creator, creation_time = CreationTime
    } = Harvester,
    {ok, #{
        <<"name">> => Name,
        <<"public">> => Public,
        <<"plugin">> => Plugin,
        <<"endpoint">> => Endpoint,
        <<"indices">> => maps:keys(Indices),
        <<"creator">> => Creator,
        <<"creationTime">> => CreationTime
    }};

get(#el_req{gri = #gri{aspect = all_plugins}}, _) ->
    {ok, onezone_plugins:get_plugins(harvester_plugin)};
get(#el_req{gri = #gri{aspect = gui_plugin_config}}, #od_harvester{gui_plugin_config = Config}) ->
    {ok, Config};    

get(#el_req{gri = #gri{aspect = users}}, Harvester) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_user, Harvester)};
get(#el_req{gri = #gri{aspect = eff_users}}, Harvester) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_user, Harvester)};
get(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, Harvester) ->
    {ok, entity_graph:get_privileges(direct, bottom_up, od_user, UserId, Harvester)};
get(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, Harvester) ->
    {ok, entity_graph:get_privileges(effective, bottom_up, od_user, UserId, Harvester)};
get(#el_req{gri = #gri{aspect = {eff_user_membership, UserId}}}, Harvester) ->
    {ok, entity_graph:get_intermediaries(bottom_up, od_user, UserId, Harvester)};

get(#el_req{gri = #gri{aspect = groups}}, Harvester) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_group, Harvester)};
get(#el_req{gri = #gri{aspect = eff_groups}}, Harvester) ->
    {ok, entity_graph:get_relations(effective, bottom_up, od_group, Harvester)};
get(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, Harvester) ->
    {ok, entity_graph:get_privileges(direct, bottom_up, od_group, GroupId, Harvester)};
get(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, Harvester) ->
    {ok, entity_graph:get_privileges(effective, bottom_up, od_group, GroupId, Harvester)};
get(#el_req{gri = #gri{aspect = {eff_group_membership, GroupId}}}, Harvester) ->
    {ok, entity_graph:get_intermediaries(bottom_up, od_group, GroupId, Harvester)};

get(#el_req{gri = #gri{aspect = spaces}}, Harvester) ->
    {ok, entity_graph:get_relations(direct, bottom_up, od_space, Harvester)};

get(#el_req{gri = #gri{aspect = indices}}, Harvester) ->
    {ok, maps:keys(Harvester#od_harvester.indices)};

get(#el_req{gri = #gri{aspect = {index, IndexId}}}, Harvester) ->
    #harvester_index{
        name = Name,
        schema = Schema,
        guiPluginName = GuiPluginName
    } = maps:get(IndexId, Harvester#od_harvester.indices),
    {ok, #{
        <<"name">> => Name,
        <<"schema">> => Schema,
        <<"guiPluginName">> => GuiPluginName
    }}.

%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().

update(#el_req{gri = #gri{id = HarvesterId, aspect = instance}, data = Data}) ->
    case od_harvester:update(HarvesterId, fun(Harvester) ->
        #od_harvester{
            name = Name, endpoint = Endpoint, 
            plugin = Plugin, public = Public
        } = Harvester,
        
        NewName = maps:get(<<"name">>, Data, Name),
        NewEndpoint = maps:get(<<"endpoint">>, Data, Endpoint),
        NewPlugin = maps:get(<<"plugin">>, Data, Plugin),
        NewPublic = maps:get(<<"public">>, Data, Public),
        
    
        NewHarvester = Harvester#od_harvester{
            name = NewName,
            plugin = NewPlugin,
            public = NewPublic
        },
        case NewEndpoint of
            Endpoint -> 
                {ok, NewHarvester};
            _ ->
                case call_plugin(ping, state_from_harvester(Harvester)) of
                    ok -> {ok, NewHarvester#od_harvester{endpoint = NewEndpoint}}; 
                    {error, _} = Error -> Error
                end
        end
    end) of
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
    {ok, _} = od_harvester:update(HarvesterId, fun(#od_harvester{indices =  Indices} = Harvester) ->
        Index = #harvester_index{
            name = Name, 
            guiPluginName = GuiPluginName
        } = maps:get(IndexId, Indices),
        NewName = maps:get(<<"name">>, Data, Name),
        NewGuiPluginName = maps:get(<<"guiPluginName">>, Data, GuiPluginName),
        {ok, Harvester#od_harvester{indices = Indices#{IndexId => Index#harvester_index{
            name = NewName,
            guiPluginName = NewGuiPluginName
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
    entity_graph:remove_relation(
        od_space, SpaceId,
        od_harvester, HarvesterId
    );

delete(#el_req{gri = #gri{aspect = {index, IndexId}, id = HarvesterId}}) ->
    State = #state{
        harvester_id = HarvesterId,
        index_id = IndexId
    },
    case perform_index_operation(delete_index, State) of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(Req = #el_req{gri = #gri{aspect = instance}}, Harvester) ->
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
    entity_graph:has_relation(direct, bottom_up, od_space, SpaceId, Harvester);

exists(#el_req{gri = #gri{aspect = {index, IndexId}}}, Harvester) ->
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
authorize(Req = #el_req{operation = create, gri = #gri{id = undefined, aspect = instance}}, _) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            user_logic_plugin:auth_by_oz_privilege(UserId, ?OZ_HARVESTERS_CREATE);

        {?USER(UserId), ?AS_GROUP(GroupId)} ->
            group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_HARVESTER) andalso
                user_logic_plugin:auth_by_oz_privilege(UserId, ?OZ_HARVESTERS_CREATE);

        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = join}}, _) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            true;
        {?USER(UserId), ?AS_GROUP(GroupId)} ->
            group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_HARVESTER);
        {?USER(UserId), ?AS_SPACE(SpaceId)} ->
            space_logic:has_eff_privilege(SpaceId, UserId, ?SPACE_ADD_HARVESTER);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, UserId}}, client = ?USER(UserId), data = #{<<"privileges">> := _}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_USER) andalso auth_by_privilege(Req, Harvester, ?HARVESTER_SET_PRIVILEGES);
authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, UserId}}, client = ?USER(UserId), data = _}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_USER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {group, GroupId}}, client = ?USER(UserId), data = #{<<"privileges">> := _}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_GROUP) andalso
        auth_by_privilege(Req, Harvester, ?HARVESTER_SET_PRIVILEGES) andalso
        group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_HARVESTER);
authorize(Req = #el_req{operation = create, gri = #gri{aspect = {group, GroupId}}, client = ?USER(UserId), data = _}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_GROUP) andalso
        group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_HARVESTER);
authorize(Req = #el_req{operation = create, gri = #gri{aspect = {space, SpaceId}}, client = ?USER(UserId), data = _}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_SPACE) andalso
        space_logic:has_eff_privilege(SpaceId, UserId, ?SPACE_ADD_HARVESTER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_user_token}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_USER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_group_token}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_GROUP);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_space_token}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_SPACE);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = group}}, Harvester) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            auth_by_privilege(Req, Harvester, ?HARVESTER_ADD_GROUP);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = index}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_UPDATE);

authorize(#el_req{operation = create, gri = #gri{aspect = {submit_entry, FileId}}, client = Client}, Harvester) ->
    case Client of
        ?PROVIDER(ProviderId) ->
            {ok, Guid} = file_id:objectid_to_guid(FileId),
            SpaceId = file_id:guid_to_space_id(Guid),
            provider_logic:supports_space(ProviderId, SpaceId) and harvester_logic:has_space(Harvester, SpaceId);
        _Other ->
            false
    end;

authorize(#el_req{operation = create, gri = #gri{aspect = {delete_entry, FileId}}, client = Client}, Harvester) ->
    case Client of
        ?PROVIDER(ProviderId) ->
            {ok, Guid} = file_id:objectid_to_guid(FileId),
            SpaceId = file_id:guid_to_space_id(Guid),
            provider_logic:supports_space(ProviderId, SpaceId) and harvester_logic:has_space(Harvester, SpaceId);
        _Other ->
            false
    end;

authorize(#el_req{operation = create, gri = #gri{aspect = {query, _}}, client = ?USER(UserId)}, Harvester) ->
    Harvester#od_harvester.public orelse
    entity_graph:has_relation(effective, bottom_up, od_user, UserId, Harvester);

authorize(#el_req{operation = get, client = Client, gri = #gri{aspect = instance, scope = private}}, Harvester) ->
    case Client of
        ?USER(UserId) ->
            auth_by_privilege(UserId, Harvester, ?HARVESTER_VIEW);
        ?PROVIDER(ProviderId) ->
            lists:any(fun(SpaceId) -> provider_logic:supports_space(ProviderId, SpaceId) end,
                Harvester#od_harvester.spaces)
    end;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = instance, scope = protected}}, Harvester) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?THROUGH_USER(UserId)} ->
            % User's membership in this harvester is checked in 'exists'
            true;

        {?USER(_UserId), ?THROUGH_USER(_OtherUserId)} ->
            false;

        {?USER(ClientUserId), ?THROUGH_GROUP(GroupId)} ->
            % Groups's membership in this harvester is checked in 'exists'
            group_logic:has_eff_privilege(GroupId, ClientUserId, ?GROUP_VIEW);

        {?USER(ClientUserId), ?THROUGH_SPACE(SpaceId)} ->
            % Space's membership in this harvester is checked in 'exists'
            space_logic:has_eff_privilege(SpaceId, ClientUserId, ?SPACE_VIEW);

        {?USER(ClientUserId), _} ->
            harvester_logic:has_eff_user(Harvester, ClientUserId);

        _ ->
            % Access to private data also allows access to protected data
            authorize(Req#el_req{gri = #gri{scope = private}}, Harvester)
    end;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {user_privileges, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_VIEW_PRIVILEGES);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_user_privileges, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_VIEW_PRIVILEGES);

authorize(#el_req{operation = get, client = ?USER(UserId), gri = #gri{aspect = {eff_user_membership, UserId}}}, _) ->
    true;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_user_membership, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_VIEW);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {group_privileges, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_VIEW_PRIVILEGES);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = {eff_group_privileges, _}}}, Harvester) ->
    auth_by_privilege(Req, Harvester, ?HARVESTER_VIEW_PRIVILEGES);

authorize(Req = #el_req{operation = get, client = ?USER(UserId), gri = #gri{aspect = {eff_group_membership, GroupId}}}, Harvester) ->
    group_logic:has_eff_user(GroupId, UserId) orelse auth_by_privilege(Req, Harvester, ?HARVESTER_VIEW);

authorize(#el_req{operation = get, gri = #gri{aspect = all_plugins}}, _) ->
    true;

authorize(Req = #el_req{operation = get, client = ?USER}, Harvester) ->
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

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = list}}) ->
    [?OZ_HARVESTERS_LIST];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance, scope = protected}}) ->
    [?OZ_HARVESTERS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = gui_plugin_config}}) ->
    [?OZ_HARVESTERS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = {index, _}}}) ->
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

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = indices}}) ->
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
        <<"name">> => {binary, name},
        <<"endpoint">> => {binary, non_empty},
        <<"plugin">> => {atom, onezone_plugins:get_plugins(harvester_plugin)}
    },
    optional => #{
        <<"guiPluginConfig">> => {json, any}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {submit_entry, _}}}) -> #{
    required => #{
        <<"json">> => {binary, non_empty},
        <<"seq">> => {integer, {not_lower_than, 0}},
        <<"maxSeq">> => {integer, {not_lower_than, 0}},
        <<"indices">> => {list_of_binaries, non_empty}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {delete_entry, _}}}) -> #{
    required => #{
        <<"seq">> => {integer, {not_lower_than, 0}},
        <<"maxSeq">> => {integer, {not_lower_than, 0}},
        <<"indices">> => {list_of_binaries, non_empty}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = index}}) -> #{
    required => #{
        <<"name">> => {binary, name},
        <<"guiPluginName">> => {binary, name},
        <<"schema">> => {binary, non_empty}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {query, _}}}) ->
    fun(Harvester) ->
        call_plugin(query_validator, state_from_harvester(Harvester))
    end;

validate(Req = #el_req{operation = create, gri = #gri{aspect = join}}) ->
    TokenType = case Req#el_req.auth_hint of
        ?AS_USER(_) -> ?HARVESTER_INVITE_USER_TOKEN;
        ?AS_GROUP(_) -> ?HARVESTER_INVITE_GROUP_TOKEN;
        ?AS_SPACE(_) -> ?HARVESTER_INVITE_SPACE_TOKEN
    end,
    #{
        required => #{
            <<"token">> => {token, TokenType}
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
        <<"endpoint">> => {binary, non_empty},
        <<"plugin">> => {atom, onezone_plugins:get_plugins(harvester_plugin)},
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
        <<"guiPluginName">> => {binary, name}
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
%% Clients of type other than user are discarded.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_privilege(entity_logic:req() | od_user:id(),
    od_harvester:id() | od_harvester:info(), privileges:harvester_privilege()) -> boolean().
auth_by_privilege(#el_req{client = ?USER(UserId)}, HarvesterOrId, Privilege) ->
    auth_by_privilege(UserId, HarvesterOrId, Privilege);
auth_by_privilege(#el_req{client = _OtherClient}, _HarvesterOrId, _Privilege) ->
    false;
auth_by_privilege(UserId, HarvesterOrId, Privilege) ->
    harvester_logic:has_eff_privilege(HarvesterOrId, UserId, Privilege).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs given operation regarding harvester index by calling harvester plugin. 
%% Updates harvester record appropriately.
%% @end
%%--------------------------------------------------------------------
-spec perform_index_operation(Operation :: atom(), State :: #state{}) -> {ok, #document{}} | {error, term()}.
perform_index_operation(Operation, #state{harvester_id = HarvesterId, index_id = IndexId} = State) ->
    od_harvester:update(HarvesterId,
        fun(#od_harvester{indices = Indices} = Harvester) ->

            case call_plugin(Operation, state_from_harvester(State, Harvester)) of
                ok ->
                    NewIndices = update_indices(Operation, Indices, IndexId, State#state.data),
                    {ok, Harvester#od_harvester{indices = NewIndices}};
                {error, _} = Error ->
                    Error
            end
        end).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates given Indices appropriately for given Operation.
%% @end
%%--------------------------------------------------------------------
-spec update_indices(Operation :: atom(), Indices :: od_harvester:indices(), 
    IndexId :: od_harvester:index_id(), Data :: maps:map()) -> od_harvester:indices().
update_indices(create_index, Indices, IndexId, Data) ->
    Name = maps:get(<<"name">>, Data),
    Schema = maps:get(<<"schema">>, Data),
    GuiPluginName = maps:get(<<"guiPluginName">>, Data),
    Index = #harvester_index{
        name = Name,
        schema = Schema,
        guiPluginName = GuiPluginName,
        seqs = #{}
    },
    Indices#{IndexId => Index};
update_indices(delete_index, Indices, IndexId, _) ->
    maps:remove(IndexId, Indices).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs given operation regarding harvester entry by calling harvester plugin. 
%% Updates harvester record appropriately.
%% Returns lists of indices for which operation failed.
%% @end
%%--------------------------------------------------------------------
-spec perform_entry_operation(Operation :: atom(), Indices :: od_harvester:indices(), 
    SubmitIndices :: [od_harvester:index_id()], ProviderId :: od_provider:id(), 
    Seqs :: {integer(), integer()}, State :: #state{}) -> {ok, [od_harvester:index_id()]}.
perform_entry_operation(Operation, Indices, SubmitIndices, ProviderId, Seqs, State) ->
    #state{file_id = FileId, harvester_id = HarvesterId} = State,
    
    IndicesToUpdate = lists:filtermap(
        fun(Index) ->
            case maps:find(Index, Indices) of
                {ok, _} ->
                    case call_plugin(Operation, State#state{index_id = Index}) of
                        ok -> {true, {Index, true}};
                        _ -> {true, {Index, false}}
                    end;
                _ -> false
            end
        end, SubmitIndices),

    {ok, Guid} = file_id:objectid_to_guid(FileId),
    SpaceId = file_id:guid_to_space_id(Guid),

    {ok, _} = od_harvester:update(HarvesterId,
        fun(#od_harvester{indices = Indices} = Harvester) ->
            FinalIndices = lists:foldl(
                fun({IndexId, UpdateCurrentSeq}, ExistingIndices) ->
                    #{IndexId := IndexRecord} = ExistingIndices,
                    ExistingIndices#{
                        IndexId => set_seqs(IndexRecord, SpaceId, ProviderId, UpdateCurrentSeq, Seqs)
                    }
                end, Indices, IndicesToUpdate),
            {ok, Harvester#od_harvester{indices = FinalIndices}}
        end),
    
    {ok, [I || {I, false} <- IndicesToUpdate]}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates max sequence number for given index record.
%% When UpdateCurrentSeq flag is set to true value of current sequence number will also be updated.
%% Sequence numbers are stored per space per provider
%% @end
%%--------------------------------------------------------------------
-spec set_seqs(#harvester_index{}, od_space:id(), od_provider:id(), boolean(), {integer(), integer()}) -> 
    #harvester_index{}.
set_seqs(IndexRecord, SpaceId, ProviderId, UpdateCurrentSeq, {NewSeq, NewMaxSeq}) -> 
    SeqsPerSpace = IndexRecord#harvester_index.seqs,
    SeqsPerProvider = maps:get(SpaceId, SeqsPerSpace, #{}),
    {CurrentSeq, CurrentMaxSeq} = maps:get(ProviderId, SeqsPerProvider, {0,0}),
    
    NewCurrentSeq = case UpdateCurrentSeq of
        true-> max(NewSeq, CurrentSeq);
        false -> CurrentSeq
    end, 
    
    IndexRecord#harvester_index{
        seqs = SeqsPerSpace#{
            SpaceId => SeqsPerProvider#{
                ProviderId => {NewCurrentSeq, max(CurrentMaxSeq, NewMaxSeq)}
            }
        }
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs given operation on proper harvester plugin.
%% @end
%%--------------------------------------------------------------------
-spec call_plugin(Operation :: atom(), State :: #state{}) -> term().
call_plugin(ping, State) ->
    #state{
        plugin = Plugin, endpoint = Endpoint
    } = State,
    Plugin:ping(Endpoint);
call_plugin(submit_entry, State) ->
    #state{
        plugin = Plugin, endpoint = Endpoint, harvester_id = HarvesterId, 
        index_id = IndexId, file_id = FileId, data = Data
    } = State,
    Plugin:submit_entry(Endpoint, HarvesterId, IndexId, FileId, Data);
call_plugin(delete_entry, State) ->
    #state{
        plugin = Plugin, endpoint = Endpoint, harvester_id = HarvesterId,
        index_id = IndexId, file_id = FileId
    } = State,
    Plugin:delete_entry(Endpoint, HarvesterId, IndexId, FileId);
call_plugin(create_index, State) ->
    #state{
        plugin = Plugin, endpoint = Endpoint, harvester_id = HarvesterId,
        index_id = IndexId, data = #{<<"schema">> := Schema}
    } = State,
    Plugin:create_index(Endpoint, HarvesterId, IndexId, Schema);
call_plugin(delete_index, State) ->
    #state{
        plugin = Plugin, endpoint = Endpoint, harvester_id = HarvesterId,
        index_id = IndexId
    } = State,
    Plugin:delete_index(Endpoint, HarvesterId, IndexId);
call_plugin(query, State) ->
    #state{
        plugin = Plugin, endpoint = Endpoint, harvester_id = HarvesterId,
        index_id = IndexId, data = Data
    } = State,
    Plugin:query(Endpoint, HarvesterId, IndexId, Data);
call_plugin(query_validator, #state{plugin = Plugin}) -> 
    Plugin:query_validator().


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves values from harvester record and returns them in state record.
%% @end
%%--------------------------------------------------------------------
-spec state_from_harvester(#od_harvester{}) -> #state{}.
state_from_harvester(Harvester) ->
    state_from_harvester(#state{}, Harvester).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Updates given state record with values retrieved from harvester record.
%% @end
%%--------------------------------------------------------------------
-spec state_from_harvester(#state{}, #od_harvester{}) -> #state{}.
state_from_harvester(State, #od_harvester{plugin = Plugin, endpoint = Endpoint}) ->
    State#state{plugin = Plugin, endpoint = Endpoint}.
