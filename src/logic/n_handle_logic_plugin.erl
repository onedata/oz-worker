%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc

%%% @end
%%%-------------------------------------------------------------------
-module(n_handle_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("errors.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").


-export([get_entity/1, create/4, get/4, update/3, delete/2]).
-export([exists/2, authorize/4, validate/2]).
-export([entity_to_string/1]).


get_entity(HandleId) ->
    case od_handle:get(HandleId) of
        {ok, #document{value = Handle}} ->
            {ok, Handle};
        _ ->
            ?ERROR_NOT_FOUND
    end.


% TODO VFS-2918
create(_Client, HandleId, {deprecated_user_privileges, UserId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_user, UserId,
        od_handle, HandleId,
        {Operation, Privileges}
    );
% TODO VFS-2918
create(_Client, HandleId, {deprecated_child_privileges, GroupId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_group, GroupId,
        od_handle, HandleId,
        {Operation, Privileges}
    );


create(Client, _, entity, Data) ->
    HandleServiceId = maps:get(<<"handleServiceId">>, Data),
    ResourceType = maps:get(<<"resourceType">>, Data),
    ResourceId = maps:get(<<"resourceId">>, Data),
    Metadata = maps:get(<<"metadata">>, Data),
    {ok, PublicHandle} = handle_proxy:register_handle(
        HandleServiceId, ResourceType, ResourceId, Metadata
    ),
    Handle = #document{value = #od_handle{
        handle_service = HandleServiceId,
        resource_type = ResourceType,
        resource_id = ResourceId,
        public_handle = PublicHandle,
        metadata = Metadata
    }},
    {ok, HandleId} = od_handle:create(Handle),
    entity_graph:add_relation(
        od_handle, HandleId,
        od_handle_service, HandleServiceId,
        privileges:handle_admin()
    ),
    case Client of
        ?USER(UserId) ->
            entity_graph:add_relation(
                od_user, UserId,
                od_handle, HandleId,
                privileges:handle_admin()
            );
        _ ->
            ok
    end,
    case ResourceType of
        <<"Share">> ->
            entity_graph:add_relation(
                od_handle, HandleId,
                od_share, ResourceId
            );
        _ ->
            ok
    end,
    {ok, HandleId};
create(?USER, HandleId, users, #{<<"userId">> := UserId}) ->
    entity_graph:add_relation(
        od_user, UserId,
        od_handle, HandleId,
        privileges:handle_user()
    ),
    {ok, HandleId};
create(?USER, HandleId, groups, #{<<"groupId">> := GroupId}) ->
    entity_graph:add_relation(
        od_group, GroupId,
        od_handle, HandleId,
        privileges:handle_user()
    ),
    {ok, HandleId}.


get(_, undefined, undefined, list) ->
    {ok, HandleDocs} = od_handle:list(),
    {ok, [HandleId || #document{key = HandleId} <- HandleDocs]};

get(_, _HandleId, #od_handle{} = Handle, data) ->
    #od_handle{handle_service = HandleService, public_handle = Handle,
        resource_type = ResourceType, resource_id = ResourceId,
        metadata = Metadata, timestamp = Timestamp
    } = Handle,
    {ok, #{
        <<"handleServiceId">> => HandleService,
        <<"handle">> => Handle,
        <<"resourceType">> => ResourceType,
        <<"resourceId">> => ResourceId,
        <<"metadata">> => Metadata,
        <<"timestamp">> => Timestamp
    }};

get(_, _HandleId, #od_handle{} = Handle, public_data) ->
    #od_handle{
        public_handle = Handle, metadata = Metadata, timestamp = Timestamp
    } = Handle,
    {ok, #{
        <<"handle">> => Handle,
        <<"metadata">> => Metadata,
        <<"timestamp">> => Timestamp
    }};

get(_, _HandleId, #od_handle{users = Users}, users) ->
    {ok, Users};
get(_, _HandleId, #od_handle{eff_users = Users}, eff_users) ->
    {ok, Users};
get(_, _HandleId, #od_handle{}, {user, UserId}) ->
    {ok, User} = ?throw_on_failure(n_user_logic_plugin:get_entity(UserId)),
    n_user_logic_plugin:get(?ROOT, UserId, User, data);
get(_, _HandleId, #od_handle{}, {eff_user, UserId}) ->
    {ok, User} = ?throw_on_failure(n_user_logic_plugin:get_entity(UserId)),
    n_user_logic_plugin:get(?ROOT, UserId, User, data);
get(_, _HandleId, #od_handle{users = Users}, {user_privileges, UserId}) ->
    {ok, maps:get(UserId, Users)};
get(_, _HandleId, #od_handle{eff_users = Users}, {eff_user_privileges, UserId}) ->
    {Privileges, _} = maps:get(UserId, Users),
    {ok, Privileges};

get(_, _HandleId, #od_handle{groups = Groups}, groups) ->
    {ok, Groups};
get(_, _HandleId, #od_handle{eff_groups = Groups}, eff_groups) ->
    {ok, Groups};
get(_, _HandleId, #od_handle{}, {group, GroupId}) ->
    {ok, Group} = ?throw_on_failure(n_group_logic_plugin:get_entity(GroupId)),
    n_group_logic_plugin:get(?ROOT, GroupId, Group, data);
get(_, _HandleId, #od_handle{}, {eff_group, GroupId}) ->
    {ok, Group} = ?throw_on_failure(n_group_logic_plugin:get_entity(GroupId)),
    n_group_logic_plugin:get(?ROOT, GroupId, Group, data);
get(_, _HandleId, #od_handle{groups = Groups}, {group_privileges, GroupId}) ->
    {ok, maps:get(GroupId, Groups)};
get(_, _HandleId, #od_handle{eff_groups = Groups}, {eff_group_privileges, GroupId}) ->
    {Privileges, _} = maps:get(GroupId, Groups),
    {ok, Privileges}.


update(HandleId, entity, #{<<"metadata">> := NewMetadata}) ->
    {ok, _} = od_handle:update(HandleId, #{metadata => NewMetadata}),
    ok;

update(HandleId, {user_privileges, UserId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_user, UserId,
        od_handle, HandleId,
        {Operation, Privileges}
    );

update(HandleId, {group_privileges, GroupId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_group, GroupId,
        od_handle, HandleId,
        {Operation, Privileges}
    ).


delete(HandleId, entity) ->
    entity_graph:delete_with_relations(od_handle, HandleId);

delete(HandleId, {user, UserId}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_handle, HandleId
    );

delete(HandleId, {group, GroupId}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_handle, HandleId
    ).


exists(undefined, _) ->
    true;

exists(_HandleId, {user, UserId}) ->
    {internal, fun(#od_handle{users = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists(_HandleId, {eff_user, UserId}) ->
    {internal, fun(#od_handle{eff_users = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists(_HandleId, {user_privileges, UserId}) ->
    {internal, fun(#od_handle{users = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists(_HandleId, {eff_user_privileges, UserId}) ->
    {internal, fun(#od_handle{eff_users = Users}) ->
        maps:is_key(UserId, Users)
    end};

exists(_HandleId, {group, UserId}) ->
    {internal, fun(#od_handle{groups = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists(_HandleId, {eff_group, UserId}) ->
    {internal, fun(#od_handle{eff_groups = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists(_HandleId, {group_privileges, UserId}) ->
    {internal, fun(#od_handle{groups = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists(_HandleId, {eff_group_privileges, UserId}) ->
    {internal, fun(#od_handle{eff_groups = Users}) ->
        maps:is_key(UserId, Users)
    end};

exists(_HandleId, _) ->
    % No matter the resource, return true if it belongs to a handle
    {internal, fun(#od_handle{}) ->
        % If the handle with HandleId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.



% TODO VFS-2918
authorize(update, _GroupId, {deprecated_user_privileges, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE);
% TODO VFS-2918
authorize(update, _GroupId, {deprecated_child_privileges, _ChildGroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE);

authorize(create, undefined, entity, ?USER(UserId)) ->
    {data_dependent, fun(Data) ->
        HServiceId = maps:get(<<"handleServiceId">>, Data, <<"">>),
        n_handle_service_logic:user_has_eff_privilege(
            HServiceId, UserId, ?HANDLE_SERVICE_REGISTER_HANDLE
        )
    end};

authorize(create, _HandleId, users, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE);

authorize(create, _HandleId, groups, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE);


authorize(get, _HandleId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_VIEW);

authorize(get, _HandleId, data, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_VIEW);

authorize(get, _HandleId, public_data, _Client) ->
    % Public data is available to whomever has handle id.
    true;

authorize(get, undefined, list, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_HANDLES_LIST);

authorize(get, _HandleId, _, ?USER(UserId)) ->
    % All other resources can be accessed with view privileges
    auth_by_privilege(UserId, ?HANDLE_VIEW);


authorize(update, _HandleId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE);

authorize(update, _HandleId, {user_privileges, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE);

authorize(update, _HandleId, {group_privileges, _GroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE);


authorize(delete, _HandleId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_DELETE);

authorize(delete, _HandleId, {user, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE);

authorize(delete, _HandleId, {group, _GroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?HANDLE_UPDATE).


validate(create, entity) -> #{
    required => #{
        <<"handleServiceId">> => {binary, {exists, fun(Value) ->
            n_handle_service_logic:exists(Value)
        end}},
        <<"resourceType">> => {binary, [<<"Share">>]},
        <<"resourceId">> => {binary, {exists, fun(Value) ->
            n_share_logic:exists(Value) end
        }},
        <<"metadata">> => {binary, non_empty}
    }
};
validate(create, users) -> #{
    required => #{
        <<"userId">> => {binary, {exists, fun(Value) ->
            n_user_logic:exists(Value)
        end}}
    }
};
validate(create, groups) -> #{
    required => #{
        <<"groupId">> => {binary, {exists, fun(Value) ->
            n_group_logic:exists(Value)
        end}}
    }
};
validate(update, entity) -> #{
    required => #{
        <<"metadata">> => {binary, non_empty}
    }
}.


entity_to_string(HandleId) ->
    od_handle:to_string(HandleId).



auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_handle{} = Handle) ->
        n_handle_logic:user_has_eff_privilege(Handle, UserId, Privilege)
    end}.
