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
-module(n_group_logic_plugin).
-author("Lukasz Opiola").
-behaviour(data_logicn_plugin_behaviour).

-include("errors.hrl").
-include("tokens.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").


-export([get_entity/1, create/4, get/4, update/3, delete/2]).
-export([exists/2, authorize/4, validate/2]).
-export([entity_to_string/1]).


get_entity(GroupId) ->
    case od_group:get(GroupId) of
        {ok, #document{value = Group}} ->
            {ok, Group};
        _ ->
            ?ERROR_NOT_FOUND
    end.

% TODO VFS-2918
create(_Client, GroupId, {deprecated_user_privileges, UserId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_user, UserId,
        od_group, GroupId,
        {Operation, Privileges}
    );
% TODO VFS-2918
create(_Client, ParentGroupId, {deprecated_child_privileges, ChildGroupId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_group, ChildGroupId,
        od_group, ParentGroupId,
        {Operation, Privileges}
    );

create(Client, _, entity, Data) ->
    Name = maps:get(<<"name">>, Data),
    Type = maps:get(<<"type">>, Data, role),
    {ok, GroupId} = od_group:create(
        #document{value = #od_group{name = Name, type = Type}}
    ),
    case Client of
        ?USER(UserId) ->
            entity_graph:add_relation(
                od_user, UserId,
                od_group, GroupId,
                privileges:group_admin()
            );
        _ ->
            ok
    end,
    {ok, GroupId};

create(Client, GroupId, invite_user_token, _) ->
    {ok, Token} = token_logic:create(
        Client,
        ?GROUP_INVITE_USER_TOKEN,
        {od_group, GroupId}
    ),
    {ok, Token};

create(Client, GroupId, invite_group_token, _) ->
    {ok, Token} = token_logic:create(
        Client,
        ?GROUP_INVITE_GROUP_TOKEN,
        {od_group, GroupId}
    ),
    {ok, Token};

create(_Client, GroupId, create_space, Data) ->
    {ok, SpaceId} = n_space_logic:create(?ROOT, Data),
    entity_graph:add_relation(
        od_group, GroupId,
        od_space, SpaceId,
        privileges:space_admin()
    ),
    {ok, SpaceId};

create(_Client, GroupId, create_handle, Data) ->
    {ok, HandleId} = n_handle_logic:create(?ROOT, Data),
    entity_graph:add_relation(
        od_group, GroupId,
        od_handle, HandleId,
        privileges:handle_admin()
    ),
    {ok, HandleId};

create(_Client, GroupId, create_handle_service, Data) ->
    {ok, HServiceId} = n_handle_service_logic:create(?ROOT, Data),
    entity_graph:add_relation(
        od_group, GroupId,
        od_handle_service, HServiceId,
        privileges:handle_service_admin()
    ),
    {ok, HServiceId};

create(_Client, GroupId, join_group, #{<<"token">> := Macaroon}) ->
    {ok, {od_group, ParentGroupId}} = token_logic:consume(Macaroon),
    entity_graph:add_relation(
        od_group, GroupId,
        od_group, ParentGroupId,
        privileges:group_user()
    ),
    {ok, ParentGroupId};

create(_Client, GroupId, join_space, #{<<"token">> := Macaroon}) ->
    {ok, {od_space, SpaceId}} = token_logic:consume(Macaroon),
    entity_graph:add_relation(
        od_group, GroupId,
        od_space, SpaceId,
        privileges:space_user()
    ),
    {ok, SpaceId};

create(_Client, GroupId, users, #{<<"userId">> := UserId}) ->
    entity_graph:add_relation(
        od_user, UserId,
        od_group, GroupId,
        privileges:group_user()
    ),
    {ok, UserId};

create(_Client, GroupId, children, #{<<"groupId">> := ChildGroupId}) ->
    entity_graph:add_relation(
        od_group, ChildGroupId,
        od_group, GroupId,
        privileges:group_user()
    ),
    {ok, ChildGroupId}.




% TODO VFS-2918
get(Client, GroupId, _, deprecated_invite_user_token) ->
    {ok, Token} = token_logic:create(
        Client,
        ?GROUP_INVITE_USER_TOKEN,
        {od_group, GroupId}
    ),
    {ok, Token};
% TODO VFS-2918
get(Client, GroupId, _, deprecated_invite_group_token) ->
    {ok, Token} = token_logic:create(
        Client,
        ?GROUP_INVITE_GROUP_TOKEN,
        {od_group, GroupId}
    ),
    {ok, Token};

get(_, undefined, undefined, list) ->
    {ok, GroupDocs} = od_group:list(),
    {ok, [GroupId || #document{key = GroupId} <- GroupDocs]};
get(_, _GroupId, #od_group{name = Name, type = Type}, data) ->
    {ok, #{
        <<"name">> => Name,
        <<"type">> => Type
    }};
get(_, _GroupId, #od_group{oz_privileges = OzPrivileges}, oz_privileges) ->
    {ok, OzPrivileges};
get(_, _GroupId, #od_group{eff_oz_privileges = OzPrivileges}, eff_oz_privileges) ->
    {ok, OzPrivileges};

get(_, _GroupId, #od_group{users = Users}, users) ->
    {ok, Users};
get(_, _GroupId, #od_group{eff_users = Users}, eff_users) ->
    {ok, Users};
get(_, _GroupId, #od_group{}, {user, UserId}) ->
    {ok, User} = ?throw_on_failure(n_user_logic_plugin:get_entity(UserId)),
    n_user_logic_plugin:get(?ROOT, UserId, User, data);
get(_, _GroupId, #od_group{}, {eff_user, UserId}) ->
    {ok, User} = ?throw_on_failure(n_user_logic_plugin:get_entity(UserId)),
    n_user_logic_plugin:get(?ROOT, UserId, User, data);
get(_, _GroupId, #od_group{users = Users}, {user_privileges, UserId}) ->
    {ok, maps:get(UserId, Users)};
get(_, _GroupId, #od_group{eff_users = Users}, {eff_user_privileges, UserId}) ->
    {Privileges, _} = maps:get(UserId, Users),
    {ok, Privileges};

get(_, _GroupId, #od_group{parents = Parents}, parents) ->
    {ok, Parents};
get(_, _GroupId, #od_group{eff_parents = Parents}, eff_parents) ->
    {ok, Parents};
get(_, _GroupId, #od_group{}, {parent, ParentId}) ->
    {ok, Parent} = ?throw_on_failure(n_group_logic_plugin:get_entity(ParentId)),
    n_group_logic_plugin:get(?ROOT, ParentId, Parent, data);
get(_, _GroupId, #od_group{}, {eff_parent, ParentId}) ->
    {ok, Parent} = ?throw_on_failure(n_group_logic_plugin:get_entity(ParentId)),
    n_group_logic_plugin:get(?ROOT, ParentId, Parent, data);

get(_, _GroupId, #od_group{children = Children}, children) ->
    {ok, Children};
get(_, _GroupId, #od_group{eff_children = Children}, eff_children) ->
    {ok, Children};
get(_, _GroupId, #od_group{}, {child, ChildId}) ->
    {ok, Child} = ?throw_on_failure(n_group_logic_plugin:get_entity(ChildId)),
    n_group_logic_plugin:get(?ROOT, ChildId, Child, data);
get(_, _GroupId, #od_group{}, {eff_child, ChildId}) ->
    {ok, Child} = ?throw_on_failure(n_group_logic_plugin:get_entity(ChildId)),
    n_group_logic_plugin:get(?ROOT, ChildId, Child, data);
get(_, _GroupId, #od_group{children = Children}, {child_privileges, ChildId}) ->
    {ok, maps:get(ChildId, Children)};
get(_, _GroupId, #od_group{eff_children = Children}, {eff_child_privileges, ChildId}) ->
    {Privileges, _} = maps:get(ChildId, Children),
    {ok, Privileges};

get(_, _GroupId, #od_group{spaces = Spaces}, spaces) ->
    {ok, Spaces};
get(_, _GroupId, #od_group{eff_spaces = Spaces}, eff_spaces) ->
    {ok, Spaces};
get(_, _GroupId, #od_group{}, {space, SpaceId}) ->
    {ok, Space} = ?throw_on_failure(n_space_logic_plugin:get_entity(SpaceId)),
    n_space_logic_plugin:get(?ROOT, SpaceId, Space, data);
get(_, _GroupId, #od_group{}, {eff_space, SpaceId}) ->
    {ok, Space} = ?throw_on_failure(n_space_logic_plugin:get_entity(SpaceId)),
    n_space_logic_plugin:get(?ROOT, SpaceId, Space, data);

get(_, _GroupId, #od_group{eff_providers = Providers}, eff_providers) ->
    {ok, Providers};
get(_, _GroupId, #od_group{}, {eff_provider, ProviderId}) ->
    {ok, Provider} = ?throw_on_failure(n_provider_logic_plugin:get_entity(ProviderId)),
    n_provider_logic_plugin:get(?ROOT, ProviderId, Provider, data);

get(_, _GroupId, #od_group{handle_services = HandleServices}, handle_services) ->
    {ok, HandleServices};
get(_, _GroupId, #od_group{eff_handle_services = HandleServices}, eff_handle_services) ->
    {ok, HandleServices};
get(_, _GroupId, #od_group{}, {handle_service, HServiceId}) ->
    {ok, HService} = ?throw_on_failure(n_handle_service_logic_plugin:get_entity(HServiceId)),
    n_handle_service_logic_plugin:get(?ROOT, HServiceId, HService, data);
get(_, _GroupId, #od_group{}, {eff_handle_service, HServiceId}) ->
    {ok, HService} = ?throw_on_failure(n_handle_service_logic_plugin:get_entity(HServiceId)),
    n_handle_service_logic_plugin:get(?ROOT, HServiceId, HService, data);

get(_, _GroupId, #od_group{handles = Handles}, handles) ->
    {ok, Handles};
get(_, _GroupId, #od_group{eff_handles = Handles}, eff_handles) ->
    {ok, Handles};
get(_, _GroupId, #od_group{}, {handle, HandleId}) ->
    {ok, Handle} = ?throw_on_failure(n_handle_logic_plugin:get_entity(HandleId)),
    n_handle_logic_plugin:get(?ROOT, HandleId, Handle, data);
get(_, _GroupId, #od_group{}, {eff_handle, HandleId}) ->
    {ok, Handle} = ?throw_on_failure(n_handle_logic_plugin:get_entity(HandleId)),
    n_handle_logic_plugin:get(?ROOT, HandleId, Handle, data).



update(GroupId, entity, Data) ->
    {ok, _} = od_group:update(GroupId, fun(Group) ->
        #od_group{name = OldName, type = OldType} = Group,
        NewName = maps:get(<<"name">>, Data, OldName),
        NewType = maps:get(<<"type">>, Data, OldType),
        {ok, Group#od_group{name = NewName, type = NewType}}
    end),
    ok;

update(GroupId, oz_privileges, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_oz_privileges(od_group, GroupId, Operation, Privileges);

update(GroupId, {user_privileges, UserId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_user, UserId,
        od_group, GroupId,
        {Operation, Privileges}
    );

update(ParentGroupId, {child_privileges, ChildGroupId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_group, ChildGroupId,
        od_group, ParentGroupId,
        {Operation, Privileges}
    ).


delete(GroupId, entity) ->
    entity_graph:delete_with_relations(od_group, GroupId);

delete(GroupId, oz_privileges) ->
    update(GroupId, oz_privileges, #{
        <<"operation">> => set, <<"privileges">> => []}
    );

delete(GroupId, {user, UserId}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_group, GroupId
    );

delete(ChildGroupId, {parent, ParentGroupId}) ->
    entity_graph:remove_relation(
        od_group, ChildGroupId,
        od_group, ParentGroupId
    );

delete(ParentGroupId, {child, ChildGroupId}) ->
    entity_graph:remove_relation(
        od_group, ChildGroupId,
        od_group, ParentGroupId
    );

delete(GroupId, {space, SpaceId}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_space, SpaceId);

delete(GroupId, {handle_service, HServiceId}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_handle_service, HServiceId);

delete(GroupId, {handle, HandleId}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_handle, HandleId).


exists(undefined, _) ->
    true;

exists(_GroupId, {user, UserId}) ->
    {internal, fun(#od_group{users = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists(_GroupId, {eff_user, UserId}) ->
    {internal, fun(#od_group{eff_users = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists(_GroupId, {user_privileges, UserId}) ->
    {internal, fun(#od_group{users = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists(_GroupId, {eff_user_privileges, UserId}) ->
    {internal, fun(#od_group{eff_users = Users}) ->
        maps:is_key(UserId, Users)
    end};

exists(_GroupId, {parent, ParentId}) ->
    {internal, fun(#od_group{parents = Parents}) ->
        lists:member(ParentId, Parents)
    end};
exists(_GroupId, {eff_parent, ParentId}) ->
    {internal, fun(#od_group{eff_parents = Parents}) ->
        maps:is_key(ParentId, Parents)
    end};

exists(_GroupId, {child, ChildId}) ->
    {internal, fun(#od_group{children = Children}) ->
        maps:is_key(ChildId, Children)
    end};
exists(_GroupId, {eff_child, ChildId}) ->
    {internal, fun(#od_group{eff_children = Children}) ->
        maps:is_key(ChildId, Children)
    end};
exists(_GroupId, {child_privileges, ChildId}) ->
    {internal, fun(#od_group{children = Children}) ->
        maps:is_key(ChildId, Children)
    end};
exists(_GroupId, {eff_child_privileges, ChildId}) ->
    {internal, fun(#od_group{eff_children = Children}) ->
        maps:is_key(ChildId, Children)
    end};

exists(_UserId, {space, SpaceId}) ->
    {internal, fun(#od_group{spaces = Spaces}) ->
        lists:member(SpaceId, Spaces)
    end};
exists(_UserId, {eff_space, SpaceId}) ->
    {internal, fun(#od_group{eff_spaces = Spaces}) ->
        maps:is_key(SpaceId, Spaces)
    end};

exists(_UserId, {eff_provider, ProviderId}) ->
    {internal, fun(#od_group{eff_providers = Providers}) ->
        maps:is_key(ProviderId, Providers)
    end};

exists(_UserId, {handle_service, HServiceId}) ->
    {internal, fun(#od_group{handle_services = HServices}) ->
        lists:member(HServiceId, HServices)
    end};
exists(_UserId, {eff_handle_service, HServiceId}) ->
    {internal, fun(#od_group{eff_handle_services = HServices}) ->
        maps:is_key(HServiceId, HServices)
    end};

exists(_UserId, {handle, HandleId}) ->
    {internal, fun(#od_group{handles = Handles}) ->
        lists:member(HandleId, Handles)
    end};
exists(_UserId, {eff_handle, HandleId}) ->
    {internal, fun(#od_group{eff_handles = Handles}) ->
        maps:is_key(HandleId, Handles)
    end};

exists(_GroupId, _) ->
    {internal, fun(#od_group{}) ->
        % If the group with GroupId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


% TODO VFS-2918
authorize(get, _GroupId, deprecated_invite_user_token, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?GROUP_INVITE_USER);
% TODO VFS-2918
authorize(get, _GroupId, deprecated_invite_group_token, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?GROUP_INVITE_GROUP);
% TODO VFS-2918
authorize(update, _GroupId, {deprecated_user_privileges, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?GROUP_SET_PRIVILEGES);
% TODO VFS-2918
authorize(update, _GroupId, {deprecated_child_privileges, _ChildGroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?GROUP_SET_PRIVILEGES);

authorize(create, undefined, entity, ?USER) ->
    true;

authorize(create, _GroupId, create_space, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?GROUP_CREATE_SPACE);

authorize(create, _GroupId, create_handle_service, ?USER(UserId)) ->
    n_handle_service_logic_plugin:authorize(create, undefined, entity, ?USER(UserId));

authorize(create, _GroupId, create_handle, ?USER(UserId)) ->
    n_handle_logic_plugin:authorize(create, undefined, entity, ?USER(UserId));

authorize(create, _GroupId, join_group, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?GROUP_JOIN_GROUP);

authorize(create, _GroupId, join_space, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?GROUP_JOIN_SPACE);

authorize(create, _GroupId, users, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_GROUPS_ADD_MEMBERS);

authorize(create, _GroupId, children, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_GROUPS_ADD_MEMBERS);

authorize(get, undefined, list, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_GROUPS_LIST);

authorize(get, _GroupId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?GROUP_VIEW);

authorize(get, _GroupId, data, ?USER(UserId)) ->
    auth_by_membership(UserId);

authorize(get, _GroupId, oz_privileges, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_VIEW_PRIVILEGES);

authorize(get, _GroupId, eff_oz_privileges, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_VIEW_PRIVILEGES);

authorize(get, _GroupId, _, ?USER(UserId)) ->
    % All other resources can be accessed with view privileges
    auth_by_privilege(UserId, ?GROUP_VIEW);


authorize(update, _GroupId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?GROUP_UPDATE);

authorize(update, _GroupId, oz_privileges, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_SET_PRIVILEGES);

authorize(update, _GroupId, {user_privileges, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?GROUP_SET_PRIVILEGES);

authorize(update, _GroupId, {child_privileges, _ChildGroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?GROUP_SET_PRIVILEGES);


authorize(delete, _GroupId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?GROUP_DELETE);

authorize(delete, _GroupId, oz_privileges, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_SET_PRIVILEGES);

authorize(delete, _GroupId, {parent, _ParentId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?GROUP_LEAVE_GROUP);

authorize(delete, _GroupId, {space, _SpaceId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?GROUP_LEAVE_SPACE);

authorize(delete, _GroupId, {handle_service, _HServiceId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?GROUP_LEAVE_HANDLE_SERVICE);

authorize(delete, _GroupId, {handle, _HandleId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?GROUP_LEAVE_HANDLE);

authorize(delete, _GroupId, {user, _UserId}, ?USER(UserId)) -> [
    auth_by_privilege(UserId, ?GROUP_REMOVE_USER),
    auth_by_oz_privilege(UserId, ?OZ_GROUPS_REMOVE_MEMBERS)
];

authorize(delete, _GroupId, {child, _ChildGroupId}, ?USER(UserId)) -> [
    auth_by_privilege(UserId, ?GROUP_REMOVE_GROUP),
    auth_by_oz_privilege(UserId, ?OZ_GROUPS_REMOVE_MEMBERS)
].


validate(create, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty}
    },
    optional => #{
        <<"type">> => {atom, [organization, unit, team, role]}
    }
};
validate(create, create_space) ->
    n_space_logic_plugin:validate(create, entity);
validate(create, create_handle_service) ->
    n_handle_service_logic_plugin:validate(create, entity);
validate(create, create_handle) ->
    n_handle_logic_plugin:validate(create, entity);
validate(create, join_group) -> #{
    required => #{
        <<"token">> => {token, ?GROUP_INVITE_GROUP_TOKEN}
    }
};
validate(create, join_space) -> #{
    required => #{
        <<"token">> => {token, ?SPACE_INVITE_GROUP_TOKEN}
    }
};
validate(create, users) -> #{
    required => #{
        <<"userId">> => {binary, {exists, fun(Value) ->
            n_user_logic:exists(Value) end}
        }
    }
};
validate(create, children) -> #{
    required => #{
        <<"groupId">> => {binary, {exists, fun(Value) ->
            n_group_logic:exists(Value) end}
        }
    }
};
validate(update, entity) -> #{
    at_least_one => #{
        <<"name">> => {binary, non_empty},
        <<"type">> => {atom, [organization, unit, team, role]}
    }
};
validate(update, {Member, _Id}) when Member =:= user orelse Member =:= child ->
    #{
        required => #{
            <<"privileges">> => {list_of_atoms, privileges:group_privileges()}
        },
        optional => #{
            <<"operation">> => {atom, [set, grant, revoke]}
        }
    };
validate(update, oz_privileges) -> #{
    required => #{
        <<"privileges">> => {list_of_atoms, privileges:oz_privileges()}
    },
    optional => #{
        <<"operation">> => {atom, [set, grant, revoke]}
    }
}.


entity_to_string(GroupId) ->
    od_group:to_string(GroupId).


auth_by_membership(UserId) ->
    {internal, fun(#od_group{eff_users = EffUsers}) ->
        maps:is_key(UserId, EffUsers)
    end}.


auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_group{} = Group) ->
        n_group_logic:has_eff_privilege(Group, UserId, Privilege)
    end}.


auth_by_oz_privilege(UserId, Privilege) ->
    {external, fun() ->
        n_user_logic:has_eff_oz_privilege(UserId, Privilege)
    end}.
