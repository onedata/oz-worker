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
-behaviour(data_logic_plugin_behaviour).

-include("entity_logic.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").


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


create(?ROOT, _, entity, Data) ->
    Name = maps:get(<<"name">>, Data),
    Type = maps:get(<<"type">>, Data, role),
    {ok, GroupId} = od_group:create(
        #document{value = #od_group{name = Name, type = Type}}
    ),
    {ok, GroupId};
create(?USER(UserId), _, entity, Data) ->
    Name = maps:get(<<"name">>, Data),
    Type = maps:get(<<"type">>, Data, role),
    {ok, GroupId} = od_group:create(
        #document{value = #od_group{name = Name, type = Type}}
    ),
    entity_graph:add_relation(
        od_user, UserId,
        od_group, GroupId,
        privileges:group_admin()
    ),
    {ok, GroupId};
create(_Client, GroupId, users, #{<<"userId">> := UserId}) ->
    entity_graph:add_relation(
        od_user, UserId,
        od_group, GroupId,
        privileges:group_user()
    ),
    {ok, UserId};
create(_Client, GroupId, groups, #{<<"groupId">> := ChildGroupId}) ->
    entity_graph:add_relation(
        od_group, ChildGroupId,
        od_group, GroupId,
        privileges:group_user()
    ),
    {ok, ChildGroupId}.


get(_, undefined, undefined, list) ->
    {ok, GroupDocs} = od_group:list(),
    {ok, [GroupId || #document{key = GroupId} <- GroupDocs]};
get(?USER, _GroupId, #od_group{users = Users}, users) ->
    {ok, Users}.



update(GroupId, entity, Data) when is_binary(GroupId) ->
    {ok, _} = od_group:update(GroupId, fun(Group) ->
        #od_group{name = OldName, type = OldType} = Group,
        NewName = maps:get(<<"name">>, Data, OldName),
        NewType = maps:get(<<"type">>, Data, OldType),
        {ok, Group#od_group{name = NewName, type = NewType}}
    end),
    ok;

update(GroupId, {user, UserId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_user, UserId,
        od_group, GroupId,
        {Operation, Privileges}
    );

update(ParentGroupId, {group, ChildGroupId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_group, ChildGroupId,
        od_group, ParentGroupId,
        {Operation, Privileges}
    );


update(GroupId, oz_privileges, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_oz_privileges(od_group, GroupId, Operation, Privileges).


delete(GroupId, entity) when is_binary(GroupId) ->
    entity_graph:delete_with_relations(od_group, GroupId);

delete(GroupId, {user, UserId}) when is_binary(GroupId) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_group, GroupId
    );

delete(ParentGroupId, {group, ChildGroupId}) when is_binary(ParentGroupId) ->
    entity_graph:remove_relation(
        od_group, ChildGroupId,
        od_group, ParentGroupId
    ).


exists(undefined, _) ->
    true;
exists(GroupId, _) when is_binary(GroupId) ->
    {internal, fun(#od_group{}) ->
        % If the group with GroupId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


authorize(create, undefined, entity, ?USER) ->
    true;
authorize(create, _GroupId, users, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, add_member_to_group);
authorize(create, _GroupId, groups, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, add_member_to_group);

authorize(get, undefined, list, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, list_groups);
authorize(get, _GroupId, users, ?USER(UserId)) ->
    auth_by_privilege(UserId, group_view_data);
authorize(get, _GroupId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, group_view_data);


authorize(update, _GroupId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, group_change_data);

authorize(update, _GroupId, {user, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, group_set_privileges);

authorize(update, _GroupId, {group, _ChildGroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, group_set_privileges);

authorize(update, _GroupId, oz_privileges, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, set_privileges);


authorize(delete, _GroupId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, group_remove);

authorize(delete, _GroupId, {user, _UserId}, ?USER(UserId)) -> [
    auth_by_privilege(UserId, group_remove_user),
    auth_by_oz_privilege(UserId, remove_member_from_group)
];

authorize(delete, _GroupId, {group, _ChildGroupId}, ?USER(UserId)) -> [
    auth_by_privilege(UserId, group_remove_group),
    auth_by_oz_privilege(UserId, remove_member_from_group)
].


validate(create, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty}
    },
    optional => #{
        <<"type">> => {atom, [organization, unit, team, role]}
    }
};
validate(create, users) -> #{
    required => #{
        <<"userId">> => {binary, {exists, fun(Value) ->
            user_logic:exists(Value) end}
        }
    }
};
validate(create, groups) -> #{
    required => #{
        <<"groupId">> => {binary, {exists, fun(Value) ->
            group_logic:exists(Value) end}
        }
    }
};
validate(update, entity) -> #{
    at_least_one => #{
        <<"name">> => {binary, non_empty},
        <<"type">> => {atom, [organization, unit, team, role]}
    }
};
validate(update, Member) when Member =:= user orelse Member =:= group -> #{
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


auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_group{} = Group) ->
        n_group_logic:has_eff_privilege(Group, UserId, Privilege)
    end}.


auth_by_oz_privilege(UserId, Privilege) ->
    {external, fun() ->
        auth_by_oz_privilege(UserId, Privilege)
    end}.
