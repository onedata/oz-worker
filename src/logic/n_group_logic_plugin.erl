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
-include("entity_logic_errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").


-export([create/4, get_entity/1, get_internal/4, get_external/2, update/3,
    delete/1]).
-export([exists/2, authorize/5, validate/2]).


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


get_entity(GroupId) ->
    case od_group:get(GroupId) of
        {ok, #document{value = Group}} ->
            {ok, Group};
        _ ->
            ?EL_NOT_FOUND
    end.


get_internal(?USER, _GroupId, #od_group{users = Users}, users) ->
    {ok, Users}.


get_external(?USER, _) ->
    ok.


update(GroupId, entity, Data) when is_binary(GroupId) ->
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
    entity_graph:update_oz_privileges(od_group, GroupId, Operation, Privileges).


delete(GroupId) when is_binary(GroupId) ->
    ok = od_group:delete(GroupId).


exists(undefined, entity) ->
    true;
exists(GroupId, _) when is_binary(GroupId) ->
    {internal, fun(#od_group{}) ->
        % If the group with GroupId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


authorize(create, undefined, entity, ?USER, _) ->
    true;
authorize(create, _GroupId, users, ?USER(UserId), _) ->
    auth_by_oz_privilege(UserId, add_member_to_group);
authorize(create, _GroupId, groups, ?USER(UserId), _) ->
    auth_by_oz_privilege(UserId, add_member_to_group);

authorize(get, _GroupId, users, ?USER(UserId), _) ->
    auth_by_privilege(UserId, group_view_data);
authorize(get, _GroupId, entity, ?USER(UserId), _) ->
    auth_by_privilege(UserId, group_view_data);
authorize(update, _GroupId, entity, ?USER(UserId), _) ->
    auth_by_privilege(UserId, group_change_data);
authorize(delete, _GroupId, entity, ?USER(UserId), _) ->
    auth_by_privilege(UserId, group_remove);
authorize(update, _GroupId, oz_privileges, ?USER(UserId), _) ->
    auth_by_oz_privilege(UserId, set_privileges).


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
validate(update, oz_privileges) -> #{
    required => #{
        <<"privileges">> => {list_of_atoms, privileges:oz_privileges()}
    },
    optional => #{
        <<"operation">> => {atom, [set, grant, revoke]}
    }
}.


auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_group{} = Group) ->
        n_group_logic:has_eff_privilege(Group, UserId, Privilege)
    end}.


auth_by_oz_privilege(UserId, Privilege) ->
    {external, fun() ->
        n_user_logic:has_eff_oz_privilege(UserId, Privilege)
    end}.
