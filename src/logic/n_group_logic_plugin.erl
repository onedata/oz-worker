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

-include("entity_logic_errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").


-export([create_impl/4, get_entity/1, get_internal/4, get_external/2, update_impl/2,
    delete_impl/1]).
-export([exists_impl/2, authorize_impl/4, validate_impl/2]).
-export([has_eff_privilege/3]).


create_impl({user, UserId}, _, entity, Data) ->
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
create_impl({user, _UserId}, GroupId, users, #{<<"userId">> := UserId}) ->
    entity_graph:add_relation(
        od_user, UserId,
        od_group, GroupId,
        privileges:group_user()
    ),
    {ok, GroupId};
create_impl({user, _UserId}, GroupId, groups, #{<<"groupId">> := ChildGroupId}) ->
    entity_graph:add_relation(
        od_group, ChildGroupId,
        od_group, GroupId,
        privileges:group_user()
    ),
    {ok, GroupId}.


get_entity(GroupId) ->
    case od_group:get(GroupId) of
        {ok, #document{value = Group}} ->
            {ok, Group};
        _ ->
            ?EL_NOT_FOUND
    end.


get_internal({user, _UserId}, _GroupId, #od_group{users = Users}, users) ->
    {ok, Users}.


get_external({user, _UserId}, _) ->
    ok.


%%add_relation_impl({GroupId, users}, od_user, UserId) ->
%%    entity_graph:add_relation(
%%        od_user, UserId,
%%        od_group, GroupId,
%%        privileges:group_user()
%%    ),
%%    {ok, GroupId}.


update_impl(GroupId, Data) when is_binary(GroupId) ->
    {ok, _} = od_group:update(GroupId, fun(Group) ->
        #od_group{name = OldName, type = OldType} = Group,
        NewName = maps:get(<<"name">>, Data, OldName),
        NewType = maps:get(<<"type">>, Data, OldType),
        {ok, Group#od_group{name = NewName, type = NewType}}
    end),
    ok.


delete_impl(GroupId) when is_binary(GroupId) ->
    ok = od_group:delete(GroupId).


exists_impl(GroupId, entity) when is_binary(GroupId) ->
    case od_group:get(GroupId) of
        {ok, #document{value = Group}} ->
            {true, Group};
        _ ->
            false
    end;
exists_impl(GroupId, users) when is_binary(GroupId) ->
    {internal, fun(#od_group{}) ->
        % If the space with SpaceId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end};
exists_impl(GroupId, groups) when is_binary(GroupId) ->
    {internal, fun(#od_group{}) ->
        % If the space with SpaceId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end};
exists_impl(_, _) ->
    true.


authorize_impl({user, _UserId}, create, undefined, entity) ->
    true;
authorize_impl({user, UserId}, create, GroupId, users) when is_binary(GroupId) ->
    auth_by_privilege(UserId, group_invite_user); %TODO admin privs
authorize_impl({user, UserId}, create, GroupId, groups) when is_binary(GroupId) ->
    auth_by_privilege(UserId, group_invite_group); %TODO admin privs

authorize_impl({user, UserId}, get, GroupId, users) when is_binary(GroupId) ->
    auth_by_privilege(UserId, group_view_data);
authorize_impl({user, UserId}, get, GroupId, entity) when is_binary(GroupId) ->
    auth_by_privilege(UserId, group_view_data);
authorize_impl({user, UserId}, update, GroupId, entity) when is_binary(GroupId) ->
    auth_by_privilege(UserId, group_change_data);
authorize_impl({user, UserId}, delete, GroupId, entity) when is_binary(GroupId) ->
    auth_by_privilege(UserId, group_remove);

authorize_impl({user, UserId}, add_relation, todo, users)  ->
    auth_by_privilege(UserId, group_invite_user).


validate_impl(create, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty}
    },
    optional => #{
        <<"type">> => {atom, [organization, unit, team, role]}
    }
};
validate_impl(create, users) -> #{
    required => #{
        <<"userId">> => {binary, fun(Value) -> user_logic:exists(Value) end}
    }
};
validate_impl(create, groups) -> #{
    required => #{
        <<"groupId">> => {binary, fun(Value) -> group_logic:exists(Value) end}
    }
};
validate_impl(update, entity) -> #{
    at_least_one => #{
        <<"name">> => {binary, non_empty},
        <<"type">> => {atom, [organization, unit, team, role]}
    }
}.



auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_group{} = Group) ->
        has_eff_privilege(Group, UserId, Privilege)
    end}.


has_eff_privilege(GroupId, UserId, Privilege) when is_binary(GroupId) ->
    {ok, #document{value = Group}} = od_group:get(GroupId),
    has_eff_privilege(Group, UserId, Privilege);
has_eff_privilege(#od_group{users = UsersPrivileges}, UserId, Privilege) ->
    % TODO eff_users
    UserPrivileges = proplists:get_value(UserId, UsersPrivileges, []),
    lists:member(Privilege, UserPrivileges).

