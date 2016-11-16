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


-export([create_impl/3, get_impl/1, add_relation_impl/3, update_impl/2,
    delete_impl/1]).
-export([exists_impl/1, authorize_impl/3, validate_impl/2]).


create_impl({user, UserId}, od_group, Data) ->
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
    {ok, GroupId}.


get_impl({#od_group{users = UsersPrivileges}, users}) ->
    {ok, UsersPrivileges};
get_impl(GroupId) when is_binary(GroupId) ->
    case od_group:get(GroupId) of
        {ok, #document{value = Group}} ->
            {ok, Group};
        _ ->
            ?EL_NOT_FOUND
    end.


add_relation_impl({GroupId, users}, od_user, UserId) ->
    entity_graph:add_relation(
        od_user, UserId,
        od_group, GroupId,
        privileges:group_user()
    ),
    {ok, GroupId}.


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


exists_impl(GroupId) when is_binary(GroupId) ->
    case od_group:get(GroupId) of
        {ok, #document{value = Group}} ->
            {true, Group};
        _ ->
            false
    end.


authorize_impl({user, _UserId}, create, od_group) ->
    true;
authorize_impl({user, UserId}, get, {GroupId, users}) when is_binary(GroupId) ->
    auth_by_privilege(UserId, group_view_data);
authorize_impl({user, UserId}, get, GroupId) when is_binary(GroupId) ->
    auth_by_privilege(UserId, group_view_data);
authorize_impl({user, UserId}, add_relation, {GroupId, users}) when is_binary(GroupId) ->
    auth_by_privilege(UserId, group_invite_user);
authorize_impl({user, UserId}, update, GroupId) when is_binary(GroupId) ->
    auth_by_privilege(UserId, group_change_data);
authorize_impl({user, UserId}, delete, GroupId) when is_binary(GroupId) ->
    auth_by_privilege(UserId, group_remove).


validate_impl(create, od_group) -> #{
    required => #{
        <<"name">> => binary
    },
    optional => #{
        <<"type">> => {atom, [organization, unit, team, role]}
    }
};
validate_impl(update, GroupId) when is_binary(GroupId) -> #{
    at_least_one => #{
        <<"name">> => binary,
        <<"type">> => {atom, [organization, unit, team, role]}
    }
}.



auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_group{} = Group) ->
        has_eff_privilege(Group, UserId, Privilege)
    end}.


has_eff_privilege(#od_group{users = UsersPrivileges}, UserId, Privilege) ->
    % TODO eff_users
    UserPrivileges = proplists:get_value(UserId, UsersPrivileges, []),
    lists:member(Privilege, UserPrivileges).

