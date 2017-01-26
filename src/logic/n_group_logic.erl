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
-module(n_group_logic).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-define(PLUGIN, n_group_logic_plugin).

-export([
    create/2, create/3
]).
-export([
    get/2,
    get_data/2,
    list/1,
    get_oz_privileges/2, get_eff_oz_privileges/2
]).
-export([
    update/3,
    update_oz_privileges/4, update_oz_privileges/3
]).
-export([
    delete/2,
    delete_oz_privileges/2
]).
-export([
    create_space/2,
    create_handle_service/4, create_handle_service/2,
    create_handle/5, create_handle/2,

    create_user_invite_token/2,
    create_group_invite_token/2,

    join_group/3,
    join_space/3,

    add_user/4, add_user/3,
    add_group/4, add_group/3,

    get_users/2, get_eff_users/2,
    get_user/3, get_eff_user/3,
    get_user_privileges/3, get_eff_user_privileges/3,

    get_parents/2, get_eff_parents/2,
    get_parent/3, get_eff_parent/3,

    get_children/2, get_eff_children/2,
    get_child/3, get_eff_child/3,
    get_child_privileges/3, get_eff_child_privileges/3,

    get_spaces/2, get_eff_spaces/2,
    get_space/3, get_eff_space/3,

    get_eff_providers/2, get_eff_provider/3,

    get_handle_services/2, get_eff_handle_services/2,
    get_handle_service/3, get_eff_handle_service/3,

    get_handles/2, get_eff_handles/2,
    get_handle/3, get_eff_handle/3,

    update_user_privileges/5, update_user_privileges/4,
    update_child_privileges/5, update_child_privileges/4,

    leave_group/3,
    leave_space/3,
    leave_handle_service/3,
    leave_handle/3,

    remove_user/3,
    remove_group/3
]).
-export([
    exists/1,
    has_eff_privilege/3
]).
-export([
    create_predefined_groups/0
]).


create(Client, Name, Type) ->
    create(Client, #{<<"name">> => Name, <<"type">> => Type}).
create(Client, Name) when is_binary(Name) ->
    create(Client, #{<<"name">> => Name});
create(Client, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, undefined, entity, Data).


get(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, entity, GroupId).


get_data(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, data, GroupId).


list(Client) ->
    n_entity_logic:get(Client, ?PLUGIN, undefined, list).


get_oz_privileges(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, oz_privileges).


get_eff_oz_privileges(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, eff_oz_privileges).


update(Client, GroupId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, GroupId, entity, Data).


update_oz_privileges(Client, GroupId, Operation, Privs) when is_list(Privs) ->
    update_oz_privileges(Client, GroupId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).
update_oz_privileges(Client, GroupId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, GroupId, oz_privileges, Data).


delete(Client, GroupId) ->
    n_entity_logic:delete(Client, ?PLUGIN, GroupId, entity).


delete_oz_privileges(Client, GroupId) ->
    n_entity_logic:delete(Client, ?PLUGIN, GroupId, oz_privileges).


create_space(Client, Data) ->
    n_space_logic:create(Client, Data).


create_handle_service(Client, Name, ProxyEndpoint, ServiceProperties) ->
    n_handle_service_logic:create(Client, Name, ProxyEndpoint, ServiceProperties).


create_handle_service(Client, Data) ->
    n_handle_service_logic:create(Client, Data).


create_handle(Client, HServiceId, ResourceType, ResourceId, Metadata) ->
    n_handle_logic:create(Client, HServiceId, ResourceType, ResourceId, Metadata).


create_handle(Client, Data) ->
    n_handle_logic:create(Client, Data).


create_user_invite_token(Client, GroupId) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, invite_user_token, #{}).


create_group_invite_token(Client, GroupId) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, invite_group_token, #{}).


join_group(Client, GroupId, Data) when is_map(Data) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, join_group, Data);
join_group(Client, GroupId, Token) ->
    join_group(Client, GroupId, #{<<"token">> => Token}).


join_space(Client, GroupId, Data) when is_map(Data) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, join_space, Data);
join_space(Client, GroupId, Token) ->
    join_space(Client, GroupId, #{<<"token">> => Token}).


add_user(Client, GroupId, UserId, Privileges) when is_binary(UserId) ->
    add_user(Client, GroupId, #{
        <<"userId">> => UserId,
        <<"privileges">> => Privileges
    }).
add_user(Client, GroupId, UserId) when is_binary(UserId) ->
    add_user(Client, GroupId, #{<<"userId">> => UserId});
add_user(Client, GroupId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, users, Data).


add_group(Client, GroupId, ChildGroupId, Privileges) when is_binary(ChildGroupId) ->
    add_group(Client, GroupId, #{
        <<"userId">> => ChildGroupId,
        <<"privileges">> => Privileges
    }).
add_group(Client, GroupId, ChildGroupId) when is_binary(ChildGroupId) ->
    add_group(Client, GroupId, #{<<"groupId">> => ChildGroupId});
add_group(Client, GroupId, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, GroupId, children, Data).


get_users(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, users).


get_eff_users(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, eff_users).


get_user(Client, GroupId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {user, UserId}).


get_eff_user(Client, GroupId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_user, UserId}).


get_user_privileges(Client, GroupId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {user_privileges, UserId}).


get_eff_user_privileges(Client, GroupId, UserId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_user_privileges, UserId}).


get_parents(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, parents).


get_eff_parents(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, eff_parents).


get_parent(Client, GroupId, ParentId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {parent, ParentId}).


get_eff_parent(Client, GroupId, ParentId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_parent, ParentId}).


get_children(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, children).


get_eff_children(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, eff_children).


get_child(Client, GroupId, ChildId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {child, ChildId}).


get_eff_child(Client, GroupId, ChildId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_child, ChildId}).


get_child_privileges(Client, GroupId, ChildId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {child_privileges, ChildId}).


get_eff_child_privileges(Client, GroupId, ChildId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_child_privileges, ChildId}).


get_spaces(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, spaces).


get_eff_spaces(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, eff_spaces).


get_space(Client, GroupId, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {space, SpaceId}).


get_eff_space(Client, GroupId, SpaceId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_space, SpaceId}).


get_eff_providers(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, eff_providers).


get_eff_provider(Client, GroupId, ProviderId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_provider, ProviderId}).


get_handle_services(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, handle_services).


get_eff_handle_services(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, eff_handle_services).


get_handle_service(Client, GroupId, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {handle_service, HServiceId}).


get_eff_handle_service(Client, GroupId, HServiceId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_handle_service, HServiceId}).


get_handles(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, handles).


get_eff_handles(Client, GroupId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, eff_handles).


get_handle(Client, GroupId, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {handle, HandleId}).


get_eff_handle(Client, GroupId, HandleId) ->
    n_entity_logic:get(Client, ?PLUGIN, GroupId, {eff_handle, HandleId}).


update_user_privileges(Client, GroupId, UserId, Operation, Privs) when is_list(Privs) ->
    update_user_privileges(Client, GroupId, UserId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).
update_user_privileges(Client, GroupId, UserId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, GroupId, {user_privileges, UserId}, Data).


update_child_privileges(Client, ParentId, ChildId, Operation, Privs) when is_list(Privs) ->
    update_child_privileges(Client, ParentId, ChildId, #{
        <<"operation">> => Operation,
        <<"privileges">> => Privs
    }).
update_child_privileges(Client, ParentId, ChildId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, ParentId, {child_privileges, ChildId}, Data).


leave_group(Client, UserId, GroupId) ->
    n_entity_logic:delete(Client, ?PLUGIN, UserId, {parent, GroupId}).


leave_space(Client, UserId, SpaceId) ->
    n_entity_logic:delete(Client, ?PLUGIN, UserId, {space, SpaceId}).


leave_handle_service(Client, UserId, HServiceId) ->
    n_entity_logic:delete(Client, ?PLUGIN, UserId, {handle_service, HServiceId}).


leave_handle(Client, UserId, HandleId) ->
    n_entity_logic:delete(Client, ?PLUGIN, UserId, {handle, HandleId}).


remove_user(Client, GroupId, UserId) ->
    n_entity_logic:delete(Client, ?PLUGIN, GroupId, {user, UserId}).


remove_group(Client, ParentId, ChildId) ->
    n_entity_logic:delete(Client, ?PLUGIN, ParentId, {child, ChildId}).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a group exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(GroupId :: od_group:id()) -> boolean().
exists(GroupId) ->
    od_group:exists(GroupId).


has_eff_privilege(GroupId, UserId, Privilege) when is_binary(GroupId) ->
    case od_group:get(GroupId) of
        {ok, #document{value = Group}} ->
            has_eff_privilege(Group, UserId, Privilege);
        _ ->
            false
    end;
has_eff_privilege(#od_group{eff_users = UsersPrivileges}, UserId, Privilege) ->
    {UserPrivileges, _} = maps:get(UserId, UsersPrivileges, {[], []}),
    lists:member(Privilege, UserPrivileges).

%%--------------------------------------------------------------------
%% @doc
%% Creates predefined groups in the system based on settings in app.config.
%% @end
%%--------------------------------------------------------------------
-spec create_predefined_groups() -> ok.
create_predefined_groups() ->
    {ok, PredefinedGroups} =
        application:get_env(?APP_NAME, predefined_groups),
    lists:foreach(
        fun(GroupMap) ->
            Id = maps:get(id, GroupMap),
            Name = maps:get(name, GroupMap),
            % Privileges can be either a list of privileges or a module and
            % function to call that will return such list.
            Privs = case maps:get(oz_privileges, GroupMap) of
                List when is_list(List) ->
                    List;
                {Module, Function} ->
                    Module:Function()
            end,
            create_predefined_group(Id, Name, Privs)
        end, PredefinedGroups).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a predefined group in the system, if it does not exist, and grants
%% given privileges to it.
%% @end
%%--------------------------------------------------------------------
-spec create_predefined_group(Id :: binary(), Name :: binary(),
    Privileges :: [privileges:oz_privilege()]) -> ok | error.
create_predefined_group(GroupId, Name, Privileges) ->
    case od_group:exists(GroupId) of
        true ->
            ?info("Predefined group '~s' already exists, "
            "skipping.", [Name]),
            ok;
        false ->
            NewGroup = #document{
                key = GroupId,
                value = #od_group{
                    name = Name,
                    type = role
                }},
            case od_group:create(NewGroup) of
                {ok, GroupId} ->
                    ok = update_oz_privileges(?ROOT, GroupId, set, Privileges),
                    ?info("Created predefined group '~s'", [Name]),
                    ok;
                Other ->
                    ?error("Cannot create predefined group '~s' - ~p",
                        [GroupId, Other]),
                    error
            end
    end.




