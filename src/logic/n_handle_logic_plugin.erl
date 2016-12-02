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

-include("entity_logic_errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").


-export([create_impl/4, get_entity/1, get_internal/4, get_external/2, update_impl/2,
    delete_impl/1]).
-export([exists_impl/2, authorize_impl/5, validate_impl/2]).
-export([has_eff_privilege/3]).


create_impl({user, UserId}, _, entity, Data) ->
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
        od_user, UserId,
        od_handle, HandleId,
        privileges:handle_user()
    ),
    % TODO add relation?
    case ResourceType of
        <<"Share">> ->
            {ok, _} = od_share:update(ResourceId, fun(Share = #od_share{}) ->
                {ok, Share#od_share{handle = HandleId}}
            end);
        _ ->
            ok
    end,
    {ok, HandleId};
create_impl({user, _UserId}, HandleId, users, #{<<"userId">> := UserId}) ->
    entity_graph:add_relation(
        od_user, UserId,
        od_handle, HandleId,
        privileges:handle_admin()
    ),
    {ok, HandleId};
create_impl({user, _UserId}, HandleId, groups, #{<<"groupId">> := GroupId}) ->
    entity_graph:add_relation(
        od_group, GroupId,
        od_handle, HandleId,
        privileges:handle_admin()
    ),
    {ok, HandleId}.


get_entity(HandleId) ->
    case od_handle:get(HandleId) of
        {ok, #document{value = Handle}} ->
            {ok, Handle};
        _ ->
            ?EL_NOT_FOUND
    end.


get_internal({user, _UserId}, _HandleId, #od_handle{users = Users}, users) ->
    {ok, Users}.


get_external({user, _UserId}, _) ->
    ok.


update_impl(HandleId, Data) when is_binary(HandleId) ->
    {ok, _} = od_handle:update(HandleId, fun(Handle) ->
        % TODO czy cos sie da update?
        {ok, Handle#od_handle{}}
    end),
    ok.


delete_impl(HandleId) when is_binary(HandleId) ->
    ok = od_handle:delete(HandleId).


exists_impl(undefined, entity) ->
    true;
exists_impl(HandleId, entity) when is_binary(HandleId) ->
    {internal, fun(#od_handle{}) ->
        % If the handle with HandleId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end};
exists_impl(HandleId, users) when is_binary(HandleId) ->
    {internal, fun(#od_handle{}) ->
        % If the handle with HandleId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end};
exists_impl(HandleId, groups) when is_binary(HandleId) ->
    {internal, fun(#od_handle{}) ->
        % If the handle with HandleId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


authorize_impl({user, UserId}, create, undefined, entity, Data) ->
    HandleServiceId = maps:get(<<"handleServiceId">>, Data, <<"">>),
    {external, fun() ->
        % TODO moze przeniesc has_eff do logic?
        n_handle_service_logic_plugin:has_eff_privilege(
            HandleServiceId, UserId, register_handle
        )
    end};
authorize_impl({user, UserId}, create, HandleId, users, _) when is_binary(HandleId) ->
    auth_by_privilege(UserId, modify_handle);
authorize_impl({user, UserId}, create, HandleId, groups, _) when is_binary(HandleId) ->
    auth_by_privilege(UserId, modify_handle);

authorize_impl({user, UserId}, get, HandleId, users, _) when is_binary(HandleId) ->
    auth_by_privilege(UserId, view_handle);
authorize_impl({user, UserId}, get, HandleId, entity, _) when is_binary(HandleId) ->
    auth_by_privilege(UserId, view_handle);

authorize_impl({user, UserId}, update, HandleId, entity, _) when is_binary(HandleId) ->
    auth_by_privilege(UserId, modify_handle);

authorize_impl({user, UserId}, delete, HandleId, entity, _) when is_binary(HandleId) ->
    auth_by_privilege(UserId, delete_handle).


validate_impl(create, entity) -> #{
    required => #{
        <<"handleServiceId">> => {binary, non_empty}, % TODO sprawdzic czy jest
        <<"resourceType">> => {binary, [<<"Share">>]},
        <<"resourceId">> => {binary, non_empty}, % TODO sprawdzic czy jest
        <<"metadata">> => {binary, non_empty}
    }
};
validate_impl(create, users) -> #{
    required => #{
        <<"userId">> => {binary, {exists, fun(Value) ->
            user_logic:exists(Value) end}
        }
    }
};
validate_impl(create, groups) -> #{
    required => #{
        <<"groupId">> => {binary, {exists, fun(Value) ->
            group_logic:exists(Value) end}
        }
    }
};
validate_impl(update, entity) -> #{
    at_least_one => #{
        <<"resourceType">> => {binary, [<<"Share">>]},
        <<"resourceId">> => {binary, non_empty},
        <<"metadata">> => {binary, non_empty}
    }
}.



auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_handle{} = Handle) ->
        has_eff_privilege(Handle, UserId, Privilege)
    end}.


has_eff_privilege(HandleId, UserId, Privilege) when is_binary(HandleId) ->
    % TODO a co jak nie ma tego handle?
    {ok, #document{value = Handle}} = od_handle:get(HandleId),
    has_eff_privilege(Handle, UserId, Privilege);
has_eff_privilege(#od_handle{eff_users = UsersPrivileges}, UserId, Privilege) ->
    % TODO eff_users
    {UserPrivileges, _} = maps:get(UserId, UsersPrivileges, []),
    lists:member(Privilege, UserPrivileges).

