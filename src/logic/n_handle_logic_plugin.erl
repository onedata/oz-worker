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

-include("entity_logic.hrl").
-include("entity_logic_errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").


-export([create/4, get_entity/1, get_internal/4, get_external/2, update/2,
    delete/1]).
-export([exists/2, authorize/5, validate/2]).


create(?USER(UserId), _, entity, Data) ->
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
        privileges:handle_admin()
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


get_entity(HandleId) ->
    case od_handle:get(HandleId) of
        {ok, #document{value = Handle}} ->
            {ok, Handle};
        _ ->
            ?EL_NOT_FOUND
    end.


get_internal(?USER, _HandleId, #od_handle{users = Users}, users) ->
    {ok, Users}.


get_external(?USER, _) ->
    ok.


update(HandleId, Data) when is_binary(HandleId) ->
    {ok, _} = od_handle:update(HandleId, fun(Handle) ->
        % TODO czy cos sie da update?
        {ok, Handle#od_handle{}}
    end),
    ok.


delete(HandleId) when is_binary(HandleId) ->
    ok = od_handle:delete(HandleId).


exists(undefined, entity) ->
    true;
exists(HandleId, entity) when is_binary(HandleId) ->
    {internal, fun(#od_handle{}) ->
        % If the handle with HandleId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end};
exists(HandleId, users) when is_binary(HandleId) ->
    {internal, fun(#od_handle{}) ->
        % If the handle with HandleId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end};
exists(HandleId, groups) when is_binary(HandleId) ->
    {internal, fun(#od_handle{}) ->
        % If the handle with HandleId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


authorize(create, undefined, entity, ?USER(UserId), Data) ->
    HandleServiceId = maps:get(<<"handleServiceId">>, Data, <<"">>),
    {external, fun() ->
        % TODO moze przeniesc has_eff do logic?
        n_handle_service_logic:has_eff_privilege(
            HandleServiceId, UserId, register_handle
        )
    end};
authorize(create, _HandleId, users, ?USER(UserId), _) ->
    auth_by_privilege(UserId, modify_handle);
authorize(create, _HandleId, groups, ?USER(UserId), _) ->
    auth_by_privilege(UserId, modify_handle);

authorize(get, _HandleId, users, ?USER(UserId), _) ->
    auth_by_privilege(UserId, view_handle);
authorize(get, _HandleId, entity, ?USER(UserId), _) ->
    auth_by_privilege(UserId, view_handle);

authorize(update, _HandleId, entity, ?USER(UserId), _) ->
    auth_by_privilege(UserId, modify_handle);

authorize(delete, _HandleId, entity, ?USER(UserId), _) ->
    auth_by_privilege(UserId, delete_handle).


validate(create, entity) -> #{
    required => #{
        <<"handleServiceId">> => {binary, non_empty}, % TODO sprawdzic czy jest
        <<"resourceType">> => {binary, [<<"Share">>]},
        <<"resourceId">> => {binary, non_empty}, % TODO sprawdzic czy jest
        <<"metadata">> => {binary, non_empty}
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
        <<"resourceType">> => {binary, [<<"Share">>]},
        <<"resourceId">> => {binary, non_empty},
        <<"metadata">> => {binary, non_empty}
    }
}.



auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_handle{} = Handle) ->
        n_handle_logic:has_eff_privilege(Handle, UserId, Privilege)
    end}.
