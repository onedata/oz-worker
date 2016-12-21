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
-module(n_handle_service_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("entity_logic.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").


-export([create/4, get_entity/1, get_internal/4, get_external/2, update/2,
    delete/1]).
-export([exists/2, authorize/5, validate/2]).



create(?USER(UserId), _, entity, Data) ->
    Name = maps:get(<<"name">>, Data),
    ProxyEndpoint = maps:get(<<"proxyEndpoint">>, Data),
    ServiceProperties = maps:get(<<"serviceProperties">>, Data),
    HandleService = #document{value = #od_handle_service{
        name = Name,
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties
    }},
    {ok, HServiceId} = od_handle_service:create(HandleService),
    entity_graph:add_relation(
        od_user, UserId,
        od_handle_service, HServiceId,
        privileges:handle_service_admin()
    ),
    {ok, HServiceId};
create(?USER, HServiceId, users, #{<<"userId">> := UserId}) ->
    entity_graph:add_relation(
        od_user, UserId,
        od_handle_service, HServiceId,
        privileges:handle_service_user()
    ),
    {ok, HServiceId};
create(?USER, HServiceId, groups, #{<<"groupId">> := GroupId}) ->
    entity_graph:add_relation(
        od_group, GroupId,
        od_handle_service, HServiceId,
        privileges:handle_service_user()
    ),
    {ok, HServiceId}.


get_entity(HServiceId) ->
    case od_handle_service:get(HServiceId) of
        {ok, #document{value = HandleService}} ->
            {ok, HandleService};
        _ ->
            ?ERROR_NOT_FOUND
    end.


get_internal(?USER, _HServiceId, #od_handle_service{users = Users}, users) ->
    {ok, Users}.


get_external(_, list) ->
    {ok, HServiceDocs} = od_handle_service:list(),
    {ok, [HServiceId || #document{key = HServiceId} <- HServiceDocs]};
get_external(?USER, _) ->
    ok.


update(HServiceId, Data) when is_binary(HServiceId) ->
    {ok, _} = od_handle_service:update(HServiceId, fun(HService) ->
        % TODO czy cos sie da update?
        {ok, HService#od_handle_service{}}
    end),
    ok.


delete(HServiceId) when is_binary(HServiceId) ->
    ok = od_handle_service:delete(HServiceId).


exists(undefined, entity) ->
    true;
exists(undefined, list) ->
    true;
exists(HServiceId, entity) when is_binary(HServiceId) ->
    {internal, fun(#od_handle_service{}) ->
        % If the handle service with HServiceId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end};
exists(HServiceId, users) when is_binary(HServiceId) ->
    {internal, fun(#od_handle_service{}) ->
        % If the handle service with HServiceId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end};
exists(HServiceId, groups) when is_binary(HServiceId) ->
    {internal, fun(#od_handle_service{}) ->
        % If the handle service with HServiceId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


authorize(create, undefined, entity, ?USER, _) ->
    true;
authorize(create, _HServiceId, users, ?USER(UserId), _) ->
    auth_by_privilege(UserId, modify_handle_service);
authorize(create, _HServiceId, groups, ?USER(UserId), _) ->
    auth_by_privilege(UserId, modify_handle_service);

authorize(get, _HServiceId, users, ?USER(UserId), _) ->
    auth_by_privilege(UserId, view_handle_service);
authorize(get, _HServiceId, entity, ?USER(UserId), _) ->
    auth_by_privilege(UserId, view_handle_service);

authorize(update, _HServiceId, entity, ?USER(UserId), _) ->
    auth_by_privilege(UserId, modify_handle_service);

authorize(delete, _HServiceId, entity, ?USER(UserId), _) ->
    auth_by_privilege(UserId, delete_handle_service).


validate(create, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty},
        <<"proxyEndpoint">> => {binary, non_empty},
        <<"serviceProperties">> => {json, non_empty}
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
        <<"proxyEndpoint">> => {binary, non_empty},
        <<"serviceProperties">> => {json, non_empty}
    }
}.



auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_handle_service{} = HService) ->
        n_handle_service_logic:has_eff_privilege(HService, UserId, Privilege)
    end}.
