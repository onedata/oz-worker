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

-include("entity_logic_errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").


-export([create_impl/4, get_entity/1, get_internal/4, get_external/2, update_impl/2,
    delete_impl/1]).
-export([exists_impl/2, authorize_impl/5, validate_impl/2]).
-export([has_eff_privilege/3]).


create_impl({user, UserId}, _, entity, Data) ->
    Name = maps:get(<<"name">>, Data),
    ProxyEndpoint = maps:get(<<"proxyEndpoint">>, Data),
    ServiceProperties = maps:get(<<"serviceProperties">>, Data),
    _Type = maps:get(<<"type">>, Data),
    HandleService = #document{value = #od_handle_service{
        name = Name,
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties
    }},
    ?dump(HandleService),
    {ok, HServiceId} = od_handle_service:create(HandleService),
    entity_graph:add_relation(
        od_user, UserId,
        od_handle_service, HServiceId,
        privileges:handle_service_admin()
    ),
    {ok, HServiceId};
create_impl({user, _UserId}, HServiceId, users, #{<<"userId">> := UserId}) ->
    entity_graph:add_relation(
        od_user, UserId,
        od_handle_service, HServiceId,
        privileges:handle_service_user()
    ),
    {ok, HServiceId};
create_impl({user, _UserId}, HServiceId, groups, #{<<"groupId">> := GroupId}) ->
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
            ?EL_NOT_FOUND
    end.


get_internal({user, _UserId}, _HServiceId, #od_handle_service{users = Users}, users) ->
    {ok, Users}.


get_external({user, _UserId}, _) ->
    ok.


update_impl(HServiceId, Data) when is_binary(HServiceId) ->
    {ok, _} = od_handle_service:update(HServiceId, fun(HService) ->
        % TODO czy cos sie da update?
        {ok, HService#od_handle_service{}}
    end),
    ok.


delete_impl(HServiceId) when is_binary(HServiceId) ->
    ok = od_handle_service:delete(HServiceId).


exists_impl(undefined, entity) ->
    true;
exists_impl(HServiceId, entity) when is_binary(HServiceId) ->
    {internal, fun(#od_handle_service{}) ->
        % If the handle service with HServiceId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end};
exists_impl(HServiceId, users) when is_binary(HServiceId) ->
    {internal, fun(#od_handle_service{}) ->
        % If the handle service with HServiceId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end};
exists_impl(HServiceId, groups) when is_binary(HServiceId) ->
    {internal, fun(#od_handle_service{}) ->
        % If the handle service with HServiceId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


authorize_impl({user, _UserId}, create, undefined, entity, _) ->
    true;
authorize_impl({user, UserId}, create, HServiceId, users, _) when is_binary(HServiceId) ->
    auth_by_privilege(UserId, modify_handle_service);
authorize_impl({user, UserId}, create, HServiceId, groups, _) when is_binary(HServiceId) ->
    auth_by_privilege(UserId, modify_handle_service);

authorize_impl({user, UserId}, get, HServiceId, users, _) when is_binary(HServiceId) ->
    auth_by_privilege(UserId, view_handle_service);
authorize_impl({user, UserId}, get, HServiceId, entity, _) when is_binary(HServiceId) ->
    auth_by_privilege(UserId, view_handle_service);

authorize_impl({user, UserId}, update, HServiceId, entity, _) when is_binary(HServiceId) ->
    auth_by_privilege(UserId, modify_handle_service);

authorize_impl({user, UserId}, delete, HServiceId, entity, _) when is_binary(HServiceId) ->
    auth_by_privilege(UserId, delete_handle_service).


validate_impl(create, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty},
        <<"proxyEndpoint">> => {binary, non_empty},
        <<"serviceProperties">> => {json, non_empty},
        <<"type">> => {binary, non_empty}
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
        <<"name">> => {binary, non_empty},
        <<"proxyEndpoint">> => {binary, non_empty},
        <<"serviceProperties">> => {json, non_empty}
    }
}.



auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_handle_service{} = HService) ->
        has_eff_privilege(HService, UserId, Privilege)
    end}.


has_eff_privilege(HServiceId, UserId, Privilege) when is_binary(HServiceId) ->
    {ok, #document{value = HService}} = od_handle_service:get(HServiceId),
    has_eff_privilege(HService, UserId, Privilege);
has_eff_privilege(#od_handle_service{eff_users = UsersPrivileges}, UserId, Privilege) ->
    {UserPrivileges, _} = maps:get(UserId, UsersPrivileges, []),
    lists:member(Privilege, UserPrivileges).

