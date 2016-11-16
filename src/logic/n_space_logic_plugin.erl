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
-module(n_space_logic_plugin).
-author("Lukasz Opiola").
-behaviour(data_logic_plugin_behaviour).

-include("entity_logic_errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").


-export([create_impl/3, get_impl/1, authorize_impl/3, validate_impl/2]).

create_impl({user, UserId}, od_space, Data) ->
    Name = maps:get(<<"name">>, Data),
    {ok, SpaceId} = od_space:create(#document{value = #od_space{name = Name}}),
    entity_graph:add_relation(
        od_user, UserId,
        od_space, SpaceId,
        privileges:space_admin()
    ),
%%  user_logic:set_space_name_mapping(UserId, SpaceId, Name, true),
    {ok, SpaceId}.


get_impl({#od_space{users = UsersPrivileges}, users}) ->
    {ok, UsersPrivileges};
get_impl(SpaceId) when is_binary(SpaceId) ->
    case od_space:get(SpaceId) of
        {ok, #document{value = Space}} ->
            {ok, Space};
        _ ->
            ?EL_NOT_FOUND
    end.


authorize_impl({user, _}, create, od_space) ->
    true;
authorize_impl({user, UserId}, get, {SpaceId, users}) when is_binary(SpaceId) ->
    {internal, fun(#od_space{} = Space) ->
        has_eff_privilege(Space, UserId, space_view_data)
    end};
authorize_impl({user, UserId}, get, SpaceId) when is_binary(SpaceId) ->
    {internal, fun(#od_space{} = Space) ->
        has_eff_privilege(Space, UserId, space_view_data)
    end}.


validate_impl(create, od_space) -> #{
    <<"name">> => non_empty_binary
}.




has_eff_privilege(#od_space{users = UsersPrivileges}, UserId, Privilege) ->
    % TODO eff_users
    UserPrivileges = proplists:get_value(UserId, UsersPrivileges, []),
    lists:member(Privilege, UserPrivileges).

