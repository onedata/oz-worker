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


-export([create_impl/4, get_entity/1, get_internal/4, get_external/2, add_relation_impl/3, update_impl/2,
    delete_impl/1]).
-export([exists_impl/2, authorize_impl/4, validate_impl/2]).
-export([has_eff_privilege/3]).

create_impl({user, UserId}, _, entity, #{<<"name">> := Name}) ->
    {ok, SpaceId} = od_space:create(#document{value = #od_space{name = Name}}),
    entity_graph:add_relation(
        od_user, UserId,
        od_space, SpaceId,
        privileges:space_admin()
    ),
%%  user_logic:set_space_name_mapping(UserId, SpaceId, Name, true),
    {ok, SpaceId};
create_impl({user, _UserId}, SpaceId, users, #{<<"userId">> := UserId}) ->
    entity_graph:add_relation(
        od_user, UserId,
        od_space, SpaceId,
        privileges:space_user()
    ),
    {ok, SpaceId}.


get_entity(SpaceId) ->
    case od_space:get(SpaceId) of
        {ok, #document{value = Space}} ->
            {ok, Space};
        _ ->
            ?EL_NOT_FOUND
    end.


% TODO czy issuer jest potrzebny?? Moze zrobic z tokena create?
get_internal({user, UserId}, SpaceId, _Space, space_invite_user_token) ->
    {ok, Token} = token_logic:create(
        #client{type = user, id = UserId},
        space_invite_user_token,
        {space, SpaceId}
    ),
    {ok, Token};
get_internal({user, _UserId}, _SpaceId, Space, users) ->
    {ok, Space#od_space.users}.


get_external({user, _UserId}, _) ->
    ok.


add_relation_impl({SpaceId, users}, od_user, UserId) ->
    entity_graph:add_relation(
        od_user, UserId,
        od_space, SpaceId,
        privileges:space_user()
    ),
    {ok, SpaceId}.


update_impl(SpaceId, Data) when is_binary(SpaceId) ->
    {ok, _} = od_space:update(SpaceId, fun(Space) ->
        #od_space{name = OldName} = Space,
        NewName = maps:get(<<"name">>, Data, OldName),
        {ok, Space#od_space{name = NewName}}
    end),
    ok.


delete_impl(SpaceId) when is_binary(SpaceId) ->
    ok = od_space:delete(SpaceId).


exists_impl(SpaceId, entity) when is_binary(SpaceId) ->
    {internal, fun(#od_space{}) ->
        % If the space with SpaceId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end};
exists_impl(SpaceId, users) when is_binary(SpaceId) ->
    {internal, fun(#od_space{}) ->
        % If the space with SpaceId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


authorize_impl({user, _UserId}, create, undefined, entity) ->
    true;

authorize_impl({user, UserId}, get, SpaceId, users) when is_binary(SpaceId) ->
    auth_by_privilege(UserId, space_view_data);
authorize_impl({user, UserId}, get, SpaceId, entity) when is_binary(SpaceId) ->
    auth_by_privilege(UserId, space_view_data);
authorize_impl({user, UserId}, get, SpaceId, space_invite_user_token) when is_binary(SpaceId) ->
    auth_by_privilege(UserId, space_invite_user);

authorize_impl({user, UserId}, update, SpaceId, entity) when is_binary(SpaceId) ->
    auth_by_privilege(UserId, space_change_data);

authorize_impl({user, UserId}, delete, SpaceId, entity) when is_binary(SpaceId) ->
    auth_by_privilege(UserId, space_remove);

authorize_impl({user, UserId}, create, SpaceId, users) when is_binary(SpaceId) ->
    auth_by_privilege(UserId, space_invite_user); %TODO admin privs

authorize_impl({user, _UserId}, consume_token, join_as_user, todo) ->
    true;
authorize_impl({user, _UserId}, consume_token, join_as_group, todo) ->
    true.


validate_impl(create, entity) -> #{
    required => #{
        <<"userId">> => {binary, fun(Value) -> user_logic:exists(Value) end}
    }
};
validate_impl(create, entity) -> #{
    required => #{
        <<"name">> => binary
    }
};
validate_impl(update, entity) -> #{
    required => #{
        <<"name">> => binary
    }
}.



auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_space{} = Space) ->
        has_eff_privilege(Space, UserId, Privilege)
    end}.


has_eff_privilege(SpaceId, UserId, Privilege) when is_binary(SpaceId) ->
    {ok, #document{value = Space}} = od_space:get(SpaceId),
    has_eff_privilege(Space, UserId, Privilege);
has_eff_privilege(#od_space{users = UsersPrivileges}, UserId, Privilege) ->
    % TODO eff_users
    UserPrivileges = proplists:get_value(UserId, UsersPrivileges, []),
    lists:member(Privilege, UserPrivileges).

