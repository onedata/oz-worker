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
-behaviour(entity_logic_plugin_behaviour).

-include("entity_logic.hrl").
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").


-export([create/4, get_entity/1, get_internal/4, get_external/2, update/2,
    delete/1]).
-export([exists/2, authorize/5, validate/2]).


create(?ROOT, _, entity, #{<<"name">> := Name}) ->
    od_space:create(#document{value = #od_space{name = Name}});
create(?USER(UserId), _, entity, #{<<"name">> := Name}) ->
    {ok, SpaceId} = od_space:create(#document{value = #od_space{name = Name}}),
    entity_graph:add_relation(
        od_user, UserId,
        od_space, SpaceId,
        privileges:space_admin()
    ),
%% TODO user_logic:set_space_name_mapping(UserId, SpaceId, Name, true),
    {ok, SpaceId};
create(_Client, SpaceId, users, #{<<"userId">> := UserId}) ->
    entity_graph:add_relation(
        od_user, UserId,
        od_space, SpaceId,
        privileges:space_user()
    ),
    {ok, UserId};
create(_Client, SpaceId, groups, #{<<"groupId">> := GroupId}) ->
    entity_graph:add_relation(
        od_group, GroupId,
        od_space, SpaceId,
        privileges:space_user()
    ),
    {ok, GroupId};
create(Client, SpaceId, invite_provider_token, _) ->
    {ok, Token} = token_logic:create(
        Client,
        space_support_token,
        {space, SpaceId}
    ),
    {ok, Token};
create(Client, SpaceId, invite_user_token, _) ->
    {ok, Token} = token_logic:create(
        Client,
        space_invite_user_token,
        {space, SpaceId}
    ),
    {ok, Token}.


get_entity(SpaceId) ->
    case od_space:get(SpaceId) of
        {ok, #document{value = Space}} ->
            {ok, Space};
        _ ->
            ?ERROR_NOT_FOUND
    end.


get_internal(?USER, _SpaceId, Space, users) ->
    {ok, Space#od_space.users}.



get_external(_, list) ->
    {ok, SpaceDocs} = od_space:list(),
    {ok, [SpaceId || #document{key = SpaceId} <- SpaceDocs]};
get_external(?USER, _) ->
    ok.


update(SpaceId, Data) when is_binary(SpaceId) ->
    {ok, _} = od_space:update(SpaceId, fun(Space) ->
        #od_space{name = OldName} = Space,
        NewName = maps:get(<<"name">>, Data, OldName),
        {ok, Space#od_space{name = NewName}}
    end),
    ok.


delete(SpaceId) when is_binary(SpaceId) ->
    ok = od_space:delete(SpaceId).


exists(undefined, entity) ->
    true;
exists(undefined, list) ->
    true;
exists(SpaceId, _) when is_binary(SpaceId) ->
    % No matter the resource, return true if it belongs to a space
    {internal, fun(#od_space{}) ->
        % If the space with SpaceId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


authorize(create, undefined, entity,?USER,  _) ->
    true;
authorize(create, _SpaceId, users, ?USER(UserId), _) ->
    auth_by_oz_privilege(UserId, add_member_to_space);
authorize(create, _SpaceId, groups, ?USER(UserId), _) ->
    auth_by_oz_privilege(UserId, add_member_to_space);
authorize(create, _SpaceId, invite_provider_token, ?USER(UserId), _) ->
    auth_by_privilege(UserId, space_add_provider);
authorize(create, _SpaceId, invite_user_token, ?USER(UserId), _) ->
    auth_by_privilege(UserId, space_invite_user);

authorize(get, undefined, list, ?USER(UserId), _) ->
    n_user_logic:has_eff_oz_privilege(UserId, list_spaces);
authorize(get, _SpaceId, users, ?USER(UserId), _) ->
    auth_by_privilege(UserId, space_view_data);
authorize(get, _SpaceId, entity, ?USER(UserId), _) ->
    auth_by_privilege(UserId, space_view_data);

authorize(update, _SpaceId, entity, ?USER(UserId), _) ->
    auth_by_privilege(UserId, space_change_data);

authorize(delete, _SpaceId, entity, ?USER(UserId), _) ->
    auth_by_privilege(UserId, space_remove).


validate(create, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty}
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
validate(create, invite_provider_token) -> #{
};
validate(create, invite_user_token) -> #{
};
validate(update, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty}
    }
}.



auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_space{} = Space) ->
       n_space_logic:has_eff_privilege(Space, UserId, Privilege)
    end}.


auth_by_oz_privilege(UserId, Privilege) ->
    {external, fun() ->
        n_user_logic:has_eff_oz_privilege(UserId, Privilege)
    end}.

