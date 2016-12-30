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

-include("errors.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").


-export([get_entity/1, create/4, get/4, update/3, delete/2]).
-export([exists/2, authorize/4, validate/2]).
-export([entity_to_string/1]).


get_entity(SpaceId) ->
    case od_space:get(SpaceId) of
        {ok, #document{value = Space}} ->
            {ok, Space};
        _ ->
            ?ERROR_NOT_FOUND
    end.


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
        {od_space, SpaceId}
    ),
    {ok, Token};
create(Client, SpaceId, invite_user_token, _) ->
    {ok, Token} = token_logic:create(
        Client,
        space_invite_user_token,
        {od_space, SpaceId}
    ),
    {ok, Token}.


get(_, undefined, undefined, list) ->
    {ok, SpaceDocs} = od_space:list(),
    {ok, [SpaceId || #document{key = SpaceId} <- SpaceDocs]};
get(?USER, _SpaceId, Space, users) ->
    {ok, Space#od_space.users}.



update(SpaceId, entity, Data) when is_binary(SpaceId) ->
    {ok, _} = od_space:update(SpaceId, fun(Space) ->
        #od_space{name = OldName} = Space,
        NewName = maps:get(<<"name">>, Data, OldName),
        {ok, Space#od_space{name = NewName}}
    end),
    ok;

update(SpaceId, {user, UserId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_user, UserId,
        od_space, SpaceId,
        {Operation, Privileges}
    );

update(SpaceId, {group, GroupId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_group, GroupId,
        od_space, SpaceId,
        {Operation, Privileges}
    ).


delete(SpaceId, entity) when is_binary(SpaceId) ->
    entity_graph:delete_with_relations(od_space, SpaceId);

delete(SpaceId, {user, UserId}) when is_binary(SpaceId) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_space, SpaceId
    );

delete(SpaceId, {group, ChildGroupId}) when is_binary(SpaceId) ->
    entity_graph:remove_relation(
        od_group, ChildGroupId,
        od_space, SpaceId
    ).


exists(undefined, _) ->
    true;
exists(SpaceId, _) when is_binary(SpaceId) ->
    % No matter the resource, return true if it belongs to a space
    {internal, fun(#od_space{}) ->
        % If the space with SpaceId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


authorize(create, undefined, entity, ?USER) ->
    true;
authorize(create, _SpaceId, users, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_SPACES_ADD_MEMBERS);
authorize(create, _SpaceId, groups, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_SPACES_ADD_MEMBERS);
authorize(create, _SpaceId, invite_provider_token, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_INVITE_PROVIDER);
authorize(create, _SpaceId, invite_user_token, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_INVITE_USER);


authorize(get, undefined, list, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, list_spaces);
authorize(get, _SpaceId, users, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_VIEW);
authorize(get, _SpaceId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_VIEW);


authorize(update, _SpaceId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_UPDATE);

authorize(update, _SpaceId, {user, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_SET_PRIVILEGES);

authorize(update, _SpaceId, {group, _GroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_SET_PRIVILEGES);


authorize(delete, _SpaceId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_DELETE);

authorize(delete, _SpaceId, {user, _UserId}, ?USER(UserId)) -> [
    auth_by_privilege(UserId, ?SPACE_REMOVE_USER),
    auth_by_oz_privilege(UserId, ?OZ_SPACES_REMOVE_MEMBERS)
];

authorize(delete, _SpaceId, {group, _UserId}, ?USER(UserId)) -> [
    auth_by_privilege(UserId, ?SPACE_REMOVE_GROUP),
    auth_by_oz_privilege(UserId, ?OZ_SPACES_REMOVE_MEMBERS)
].


validate(create, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty}
    }
};
validate(create, users) -> #{
    required => #{
        <<"userId">> => {binary, {exists, fun(Value) ->
            n_user_logic:exists(Value)
        end}}
    }
};
validate(create, groups) -> #{
    required => #{
        <<"groupId">> => {binary, {exists, fun(Value) ->
            n_group_logic:exists(Value)
        end}}
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
};
validate(update, Member) when Member =:= user orelse Member =:= group -> #{
    required => #{
        <<"privileges">> => {list_of_atoms, privileges:space_privileges()}
    },
    optional => #{
        <<"operation">> => {atom, [set, grant, revoke]}
    }
}.


entity_to_string(SpaceId) ->
    od_space:to_string(SpaceId).



auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_space{} = Space) ->
        n_space_logic:has_eff_privilege(Space, UserId, Privilege)
    end}.


auth_by_oz_privilege(UserId, Privilege) ->
    {external, fun() ->
        n_user_logic:has_eff_oz_privilege(UserId, Privilege)
    end}.

