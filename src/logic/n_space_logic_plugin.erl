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
-include("tokens.hrl").
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


% TODO VFS-2918
create(_Client, SpaceId, {deprecated_user_privileges, UserId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_user, UserId,
        od_space, SpaceId,
        {Operation, Privileges}
    );
% TODO VFS-2918
create(_Client, SpaceId, {deprecated_child_privileges, GroupId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_group, GroupId,
        od_space, SpaceId,
        {Operation, Privileges}
    );
% TODO VFS-2918
create(Client, SpaceId, {create_share, ShareId}, Data) ->
    case n_share_logic:exists(ShareId) of
        true ->
            ?ERROR_BAD_VALUE_ID_OCCUPIED(<<"shareId">>);
        false ->
            n_share_logic_plugin:create(Client, undefined, entity, Data#{
                <<"spaceId">> => SpaceId,
                <<"shareId">> => ShareId
            })
    end;

create(Client, _, entity, #{<<"name">> := Name}) ->
    {ok, SpaceId} = od_space:create(#document{value = #od_space{name = Name}}),
    case Client of
        ?USER(UserId) ->
            entity_graph:add_relation(
                od_user, UserId,
                od_space, SpaceId,
                privileges:space_admin()
            );
        _ ->
            ok
    end,
    {ok, SpaceId};

create(Client, SpaceId, invite_user_token, _) ->
    {ok, Token} = token_logic:create(
        Client,
        ?SPACE_INVITE_USER_TOKEN,
        {od_space, SpaceId}
    ),
    {ok, Token};

create(Client, SpaceId, invite_group_token, _) ->
    {ok, Token} = token_logic:create(
        Client,
        ?SPACE_INVITE_GROUP_TOKEN,
        {od_space, SpaceId}
    ),
    {ok, Token};

create(Client, SpaceId, invite_provider_token, _) ->
    {ok, Token} = token_logic:create(
        Client,
        ?SPACE_SUPPORT_TOKEN,
        {od_space, SpaceId}
    ),
    {ok, Token};

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
    {ok, GroupId}.


% TODO VFS-2918 - remove Client from get when these are not needed
% TODO VFS-2918
get(Client, SpaceId, _, deprecated_invite_user_token) ->
    {ok, Token} = token_logic:create(
        Client,
        ?SPACE_INVITE_USER_TOKEN,
        {od_space, SpaceId}
    ),
    {ok, Token};
% TODO VFS-2918
get(Client, SpaceId, _, deprecated_invite_group_token) ->
    {ok, Token} = token_logic:create(
        Client,
        ?SPACE_INVITE_GROUP_TOKEN,
        {od_space, SpaceId}
    ),
    {ok, Token};
% TODO VFS-2918
get(Client, SpaceId, _, deprecated_invite_provider_token) ->
    {ok, Token} = token_logic:create(
        Client,
        ?SPACE_SUPPORT_TOKEN,
        {od_space, SpaceId}
    ),
    {ok, Token};

get(_, undefined, undefined, list) ->
    {ok, SpaceDocs} = od_space:list(),
    {ok, [SpaceId || #document{key = SpaceId} <- SpaceDocs]};
get(_, _SpaceId, #od_space{name = Name}, data) ->
    {ok, #{<<"name">> => Name}};

get(_, _SpaceId, #od_space{users = Users}, users) ->
    {ok, Users};
get(_, _SpaceId, #od_space{eff_users = Users}, eff_users) ->
    {ok, Users};
get(_, _SpaceId, #od_space{}, {user, UserId}) ->
    {ok, User} = ?throw_on_failure(n_user_logic_plugin:get_entity(UserId)),
    n_user_logic_plugin:get(?ROOT, UserId, User, data);
get(_, _SpaceId, #od_space{}, {eff_user, UserId}) ->
    {ok, User} = ?throw_on_failure(n_user_logic_plugin:get_entity(UserId)),
    n_user_logic_plugin:get(?ROOT, UserId, User, data);
get(_, _SpaceId, #od_space{users = Users}, {user_privileges, UserId}) ->
    {ok, maps:get(UserId, Users)};
get(_, _SpaceId, #od_space{eff_users = Users}, {eff_user_privileges, UserId}) ->
    {Privileges, _} = maps:get(UserId, Users),
    {ok, Privileges};

get(_, _SpaceId, #od_space{groups = Groups}, groups) ->
    {ok, Groups};
get(_, _SpaceId, #od_space{eff_groups = Groups}, eff_groups) ->
    {ok, Groups};
get(_, _SpaceId, #od_space{}, {group, GroupId}) ->
    {ok, Group} = ?throw_on_failure(n_group_logic_plugin:get_entity(GroupId)),
    n_group_logic_plugin:get(?ROOT, GroupId, Group, data);
get(_, _SpaceId, #od_space{}, {eff_group, GroupId}) ->
    {ok, Group} = ?throw_on_failure(n_group_logic_plugin:get_entity(GroupId)),
    n_group_logic_plugin:get(?ROOT, GroupId, Group, data);
get(_, _SpaceId, #od_space{groups = Groups}, {group_privileges, GroupId}) ->
    {ok, maps:get(GroupId, Groups)};
get(_, _SpaceId, #od_space{eff_groups = Groups}, {eff_group_privileges, GroupId}) ->
    {Privileges, _} = maps:get(GroupId, Groups),
    {ok, Privileges};

get(_, _SpaceId, #od_space{shares = Shares}, shares) ->
    {ok, Shares};
get(_, _SpaceId, #od_space{}, {share, ShareId}) ->
    {ok, Share} = ?throw_on_failure(n_share_logic_plugin:get_entity(ShareId)),
    n_share_logic_plugin:get(?ROOT, ShareId, Share, data).



update(SpaceId, entity, #{<<"name">> := NewName}) ->
    {ok, _} = od_space:update(SpaceId, #{name => NewName}),
    ok;

update(SpaceId, {user_privileges, UserId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_user, UserId,
        od_space, SpaceId,
        {Operation, Privileges}
    );

update(SpaceId, {group_privileges, GroupId}, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_relation(
        od_group, GroupId,
        od_space, SpaceId,
        {Operation, Privileges}
    ).


delete(SpaceId, entity) ->
    entity_graph:delete_with_relations(od_space, SpaceId);

delete(SpaceId, {user, UserId}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_space, SpaceId
    );

delete(SpaceId, {group, GroupId}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_space, SpaceId
    ).


exists(undefined, _) ->
    true;

exists(_SpaceId, {user, UserId}) ->
    {internal, fun(#od_space{users = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists(_SpaceId, {eff_user, UserId}) ->
    {internal, fun(#od_space{eff_users = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists(_SpaceId, {user_privileges, UserId}) ->
    {internal, fun(#od_space{users = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists(_SpaceId, {eff_user_privileges, UserId}) ->
    {internal, fun(#od_space{eff_users = Users}) ->
        maps:is_key(UserId, Users)
    end};

exists(_SpaceId, {group, UserId}) ->
    {internal, fun(#od_space{groups = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists(_SpaceId, {eff_group, UserId}) ->
    {internal, fun(#od_space{eff_groups = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists(_SpaceId, {group_privileges, UserId}) ->
    {internal, fun(#od_space{groups = Users}) ->
        maps:is_key(UserId, Users)
    end};
exists(_SpaceId, {eff_group_privileges, UserId}) ->
    {internal, fun(#od_space{eff_groups = Users}) ->
        maps:is_key(UserId, Users)
    end};

exists(_SpaceId, {share, ProviderId}) ->
    {internal, fun(#od_space{shares = Providers}) ->
        maps:is_key(ProviderId, Providers)
    end};

exists(_SpaceId, {provider, ProviderId}) ->
    {internal, fun(#od_space{providers = Providers}) ->
        maps:is_key(ProviderId, Providers)
    end};

exists(_SpaceId, _) ->
    % No matter the resource, return true if it belongs to a space
    {internal, fun(#od_space{}) ->
        % If the space with SpaceId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


% TODO VFS-2918
authorize(create, _GroupId, {create_share, _ShareId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_MANAGE_SHARES);
% TODO VFS-2918
authorize(get, _GroupId, deprecated_invite_user_token, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_INVITE_USER);
% TODO VFS-2918
authorize(get, _GroupId, deprecated_invite_group_token, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_INVITE_GROUP);
% TODO VFS-2918
authorize(get, _GroupId, deprecated_invite_provider_token, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_INVITE_PROVIDER);
% TODO VFS-2918
authorize(create, _GroupId, {deprecated_user_privileges, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_SET_PRIVILEGES);
% TODO VFS-2918
authorize(create, _GroupId, {deprecated_child_privileges, _ChildGroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_SET_PRIVILEGES);

authorize(create, undefined, entity, ?USER) ->
    true;

authorize(create, _SpaceId, invite_user_token, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_INVITE_USER);

authorize(create, _SpaceId, invite_group_token, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_INVITE_GROUP);

authorize(create, _SpaceId, invite_provider_token, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_INVITE_PROVIDER);

authorize(create, _SpaceId, users, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_SPACES_ADD_MEMBERS);

authorize(create, _SpaceId, groups, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_SPACES_ADD_MEMBERS);


authorize(get, undefined, list, ?USER(UserId)) ->
    n_user_logic:has_eff_oz_privilege(UserId, ?OZ_SPACES_LIST);

authorize(get, _SpaceId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_VIEW);

authorize(get, _SpaceId, data, ?USER(UserId)) ->
    auth_by_membership(UserId);

authorize(get, _SpaceId, data, ?PROVIDER(ProviderId)) ->
    auth_by_support(ProviderId);

authorize(get, _SpaceId, shares, ?USER(UserId)) ->
    auth_by_membership(UserId);

authorize(get, _SpaceId, shares, ?PROVIDER(ProviderId)) ->
    auth_by_support(ProviderId);

authorize(get, _SpaceId, {share, _ShareId}, ?USER(UserId)) ->
    auth_by_membership(UserId);

authorize(get, _SpaceId, {share, _ShareId}, ?PROVIDER(ProviderId)) ->
    auth_by_support(ProviderId);

authorize(get, _SpaceId, _, ?USER(UserId)) ->
    % All other resources can be accessed with view privileges
    auth_by_privilege(UserId, ?SPACE_VIEW);


authorize(update, _SpaceId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_UPDATE);

authorize(update, _SpaceId, {user_privileges, _UserId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_SET_PRIVILEGES);

authorize(update, _SpaceId, {group_privileges, _GroupId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_SET_PRIVILEGES);


authorize(delete, _SpaceId, entity, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_DELETE);

authorize(delete, _SpaceId, {user, _UserId}, ?USER(UserId)) -> [
    auth_by_privilege(UserId, ?SPACE_REMOVE_USER),
    auth_by_oz_privilege(UserId, ?OZ_SPACES_REMOVE_MEMBERS)
];

authorize(delete, _SpaceId, {group, _GroupId}, ?USER(UserId)) -> [
    auth_by_privilege(UserId, ?SPACE_REMOVE_GROUP),
    auth_by_oz_privilege(UserId, ?OZ_SPACES_REMOVE_MEMBERS)
];

authorize(delete, _SpaceId, {provider, _ProviderId}, ?USER(UserId)) ->
    auth_by_privilege(UserId, ?SPACE_REMOVE_PROVIDER).


% TODO VFS-2918
validate(create, {create_share, _ShareId}) -> #{
    required => #{
        <<"name">> => {binary, non_empty},
        <<"rootFileId">> => {binary, non_empty}
    }
};

validate(create, entity) -> #{
    required => #{
        <<"name">> => {binary, non_empty}
    }
};
validate(create, invite_user_token) -> #{
};
validate(create, invite_group_token) -> #{
};
validate(create, invite_provider_token) -> #{
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


auth_by_membership(UserId) ->
    {internal, fun(#od_space{eff_users = EffUsers}) ->
        maps:is_key(UserId, EffUsers)
    end}.


auth_by_support(ProviderId) ->
    {internal, fun(#od_space{providers = Providers}) ->
        maps:is_key(ProviderId, Providers)
    end}.


auth_by_privilege(UserId, Privilege) ->
    {internal, fun(#od_space{} = Space) ->
        n_space_logic:has_eff_privilege(Space, UserId, Privilege)
    end}.


auth_by_oz_privilege(UserId, Privilege) ->
    {external, fun() ->
        n_user_logic:has_eff_oz_privilege(UserId, Privilege)
    end}.

