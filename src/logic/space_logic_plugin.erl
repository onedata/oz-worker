%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_space model.
%%% @end
%%%-------------------------------------------------------------------
-module(space_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("tokens.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/api_errors.hrl").

-export([fetch_entity/1, operation_supported/3]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, validate/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity from datastore based on its EntityId.
%% Should return ?ERROR_NOT_FOUND if the entity does not exist.
%% @end
%%--------------------------------------------------------------------
-spec fetch_entity(entity_logic:entity_id()) ->
    {ok, entity_logic:entity()} | entity_logic:error().
fetch_entity(SpaceId) ->
    case od_space:get(SpaceId) of
        {ok, #document{value = Space}} ->
            {ok, Space};
        _ ->
            ?ERROR_NOT_FOUND
    end.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given operation is supported based on operation, aspect and
%% scope (entity type is known based on the plugin itself).
%% @end
%%--------------------------------------------------------------------
-spec operation_supported(entity_logic:operation(), entity_logic:aspect(),
    entity_logic:scope()) -> boolean().
operation_supported(create, invite_user_token, private) -> true;
operation_supported(create, invite_group_token, private) -> true;
operation_supported(create, invite_provider_token, private) -> true;

operation_supported(create, instance, private) -> true;
operation_supported(create, join, private) -> true;

operation_supported(create, {user, _}, private) -> true;
operation_supported(create, {group, _}, private) -> true;

operation_supported(get, list, private) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, instance, protected) -> true;

operation_supported(get, users, private) -> true;
operation_supported(get, eff_users, private) -> true;
operation_supported(get, {user_privileges, _}, private) -> true;
operation_supported(get, {eff_user_privileges, _}, private) -> true;

operation_supported(get, groups, private) -> true;
operation_supported(get, eff_groups, private) -> true;
operation_supported(get, {group_privileges, _}, private) -> true;
operation_supported(get, {eff_group_privileges, _}, private) -> true;

operation_supported(get, shares, private) -> true;

operation_supported(get, providers, private) -> true;

operation_supported(update, instance, private) -> true;
operation_supported(update, {user_privileges, _}, private) -> true;
operation_supported(update, {group_privileges, _}, private) -> true;

operation_supported(delete, instance, private) -> true;
operation_supported(delete, {user, _}, private) -> true;
operation_supported(delete, {group, _}, private) -> true;
operation_supported(delete, {provider, _}, private) -> true;

operation_supported(_, _, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(Req = #el_req{gri = #gri{id = undefined, aspect = instance} = GRI}) ->
    #{<<"name">> := Name} = Req#el_req.data,
    {ok, #document{key = SpaceId}} = od_space:create(#document{
        value = #od_space{name = Name}
    }),
    case Req#el_req.auth_hint of
        ?AS_USER(UserId) ->
            entity_graph:add_relation(
                od_user, UserId,
                od_space, SpaceId,
                privileges:space_admin()
            );
        ?AS_GROUP(GroupId) ->
            entity_graph:add_relation(
                od_group, GroupId,
                od_space, SpaceId,
                privileges:space_admin()
            );
        _ ->
            ok
    end,
    % Space has been modified by adding relation, so it will need to be
    % fetched again.
    {ok, {not_fetched, GRI#gri{id = SpaceId}}};

create(Req = #el_req{gri = #gri{id = undefined, aspect = join}}) ->
    Macaroon = maps:get(<<"token">>, Req#el_req.data),
    {ok, {od_space, SpaceId}} = token_logic:consume(Macaroon),
    % In the future, privileges can be included in token
    Privileges = privileges:space_user(),
    case Req#el_req.auth_hint of
        ?AS_USER(UserId) ->
            entity_graph:add_relation(
                od_user, UserId,
                od_space, SpaceId,
                Privileges
            );
        ?AS_GROUP(GroupId) ->
            entity_graph:add_relation(
                od_group, GroupId,
                od_space, SpaceId,
                Privileges
            );
        _ ->
            ok
    end,
    NewGRI = case lists:member(?SPACE_VIEW, Privileges) of
        true ->
            #gri{type = od_space, id = SpaceId, aspect = instance, scope = private};
        false ->
            #gri{type = od_space, id = SpaceId, aspect = instance, scope = protected}
    end,
    {ok, {not_fetched, NewGRI}};

create(Req = #el_req{gri = #gri{id = SpaceId, aspect = invite_user_token}}) ->
    {ok, Macaroon} = token_logic:create(
        Req#el_req.client,
        ?SPACE_INVITE_USER_TOKEN,
        {od_space, SpaceId}
    ),
    {ok, {data, Macaroon}};

create(Req = #el_req{gri = #gri{id = SpaceId, aspect = invite_group_token}}) ->
    {ok, Macaroon} = token_logic:create(
        Req#el_req.client,
        ?SPACE_INVITE_GROUP_TOKEN,
        {od_space, SpaceId}
    ),
    {ok, {data, Macaroon}};

create(Req = #el_req{gri = #gri{id = SpaceId, aspect = invite_provider_token}}) ->
    {ok, Macaroon} = token_logic:create(
        Req#el_req.client,
        ?SPACE_SUPPORT_TOKEN,
        {od_space, SpaceId}
    ),
    {ok, {data, Macaroon}};

create(#el_req{gri = #gri{id = SpaceId, aspect = {user, UserId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:space_user()),
    entity_graph:add_relation(
        od_user, UserId,
        od_space, SpaceId,
        Privileges
    ),
    NewGRI = #gri{type = od_user, id = UserId, aspect = instance, scope = shared},
    {ok, {not_fetched, NewGRI, ?THROUGH_SPACE(SpaceId)}};

create(#el_req{gri = #gri{id = SpaceId, aspect = {group, GroupId}}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data, privileges:space_user()),
    entity_graph:add_relation(
        od_group, GroupId,
        od_space, SpaceId,
        Privileges
    ),
    NewGRI = #gri{type = od_group, id = GroupId, aspect = instance, scope = shared},
    {ok, {not_fetched, NewGRI, ?THROUGH_SPACE(SpaceId)}}.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = list}}, _) ->
    {ok, SpaceDocs} = od_space:list(),
    {ok, [SpaceId || #document{key = SpaceId} <- SpaceDocs]};

get(#el_req{gri = #gri{aspect = instance, scope = private}}, Space) ->
    {ok, Space};
get(#el_req{gri = #gri{aspect = instance, scope = protected}}, Space) ->
    #od_space{name = Name, providers = Providers} = Space,
    {ok, #{
        <<"name">> => Name,
        <<"providers">> => Providers
    }};

get(#el_req{gri = #gri{aspect = users}}, Space) ->
    {ok, maps:keys(Space#od_space.users)};
get(#el_req{gri = #gri{aspect = eff_users}}, Space) ->
    {ok, maps:keys(Space#od_space.eff_users)};
get(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, Space) ->
    {ok, maps:get(UserId, Space#od_space.users)};
get(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, Space) ->
    {Privileges, _} = maps:get(UserId, Space#od_space.eff_users),
    {ok, Privileges};

get(#el_req{gri = #gri{aspect = groups}}, Space) ->
    {ok, maps:keys(Space#od_space.groups)};
get(#el_req{gri = #gri{aspect = eff_groups}}, Space) ->
    {ok, maps:keys(Space#od_space.eff_groups)};
get(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, Space) ->
    {ok, maps:get(GroupId, Space#od_space.groups)};
get(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, Space) ->
    {Privileges, _} = maps:get(GroupId, Space#od_space.eff_groups),
    {ok, Privileges};

get(#el_req{gri = #gri{aspect = shares}}, Space) ->
    {ok, Space#od_space.shares};

get(#el_req{gri = #gri{aspect = providers}}, Space) ->
    {ok, maps:keys(Space#od_space.providers)}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = SpaceId, aspect = instance}, data = Data}) ->
    NewName = maps:get(<<"name">>, Data),
    {ok, _} = od_space:update(SpaceId, fun(Space = #od_space{}) ->
        {ok, Space#od_space{name = NewName}}
    end),
    ok;

update(Req = #el_req{gri = #gri{id = SpaceId, aspect = {user_privileges, UserId}}}) ->
    Privileges = maps:get(<<"privileges">>, Req#el_req.data),
    Operation = maps:get(<<"operation">>, Req#el_req.data, set),
    entity_graph:update_relation(
        od_user, UserId,
        od_space, SpaceId,
        {Operation, Privileges}
    );

update(Req = #el_req{gri = #gri{id = SpaceId, aspect = {group_privileges, GroupId}}}) ->
    Privileges = maps:get(<<"privileges">>, Req#el_req.data),
    Operation = maps:get(<<"operation">>, Req#el_req.data, set),
    entity_graph:update_relation(
        od_group, GroupId,
        od_space, SpaceId,
        {Operation, Privileges}
    ).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = SpaceId, aspect = instance}}) ->
    entity_graph:delete_with_relations(od_space, SpaceId);

delete(#el_req{gri = #gri{id = SpaceId, aspect = {user, UserId}}}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_space, SpaceId
    );

delete(#el_req{gri = #gri{id = SpaceId, aspect = {group, GroupId}}}) ->
    entity_graph:remove_relation(
        od_group, GroupId,
        od_space, SpaceId
    );

delete(#el_req{gri = #gri{id = SpaceId, aspect = {provider, ProviderId}}}) ->
    entity_graph:remove_relation(
        od_space, SpaceId,
        od_provider, ProviderId
    ).


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(Req = #el_req{gri = #gri{aspect = instance, scope = protected}}, Space) ->
    case Req#el_req.auth_hint of
        ?THROUGH_USER(UserId) ->
            space_logic:has_eff_user(Space, UserId);
        ?THROUGH_GROUP(GroupId) ->
            space_logic:has_eff_group(Space, GroupId);
        ?THROUGH_PROVIDER(ProviderId) ->
            space_logic:has_provider(Space, ProviderId);
        undefined ->
            true
    end;

exists(#el_req{gri = #gri{aspect = {user, UserId}}}, Space) ->
    maps:is_key(UserId, Space#od_space.users);

exists(#el_req{gri = #gri{aspect = {user_privileges, UserId}}}, Space) ->
    maps:is_key(UserId, Space#od_space.users);

exists(#el_req{gri = #gri{aspect = {eff_user_privileges, UserId}}}, Space) ->
    maps:is_key(UserId, Space#od_space.eff_users);

exists(#el_req{gri = #gri{aspect = {group, GroupId}}}, Space) ->
    maps:is_key(GroupId, Space#od_space.groups);

exists(#el_req{gri = #gri{aspect = {group_privileges, GroupId}}}, Space) ->
    maps:is_key(GroupId, Space#od_space.groups);

exists(#el_req{gri = #gri{aspect = {eff_group_privileges, GroupId}}}, Space) ->
    maps:is_key(GroupId, Space#od_space.eff_groups);

exists(#el_req{gri = #gri{aspect = {provider, ProviderId}}}, Space) ->
    maps:is_key(ProviderId, Space#od_space.providers);

% All other aspects exist if space record exists.
exists(#el_req{gri = #gri{id = Id}}, #od_space{}) ->
    Id =/= undefined.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(Req = #el_req{operation = create, gri = #gri{aspect = instance}}, _) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            true;
        {?USER(UserId), ?AS_GROUP(GroupId)} ->
            group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_CREATE_SPACE);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = join}}, _) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?AS_USER(UserId)} ->
            true;
        {?USER(UserId), ?AS_GROUP(GroupId)} ->
            group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_JOIN_SPACE);
        _ ->
            false
    end;

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_user_token}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_INVITE_USER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_group_token}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_INVITE_GROUP);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = invite_provider_token}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_INVITE_PROVIDER);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {user, _}}}, _) ->
    user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_SPACES_ADD_MEMBERS);

authorize(Req = #el_req{operation = create, gri = #gri{aspect = {group, _}}}, _) ->
    user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_SPACES_ADD_MEMBERS);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = list}}, _) ->
    user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_SPACES_LIST);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = instance, scope = private}}, Space) ->
    case Req#el_req.client of
        ?USER(UserId) ->
            auth_by_privilege(UserId, Space, ?SPACE_VIEW);
        ?PROVIDER(ProviderId) ->
            space_logic:has_provider(Space, ProviderId)
    end;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = instance, scope = protected}}, Space) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(UserId), ?THROUGH_USER(UserId)} ->
            % User's membership in this space is checked in 'exists'
            true;

        {?USER(_UserId), ?THROUGH_USER(_OtherUserId)} ->
            false;

        {?USER(ClientUserId), ?THROUGH_GROUP(GroupId)} ->
            % Groups's membership in this space is checked in 'exists'
            group_logic:has_eff_privilege(GroupId, ClientUserId, ?GROUP_VIEW);

        {?PROVIDER(ProviderId), ?THROUGH_PROVIDER(ProviderId)} ->
            % Provider's support in this space is checked in 'exists'
            true;

        {?PROVIDER(_ProviderId), ?THROUGH_PROVIDER(_OtherProviderId)} ->
            false;

        {?USER(ClientUserId), ?THROUGH_PROVIDER(_ProviderId)} ->
            % Provider's support in this space is checked in 'exists'
            user_logic:has_eff_oz_privilege(ClientUserId, ?OZ_PROVIDERS_LIST_SPACES);

        {?USER(ClientUserId), _} ->
            auth_by_membership(ClientUserId, Space) orelse
                user_logic_plugin:auth_by_oz_privilege(ClientUserId, ?OZ_SPACES_LIST);

        _ ->
            % Access to private data also allows access to protected data
            authorize(Req#el_req{gri = #gri{scope = private}}, Space)
    end;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = shares}}, Space) ->
    auth_by_membership(Req, Space);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = users}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_VIEW) orelse
        user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_SPACES_LIST_USERS);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = eff_users}}, Space) ->
    authorize(Req#el_req{operation = get, gri = #gri{aspect = users}}, Space);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = groups}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_VIEW) orelse
        user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_SPACES_LIST_GROUPS);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = eff_groups}}, Space) ->
    authorize(Req#el_req{operation = get, gri = #gri{aspect = groups}}, Space);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = providers}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_VIEW) orelse
        user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_SPACES_LIST_PROVIDERS);

authorize(Req = #el_req{operation = get, client = ?USER}, Space) ->
    % All other resources can be accessed with view privileges
    auth_by_privilege(Req, Space, ?SPACE_VIEW);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = instance}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_UPDATE);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_SET_PRIVILEGES);

authorize(Req = #el_req{operation = update, gri = #gri{aspect = {group_privileges, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_SET_PRIVILEGES);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = instance}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_DELETE);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {user, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_REMOVE_USER) orelse
        user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_SPACES_REMOVE_MEMBERS);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {group, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_REMOVE_GROUP) orelse
        user_logic_plugin:auth_by_oz_privilege(Req, ?OZ_SPACES_REMOVE_MEMBERS);

authorize(Req = #el_req{operation = delete, gri = #gri{aspect = {provider, _}}}, Space) ->
    auth_by_privilege(Req, Space, ?SPACE_REMOVE_PROVIDER);

authorize(_, _) ->
    false.


%%--------------------------------------------------------------------
%% @doc
%% Returns validity verificators for given request.
%% Returns a map with 'required', 'optional' and 'at_least_one' keys.
%% Under each of them, there is a map:
%%      Key => {type_verificator, value_verificator}
%% Which means how value of given Key should be validated.
%% @end
%%--------------------------------------------------------------------
-spec validate(entity_logic:req()) -> entity_logic:validity_verificator().
validate(#el_req{operation = create, gri = #gri{aspect = instance}}) -> #{
    required => #{
        <<"name">> => {binary, name}
    }
};

validate(Req = #el_req{operation = create, gri = #gri{aspect = join}}) ->
    TokenType = case Req#el_req.auth_hint of
        ?AS_USER(_) -> ?SPACE_INVITE_USER_TOKEN;
        ?AS_GROUP(_) -> ?SPACE_INVITE_GROUP_TOKEN
    end,
    #{
        required => #{
            <<"token">> => {token, TokenType}
        }
    };

validate(#el_req{operation = create, gri = #gri{aspect = invite_user_token}}) ->
    #{
    };

validate(#el_req{operation = create, gri = #gri{aspect = invite_group_token}}) ->
    #{
    };

validate(#el_req{operation = create, gri = #gri{aspect = invite_provider_token}}) ->
    #{
    };

validate(#el_req{operation = create, gri = #gri{aspect = {user, _}}}) -> #{
    required => #{
        {aspect, <<"userId">>} => {any, {exists, fun(UserId) ->
            user_logic:exists(UserId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:space_privileges()}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = {group, _}}}) -> #{
    required => #{
        {aspect, <<"groupId">>} => {any, {exists, fun(GroupId) ->
            group_logic:exists(GroupId) end}
        }
    },
    optional => #{
        <<"privileges">> => {list_of_atoms, privileges:space_privileges()}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    required => #{
        <<"name">> => {binary, name}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = {user_privileges, _}}}) ->
    #{
        required => #{
            <<"privileges">> => {list_of_atoms, privileges:space_privileges()}
        },
        optional => #{
            <<"operation">> => {atom, [set, grant, revoke]}
        }
    };

validate(#el_req{operation = update, gri = #gri{aspect = {group_privileges, Id}}}) ->
    validate(#el_req{operation = update, gri = #gri{aspect = {user_privileges, Id}}}).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns if given user belongs to the space represented by entity.
%% UserId is either given explicitly or derived from entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_membership(entity_logic:req() | od_user:id(), od_space:info()) ->
    boolean().
auth_by_membership(#el_req{client = ?USER(UserId)}, Space) ->
    auth_by_membership(UserId, Space);
auth_by_membership(#el_req{client = _OtherClient}, _Space) ->
    false;
auth_by_membership(UserId, #od_space{eff_users = EffUsers}) ->
    maps:is_key(UserId, EffUsers).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns if given user has specific effective privilege in the space.
%% UserId is either given explicitly or derived from entity logic request.
%% Clients of type other than user are discarded.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_privilege(entity_logic:req() | od_user:id(),
    od_space:id() | od_space:info(), privileges:space_privilege()) -> boolean().
auth_by_privilege(#el_req{client = ?USER(UserId)}, SpaceOrId, Privilege) ->
    auth_by_privilege(UserId, SpaceOrId, Privilege);
auth_by_privilege(#el_req{client = _OtherClient}, _SpaceOrId, _Privilege) ->
    false;
auth_by_privilege(UserId, SpaceOrId, Privilege) ->
    space_logic:has_eff_privilege(SpaceOrId, UserId, Privilege).
