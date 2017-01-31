%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements entity logic plugin behaviour and handles
%%% entity logic operations corresponding to od_user model.
%%% @end
%%%-------------------------------------------------------------------
-module(n_user_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("errors.hrl").
-include("tokens.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/utils/utils.hrl").

-type resource() :: deprecated_default_space | % TODO VFS-2918
authorize | entity | data | list |
client_tokens | {client_token, binary()} |
default_space | {space_alias, od_space:id()} |
default_provider |
oz_privileges | eff_oz_privileges |
create_group | create_space | create_handle_service | create_handle |
join_group | join_space |
groups | eff_groups | {group, od_group:id()} | {eff_group, od_group:id()} |
spaces | eff_spaces | {space, od_space:id()} | {eff_space, od_space:id()} |
eff_providers | {eff_provider, od_provider:id()} |
handle_services | eff_handle_services |
{handle_service, od_handle_service:id()} |
{eff_handle_service, od_handle_service:id()} |
handles | eff_handles | {handle, od_handle:id()} | {eff_handle, od_handle:id()}.

-export_type([resource/0]).


-export([get_entity/1, create/4, get/4, update/3, delete/2]).
-export([exists/1, authorize/4, validate/2]).
-export([entity_to_string/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity from datastore based on its EntityId.
%% Should return ?ERROR_NOT_FOUND if the entity does not exist.
%% @end
%%--------------------------------------------------------------------
-spec get_entity(EntityId :: n_entity_logic:entity_id()) ->
    {ok, n_entity_logic:entity()} | {error, Reason :: term()}.
get_entity(UserId) ->
    case od_user:get(UserId) of
        {ok, #document{value = Group}} ->
            {ok, Group};
        _ ->
            ?ERROR_NOT_FOUND
    end.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource based on EntityId, Resource identifier and Data.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: n_entity_logic:client(),
    EntityId :: n_entity_logic:entity_id(), Resource :: resource(),
    n_entity_logic:data()) -> n_entity_logic:result().
% TODO VFS-2918
create(_Client, UserId, deprecated_default_space, #{<<"spaceId">> := SpaceId}) ->
    {ok, _} = od_user:update(UserId, #{default_space => SpaceId}),
    ok;

create(_Client, _UserId, authorize, Data) ->
    Identifier = maps:get(<<"identifier">>, Data),
    case auth_logic:authenticate_user(Identifier) of
        {ok, DischargeMacaroonToken} ->
            {ok, DischargeMacaroonToken};
        _ ->
            ?ERROR_BAD_VALUE_IDENTIFIER(<<"identifier">>)
    end;

create(_Client, UserId, client_tokens, _Data) ->
    Token = auth_logic:gen_token(UserId),
    {ok, _} = od_user:update(UserId, fun(#od_user{client_tokens = Tokens} = User) ->
        {ok, User#od_user{client_tokens = [Token | Tokens]}}
    end),
    {ok, Token};

create(_Client, UserId, default_space, Data) ->
    SpaceId = maps:get(<<"spaceId">>, Data),
    case n_user_logic:has_eff_space(UserId, SpaceId) of
        true ->
            {ok, _} = od_user:update(UserId, #{default_space => SpaceId}),
            ok;
        false ->
            ?ERROR_RELATION_DOES_NOT_EXIST(od_user, UserId, od_space, SpaceId)
    end;

create(_Client, UserId, {space_alias, SpaceId}, Data) ->
    Alias = maps:get(<<"alias">>, Data),
    {ok, _} = od_user:update(UserId, fun(#od_user{space_aliases = Aliases} = User) ->
        {ok, User#od_user{space_aliases = maps:put(SpaceId, Alias, Aliases)}}
    end),
    ok;

create(_Client, UserId, default_provider, Data) ->
    ProviderId = maps:get(<<"providerId">>, Data),
    case n_user_logic:has_eff_provider(UserId, ProviderId) of
        true ->
            {ok, _} = od_user:update(UserId, #{default_provider => ProviderId}),
            ok;
        false ->
            ?ERROR_RELATION_DOES_NOT_EXIST(od_user, UserId, od_provider, ProviderId)
    end;

create(_Client, UserId, create_group, Data) ->
    n_group_logic_plugin:create(?USER(UserId), undefined, entity, Data);

create(_Client, UserId, create_space, Data) ->
    n_space_logic_plugin:create(?USER(UserId), undefined, entity, Data);

create(_Client, UserId, create_handle_service, Data) ->
    n_handle_service_logic_plugin:create(?USER(UserId), undefined, entity, Data);

create(_Client, UserId, create_handle, Data) ->
    n_handle_logic_plugin:create(?USER(UserId), undefined, entity, Data);

create(_Client, UserId, join_group, Data) ->
    Macaroon = maps:get(<<"token">>, Data),
    {ok, {od_group, GroupId}} = token_logic:consume(Macaroon),
    entity_graph:add_relation(
        od_user, UserId,
        od_group, GroupId,
        privileges:group_user()
    ),
    {ok, GroupId};

create(_Client, UserId, join_space, Data) ->
    Macaroon = maps:get(<<"token">>, Data),
    {ok, {od_space, SpaceId}} = token_logic:consume(Macaroon),
    entity_graph:add_relation(
        od_user, UserId,
        od_space, SpaceId,
        privileges:space_user()
    ),
    {ok, SpaceId}.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource based on EntityId and Resource identifier.
%% @end
%%--------------------------------------------------------------------
-spec get(Client :: n_entity_logic:client(), EntityId :: n_entity_logic:entity_id(),
    Entity :: n_entity_logic:entity(), Resource :: resource()) ->
    n_entity_logic:result().
% TODO VFS-2918
get(_, _UserId, #od_user{default_space = DefaultSpace}, deprecated_default_space) ->
    {ok, DefaultSpace};

get(_, _UserId, #od_user{} = User, data) ->
    #od_user{
        name = Name, login = Login, alias = Alias, email_list = EmailList,
        connected_accounts = ConnectedAccounts
    } = User,
    % TODO VFS-2918 do we need connected accounts?
    ConnectedAccountProplists = lists:map(fun(Account) ->
        ?record_to_list(oauth_account, Account)
    end, ConnectedAccounts),
    ConnectedAccountMaps = lists:map(fun(ConnectedAccountProplist) ->
        maps:from_list(ConnectedAccountProplist)
    end, ConnectedAccountProplists),
    {ok, #{
        <<"name">> => Name, <<"login">> => Login,
        <<"alias">> => Alias, <<"emailList">> => EmailList,
        <<"connectedAccounts">> => ConnectedAccountMaps
    }};
get(_, undefined, undefined, list) ->
    {ok, UserDocs} = od_user:list(),
    {ok, [UserId || #document{key = UserId} <- UserDocs]};
get(_, _UserId, #od_user{oz_privileges = OzPrivileges}, oz_privileges) ->
    {ok, OzPrivileges};
get(_, _UserId, #od_user{eff_oz_privileges = OzPrivileges}, eff_oz_privileges) ->
    {ok, OzPrivileges};
get(_, _UserId, #od_user{default_space = DefaultSpace}, default_space) ->
    {ok, DefaultSpace};
get(_, _UserId, #od_user{space_aliases = SpaceAliases}, {space_alias, SpaceId}) ->
    {ok, maps:get(SpaceId, SpaceAliases)};
get(_, _UserId, #od_user{default_provider = DefaultProvider}, default_provider) ->
    {ok, DefaultProvider};
get(_, _UserId, #od_user{client_tokens = ClientTokens}, client_tokens) ->
    {ok, ClientTokens};

get(_, _UserId, #od_user{groups = Groups}, groups) ->
    {ok, Groups};
get(_, _UserId, #od_user{eff_groups = Groups}, eff_groups) ->
    {ok, maps:keys(Groups)};
get(_, _UserId, #od_user{}, {group, GroupId}) ->
    {ok, Group} = ?throw_on_failure(n_group_logic_plugin:get_entity(GroupId)),
    n_group_logic_plugin:get(?ROOT, GroupId, Group, data);
get(_, _UserId, #od_user{}, {eff_group, GroupId}) ->
    {ok, Group} = ?throw_on_failure(n_group_logic_plugin:get_entity(GroupId)),
    n_group_logic_plugin:get(?ROOT, GroupId, Group, data);

get(_, _UserId, #od_user{spaces = Spaces}, spaces) ->
    {ok, Spaces};
get(_, _UserId, #od_user{eff_spaces = Spaces}, eff_spaces) ->
    {ok, maps:keys(Spaces)};
get(_, _UserId, #od_user{}, {space, SpaceId}) ->
    {ok, Space} = ?throw_on_failure(n_space_logic_plugin:get_entity(SpaceId)),
    n_space_logic_plugin:get(?ROOT, SpaceId, Space, data);
get(_, _UserId, #od_user{}, {eff_space, SpaceId}) ->
    {ok, Space} = ?throw_on_failure(n_space_logic_plugin:get_entity(SpaceId)),
    n_space_logic_plugin:get(?ROOT, SpaceId, Space, data);

get(_, _UserId, #od_user{eff_providers = Providers}, eff_providers) ->
    {ok, maps:keys(Providers)};
get(_, _UserId, #od_user{}, {eff_provider, ProviderId}) ->
    {ok, Provider} = ?throw_on_failure(n_provider_logic_plugin:get_entity(ProviderId)),
    n_provider_logic_plugin:get(?ROOT, ProviderId, Provider, data);

get(_, _UserId, #od_user{handle_services = HandleServices}, handle_services) ->
    {ok, HandleServices};
get(_, _UserId, #od_user{eff_handle_services = HandleServices}, eff_handle_services) ->
    {ok, maps:keys(HandleServices)};
get(_, _UserId, #od_user{}, {handle_service, HServiceId}) ->
    {ok, HService} = ?throw_on_failure(n_handle_service_logic_plugin:get_entity(HServiceId)),
    n_handle_service_logic_plugin:get(?ROOT, HServiceId, HService, data);
get(_, _UserId, #od_user{}, {eff_handle_service, HServiceId}) ->
    {ok, HService} = ?throw_on_failure(n_handle_service_logic_plugin:get_entity(HServiceId)),
    n_handle_service_logic_plugin:get(?ROOT, HServiceId, HService, data);

get(_, _UserId, #od_user{handles = Handles}, handles) ->
    {ok, Handles};
get(_, _UserId, #od_user{eff_handles = Handles}, eff_handles) ->
    {ok, maps:keys(Handles)};
get(_, _UserId, #od_user{}, {handle, HandleId}) ->
    {ok, Handle} = ?throw_on_failure(n_handle_logic_plugin:get_entity(HandleId)),
    n_handle_logic_plugin:get(?ROOT, HandleId, Handle, data);
get(_, _UserId, #od_user{}, {eff_handle, HandleId}) ->
    {ok, Handle} = ?throw_on_failure(n_handle_logic_plugin:get_entity(HandleId)),
    n_handle_logic_plugin:get(?ROOT, HandleId, Handle, data).


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource based on EntityId, Resource identifier and Data.
%% @end
%%--------------------------------------------------------------------
-spec update(EntityId :: n_entity_logic:entity_id(), Resource :: resource(),
    n_entity_logic:data()) -> n_entity_logic:result().
update(UserId, entity, Data) ->
    UserUpdateFun = fun(#od_user{name = OldName, alias = OldAlias} = User) ->
        {ok, User#od_user{
            name = maps:get(<<"name">>, Data, OldName),
            alias = maps:get(<<"alias">>, Data, OldAlias)
        }}
    end,
    % If alias is specified, run update in synchronized block so no two
    % identical aliases can be set
    case maps:get(<<"alias">>, Data, undefined) of
        undefined ->
            {ok, _} = od_user:update(UserId, UserUpdateFun),
            ok;
        Alias ->
            critical_section:run({alias, Alias}, fun() ->
                % Check if this alias is occupied
                case od_user:get_by_criterion({alias, Alias}) of
                    {ok, #document{key = UserId}} ->
                        % DB returned the same user, so the alias was modified
                        % but is identical, don't report errors.
                        {ok, _} = od_user:update(UserId, UserUpdateFun),
                        ok;
                    {ok, #document{}} ->
                        % Alias is occupied by another user
                        ?ERROR_ALIAS_OCCUPIED;
                    _ ->
                        % Alias is not occupied, update user doc
                        {ok, _} = od_user:update(UserId, UserUpdateFun),
                        ok
                end
            end)
    end;

update(UserId, oz_privileges, Data) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_oz_privileges(od_user, UserId, Operation, Privileges).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource based on EntityId and Resource identifier.
%% @end
%%--------------------------------------------------------------------
-spec delete(EntityId :: n_entity_logic:entity_id(), Resource :: resource()) ->
    n_entity_logic:result().
delete(UserId, entity) ->
    % Invalidate auth tokens
    auth_logic:invalidate_user_tokens(UserId),
    % Invalidate client tokens
    {ok, #document{
        value = #od_user{
            client_tokens = Tokens
        }}} = od_user:get(UserId),
    lists:foreach(
        fun(Token) ->
            {ok, Macaroon} = token_logic:deserialize(Token),
            ok = token_logic:delete(Macaroon)
        end, Tokens),
    entity_graph:delete_with_relations(od_user, UserId);

delete(UserId, oz_privileges) ->
    update(UserId, oz_privileges, #{
        <<"operation">> => set, <<"privileges">> => []}
    );

delete(UserId, {client_token, TokenId}) ->
    {ok, Macaroon} = token_utils:deserialize(TokenId),
    Identifier = macaroon:identifier(Macaroon),
    onedata_auth:delete(Identifier),
    {ok, _} = od_user:update(UserId, fun(#od_user{client_tokens = Tokens} = User) ->
        {ok, User#od_user{client_tokens = Tokens -- [TokenId]}}
    end),
    ok;

delete(UserId, default_space) ->
    {ok, _} = od_user:update(UserId, #{default_space => undefined}),
    ok;

delete(UserId, {space_alias, SpaceId}) ->
    {ok, _} = od_user:update(UserId, fun(#od_user{space_aliases = Aliases} = User) ->
        {ok, User#od_user{space_aliases = maps:remove(SpaceId, Aliases)}}
    end),
    ok;

delete(UserId, default_provider) ->
    {ok, _} = od_user:update(UserId, #{default_provider => undefined}),
    ok;

delete(UserId, {group, GroupId}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_group, GroupId);

delete(UserId, {space, SpaceId}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_space, SpaceId);

delete(UserId, {handle_service, HServiceId}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_handle_service, HServiceId);

delete(UserId, {handle, HandleId}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_handle, HandleId).


%%--------------------------------------------------------------------
%% @doc
%% Returns existence verificators for given Resource identifier.
%% Existence verificators can be internal, which means they operate on the
%% entity to which the resource corresponds, or external - independent of
%% the entity. If there are multiple verificators, they will be checked in
%% sequence until one of them returns true.
%% Implicit verificators 'true' | 'false' immediately stop the verification
%% process with given result.
%% @end
%%--------------------------------------------------------------------
-spec exists(Resource :: resource()) ->
    n_entity_logic:existence_verificator()|
    [n_entity_logic:existence_verificator()].
% TODO VFS-2918
exists(deprecated_default_space) ->
    true;

exists({client_token, TokenId}) ->
    {internal, fun(#od_user{client_tokens = Tokens}) ->
        lists:member(TokenId, Tokens)
    end};

exists(default_space) ->
    {internal, fun(#od_user{default_space = DefaultSpace}) ->
        undefined =/= DefaultSpace
    end};
exists({space_alias, SpaceId}) ->
    {internal, fun(#od_user{space_aliases = Aliases}) ->
        maps:is_key(SpaceId, Aliases)
    end};

exists(default_provider) ->
    {internal, fun(#od_user{default_provider = DefaultProvider}) ->
        undefined =/= DefaultProvider
    end};

exists({group, GroupId}) ->
    {internal, fun(#od_user{groups = Groups}) ->
        lists:member(GroupId, Groups)
    end};
exists({eff_group, GroupId}) ->
    {internal, fun(#od_user{eff_groups = Groups}) ->
        maps:is_key(GroupId, Groups)
    end};

exists({space, SpaceId}) ->
    {internal, fun(#od_user{spaces = Spaces}) ->
        lists:member(SpaceId, Spaces)
    end};
exists({eff_space, SpaceId}) ->
    {internal, fun(#od_user{eff_spaces = Spaces}) ->
        maps:is_key(SpaceId, Spaces)
    end};

exists({eff_provider, ProviderId}) ->
    {internal, fun(#od_user{eff_providers = Providers}) ->
        maps:is_key(ProviderId, Providers)
    end};

exists({handle_service, HServiceId}) ->
    {internal, fun(#od_user{handle_services = HServices}) ->
        lists:member(HServiceId, HServices)
    end};
exists({eff_handle_service, HServiceId}) ->
    {internal, fun(#od_user{eff_handle_services = HServices}) ->
        maps:is_key(HServiceId, HServices)
    end};

exists({handle, HandleId}) ->
    {internal, fun(#od_user{handles = Handles}) ->
        lists:member(HandleId, Handles)
    end};
exists({eff_handle, HandleId}) ->
    {internal, fun(#od_user{eff_handles = Handles}) ->
        maps:is_key(HandleId, Handles)
    end};

exists(_) ->
    {internal, fun(#od_user{}) ->
        % If the user with UserId can be found, it exists. If not, the
        % verification will fail before this function is called.
        true
    end}.


%%--------------------------------------------------------------------
%% @doc
%% Returns existence verificators for given Resource identifier.
%% Existence verificators can be internal, which means they operate on the
%% entity to which the resource corresponds, or external - independent of
%% the entity. If there are multiple verificators, they will be checked in
%% sequence until one of them returns true.
%% Implicit verificators 'true' | 'false' immediately stop the verification
%% process with given result.
%% @end
%%--------------------------------------------------------------------
-spec authorize(Operation :: n_entity_logic:operation(),
    EntityId :: n_entity_logic:entity_id(), Resource :: resource(),
    Client :: n_entity_logic:client()) ->
    n_entity_logic:authorization_verificator() |
    [authorization_verificator:existence_verificator()].
% TODO VFS-2918
authorize(create, UserId, deprecated_default_space, ?USER(UserId)) ->
    true;
% TODO VFS-2918
authorize(get, UserId, deprecated_default_space, ?USER(UserId)) ->
    true;

authorize(create, _UserId, authorize, _Client) ->
    true;

authorize(create, UserId, create_group, ?USER(UserId)) ->
    n_group_logic_plugin:authorize(create, undefined, entity, ?USER(UserId));

authorize(create, UserId, create_space, ?USER(UserId)) ->
    n_space_logic_plugin:authorize(create, undefined, entity, ?USER(UserId));

authorize(create, UserId, create_handle_service, ?USER(UserId)) ->
    n_handle_service_logic_plugin:authorize(create, undefined, entity, ?USER(UserId));

authorize(create, UserId, create_handle, ?USER(UserId)) ->
    n_handle_logic_plugin:authorize(create, undefined, entity, ?USER(UserId));

% User trying to access its own oz_privileges
authorize(get, UserId, oz_privileges, ?USER(UserId)) ->
    auth_self_by_oz_privilege(?OZ_VIEW_PRIVILEGES);
authorize(get, UserId, eff_oz_privileges, ?USER(UserId)) ->
    auth_self_by_oz_privilege(?OZ_VIEW_PRIVILEGES);
authorize(update, UserId, oz_privileges, ?USER(UserId)) ->
    auth_self_by_oz_privilege(?OZ_SET_PRIVILEGES);
authorize(delete, UserId, oz_privileges, ?USER(UserId)) ->
    auth_self_by_oz_privilege(?OZ_SET_PRIVILEGES);

% User trying to access someone else's oz_privileges
authorize(get, _UserId, oz_privileges, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_VIEW_PRIVILEGES);
authorize(get, _UserId, eff_oz_privileges, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_VIEW_PRIVILEGES);
authorize(update, _UserId, oz_privileges, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_SET_PRIVILEGES);
authorize(delete, _UserId, oz_privileges, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_SET_PRIVILEGES);

% User can create/get/update/delete any information about himself
% (except oz_privileges)
authorize(_, UserId, _, ?USER(UserId)) ->
    true;

% Other admin endpoints
authorize(get, undefined, list, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_USERS_LIST);
authorize(get, _UserId, data, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_USERS_LIST);
authorize(get, _UserId, entity, ?USER(UserId)) ->
    auth_by_oz_privilege(UserId, ?OZ_USERS_LIST).


%%--------------------------------------------------------------------
%% @doc
%% Returns validity verificators for given Operation and Resource identifier.
%% Returns a map with 'required', 'optional' and 'at_least_one' keys.
%% Under each of them, there is a map:
%%      Key => {type_verificator, value_verificator}
%% Which means how value of given Key should be validated.
%% @end
%%--------------------------------------------------------------------
-spec validate(Operation :: n_entity_logic:operation(),
    Resource :: resource()) ->
    n_entity_logic:validity_verificator().
% TODO VFS-2918
validate(create, deprecated_default_space) -> #{
    required => #{
        <<"spaceId">> => {binary, {exists, fun(Value) ->
            n_space_logic:exists(Value)
        end}}
    }
};

validate(create, authorize) -> #{
    required => #{
        <<"identifier">> => {binary, non_empty}
    }
};
validate(create, client_tokens) -> #{
};
validate(create, default_space) -> #{
    required => #{
        <<"spaceId">> => {binary, {exists, fun(Value) ->
            n_space_logic:exists(Value)
        end}}
    }
};
validate(create, {space_alias, _SpaceId}) -> #{
    required => #{
        <<"alias">> => {binary, non_empty}
    }
};
validate(create, default_provider) -> #{
    required => #{
        <<"providerId">> => {binary, {exists, fun(Value) ->
            n_provider_logic:exists(Value)
        end}}
    }
};
validate(create, create_group) ->
    n_group_logic_plugin:validate(create, entity);
validate(create, create_space) ->
    n_space_logic_plugin:validate(create, entity);
validate(create, create_handle_service) ->
    n_handle_service_logic_plugin:validate(create, entity);
validate(create, create_handle) ->
    n_handle_logic_plugin:validate(create, entity);
validate(create, join_group) -> #{
    required => #{
        <<"token">> => {token, ?GROUP_INVITE_USER_TOKEN}
    }
};
validate(create, join_space) -> #{
    required => #{
        <<"token">> => {token, ?SPACE_INVITE_USER_TOKEN}
    }
};
validate(update, entity) -> #{
    at_least_one => #{
        <<"name">> => {binary, non_empty},
        <<"alias">> => {binary, alias}
    }
};
validate(update, oz_privileges) -> #{
    required => #{
        <<"privileges">> => {list_of_atoms, privileges:oz_privileges()}
    },
    optional => #{
        <<"operation">> => {atom, [set, grant, revoke]}
    }
}.


%%--------------------------------------------------------------------
%% @doc
%% Returns readable string representing the entity with given id.
%% @end
%%--------------------------------------------------------------------
-spec entity_to_string(EntityId :: n_entity_logic:entity_id()) -> binary().
entity_to_string(UserId) ->
    od_user:to_string(UserId).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns authorization verificator that checks if given user has specified
%% effective oz privilege.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_oz_privilege(UserId :: od_user:id(),
    Privilege :: privileges:oz_privilege()) ->
    n_entity_logic:authorization_verificator().
auth_by_oz_privilege(UserId, Privilege) ->
    {external, fun() ->
        n_user_logic:has_eff_oz_privilege(UserId, Privilege)
    end}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns authorization verificator that checks if user represented
%% by entity has specified effective oz privilege.
%% @end
%%--------------------------------------------------------------------
-spec auth_self_by_oz_privilege(Privilege :: privileges:oz_privilege()) ->
    n_entity_logic:authorization_verificator().
auth_self_by_oz_privilege(Privilege) ->
    {internal, fun(User) ->
        n_user_logic:has_eff_oz_privilege(User, Privilege)
    end}.
