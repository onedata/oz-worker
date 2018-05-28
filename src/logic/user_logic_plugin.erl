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
-module(user_logic_plugin).
-author("Lukasz Opiola").
-behaviour(entity_logic_plugin_behaviour).

-include("tokens.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/utils/utils.hrl").
-include_lib("ctool/include/api_errors.hrl").

-export([fetch_entity/1, operation_supported/3]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, validate/1]).
-export([auth_by_oz_privilege/2]).

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
fetch_entity(UserId) ->
    case od_user:get(UserId) of
        {ok, #document{value = Group}} ->
            {ok, Group};
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
operation_supported(create, authorize, private) -> true;
operation_supported(create, client_tokens, private) -> true;
operation_supported(create, default_space, private) -> true;
operation_supported(create, {space_alias, _}, private) -> true;
operation_supported(create, default_provider, private) -> true;

operation_supported(get, list, private) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, instance, protected) -> true;
operation_supported(get, instance, shared) -> true;

operation_supported(get, oz_privileges, private) -> true;
operation_supported(get, eff_oz_privileges, private) -> true;

operation_supported(get, client_tokens, private) -> true;
operation_supported(get, {client_token, _}, private) -> true;
operation_supported(get, linked_accounts, private) -> true;
operation_supported(get, {linked_account, _}, private) -> true;
operation_supported(get, default_space, private) -> true;
operation_supported(get, {space_alias, _}, private) -> true;
operation_supported(get, default_provider, private) -> true;

operation_supported(get, groups, private) -> true;
operation_supported(get, eff_groups, private) -> true;

operation_supported(get, spaces, private) -> true;
operation_supported(get, eff_spaces, private) -> true;

operation_supported(get, eff_providers, private) -> true;

operation_supported(get, handle_services, private) -> true;
operation_supported(get, eff_handle_services, private) -> true;

operation_supported(get, handles, private) -> true;
operation_supported(get, eff_handles, private) -> true;

operation_supported(update, instance, private) -> true;
operation_supported(update, oz_privileges, private) -> true;

operation_supported(delete, instance, private) -> true;
operation_supported(delete, oz_privileges, private) -> true;

operation_supported(delete, {client_token, _}, private) -> true;
operation_supported(delete, default_space, private) -> true;
operation_supported(delete, {space_alias, _}, private) -> true;
operation_supported(delete, default_provider, private) -> true;

operation_supported(delete, {group, _}, private) -> true;
operation_supported(delete, {space, _}, private) -> true;
operation_supported(delete, {handle_service, _}, private) -> true;
operation_supported(delete, {handle, _}, private) -> true;

operation_supported(_, _, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(#el_req{gri = #gri{aspect = authorize}, data = Data}) ->
    Identifier = maps:get(<<"identifier">>, Data),
    case auth_logic:authenticate_user(Identifier) of
        {ok, DischargeMacaroonToken} ->
            % TODO VFS-3835 the macaroon should be serialized in translator
            % rather than here
            {ok, {data, DischargeMacaroonToken}};
        _ ->
            ?ERROR_BAD_VALUE_IDENTIFIER(<<"identifier">>)
    end;

create(#el_req{gri = #gri{id = UserId, aspect = client_tokens} = GRI}) ->
    Token = auth_logic:gen_token(UserId),
    {ok, _} = od_user:update(UserId, fun(#od_user{client_tokens = Tokens} = User) ->
        {ok, User#od_user{client_tokens = [Token | Tokens]}}
    end),
    {ok, {fetched, GRI#gri{aspect = {client_token, Token}}, Token}};

create(Req = #el_req{gri = #gri{id = UserId, aspect = default_space}}) ->
    SpaceId = maps:get(<<"spaceId">>, Req#el_req.data),
    {ok, _} = od_user:update(UserId, fun(User = #od_user{}) ->
        {ok, User#od_user{default_space = SpaceId}}
    end),
    ok;

create(Req = #el_req{gri = #gri{id = UserId, aspect = {space_alias, SpaceId}}}) ->
    Alias = maps:get(<<"alias">>, Req#el_req.data),
    {ok, _} = od_user:update(UserId, fun(#od_user{space_aliases = Aliases} = User) ->
        {ok, User#od_user{space_aliases = maps:put(SpaceId, Alias, Aliases)}}
    end),
    ok;

create(#el_req{gri = #gri{id = UserId, aspect = default_provider}, data = Data}) ->
    ProviderId = maps:get(<<"providerId">>, Data),
    {ok, _} = od_user:update(UserId, fun(User = #od_user{}) ->
        {ok, User#od_user{default_provider = ProviderId}}
    end),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on entity logic request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec get(entity_logic:req(), entity_logic:entity()) ->
    entity_logic:get_result().
get(#el_req{gri = #gri{aspect = list}}, _) ->
    {ok, UserDocs} = od_user:list(),
    {ok, [UserId || #document{key = UserId} <- UserDocs]};

get(#el_req{gri = #gri{aspect = instance, scope = private}}, User) ->
    {ok, User};
get(#el_req{gri = #gri{aspect = instance, scope = protected}}, User) ->
    #od_user{
        name = Name, alias = Alias, email_list = EmailList,
        linked_accounts = LinkedAccounts
    } = User,
    {ok, #{
        <<"name">> => Name, <<"alias">> => Alias,
        <<"emailList">> => EmailList,
        <<"linkedAccounts">> => user_logic:linked_accounts_to_maps(LinkedAccounts)
    }};
get(#el_req{gri = #gri{aspect = instance, scope = shared}}, User) ->
    #od_user{name = Name, alias = Alias} = User,
    {ok, #{<<"name">> => Name, <<"alias">> => Alias}};

get(#el_req{gri = #gri{aspect = oz_privileges}}, User) ->
    {ok, User#od_user.oz_privileges};
get(#el_req{gri = #gri{aspect = eff_oz_privileges}}, User) ->
    {ok, User#od_user.eff_oz_privileges};

get(#el_req{gri = #gri{aspect = client_tokens}}, User) ->
    {ok, User#od_user.client_tokens};

get(#el_req{gri = #gri{aspect = {client_token, Id}}}, _User) ->
    {ok, Id};

get(#el_req{gri = #gri{aspect = linked_accounts}}, User) ->
    {ok, User#od_user.linked_accounts};
get(#el_req{gri = #gri{aspect = {linked_account, SubId}}}, User) ->
    {ok, find_linked_account(SubId, User#od_user.linked_accounts)};

get(#el_req{gri = #gri{aspect = default_space}}, User) ->
    {ok, User#od_user.default_space};
get(#el_req{gri = #gri{aspect = {space_alias, SpaceId}}}, User) ->
    {ok, maps:get(SpaceId, User#od_user.space_aliases)};
get(#el_req{gri = #gri{aspect = default_provider}}, User) ->
    {ok, User#od_user.default_provider};

get(#el_req{gri = #gri{aspect = groups}}, User) ->
    {ok, User#od_user.groups};
get(#el_req{gri = #gri{aspect = eff_groups}}, User) ->
    {ok, maps:keys(User#od_user.eff_groups)};

get(#el_req{gri = #gri{aspect = spaces}}, User) ->
    {ok, User#od_user.spaces};
get(#el_req{gri = #gri{aspect = eff_spaces}}, User) ->
    {ok, maps:keys(User#od_user.eff_spaces)};

get(#el_req{gri = #gri{aspect = eff_providers}}, User) ->
    {ok, maps:keys(User#od_user.eff_providers)};

get(#el_req{gri = #gri{aspect = handle_services}}, User) ->
    {ok, User#od_user.handle_services};
get(#el_req{gri = #gri{aspect = eff_handle_services}}, User) ->
    {ok, maps:keys(User#od_user.eff_handle_services)};

get(#el_req{gri = #gri{aspect = handles}}, User) ->
    {ok, User#od_user.handles};
get(#el_req{gri = #gri{aspect = eff_handles}}, User) ->
    {ok, maps:keys(User#od_user.eff_handles)}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = UserId, aspect = instance}, data = Data}) ->
    UserUpdateFun = fun(#od_user{name = OldName, alias = OldAlias} = User) ->
        {ok, User#od_user{
            name = maps:get(<<"name">>, Data, OldName),
            alias = maps:get(<<"alias">>, Data, OldAlias)
        }}
    end,
    % If alias is specified and is not undefined (which is valid in case of
    % removing alias), run update in synchronized block so no two
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
                        ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"alias">>);
                    _ ->
                        % Alias is not occupied, update user doc
                        {ok, _} = od_user:update(UserId, UserUpdateFun),
                        ok
                end
            end)
    end;

update(#el_req{gri = #gri{id = UserId, aspect = oz_privileges}, data = Data}) ->
    Privileges = maps:get(<<"privileges">>, Data),
    Operation = maps:get(<<"operation">>, Data, set),
    entity_graph:update_oz_privileges(od_user, UserId, Operation, Privileges).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = UserId, aspect = instance}}) ->
    % Invalidate auth tokens
    auth_logic:invalidate_user_tokens(UserId),
    % Invalidate basic auth cache and client tokens
    {ok, #document{
        value = #od_user{
            alias = Alias,
            client_tokens = Tokens
        }}} = od_user:get(UserId),
    basic_auth_cache:delete(Alias),
    lists:foreach(
        fun(Token) ->
            {ok, Macaroon} = token_logic:deserialize(Token),
            ok = token_logic:delete(Macaroon)
        end, Tokens),
    entity_graph:delete_with_relations(od_user, UserId);

delete(#el_req{gri = #gri{id = UserId, aspect = oz_privileges}}) ->
    update(#el_req{gri = #gri{id = UserId, aspect = oz_privileges}, data = #{
        <<"operation">> => set, <<"privileges">> => []
    }});

delete(#el_req{gri = #gri{id = UserId, aspect = {client_token, TokenId}}}) ->
    {ok, Macaroon} = onedata_macaroons:deserialize(TokenId),
    Identifier = macaroon:identifier(Macaroon),
    onedata_auth:delete(Identifier),
    {ok, _} = od_user:update(UserId, fun(User = #od_user{client_tokens = Tokens}) ->
        {ok, User#od_user{client_tokens = Tokens -- [TokenId]}}
    end),
    ok;

delete(#el_req{gri = #gri{id = UserId, aspect = default_space}}) ->
    {ok, _} = od_user:update(UserId, fun(User = #od_user{}) ->
        {ok, User#od_user{default_space = undefined}}
    end),
    ok;

delete(#el_req{gri = #gri{id = UserId, aspect = {space_alias, SpaceId}}}) ->
    {ok, _} = od_user:update(UserId, fun(#od_user{space_aliases = Aliases} = User) ->
        {ok, User#od_user{space_aliases = maps:remove(SpaceId, Aliases)}}
    end),
    ok;

delete(#el_req{gri = #gri{id = UserId, aspect = default_provider}}) ->
    {ok, _} = od_user:update(UserId, fun(User = #od_user{}) ->
        {ok, User#od_user{default_provider = undefined}}
    end),
    ok;

delete(#el_req{gri = #gri{id = UserId, aspect = {group, GroupId}}}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_group, GroupId
    );

delete(#el_req{gri = #gri{id = UserId, aspect = {space, SpaceId}}}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_space, SpaceId
    );

delete(#el_req{gri = #gri{id = UserId, aspect = {handle_service, HServiceId}}}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_handle_service, HServiceId
    );

delete(#el_req{gri = #gri{id = UserId, aspect = {handle, HandleId}}}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_handle, HandleId
    ).


%%--------------------------------------------------------------------
%% @doc
%% Determines if given resource (aspect of entity) exists, based on entity
%% logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec exists(entity_logic:req(), entity_logic:entity()) -> boolean().
exists(Req = #el_req{gri = #gri{aspect = instance, scope = protected}}, User) ->
    case Req#el_req.auth_hint of
        ?THROUGH_PROVIDER(ProviderId) ->
            user_logic:has_eff_provider(User, ProviderId);
        undefined ->
            true
    end;

exists(Req = #el_req{gri = #gri{aspect = instance, scope = shared}}, User) ->
    case Req#el_req.auth_hint of
        ?THROUGH_GROUP(GroupId) ->
            user_logic:has_eff_group(User, GroupId);
        ?THROUGH_SPACE(SpaceId) ->
            user_logic:has_eff_space(User, SpaceId);
        ?THROUGH_HANDLE_SERVICE(HServiceId) ->
            user_logic:has_eff_handle_service(User, HServiceId);
        ?THROUGH_HANDLE(HandleId) ->
            user_logic:has_eff_handle(User, HandleId);
        undefined ->
            true
    end;

exists(#el_req{gri = #gri{aspect = {client_token, TokenId}}}, User) ->
    lists:member(TokenId, User#od_user.client_tokens);

exists(#el_req{gri = #gri{aspect = {linked_account, SubId}}}, User) ->
    find_linked_account(SubId, User#od_user.linked_accounts) /= undefined;

exists(#el_req{gri = #gri{aspect = default_space}}, User) ->
    undefined =/= User#od_user.default_space;

exists(#el_req{gri = #gri{aspect = {space_alias, SpaceId}}}, User) ->
    maps:is_key(SpaceId, User#od_user.space_aliases);

exists(#el_req{gri = #gri{aspect = default_provider}}, User) ->
    undefined =/= User#od_user.default_provider;

exists(#el_req{gri = #gri{aspect = {group, GroupId}}}, User) ->
    lists:member(GroupId, User#od_user.groups);

exists(#el_req{gri = #gri{aspect = {space, SpaceId}}}, User) ->
    lists:member(SpaceId, User#od_user.spaces);

exists(#el_req{gri = #gri{aspect = {handle_service, HServiceId}}}, User) ->
    lists:member(HServiceId, User#od_user.handle_services);

exists(#el_req{gri = #gri{aspect = {handle, HandleId}}}, User) ->
    lists:member(HandleId, User#od_user.handles);

% All other aspects exist if user record exists.
exists(#el_req{gri = #gri{id = Id}}, #od_user{}) ->
    Id =/= undefined.


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on entity logic request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-spec authorize(entity_logic:req(), entity_logic:entity()) -> boolean().
authorize(Req = #el_req{operation = get, gri = #gri{aspect = list}}, User) ->
    auth_by_oz_privilege(Req, User, ?OZ_USERS_LIST);

authorize(Req = #el_req{operation = get, gri = #gri{aspect = oz_privileges}}, User) ->
    auth_by_oz_privilege(Req, User, ?OZ_VIEW_PRIVILEGES);
authorize(Req = #el_req{operation = get, gri = #gri{aspect = eff_oz_privileges}}, User) ->
    auth_by_oz_privilege(Req, User, ?OZ_VIEW_PRIVILEGES);
authorize(Req = #el_req{operation = update, gri = #gri{aspect = oz_privileges}}, User) ->
    auth_by_oz_privilege(Req, User, ?OZ_SET_PRIVILEGES);
authorize(Req = #el_req{operation = delete, gri = #gri{aspect = oz_privileges}}, User) ->
    auth_by_oz_privilege(Req, User, ?OZ_SET_PRIVILEGES);

authorize(#el_req{operation = create, gri = #gri{aspect = authorize}}, _) ->
    true;

% Beside oz_privileges, user can perform all operations on his record
authorize(#el_req{client = ?USER(UserId), gri = #gri{id = UserId}}, _) ->
    true;

authorize(Req = #el_req{operation = get, gri = #gri{aspect = instance, scope = protected}}, User) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?PROVIDER(ProviderId), ?THROUGH_PROVIDER(ProviderId)} ->
            % User's membership in provider is checked in 'exists'
            true;

        {?USER(ClientUserId), ?THROUGH_PROVIDER(_ProviderId)} ->
            % User's membership in provider is checked in 'exists'
            auth_by_oz_privilege(ClientUserId, ?OZ_PROVIDERS_LIST_USERS);

        {?USER(ClientUserId), _} ->
            auth_by_oz_privilege(ClientUserId, ?OZ_USERS_LIST);

        _ ->
            % Access to private data also allows access to protected data
            authorize(Req#el_req{gri = #gri{scope = private}}, User)
    end;

authorize(Req = #el_req{operation = get, gri = GRI = #gri{aspect = instance, scope = shared}}, User) ->
    case {Req#el_req.client, Req#el_req.auth_hint} of
        {?USER(ClientUserId), ?THROUGH_GROUP(GroupId)} ->
            % User's membership in group is checked in 'exists'
            group_logic:has_eff_privilege(GroupId, ClientUserId, ?GROUP_VIEW) orelse
                user_logic:has_eff_oz_privilege(ClientUserId, ?OZ_GROUPS_LIST_USERS);

        {?USER(ClientUserId), ?THROUGH_SPACE(SpaceId)} ->
            % User's membership in space is checked in 'exists'
            space_logic:has_eff_privilege(SpaceId, ClientUserId, ?SPACE_VIEW) orelse
                user_logic:has_eff_oz_privilege(ClientUserId, ?OZ_SPACES_LIST_USERS);

        {?USER(ClientUserId), ?THROUGH_HANDLE_SERVICE(HServiceId)} ->
            % User's membership in handle_service is checked in 'exists'
            handle_service_logic:has_eff_privilege(HServiceId, ClientUserId, ?HANDLE_SERVICE_VIEW);

        {?USER(ClientUserId), ?THROUGH_HANDLE(HandleId)} ->
            % User's membership in handle is checked in 'exists'
            handle_logic:has_eff_privilege(HandleId, ClientUserId, ?HANDLE_VIEW);

        _ ->
            % Access to protected data also allows access to shared data
            authorize(Req#el_req{gri = GRI#gri{scope = protected}}, User)
    end;

authorize(#el_req{client = ?USER(UserId), operation = delete, gri = #gri{aspect = instance}}, _) ->
    auth_by_oz_privilege(UserId, ?OZ_USERS_DELETE);

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
validate(#el_req{operation = create, gri = #gri{aspect = authorize}}) -> #{
    required => #{
        <<"identifier">> => {binary, non_empty}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = client_tokens}}) -> #{
};

validate(#el_req{operation = create, gri = #gri{id = UserId, aspect = default_space}}) ->
    #{
        required => #{
            <<"spaceId">> => {binary, {exists, fun(SpaceId) ->
                space_logic:has_eff_user(SpaceId, UserId)
            end}}
        }
    };

validate(#el_req{operation = create, gri = #gri{id = UserId, aspect = {space_alias, SpaceId}}}) ->
    #{
        required => #{
            {aspect, <<"spaceId">>} => {any, {relation_exists,
                od_user, UserId, od_space, SpaceId, fun(SpId) ->
                    space_logic:has_eff_user(SpId, UserId)
                end}},
            <<"alias">> => {binary, non_empty}
        }
    };

validate(#el_req{operation = create, gri = #gri{id = UserId, aspect = default_provider}}) ->
    #{
        required => #{
            <<"providerId">> => {binary, {exists, fun(ProviderId) ->
                provider_logic:has_eff_user(ProviderId, UserId)
            end}}
        }
    };

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    at_least_one => #{
        <<"name">> => {binary, user_name},
        <<"alias">> => {alias, alias}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = oz_privileges}}) -> #{
    required => #{
        <<"privileges">> => {list_of_atoms, privileges:oz_privileges()}
    },
    optional => #{
        <<"operation">> => {atom, [set, grant, revoke]}
    }
}.


%%%===================================================================
%%% Functions used in other *_logic_plugin modules
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Predicate checking if given client has specified effective oz privilege based
%% on user id, user record or entity logic request client.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_oz_privilege(entity_logic:req() | od_user:id() | od_user:info(),
    privileges:oz_privilege()) -> boolean().
auth_by_oz_privilege(#el_req{client = ?USER(UserId)}, Privilege) ->
    auth_by_oz_privilege(UserId, Privilege);
auth_by_oz_privilege(#el_req{client = _OtherClient}, _Privilege) ->
    false;
auth_by_oz_privilege(UserOrId, Privilege) ->
    user_logic:has_eff_oz_privilege(UserOrId, Privilege).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Predicate checking if given client has specified effective oz privilege based
%% on entity logic request. If the client is the same as GRI subject, the
%% privileges can be checked using USEr record.
%% @end
%%--------------------------------------------------------------------
-spec auth_by_oz_privilege(entity_logic:req(), od_user:info(),
    privileges:oz_privilege()) -> boolean().
auth_by_oz_privilege(ElReq, User, Privilege) ->
    case ElReq of
        #el_req{client = ?USER(UserId), gri = #gri{id = UserId}} ->
            auth_by_oz_privilege(User, Privilege);
        #el_req{client = ?USER(OtherUser)} ->
            auth_by_oz_privilege(OtherUser, Privilege)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Finds a linked account in given list based on subject id.
%% @end
%%--------------------------------------------------------------------
-spec find_linked_account(SubId :: binary(), [od_user:linked_account()]) ->
    undefined | od_user:linked_account().
find_linked_account(_, []) ->
    undefined;
find_linked_account(SubId, [Account = #linked_account{subject_id = SubId} | _]) ->
    Account;
find_linked_account(SubId, [_ | Rest]) ->
    find_linked_account(SubId, Rest).
