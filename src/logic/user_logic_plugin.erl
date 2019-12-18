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

-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/errors.hrl").

-export([fetch_entity/1, operation_supported/3, is_subscribable/2]).
-export([create/1, get/2, update/1, delete/1]).
-export([exists/2, authorize/2, required_admin_privileges/1, validate/1]).
-export([auth_by_oz_privilege/2]).

-define(LOCK_ON_USERNAME(Username, Fun), critical_section:run({username, Username}, Fun)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity and its revision from datastore, if applicable.
%% Should return:
%%  * {true, entity_logic:versioned_entity()}
%%      if the fetch was successful
%%  * false
%%      if fetch is not applicable for this operation
%%  * {error, _}
%%      if there was an error, such as ?ERROR_NOT_FOUND
%% @end
%%--------------------------------------------------------------------
-spec fetch_entity(gri:gri()) ->
    {true, entity_logic:versioned_entity()} | false | errors:error().
fetch_entity(#gri{id = UserId}) ->
    case od_user:get(UserId) of
        {ok, #document{value = User, revs = [DbRev | _]}} ->
            {Revision, _Hash} = datastore_utils:parse_rev(DbRev),
            {true, {User, Revision}};
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
operation_supported(create, instance, private) -> true;
%% @TODO VFS-5846 old client tokens API kept for backward compatibility
operation_supported(create, client_tokens, private) -> true;
operation_supported(create, default_space, private) -> true;
operation_supported(create, {space_alias, _}, private) -> true;
operation_supported(create, default_provider, private) -> true;
operation_supported(create, {idp_access_token, _}, private) -> true;
operation_supported(create, provider_registration_token, private) -> true;

operation_supported(get, list, private) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, instance, protected) -> true;
operation_supported(get, instance, shared) -> true;

operation_supported(get, oz_privileges, private) -> true;
operation_supported(get, eff_oz_privileges, private) -> true;

%% @TODO VFS-5846 old client tokens API kept for backward compatibility
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

operation_supported(get, harvesters, private) -> true;
operation_supported(get, eff_harvesters, private) -> true;

operation_supported(get, eff_providers, private) -> true;

operation_supported(get, handle_services, private) -> true;
operation_supported(get, eff_handle_services, private) -> true;

operation_supported(get, handles, private) -> true;
operation_supported(get, eff_handles, private) -> true;

operation_supported(get, clusters, private) -> true;
operation_supported(get, eff_clusters, private) -> true;

operation_supported(update, instance, private) -> true;
operation_supported(update, password, private) -> true;
operation_supported(update, basic_auth, private) -> true;
operation_supported(update, oz_privileges, private) -> true;

operation_supported(delete, instance, private) -> true;
operation_supported(delete, oz_privileges, private) -> true;

%% @TODO VFS-5846 old client tokens API kept for backward compatibility
operation_supported(delete, {client_token, _}, private) -> true;
operation_supported(delete, default_space, private) -> true;
operation_supported(delete, {space_alias, _}, private) -> true;
operation_supported(delete, default_provider, private) -> true;

operation_supported(delete, {group, _}, private) -> true;
operation_supported(delete, {space, _}, private) -> true;
operation_supported(delete, {handle_service, _}, private) -> true;
operation_supported(delete, {handle, _}, private) -> true;
operation_supported(delete, {harvester, _}, private) -> true;
operation_supported(delete, {cluster, _}, private) -> true;

operation_supported(_, _, _) -> false.


%%--------------------------------------------------------------------
%% @doc
%% Determines if given {Aspect, Scope} pair is subscribable, i.e. clients can
%% subscribe to receive updates concerning the aspect of entity.
%% @end
%%--------------------------------------------------------------------
-spec is_subscribable(entity_logic:aspect(), entity_logic:scope()) ->
    boolean().
is_subscribable(instance, _) -> true;
%% @TODO VFS-5846 old client tokens API kept for backward compatibility
is_subscribable(client_tokens, private) -> true;
is_subscribable({client_token, _}, private) -> true;
is_subscribable(linked_accounts, private) -> true;
is_subscribable({linked_account, _}, private) -> true;
is_subscribable(eff_groups, private) -> true;
is_subscribable(eff_spaces, private) -> true;
is_subscribable(eff_providers, private) -> true;
is_subscribable(eff_harvesters, private) -> true;
is_subscribable(eff_clusters, private) -> true;
is_subscribable(_, _) -> false.

%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec create(entity_logic:req()) -> entity_logic:create_result().
create(#el_req{gri = GRI = #gri{id = ProposedUserId, aspect = instance}, data = Data}) ->
    % Creating users with predefined UserId is reserved for Onezone logic (?ROOT auth).
    % Users with ?OZ_USERS_CREATE privilege can create new users but the UserIds are
    % assigned automatically (in this case ProposedUserId is undefined).
    FullName = maps:get(<<"fullName">>, Data, ?DEFAULT_FULL_NAME),
    Username = maps:get(<<"username">>, Data, undefined),
    BasicUserRecord = #od_user{full_name = FullName, username = Username},

    UserRecord = case maps:find(<<"password">>, Data) of
        error ->
            BasicUserRecord;
        {ok, Password} ->
            {ok, UserRecord2} = basic_auth:toggle_basic_auth(BasicUserRecord, true),
            {ok, UserRecord3} = basic_auth:set_password(UserRecord2, Password),
            UserRecord3
    end,

    CreateFun = fun() ->
        case od_user:create(#document{key = ProposedUserId, value = UserRecord}) of
            {error, already_exists} ->
                ?ERROR_ALREADY_EXISTS;
            {ok, #document{key = UserId}} ->
                set_up_user(UserId),
                {true, {User, Rev}} = fetch_entity(#gri{aspect = instance, id = UserId}),
                {ok, resource, {GRI#gri{id = UserId}, {User, Rev}}}
        end
    end,

    case Username of
        undefined ->
            CreateFun();
        _ ->
            ?LOCK_ON_USERNAME(Username, fun() ->
                case od_user:get_by_username(Username) of
                    {ok, #document{}} ->
                        % Username is occupied by another user
                        ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"username">>);
                    _ ->
                        CreateFun()
                end
            end)
    end;

create(#el_req{gri = #gri{id = UserId, aspect = client_tokens} = GRI}) ->
    %% @TODO VFS-5846 old client tokens API kept for backward compatibility
    case token_logic:create_legacy_client_token(?USER(UserId)) of
        {ok, Token} ->
            {ok, Serialized} = tokens:serialize(Token),
            {ok, resource, {GRI#gri{aspect = {client_token, Serialized}}, {Serialized, inherit_rev}}};
        {error, _} = Error ->
            Error
    end;




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
    ok;

create(Req = #el_req{gri = GRI = #gri{aspect = {idp_access_token, IdPBin}}}) when is_binary(IdPBin) ->
    create(Req#el_req{gri = GRI#gri{aspect = {idp_access_token, binary_to_existing_atom(IdPBin, utf8)}}});
create(#el_req{gri = #gri{aspect = {idp_access_token, IdP}}}) ->
    fun(User) ->
        case idp_auth:acquire_idp_access_token(User#od_user.linked_accounts, IdP) of
            {ok, {AccessToken, Expires}} -> {ok, value, {AccessToken, Expires}};
            {error, _} = Error -> Error
        end
    end;

create(#el_req{auth = Auth, gri = #gri{id = UserId, aspect = provider_registration_token}}) ->
    %% @TODO VFS-5815 deprecated, should be removed in the next major version AFTER 19.09.*
    token_logic:create_legacy_invite_token(Auth, ?REGISTER_ONEPROVIDER, UserId).


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
        basic_auth_enabled = BasicAuthEnabled,
        full_name = FullName, username = Username, emails = Emails,
        linked_accounts = LinkedAccounts, creation_time = CreationTime
    } = User,
    {ok, #{
        <<"basicAuthEnabled">> => BasicAuthEnabled,
        <<"fullName">> => FullName, <<"username">> => Username,
        <<"emails">> => Emails,
        <<"linkedAccounts">> => linked_accounts:to_maps(LinkedAccounts, all_fields),
        <<"creationTime">> => CreationTime
    }};
get(#el_req{gri = #gri{aspect = instance, scope = shared}}, User) ->
    #od_user{full_name = FullName, username = Username, creation_time = CreationTime} = User,
    {ok, #{
        <<"fullName">> => FullName,
        <<"username">> => Username,
        <<"creationTime">> => CreationTime
    }};

get(#el_req{gri = #gri{aspect = oz_privileges}}, User) ->
    {ok, entity_graph:get_oz_privileges(direct, User)};
get(#el_req{gri = #gri{aspect = eff_oz_privileges}}, User) ->
    {ok, entity_graph:get_oz_privileges(effective, User)};

%% @TODO VFS-5846 old client tokens API kept for backward compatibility
get(#el_req{auth = Auth, gri = #gri{id = UserId, aspect = client_tokens}}, _User) ->
    case token_logic:list_user_named_tokens(Auth, UserId) of
        {error, _} = Error ->
            Error;
        {ok, UserTokens} ->
            {ok, lists:filtermap(fun(TokenId) ->
                case od_token:get(TokenId) of
                    {ok, #document{value = #od_token{type = ?ACCESS_TOKEN} = NamedToken}} ->
                        Token = od_token:named_token_to_token(TokenId, NamedToken),
                        {ok, Serialized} = tokens:serialize(Token),
                        {true, Serialized};
                    _ ->
                        false
                end
            end, UserTokens)}
    end;

%% @TODO VFS-5846 old client tokens API kept for backward compatibility
get(#el_req{gri = #gri{aspect = {client_token, Serialized}}}, _User) ->
    {ok, Serialized};

get(#el_req{gri = #gri{aspect = linked_accounts}}, User) ->
    {ok, lists:map(fun linked_accounts:gen_user_id/1, User#od_user.linked_accounts)};
get(#el_req{gri = #gri{aspect = {linked_account, UserId}}}, User) ->
    {ok, find_linked_account(UserId, User#od_user.linked_accounts)};

get(#el_req{gri = #gri{aspect = default_space}}, User) ->
    {ok, User#od_user.default_space};
get(#el_req{gri = #gri{aspect = {space_alias, SpaceId}}}, User) ->
    {ok, maps:get(SpaceId, User#od_user.space_aliases)};
get(#el_req{gri = #gri{aspect = default_provider}}, User) ->
    {ok, User#od_user.default_provider};

get(#el_req{gri = #gri{aspect = groups}}, User) ->
    {ok, entity_graph:get_relations(direct, top_down, od_group, User)};
get(#el_req{gri = #gri{aspect = eff_groups}}, User) ->
    {ok, entity_graph:get_relations(effective, top_down, od_group, User)};

get(#el_req{gri = #gri{aspect = spaces}}, User) ->
    {ok, entity_graph:get_relations(direct, top_down, od_space, User)};
get(#el_req{gri = #gri{aspect = eff_spaces}}, User) ->
    {ok, entity_graph:get_relations(effective, top_down, od_space, User)};

get(#el_req{gri = #gri{aspect = eff_providers}}, User) ->
    {ok, entity_graph:get_relations(effective, top_down, od_provider, User)};

get(#el_req{gri = #gri{aspect = handle_services}}, User) ->
    {ok, entity_graph:get_relations(direct, top_down, od_handle_service, User)};
get(#el_req{gri = #gri{aspect = eff_handle_services}}, User) ->
    {ok, entity_graph:get_relations(effective, top_down, od_handle_service, User)};

get(#el_req{gri = #gri{aspect = handles}}, User) ->
    {ok, entity_graph:get_relations(direct, top_down, od_handle, User)};
get(#el_req{gri = #gri{aspect = eff_handles}}, User) ->
    {ok, entity_graph:get_relations(effective, top_down, od_handle, User)};

get(#el_req{gri = #gri{aspect = harvesters}}, User) ->
    {ok, entity_graph:get_relations(direct, top_down, od_harvester, User)};
get(#el_req{gri = #gri{aspect = eff_harvesters}}, User) ->
    {ok, entity_graph:get_relations(effective, top_down, od_harvester, User)};

get(#el_req{gri = #gri{aspect = clusters}}, User) ->
    {ok, entity_graph:get_relations(direct, top_down, od_cluster, User)};
get(#el_req{gri = #gri{aspect = eff_clusters}}, User) ->
    {ok, entity_graph:get_relations(effective, top_down, od_cluster, User)}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec update(entity_logic:req()) -> entity_logic:update_result().
update(#el_req{gri = #gri{id = UserId, aspect = instance}, data = Data}) ->
    UserUpdateFun = fun(NewUsername) ->
        {ok, _} = od_user:update(UserId, fun(User) ->
            {ok, User#od_user{
                full_name = maps:get(<<"fullName">>, Data, User#od_user.full_name),
                username = case NewUsername of
                    keep -> User#od_user.username;
                    Bin when is_binary(Bin) -> NewUsername
                end

            }}
        end),
        ok
    end,

    % If username is specified, run update in synchronized block so no two
    % identical usernames can be set
    case maps:find(<<"username">>, Data) of
        error ->
            UserUpdateFun(keep);
        {ok, Username} ->
            ?LOCK_ON_USERNAME(Username, fun() ->
                case od_user:get_by_username(Username) of
                    {ok, #document{key = UserId}} ->
                        % Username is held by then same user, so it was changed to
                        % identical -> update user doc
                        % (the full_name might have changed)
                        UserUpdateFun(Username);
                    {ok, #document{}} ->
                        % Username is occupied by another user
                        ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"username">>);
                    _ ->
                        % Username is not occupied -> update user doc
                        UserUpdateFun(Username)
                end
            end)
    end;

update(#el_req{gri = #gri{id = UserId, aspect = password}, data = Data}) ->
    OldPassword = maps:get(<<"oldPassword">>, Data),
    NewPassword = maps:get(<<"newPassword">>, Data),

    Result = od_user:update(UserId, fun(User) ->
        basic_auth:change_password(User, OldPassword, NewPassword)
    end),

    case Result of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end;

update(#el_req{gri = #gri{id = UserId, aspect = basic_auth}, data = Data}) ->
    Result = od_user:update(UserId, fun(User) ->
        {ok, User2} = case maps:find(<<"basicAuthEnabled">>, Data) of
            {ok, Flag} -> basic_auth:toggle_basic_auth(User, Flag);
            error -> {ok, User}
        end,
        case maps:find(<<"newPassword">>, Data) of
            {ok, NewPassword} -> basic_auth:set_password(User2, NewPassword);
            error -> {ok, User2}
        end
    end),

    case Result of
        {ok, _} -> ok;
        {error, _} = Error -> Error
    end;

update(#el_req{gri = #gri{id = UserId, aspect = oz_privileges}, data = Data}) ->
    PrivsToGrant = maps:get(<<"grant">>, Data, []),
    PrivsToRevoke = maps:get(<<"revoke">>, Data, []),
    entity_graph:update_oz_privileges(od_user, UserId, PrivsToGrant, PrivsToRevoke).


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on entity logic request.
%% @end
%%--------------------------------------------------------------------
-spec delete(entity_logic:req()) -> entity_logic:delete_result().
delete(#el_req{gri = #gri{id = UserId, aspect = instance}}) ->
    % Invalidate client tokens
    token_logic:delete_all_user_named_tokens(?USER(UserId), UserId),
    entity_graph:delete_with_relations(od_user, UserId);

delete(#el_req{gri = #gri{id = UserId, aspect = oz_privileges}}) ->
    update(#el_req{gri = #gri{id = UserId, aspect = oz_privileges}, data = #{
        <<"grant">> => [], <<"revoke">> => privileges:oz_privileges()
    }});

%% @TODO VFS-5846 old client tokens API kept for backward compatibility
delete(#el_req{gri = #gri{id = UserId, aspect = {client_token, Serialized}}}) ->
    {ok, #token{id = TokenId}} = tokens:deserialize(Serialized),
    token_logic:delete_named_token(?USER(UserId), TokenId);

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
    );

delete(#el_req{gri = #gri{id = UserId, aspect = {harvester, HarvesterId}}}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_harvester, HarvesterId
    );

delete(#el_req{gri = #gri{id = UserId, aspect = {cluster, ClusterId}}}) ->
    entity_graph:remove_relation(
        od_user, UserId,
        od_cluster, ClusterId
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

exists(Req = #el_req{gri = #gri{id = UserId, aspect = instance, scope = shared}}, User) ->
    case Req#el_req.auth_hint of
        ?THROUGH_GROUP(GroupId) ->
            user_logic:has_eff_group(User, GroupId) orelse begin
                {true, {Group, _}} = group_logic_plugin:fetch_entity(#gri{id = GroupId}),
                Group#od_group.creator =:= ?SUB(user, UserId)
            end;
        ?THROUGH_SPACE(SpaceId) ->
            user_logic:has_eff_space(User, SpaceId) orelse begin
                {true, {Space, _}} = space_logic_plugin:fetch_entity(#gri{id = SpaceId}),
                Space#od_space.creator =:= ?SUB(user, UserId)
            end;
        ?THROUGH_HANDLE_SERVICE(HServiceId) ->
            user_logic:has_eff_handle_service(User, HServiceId) orelse begin
                {true, {HService, _}} = handle_service_logic_plugin:fetch_entity(#gri{id = HServiceId}),
                HService#od_handle_service.creator =:= ?SUB(user, UserId)
            end;
        ?THROUGH_HANDLE(HandleId) ->
            user_logic:has_eff_handle(User, HandleId) orelse begin
                {true, {Handle, _}} = handle_logic_plugin:fetch_entity(#gri{id = HandleId}),
                Handle#od_handle.creator =:= ?SUB(user, UserId)
            end;
        ?THROUGH_HARVESTER(HarvesterId) ->
            user_logic:has_eff_harvester(User, HarvesterId) orelse begin
                {true, {Harvester, _}} = harvester_logic_plugin:fetch_entity(#gri{id = HarvesterId}),
                Harvester#od_harvester.creator =:= ?SUB(user, UserId)
            end;
        ?THROUGH_CLUSTER(ClusterId) ->
            user_logic:has_eff_cluster(User, ClusterId) orelse begin
                {true, {Cluster, _}} = cluster_logic_plugin:fetch_entity(#gri{id = ClusterId}),
                Cluster#od_cluster.creator =:= ?SUB(user, UserId)
            end;
        undefined ->
            true
    end;

%% @TODO VFS-5846 old client tokens API kept for backward compatibility
exists(#el_req{gri = #gri{aspect = {client_token, Serialized}}}, _User) ->
    {ok, #token{id = TokenId}} = tokens:deserialize(Serialized),
    token_logic:exists(TokenId);

exists(#el_req{gri = #gri{aspect = {linked_account, SubId}}}, User) ->
    find_linked_account(SubId, User#od_user.linked_accounts) /= undefined;

exists(#el_req{gri = #gri{aspect = default_space}}, User) ->
    undefined =/= User#od_user.default_space;

exists(#el_req{gri = #gri{aspect = {space_alias, SpaceId}}}, User) ->
    maps:is_key(SpaceId, User#od_user.space_aliases);

exists(#el_req{gri = #gri{aspect = default_provider}}, User) ->
    undefined =/= User#od_user.default_provider;

exists(#el_req{gri = #gri{aspect = {group, GroupId}}}, User) ->
    entity_graph:has_relation(direct, top_down, od_group, GroupId, User);

exists(#el_req{gri = #gri{aspect = {space, SpaceId}}}, User) ->
    entity_graph:has_relation(direct, top_down, od_space, SpaceId, User);

exists(#el_req{gri = #gri{aspect = {handle_service, HServiceId}}}, User) ->
    entity_graph:has_relation(direct, top_down, od_handle_service, HServiceId, User);

exists(#el_req{gri = #gri{aspect = {handle, HandleId}}}, User) ->
    entity_graph:has_relation(direct, top_down, od_handle, HandleId, User);

exists(#el_req{gri = #gri{aspect = {harvester, HarvesterId}}}, User) ->
    entity_graph:has_relation(direct, top_down, od_harvester, HarvesterId, User);

exists(#el_req{gri = #gri{aspect = {cluster, ClusterId}}}, User) ->
    entity_graph:has_relation(direct, top_down, od_cluster, ClusterId, User);

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

%% Operations reserved for admins or available to users in certain circumstances
authorize(Req = #el_req{operation = create, gri = #gri{id = UserId, aspect = provider_registration_token}}, _) ->
    case Req#el_req.auth of
        ?USER(UserId) ->
            % Issuing provider registration token for self. In case of 'restricted'
            % policy, the admin rights will be checked in required_admin_privileges/1
            open =:= oz_worker:get_env(provider_registration_policy, open);
        _ ->
            false
    end;

authorize(#el_req{auth = ?USER(UserId), operation = Operation, gri = #gri{id = UserId, aspect = oz_privileges}}, _) ->
    % Regular users are allowed only to view their own OZ privileges
    % (other operations are checked in required_admin_privileges/1)
    Operation =:= get;

authorize(#el_req{operation = update, gri = #gri{aspect = basic_auth}}, _) ->
    % basic_auth settings modification is restricted to admins only
    false;


%% User can perform all operations on his record except the restricted ones handled above
authorize(#el_req{auth = ?USER(UserId), gri = #gri{id = UserId}}, _) ->
    true;


%% Operations available to other subjects
authorize(Req = #el_req{operation = get, gri = GRI = #gri{aspect = instance, scope = protected}}, User) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?PROVIDER(ProviderId), ?THROUGH_PROVIDER(ProviderId)} ->
            % User's membership in provider is checked in 'exists'
            true;

        {?USER(ClientUserId), ?THROUGH_PROVIDER(ProviderId)} ->
            % User's membership in provider is checked in 'exists'
            ClusterId = ProviderId,
            cluster_logic:has_eff_privilege(ClusterId, ClientUserId, ?CLUSTER_VIEW);

        _ ->
            % Access to private data also allows access to protected data
            authorize(Req#el_req{gri = GRI#gri{scope = private}}, User)
    end;

authorize(Req = #el_req{operation = get, gri = GRI = #gri{id = UserId, aspect = instance, scope = shared}}, User) ->
    case {Req#el_req.auth, Req#el_req.auth_hint} of
        {?USER(ClientUserId), ?THROUGH_GROUP(GroupId)} ->
            {true, {Group, _}} = group_logic_plugin:fetch_entity(#gri{id = GroupId}),
            % UserId's membership in group is checked in 'exists'
            group_logic:has_eff_privilege(Group, ClientUserId, ?GROUP_VIEW) orelse begin
            % Members of a group can see the shared data of its creator
                group_logic:has_eff_user(Group, ClientUserId) andalso
                    Group#od_group.creator =:= ?SUB(user, UserId)
            end;

        {?USER(ClientUserId), ?THROUGH_SPACE(SpaceId)} ->
            {true, {Space, _}} = space_logic_plugin:fetch_entity(#gri{id = SpaceId}),
            % UserId's membership in space is checked in 'exists'
            space_logic:has_eff_privilege(Space, ClientUserId, ?SPACE_VIEW) orelse begin
            % Members of a space can see the shared data of its creator
                space_logic:has_eff_user(Space, ClientUserId) andalso
                    Space#od_space.creator =:= ?SUB(user, UserId)
            end;

        {?USER(ClientUserId), ?THROUGH_HANDLE_SERVICE(HServiceId)} ->
            {true, {HService, _}} = handle_service_logic_plugin:fetch_entity(#gri{id = HServiceId}),
            % UserId's membership in handle_service is checked in 'exists'
            handle_service_logic:has_eff_privilege(HService, ClientUserId, ?HANDLE_SERVICE_VIEW) orelse begin
            % Members of a handle service can see the shared data of its creator
                handle_service_logic:has_eff_user(HService, ClientUserId) andalso
                    HService#od_handle_service.creator =:= ?SUB(user, UserId)
            end;

        {?USER(ClientUserId), ?THROUGH_HANDLE(HandleId)} ->
            {true, {Handle, _}} = handle_logic_plugin:fetch_entity(#gri{id = HandleId}),
            % UserId's membership in handle is checked in 'exists'
            handle_logic:has_eff_privilege(Handle, ClientUserId, ?HANDLE_VIEW) orelse begin
            % Members of a handle can see the shared data of its creator
                handle_logic:has_eff_user(Handle, ClientUserId) andalso
                    Handle#od_handle.creator =:= ?SUB(user, UserId)
            end;

        {?USER(ClientUserId), ?THROUGH_HARVESTER(HarvesterId)} ->
            {true, {Harvester, _}} = harvester_logic_plugin:fetch_entity(#gri{id = HarvesterId}),
            % UserId's membership in harvester is checked in 'exists'
            harvester_logic:has_eff_privilege(Harvester, ClientUserId, ?HARVESTER_VIEW) orelse begin
            % Members of a harvester can see the shared data of its creator
                harvester_logic:has_eff_user(Harvester, ClientUserId) andalso
                    Harvester#od_harvester.creator =:= ?SUB(user, UserId)
            end;

        {?USER(ClientUserId), ?THROUGH_CLUSTER(ClusterId)} ->
            {true, {Cluster, _}} = cluster_logic_plugin:fetch_entity(#gri{id = ClusterId}),
            % UserId's membership in cluster is checked in 'exists'
            cluster_logic:has_eff_privilege(Cluster, ClientUserId, ?CLUSTER_VIEW) orelse begin
            % Members of a cluster can see the shared data of its creator
                cluster_logic:has_eff_user(Cluster, ClientUserId) andalso
                    Cluster#od_cluster.creator =:= ?SUB(user, UserId)
            end;

        {?PROVIDER(ProviderId), ?THROUGH_CLUSTER(ClusterId)} ->
            cluster_logic:is_provider_cluster(ClusterId, ProviderId);

        _ ->
            % Access to protected data also allows access to shared data
            authorize(Req#el_req{gri = GRI#gri{scope = protected}}, User)
    end;

authorize(_, _) ->
    false.


%%--------------------------------------------------------------------
%% @doc
%% Returns list of admin privileges needed to perform given operation.
%% @end
%%--------------------------------------------------------------------
-spec required_admin_privileges(entity_logic:req()) -> [privileges:oz_privilege()] | forbidden.
required_admin_privileges(#el_req{operation = create, gri = #gri{id = undefined, aspect = instance}}) ->
    % Creating users with predefined UserId is reserved for Onezone logic (?ROOT auth).
    % Users with ?OZ_USERS_CREATE privilege can create new users but the UserIds are
    % assigned automatically.
    [?OZ_USERS_CREATE];

required_admin_privileges(#el_req{operation = create, gri = #gri{aspect = provider_registration_token}}) ->
    [?OZ_PROVIDERS_INVITE];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = list}}) ->
    [?OZ_USERS_LIST];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = oz_privileges}}) ->
    [?OZ_VIEW_PRIVILEGES];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_oz_privileges}}) ->
    [?OZ_VIEW_PRIVILEGES];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance, scope = shared}}) ->
    [?OZ_USERS_VIEW];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = instance, scope = protected}}) ->
    [?OZ_USERS_VIEW];

required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = groups}}) ->
    [?OZ_USERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_groups}}) ->
    [?OZ_USERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = spaces}}) ->
    [?OZ_USERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_spaces}}) ->
    [?OZ_USERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_providers}}) ->
    [?OZ_USERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = handle_services}}) ->
    [?OZ_USERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_handle_services}}) ->
    [?OZ_USERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = handles}}) ->
    [?OZ_USERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_handles}}) ->
    [?OZ_USERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = harvesters}}) ->
    [?OZ_USERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_harvesters}}) ->
    [?OZ_USERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = clusters}}) ->
    [?OZ_USERS_LIST_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = get, gri = #gri{aspect = eff_clusters}}) ->
    [?OZ_USERS_LIST_RELATIONSHIPS];

required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = instance}}) ->
    [?OZ_USERS_UPDATE];
required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = basic_auth}}) ->
    [?OZ_USERS_MANAGE_PASSWORDS];
required_admin_privileges(#el_req{operation = update, gri = #gri{aspect = oz_privileges}}) ->
    [?OZ_SET_PRIVILEGES];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = instance}}) ->
    [?OZ_USERS_DELETE];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = oz_privileges}}) ->
    [?OZ_SET_PRIVILEGES];

required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {group, _}}}) ->
    [?OZ_USERS_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {space, _}}}) ->
    [?OZ_USERS_REMOVE_RELATIONSHIPS, ?OZ_SPACES_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {handle_service, _}}}) ->
    [?OZ_USERS_REMOVE_RELATIONSHIPS, ?OZ_HANDLE_SERVICES_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {handle, _}}}) ->
    [?OZ_USERS_REMOVE_RELATIONSHIPS, ?OZ_HANDLES_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {harvester, _}}}) ->
    [?OZ_USERS_REMOVE_RELATIONSHIPS, ?OZ_HARVESTERS_REMOVE_RELATIONSHIPS];
required_admin_privileges(#el_req{operation = delete, gri = #gri{aspect = {cluster, _}}}) ->
    [?OZ_USERS_REMOVE_RELATIONSHIPS, ?OZ_CLUSTERS_REMOVE_RELATIONSHIPS];

required_admin_privileges(_) ->
    forbidden.


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
validate(#el_req{operation = create, gri = #gri{aspect = instance}, data = Data}) ->
    case maps:is_key(<<"password">>, Data) of
        true -> #{
            required => #{
                <<"username">> => {binary, username},
                <<"password">> => {binary, password}
            },
            optional => #{
                <<"fullName">> => {binary, full_name}
            }
        };
        false -> #{
            optional => #{
                <<"fullName">> => {binary, full_name},
                <<"username">> => {binary, username}
            }
        }
    end;

%% @TODO VFS-5846 old client tokens API kept for backward compatibility
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

validate(#el_req{operation = create, gri = #gri{aspect = {idp_access_token, _}}}) -> #{
    required => #{
        {aspect, <<"idp">>} => {atom, auth_config:get_idps_with_offline_access()}
    }
};

validate(#el_req{operation = create, gri = #gri{aspect = provider_registration_token}}) -> #{
};

validate(#el_req{operation = update, gri = #gri{aspect = instance}}) -> #{
    at_least_one => #{
        <<"fullName">> => {binary, full_name},
        <<"username">> => {binary, username}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = password}}) -> #{
    required => #{
        <<"oldPassword">> => {binary, any},
        <<"newPassword">> => {binary, password}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = basic_auth}}) -> #{
    at_least_one => #{
        <<"basicAuthEnabled">> => {boolean, any},
        <<"newPassword">> => {binary, password}
    }
};

validate(#el_req{operation = update, gri = #gri{aspect = oz_privileges}}) -> #{
    at_least_one => #{
        <<"grant">> => {list_of_atoms, privileges:oz_privileges()},
        <<"revoke">> => {list_of_atoms, privileges:oz_privileges()}
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
-spec auth_by_oz_privilege(entity_logic:req() | od_user:id() | od_user:record(),
    privileges:oz_privilege()) -> boolean().
auth_by_oz_privilege(#el_req{auth = ?USER(UserId)}, Privilege) ->
    auth_by_oz_privilege(UserId, Privilege);
auth_by_oz_privilege(#el_req{auth = _OtherAuth}, _Privilege) ->
    false;
auth_by_oz_privilege(UserOrId, Privilege) ->
    user_logic:has_eff_oz_privilege(UserOrId, Privilege).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Finds a linked account in given list based on UserId generated from the
%% linked account record.
%% @end
%%--------------------------------------------------------------------
-spec find_linked_account(GeneratedUserId :: binary(), [od_user:linked_account()]) ->
    undefined | od_user:linked_account().
find_linked_account(_, []) ->
    undefined;
find_linked_account(GeneratedUserId, [LinkedAccount | Rest]) ->
    case linked_accounts:gen_user_id(LinkedAccount) of
        GeneratedUserId ->
            LinkedAccount;
        _ ->
            find_linked_account(GeneratedUserId, Rest)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sets up environment for a new user by granting automatic space/group
%% memberships (depending on Onezone config).
%% @end
%%--------------------------------------------------------------------
-spec set_up_user(od_user:id()) -> ok.
set_up_user(UserId) ->
    case oz_worker:get_env(enable_automatic_first_space, false) of
        true ->
            {ok, _} = user_logic:create_space(?USER(UserId), UserId, ?FIRST_SPACE_NAME);
        _ ->
            ok
    end,

    case oz_worker:get_env(enable_global_groups, false) of
        true ->
            GlobalGroups = oz_worker:get_env(global_groups),
            lists:foreach(fun({GroupId, Privileges}) ->
                {ok, UserId} = group_logic:add_user(?ROOT, GroupId, UserId, Privileges),
                ?info("User '~s' has been added to global group '~s'", [UserId, GroupId])
            end, GlobalGroups);
        _ ->
            ok
    end.