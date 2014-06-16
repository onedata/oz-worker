%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The module handling logic behind /spaces REST resources.
%% ===================================================================
-module(spaces_rest_module).
-author("Konrad Zemek").

-include("handlers/rest_handler.hrl").

-behaviour(rest_module_behavior).


%% API
-export([routes/0, is_authorized/4, accept_resource/6, provide_resource/4,
    delete_resource/3, resource_exists/3]).


%% routes/0
%% ====================================================================
%% @doc Returns a Cowboy-understandable PathList of routes supported by a module
%% implementing this behavior.
%% @see rest_module_behavior
%% @end
-spec routes() ->
    [{PathMatch :: binary(), rest_handler, State :: rstate()}].
%% ====================================================================
routes() ->
    S = #rstate{module = ?MODULE},
    M = rest_handler,
    [
        {<<"/spaces">>,                              M, S#rstate{resource = spaces,    methods = [post]        }},
        {<<"/spaces/:id">>,                          M, S#rstate{resource = space,     methods = [get, patch, delete]}},
        {<<"/spaces/:id/users">>,                    M, S#rstate{resource = users,     methods = [get]         }},
        {<<"/spaces/:id/users/token">>,              M, S#rstate{resource = uinvite,   methods = [get]         }},
        {<<"/spaces/:id/users/:uid">>,               M, S#rstate{resource = user,      methods = [get, delete] }},
        {<<"/spaces/:id/users/:uid/privileges">>,    M, S#rstate{resource = upriv,     methods = [get, put]    }},
        {<<"/spaces/:id/groups">>,                   M, S#rstate{resource = groups,    methods = [get]         }},
        {<<"/spaces/:id/groups/token">>,             M, S#rstate{resource = ginvite,   methods = [get]         }},
        {<<"/spaces/:id/groups/:gid">>,              M, S#rstate{resource = group,     methods = [get, delete] }},
        {<<"/spaces/:id/groups/:gid/privileges">>,   M, S#rstate{resource = gpriv,     methods = [get, put]    }},
        {<<"/spaces/:id/providers">>,                M, S#rstate{resource = providers, methods = [get]         }},
        {<<"/spaces/:id/providers/token">>,          M, S#rstate{resource = pinvite,   methods = [get]         }},
        {<<"/spaces/:id/providers/:pid">>,           M, S#rstate{resource = provider,  methods = [get, delete] }}
    ].


%% is_authorized/4
%% ====================================================================
%% @doc Returns a boolean() determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec is_authorized(Resource :: atom(), Method :: method(),
                    SpaceId :: binary() | undefined, Client :: client()) ->
    boolean().
%% ====================================================================
is_authorized(_, _, _, #client{type = undefined}) ->
    false;
is_authorized(spaces, post, _SpaceId, _Client) ->
    true;
is_authorized(space, patch, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_privilege(SpaceId, UserId, space_change_data);
is_authorized(space, delete, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_privilege(SpaceId, UserId, space_remove);
is_authorized(uinvite, get, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_privilege(SpaceId, UserId, space_invite_user);
is_authorized(user, delete, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_privilege(SpaceId, UserId, space_remove_user);
is_authorized(ginvite, get, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_privilege(SpaceId, UserId, space_invite_group);
is_authorized(group, delete, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_privilege(SpaceId, UserId, space_remove_group);
is_authorized(pinvite, get, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_privilege(SpaceId, UserId, space_add_provider);
is_authorized(provider, delete, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_privilege(SpaceId, UserId, space_remove_provider);
is_authorized(R, put, SpaceId, #client{type = user, id = UserId})
        when R =:= upriv; R =:= gpriv ->
    space_logic:has_privilege(SpaceId, UserId, space_set_privileges);
is_authorized(_, get, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_privilege(SpaceId, UserId, space_view_data);
is_authorized(R, get, SpaceId, #client{type = provider, id = ProviderId})
        when R =/= groups, R =/= ginvite, R =/= group, R =/= gpriv ->
    space_logic:has_provider(SpaceId, ProviderId);
is_authorized(_, _, _, _) ->
    false.


%% resource_exists/3
%% ====================================================================
%% @doc Returns whether a resource exists.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec resource_exists(Resource :: atom(), SpaceId :: binary() | undefined,
                      Bindings :: [{atom(), any()}]) -> boolean().
%% ====================================================================
resource_exists(spaces, _SpaceId, _Bindings) ->
    true;
resource_exists(UserBound, SpaceId, Bindings) when UserBound =:= user; UserBound =:= upriv ->
    UID = proplists:get_value(uid, Bindings),
    space_logic:has_user(SpaceId, UID);
resource_exists(GroupBound, SpaceId, Bindings) when GroupBound =:= group; GroupBound =:= gpriv ->
    GID = proplists:get_value(gid, Bindings),
    space_logic:has_group(SpaceId, GID);
resource_exists(provider, SpaceId, Bindings) ->
    PID = proplists:get_value(pid, Bindings),
    space_logic:has_provider(SpaceId, PID);
resource_exists(_, SpaceId, _Bindings) ->
    space_logic:exists(SpaceId).


%% accept_resource/6
%% ====================================================================
%% @doc Processes data submitted by a client through POST, PATCH on a REST
%% resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec accept_resource(Resource :: atom(), Method :: method(),
                      SpaceId :: binary() | undefined,
                      Data :: [proplists:property()], Client :: client(),
                      Bindings :: [{atom(), any()}]) ->
    {true, URL :: binary()} | boolean().
%% ====================================================================
accept_resource(spaces, post, _SpaceId, Data, #client{type = user, id = UserId}, _Bindings) ->
    Name = proplists:get_value(<<"name">>, Data),
    if
        Name =:= undefined -> false;
        true ->
            {ok, SpaceId} = space_logic:create({user, UserId}, Name),
            {true, <<"/spaces/", SpaceId/binary>>}
    end;
accept_resource(spaces, post, _SpaceId, Data, #client{type = provider, id = ProviderId}, _Bindings) ->
    Name = proplists:get_value(<<"name">>, Data),
    Token = proplists:get_value(<<"token">>, Data),
    if
        Name =:= undefined -> false;
        true -> case token_logic:is_valid(Token, space_create_token) of
            false -> false;
            true ->
                {ok, SpaceId} = space_logic:create({provider, ProviderId}, Name, Token),
                {true, <<"/spaces/", SpaceId/binary>>}
        end
    end;
accept_resource(space, patch, SpaceId, Data, _Client, _Bindings) ->
    Name = proplists:get_value(<<"name">>, Data),
    if
        Name =:= undefined -> false;
        true ->
            ok = space_logic:modify(SpaceId, Name),
            true
    end;
accept_resource(upriv, put, SpaceId, Data, _Client, Bindings) ->
    UID = proplists:get_value(uid, Bindings),
    BinPrivileges = proplists:get_value(<<"privileges">>, Data),
    if
        BinPrivileges =:= undefined -> false;
        true ->
            Privileges = [binary_to_existing_atom(P, latin1) || P <- BinPrivileges],
            ok = space_logic:set_privileges(SpaceId, {user, UID}, Privileges),
            true
    end;
accept_resource(gpriv, put, SpaceId, Data, _Client, Bindings) ->
    GID = proplists:get_value(gid, Bindings),
    BinPrivileges = proplists:get_value(<<"privileges">>, Data),
    if
        BinPrivileges =:= undefined -> false;
        true ->
            Privileges = [binary_to_existing_atom(P, latin1) || P <- BinPrivileges],
            ok = space_logic:set_privileges(SpaceId, {group, GID}, Privileges),
            true
    end.


%% provide_resource/4
%% ====================================================================
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec provide_resource(Resource :: atom(), SpaceId :: binary() | undefined,
                       Client :: client(), Bindings :: [{atom(), any()}]) ->
    Data :: [proplists:property()].
%% ====================================================================
provide_resource(space, SpaceId, #client{type = ClientType}, _Bindings) ->
    {ok, Data} = space_logic:get_data(SpaceId, ClientType),
    Data;
provide_resource(users, SpaceId, #client{type = ClientType}, _Bindings) ->
    {ok, Users} = space_logic:get_users(SpaceId, ClientType),
    Users;
provide_resource(uinvite, SpaceId, _Client, _Bindings) ->
    {ok, Token} = token_logic:create(space_invite_user_token, {space, SpaceId}),
    [{token, Token}];
provide_resource(user, SpaceId, #client{type = ClientType}, Bindings) ->
    UID = proplists:get_value(uid, Bindings),
    {ok, User} = space_logic:get_user(SpaceId, ClientType, UID),
    User;
provide_resource(upriv, SpaceId, _Client, Bindings) ->
    UID = proplists:get_value(uid, Bindings),
    {ok, Privileges} = space_logic:get_privileges(SpaceId, {user, UID}),
    [{privileges, Privileges}];
provide_resource(groups, SpaceId, _Client, _Bindings) ->
    {ok, Groups} = space_logic:get_groups(SpaceId),
    Groups;
provide_resource(ginvite, SpaceId, _Client, _Bindings) ->
    {ok, Token} = token_logic:create(space_invite_group_token, {space, SpaceId}),
    [{token, Token}];
provide_resource(group, SpaceId, _Client, Bindings) ->
    GID = proplists:get_value(gid, Bindings),
    {ok, Group} = space_logic:get_group(SpaceId, GID),
    Group;
provide_resource(gpriv, SpaceId, _Client, Bindings) ->
    GID = proplists:get_value(gid, Bindings),
    {ok, Privileges} = space_logic:get_privileges(SpaceId, {group, GID}),
    [{privileges, Privileges}];
provide_resource(providers, SpaceId, #client{type = ClientType}, _Bindings) ->
    {ok, Providers} = space_logic:get_providers(SpaceId, ClientType),
    Providers;
provide_resource(pinvite, SpaceId, _Client, _Bindings) ->
    {ok, Token} = token_logic:create(space_support_token, {space, SpaceId}),
    [{token, Token}];
provide_resource(provider, SpaceId, #client{type = ClientType}, Bindings) ->
    PID = proplists:get_value(pid, Bindings),
    {ok, Provider} = space_logic:get_provider(SpaceId, ClientType, PID),
    Provider.


%% delete_resource/3
%% ====================================================================
%% @doc Deletes the resource identified by the SpaceId parameter.
%% @see rest_module_behavior
%% @end
%% ====================================================================
-spec delete_resource(Resource :: atom(), SpaceId :: binary() | undefined,
                      Bindings :: [{atom(), any()}]) -> boolean().
%% ====================================================================
delete_resource(space, SpaceId, _Bindings) ->
    space_logic:remove(SpaceId);
delete_resource(user, SpaceId, Bindings) ->
    UID = proplists:get_value(uid, Bindings),
    space_logic:remove_user(SpaceId, UID);
delete_resource(group, SpaceId, Bindings) ->
    GID = proplists:get_value(gid, Bindings),
    space_logic:remove_group(SpaceId, GID);
delete_resource(provider, SpaceId, Bindings) ->
    PID = proplists:get_value(pid, Bindings),
    space_logic:remove_provider(SpaceId, PID).
