%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /spaces REST resources.
%%%-------------------------------------------------------------------
-module(spaces_rest_module).
-author("Konrad Zemek").

-include("http/handlers/rest_handler.hrl").
-include_lib("ctool/include/logging.hrl").

-behavior(rest_module_behavior).


-type provided_resource() :: spaces | space | shares | users |
uinvite | user | upriv | groups | ginvite | group | gpriv | providers |
pinvite | provider.
-type accepted_resource() :: spaces | space | share | upriv | gpriv.
-type removable_resource() :: space | user | group | provider.
-type resource() :: provided_resource() | accepted_resource() | removable_resource().

%% API
-export([routes/0, is_authorized/4, accept_resource/6, provide_resource/4,
    delete_resource/3, resource_exists/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a Cowboy-understandable PathList of routes supported by a module
%% implementing this behavior. The paths should not include rest_api_prefix, as
%% it is added automatically.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec routes() ->
    [{PathMatch :: binary(), rest_handler, State :: rstate()}].
routes() ->
    S = #rstate{module = ?MODULE, root = spaces},
    M = rest_handler,
    [
        {<<"/spaces">>, M, S#rstate{resource = spaces, methods = [post, get]}},
        {<<"/spaces/:id">>, M, S#rstate{resource = space, methods = [get, patch, delete]}},
        {<<"/spaces/:id/shares">>, M, S#rstate{resource = shares, methods = [get]}},
        {<<"/spaces/:id/shares/:sid">>, M, S#rstate{resource = share, methods = [put]}}, %TODO POST
        {<<"/spaces/:id/users">>, M, S#rstate{resource = users, methods = [get, post]}},
        {<<"/spaces/:id/users/token">>, M, S#rstate{resource = uinvite, methods = [get]}},
        {<<"/spaces/:id/users/:uid">>, M, S#rstate{resource = user, methods = [get, delete]}},
        {<<"/spaces/:id/users/:uid/privileges">>, M, S#rstate{resource = upriv, methods = [get, put]}},
        {<<"/spaces/:id/groups">>, M, S#rstate{resource = groups, methods = [get, post]}},
        {<<"/spaces/:id/groups/token">>, M, S#rstate{resource = ginvite, methods = [get]}},
        {<<"/spaces/:id/groups/:gid">>, M, S#rstate{resource = group, methods = [get, delete]}},
        {<<"/spaces/:id/groups/:gid/privileges">>, M, S#rstate{resource = gpriv, methods = [get, put]}},
        {<<"/spaces/:id/providers">>, M, S#rstate{resource = providers, methods = [get]}},
        {<<"/spaces/:id/providers/token">>, M, S#rstate{resource = pinvite, methods = [get]}},
        {<<"/spaces/:id/providers/:pid">>, M, S#rstate{resource = provider, methods = [get, delete]}}
    ].

%%--------------------------------------------------------------------
%% @doc Returns a boolean() determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Resource :: resource(), Method :: method(),
    SpaceId :: binary() | undefined, Client :: rest_handler:client()) ->
    boolean().
is_authorized(_, _, _, #client{type = undefined}) ->
    false;
is_authorized(spaces, post, _SpaceId, _Client) ->
    true;
is_authorized(spaces, get, _SpaceId, #client{type = user, id = UserId}) ->
    user_logic:has_eff_oz_privilege(UserId, list_spaces);
is_authorized(space, patch, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_effective_privilege(SpaceId, UserId, space_change_data);
is_authorized(space, delete, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_effective_privilege(SpaceId, UserId, space_remove);
is_authorized(share, put, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_effective_privilege(SpaceId, UserId, space_manage_shares);
is_authorized(shares, get, SpaceId, #client{type = user, id = UserId}) ->
    % Share - to view shares, it's enough to belong to parent space
    space_logic:has_effective_user(SpaceId, UserId);
is_authorized(shares, get, SpaceId, #client{type = provider, id = ProviderId}) ->
    % Share - to view shares as provider, it's enough to support parent space
    space_logic:has_provider(SpaceId, ProviderId);
is_authorized(uinvite, get, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_effective_privilege(SpaceId, UserId, space_invite_user);
is_authorized(user, delete, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_effective_privilege(SpaceId, UserId, space_remove_user) orelse
        user_logic:has_eff_oz_privilege(UserId, remove_member_from_space);
is_authorized(users, post, _SpaceId, #client{type = user, id = UserId}) ->
    user_logic:has_eff_oz_privilege(UserId, add_member_to_space);
is_authorized(ginvite, get, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_effective_privilege(SpaceId, UserId, space_invite_group);
is_authorized(group, delete, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_effective_privilege(SpaceId, UserId, space_remove_group) orelse
        user_logic:has_eff_oz_privilege(UserId, remove_member_from_space);
is_authorized(groups, post, _SpaceId, #client{type = user, id = UserId}) ->
    user_logic:has_eff_oz_privilege(UserId, add_member_to_space);
is_authorized(pinvite, get, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_effective_privilege(SpaceId, UserId, space_add_provider);
is_authorized(provider, delete, SpaceId, #client{type = user, id = UserId}) ->
    space_logic:has_effective_privilege(SpaceId, UserId, space_remove_provider);
is_authorized(R, put, SpaceId, #client{type = user, id = UserId})
    when R =:= upriv; R =:= gpriv ->
    space_logic:has_effective_privilege(SpaceId, UserId, space_set_privileges);
is_authorized(space, get, SpaceId, #client{type = user, id = UserId}) ->
    % To get data about a space, it is enough to be its user, but without view
    % data privilege only space name is returned.
    case space_logic:has_effective_user(SpaceId, UserId) of
        false ->
            % If the user is not authorized by perms in space to view spaces,
            % check if he has oz_privileges to list spaces
            user_logic:has_eff_oz_privilege(
                UserId, list_spaces);
        true ->
            true
    end;
is_authorized(R, get, SpaceId, #client{type = user, id = UserId}) ->
    Result = space_logic:has_effective_privilege(
        SpaceId, UserId, space_view_data),
    case {R, Result} of
        {provider, false} ->
            % If the user is not authorized by perms in space to view a
            % provider, check if he has oz_privileges to list providers of space
            user_logic:has_eff_oz_privilege(
                UserId, list_providers_of_space);
        {providers, false} ->
            % If the user is not authorized by perms in space to view providers,
            % check if he has oz_privileges to list providers of space
            user_logic:has_eff_oz_privilege(
                UserId, list_providers_of_space);
        _ ->
            Result
    end;
is_authorized(R, get, SpaceId, #client{type = provider, id = ProviderId})
    when R =/= ginvite ->
    space_logic:has_provider(SpaceId, ProviderId);
is_authorized(_, _, _, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc Returns whether a resource exists.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(Resource :: resource(), SpaceId :: binary() | undefined,
    Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
resource_exists(spaces, _SpaceId, Req) ->
    {true, Req};
resource_exists(UserBound, SpaceId, Req) when UserBound =:= user; UserBound =:= upriv ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    case rest_handler:requests_effective_state(Req) of
        false -> {space_logic:has_user(SpaceId, UID), Req2};
        true -> {space_logic:has_effective_user(SpaceId, UID), Req2}
    end;
resource_exists(GroupBound, SpaceId, Req) when GroupBound =:= group; GroupBound =:= gpriv ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {space_logic:has_group(SpaceId, GID), Req2};
resource_exists(provider, SpaceId, Req) ->
    {PID, Req2} = cowboy_req:binding(pid, Req),
    {space_logic:has_provider(SpaceId, PID), Req2};
resource_exists(_, SpaceId, Req) ->
    {space_logic:exists(SpaceId), Req}.

%%--------------------------------------------------------------------
%% @doc Processes data submitted by a client through POST, PATCH, PUT on a REST
%% resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Resource :: accepted_resource(), Method :: accept_method(),
    SpaceId :: binary() | undefined, Data :: data(),
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {boolean() | {true, URL :: binary()}, cowboy_req:req()} | no_return().
accept_resource(spaces, post, _SpaceId, Data, #client{type = user, id = UserId}, Req) ->
    Name = rest_module_helper:assert_key(<<"name">>, Data, binary, Req),
    {ok, SpaceId} = space_logic:create({user, UserId}, Name),
    {{true, <<"/spaces/", SpaceId/binary>>}, Req};
accept_resource(share, put, SpaceId, Data, _Client, Req) ->
    {ShareId, _} = cowboy_req:binding(sid, Req),
    case share_logic:exists(ShareId) of
        true ->
            Description = <<"Share with given ID exists. Modify with PATCH.">>,
            rest_module_helper:report_error(invalid_request, Description, Req);
        false ->
            ok
    end,
    Name = rest_module_helper:assert_key(<<"name">>, Data, binary, Req),
    RootFileId = rest_module_helper:assert_key(<<"rootFileId">>, Data, binary, Req),
    {ok, ShareId} = share_logic:create(ShareId, Name, RootFileId, SpaceId),
    {true, Req};
accept_resource(spaces, post, _SpaceId, Data, #client{type = provider, id = ProviderId}, Req) ->
    Name = rest_module_helper:assert_key(<<"name">>, Data, binary, Req),
    Token = rest_module_helper:assert_key(<<"token">>, Data, binary, Req),
    Size = rest_module_helper:assert_key(<<"size">>, Data, pos_integer, Req),
    case token_logic:validate(Token, space_create_token) of
        false ->
            rest_module_helper:report_invalid_value(<<"token">>, Token, Req);
        {true, Macaroon} ->
            {ok, SpaceId} = space_logic:create({provider, ProviderId}, Name, Macaroon, Size),
            {{true, <<"/spaces/", SpaceId/binary>>}, Req}
    end;
accept_resource(space, patch, SpaceId, Data, #client{type = user, id = UserId}, Req) ->
    Name = rest_module_helper:assert_key(<<"name">>, Data, binary, Req),
    ok = space_logic:modify(SpaceId, {user, UserId}, Name),
    {true, Req};
accept_resource(space, patch, SpaceId, Data, #client{type = provider}, Req) ->
    Name = rest_module_helper:assert_key(<<"name">>, Data, binary, Req),
    ok = space_logic:modify(SpaceId, provider, Name),
    {true, Req};
accept_resource(users, post, SpaceId, Data, _Client, Req) ->
    UserId = rest_module_helper:assert_key(<<"userId">>, Data, binary, Req),
    case user_logic:exists(UserId) of
        false ->
            Description = <<"User with given ID does not exist">>,
            rest_module_helper:report_error(invalid_request, Description, Req);
        true ->
            {ok, SpaceId} = space_logic:add_user(SpaceId, UserId),
            {true, Req}
    end;
accept_resource(groups, post, SpaceId, Data, _Client, Req) ->
    GroupId = rest_module_helper:assert_key(<<"groupId">>, Data, binary, Req),
    case group_logic:exists(GroupId) of
        false ->
            Description = <<"Group with given ID does not exist">>,
            rest_module_helper:report_error(invalid_request, Description, Req);
        true ->
            {ok, SpaceId} = space_logic:add_group(SpaceId, GroupId),
            {true, Req}
    end;
accept_resource(upriv, put, SpaceId, Data, _Client, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),

    BinPrivileges = rest_module_helper:assert_key_value(<<"privileges">>,
        [atom_to_binary(P, latin1) || P <- privileges:space_privileges()], Data,
        list_of_bin, Req2),

    Privileges = [binary_to_existing_atom(P, latin1) || P <- BinPrivileges],
    ok = space_logic:set_privileges(SpaceId, {user, UID}, Privileges),
    {true, Req2};
accept_resource(gpriv, put, SpaceId, Data, _Client, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),

    BinPrivileges = rest_module_helper:assert_key_value(<<"privileges">>,
        [atom_to_binary(P, latin1) || P <- privileges:space_privileges()], Data,
        list_of_bin, Req2),

    Privileges = [binary_to_existing_atom(P, latin1) || P <- BinPrivileges],
    ok = space_logic:set_privileges(SpaceId, {group, GID}, Privileges),
    {true, Req2}.

%%--------------------------------------------------------------------
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Resource :: provided_resource(), SpaceId :: binary() | undefined,
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {Data :: json_object(), cowboy_req:req()}.
provide_resource(spaces, _EntityId, _Client, Req) ->
    {ok, SpaceIds} = space_logic:list(),
    {[{spaces, SpaceIds}], Req};
provide_resource(space, SpaceId, #client{type = user, id = UserId}, Req) ->
    % Check if the user belongs to given space. If yes, return the
    % space data as he sees it. If not, it means that he used his OZ API
    % privileges to access it (use 'provider' client for public space data).
    {ok, Data} = case space_logic:has_effective_user(SpaceId, UserId) of
        true ->
            HasViewPrivs = space_logic:has_effective_privilege(
                SpaceId, UserId, space_view_data),
            case HasViewPrivs of
                true ->
                    space_logic:get_data(SpaceId, {user, UserId});
                false ->
                    space_logic:get_public_data(SpaceId, {user, UserId})
            end;
        false ->
            space_logic:get_data(SpaceId, provider)
    end,
    {Data, Req};
provide_resource(space, SpaceId, #client{type = provider}, Req) ->
    {ok, Data} = space_logic:get_data(SpaceId, provider),
    {Data, Req};
provide_resource(shares, SpaceId, _Client, Req) ->
    {ok, Shares} = space_logic:get_shares(SpaceId),
    {Shares, Req};
provide_resource(users, SpaceId, #client{type = user}, Req) ->
    {ok, Users} =
        case rest_handler:requests_effective_state(Req) of
            false -> space_logic:get_users(SpaceId);
            true -> space_logic:get_effective_users(SpaceId)
        end,
    {Users, Req};
provide_resource(users, SpaceId, #client{type = provider}, Req) ->
    {ok, Users} = space_logic:get_effective_users(SpaceId),
    {Users, Req};
provide_resource(uinvite, SpaceId, Client, Req) ->
    {ok, Token} = token_logic:create(Client, space_invite_user_token, {space, SpaceId}),
    {[{token, Token}], Req};
provide_resource(user, SpaceId, #client{type = ClientType}, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    {ok, User} = space_logic:get_user(SpaceId, ClientType, UID),
    {User, Req2};
provide_resource(upriv, SpaceId, _Client, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    {ok, Privileges} =
        case rest_handler:requests_effective_state(Req) of
            false -> space_logic:get_privileges(SpaceId, {user, UID});
            true -> space_logic:get_effective_privileges(SpaceId, UID)
        end,
    {[{privileges, Privileges}], Req2};
provide_resource(groups, SpaceId, _Client, Req) ->
    {ok, Groups} = space_logic:get_groups(SpaceId),
    {Groups, Req};
provide_resource(ginvite, SpaceId, Client, Req) ->
    {ok, Token} = token_logic:create(Client, space_invite_group_token, {space, SpaceId}),
    {[{token, Token}], Req};
provide_resource(group, SpaceId, _Client, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {ok, Group} = space_logic:get_group(SpaceId, GID),
    {Group, Req2};
provide_resource(gpriv, SpaceId, _Client, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {ok, Privileges} = space_logic:get_privileges(SpaceId, {group, GID}),
    {[{privileges, Privileges}], Req2};
provide_resource(pinvite, SpaceId, Client, Req) ->
    {ok, Token} = token_logic:create(Client, space_support_token, {space, SpaceId}),
    {[{token, Token}], Req};
provide_resource(providers, SpaceId, #client{type = ClientType}, Req) ->
    {ok, Providers} = space_logic:get_providers(SpaceId, ClientType),
    {Providers, Req};
provide_resource(provider, SpaceId, #client{type = ClientType}, Req) ->
    {PID, Req2} = cowboy_req:binding(pid, Req),
    {ok, Provider} = space_logic:get_provider(SpaceId, ClientType, PID),
    {Provider, Req2}.

%%--------------------------------------------------------------------
%% @doc Deletes the resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Resource :: removable_resource(),
    SpaceId :: binary() | undefined, Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
delete_resource(space, SpaceId, Req) ->
    {space_logic:remove(SpaceId), Req};
delete_resource(user, SpaceId, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    {space_logic:remove_user(SpaceId, UID), Req2};
delete_resource(group, SpaceId, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {space_logic:remove_group(SpaceId, GID), Req2};
delete_resource(provider, SpaceId, Req) ->
    {PID, Req2} = cowboy_req:binding(pid, Req),
    {space_logic:remove_provider(SpaceId, PID), Req2}.
