%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /handles REST resources.
%%%-------------------------------------------------------------------
-module(handles_rest_module).
-author("Tomasz Lichon").

-include("http/handlers/rest_handler.hrl").

-behavior(rest_module_behavior).

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
    S = #rstate{module = ?MODULE, root = handles},
    M = rest_handler,
    [
        {<<"/handles">>, M, S#rstate{resource = handles, methods = [post, get]}},
        {<<"/handles/:id">>, M, S#rstate{resource = handle, methods = [get, patch, delete]}},
        {<<"/handles/:id/users">>, M, S#rstate{resource = users, methods = [get]}},
        {<<"/handles/:id/users/:uid">>, M, S#rstate{resource = user, methods = [put, delete]}},
        {<<"/handles/:id/users/:uid/privileges">>, M, S#rstate{resource = upriv, methods = [get, put]}},
        {<<"/handles/:id/groups">>, M, S#rstate{resource = groups, methods = [get]}},
        {<<"/handles/:id/groups/:gid">>, M, S#rstate{resource = group, methods = [put, delete]}},
        {<<"/handles/:id/groups/:gid/privileges">>, M, S#rstate{resource = gpriv, methods = [get, put]}}
    ].

%%--------------------------------------------------------------------
%% @doc Returns a boolean() determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Resource :: resource(), Method :: method(),
    HandleServiceId :: binary() | undefined, Client :: rest_handler:client()) ->
    boolean().
is_authorized(_, _, _, #client{type = undefined}) ->
    false;
is_authorized(handles, post, _HandleServiceId, _Client) -> % todo what privilege should be checked here?
    true;
is_authorized(handles, get, HandleServiceId, #client{type = user, id = UserId}) ->
    handles_logic:has_effective_privilege(HandleServiceId, UserId, view_handle);
is_authorized(handle, get, HandleServiceId, #client{type = user, id = UserId}) ->
    handles_logic:has_effective_privilege(HandleServiceId, UserId, view_handle);
is_authorized(handle, patch, HandleServiceId, #client{type = user, id = UserId}) ->
    handles_logic:has_effective_privilege(HandleServiceId, UserId, modify_handle);
is_authorized(handle, delete, HandleServiceId, #client{type = user, id = UserId}) ->
    handles_logic:has_effective_privilege(HandleServiceId, UserId, delete_handle);
is_authorized(users, get, HandleServiceId, #client{type = user, id = UserId}) ->
    handles_logic:has_effective_privilege(HandleServiceId, UserId, view_handle);
is_authorized(user, put, HandleServiceId, #client{type = user, id = UserId}) ->
    handles_logic:has_effective_privilege(HandleServiceId, UserId, modify_handle);
is_authorized(user, delete, HandleServiceId, #client{type = user, id = UserId}) ->
    handles_logic:has_effective_privilege(HandleServiceId, UserId, modify_handle);
is_authorized(groups, get, HandleServiceId, #client{type = user, id = UserId}) ->
    handles_logic:has_effective_privilege(HandleServiceId, UserId, view_handle);
is_authorized(group, put, HandleServiceId, #client{type = user, id = UserId}) ->
    handles_logic:has_effective_privilege(HandleServiceId, UserId, modify_handle);
is_authorized(group, delete, HandleServiceId, #client{type = user, id = UserId}) ->
    handles_logic:has_effective_privilege(HandleServiceId, UserId, modify_handle);
is_authorized(upriv, get, HandleServiceId, #client{type = user, id = UserId}) ->
    handles_logic:has_effective_privilege(HandleServiceId, UserId, view_handle);
is_authorized(upriv, put, HandleServiceId, #client{type = user, id = UserId}) ->
    handles_logic:has_effective_privilege(HandleServiceId, UserId, modify_handle);
is_authorized(gpriv, get, HandleServiceId, #client{type = user, id = UserId}) ->
    handles_logic:has_effective_privilege(HandleServiceId, UserId, view_handle);
is_authorized(gpriv, put, HandleServiceId, #client{type = user, id = UserId}) ->
    handles_logic:has_effective_privilege(HandleServiceId, UserId, modify_handle);
is_authorized(_, _, _, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc Returns whether a resource exists.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(Resource :: resource(), HandleServiceId :: binary() | undefined,
    Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
resource_exists(handles, _HandleServiceId, Req) ->
    {true, Req};
resource_exists(handle, HandleServiceId, Req) ->
    {handles_logic:exists(HandleServiceId), Req};
resource_exists(users, HandleServiceId, Req) ->
    {handles_logic:exists(HandleServiceId), Req};
resource_exists(groups, HandleServiceId, Req) ->
    {handles_logic:exists(HandleServiceId), Req};
resource_exists(user, HandleServiceId, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    {handles_logic:has_user(HandleServiceId, UID), Req2};
resource_exists(upriv, HandleServiceId, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    {handles_logic:has_user(HandleServiceId, UID), Req2};
resource_exists(group, HandleServiceId, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {handles_logic:has_group(HandleServiceId, GID), Req2};
resource_exists(gpriv, HandleServiceId, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {handles_logic:has_group(HandleServiceId, GID), Req2}.

%%--------------------------------------------------------------------
%% @doc Processes data submitted by a client through POST, PATCH, PUT on a REST
%% resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Resource :: accepted_resource(), Method :: accept_method(),
    HandleServiceId :: binary() | undefined, Data :: data(),
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {boolean() | {true, URL :: binary()}, cowboy_req:req()} | no_return().
accept_resource(handles, post, _HandleServiceId, Data, #client{type = user, id = UserId}, Req) ->
    HandleServiceId = rest_module_helper:assert_key(<<"handleServiceId">>, Data, binary, Req),
    ResourceType = rest_module_helper:assert_key(<<"resourceType">>, Data, binary, Req),
    ResourceId = rest_module_helper:assert_key(<<"resourceId">>, Data, binary, Req),
    {ok, HandleServiceId} = handles_logic:create(UserId, HandleServiceId, ResourceType, ResourceId),
    {{true, <<"/handles/", HandleServiceId/binary>>}, Req};

accept_resource(handle, patch, HandleServiceId, Data, #client{type = user, id = UserId}, Req) ->
    ok = handles_logic:modify(HandleServiceId, UserId, Data),
    {true, Req};

accept_resource(user, put, HandleServiceId, _Data, _Client, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    case user_logic:exists(UID) of
        false ->
            Description = <<"User with given ID does not exist">>,
            rest_module_helper:report_error(invalid_request, Description, Req2);
        true ->
            {ok, HandleServiceId} = handles_logic:add_user(HandleServiceId, UID),
            {true, Req2}
    end;
accept_resource(group, put, HandleServiceId, _Data, _Client, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    case group_logic:exists(GID) of
        false ->
            Description = <<"Group with given ID does not exist">>,
            rest_module_helper:report_error(invalid_request, Description, Req2);
        true ->
            {ok, HandleServiceId} = handles_logic:add_group(HandleServiceId, GID),
            {true, Req2}
    end;
accept_resource(upriv, put, HandleServiceId, Data, _Client, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),

    BinPrivileges = rest_module_helper:assert_key_value(<<"privileges">>,
        [atom_to_binary(P, latin1) || P <- privileges:handle_privileges()], Data,
        list_of_bin, Req2),

    Privileges = [binary_to_existing_atom(P, latin1) || P <- BinPrivileges],
    ok = handles_logic:set_user_privileges(HandleServiceId, UID, Privileges),
    {true, Req2};
accept_resource(gpriv, put, HandleServiceId, Data, _Client, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),

    BinPrivileges = rest_module_helper:assert_key_value(<<"privileges">>,
        [atom_to_binary(P, latin1) || P <- privileges:handle_privileges()], Data,
        list_of_bin, Req2),

    Privileges = [binary_to_existing_atom(P, latin1) || P <- BinPrivileges],
    ok = handles_logic:set_group_privileges(HandleServiceId, GID, Privileges),
    {true, Req2}.

%%--------------------------------------------------------------------
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Resource :: provided_resource(), HandleServiceId :: binary() | undefined,
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {Data :: json_object(), cowboy_req:req()}.
provide_resource(handles, _EntityId, #client{type = user, id = UserId}, Req) ->
    {ok, HandleServiceIds} = handles_logic:list(UserId),
    {[{handles, HandleServiceIds}], Req};
provide_resource(handle, HandleServiceId, #client{type = user, id = UserId}, Req) ->
    {ok, Data} = handles_logic:get_data(HandleServiceId, UserId),
    {Data, Req};
provide_resource(users, HandleServiceId, _Client, Req) ->
    {ok, Users} = handles_logic:get_effective_users(HandleServiceId),
    {Users, Req};
provide_resource(user, HandleServiceId, _Client, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    {ok, User} = handles_logic:get_user(HandleServiceId, UID),
    {User, Req2};
provide_resource(upriv, HandleServiceId, _Client, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    {ok, Privileges} = handles_logic:get_user_privileges(HandleServiceId, UID),
    {[{privileges, Privileges}], Req2};
provide_resource(groups, HandleServiceId, _Client, Req) ->
    {ok, Groups} = handles_logic:get_groups(HandleServiceId),
    {Groups, Req};
provide_resource(group, HandleServiceId, _Client, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {ok, Group} = handles_logic:get_group(HandleServiceId, GID),
    {Group, Req2};
provide_resource(gpriv, HandleServiceId, _Client, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {ok, Privileges} = handles_logic:get_group_privileges(HandleServiceId, GID),
    {[{privileges, Privileges}], Req2}.

%%--------------------------------------------------------------------
%% @doc Deletes the resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Resource :: removable_resource(),
    HandleServiceId :: binary() | undefined, Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
delete_resource(handle, HandleServiceId, Req) ->
    {handles_logic:remove(HandleServiceId), Req};
delete_resource(user, HandleServiceId, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    {handles_logic:remove_user(HandleServiceId, UID), Req2};
delete_resource(group, HandleServiceId, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {handles_logic:remove_group(HandleServiceId, GID), Req2}.
