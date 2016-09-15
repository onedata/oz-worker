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

-type provided_resource() :: handles | handle | users | upriv | groups | gpriv .
-type accepted_resource() :: handles | handle | user | upriv | group | gpriv.
-type removable_resource() :: handle | user | group.
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
    HandleId :: binary() | undefined, Client :: rest_handler:client()) ->
    boolean().
is_authorized(_, _, _, #client{type = undefined}) ->
    false;
is_authorized(handles, post, _HandleId, _Client) -> % todo what privilege should be checked here?
    true;
is_authorized(handles, get, _HandleId, #client{type = user, id = UserId}) ->
    true;
is_authorized(handle, get, HandleId, #client{type = user, id = UserId}) ->
    handle_logic:has_effective_privilege(HandleId, UserId, view_handle);
is_authorized(handle, patch, HandleId, #client{type = user, id = UserId}) ->
    handle_logic:has_effective_privilege(HandleId, UserId, modify_handle);
is_authorized(handle, delete, HandleId, #client{type = user, id = UserId}) ->
    handle_logic:has_effective_privilege(HandleId, UserId, delete_handle);
is_authorized(users, get, HandleId, #client{type = user, id = UserId}) ->
    handle_logic:has_effective_privilege(HandleId, UserId, view_handle);
is_authorized(user, put, HandleId, #client{type = user, id = UserId}) ->
    handle_logic:has_effective_privilege(HandleId, UserId, modify_handle);
is_authorized(user, delete, HandleId, #client{type = user, id = UserId}) ->
    handle_logic:has_effective_privilege(HandleId, UserId, modify_handle);
is_authorized(groups, get, HandleId, #client{type = user, id = UserId}) ->
    handle_logic:has_effective_privilege(HandleId, UserId, view_handle);
is_authorized(group, put, HandleId, #client{type = user, id = UserId}) ->
    handle_logic:has_effective_privilege(HandleId, UserId, modify_handle);
is_authorized(group, delete, HandleId, #client{type = user, id = UserId}) ->
    handle_logic:has_effective_privilege(HandleId, UserId, modify_handle);
is_authorized(upriv, get, HandleId, #client{type = user, id = UserId}) ->
    handle_logic:has_effective_privilege(HandleId, UserId, view_handle);
is_authorized(upriv, put, HandleId, #client{type = user, id = UserId}) ->
    handle_logic:has_effective_privilege(HandleId, UserId, modify_handle);
is_authorized(gpriv, get, HandleId, #client{type = user, id = UserId}) ->
    handle_logic:has_effective_privilege(HandleId, UserId, view_handle);
is_authorized(gpriv, put, HandleId, #client{type = user, id = UserId}) ->
    handle_logic:has_effective_privilege(HandleId, UserId, modify_handle);
is_authorized(_, _, _, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc Returns whether a resource exists.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(Resource :: resource(), HandleId :: binary() | undefined,
    Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
resource_exists(handles, _HandleId, Req) ->
    {true, Req};
resource_exists(handle, HandleId, Req) ->
    {handle_logic:exists(HandleId), Req};
resource_exists(users, HandleId, Req) ->
    {handle_logic:exists(HandleId), Req};
resource_exists(groups, HandleId, Req) ->
    {handle_logic:exists(HandleId), Req};
resource_exists(user, HandleId, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    {handle_logic:has_user(HandleId, UID), Req2};
resource_exists(upriv, HandleId, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    {handle_logic:has_user(HandleId, UID), Req2};
resource_exists(group, HandleId, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {handle_logic:has_group(HandleId, GID), Req2};
resource_exists(gpriv, HandleId, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {handle_logic:has_group(HandleId, GID), Req2}.

%%--------------------------------------------------------------------
%% @doc Processes data submitted by a client through POST, PATCH, PUT on a REST
%% resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Resource :: accepted_resource(), Method :: accept_method(),
    HandleId :: binary() | undefined, Data :: data(),
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {boolean() | {true, URL :: binary()}, cowboy_req:req()} | no_return().
accept_resource(handles, post, _HandleId, Data, #client{type = user, id = UserId}, Req) ->
    HandleServiceId = rest_module_helper:assert_key(<<"handleServiceId">>, Data, binary, Req),
    ResourceType = rest_module_helper:assert_key(<<"resourceType">>, Data, binary, Req),
    ResourceId = rest_module_helper:assert_key(<<"resourceId">>, Data, binary, Req),
    {ok, HandleLocation} = handle_proxy:register_handle(HandleServiceId, ResourceType, ResourceId),
    {ok, HandleId} = handle_logic:create(UserId, HandleServiceId, ResourceType, ResourceId, HandleLocation),
    {{true, <<"/handles/", HandleId/binary>>}, Req};

accept_resource(handle, patch, HandleId, Data, #client{type = user, id = _UserId}, Req) ->
    HandleServiceId = rest_module_helper:assert_type(<<"handleServiceId">>, Data, binary, Req),
    ResourceType = rest_module_helper:assert_type(<<"resourceType">>, Data, binary, Req),
    ResourceId = rest_module_helper:assert_type(<<"resourceId">>, Data, binary, Req),
    ok = handle_proxy:modify_handle(HandleId, ResourceType, ResourceId),
    ok = handle_logic:modify(HandleId, HandleServiceId, ResourceType, ResourceId),
    {true, Req};

accept_resource(user, put, HandleId, _Data, _Client, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    case user_logic:exists(UID) of
        false ->
            Description = <<"User with given ID does not exist">>,
            rest_module_helper:report_error(invalid_request, Description, Req2);
        true ->
            {ok, HandleId} = handle_logic:add_user(HandleId, UID),
            {ok, Req3} = cowboy_req:reply(204, Req2),
            {true, Req3}
    end;
accept_resource(group, put, HandleId, _Data, _Client, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    case group_logic:exists(GID) of
        false ->
            Description = <<"Group with given ID does not exist">>,
            rest_module_helper:report_error(invalid_request, Description, Req2);
        true ->
            {ok, HandleId} = handle_logic:add_group(HandleId, GID),
            {ok, Req3} = cowboy_req:reply(204, Req2),
            {true, Req3}
    end;
accept_resource(upriv, put, HandleId, Data, _Client, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),

    BinPrivileges = rest_module_helper:assert_key_value(<<"privileges">>,
        [atom_to_binary(P, latin1) || P <- privileges:handle_privileges()], Data,
        list_of_bin, Req2),

    Privileges = [binary_to_existing_atom(P, latin1) || P <- BinPrivileges],
    ok = handle_logic:set_user_privileges(HandleId, UID, Privileges),
    {ok, Req3} = cowboy_req:reply(204, Req2),
    {true, Req3};
accept_resource(gpriv, put, HandleId, Data, _Client, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),

    BinPrivileges = rest_module_helper:assert_key_value(<<"privileges">>,
        [atom_to_binary(P, latin1) || P <- privileges:handle_privileges()], Data,
        list_of_bin, Req2),

    Privileges = [binary_to_existing_atom(P, latin1) || P <- BinPrivileges],
    ok = handle_logic:set_group_privileges(HandleId, GID, Privileges),
    {ok, Req3} = cowboy_req:reply(204, Req2),
    {true, Req3}.

%%--------------------------------------------------------------------
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Resource :: provided_resource(), HandleId :: binary() | undefined,
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {Data :: json_object(), cowboy_req:req()}.
provide_resource(handles, _EntityId, #client{type = user, id = UserId}, Req) ->
    {ok, HandleIds} = handle_logic:list(UserId),
    {HandleIds, Req};
provide_resource(handle, HandleId, #client{type = user, id = _UserId}, Req) ->
    {ok, Data} = handle_logic:get_data(HandleId),
    {Data, Req};
provide_resource(users, HandleId, _Client, Req) ->
    {ok, Users} = handle_logic:get_users(HandleId),
    {Users, Req};
provide_resource(upriv, HandleId, _Client, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    {ok, Privileges} = handle_logic:get_user_privileges(HandleId, UID),
    {Privileges, Req2};
provide_resource(groups, HandleId, _Client, Req) ->
    {ok, Groups} = handle_logic:get_groups(HandleId),
    {Groups, Req};
provide_resource(gpriv, HandleId, _Client, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {ok, Privileges} = handle_logic:get_group_privileges(HandleId, GID),
    {Privileges, Req2}.

%%--------------------------------------------------------------------
%% @doc Deletes the resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Resource :: removable_resource(),
    HandleId :: binary() | undefined, Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
delete_resource(handle, HandleId, Req) ->
    ok = handle_proxy:unregister_handle(HandleId),
    {handle_logic:remove(HandleId), Req};
delete_resource(user, HandleId, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    {handle_logic:remove_user(HandleId, UID), Req2};
delete_resource(group, HandleId, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {handle_logic:remove_group(HandleId, GID), Req2}.