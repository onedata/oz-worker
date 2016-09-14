%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /handle_services REST resources.
%%%-------------------------------------------------------------------
-module(handle_services_rest_module).
-author("Tomasz Lichon").

-include("http/handlers/rest_handler.hrl").

-behavior(rest_module_behavior).

-type provided_resource() :: handle_services | handle_service | users | upriv | groups | gpriv .
-type accepted_resource() :: handle_services | handle_service | user | upriv | group | gpriv.
-type removable_resource() :: handle_service | user | group.
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
    S = #rstate{module = ?MODULE, root = handle_services},
    M = rest_handler,
    [
        {<<"/handle_services">>, M, S#rstate{resource = handle_services, methods = [post, get]}},
        {<<"/handle_services/:id">>, M, S#rstate{resource = handle_service, methods = [get, patch, delete]}},
        {<<"/handle_services/:id/users">>, M, S#rstate{resource = users, methods = [get]}},
        {<<"/handle_services/:id/users/:uid">>, M, S#rstate{resource = user, methods = [put, delete]}},
        {<<"/handle_services/:id/users/:uid/privileges">>, M, S#rstate{resource = upriv, methods = [get, put]}},
        {<<"/handle_services/:id/groups">>, M, S#rstate{resource = groups, methods = [get]}},
        {<<"/handle_services/:id/groups/:gid">>, M, S#rstate{resource = group, methods = [put, delete]}},
        {<<"/handle_services/:id/groups/:gid/privileges">>, M, S#rstate{resource = gpriv, methods = [get, put]}}
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
is_authorized(handle_services, post, _HandleServiceId, _Client) -> % todo what privilege should be checked here?
    true;
is_authorized(handle_services, get, _HandleServiceId, #client{type = user}) ->
    true;
is_authorized(handle_service, get, HandleServiceId, #client{type = user, id = UserId}) ->
    handle_service_logic:has_effective_privilege(HandleServiceId, UserId, view_handle_service);
is_authorized(handle_service, patch, HandleServiceId, #client{type = user, id = UserId}) ->
    handle_service_logic:has_effective_privilege(HandleServiceId, UserId, modify_handle_service);
is_authorized(handle_service, delete, HandleServiceId, #client{type = user, id = UserId}) ->
    handle_service_logic:has_effective_privilege(HandleServiceId, UserId, delete_handle_service);
is_authorized(users, get, HandleServiceId, #client{type = user, id = UserId}) ->
    handle_service_logic:has_effective_privilege(HandleServiceId, UserId, view_handle_service);
is_authorized(user, put, HandleServiceId, #client{type = user, id = UserId}) ->
    handle_service_logic:has_effective_privilege(HandleServiceId, UserId, modify_handle_service);
is_authorized(user, delete, HandleServiceId, #client{type = user, id = UserId}) ->
    handle_service_logic:has_effective_privilege(HandleServiceId, UserId, modify_handle_service);
is_authorized(groups, get, HandleServiceId, #client{type = user, id = UserId}) ->
    handle_service_logic:has_effective_privilege(HandleServiceId, UserId, view_handle_service);
is_authorized(group, put, HandleServiceId, #client{type = user, id = UserId}) ->
    handle_service_logic:has_effective_privilege(HandleServiceId, UserId, modify_handle_service);
is_authorized(group, delete, HandleServiceId, #client{type = user, id = UserId}) ->
    handle_service_logic:has_effective_privilege(HandleServiceId, UserId, modify_handle_service);
is_authorized(upriv, get, HandleServiceId, #client{type = user, id = UserId}) ->
    handle_service_logic:has_effective_privilege(HandleServiceId, UserId, view_handle_service);
is_authorized(upriv, put, HandleServiceId, #client{type = user, id = UserId}) ->
    handle_service_logic:has_effective_privilege(HandleServiceId, UserId, modify_handle_service);
is_authorized(gpriv, get, HandleServiceId, #client{type = user, id = UserId}) ->
    handle_service_logic:has_effective_privilege(HandleServiceId, UserId, view_handle_service);
is_authorized(gpriv, put, HandleServiceId, #client{type = user, id = UserId}) ->
    handle_service_logic:has_effective_privilege(HandleServiceId, UserId, modify_handle_service);
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
resource_exists(handle_services, _HandleServiceId, Req) ->
    {true, Req};
resource_exists(handle_service, HandleServiceId, Req) ->
    {handle_service_logic:exists(HandleServiceId), Req};
resource_exists(users, HandleServiceId, Req) ->
    {handle_service_logic:exists(HandleServiceId), Req};
resource_exists(groups, HandleServiceId, Req) ->
    {handle_service_logic:exists(HandleServiceId), Req};
resource_exists(user, HandleServiceId, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    {handle_service_logic:has_user(HandleServiceId, UID), Req2};
resource_exists(upriv, HandleServiceId, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    {handle_service_logic:has_user(HandleServiceId, UID), Req2};
resource_exists(group, HandleServiceId, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {handle_service_logic:has_group(HandleServiceId, GID), Req2};
resource_exists(gpriv, HandleServiceId, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {handle_service_logic:has_group(HandleServiceId, GID), Req2}.

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
accept_resource(handle_services, post, _HandleServiceId, Data, #client{type = user, id = UserId}, Req) ->
    Name = rest_module_helper:assert_key(<<"name">>, Data, binary, Req),
    ProxyEndpoint = rest_module_helper:assert_key(<<"proxyEndpoint">>, Data, binary, Req),
    ServiceProperties = rest_module_helper:assert_key(<<"serviceProperties">>, Data, any, Req),
    {ok, HandleServiceId} = handle_service_logic:create(UserId, Name, ProxyEndpoint, ServiceProperties),
    {{true, <<"/handle_services/", HandleServiceId/binary>>}, Req};

accept_resource(handle_service, patch, HandleServiceId, Data, #client{type = user, id = _UserId}, Req) ->
    Name = rest_module_helper:assert_type(<<"name">>, Data, binary, Req),
    ProxyEndpoint = rest_module_helper:assert_type(<<"proxyEndpoint">>, Data, binary, Req),
    ServiceProperties = rest_module_helper:assert_type(<<"serviceProperties">>, Data, any, Req),
    ok = handle_service_logic:modify(HandleServiceId, Name, ProxyEndpoint, ServiceProperties),
    {ok, Req2} = cowboy_req:reply(204, Req),
    {true, Req2};

accept_resource(user, put, HandleServiceId, _Data, _Client, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    case user_logic:exists(UID) of
        false ->
            Description = <<"User with given ID does not exist">>,
            rest_module_helper:report_error(invalid_request, Description, Req2);
        true ->
            {ok, HandleServiceId} = handle_service_logic:add_user(HandleServiceId, UID),
            {ok, Req3} = cowboy_req:reply(204, Req2),
            {true, Req3}
    end;
accept_resource(group, put, HandleServiceId, _Data, _Client, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    case group_logic:exists(GID) of
        false ->
            Description = <<"Group with given ID does not exist">>,
            rest_module_helper:report_error(invalid_request, Description, Req2);
        true ->
            {ok, HandleServiceId} = handle_service_logic:add_group(HandleServiceId, GID),
            {ok, Req3} = cowboy_req:reply(204, Req2),
            {true, Req3}
    end;
accept_resource(upriv, put, HandleServiceId, Data, _Client, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),

    BinPrivileges = rest_module_helper:assert_key_value(<<"privileges">>,
        [atom_to_binary(P, latin1) || P <- privileges:handle_service_privileges()], Data,
        list_of_bin, Req2),

    Privileges = [binary_to_existing_atom(P, latin1) || P <- BinPrivileges],
    ok = handle_service_logic:set_user_privileges(HandleServiceId, UID, Privileges),
    {ok, Req3} = cowboy_req:reply(204, Req2),
    {true, Req3};
accept_resource(gpriv, put, HandleServiceId, Data, _Client, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),

    BinPrivileges = rest_module_helper:assert_key_value(<<"privileges">>,
        [atom_to_binary(P, latin1) || P <- privileges:handle_service_privileges()], Data,
        list_of_bin, Req2),

    Privileges = [binary_to_existing_atom(P, latin1) || P <- BinPrivileges],
    ok = handle_service_logic:set_group_privileges(HandleServiceId, GID, Privileges),
    {ok, Req3} = cowboy_req:reply(204, Req2),
    {true, Req3}.

%%--------------------------------------------------------------------
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Resource :: provided_resource(), HandleServiceId :: binary() | undefined,
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {Data :: json_object(), cowboy_req:req()}.
provide_resource(handle_services, _EntityId, #client{type = user, id = UserId}, Req) ->
    {ok, HandleServiceIds} = handle_service_logic:list(UserId),
    {HandleServiceIds, Req};
provide_resource(handle_service, HandleServiceId, #client{type = user, id = _UserId}, Req) ->
    {ok, Data} = handle_service_logic:get_data(HandleServiceId),
    {Data, Req};
provide_resource(users, HandleServiceId, _Client, Req) ->
    {ok, Users} = handle_service_logic:get_users(HandleServiceId),
    {Users, Req};
provide_resource(upriv, HandleServiceId, _Client, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    {ok, Privileges} = handle_service_logic:get_user_privileges(HandleServiceId, UID),
    {Privileges, Req2};
provide_resource(groups, HandleServiceId, _Client, Req) ->
    {ok, Groups} = handle_service_logic:get_groups(HandleServiceId),
    {Groups, Req};
provide_resource(gpriv, HandleServiceId, _Client, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {ok, Privileges} = handle_service_logic:get_group_privileges(HandleServiceId, GID),
    {Privileges, Req2}.

%%--------------------------------------------------------------------
%% @doc Deletes the resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Resource :: removable_resource(),
    HandleServiceId :: binary() | undefined, Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
delete_resource(handle_service, HandleServiceId, Req) ->
    {handle_service_logic:remove(HandleServiceId), Req};
delete_resource(user, HandleServiceId, Req) ->
    {UID, Req2} = cowboy_req:binding(uid, Req),
    {handle_service_logic:remove_user(HandleServiceId, UID), Req2};
delete_resource(group, HandleServiceId, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {handle_service_logic:remove_group(HandleServiceId, GID), Req2}.
