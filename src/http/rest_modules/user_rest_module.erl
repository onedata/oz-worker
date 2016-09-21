%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /user REST resources.
%%%-------------------------------------------------------------------
-module(user_rest_module).
-author("Konrad Zemek").

-include("datastore/oz_datastore_models_def.hrl").
-include("http/handlers/rest_handler.hrl").

-include_lib("ctool/include/logging.hrl").

-behavior(rest_module_behavior).


-type provided_resource() :: user | client_token | spaces | defspace | screate | space | groups | group | effective_groups.
-type accepted_resource() :: user | auth | spaces | defspace | sjoin | groups | gjoin.
-type removable_resource() :: user | space | group.
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
    S = #rstate{module = ?MODULE, root = user},
    M = rest_handler,
    [
        {<<"/user">>, M, S#rstate{resource = user, methods = [get, patch, delete]}},
        {<<"/user/authorize">>, M, S#rstate{resource = auth, methods = [post], noauth = [post]}},
        {<<"/user/client_token">>, M, S#rstate{resource = client_token, methods = [get]}},
        {<<"/user/spaces">>, M, S#rstate{resource = spaces, methods = [get, post]}},
        {<<"/user/spaces/default">>, M, S#rstate{resource = defspace, methods = [get, put]}},
        {<<"/user/spaces/join">>, M, S#rstate{resource = sjoin, methods = [post]}},
        {<<"/user/spaces/token">>, M, S#rstate{resource = screate, methods = [get]}},
        {<<"/user/spaces/:sid">>, M, S#rstate{resource = space, methods = [get, delete]}},
        {<<"/user/effective_groups">>, M, S#rstate{resource = effective_groups, methods = [get]}},
        {<<"/user/groups">>, M, S#rstate{resource = groups, methods = [get, post]}},
        {<<"/user/groups/join">>, M, S#rstate{resource = gjoin, methods = [post]}},
        {<<"/user/groups/:gid">>, M, S#rstate{resource = group, methods = [get, delete]}}
    ].

%%--------------------------------------------------------------------
%% @doc Returns a boolean() determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Resource :: resource(), Method :: method(),
    UserId :: binary() | undefined, Client :: rest_handler:client()) ->
    boolean().
is_authorized(auth, post, _, _) ->
    true;
is_authorized(_, _, _, #client{type = user}) ->
    true;
is_authorized(_, _, _, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc Returns whether a resource exists.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(Resource :: resource(), UserId :: binary() | undefined,
    Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
resource_exists(space, UserId, Req) ->
    {SID, Req2} = cowboy_req:binding(sid, Req),
    {space_logic:has_effective_user(SID, UserId), Req2};
resource_exists(group, UserId, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {group_logic:has_effective_user(GID, UserId), Req2};
resource_exists(_, _, Req) ->
    {true, Req}.

%%--------------------------------------------------------------------
%% @doc Processes data submitted by a client through POST, PATCH, PUT on a REST
%% resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Resource :: accepted_resource(), Method :: accept_method(),
    UserId :: binary() | undefined, Data :: data(),
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {boolean() | {true, URL :: binary()}, cowboy_req:req()} | no_return().
accept_resource(user, patch, UserId, Data, _Client, Req) ->
    rest_module_helper:assert_type(<<"name">>, Data, binary, Req),
    rest_module_helper:assert_type(<<"alias">>, Data, binary, Req),
    % Convert proplist keys to atoms as user_logic expects them as atoms.
    DataAtoms = [{binary_to_existing_atom(K, utf8), V} || {K, V} <- Data],
    ok = user_logic:modify(UserId, DataAtoms),
    {true, Req};
accept_resource(auth, post, _User, Data, _Client, Req) ->
    Identifier = rest_module_helper:assert_key(<<"identifier">>, Data, binary, Req),
    case auth_logic:authenticate_user(Identifier) of
        {ok, DischargeMacaroonToken} ->
            {ok, Req2} = cowboy_req:reply(200, [], DischargeMacaroonToken, Req),
            {true, Req2};
        _ ->
            {false, Req}
    end;
accept_resource(spaces, post, _UserId, Data, Client, Req) ->
    spaces_rest_module:accept_resource(spaces, post, undefined, Data, Client, Req);
accept_resource(defspace, put, UserId, Data, _Client, Req) ->
    SpaceId = rest_module_helper:assert_key(<<"spaceId">>, Data, binary, Req),
    Result = user_logic:set_default_space(UserId, SpaceId),
    {Result, Req};
accept_resource(sjoin, post, UserId, Data, _Client, Req) ->
    Token = rest_module_helper:assert_key(<<"token">>, Data, binary, Req),
    case token_logic:validate(Token, space_invite_user_token) of
        false ->
            rest_module_helper:report_invalid_value(<<"token">>, Token, Req);
        {true, Macaroon} ->
            {ok, SpaceId} = space_logic:join({user, UserId}, Macaroon),
            {{true, <<"/user/spaces/", SpaceId/binary>>}, Req}
    end;
accept_resource(groups, post, _UserId, Data, Client, Req) ->
    groups_rest_module:accept_resource(groups, post, undefined, Data, Client, Req);
accept_resource(gjoin, post, UserId, Data, _Client, Req) ->
    Token = rest_module_helper:assert_key(<<"token">>, Data, binary, Req),
    case token_logic:validate(Token, group_invite_token) of
        false ->
            rest_module_helper:report_invalid_value(<<"token">>, Token, Req);
        {true, Macaroon} ->
            {ok, GroupId} = group_logic:join(UserId, Macaroon),
            {{true, <<"/user/groups/", GroupId/binary>>}, Req}
    end.

%%--------------------------------------------------------------------
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Resource :: provided_resource(), UserId :: binary() | undefined,
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {Data :: json_object(), cowboy_req:req()}.
provide_resource(user, UserId, #client{type = Type}, Req) ->
    {ok, User} = user_logic:get_data(UserId, Type),
    {User, Req};
provide_resource(spaces, UserId, _Client, Req) ->
    {ok, Spaces} = user_logic:get_spaces(UserId),
    {Spaces, Req};
provide_resource(defspace, UserId, _Client, Req) ->
    {ok, DefaultSpaceId} = user_logic:get_default_space(UserId),
    {[{spaceId, DefaultSpaceId}], Req};
provide_resource(screate, UserId, Client, Req) ->
    {ok, Token} = token_logic:create(Client, space_create_token, {user, UserId}),
    {[{token, Token}], Req};
provide_resource(space, UserId, _Client, Req) ->
    {SID, Req2} = cowboy_req:binding(sid, Req),
    {ok, Space} = space_logic:get_data(SID, {user, UserId}),
    {Space, Req2};
provide_resource(effective_groups, UserId, _Client, Req) ->
    {ok, Groups} = user_logic:get_effective_groups(UserId),
    {Groups, Req};
provide_resource(groups, UserId, _Client, Req) ->
    {ok, Groups} = user_logic:get_groups(UserId),
    {Groups, Req};
provide_resource(group, _UserId, _Client, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {ok, Group} = group_logic:get_data(GID),
    {Group, Req2};
provide_resource(client_token, UserId, _Client, Req) ->
    Token = auth_logic:gen_token(UserId),
    user_logic:add_client_token(UserId, Token),
    {[{token, Token}], Req}.

%%--------------------------------------------------------------------
%% @doc Deletes the resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Resource :: removable_resource(),
    UserId :: binary() | undefined, Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
delete_resource(user, UserId, Req) ->
    {user_logic:remove(UserId), Req};
delete_resource(space, UserId, Req) ->
    {SID, Req2} = cowboy_req:binding(sid, Req),
    {space_logic:remove_user(SID, UserId), Req2};
delete_resource(group, UserId, Req) ->
    {GID, Req2} = cowboy_req:binding(gid, Req),
    {group_logic:remove_user(GID, UserId), Req2}.
