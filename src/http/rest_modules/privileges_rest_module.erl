%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /oz_api_privileges REST resource.
%%%-------------------------------------------------------------------
-module(privileges_rest_module).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models_def.hrl").
-include("http/handlers/rest_handler.hrl").
-include("registered_names.hrl").

-behavior(rest_module_behavior).


-type provided_resource() :: user | group.
-type accepted_resource() :: user | group.
-type removable_resource() :: user | group.
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
    S = #rstate{module = ?MODULE, root = oz_api_privileges},
    M = rest_handler,
    [
        {<<"/privileges/users/:id/">>, M, S#rstate{resource = user, methods = [get, patch, delete]}},
        {<<"/privileges/groups/:id/">>, M, S#rstate{resource = group, methods = [get, patch, delete]}}
    ].


%%--------------------------------------------------------------------
%% @doc Returns a boolean() determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Resource :: resource(), Method :: method(),
    EntityId :: binary() | undefined, Client :: rest_handler:client()) ->
    boolean().
is_authorized(user, get, _EntityId, #client{type = user, id = UserId}) ->
    oz_api_privileges_logic:has_effective_privilege(UserId, view_privileges);
is_authorized(user, _, _EntityId, #client{type = user, id = UserId}) ->
    oz_api_privileges_logic:has_effective_privilege(UserId, set_privileges);
is_authorized(group, get, _EntityId, #client{type = user, id = UserId}) ->
    oz_api_privileges_logic:has_effective_privilege(UserId, view_privileges);
is_authorized(group, _, _EntityId, #client{type = user, id = UserId}) ->
    oz_api_privileges_logic:has_effective_privilege(UserId, set_privileges);
is_authorized(_, _, _, _) ->
    false.


%%--------------------------------------------------------------------
%% @doc Returns whether a resource exists.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(Resource :: resource(), EntityId :: binary() | undefined,
    Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
% Every existing entity has privileges to OZ API - however they can be an empty
% list, which is usually the case.
resource_exists(user, UserId, Req) ->
    Result = user_logic:exists(UserId),
    {Result, Req};
resource_exists(group, GroupId, Req) ->
    Result = group_logic:exists(GroupId),
    {Result, Req};
resource_exists(_, _, Req) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc Processes data submitted by a client through POST, PATCH, PUT on a REST
%% resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Res :: accepted_resource(), Method :: accept_method(),
    ProviderId :: binary() | undefined, Data :: data(),
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {boolean() | {true, URL :: binary()}, cowboy_req:req()} | no_return().
accept_resource(Resource, patch, EntityId, Data, _Client, Req) ->
    BinPrivileges = rest_module_helper:assert_key_value(<<"privileges">>,
        [atom_to_binary(P, latin1) || P <- oz_api_privileges:all_privileges()],
        Data, list_of_bin, Req),
    Privileges = [binary_to_existing_atom(P, latin1) || P <- BinPrivileges],
    EntityType = resource_to_entity_type(Resource),
    case oz_api_privileges_logic:modify(EntityId, EntityType, Privileges) of
        ok ->
            {true, Req};
        _ ->
            {false, Req}
    end.


%%--------------------------------------------------------------------
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Resource :: provided_resource(),
    ProviderId :: binary() | undefined,
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {Data :: json_object(), cowboy_req:req()}.
provide_resource(Resource, EntityId, _Client, Req) ->
    EntityType = resource_to_entity_type(Resource),
    {ok, Privileges} = oz_api_privileges_logic:get(EntityId, EntityType),
    {[{privileges, Privileges}], Req}.


%%--------------------------------------------------------------------
%% @doc Deletes the resource identified by the SpaceId parameter.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Resource :: removable_resource(),
    EntityId :: binary() | undefined, Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
delete_resource(Resource, EntityId, Req) ->
    EntityType = resource_to_entity_type(Resource),
    {oz_api_privileges_logic:remove(EntityId, EntityType), Req}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts resource type as defined in routes to entity type
%% of oz_api_privilege.
%% @end
%%--------------------------------------------------------------------
-spec resource_to_entity_type(Resource :: resource()) ->
    oz_api_privileges:entity_type().
resource_to_entity_type(user) -> od_user;
resource_to_entity_type(group) -> od_group.
