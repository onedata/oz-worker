%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /oz_api_privileges REST resource.
%%%-------------------------------------------------------------------
-module(admin_rest_module).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models_def.hrl").
-include("http/handlers/rest_handler.hrl").
-include("registered_names.hrl").

-behavior(rest_module_behavior).


-type provided_resource() :: upriv | gpriv | spaces | space | space_providers | providers | provider.
-type accepted_resource() :: upriv | gpriv.
-type removable_resource() :: upriv | gpriv.
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
        {<<"/admin/users/:id/privileges">>, M, S#rstate{resource = upriv, methods = [get, put, delete]}},
        {<<"/admin/groups/:id/privileges">>, M, S#rstate{resource = gpriv, methods = [get, put, delete]}},
        {<<"/admin/spaces">>, M, S#rstate{resource = spaces, methods = [get]}},
        {<<"/admin/spaces/:id">>, M, S#rstate{resource = space, methods = [get]}},
        {<<"/admin/spaces/:id/providers">>, M, S#rstate{resource = space_providers, methods = [get]}},
        {<<"/admin/providers/">>, M, S#rstate{resource = providers, methods = [get]}},
        {<<"/admin/providers/:id">>, M, S#rstate{resource = provider, methods = [get]}}
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
is_authorized(upriv, _, _EntityId, #client{id = UserId}) ->
    oz_api_privileges_logic:has_effective_privilege(UserId, set_privileges);
is_authorized(gpriv, _, _EntityId, #client{id = UserId}) ->
    oz_api_privileges_logic:has_effective_privilege(UserId, set_privileges);
is_authorized(spaces, _, _EntityId, #client{id = UserId}) ->
    oz_api_privileges_logic:has_effective_privilege(UserId, list_spaces);
is_authorized(space, _, _EntityId, #client{id = UserId}) ->
    oz_api_privileges_logic:has_effective_privilege(UserId, list_spaces);
is_authorized(space_providers, _, _EntityId, #client{id = UserId}) ->
    oz_api_privileges_logic:has_effective_privilege(UserId, list_providers_of_space);
is_authorized(providers, _, _EntityId, #client{id = UserId}) ->
    oz_api_privileges_logic:has_effective_privilege(UserId, list_providers);
is_authorized(provider, _, _EntityId, #client{id = UserId}) ->
    oz_api_privileges_logic:has_effective_privilege(UserId, list_providers);
is_authorized(_, _, _, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc Returns whether a resource exists.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(Resource :: resource(), ProviderId :: binary() | undefined,
    Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
% Every entity has privileges to OZ API - however they can be empty.
resource_exists(upriv, _, Req) ->
    {true, Req};
resource_exists(gpriv, _, Req) ->
    {true, Req};
resource_exists(space, SpaceId, Req) ->
    {space_logic:exists(SpaceId), Req};
resource_exists(space_providers, SpaceId, Req) ->
    {space_logic:exists(SpaceId), Req};
resource_exists(provider, ProviderId, Req) ->
    {provider_logic:exists(ProviderId), Req};
resource_exists(_, _, Req) ->
    {true, Req}.

%%--------------------------------------------------------------------
%% @doc Processes data submitted by a client through POST, PATCH, PUT on a REST
%% resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Resource :: accepted_resource(), Method :: accept_method(),
    ProviderId :: binary() | undefined, Data :: data(),
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {boolean() | {true, URL :: binary()}, cowboy_req:req()} | no_return().
accept_resource(Resource, put, EntityId, Data, _Client, Req) ->
    EntityType = resource_to_entity_type(Resource),
    BinPrivileges = rest_module_helper:assert_key_value(<<"privileges">>,
        [atom_to_binary(P, latin1) || P <- privileges:group_privileges()], Data,
        list_of_bin, Req),
    Privileges = [binary_to_existing_atom(P, latin1) || P <- BinPrivileges],
    ok = oz_api_privileges_logic:modify(EntityId, EntityType, Privileges),
    {true, Req}.

%%--------------------------------------------------------------------
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Resource :: provided_resource(), ProviderId :: binary() | undefined,
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {Data :: json_object(), cowboy_req:req()}.
provide_resource(upriv, EntityId, _Client, Req) ->
    {ok, Privileges} = oz_api_privileges_logic:get(EntityId, onedata_user),
    {[{privileges, Privileges}], Req};
provide_resource(gpriv, EntityId, _Client, Req) ->
    {ok, Privileges} = oz_api_privileges_logic:get(EntityId, user_group),
    {[{privileges, Privileges}], Req};
provide_resource(spaces, _EntityId, _Client, Req) ->
    {ok, SpaceDocs} = space:list(),
    Spaces = lists:map(fun(#document{key = SpaceId}) ->
        SpaceId
    end, SpaceDocs),
    {[{spaces, Spaces}], Req};
provide_resource(space, SpaceId, _Client, Req) ->
    {ok, Data} = space_logic:get_data(SpaceId, provider),
    {Data, Req};
provide_resource(space_providers, SpaceId, _Client, Req) ->
    {ok, Data} = space_logic:get_providers(SpaceId, provider),
    {Data, Req};
provide_resource(providers, _EntityId, _Client, Req) ->
    {ok, ProviderDocs} = provider:list(),
    Providers = lists:map(fun(#document{key = ProviderId}) ->
        ProviderId
    end, ProviderDocs),
    {[{providers, Providers}], Req};
provide_resource(provider, ProviderId, _Client, Req) ->
    {ok, Data} = provider_logic:get_data(ProviderId),
    {Data, Req}.

%%--------------------------------------------------------------------
%% @doc Deletes the resource identified by the SpaceId parameter.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Resource :: removable_resource(),
    ProviderId :: binary() | undefined, Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
delete_resource(Resource, EntityId, Req) ->
    EntityType = resource_to_entity_type(Resource),
    {oz_api_privileges_logic:remove(EntityType, EntityId), Req}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Converts REST resource to corresponding entity type accepted by
%% oz_api_privileges_logic.
%% @end
%%--------------------------------------------------------------------
-spec resource_to_entity_type(Resource :: atom()) -> atom().
resource_to_entity_type(upriv) -> onedata_user;
resource_to_entity_type(gpriv) -> user_group.
