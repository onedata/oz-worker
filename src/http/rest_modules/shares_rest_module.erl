%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /shares REST resources.
%%%-------------------------------------------------------------------
-module(shares_rest_module).
-author("Lukasz Opiola").

-include("http/handlers/rest_handler.hrl").
-include_lib("ctool/include/logging.hrl").

-behavior(rest_module_behavior).


-type provided_resource() :: share.
-type accepted_resource() :: share.
-type removable_resource() :: share.
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
    S = #rstate{module = ?MODULE, root = share},
    M = rest_handler,
    [
        {<<"/shares/:id">>, M, S#rstate{resource = share, methods = [get, patch, delete]}}
    ].

%%--------------------------------------------------------------------
%% @doc Returns a boolean() determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Resource :: resource(), Method :: method(),
    ShareId :: binary() | undefined, Client :: rest_handler:client()) ->
    boolean().
is_authorized(_, _, _, #client{type = undefined}) ->
    false;
is_authorized(share, get, ShareId, #client{type = user, id = UserId}) ->
    {ok, ParentSpace} = share_logic:get_parent(ShareId),
    % Share - to view shares, it's enough to belong to parent space
    space_logic:has_effective_user(ParentSpace, UserId);
is_authorized(share, get, _ShareId, #client{type = provider}) ->
    % All providers are allowed to get information about a share - it is public
    % and all of them should be able to display the shared data.
    true;
is_authorized(share, patch, ShareId, #client{type = user, id = UserId}) ->
    {ok, ParentSpace} = share_logic:get_parent(ShareId),
    space_logic:has_effective_privilege(ParentSpace, UserId, space_manage_shares);
is_authorized(share, delete, ShareId, #client{type = user, id = UserId}) ->
    {ok, ParentSpace} = share_logic:get_parent(ShareId),
    space_logic:has_effective_privilege(ParentSpace, UserId, space_manage_shares);
is_authorized(_, _, _, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc Returns whether a resource exists.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(Resource :: resource(), ShareId :: binary() | undefined,
    Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
resource_exists(share, ShareId, Req) ->
    {share_logic:exists(ShareId), Req}.

%%--------------------------------------------------------------------
%% @doc Processes data submitted by a client through POST, PATCH, PUT on a REST
%% resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Resource :: accepted_resource(), Method :: accept_method(),
    ShareId :: binary() | undefined, Data :: data(),
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {boolean() | {true, URL :: binary()}, cowboy_req:req()} | no_return().
accept_resource(share, patch, ShareId, Data, _Client, Req) ->
    NewName = rest_module_helper:assert_key(<<"name">>, Data, binary, Req),
    {ok, ShareId} = share_logic:modify(ShareId, NewName),
    {true, Req}.

%%--------------------------------------------------------------------
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Resource :: provided_resource(), ShareId :: binary() | undefined,
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {Data :: json_object(), cowboy_req:req()}.
provide_resource(share, ShareId, #client{type = ClientType}, Req) ->
    {ok, Data} = share_logic:get_data(ShareId, ClientType),
    {Data, Req}.

%%--------------------------------------------------------------------
%% @doc Deletes the resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Resource :: removable_resource(),
    ShareId :: binary() | undefined, Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
delete_resource(share, ShareId, Req) ->
    {share_logic:remove(ShareId), Req}.
