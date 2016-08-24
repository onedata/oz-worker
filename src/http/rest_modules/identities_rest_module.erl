%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /provider REST resource.
%%%-------------------------------------------------------------------
-module(identities_rest_module).
-author("Michal Zmuda").

-include("http/handlers/rest_handler.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-behavior(rest_module_behavior).


-type provided_resource() :: publickey.
-type accepted_resource() :: publickey.
-type removable_resource() :: publickey.
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
    S = #rstate{module = ?MODULE, root = publickey},
    M = rest_handler,
    [
        {<<"/publickey/:id">>, M, S#rstate{resource = publickey, methods = [get, patch], noauth = [get]}},
        {<<"/provider_data/:id">>, M, S#rstate{resource = provider, methods = [post], noauth = [post]}}
    ].

%%--------------------------------------------------------------------
%% @doc Returns a boolean() determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Resource :: resource(), Method :: method(),
    ID :: binary() | undefined, Client :: rest_handler:client()) ->
    boolean().
is_authorized(provider, _, _, _) ->
    true;
is_authorized(publickey, put, MatchingID, #client{type = provider, id = MatchingID}) ->
    true;
is_authorized(publickey, post, MatchingID, #client{type = provider, id = MatchingID}) ->
    true;
is_authorized(publickey, _,_, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc Returns whether a resource exists.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(Resource :: resource(), ID :: binary() | undefined,
    Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
resource_exists(publickey, ID, Req) ->
    case plugins:apply(identity_repository, get, [ID]) of
        {error, _} -> {false, Req};
        {ok, _} -> {true, Req}
    end.

%%--------------------------------------------------------------------
%% @doc Processes data submitted by a client through POST, PATCH, PUT on a REST
%% resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Resource :: accepted_resource(), Method :: accept_method(),
    ID :: binary() | undefined, Data :: data(),
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {boolean() | {true, URL :: binary()}, cowboy_req:req()} | no_return().
accept_resource(provider, _, ID, Data, _Client, Req) ->
    EncodedPublicKey = rest_module_helper:assert_key(<<"publicKey">>, Data, binary, Req),
    URLs = rest_module_helper:assert_key(<<"urls">>, Data, list_of_bin, Req),
    RedirectionPoint = rest_module_helper:assert_key(<<"redirectionPoint">>, Data, binary, Req),

    case plugins:apply(identity_repository, publish, [ID, EncodedPublicKey]) of
        ok ->
            Provider = #provider{client_name = ID, urls = URLs, redirection_point = RedirectionPoint},
            {ok, _} = provider:save(#document{key = ID, value = Provider}),
            {true, Req};
        {error, Reason} ->
            ?warning("Unable to create new provider with ID ~p due to ~p", [ID, Reason]),
            {false, Req}
    end;
accept_resource(publickey, _, ID, Data, _Client, Req) ->
    EncodedPublicKey = rest_module_helper:assert_key(<<"publicKey">>, Data, binary, Req),
    case plugins:apply(identity_repository, publish, [ID, EncodedPublicKey]) of
        ok -> {true, Req};
        {error, _Reason} ->
            ?warning("Client ~p unsucessfuly tried to override key of ~p", [_Client, ID]),
            {false, Req}
    end.

%%--------------------------------------------------------------------
%% @doc Returns data requested by a client through GET on a REST resource.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Resource :: provided_resource(), ID :: binary() | undefined,
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {Data :: json_object(), cowboy_req:req()}.
provide_resource(publickey, ID, _, Req) ->
    %% resource_exists verified, that resource is obtainable
    {ok, EncodedPublicKey} = plugins:apply(identity_repository, get, [ID]),
    {[{<<"publicKey">>, EncodedPublicKey}], Req}.

%%--------------------------------------------------------------------
%% @doc Deletes the resource identified by the SpaceId parameter.
%% @see rest_module_behavior
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Resource :: removable_resource(),
    ID :: binary() | undefined, Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.
delete_resource(_, _, Req) ->
    {false, Req}.