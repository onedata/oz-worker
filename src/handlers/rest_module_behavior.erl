%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The behavior implemented by different logic handlers behind the REST API.
%%%-------------------------------------------------------------------
-module(rest_module_behavior).
-author("Konrad Zemek").

-include("handlers/rest_handler.hrl").

%%--------------------------------------------------------------------
%% @doc Returns a Cowboy-understandable PathList of routes supported by a module
%% implementing this behavior. The State shall contain an opts record where
%% the #opts.module shall be set to the atom representing the implementing
%% module. If the route contains a resource identifier, it shall be bound to :id
%% so it will be passed as an argument of other callbacks.
%% @end
%%--------------------------------------------------------------------
-callback routes() ->
    [{PathMatch :: binary(), rest_handler, State :: rstate()}].

%%--------------------------------------------------------------------
%% @doc Returns a boolean determining if the client is authorized to carry the
%% request on the resource. ResId shall be set to the value of :id binding or
%% client's Id if there's no binding defined.
%% @end
%%--------------------------------------------------------------------
-callback is_authorized(Resource :: atom(), Method :: method(),
    ResId :: binary() | undefined, Client :: rest_handler:client()) ->
    boolean().

%%--------------------------------------------------------------------
%% @doc Returns whether a resource exists. ResId shall be set to the value of
%% :id binding or client's Id if there's no binding defined.
%% Note: The resource is always assumed to not exist when POST method is used,
%% so that 201 Created is returned. That means that POST shall only be used
%% to create a resource.
%% @end
%%--------------------------------------------------------------------
-callback resource_exists(Resource :: atom(), ResId :: binary() | undefined,
    Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.

%%--------------------------------------------------------------------
%% @doc Processes data submitted by a client through POST, PUT, PATCH on a REST
%% resource. The callback shall return {true, URL} with an URL pointing to the
%% newly created resource if it was created. Otherwise, it shall return whether
%% the operation was performed successfuly. ResId shall be set to the value of
%% :id binding or client's Id if there's no binding defined.
%% @end
%%--------------------------------------------------------------------
-callback accept_resource(Resource :: atom(), Method :: accept_method(),
    ResId :: binary() | undefined, Data :: data(),
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {boolean() | {true, URL :: binary()}, cowboy_req:req()} | no_return().

%%--------------------------------------------------------------------
%% @doc Returns data requested by a client through GET request on a REST
%% resource. ResId shall be set to the value of :id binding or client's Id if
%% there's no binding defined.
%% @end
%%--------------------------------------------------------------------
-callback provide_resource(Resource :: atom(), ResId :: binary() | undefined,
    Client :: rest_handler:client(), Req :: cowboy_req:req()) ->
    {Data :: json_object(), cowboy_req:req()}.

%%--------------------------------------------------------------------
%% @doc Deletes the resource. Returns whether the deletion was successful.
%% ResId shall be set to the value of :id binding or client's Id if there's no
%% binding defined.
%% @end
%%--------------------------------------------------------------------
-callback delete_resource(Resource :: atom(), ResId :: binary() | undefined,
    Req :: cowboy_req:req()) ->
    {boolean(), cowboy_req:req()}.