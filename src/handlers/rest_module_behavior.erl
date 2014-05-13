%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The behavior implemented by different logic handlers behind the REST API.
%% ===================================================================
-module(rest_module_behavior).
-author("Konrad Zemek").

-include("handlers/rest_handler.hrl").


%% routes/0
%% ====================================================================
%% @doc Returns a Cowboy-understandable PathList of routes supported by a module
%% implementing this behavior. The State shall contain a reqstate record
%% adhering to following rules:
%% * the #reqstate.resource shall be set to `main` for the main resource point,
%% * the #reqstate.resource shall be set to `create` for the create method.
%% * the #reqstate.module shall be set to the name of the implementing module.
%% If the route contains a resource identifier, it shall be bound to :id .
%% By convention, only the main resource supports GET, POST, DELETE methods;
%% other resources are assumed to support only POST.
%% @end
-callback routes() ->
    [{PathMatch :: string() | binary(), rest_handler, State :: #reqstate{}}].


%% is_authorized/2
%% ====================================================================
%% @doc Returns a boolean determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @end
%% ====================================================================
-callback is_authorized(Method :: binary(), State :: #reqstate{}) ->
    boolean().


%% accept_resource/3
%% ====================================================================
%% @doc Processes data submitted by a client through POST on a REST resource.
%% If a Response is returned, it's then enoded to JSON and returned as a body.
%% @end
%% ====================================================================
-callback accept_resource(State :: #reqstate{}) ->
    {ok, Response :: reqdata()} | ok | {error, Reason :: term()}.


%% provide_resource/2
%% ====================================================================
%% @doc Returns data requested by a client through GET on a REST resource.
%% ====================================================================
-callback provide_resource(State :: #reqstate{}) ->
    {ok, Data :: reqdata()} | {error, Reason :: term()}.


%% delete_resource/2
%% ====================================================================
%% @doc Deletes the resource.
%% ====================================================================
-callback delete_resource(State :: #reqstate{}) ->
    ok | {error, Reason :: term()}.
