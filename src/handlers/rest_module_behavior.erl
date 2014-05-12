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

%% API
-export([]).


%% routes/0
%% ====================================================================
%% @doc Returns a Cowboy-understandable PathList of routes supported by a module
%% implementing this behavior. The State shall contain a reqstate record
%% adhering to following rules:
%% * the #reqstate.resource shall be set to `main` for the main resource point
%% * the #reqstate.module shall be set to the name of the implementing module.
%% By convention, only the main resource supports GET, POST, DELETE methods;
%% other resources are assumed to support only POST.
%% If the route contains a resource identifier, it shall be bound to :id .
%% @end
-callback routes() ->
    [{PathMatch :: string() | binary(), rest_handler, State :: #reqstate{}}].


%% is_authorized/2
%% ====================================================================
%% @doc Returns a boolean() determining if the authenticated client is
%% authorized to carry the request on the resource.
%% @end
%% ====================================================================
-callback is_authorized(Id :: binary(), State :: #reqstate{}) ->
    boolean().


%% accept_resource/3
%% ====================================================================
%% @doc Processes data submitted by a client through POST on a REST resource.
%% The data is given as a proplist where strings are given as binary().
%% If a Response is returned, it's then enoded to JSON and returned as a body;
%% thus the returned proplist shall be constructed in such a way that it
%% will produce a valid JSON response when encoded.
%% @end
%% ====================================================================
-callback accept_resource(Id :: binary(), Data :: [proplists:property()],
                          State :: #reqstate{}) ->
    {ok, Response :: [proplists:property()]} | ok | {error, Reason :: term()}.


%% provide_resource/2
%% ====================================================================
%% @doc Returns data requested by a client through GET on a REST resource.
%% The data returned shall be a proplist constructed in such a way that it
%% will produce a valid JSON response when encoded.
%% @end
%% ====================================================================
-callback provide_resource(Id :: binary(), State :: #reqstate{}) ->
    {ok, Data :: [proplists:property()]} | {error, Reason :: term()}.


%% delete_resource/2
%% ====================================================================
%% @doc Deletes the resource identified by the Id parameter.
%% ====================================================================
-callback delete_resource(Id :: binary(), State :: #reqstate{}) ->
    ok | {error, Reason :: term()}.
