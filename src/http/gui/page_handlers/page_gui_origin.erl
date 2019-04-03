%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and returns
%%% the origin (domain) of the GUI data backend for given cluster.
%%% @end
%%%-------------------------------------------------------------------
-module(page_gui_origin).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("http/codes.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

-export([handle/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link dynamic_page_behaviour} callback handle/2.
%% @end
%%--------------------------------------------------------------------
-spec handle(gui:method(), cowboy_req:req()) -> cowboy_req:req().
handle(<<"POST">>, Req) ->
    try
        {ok, RequestBody, Req2} = cowboy_req:read_body(Req),
        cowboy_req:reply(
            ?HTTP_200_OK,
            #{<<"content-type">> => <<"application/json">>},
            get_origin(RequestBody),
            Req2
        )
    catch
        Type:Message ->
            ?debug_stacktrace("Bad request in ~p - ~p:~p", [?MODULE, Type, Message]),
            cowboy_req:reply(?HTTP_400_BAD_REQUEST, Req)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec get_origin(RequestBody :: binary()) ->
    Response :: binary() | no_return().
get_origin(RequestBody) ->
    #{
        <<"clusterType">> := ClusterTypeBin,
        <<"clusterId">> := ClusterId
    } = json_utils:decode(RequestBody),
    ClusterType = binary_to_existing_atom(ClusterTypeBin, utf8),
    % Ensure request correctness or crash with a badmatch
    {ok, #od_cluster{type = ClusterType}} = cluster_logic:get(?ROOT, ClusterId),
    {ok, Domain} = cluster_logic:get_domain(ClusterId),
    json_utils:encode(#{
        <<"domain">> => Domain
    }).
