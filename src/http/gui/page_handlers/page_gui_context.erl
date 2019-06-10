%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and returns the context needed
%%% by the front-end web application to connect to the proper back-end, depending
%%% on the cluster and service type.
%%% @end
%%%-------------------------------------------------------------------
-module(page_gui_context).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("http/codes.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
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
handle(<<"GET">>, Req) ->
    try
        GuiPrefix = cowboy_req:binding(gui_prefix, Req),
        ClusterId = cowboy_req:binding(gui_id, Req),

        Service = try
            onedata:service_by_gui(onedata:gui_by_prefix(GuiPrefix), ClusterId)
        catch error:badarg ->
            throw(?HTTP_404_NOT_FOUND)
        end,

        ClusterType = onedata:service_to_cluster_type(Service),
        case cluster_logic:get(?ROOT, ClusterId) of
            {ok, #od_cluster{type = ClusterType}} -> ok;
            _ -> throw(?HTTP_404_NOT_FOUND)
        end,

        ServiceType = onedata:service_type(Service),
        {ok, Domain} = cluster_logic:get_domain(ClusterId),
        ApiOrigin = case ServiceType of
            ?WORKER -> Domain;
            % @TODO VFS-4698 Should depend on Onepanel proxy
            ?ONEPANEL -> str_utils:format_bin("~s:9443", [Domain])
        end,

        cowboy_req:reply(
            ?HTTP_200_OK,
            #{<<"content-type">> => <<"application/json">>},
            json_utils:encode(#{
                <<"clusterType">> => ClusterType,
                <<"clusterId">> => ClusterId,
                <<"serviceType">> => ServiceType,
                <<"apiOrigin">> => ApiOrigin,
                <<"guiMode">> => ?UNIFIED
            }),
            Req
        )
    catch
        throw:Code when is_integer(Code) ->
            cowboy_req:reply(Code, Req);
        Type:Message ->
            ?debug_stacktrace("Bad request in ~p - ~p:~p", [?MODULE, Type, Message]),
            cowboy_req:reply(?HTTP_400_BAD_REQUEST, Req)
    end.
