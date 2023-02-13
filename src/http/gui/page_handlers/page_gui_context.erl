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

-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
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
        OnepanelProxy = case cluster_logic:get(?ROOT, ClusterId) of
            {ok, #od_cluster{type = ClusterType, onepanel_proxy = Proxy}} ->
                Proxy;
            _ ->
                throw(?HTTP_404_NOT_FOUND)
        end,

        ServiceType = onedata:service_type(Service),

        % in case of Onezone, use the same host as requested by the client
        % (i.e. the same address that the client visited in his web browser)
        Domain = case ClusterId of
            ?ONEZONE_CLUSTER_ID ->
                cowboy_req:host(Req);
            _ ->
                {ok, ServiceDomain} = cluster_logic:get_domain(ClusterId),
                ServiceDomain
        end,
        ApiOrigin = case {ServiceType, OnepanelProxy} of
            {?WORKER, _} -> Domain;
            {?ONEPANEL, true} -> Domain;
            {?ONEPANEL, false} -> str_utils:format_bin("~s:9443", [Domain])
        end,

        cowboy_req:reply(
            ?HTTP_200_OK,
            #{?HDR_CONTENT_TYPE => <<"application/json">>},
            json_utils:encode(#{
                <<"clusterType">> => ClusterType,
                <<"clusterId">> => ClusterId,
                <<"serviceType">> => ServiceType,
                <<"apiOrigin">> => ApiOrigin,
                <<"guiMode">> => ?UNIFIED,
                <<"browserDebugLogs">> => oz_worker:get_env(gui_debug_mode, false)
            }),
            Req
        )
    catch
        throw:Code when is_integer(Code) ->
            cowboy_req:reply(Code, Req);
        Class:Reason:Stacktrace ->
            ?debug_exception(Class, Reason, Stacktrace),
            cowboy_req:reply(?HTTP_400_BAD_REQUEST, Req)
    end.
