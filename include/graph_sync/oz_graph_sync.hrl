%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains common macros for modules related to Graph Sync.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(OZ_GRAPH_SYNC_HRL).
-define(OZ_GRAPH_SYNC_HRL, 1).

% Global worker identifier
-define(GS_SERVER_WORKER_GLOBAL_NAME, graph_sync_server_worker).

% Graph Sync endpoints
-define(PROVIDER_GRAPH_SYNC_WS_PATH, "/graph_sync/provider").
-define(GUI_GRAPH_SYNC_WS_PATH, "/graph_sync/gui").

-endif.

