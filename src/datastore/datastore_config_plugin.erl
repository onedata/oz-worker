%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module provides datastore config.
%%% @end
%%%-------------------------------------------------------------------
-module(datastore_config_plugin).
-author("Michal Zmuda").

%% datastore_config callbacks
-export([get_models/0]).

%%%===================================================================
%%% datastore_config callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns list of datastore custom models.
%% @end
%%--------------------------------------------------------------------
-spec get_models() -> [datastore_model:model()].
get_models() -> [
    od_user,
    od_group,
    od_space,
    od_share,
    od_provider,
    od_handle_service,
    od_handle,
    od_harvester,
    od_cluster,
    od_token,

    gs_server_state,
    entity_graph_state,
    session,
    state_token,
    dns_state,
    gui_message,
    provider_connection,
    user_connections,
    shared_token_secret,

    onedata_auth,
    macaroon_auth
].
