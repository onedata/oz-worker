%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is interface to data handled by zone_logic_plugin.
%%% @end
%%%-------------------------------------------------------------------
-module(zone_logic).
-author("Wojciech Geisler").

-include("http/gui_paths.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

-export([get_configuration/0]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns Onezone configuration details, as needed by the configuration
%% endpoint.
%% @end
%%--------------------------------------------------------------------
-spec get_configuration() -> {ok, #{atom() := term()}} | {error, Reason :: term()}.
get_configuration() ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = ?NOBODY,
        gri = #gri{type = oz_worker, id = undefined, aspect = configuration}
    }).
