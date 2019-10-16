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
-include_lib("ctool/include/aai/aai.hrl").

-export([get_configuration/0]).
-export([get_gui_message_as_map/1, gui_message_exists/1, update_gui_message/3]).


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


-spec get_gui_message_as_map(gui_message:id()) ->
    {ok, gui_message:map_repr()} | {error, term()}.
get_gui_message_as_map(MessageId) ->
    case entity_logic:handle(#el_req{
        operation = get,
        auth = ?NOBODY,
        gri = #gri{type = oz_worker, id = undefined, aspect = {gui_message, MessageId}}
    }) of
        {error, _} = Error ->
            Error;
        {ok, #gui_message{enabled = Enabled, body = Body}} ->
            {ok, #{enabled => Enabled, body => Body}}
    end.


-spec gui_message_exists(gui_message:id()) -> boolean().
gui_message_exists(MessageId) ->
    gui_message:exists(MessageId).


-spec update_gui_message(aai:auth(), gui_message:id(), Data :: map()) ->
    ok | {error, term()}.
update_gui_message(Auth, MessageId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = oz_worker, id = undefined,
            aspect = {gui_message, MessageId}},
        data = Data
    }).
