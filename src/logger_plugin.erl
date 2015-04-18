%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module implements logger_plugin_behaviour in order
%% to customize logging in globalregistry application.
%% @end
%% ===================================================================
-module(logger_plugin).
-behaviour(logger_plugin_behaviour).

%% logger_plugin_behaviour API
-export([gather_metadata/0]).


%% gather_metadata/0
%% ====================================================================
%% @doc Should return a list of key, value tuples to be concatenated
%% to standard log metadata.
%% @end
-spec gather_metadata() -> [{Key :: term(), Value :: term()}].
%% ====================================================================
gather_metadata() ->
    % No extra metadata will be collected.
    [].