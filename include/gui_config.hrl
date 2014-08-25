%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Gui common definitions
%% @end
%% ===================================================================
-author("Tomasz Lichon").


-ifndef(N2O_CONFIG_HRL).
-define(N2O_CONFIG_HRL, 1).

% ID of GUI listener
-define(gui_https_listener, https).

% Path to directory that contains gui static files
-define(gui_static_root, "releases/data/gui_static").
% Paths in gui static directory
-define(static_paths, ["/css/", "/fonts/", "/images/", "/js/", "/n2o/"]).

% Session logic module
-define(session_logic_module, session_logic).

% GUI routing module
-define(gui_routing_module, gui_routes).

-endif.