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


-define(gui_port,443).
-define(gui_https_listener,https).
% Paths in gui static directory
-define(static_paths, ["/css/", "/fonts/", "/images/", "/js/", "/n2o/"]).
-define(gui_https_acceptors,100).
-define(socket_timeout,60000).
-define(max_keepalive,30).
-define(gui_static_root,"./gui_static").

-endif.