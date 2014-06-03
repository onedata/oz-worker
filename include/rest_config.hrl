%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Rest common definitions
%% @end
%% ===================================================================
-author("Tomasz Lichon").

-ifndef(REST_CONFIG_HRL).
-define(REST_CONFIG_HRL, 1).

-define(rest_port,8080).
-define(rest_https_acceptors,50).
-define(rest_listener, rest).

% Handler urls
-define(hello_world_url,"/hello_world").


-endif.