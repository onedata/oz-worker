%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%% Definitions connected with rest service
%%% @end
%%% Created : 15. Apr 2014 3:19 PM
%%%-------------------------------------------------------------------
-author("Tomasz Lichon").

-ifndef(REST_CONFIG_HRL).
-define(REST_CONFIG_HRL, 1).

-define(rest_port,8080).
-define(rest_https_acceptors,50).
-define(rest_listener, rest).

% Handler urls
-define(hello_world_url,"/hello_world").


-endif.