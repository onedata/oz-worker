%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%% Defines connected with rest service
%%% @end
%%% Created : 15. Apr 2014 3:19 PM
%%%-------------------------------------------------------------------
-author("Tomasz Lichon").

-ifndef(REST_CONFIG_HRL).
-define(REST_CONFIG_HRL, 1).

-define(REST_PORT,8080).
-define(HTTP_ACCEPTORS,50).

% Handler urls
-define(HELLO_WORLD_URL,"/hello_world").

-endif.