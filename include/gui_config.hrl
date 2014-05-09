%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%% Definitions connected with n2o
%%% @end
%%% Created : 09. May 2014 12:48 PM
%%%-------------------------------------------------------------------
-author("Tomasz Lichoń").

-ifndef(N2O_CONFIG_HRL).
-define(N2O_CONFIG_HRL, 1).


-define(gui_port,443).
% Paths in gui static directory
-define(static_paths, ["/css/", "/fonts/", "/images/", "/js/", "/n2o/"]).
-define(gui_https_acceptors,100).
-define(socket_timeout,60000).
-define(max_keepalive,30).
-define(gui_static_root,"./gui_static").

% todo generate dedicated certs (this are example ones)
-define(ca_cert_file,"./cacerts/ca.crt").
-define(cert_file,"./cacerts/server.crt").
-define(key_file,"./cacerts/server.key").

-endif.