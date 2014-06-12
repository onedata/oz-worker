%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Common definitions for ct tests
%% @end
%% ===================================================================
-author("Tomasz Lichon").


-ifndef(TEST_UTILS_HRL).
-define(TEST_UTILS_HRL, 1).

-define(GR_DEPS,[sasl,lager,ssl,erlydtl,mimetypes,ranch,crypto,cowboy,gproc,n2o,ibrowse]).

-define(cert_paths,{ca_cert_file,"../../test_certs/ca.crt"},{cert_file,"../../test_certs/server.crt"},{key_file,"../../test_certs/server.key"}).

-endif.
