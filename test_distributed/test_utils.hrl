%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Common definitions for ct tests.
%% @end
%% ===================================================================
-author("Tomasz Lichon").

-include_lib("ctool/include/test/test_utils.hrl").

-ifndef(GR_TEST_UTILS_HRL).
-define(GR_TEST_UTILS_HRL, 1).

%% temporary directory for test files
-define(TEMP_DIR, "/tmp").

%% Initializes test environment
-define(TEST_INIT(Config, EnvDescription),
    ?TEST_INIT(Config, EnvDescription, "globalregistry_up.py")
).

-endif.
