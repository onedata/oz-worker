%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Definitions of names used to identify different parts of application (or whole application).
%% @end
%% ===================================================================
-author("Tomasz Lichon").

-ifndef(REGISTERED_NAMES_HRL).
-define(REGISTERED_NAMES_HRL, 1).

%% App name
-define(APP_Name, globalregistry).

%% Name of dao genserver
-define(Dao, dao_worker).

%% Name of oneprovider channel gen_server
-define(OpChannel, op_channel).

-endif.