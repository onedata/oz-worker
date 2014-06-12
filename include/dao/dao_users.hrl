%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Dao definitions for user records
%% @end
%% ===================================================================
-author("Tomasz Lichon").


-ifndef(DAO_USERS_HRL).
-define(DAO_USERS_HRL, 1).

%% This record defines a user and is handled as a database document
-record(user, {
    name :: binary(),
    spaces = [] :: [SpaceId :: binary()],
    groups = [] :: [GroupId :: binary()]
}).

-endif.
