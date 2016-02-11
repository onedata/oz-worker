%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Dao definitions for group records
%%% @end
%%%-------------------------------------------------------------------

-ifndef(DAO_GROUPS_HRL).
-define(DAO_GROUPS_HRL, 1).

%% This record defines a group of users, it has: name, list of users that belongs to it, list of spaces that are used by this group
-record(user_group, {
    name :: binary(),
    users = [] :: [{UserId :: binary(), [privileges:group_privilege()]}],
    spaces = [] :: [SpaceId :: binary()]
}).

-endif.
