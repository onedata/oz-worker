%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%% dao_users header
%%% @end
%%% Created : 12. May 2014 12:34 PM
%%%-------------------------------------------------------------------

-ifndef(DAO_USERS_HRL).
-define(DAO_USERS_HRL, 1).

%% This record defines a user and is handled as a database document
-record(user, {
    name :: binary(),
    spaces = [] :: [SpaceId :: binary()],
    groups = [] :: [GroupId :: binary()]
}).

-endif.
