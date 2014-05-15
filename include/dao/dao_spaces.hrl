%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%% dao_spaces header
%%% @end
%%% Created : 12. May 2014 12:44 PM
%%%-------------------------------------------------------------------

-ifndef(DAO_SPACES_HRL).
-define(DAO_SPACES_HRL, 1).

%% This record defines a space that can be used by users to store their files
-record(space, {
    name :: binary(),
    users = [] :: [{UserId :: binary(), Privileges :: [privileges:space_privilege()]}],
    groups = [] :: [{GroupId :: binary(), Privileges :: [privileges:space_privilege()]}],
    providers = [] :: [ProviderId :: binary()]
}).

-endif.
