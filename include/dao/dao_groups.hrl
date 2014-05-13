%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2014, ACK CYFRONET AGH
%%% @doc
%%% dao_groups header
%%% @end
%%% Created : 12. May 2014 12:41 PM
%%%-------------------------------------------------------------------

-ifndef(DAO_GROUPS_HRL).
-define(DAO_GROUPS_HRL, 1).

%% This record defines a group of users
-record(user_group, {name = "", users = [], spaces = []}).

-endif.