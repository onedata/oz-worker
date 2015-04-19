%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Dao definitions for space records
%%% @end
%%%-------------------------------------------------------------------

-ifndef(DAO_SPACES_HRL).
-define(DAO_SPACES_HRL, 1).

%% This record defines a space that can be used by users to store their files
-record(space, {
    name :: binary(),
    size = [] :: [{ProviderId :: binary(), Size :: pos_integer()}],
    users = [] :: [{UserId :: binary(), [privileges:space_privilege()]}],
    groups = [] :: [{GroupId :: binary(), [privileges:space_privilege()]}],
    providers = [] :: [ProviderId :: binary()]
}).

-endif.
