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

%% This record defines user's account info
%% received from an openid / oauth provider
-record(oauth_account, {
    provider_id = undefined,
    user_id = <<"">>,
    login = <<"">>,
    name = <<"">>,
    emails = <<"">>
}).

%% This record defines a user and is handled as a database document
-record(user, {
    name = <<"">> :: binary(),
    emails = [] :: [binary()],
    connected_accounts = [] :: [#oauth_account{}],
    spaces = [] :: [SpaceId :: binary()],
    groups = [] :: [GroupId :: binary()]
}).

-endif.
