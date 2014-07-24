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

%% This record defines user's account info
%% received from an openid / oauth provider
-record(oauth_account, {
    provider_id = undefined,
    user_id = <<"">>,
    login = <<"">>,
    name = <<"">>,
    email_list = []
}).

%% This record defines a user and is handled as a database document
-record(user, {
    name = <<"">> :: binary(),
    email_list = [] :: [binary()],
    connected_accounts = [] :: [#oauth_account{}],
    spaces = [] :: [SpaceId :: binary()],
    groups = [] :: [GroupId :: binary()],
    % TODO this is a mock
    first_space_support_token = <<"">> :: binary()
}).

-endif.
