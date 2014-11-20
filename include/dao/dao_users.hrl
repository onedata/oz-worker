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

% Value in DB meaning that alias is not set.
% Empty list, must be used as a list not binary so JS view will work correctly
-define(EMPTY_ALIAS, "").

% Regexp to validate aliases - at lest 5 alphanumeric chars
-define(ALIAS_VALIDATION_REGEXP, <<"^[a-z0-9]{5,}$">>).

% String that will be put in front of uuid when a user does not have an alias set.
% Aliases are not allowed to start with this string.
-define(NO_ALIAS_UUID_PREFIX, "uuid_").

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
    alias = ?EMPTY_ALIAS :: integer() | binary(),
    email_list = [] :: [binary()],
    connected_accounts = [] :: [#oauth_account{}],
    spaces = [] :: [SpaceId :: binary()],
    default_space :: binary() | undefined,
    groups = [] :: [GroupId :: binary()],
    % TODO this is a mock
    first_space_support_token = <<"">> :: binary(),
    % TODO temporary solution
    % This allows to remember the provider which was selected for user, so DNS knows where to redirect
    default_provider = <<"">> :: binary()
}).

-endif.
