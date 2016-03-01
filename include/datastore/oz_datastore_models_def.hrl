%%%-------------------------------------------------------------------
%%% @author Rafal Slota
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Models definitions. Extends datastore models.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(GR_DATASTORE_MODELS_HRL).
-define(GR_DATASTORE_MODELS_HRL, 1).

-include_lib("cluster_worker/include/modules/datastore/datastore_models_def.hrl").
-include("handlers/rest_handler.hrl").

-define(SUBSCRIPTIONS_STATE_KEY, <<"current_state">>).
-record(subscriptions_state, {
    cache :: gb_trees:tree()
}).

-record(provider_subscription, {
    node :: node(),
    endpoint :: binary(),
    seq :: pos_integer(),
    expires :: pos_integer(),
    clients :: #{binary() => pos_integer()}
}).

%% Records of this type store a macaroons secret
-record(onedata_auth, {
    secret :: binary(),
    user_id :: binary()
}).

%% This record defines a group of users, it has: name, list of users that belongs to it, list of spaces that are used by this group
-record(user_group, {
    name :: binary(),
    users = [] :: [{UserId :: binary(), [privileges:group_privilege()]}],
    spaces = [] :: [SpaceId :: binary()]
}).

%% This record defines a provider who support spaces and can be reached via url
-record(provider, {
    client_name :: binary(),
    redirection_point :: binary(),
    urls :: [binary()],
    spaces = [] :: [SpaceId :: binary()],
    serial :: binary()
}).

%% This record defines a space that can be used by users to store their files
-record(space, {
    name :: binary(),
    size = [] :: [{ProviderId :: binary(), Size :: pos_integer()}],
    users = [] :: [{UserId :: binary(), [privileges:space_privilege()]}],
    groups = [] :: [{GroupId :: binary(), [privileges:space_privilege()]}],
    providers = [] :: [ProviderId :: binary()]
}).

%% This record defines a token that can be used by user to do something
-record(token, {
    secret :: binary(),
    resource :: atom(),
    resource_id :: binary(),
    issuer :: rest_handler:client()
}).

% Value in DB meaning that alias is not set.
% Empty list, must be used as a list not binary so JS view will work correctly
-define(EMPTY_ALIAS, "").

% Regexp to validate aliases - at least 5 alphanumeric chars
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
-record(onedata_user, {
    name = <<"">> :: binary(),
    alias = ?EMPTY_ALIAS :: string() | integer() | binary(),
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

-type user_info() :: #onedata_user{}.
-type provider_info() :: #provider{}.
-type group_info() :: #user_group{}.
-type space_info() :: #space{}.
-type token_info() :: #token{}.
-type auth_info() :: #onedata_auth{}.

-endif.
