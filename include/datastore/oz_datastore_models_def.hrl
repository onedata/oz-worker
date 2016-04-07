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

% Describes state of batch.
-record(outbox, {
    timer_expires :: pos_integer(),
    timer :: timer:tref(),
    buffer :: [term()]
}).

% Stores data used to provide subscription updates
-record(subscriptions_state, {
    cache :: gb_trees:tree()
}).

% Stores state of provider subscription
-record(provider_subscription, {
    connections = [] :: [pid()],
    provider :: binary(),
    resume_at = 1 :: subscriptions:seq(),
    missing = [] :: [subscriptions:seq()],
    users = [] :: [binary()]
}).

%% Stores CA dedicated node
%% todo: implement distributed CA properly (connected with VFS-1499)
-record(ozpca_state, {
    dedicated_node :: {ok, node()} | {error, Reason :: term()}
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
    serial :: binary(),
    latitude :: float(),
    longitude :: float()
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

%% This record defines a GUI session
-record(session, {
    user_id = undefined :: onedata_user:id() | undefined,
    memory = [] :: session:memory(),
    accessed = {0, 0, 0} :: erlang:timestamp()
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
    provider_id = undefined :: atom(),
    user_id = <<"">> :: binary(),
    login = <<"">> :: binary(),
    name = <<"">> :: binary(),
    email_list = [] :: [binary()]
}).

%% This record defines a user and is handled as a database document
-record(onedata_user, {
    name = <<"">> :: binary(),
    alias = ?EMPTY_ALIAS :: string() | integer() | binary(),
    email_list = [] :: [binary()],
    connected_accounts = [] :: [#oauth_account{}],
    spaces = [] :: [SpaceId :: binary()],
    space_names = #{} :: #{SpaceId :: binary() => SpaceName :: binary()},
    default_space :: binary() | undefined,
    groups = [] :: [GroupId :: binary()],
    % TODO this is a mock
    first_space_support_token = <<"">> :: binary(),
    % This allows to remember the provider which was selected for user, so DNS knows where to redirect
    default_provider = undefined :: binary() | undefined,
    % This allows to remember to which provider user is being redirected.
    % It is needed in DNS so it knows where to redirect.
    chosen_provider = undefined :: binary() | undefined,
    % List of user's client tokens
    client_tokens = [] :: [binary()]
}).

-type user_info() :: #onedata_user{}.
-type provider_info() :: #provider{}.
-type group_info() :: #user_group{}.
-type space_info() :: #space{}.
-type token_info() :: #token{}.
-type auth_info() :: #onedata_auth{}.

-endif.
