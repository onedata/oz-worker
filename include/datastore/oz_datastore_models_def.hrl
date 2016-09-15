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
-include("http/handlers/rest_handler.hrl").

% Info about identities, which are owned by this OZ
-record(owned_identity, {
    id :: undefined | identity:id(),
    encoded_public_key :: undefined | identity:encoded_public_key()
}).

% Describes state of batch.
-record(outbox, {
    timer_expires :: undefined | pos_integer(),
    timer :: undefined | timer:tref(),
    buffer :: undefined | [term()]
}).

% Stores data used to provide subscription updates
-record(subscriptions_state, {
    cache :: undefined | gb_trees:tree()
}).

% Stores state of provider subscription
-record(provider_subscription, {
    connections = [] :: [pid()],
    provider :: undefined | binary(),
    resume_at = 1 :: subscriptions:seq(),
    missing = [] :: [subscriptions:seq()],
    users = [] :: [binary()]
}).

%% Stores CA dedicated node
%% todo: implement distributed CA properly (connected with VFS-1499)
-record(ozpca_state, {
    dedicated_node :: undefined | {ok, node()} | {error, Reason :: term()}
}).

%% Records of this type store a macaroons secret
-record(onedata_auth, {
    secret :: undefined | binary(),
    user_id :: undefined | binary()
}).

%% This record defines a group of users, it has: name, list of users that belongs to it, list of spaces that are used by this group
-record(user_group, {
    name :: undefined | binary(),
    type :: undefined | user_group:type(),
    users = [] :: [{UserID :: binary(), [privileges:group_privilege()]}],
    effective_users = [] :: group_graph:effective_users(),
    effective_groups = [] :: group_graph:effective_groups(),
    nested_groups = [] :: [{GroupID :: binary(), [privileges:group_privilege()]}],
    parent_groups = [] :: [GroupID :: binary()],
    spaces = [] :: [SpaceId :: binary()]
}).

-record(groups_graph_caches_state, {
    changed_groups = [] :: [GroupID :: binary()],
    changed_users = [] :: [UserID :: binary()],
    last_rebuild = 0 :: integer()
}).

%% This record defines a provider who support spaces and can be reached via url
-record(provider, {
    client_name :: undefined | binary(),
    redirection_point :: undefined | binary(),
    urls :: undefined | [binary()],
    spaces = [] :: [SpaceId :: binary()],
    serial :: undefined | binary(),
    latitude :: undefined | float(),
    longitude :: undefined | float()
}).

%% This record defines a space that can be used by users to store their files
-record(space, {
    name :: undefined | binary(),
    providers_supports = [] :: [{ProviderId :: binary(), Size :: pos_integer()}],
    users = [] :: [{UserId :: binary(), [privileges:space_privilege()]}],
    groups = [] :: [{GroupId :: binary(), [privileges:space_privilege()]}],
    % All shares that belong to this space.
    shares = [] :: [ShareId :: binary()]
}).

%% This record defines a file/directory public share
-record(share, {
    name = undefined :: undefined | binary(),
    public_url = undefined :: undefined | binary(),
    root_file_id = undefined :: undefined | binary(),
    parent_space = undefined :: undefined | binary()
}).

%% This record defines a token that can be used by user to do something
-record(token, {
    secret :: undefined | binary(),
    resource :: undefined | atom(),
    resource_id :: undefined | binary(),
    issuer :: undefined | rest_handler:client()
}).

%% This record defines a GUI session
-record(session, {
    user_id = undefined :: onedata_user:id() | undefined,
    memory = [] :: session:memory(),
    accessed = {0, 0, 0} :: erlang:timestamp()
}).

% Value in DB meaning that alias is not set.
% Empty list, must be used as a list not binary so JS view will work correctly
-define(EMPTY_ALIAS, <<"">>).

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
    login = <<"">> :: binary(),
    % Decides if this user can login via login:password, only users created in
    % onepanel are currently allowed to do that.
    basic_auth_enabled = false :: boolean(),
    alias = ?EMPTY_ALIAS :: binary(),
    email_list = [] :: [binary()],
    connected_accounts = [] :: [#oauth_account{}],
    spaces = [] :: [SpaceId :: binary()],
    space_names = #{} :: #{SpaceId :: binary() => SpaceName :: binary()},
    default_space :: binary() | undefined,
    groups = [] :: [GroupId :: binary()],
    effective_groups = [] :: group_graph:effective_groups(),
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

%% This record contains a list of privileges possessed by certain entity
%% (user / group) to use onezone API.
-record(oz_api_privileges, {
    privileges = [] :: [oz_api_privileges:privilege()]
}).

-type user_info() :: #onedata_user{}.
-type provider_info() :: #provider{}.
-type group_info() :: #user_group{}.
-type space_info() :: #space{}.
-type token_info() :: #token{}.
-type auth_info() :: #onedata_auth{}.

-endif.
