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

% Value in DB meaning that alias is not set.
% Empty list, must be used as a list not binary so JS view will work correctly
-define(EMPTY_ALIAS, <<"">>).

% Regexp to validate aliases - at least 5 alphanumeric chars
-define(ALIAS_VALIDATION_REGEXP, <<"^[a-z0-9]{5,}$">>).

% String that will be put in front of uuid when a user does not have an alias set.
% Aliases are not allowed to start with this string.
-define(NO_ALIAS_UUID_PREFIX, "uuid_").


%% This record defines a user and is handled as a database document
-record(od_user, {
    name = <<"">> :: binary(),
    login = <<"">> :: binary(),
    % Decides if this user can login via login:password, only users created in
    % onepanel are currently allowed to do that.
    basic_auth_enabled = false :: boolean(),
    alias = ?EMPTY_ALIAS :: binary(),
    email_list = [] :: [binary()],
    connected_accounts = [] :: [#oauth_account{}],
    default_space = undefined :: binary() | undefined,
    % This allows to remember the provider which was selected for user,
    % so DNS knows where to redirect
    default_provider = undefined :: binary() | undefined,
    % This allows to remember to which provider user is being redirected.
    % It is needed in DNS so it knows where to redirect.
    chosen_provider = undefined :: binary() | undefined,
    % List of user's client tokens
    client_tokens = [] :: [binary()],
    % List of user's aliases for spaces
    space_aliases = #{} :: #{od_space:id() => binary()},

    % Direct relations to other entities
    spaces = [] :: [od_space:id()],
    groups = [] :: [od_group:id()],
    handle_services = [] :: [od_handle_service:id()],
    handles = [] :: [od_handle:id()],

    % Effective relations to other entities
    eff_groups = [] :: [od_group:id()],
    eff_spaces = [] :: [od_space:id()],
    eff_shares = [] :: [od_share:id()],
    eff_providers = [] :: [od_provider:id()],
    eff_handle_services = [] :: [od_handle_service:id()],
    eff_handles = [] :: [od_handle:id()]
}).


%% This record defines a group of users, it has: name, list of users that
%% belongs to it, list of spaces that are used by this group
-record(od_group, {
    name = undefined :: binary() | undefined,
    type = undefined :: od_group:type() | undefined,

    % Group graph related entities
    parent_groups = [] :: [od_group:id()],
    eff_parent_groups = [] :: [od_group:id()],
    nested_groups = [] :: [{od_group:id(), [privileges:group_privilege()]}],

    % Direct relations to other entities
    users = [] :: [{od_user:id(), [privileges:group_privilege()]}],
    spaces = [] :: [od_space:id()],
    handle_services = [] :: [od_handle_service:id()],
    handles = [] :: [od_handle:id()],

    % Effective relations to other entities
    eff_users = [] :: [{od_user:id(), [privileges:group_privilege()]}],
    eff_spaces = [] :: [od_space:id()],
    eff_shares = [] :: [od_share:id()],
    eff_providers = [] :: [od_provider:id()],
    eff_handle_services = [] :: [od_handle_service:id()],
    eff_handles = [] :: [od_handle:id()]
}).


%% This record defines a space that can be used by users to store their files
-record(od_space, {
    name :: undefined | binary(),
    providers_supports = [] :: [{od_provider:id(), Size :: pos_integer()}],
    users = [] :: [{od_user:id(), [privileges:space_privilege()]}],
    groups = [] :: [{od_group:id(), [privileges:space_privilege()]}],
    % All shares that belong to this space.
    shares = [] :: [od_share:id()]
}).


%% This record defines a file/directory public share
-record(od_share, {
    name = undefined :: undefined | binary(),
    public_url = undefined :: undefined | binary(),
    root_file_id = undefined :: undefined | binary(),
    parent_space = undefined :: undefined | binary(),
    handle = undefined :: undefined | od_handle:id()
}).


-record(od_handle_service, {
    name :: od_handle_service:name() | undefined,
    proxy_endpoint :: od_handle_service:proxy_endpoint() | undefined,
    service_properties = [] :: od_handle_service:service_properties(),
    users = [] :: [{od_user:id(), [privileges:handle_service_privilege()]}],
    groups = [] :: [{od_group:id(), [privileges:handle_service_privilege()]}]
}).


-record(od_handle, {
    handle_service_id :: od_handle_service:id() | undefined,
    public_handle :: od_handle:public_handle() | undefined,
    resource_type :: od_handle:resource_type() | undefined,
    resource_id :: od_handle:resource_id() | undefined,
    metadata :: od_handle:metadata() | undefined,
    users = [] :: [{od_user:id(), [privileges:handle_privilege()]}],
    groups = [] :: [{od_group:id(), [privileges:handle_privilege()]}],
    timestamp = od_handle:actual_timestamp() :: od_handle:timestamp()
}).


%% This record defines a provider who support spaces and can be reached via url
-record(od_provider, {
    client_name :: undefined | binary(),
    redirection_point :: undefined | binary(),
    urls :: undefined | [binary()],
    spaces = [] :: [od_space:id()],
    serial :: undefined | binary(),
    latitude :: undefined | float(),
    longitude :: undefined | float()
}).


%% This record contains a list of privileges possessed by certain entity
%% (user / group) to use onezone API.
-record(oz_api_privileges, {
    privileges = [] :: [oz_api_privileges:privilege()]
}).


%% This record defines a GUI session
-record(session, {
    user_id = undefined :: od_user:id() | undefined,
    memory = [] :: session:memory(),
    accessed = {0, 0, 0} :: erlang:timestamp()
}).


%% This record defines a token that can be used by user to do something
-record(token, {
    secret :: undefined | binary(),
    resource :: undefined | atom(),
    resource_id :: undefined | binary(),
    issuer :: undefined | rest_handler:client()
}).


%% Records of this type store a macaroons secret
-record(onedata_auth, {
    secret :: undefined | binary(),
    user_id :: undefined | binary()
}).


%% This record defines user's account info
%% received from an openid / oauth provider
-record(oauth_account, {
    provider_id = undefined :: atom(),
    user_id = <<"">> :: binary(),
    login = <<"">> :: binary(),
    name = <<"">> :: binary(),
    email_list = [] :: [binary()]
}).


%% Stores CA dedicated node
%% todo: implement distributed CA properly (connected with VFS-1499)
-record(ozpca_state, {
    dedicated_node :: undefined | {ok, node()} | {error, Reason :: term()}
}).


-record(groups_graph_caches_state, {
    changed_groups = [] :: [od_group:id()],
    changed_users = [] :: [od_user:id()],
    last_rebuild = 0 :: integer()
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


% Info about identities, which are owned by this OZ
-record(owned_identity, {
    id :: undefined | identity:id(),
    encoded_public_key :: undefined | identity:encoded_public_key()
}).

-endif.
