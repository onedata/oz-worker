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

-ifndef(OZ_DATASTORE_MODELS_HRL).
-define(OZ_DATASTORE_MODELS_HRL, 1).

-include("entity_logic.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("cluster_worker/include/modules/datastore/datastore_models.hrl").

-define(DEFAULT_GROUP_TYPE, team).
-define(DEFAULT_RELEASE_VERSION, <<"18.02.*">>).
-define(DEFAULT_BUILD_VERSION, <<"unknown">>).
-define(EMPTY_GUI_HASH, <<"empty">>).

%%%===================================================================
%%% DB records definitions
%%%===================================================================

%% This record must be defined here as od_user depends on it.
%% This record defines user's account info received from a SAML / OpenID provider.
-record(linked_account, {
    idp :: atom(),
    subject_id :: binary(),
    full_name = undefined :: undefined | binary(),
    username = undefined :: undefined | binary(),
    emails = [] :: [binary()],
    % A list of entitlements in given IdP, they must be normalized according
    % to specification in entitlement_mapping.
    entitlements = [] :: [entitlement_mapping:raw_entitlement()],
    % Custom attributes received from the IdP, opaque to Onezone (used in LUMA).
    custom = #{} :: json_utils:json_term(),
    access_token = {undefined, 0} :: {undefined | binary(), Expires :: non_neg_integer()},
    refresh_token = undefined :: undefined | binary()
}).


%% This record must be defined here as od_harvester depends on it.
-record(harvester_index, {
    name :: binary(),
    schema = undefined :: od_harvester:schema() | undefined,
    % mapping of index name to one recognized by gui plugin.
    gui_plugin_name = undefined :: binary() | undefined,
    stats = #{} :: od_harvester:indices_stats()
}).


%% This record must be defined here as od_harvester depends on it.
-record(index_stats, {
    % sequence harvested in this index
    current_seq = 0 :: integer(),
    % highest sequence known in given space in given provider
    max_seq = 0 :: integer(),
    % timestamp of last harvesting
    last_update = undefined :: integer() | undefined,
    % short description of encountered error if last harvesting failed
    error = undefined :: binary() | undefined,
    % stats are marked archival when it is no longer possible to harvest metadata
    % in given space in given provider e.g space was removed from harvester
    archival = false :: boolean()
}).


%%%===================================================================
%%% Records synchronized via Graph Sync
%%%===================================================================

% Records starting with prefix od_ are special records that represent entities
% in the system and are synchronized to providers via Graph Sync.
% The entities can have various relations between them, especially effective
% membership is possible via groups and nested groups.
% The effective relations are calculated top-down,
% to obtain effective memberships to other entities for every user,
% and bottom-up to obtain effective members for every entity. The records
% contain the fields top_down_dirty and bottom_up_dirty that mark records
% that have not yet been calculated in given direction.
%
% The below ASCII visual shows possible relations in entities graph.
%
%           provider------------------------------------------>cluster
%              ^                                                ^  ^
%              |                                 share          |  |
%              |                                   ^           /   |
%              |                                   |          /    |
%            space            handle_service<----handle      /     |
%           ^ ^ ^ ^             ^         ^       ^  ^      /     /
%          /  | |  \           /          |      /   |     /     /
%         /   | |   \         /           |     /    |    /     /
%        /   /   \   \       /            |    /     |   /     /
%       /   /     \   \     /             |   /      |  /     /
% share user harvester group             user      group     /
%              ^    ^     ^                          ^      /
%             /      \    |                          |     /
%            /        \   |                          |    /
%          user        group                        user-'
%                      ^   ^
%                     /     \
%                    /       \
%                  user      user
%
% Members of groups, spaces, providers, handle_services, handles and harvesters are
% calculated bottom-up.
%
% Memberships of users, groups and spaces are calculated top-down.
%
% Groups and spaces must be processed top-down and bottom-up, as they hold the
% information about users' memberships and members of entities.
%
% After a new relation appears (or existing one is changed or deleted),
% the parent is marked bottom_up_dirty and the child is marked top_down_dirty.

%% This record defines a user and is handled as a database document
-record(od_user, {
    full_name = ?DEFAULT_FULL_NAME :: od_user:full_name(),
    username = undefined :: undefined | od_user:username(),
    % Decides if this user can login via login:password - this feature must be
    % enabled by an admin, by default regular users are only allowed to sign in
    % using their IdPs.
    basic_auth_enabled = false :: boolean(),
    password_hash = undefined :: undefined | basic_auth:password_hash(),
    emails = [] :: [od_user:email()],

    linked_accounts = [] :: [od_user:linked_account()],
    entitlements = [] :: od_user:entitlements(),

    active_sessions = [] :: [session:id()],

    % List of user's client tokens
    client_tokens = [] :: [binary()],
    % List of user's aliases for spaces
    space_aliases = #{} :: #{od_space:id() => binary()},

    % Privileges of this user in admin's OZ API
    oz_privileges = [] :: [privileges:oz_privilege()],
    eff_oz_privileges = [] :: [privileges:oz_privilege()],

    % Direct relations to other entities
    groups = [] :: entity_graph:relations(od_group:id()),
    spaces = [] :: entity_graph:relations(od_space:id()),
    handle_services = [] :: entity_graph:relations(od_handle_service:id()),
    handles = [] :: entity_graph:relations(od_handle:id()),
    harvesters = [] :: entity_graph:relations(od_harvester:id()),
    clusters = [] :: entity_graph:relations(od_cluster:id()),

    % Effective relations to other entities
    eff_groups = #{} :: entity_graph:eff_relations(od_group:id()),
    eff_spaces = #{} :: entity_graph:eff_relations(od_space:id()),
    eff_providers = #{} :: entity_graph:eff_relations(od_provider:id()),
    eff_handle_services = #{} :: entity_graph:eff_relations(od_handle_service:id()),
    eff_handles = #{} :: entity_graph:eff_relations(od_handle:id()),
    eff_harvesters = #{} :: entity_graph:eff_relations(od_harvester:id()),
    eff_clusters = #{} :: entity_graph:eff_relations(od_cluster:id()),

    creation_time = time_utils:system_time_seconds() :: entity_logic:creation_time(),

    % Marks that the record's effective relations are not up to date.
    top_down_dirty = true :: boolean()
}).

%% This record defines a group of users, it has: name, list of users that
%% belongs to it, list of spaces that are used by this group
-record(od_group, {
    name = <<"">> :: od_group:name(),
    type = ?DEFAULT_GROUP_TYPE :: od_group:type(),
    % if group is protected it cannot be deleted
    protected = false :: boolean(),

    % Privileges of this group in admin's OZ API
    oz_privileges = [] :: [privileges:oz_privilege()],
    eff_oz_privileges = [] :: [privileges:oz_privilege()],

    % Group graph related entities (direct and effective)
    parents = [] :: entity_graph:relations(od_group:id()),
    children = #{} :: entity_graph:relations_with_attrs(od_group:id(), [privileges:group_privilege()]),
    eff_parents = #{} :: entity_graph:eff_relations(od_group:id()),
    eff_children = #{} :: entity_graph:eff_relations_with_attrs(od_group:id(), [privileges:group_privilege()]),

    % Direct relations to other entities
    users = #{} :: entity_graph:relations_with_attrs(od_user:id(), [privileges:group_privilege()]),
    spaces = [] :: entity_graph:relations(od_space:id()),
    handle_services = [] :: entity_graph:relations(od_handle_service:id()),
    handles = [] :: entity_graph:relations(od_handle:id()),
    harvesters = [] :: entity_graph:relations(od_harvester:id()),
    clusters = [] :: entity_graph:relations(od_cluster:id()),

    % Effective relations to other entities
    eff_users = #{} :: entity_graph:eff_relations_with_attrs(od_user:id(), [privileges:group_privilege()]),
    eff_spaces = #{} :: entity_graph:eff_relations(od_space:id()),
    eff_providers = #{} :: entity_graph:eff_relations(od_provider:id()),
    eff_handle_services = #{} :: entity_graph:eff_relations(od_handle_service:id()),
    eff_handles = #{} :: entity_graph:eff_relations(od_handle:id()),
    eff_harvesters = #{} :: entity_graph:eff_relations(od_harvester:id()),
    eff_clusters = #{} :: entity_graph:eff_relations(od_cluster:id()),

    creation_time = time_utils:system_time_seconds() :: entity_logic:creation_time(),
    creator = undefined :: undefined | aai:subject(),

    % Marks that the record's effective relations are not up to date.
    % Groups' effective relations must be calculated top-down and bottom-up.
    top_down_dirty = true :: boolean(),
    bottom_up_dirty = true :: boolean()
}).

%% This record defines a space that can be used by users to store their files
-record(od_space, {
    name = <<"">> :: od_space:name(),

    % Direct relations to other entities
    users = #{} :: entity_graph:relations_with_attrs(od_user:id(), [privileges:space_privilege()]),
    groups = #{} :: entity_graph:relations_with_attrs(od_group:id(), [privileges:space_privilege()]),
    providers = #{} :: entity_graph:relations_with_attrs(od_provider:id(), Size :: pos_integer()),
    % All shares that belong to this space.
    shares = [] :: entity_graph:relations(od_share:id()),
    harvesters = [] :: entity_graph:relations(od_harvester:id()),

    % Effective relations to other entities
    eff_users = #{} :: entity_graph:eff_relations_with_attrs(od_user:id(), [privileges:space_privilege()]),
    eff_groups = #{} :: entity_graph:eff_relations_with_attrs(od_group:id(), [privileges:space_privilege()]),
    % Effective providers contain only direct providers, but this is needed to
    % track changes in spaces and propagate them top-down.
    eff_providers = #{} :: entity_graph:eff_relations(od_provider:id()),
    % Effective harvesters contain only direct harvesters, but this is needed to
    % track changes in spaces and propagate them bottom-up.
    eff_harvesters = #{} :: entity_graph:eff_relations(od_provider:id()),

    creation_time = time_utils:system_time_seconds() :: entity_logic:creation_time(),
    creator = undefined :: undefined | aai:subject(),

    % Marks that the record's effective relations are not up to date.
    % Groups' effective relations must be calculated top-down and bottom-up.
    top_down_dirty = true :: boolean(),
    bottom_up_dirty = true :: boolean()
}).

%% This record defines a file/directory public share
%% Shares do not take part in effective relations computation
%% (every share belongs to one space, so its effective relations are the same
%% as of the parent space).
-record(od_share, {
    name = <<"">> :: od_share:name(),
    public_url = undefined :: undefined | binary(),

    % Direct relations to other entities
    space = undefined :: undefined | od_space:id(),
    handle = undefined :: undefined | od_handle:id(),
    root_file = undefined :: undefined | binary(),

    creation_time = time_utils:system_time_seconds() :: entity_logic:creation_time(),
    creator = undefined :: undefined | aai:subject()
}).

%% This record defines a provider who supports spaces and can be reached via url
-record(od_provider, {
    name = <<"">> :: od_provider:name(),
    admin_email = undefined :: undefined | binary(),
    root_macaroon = undefined :: undefined | tokens:nonce(),

    subdomain_delegation = false :: boolean(),
    domain :: binary(),
    subdomain = undefined :: undefined | binary(),

    latitude = 0.0 :: float(),
    longitude = 0.0 :: float(),

    % Direct relations to other entities
    spaces = #{} :: entity_graph:relations_with_attrs(od_space:id(), Size :: pos_integer()),

    % Effective relations to other entities
    eff_users = #{} :: entity_graph:eff_relations(od_user:id()),
    eff_groups = #{} :: entity_graph:eff_relations(od_group:id()),
    eff_harvesters = #{} :: entity_graph:eff_relations(od_harvester:id()),

    creation_time = time_utils:system_time_seconds() :: entity_logic:creation_time(),

    % Marks that the record's effective relations are not up to date.
    bottom_up_dirty = true :: boolean()
}).

-record(od_handle_service, {
    name = <<"">> :: od_handle_service:name(),
    proxy_endpoint :: od_handle_service:proxy_endpoint() | undefined,
    service_properties = #{} :: od_handle_service:service_properties(),

    % Direct relations to other entities
    users = #{} :: entity_graph:relations_with_attrs(od_user:id(), [privileges:handle_service_privilege()]),
    groups = #{} :: entity_graph:relations_with_attrs(od_group:id(), [privileges:handle_service_privilege()]),
    handles = [] :: entity_graph:relations(od_handle:id()),

    % Effective relations to other entities
    eff_users = #{} :: entity_graph:eff_relations_with_attrs(od_user:id(), [privileges:handle_service_privilege()]),
    eff_groups = #{} :: entity_graph:eff_relations_with_attrs(od_group:id(), [privileges:handle_service_privilege()]),

    creation_time = time_utils:system_time_seconds() :: entity_logic:creation_time(),
    creator = undefined :: undefined | aai:subject(),

    % Marks that the record's effective relations are not up to date.
    bottom_up_dirty = true :: boolean()
}).

-record(od_handle, {
    public_handle :: od_handle:public_handle() | undefined,
    resource_type :: od_handle:resource_type() | undefined,
    metadata :: od_handle:metadata() | undefined,
    timestamp = od_handle:actual_timestamp() :: od_handle:timestamp(),

    % Direct relations to other entities
    resource_id :: od_handle:resource_id() | undefined,
    handle_service :: od_handle_service:id() | undefined,
    users = #{} :: entity_graph:relations_with_attrs(od_user:id(), [privileges:handle_privilege()]),
    groups = #{} :: entity_graph:relations_with_attrs(od_group:id(), [privileges:handle_privilege()]),

    % Effective relations to other entities
    eff_users = #{} :: entity_graph:eff_relations_with_attrs(od_user:id(), [privileges:handle_privilege()]),
    eff_groups = #{} :: entity_graph:eff_relations_with_attrs(od_group:id(), [privileges:handle_privilege()]),

    creation_time = time_utils:system_time_seconds() :: entity_logic:creation_time(),
    creator = undefined :: undefined | aai:subject(),

    % Marks that the record's effective relations are not up to date.
    bottom_up_dirty = true :: boolean()
}).

-record(od_harvester, {
    name = <<"">> :: od_harvester:name(),
    plugin :: od_harvester:plugin(),
    endpoint :: od_harvester:endpoint(),

    gui_plugin_config = #{} :: json_utils:json_term(),
    public = false :: boolean(),

    indices = #{} :: od_harvester:indices(),

    % Direct relations to other entities
    users = #{} :: entity_graph:relations_with_attrs(od_user:id(), [privileges:space_privilege()]),
    groups = #{} :: entity_graph:relations_with_attrs(od_group:id(), [privileges:space_privilege()]),
    spaces = [] :: entity_graph:relations(od_space:id()),

    % Effective relations to other entities
    eff_users = #{} :: entity_graph:eff_relations_with_attrs(od_user:id(), [privileges:space_privilege()]),
    eff_groups = #{} :: entity_graph:eff_relations_with_attrs(od_group:id(), [privileges:space_privilege()]),
    eff_providers = #{} :: entity_graph:eff_relations(od_provider:id()),

    creation_time = time_utils:system_time_seconds() :: entity_logic:creation_time(),
    creator = undefined :: undefined | aai:subject(),

    % Marks that the record's effective relations are not up to date.
    bottom_up_dirty = true :: boolean(),
    top_down_dirty = true :: boolean()
}).

%% This record defines a Onezone/Oneprovider cluster.
-record(od_cluster, {
    type = ?ONEZONE :: onedata:cluster_type(),

    % Version 18.02.* is default for legacy providers
    worker_version = {?DEFAULT_RELEASE_VERSION, ?DEFAULT_BUILD_VERSION, ?EMPTY_GUI_HASH} :: od_cluster:version_info(),
    onepanel_version = {?DEFAULT_RELEASE_VERSION, ?DEFAULT_BUILD_VERSION, ?EMPTY_GUI_HASH} :: od_cluster:version_info(),
    % If enabled, onepanel is served on port 443 by oneprovider/onezone (rather than 9443)
    onepanel_proxy = true :: boolean(),

    creation_time = time_utils:system_time_seconds() :: entity_logic:creation_time(),
    creator = undefined :: undefined | aai:subject(),

    % Direct relations to other entities
    users = #{} :: entity_graph:relations_with_attrs(od_user:id(), [privileges:cluster_privilege()]),
    groups = #{} :: entity_graph:relations_with_attrs(od_group:id(), [privileges:cluster_privilege()]),

    % Effective relations to other entities
    eff_users = #{} :: entity_graph:eff_relations_with_attrs(od_user:id(), [privileges:cluster_privilege()]),
    eff_groups = #{} :: entity_graph:eff_relations_with_attrs(od_group:id(), [privileges:cluster_privilege()]),

    % Marks that the record's effective relations are not up to date.
    bottom_up_dirty = true :: boolean()
}).

%%%===================================================================
%%% Records specific for onezone
%%%===================================================================

%% This record defines a GUI session
-record(session, {
    user_id :: od_user:id(),
    last_refresh = 0 :: non_neg_integer(),
    nonce = <<"">> :: binary(),
    previous_nonce = <<"">> :: binary()
}).

%% Singleton record that stores a global, shared token secret for temporary tokens.
-record(shared_token_secret, {
    secret :: tokens:secret()
}).

-record(dns_state, {
    subdomain_to_provider = #{} :: #{dns_state:subdomain() => od_provider:id()},
    provider_to_subdomain = #{} :: #{od_provider:id() => dns_state:subdomain()},
    provider_to_ips = #{} :: #{od_provider:id() => [inet:ip4_address()]},
    provider_to_txt_records = #{} :: #{
        od_provider:id() => [{binary(), binary(), integer() | undefined}]
    }
}).


% stores static content to be displayed in GUI
-record(gui_message, {
    % the default state is 'enabled' to encourage admin setting the message
    enabled = true :: boolean(),
    body = <<>> :: binary()
}).


-record(entity_graph_state, {
    refresh_in_progress = false :: boolean(),
    bottom_up_dirty = ordsets:new() :: entity_graph_state:dirty_queue(),
    top_down_dirty = ordsets:new() :: entity_graph_state:dirty_queue()
}).

%% Model that holds the last processed seq for Graph Sync server.
-record(gs_server_state, {
    seq = 1 :: couchbase_changes:seq()
}).

%% Stores information about active provider connection
-record(provider_connection, {
    connection_ref :: gs_server:conn_ref()
}).

%% Stores information about active user connections per session id
-record(user_connections, {
    connections = [] :: [gs_server:conn_ref()]
}).

% Token used to match together OIDC/SAML requests and responses and protect
% against replay attacks. It is correlated with some state, defining for example
% to which IdP the client was redirected.
-record(state_token, {
    timestamp = 0 :: integer(),  % In seconds since epoch
    state_info = #{} :: state_token:state_info()
}).

% @todo VFS-5524 deemed for deletion when invite tokens are reworked
%% This record defines a token that can be used by user to do something
-record(token, {
    secret :: undefined | binary(),
    resource :: undefined | atom(),
    resource_id :: undefined | binary(),
    issuer :: undefined | aai:subject(),
    % Locked token cannot be consumed.
    locked = false :: boolean()
}).

%% @todo VFS-5554 This record is deprecated, kept for backward compatibility
%% Records of this type store a macaroons secret used for authorizing users
-record(onedata_auth, {
    secret :: undefined | binary(),
    user_id :: undefined | binary()
}).

%% @todo VFS-5554 This record is deprecated, kept for backward compatibility
%% Stores information about authorization correlated with a macaroon.
%% Record id serves as macaroon identifier.
-record(macaroon_auth, {
    secret :: tokens:secret(),
    type :: authorization,
    issuer :: aai:subject()
}).

-endif.
