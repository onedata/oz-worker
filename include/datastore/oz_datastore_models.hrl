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
-include_lib("cluster_worker/include/modules/datastore/datastore_models.hrl").


%%%===================================================================
%%% DB records definitions
%%%===================================================================

%% This record must be defined here as od_user depends on it.
%% This record defines user's account info
%% received from an SAML / openid / oauth provider.
-record(linked_account, {
    idp = undefined :: atom(),
    subject_id = <<"">> :: binary(),
    login = <<"">> :: binary(),
    name = <<"">> :: binary(),
    email_list = [] :: [binary()],
    % A list of strings that do not change for each given group so that
    % a diff can be computed every time a user logs in. Must be normalized
    % according to specification in idp_group_mapping.
    groups = [] :: [idp_group_mapping:membership_spec()]
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
%       provider
%           ^
%           |
%           |
%         space    handle_service     handle
%        ^ ^  ^       ^         ^       ^   ^
%       /  |   \     /          |      /    |
%      /   |    \   /           |     /     |
%  share  user   group           user     group
%                  ^                        ^
%                  |                        |
%                  |                        |
%                group                     user
%                ^   ^
%               /     \
%              /       \
%            user     user
%
% Members of groups, spaces, providers, handle_services and handles are
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
    name = <<"">> :: od_user:name(),
    alias = undefined :: od_user:alias(),
    email_list = [] :: [binary()],
    % Decides if this user can login via login:password, only users created in
    % onepanel are currently allowed to do that.
    basic_auth_enabled = false :: boolean(),
    linked_accounts = [] :: [od_user:linked_account()],

    default_space = undefined :: undefined | binary(),
    default_provider = undefined :: undefined | binary(),

    % List of user's client tokens
    client_tokens = [] :: [binary()],
    % List of user's aliases for spaces
    space_aliases = #{} :: #{od_space:id() => binary()},

    % Privileges of this user in admin's OZ API
    oz_privileges = [] :: [privileges:oz_privilege()],
    eff_oz_privileges = [] :: [privileges:oz_privilege()],

    % Direct relations to other entities
    groups = [] :: entity_graph:relation(od_group:id()),
    spaces = [] :: entity_graph:relation(od_space:id()),
    handle_services = [] :: entity_graph:relation(od_handle_service:id()),
    handles = [] :: entity_graph:relation(od_handle:id()),

    % Effective relations to other entities
    eff_groups = #{} :: entity_graph:eff_relation(od_group:id()),
    eff_spaces = #{} :: entity_graph:eff_relation(od_space:id()),
    eff_providers = #{} :: entity_graph:eff_relation(od_provider:id()),
    eff_handle_services = #{} :: entity_graph:eff_relation(od_handle_service:id()),
    eff_handles = #{} :: entity_graph:eff_relation(od_handle:id()),

    % Marks that the record's effective relations are not up to date.
    top_down_dirty = true :: boolean()
}).

%% This record defines a group of users, it has: name, list of users that
%% belongs to it, list of spaces that are used by this group
-record(od_group, {
    name = <<"">> :: od_group:name(),
    type = role :: od_group:type(),

    % Privileges of this group in admin's OZ API
    oz_privileges = [] :: [privileges:oz_privilege()],
    eff_oz_privileges = [] :: [privileges:oz_privilege()],

    % Group graph related entities (direct and effective)
    parents = [] :: entity_graph:relation(od_group:id()),
    children = #{} :: entity_graph:relation_with_attrs(od_group:id(), [privileges:group_privilege()]),
    eff_parents = #{} :: entity_graph:eff_relation(od_group:id()),
    eff_children = #{} :: entity_graph:eff_relation_with_attrs(od_group:id(), [privileges:group_privilege()]),

    % Direct relations to other entities
    users = #{} :: entity_graph:relation_with_attrs(od_user:id(), [privileges:group_privilege()]),
    spaces = [] :: entity_graph:relation(od_space:id()),
    handle_services = [] :: entity_graph:relation(od_handle_service:id()),
    handles = [] :: entity_graph:relation(od_handle:id()),

    % Effective relations to other entities
    eff_users = #{} :: entity_graph:eff_relation_with_attrs(od_user:id(), [privileges:group_privilege()]),
    eff_spaces = #{} :: entity_graph:eff_relation(od_space:id()),
    eff_providers = #{} :: entity_graph:eff_relation(od_provider:id()),
    eff_handle_services = #{} :: entity_graph:eff_relation(od_handle_service:id()),
    eff_handles = #{} :: entity_graph:eff_relation(od_handle:id()),

    % Marks that the record's effective relations are not up to date.
    % Groups' effective relations must be calculated top-down and bottom-up.
    top_down_dirty = true :: boolean(),
    bottom_up_dirty = true :: boolean()
}).

%% This record defines a space that can be used by users to store their files
-record(od_space, {
    name = <<"">> :: od_space:name(),

    % Direct relations to other entities
    users = #{} :: entity_graph:relation_with_attrs(od_user:id(), [privileges:space_privilege()]),
    groups = #{} :: entity_graph:relation_with_attrs(od_group:id(), [privileges:space_privilege()]),
    providers = #{} :: entity_graph:relation_with_attrs(od_provider:id(), Size :: pos_integer()),
    % All shares that belong to this space.
    shares = [] :: entity_graph:relation(od_share:id()),

    % Effective relations to other entities
    eff_users = #{} :: entity_graph:eff_relation_with_attrs(od_user:id(), [privileges:space_privilege()]),
    eff_groups = #{} :: entity_graph:eff_relation_with_attrs(od_group:id(), [privileges:space_privilege()]),
    % Effective providers contain only direct providers, but this is needed to
    % track changes in spaces and propagate them top-down.
    eff_providers = #{} :: entity_graph:eff_relation(od_provider:id()),

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
    root_file = undefined :: undefined | binary()
}).

%% This record defines a provider who supports spaces and can be reached via url
-record(od_provider, {
    name = <<"">> :: od_provider:name(),
    admin_email :: undefined | binary(),
    root_macaroon :: undefined | macaroon_auth:id(),
    subdomain_delegation = false :: boolean(),
    domain :: binary(),
    subdomain = undefined :: undefined | binary(),

    latitude = 0.0 :: float(),
    longitude = 0.0 :: float(),

    % Direct relations to other entities
    spaces = #{} :: entity_graph:relation_with_attrs(od_space:id(), Size :: pos_integer()),

    % Effective relations to other entities
    eff_users = #{} :: entity_graph:eff_relation(od_user:id()),
    eff_groups = #{} :: entity_graph:eff_relation(od_group:id()),

    % Marks that the record's effective relations are not up to date.
    bottom_up_dirty = true :: boolean()
}).

-record(od_handle_service, {
    name = <<"">> :: od_handle_service:name(),
    proxy_endpoint :: od_handle_service:proxy_endpoint() | undefined,
    service_properties = #{} :: od_handle_service:service_properties(),

    % Direct relations to other entities
    users = #{} :: entity_graph:relation_with_attrs(od_user:id(), [privileges:handle_service_privilege()]),
    groups = #{} :: entity_graph:relation_with_attrs(od_group:id(), [privileges:handle_service_privilege()]),
    handles = [] :: entity_graph:relation(od_handle:id()),

    % Effective relations to other entities
    eff_users = #{} :: entity_graph:eff_relation_with_attrs(od_user:id(), [privileges:handle_service_privilege()]),
    eff_groups = #{} :: entity_graph:eff_relation_with_attrs(od_group:id(), [privileges:handle_service_privilege()]),

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
    users = #{} :: entity_graph:relation_with_attrs(od_user:id(), [privileges:handle_privilege()]),
    groups = #{} :: entity_graph:relation_with_attrs(od_group:id(), [privileges:handle_privilege()]),

    % Effective relations to other entities
    eff_users = #{} :: entity_graph:eff_relation_with_attrs(od_user:id(), [privileges:handle_privilege()]),
    eff_groups = #{} :: entity_graph:eff_relation_with_attrs(od_group:id(), [privileges:handle_privilege()]),

    % Marks that the record's effective relations are not up to date.
    bottom_up_dirty = true :: boolean()
}).

%%%===================================================================
%%% Records specific for onezone
%%%===================================================================

%% This record defines a GUI session
-record(session, {
    user_id :: od_user:id(),
    accessed = 0 :: non_neg_integer()
}).

%% This record defines a token that can be used by user to do something
-record(token, {
    secret :: undefined | binary(),
    resource :: undefined | atom(),
    resource_id :: undefined | binary(),
    issuer :: undefined | entity_logic:client()
}).

%% Records of this type store a macaroons secret used for authorizing users
-record(onedata_auth, {
    secret :: undefined | binary(),
    user_id :: undefined | binary()
}).

%% Stores information about authorization correlated with a macaroon.
%% Record id serves as macaroon identifier.
-record(macaroon_auth, {
    secret :: macaroon_auth:secret(),
    type :: macaroon_auth:type(),
    issuer :: macaroon_auth:issuer()
}).


-record(dns_state, {
    subdomain_to_provider = #{} :: #{dns_state:subdomain() => od_provider:id()},
    provider_to_subdomain = #{} :: #{od_provider:id() => dns_state:subdomain()},
    provider_to_ips = #{} :: #{od_provider:id() => [inet:ipv4_address()]},
    provider_to_txt_records = #{} :: #{od_provider:id() =>
        [{binary(), binary(), integer() | undefined}]}
}).

-record(entity_graph_state, {
    bottom_up_dirty = [] :: [{Priority :: integer(), EntityType :: atom(), EntityId :: binary()}],
    top_down_dirty = [] :: [{Priority :: integer(), EntityType :: atom(), EntityId :: binary()}]
}).

%% Model that holds the last processed seq for Graph Sync server.
-record(gs_server_state, {
    seq = 1 :: couchbase_changes:seq()
}).

%% Stores information about active provider connection
-record(provider_connection, {
    connection_ref :: gs_server:conn_ref()
}).

% Token used to match together OIDC/SAML requests and responses and protect
% against replay attacks. It is correlated with some state, defining for example
% to which IdP the client was redirected.
-record(state_token, {
    timestamp = 0 :: integer(),  % In seconds since epoch
    state_info = #{} :: state_token:state_info()
}).

%% Record used to cache authorization via basic auth credentials
-record(basic_auth_cache, {
    expires :: non_neg_integer(),
    password_hash :: binary(),
    props :: maps:map()
}).

-endif.
