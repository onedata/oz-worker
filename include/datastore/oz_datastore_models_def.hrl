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

% String that will be put in front of uuid when a user does not have
% an alias set. Aliases are not allowed to start with this string.
-define(NO_ALIAS_UUID_PREFIX, "uuid_").

% Data types that hold different types of relations.
-type relation(EntityId) :: [EntityId].
-type relation_with_attrs(EntityId, Attributes) :: #{EntityId => Attributes}.
% Data types that hold different types of effective relations. {Model, Id} pairs
% hold pairs of {record type, record id} through which the effective
% relation exists (intermediaries).
% There may be multiple such pairs on the list. If the effective neighbour
% has a direct relation to the entity, it will be listed there (which means
% that it belongs "by itself").
-type eff_relation(EntityId) :: #{EntityId => [{Model :: atom(), Id :: binary()}]}.
-type eff_relation_with_attrs(EntityId, Attributes) :: #{EntityId => {Attributes, [{Model :: atom(), Id :: binary()}]}}.


%%%===================================================================
%%% DB records definitions
%%%===================================================================

%% This record must be defined here as od_user depends on it.
%% This record defines user's account info
%% received from an openid / oauth provider.
-record(oauth_account, {
    provider_id = undefined :: atom(),
    user_id = <<"">> :: binary(),
    login = <<"">> :: binary(),
    name = <<"">> :: binary(),
    email_list = [] :: [binary()]
}).

%%%===================================================================
%%% Records synchronized via subscriptions
%%%===================================================================

% Records starting with prefix od_ are special records that represent entities
% in the system and are synchronized to providers via subscriptions.
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
%        provider
%           ^
%           |
%           |
%         space    handle_service     handle
%         ^  ^        ^        ^       ^   ^
%         |   \      /         |      /    |
%         |    \    /          |     /     |
%        user    group          user      group
%                  ^                        ^
%                  |                        |
%                  |                        |
%                group                     user
%                ^   ^
%               /     \
%              /       \
%            user     user
%
% Members of providers, shares, handle_services and handles are
% calculated bottom-up.
%
% Memberships of users are calculated top-down.
%
% Groups and spaces must be processed top-down and bottom-up, as they hold the
% information about users' memberships and members of entities.
%
% After a new relation appears, the parent is marked bottom_up_dirty and the
% child is marked top_down_dirty.

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

    % Privileges of this user in admin's OZ API
    oz_privileges = [] :: [privileges:oz_privilege()],
    eff_oz_privileges = [] :: [privileges:oz_privilege()], % TODO currently always empty

    % Direct relations to other entities
    groups = [] :: relation(od_group:id()),
    spaces = [] :: relation(od_space:id()),
    handle_services = [] :: relation(od_handle_service:id()),
    handles = [] :: relation(od_handle:id()),

    % Effective relations to other entities
    eff_groups = #{} :: eff_relation(od_group:id()),
    eff_spaces = #{} :: eff_relation(od_space:id()), % TODO currently always empty
    eff_providers = #{} :: eff_relation(od_provider:id()), % TODO currently always empty
    eff_handle_services = #{} :: eff_relation(od_handle_service:id()), % TODO currently always empty
    eff_handles = #{} :: eff_relation(od_handle:id()), % TODO currently always empty

    % Marks that the record's effective relations are not up to date.
    top_down_dirty = true :: boolean()
}).

%% This record defines a group of users, it has: name, list of users that
%% belongs to it, list of spaces that are used by this group
-record(od_group, {
    name = <<"New Group">> :: binary(),
    type = role :: od_group:type(),

    % Privileges of this group in admin's OZ API
    oz_privileges = [] :: [privileges:oz_privilege()],
    eff_oz_privileges = [] :: [privileges:oz_privilege()], % TODO currently always empty

    % Group graph related entities (direct and effective)
    parents = [] :: relation(od_group:id()),
    children = #{} :: relation_with_attrs(od_group:id(), [privileges:group_privilege()]),
    eff_parents = #{} :: eff_relation(od_group:id()), % TODO currently always empty
    eff_children = #{} :: eff_relation_with_attrs(od_group:id(), [privileges:group_privilege()]), % TODO privileges list currently always empty

    % Direct relations to other entities
    users = #{} :: relation_with_attrs(od_user:id(), [privileges:group_privilege()]),
    spaces = [] :: relation(od_space:id()),
    handle_services = [] :: relation(od_handle_service:id()),
    handles = [] :: relation(od_handle:id()),

    % Effective relations to other entities
    eff_users = #{} :: eff_relation_with_attrs(od_user:id(), [privileges:group_privilege()]),
    eff_spaces = #{} :: eff_relation(od_space:id()), % TODO currently always empty
    eff_providers = #{} :: eff_relation(od_provider:id()), % TODO currently always empty
    eff_handle_services = #{} :: eff_relation(od_handle_service:id()), % TODO currently always empty
    eff_handles = #{} :: eff_relation(od_handle:id()), % TODO currently always empty

    % Marks that the record's effective relations are not up to date.
    % Groups' effective relations must be calculated top-down and bottom-up.
    top_down_dirty = true :: boolean(),
    bottom_up_dirty = true :: boolean()
}).

%% This record defines a space that can be used by users to store their files
-record(od_space, {
    name = <<"New Space">> :: binary(),

    % Direct relations to other entities
    providers = #{} :: relation_with_attrs(od_provider:id(), Size :: pos_integer()),
    users = #{} :: relation_with_attrs(od_user:id(), [privileges:space_privilege()]),
    groups = #{} :: relation_with_attrs(od_group:id(), [privileges:space_privilege()]),
    % All shares that belong to this space.
    shares = [] :: relation(od_share:id()),

    % Effective relations to other entities
    eff_users = #{} :: eff_relation_with_attrs(od_user:id(), [privileges:space_privilege()]), % TODO currently always empty
    eff_groups = #{} :: eff_relation_with_attrs(od_group:id(), [privileges:space_privilege()]), % TODO currently always empty

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
    name = undefined :: undefined | binary(),
    public_url = undefined :: undefined | binary(),

    % Direct relations to other entities
    space = undefined :: undefined | od_space:id(),
    handle = undefined :: undefined | od_handle:id(),
    root_file = undefined :: undefined | binary()
}).

%% This record defines a provider who supports spaces and can be reached via url
-record(od_provider, {
    name :: undefined | binary(),
    redirection_point :: undefined | binary(),
    urls :: undefined | [binary()],
    serial :: undefined | binary(),
    latitude :: undefined | float(),
    longitude :: undefined | float(),

    % Direct relations to other entities
    spaces = [] :: relation(od_space:id()),

    % Effective relations to other entities
    eff_users = #{} :: eff_relation(od_user:id()), % TODO currently always empty
    eff_groups = #{} :: eff_relation(od_group:id()), % TODO currently always empty

    % Marks that the record's effective relations are not up to date.
    bottom_up_dirty = true :: boolean()
}).

-record(od_handle_service, {
    name :: od_handle_service:name() | undefined,
    proxy_endpoint :: od_handle_service:proxy_endpoint() | undefined,
    service_properties = #{} :: od_handle_service:service_properties(),

    % Direct relations to other entities
    users = #{} :: relation_with_attrs(od_user:id(), [privileges:handle_service_privilege()]),
    groups = #{} :: relation_with_attrs(od_group:id(), [privileges:handle_service_privilege()]),

    % Effective relations to other entities
    eff_users = #{} :: eff_relation_with_attrs(od_user:id(), [privileges:handle_service_privilege()]), % TODO currently always empty
    eff_groups = #{} :: eff_relation_with_attrs(od_group:id(), [privileges:handle_service_privilege()]), % TODO currently always empty

    % Marks that the record's effective relations are not up to date.
    bottom_up_dirty = true :: boolean()
}).

-record(od_handle, {
    public_handle :: od_handle:public_handle() | undefined,
    resource_type :: od_handle:resource_type() | undefined,
    resource_id :: od_handle:resource_id() | undefined,
    metadata :: od_handle:metadata() | undefined,
    timestamp = od_handle:actual_timestamp() :: od_handle:timestamp(),

    % Direct relations to other entities
    handle_service :: od_handle_service:id() | undefined,
    users = #{} :: relation_with_attrs(od_user:id(), [privileges:handle_privilege()]),
    groups = #{} :: relation_with_attrs(od_group:id(), [privileges:handle_privilege()]),

    % Effective relations to other entities
    eff_users = #{} :: eff_relation_with_attrs(od_user:id(), [privileges:handle_privilege()]), % TODO currently always empty
    eff_groups = #{} :: eff_relation_with_attrs(od_group:id(), [privileges:handle_privilege()]), % TODO currently always empty

    % Marks that the record's effective relations are not up to date.
    bottom_up_dirty = true :: boolean()
}).

%%%===================================================================
%%% Records specific for onezone
%%%===================================================================

%% This record defines a GUI session
-record(session, {
    user_id = undefined :: od_user:id() | undefined,
    memory = #{} :: session:memory(),
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

%% Stores CA dedicated node
%% todo: implement distributed CA properly (connected with VFS-1499)
-record(ozpca_state, {
    dedicated_node :: undefined | {ok, node()} | {error, Reason :: term()}
}).

-record(entity_graph_state, {
    bottom_up_dirty = [] :: [{Priority :: integer(), Model :: atom(), Id :: binary()}],
    top_down_dirty = [] :: [{Priority :: integer(), Model :: atom(), Id :: binary()}]
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
