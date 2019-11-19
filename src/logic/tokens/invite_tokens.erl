%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles consuming invite tokens and checking the rights to invite.
%%% @end
%%%-------------------------------------------------------------------
-module(invite_tokens).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/logging.hrl").

-type consume_fun() :: fun((gri:entity_id(), token_metadata:invite_privileges()) -> entity_logic:create_result()).

-export([consume/4]).
-export([ensure_valid_invitation/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Unified procedure to be used from logic plugin modules when consuming an
%% invite token. The procedure checks the token issuer's authorization to invite
%% (it could have changed since the token was issued, e.g. if a user has lost
%% certain privileges).
%%
%% NOTE: The token consumer's authorization to consume the token is NOT checked,
%% it must be done in the calling logic plugin module.
%% @end
%%--------------------------------------------------------------------
-spec consume(aai:auth(), tokens:token(), tokens:invite_token_type(), consume_fun()) ->
    entity_logic:create_result() | errors:error().
consume(ConsumerAuth, Token, ExpectedType, ConsumeFun) ->
    PeerIp = ConsumerAuth#auth.peer_ip,
    ConsumerAudience = aai:auth_to_audience(ConsumerAuth),

    case token_auth:verify_invite_token(Token, ExpectedType, PeerIp, ConsumerAudience) of
        {ok, _} -> ok;
        {error, _} = Err1 -> throw(Err1)
    end,

    #token{type = ?INVITE_TOKEN(InviteTokenType, _)} = Token,
    ConsumerSubject = ConsumerAuth#auth.subject,
    is_valid_consumer(InviteTokenType, ConsumerSubject) orelse
        throw(?ERROR_INVITE_TOKEN_CONSUMER_INVALID(ConsumerSubject)),

    consume_internal(Token, ConsumeFun).


%%--------------------------------------------------------------------
%% @doc
%% Validates if certain invitation can be issued by a subject. Checks if the
%% target entity id is valid, if the subject can issue invites and if the
%% subject can set privileges (given that custom privileges were requested).
%% @end
%%--------------------------------------------------------------------
-spec ensure_valid_invitation(aai:subject(), tokens:invite_token_type(), gri:entity_id(),
    token_metadata:privileges_profile()) -> true | no_return().
ensure_valid_invitation(Subject, InviteTokenType, EntityId, default_privileges) ->
    is_valid_target_id(InviteTokenType, EntityId) orelse
        throw(?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(EntityId)),
    is_authorized_to_invite(Subject, InviteTokenType, EntityId) orelse
        throw(?ERROR_INVITE_TOKEN_CREATOR_NOT_AUTHORIZED);
ensure_valid_invitation(Subject, InviteTokenType, EntityId, custom_privileges) ->
    ensure_valid_invitation(Subject, InviteTokenType, EntityId, default_privileges),
    is_authorized_to_set_privileges(Subject, InviteTokenType, EntityId) orelse
        throw(?ERROR_INVITE_TOKEN_CREATOR_NOT_AUTHORIZED).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec is_valid_consumer(tokens:invite_token_type(), aai:subject()) -> boolean().
is_valid_consumer(?USER_JOIN_GROUP, ?SUB(user, _)) -> true;
is_valid_consumer(?GROUP_JOIN_GROUP, ?SUB(user, _)) -> true;
is_valid_consumer(?USER_JOIN_SPACE, ?SUB(user, _)) -> true;
is_valid_consumer(?GROUP_JOIN_SPACE, ?SUB(user, _)) -> true;
is_valid_consumer(?SUPPORT_SPACE, ?SUB(?ONEPROVIDER, _)) -> true;
% Anyone (including nobody) is allowed to register a Oneprovider
is_valid_consumer(?REGISTER_ONEPROVIDER, _) -> true;
is_valid_consumer(?USER_JOIN_CLUSTER, ?SUB(user, _)) -> true;
is_valid_consumer(?GROUP_JOIN_CLUSTER, ?SUB(user, _)) -> true;
is_valid_consumer(?USER_JOIN_HARVESTER, ?SUB(user, _)) -> true;
is_valid_consumer(?GROUP_JOIN_HARVESTER, ?SUB(user, _)) -> true;
is_valid_consumer(?SPACE_JOIN_HARVESTER, ?SUB(user, _)) -> true;
is_valid_consumer(_, _) -> false.


%% @private
-spec is_valid_target_id(tokens:invite_token_type(), gri:entity_id()) -> boolean().
is_valid_target_id(?USER_JOIN_GROUP, GroupId) -> group_logic:exists(GroupId);
is_valid_target_id(?GROUP_JOIN_GROUP, GroupId) -> group_logic:exists(GroupId);
is_valid_target_id(?USER_JOIN_SPACE, SpaceId) -> space_logic:exists(SpaceId);
is_valid_target_id(?GROUP_JOIN_SPACE, SpaceId) -> space_logic:exists(SpaceId);
is_valid_target_id(?SUPPORT_SPACE, SpaceId) -> space_logic:exists(SpaceId);
is_valid_target_id(?REGISTER_ONEPROVIDER, AdminUserId) -> user_logic:exists(AdminUserId);
is_valid_target_id(?USER_JOIN_CLUSTER, ClusterId) -> cluster_logic:exists(ClusterId);
is_valid_target_id(?GROUP_JOIN_CLUSTER, ClusterId) -> cluster_logic:exists(ClusterId);
is_valid_target_id(?USER_JOIN_HARVESTER, HarvesterId) -> harvester_logic:exists(HarvesterId);
is_valid_target_id(?GROUP_JOIN_HARVESTER, HarvesterId) -> harvester_logic:exists(HarvesterId);
is_valid_target_id(?SPACE_JOIN_HARVESTER, HarvesterId) -> harvester_logic:exists(HarvesterId);
is_valid_target_id(_, _) -> false.


%% @private
-spec consume_internal(tokens:token(), consume_fun()) ->
    entity_logic:create_result() | errors:error().
consume_internal(#token{persistent = false} = Token, ConsumeFun) ->
    #token{subject = Subject, type = ?INVITE_TOKEN(InviteTokenType, EntityId)} = Token,
    % Temporary tokens cannot carry any privileges
    ensure_valid_invitation(Subject, InviteTokenType, EntityId, default_privileges),
    ConsumeFun(EntityId, token_metadata:default_invite_privileges(InviteTokenType));

consume_internal(#token{persistent = true} = Token, ConsumeFun) ->
    % Named tokens must be consumed in a critical section to avoid
    % race conditions with multi-use tokens.
    critical_section:run({invite_token, Token#token.id}, fun() ->
        consume_named_unsafe(Token, ConsumeFun)
    end).


%% @private
-spec consume_named_unsafe(tokens:token(), consume_fun()) ->
    entity_logic:create_result() | errors:error().
consume_named_unsafe(Token, ConsumeFun) ->
    #token{
        id = TokenId,
        subject = Subject,
        type = ?INVITE_TOKEN(InviteTokenType, EntityId)
    } = Token,
    {ok, #document{value = #od_token{metadata = Metadata}}} = od_token:get(TokenId),

    token_metadata:is_usage_limit_reached(Metadata) andalso
        throw(?ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED),

    {Profile, Privileges} = token_metadata:inspect_carried_privileges(InviteTokenType, Metadata),
    ensure_valid_invitation(Subject, InviteTokenType, EntityId, Profile),

    % The ConsumeFun can throw, in which case the usage count will not be updated
    case ConsumeFun(EntityId, Privileges) of
        {error, _} = Error ->
            Error;
        Result ->
            od_token:update(TokenId, fun(NamedToken = #od_token{metadata = OldMetadata}) ->
                {ok, NamedToken#od_token{
                    metadata = token_metadata:increment_usage_count(OldMetadata)
                }}
            end),
            Result
    end.


%% @private
-spec is_authorized_to_invite(aai:subject(), tokens:invite_token_type(), gri:entity_id()) ->
    boolean() | no_return().
is_authorized_to_invite(Subject, InviteTokenType, EntityId) ->
    has_privileges_to_invite(Subject, InviteTokenType, EntityId) orelse
        has_admin_privileges_to_invite(Subject, InviteTokenType).


%% @private
-spec is_authorized_to_set_privileges(aai:subject(), tokens:invite_token_type(), gri:entity_id()) ->
    boolean() | no_return().
is_authorized_to_set_privileges(Subject, InviteTokenType, EntityId) ->
    has_privileges_to_set_privileges(Subject, InviteTokenType, EntityId) orelse
        has_admin_privileges_to_set_privileges(Subject, InviteTokenType).


%% @private
-spec has_privileges_to_invite(aai:subject(), tokens:invite_token_type(), gri:entity_id()) ->
    boolean().
has_privileges_to_invite(?SUB(user, UserId), ?USER_JOIN_GROUP, GroupId) ->
    group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_USER);
has_privileges_to_invite(?SUB(user, UserId), ?GROUP_JOIN_GROUP, GroupId) ->
    group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_ADD_CHILD);

has_privileges_to_invite(?SUB(user, UserId), ?USER_JOIN_SPACE, SpaceId) ->
    space_logic:has_eff_privilege(SpaceId, UserId, ?SPACE_ADD_USER);
has_privileges_to_invite(?SUB(user, UserId), ?GROUP_JOIN_SPACE, SpaceId) ->
    space_logic:has_eff_privilege(SpaceId, UserId, ?SPACE_ADD_GROUP);
has_privileges_to_invite(?SUB(user, UserId), ?SUPPORT_SPACE, SpaceId) ->
    space_logic:has_eff_privilege(SpaceId, UserId, ?SPACE_ADD_PROVIDER);

has_privileges_to_invite(?SUB(user, UserId), ?REGISTER_ONEPROVIDER, UserId) ->
    % Issuing provider registration token for self.
    % In case of 'restricted' policy, the admin rights are required.
    open =:= oz_worker:get_env(provider_registration_policy, open);

has_privileges_to_invite(?SUB(user, UserId), ?USER_JOIN_CLUSTER, ClusterId) ->
    cluster_logic:has_eff_privilege(ClusterId, UserId, ?CLUSTER_ADD_USER);
has_privileges_to_invite(?SUB(user, UserId), ?GROUP_JOIN_CLUSTER, ClusterId) ->
    cluster_logic:has_eff_privilege(ClusterId, UserId, ?CLUSTER_ADD_GROUP);
% Providers are allowed to create invite tokens for clusters
has_privileges_to_invite(?SUB(?ONEPROVIDER, ProviderId), ?USER_JOIN_CLUSTER, ClusterId) ->
    cluster_logic:is_provider_cluster(ClusterId, ProviderId);
has_privileges_to_invite(?SUB(?ONEPROVIDER, ProviderId), ?GROUP_JOIN_CLUSTER, ClusterId) ->
    cluster_logic:is_provider_cluster(ClusterId, ProviderId);

has_privileges_to_invite(?SUB(user, UserId), ?USER_JOIN_HARVESTER, HarvesterId) ->
    harvester_logic:has_eff_privilege(HarvesterId, UserId, ?HARVESTER_ADD_USER);
has_privileges_to_invite(?SUB(user, UserId), ?GROUP_JOIN_HARVESTER, HarvesterId) ->
    harvester_logic:has_eff_privilege(HarvesterId, UserId, ?HARVESTER_ADD_GROUP);
has_privileges_to_invite(?SUB(user, UserId), ?SPACE_JOIN_HARVESTER, HarvesterId) ->
    harvester_logic:has_eff_privilege(HarvesterId, UserId, ?HARVESTER_ADD_SPACE);

has_privileges_to_invite(_, _, _) ->
    false.


%% @private
-spec has_privileges_to_set_privileges(aai:subject(), tokens:invite_token_type(), gri:entity_id()) ->
    boolean().
has_privileges_to_set_privileges(?SUB(user, UserId), ?USER_JOIN_GROUP, GroupId) ->
    group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_SET_PRIVILEGES);
has_privileges_to_set_privileges(?SUB(user, UserId), ?GROUP_JOIN_GROUP, GroupId) ->
    group_logic:has_eff_privilege(GroupId, UserId, ?GROUP_SET_PRIVILEGES);

has_privileges_to_set_privileges(?SUB(user, UserId), ?USER_JOIN_SPACE, SpaceId) ->
    space_logic:has_eff_privilege(SpaceId, UserId, ?SPACE_SET_PRIVILEGES);
has_privileges_to_set_privileges(?SUB(user, UserId), ?GROUP_JOIN_SPACE, SpaceId) ->
    space_logic:has_eff_privilege(SpaceId, UserId, ?SPACE_SET_PRIVILEGES);

has_privileges_to_set_privileges(?SUB(user, UserId), ?USER_JOIN_CLUSTER, ClusterId) ->
    cluster_logic:has_eff_privilege(ClusterId, UserId, ?CLUSTER_SET_PRIVILEGES);
has_privileges_to_set_privileges(?SUB(user, UserId), ?GROUP_JOIN_CLUSTER, ClusterId) ->
    cluster_logic:has_eff_privilege(ClusterId, UserId, ?CLUSTER_SET_PRIVILEGES);
% Providers are allowed to set any privileges for invited users / groups
has_privileges_to_set_privileges(?SUB(?ONEPROVIDER, ProviderId), ?USER_JOIN_CLUSTER, ClusterId) ->
    cluster_logic:is_provider_cluster(ClusterId, ProviderId);
has_privileges_to_set_privileges(?SUB(?ONEPROVIDER, ProviderId), ?GROUP_JOIN_CLUSTER, ClusterId) ->
    cluster_logic:is_provider_cluster(ClusterId, ProviderId);

has_privileges_to_set_privileges(?SUB(user, UserId), ?USER_JOIN_HARVESTER, HarvesterId) ->
    harvester_logic:has_eff_privilege(HarvesterId, UserId, ?HARVESTER_SET_PRIVILEGES);
has_privileges_to_set_privileges(?SUB(user, UserId), ?GROUP_JOIN_HARVESTER, HarvesterId) ->
    harvester_logic:has_eff_privilege(HarvesterId, UserId, ?HARVESTER_SET_PRIVILEGES);

has_privileges_to_set_privileges(_, _, _) ->
    false.


%% @private
-spec has_admin_privileges_to_invite(aai:subject(), tokens:invite_token_type()) -> boolean().
has_admin_privileges_to_invite(?SUB(user, UserId), ?USER_JOIN_GROUP) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_GROUPS_ADD_RELATIONSHIPS);
has_admin_privileges_to_invite(?SUB(user, UserId), ?GROUP_JOIN_GROUP) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_GROUPS_ADD_RELATIONSHIPS);

has_admin_privileges_to_invite(?SUB(user, UserId), ?USER_JOIN_SPACE) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_SPACES_ADD_RELATIONSHIPS);
has_admin_privileges_to_invite(?SUB(user, UserId), ?GROUP_JOIN_SPACE) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_SPACES_ADD_RELATIONSHIPS);
has_admin_privileges_to_invite(?SUB(user, UserId), ?SUPPORT_SPACE) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_SPACES_ADD_RELATIONSHIPS);

has_admin_privileges_to_invite(?SUB(user, UserId), ?REGISTER_ONEPROVIDER) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_PROVIDERS_INVITE);

has_admin_privileges_to_invite(?SUB(user, UserId), ?USER_JOIN_CLUSTER) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_CLUSTERS_ADD_RELATIONSHIPS);
has_admin_privileges_to_invite(?SUB(user, UserId), ?GROUP_JOIN_CLUSTER) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_CLUSTERS_ADD_RELATIONSHIPS);

has_admin_privileges_to_invite(?SUB(user, UserId), ?USER_JOIN_HARVESTER) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_HARVESTERS_ADD_RELATIONSHIPS);
has_admin_privileges_to_invite(?SUB(user, UserId), ?GROUP_JOIN_HARVESTER) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_HARVESTERS_ADD_RELATIONSHIPS);
has_admin_privileges_to_invite(?SUB(user, UserId), ?SPACE_JOIN_HARVESTER) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_HARVESTERS_ADD_RELATIONSHIPS);

has_admin_privileges_to_invite(_, _) ->
    false.


%% @private
-spec has_admin_privileges_to_set_privileges(aai:subject(), tokens:invite_token_type()) -> boolean().
has_admin_privileges_to_set_privileges(?SUB(user, UserId), ?USER_JOIN_GROUP) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_GROUPS_SET_PRIVILEGES);
has_admin_privileges_to_set_privileges(?SUB(user, UserId), ?GROUP_JOIN_GROUP) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_GROUPS_SET_PRIVILEGES);

has_admin_privileges_to_set_privileges(?SUB(user, UserId), ?USER_JOIN_SPACE) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_SPACES_SET_PRIVILEGES);
has_admin_privileges_to_set_privileges(?SUB(user, UserId), ?GROUP_JOIN_SPACE) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_SPACES_SET_PRIVILEGES);

has_admin_privileges_to_set_privileges(?SUB(user, UserId), ?USER_JOIN_CLUSTER) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_CLUSTERS_SET_PRIVILEGES);
has_admin_privileges_to_set_privileges(?SUB(user, UserId), ?GROUP_JOIN_CLUSTER) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_CLUSTERS_SET_PRIVILEGES);

has_admin_privileges_to_set_privileges(?SUB(user, UserId), ?USER_JOIN_HARVESTER) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_HARVESTERS_SET_PRIVILEGES);
has_admin_privileges_to_set_privileges(?SUB(user, UserId), ?GROUP_JOIN_HARVESTER) ->
    user_logic:has_eff_oz_privilege(UserId, ?OZ_HARVESTERS_SET_PRIVILEGES);

has_admin_privileges_to_set_privileges(_, _) ->
    false.
