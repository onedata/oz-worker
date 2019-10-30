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

% Privileges carried by an invite token.
% 'undefined' when not applicable (e.g. space support token).
-type carried_privileges() :: undefined | [atom()].
-type consume_fun() :: fun((gri:entity_id(), carried_privileges()) -> entity_logic:create_result() | errors:error()).

-export([consume/4]).
-export([validate_invitation/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Unified procedure to be used from logic plugin modules when consuming an
%% invite token. The procedure checks the authorization to invite by the token
%% issuer (it could have changed since the token was issued, e.g. if a user
%% has lost certain privileges).
%%
%% NOTE: The authorization to consume the token is NOT checked, it must be done
%% in the calling logic plugin module.
%% @end
%%--------------------------------------------------------------------
-spec consume(aai:auth(), tokens:token(), tokens:invite_token_type(), consume_fun()) ->
    entity_logic:create_result() | errors:error().
consume(ConsumerAuth, Token, ExpectedType, ConsumeFun) ->
    PeerIp = ConsumerAuth#auth.peer_ip,
    ConsumerAudience = aai:auth_to_audience(ConsumerAuth),

    IssuerSubject = case token_auth:verify_invite_token(Token, ExpectedType, PeerIp, ConsumerAudience) of
        {ok, Auth} -> Auth#auth.subject;
        {error, _} = Err1 -> throw(Err1)
    end,

    #token{type = ?INVITE_TOKEN(InviteTokenType, EntityId)} = Token,
    ConsumerSubject = ConsumerAuth#auth.subject,

    can_consume(InviteTokenType, ConsumerSubject) orelse
        throw(?ERROR_INVITE_TOKEN_CONSUMER_INVALID(ConsumerSubject)),

    case validate_invitation(IssuerSubject, InviteTokenType, EntityId) of
        ok ->
            consume_internal(Token, ConsumeFun);
        {error, _} = Err2 ->
            Err2
    end.


%%--------------------------------------------------------------------
%% @doc
%% Validates if certain invitation can be issued by a subject. Apart from the
%% special case of provider registration token, it boils down to checking the
%% authorization of subject to issue an invite.
%% @end
%%--------------------------------------------------------------------
-spec validate_invitation(aai:subject(), tokens:invite_token_type(), gri:entity_id()) ->
    ok | errors:error().
validate_invitation(Subject, ?REGISTER_ONEPROVIDER, AdminUserId) ->
    case user_logic:exists(AdminUserId) of
        false ->
            ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"adminUserId">>);
        true ->
            ensure_authorized_to_invite(Subject, ?REGISTER_ONEPROVIDER, AdminUserId)
    end;
validate_invitation(Subject, InviteTokenType, EntityId) ->
    ensure_authorized_to_invite(Subject, InviteTokenType, EntityId).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec can_consume(tokens:invite_token_type(), aai:subject()) -> boolean().
can_consume(?USER_JOIN_GROUP, ?SUB(user, _)) -> true;
can_consume(?GROUP_JOIN_GROUP, ?SUB(user, _)) -> true;
can_consume(?USER_JOIN_SPACE, ?SUB(user, _)) -> true;
can_consume(?GROUP_JOIN_SPACE, ?SUB(user, _)) -> true;
can_consume(?SUPPORT_SPACE, ?SUB(?ONEPROVIDER, _)) -> true;
% Anyone (including nobody) is allowed to register a Oneprovider
can_consume(?REGISTER_ONEPROVIDER, _) -> true;
can_consume(?USER_JOIN_CLUSTER, ?SUB(user, _)) -> true;
can_consume(?GROUP_JOIN_CLUSTER, ?SUB(user, _)) -> true;
can_consume(?USER_JOIN_HARVESTER, ?SUB(user, _)) -> true;
can_consume(?GROUP_JOIN_HARVESTER, ?SUB(user, _)) -> true;
can_consume(?SPACE_JOIN_HARVESTER, ?SUB(user, _)) -> true;
can_consume(_, _) -> false.


%% @private
-spec ensure_authorized_to_invite(aai:subject(), tokens:invite_token_type(), gri:entity_id()) ->
    ok | errors:error().
ensure_authorized_to_invite(Subject, InviteTokenType, EntityId) ->
    Authorized = has_privileges_to_invite(Subject, InviteTokenType, EntityId) orelse
        has_admin_privileges_to_invite(Subject, InviteTokenType),
    case Authorized of
        true -> ok;
        false -> ?ERROR_INVITE_TOKEN_CREATOR_NOT_AUTHORIZED
    end.


%% @private
-spec consume_internal(tokens:token(), consume_fun()) ->
    entity_logic:create_result() | errors:error().
consume_internal(#token{persistent = false, type = ?INVITE_TOKEN(Type, EntityId)}, ConsumeFun) ->
    % Temporary tokens cannot carry any privileges
    ConsumeFun(EntityId, token_metadata:default_invite_privileges(Type));
consume_internal(#token{persistent = true, type = ?INVITE_TOKEN(_, EntityId), id = TokenId}, ConsumeFun) ->
    % Named tokens must be consumed in a critical section to avoid
    % race conditions with multi-use tokens.
    critical_section:run({invite_token, TokenId}, fun() ->
        consume_named_unsafe(TokenId, EntityId, ConsumeFun)
    end).


%% @private
-spec consume_named_unsafe(tokens:id(), gri:entity_id(), consume_fun()) ->
    entity_logic:create_result() | errors:error().
consume_named_unsafe(TokenId, EntityId, ConsumeFun) ->
    {ok, #document{value = #od_token{metadata = Metadata}}} = od_token:get(TokenId),

    token_metadata:is_usage_limit_reached(Metadata) andalso
        throw(?ERROR_INVITE_TOKEN_USAGE_LIMIT_REACHED),

    Privileges = token_metadata:carried_invite_privileges(Metadata),
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
