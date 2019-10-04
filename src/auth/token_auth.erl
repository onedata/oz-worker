%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module provides an API for verifying token's authorization.
%%% @end
%%%-------------------------------------------------------------------
-module(token_auth).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").

%% API
-export([check_token_auth/1, check_token_auth/3]).
-export([verify_access_token/3]).
-export([verify_identity/3]).
-export([verify_invite_token/4]).
-export([group_membership_checker/2]).
-export([is_audience_allowed/2]).

-define(NOW(), time_utils:cluster_time_seconds()).
-define(ONEZONE_DOMAIN, oz_worker:get_domain()).
-define(GUI_TOKEN_TTL, oz_worker:get_env(gui_token_ttl, 600)).
-define(AUTH_CTX(PeerIp, Audience), #auth_ctx{
    current_timestamp = ?NOW(),
    ip = PeerIp,
    % This onezone is the default audience if it was not specified (being the
    % service that consumes the token).
    audience = case Audience of
        undefined -> ?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID);
        _ -> Audience
    end,
    group_membership_checker = fun group_membership_checker/2
}).

-define(SUPPORTED_ACCESS_TOKEN_CAVEATS, [
    cv_time, cv_audience, cv_ip, cv_asn, cv_country, cv_region,
    cv_api, cv_data_space, cv_data_access, cv_data_path, cv_data_objectid
]).
-define(SUPPORTED_IDENTITY_TOKEN_CAVEATS, [
    cv_authorization_none | ?SUPPORTED_ACCESS_TOKEN_CAVEATS
]).
-define(SUPPORTED_INVITE_TOKEN_CAVEATS, [
    cv_time, cv_audience, cv_ip, cv_asn, cv_country, cv_region
]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Tries to a authorize a client by an access token - either a token issued
%% by Onezone or an external access token - originating from an Identity Provider.
%%      {true, Auth} - the client was authorized
%%      false - access token was not found
%%      errors:error() - provided access token is invalid
%% @end
%%--------------------------------------------------------------------
-spec check_token_auth(tokens:serialized() | tokens:token() | cowboy_req:req()) ->
    {true, aai:auth()} | false | errors:error().
check_token_auth(Req) when is_map(Req) ->
    case tokens:parse_access_token_header(Req) of
        undefined ->
            false;
        AccessToken ->
            case resolve_audience(Req) of
                {ok, Audience} ->
                    {PeerIp, _} = cowboy_req:peer(Req),
                    check_token_auth(AccessToken, PeerIp, Audience);
                {error, _} = Error ->
                    Error
            end
    end;
check_token_auth(Token) ->
    check_token_auth(Token, undefined, undefined).

-spec check_token_auth(tokens:serialized() | tokens:token(),
    undefined | ip_utils:ip(), undefined | aai:audience()) ->
    {true, aai:auth()} | errors:error().
check_token_auth(Serialized, PeerIp, Audience) when is_binary(Serialized) ->
    case tokens:deserialize(Serialized) of
        {ok, Token} ->
            check_token_auth(Token, PeerIp, Audience);
        ?ERROR_BAD_TOKEN ->
            case openid_protocol:authorize_by_idp_access_token(Serialized) of
                {true, {IdP, Attributes}} ->
                    LinkedAccount = attribute_mapping:map_attributes(IdP, Attributes),
                    {ok, #document{key = UserId}} = linked_accounts:acquire_user(LinkedAccount),
                    {true, ?USER(UserId)};
                {error, _} = Error ->
                    Error;
                false ->
                    ?ERROR_BAD_TOKEN
            end
    end;
check_token_auth(Token, PeerIp, Audience) ->
    case verify_access_token(Token, PeerIp, Audience) of
        {ok, Auth} -> {true, Auth};
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Verifies an access token, and if it's valid, returns the auth carried by that
%% token (auth object).
%% @end
%%--------------------------------------------------------------------
-spec verify_access_token(tokens:token(), undefined | ip_utils:ip(),
    undefined | aai:audience()) -> {ok, aai:auth()} | errors:error().
verify_access_token(Token, PeerIp, Audience) ->
    verify_access_token(Token, PeerIp, Audience, ?SUPPORTED_ACCESS_TOKEN_CAVEATS).

-spec verify_access_token(tokens:token(), undefined | ip_utils:ip(),
    undefined | aai:audience(), [caveats:type()]) -> {ok, aai:auth()} | errors:error().
verify_access_token(Token, PeerIp, Audience, SupportedCaveats) ->
    case Token#token.type of
        ?ACCESS_TOKEN ->
            verify_token_auth(Token, ?AUTH_CTX(PeerIp, Audience), SupportedCaveats);
        ?GUI_ACCESS_TOKEN(_) ->
            verify_token_auth(Token, ?AUTH_CTX(PeerIp, Audience), SupportedCaveats);
        _ ->
            ?ERROR_NOT_AN_ACCESS_TOKEN
    end.


%%--------------------------------------------------------------------
%% @doc
%% Verifies the identity carried by a token - verifies the token itself and
%% returns the token's subject. In contrast to access tokens, supports the
%% cv_authorization_none caveat (which nullifies the auth carried by the token,
%% but still allows to check the subject).
%% @end
%%--------------------------------------------------------------------
-spec verify_identity(tokens:token(), undefined | ip_utils:ip(),
    undefined | aai:audience()) -> {ok, aai:subject()} | errors:error().
verify_identity(Token, PeerIp, Audience) ->
    case verify_access_token(Token, PeerIp, Audience, ?SUPPORTED_IDENTITY_TOKEN_CAVEATS) of
        {error, _} = Error -> Error;
        {ok, #auth{subject = Subject}} -> {ok, Subject}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Verifies an invite token and checks against given expected invite token type,
%% and if it's valid, returns the auth carried by that token (auth object).
%% @end
%%--------------------------------------------------------------------
-spec verify_invite_token(tokens:token(), tokens:invite_token_type(),
    undefined | ip_utils:ip(), undefined | aai:audience()) ->
    {ok, aai:auth()} | errors:error().
verify_invite_token(Token, ExpectedType, PeerIp, Audience) ->
    case Token#token.type of
        ?INVITE_TOKEN(ExpectedType, _) ->
            verify_token_auth(Token, ?AUTH_CTX(PeerIp, Audience), ?SUPPORTED_INVITE_TOKEN_CAVEATS);
        _ ->
            ?ERROR_NOT_AN_INVITE_TOKEN(ExpectedType)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Used as callback during caveat verification (provided in #auth_ctx{}).
%% @end
%%--------------------------------------------------------------------
-spec group_membership_checker(aai:audience(), od_group:id()) -> boolean().
group_membership_checker(?AUD(user, UserId), GroupId) ->
    group_logic:has_eff_user(GroupId, UserId);
group_membership_checker(_, _) ->
    false.


%% @private
-spec is_audience_allowed(aai:subject(), aai:audience()) -> boolean().
is_audience_allowed(_Subject, ?AUD(user, _)) ->
    true; % User / provider can grant his authorization to any user
is_audience_allowed(_Subject, ?AUD(group, _)) ->
    true; % User / provider can grant his authorization to any group
is_audience_allowed(_Subject, ?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)) ->
    true; % Any user / provider can generate a token for Onezone
is_audience_allowed(?SUB(user, UserId), ?AUD(?OZ_PANEL, ?ONEZONE_CLUSTER_ID)) ->
    cluster_logic:has_eff_user(?ONEZONE_CLUSTER_ID, UserId);
is_audience_allowed(?SUB(user, UserId), ?AUD(?OP_WORKER, ProviderId)) ->
    provider_logic:has_eff_user(ProviderId, UserId);
is_audience_allowed(?SUB(?ONEPROVIDER, _), ?AUD(?OP_WORKER, _)) ->
    true; % Providers can grant their authorization to others (e.g. for identity check)
is_audience_allowed(?SUB(user, UserId), ?AUD(?OP_PANEL, ProviderId)) ->
    cluster_logic:has_eff_user(ProviderId, UserId);
is_audience_allowed(_, _) ->
    false.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec resolve_audience(cowboy_req:req()) -> {ok, undefined | aai:audience()} | errors:error().
resolve_audience(Req) ->
    case tokens:parse_audience_token_header(Req) of
        undefined ->
            {ok, undefined};
        SerializedAudienceToken ->
            case tokens:deserialize(SerializedAudienceToken) of
                {error, _} = Error ->
                    ?ERROR_BAD_AUDIENCE_TOKEN(Error);
                {ok, AudienceToken} ->
                    {PeerIp, _} = cowboy_req:peer(Req),
                    resolve_audience(AudienceToken, PeerIp)
            end
    end.

-spec resolve_audience(tokens:token(), undefined | ip_utils:ip()) ->
    {ok, undefined | aai:audience()} | errors:error().
resolve_audience(Token, PeerIp) ->
    case verify_access_token(Token, PeerIp, undefined) of
        {ok, Auth} -> {ok, aai:auth_to_audience(Auth)};
        {error, _} = Error -> ?ERROR_BAD_AUDIENCE_TOKEN(Error)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Verifies a token according to the list of supported caveats and returns
%% the auth that it carries.
%% @end
%%--------------------------------------------------------------------
-spec verify_token_auth(tokens:token(), aai:auth_ctx(), [caveats:type()]) ->
    {ok, aai:auth()} | errors:error().
verify_token_auth(Token = #token{persistent = false, subject = Subject}, AuthCtx, SupportedCaveats) ->
    % This check is not required in case of named tokens, which are deleted
    % alongside the user / provider.
    case subject_exists(Subject) of
        false -> ?ERROR_TOKEN_INVALID;
        true -> verify_token_auth(Token, AuthCtx, SupportedCaveats, shared_token_secret:get())
    end;
verify_token_auth(Token = #token{persistent = true}, AuthCtx, SupportedCaveats) ->
    case od_token:get(Token#token.nonce) of
        {ok, #document{value = #od_token{revoked = true}}} ->
            ?ERROR_TOKEN_REVOKED;
        {ok, #document{value = #od_token{secret = Secret, subject = ?SUB(SubType, _, SubId) = Subject}}} ->
            case Token of
                #token{version = 1} ->
                    % Legacy tokens do not include the subject - inject the one
                    % stored in named token record
                    verify_token_auth(Token#token{subject = Subject}, AuthCtx, SupportedCaveats, Secret);
                #token{subject = ?SUB(SubType, _, SubId)} ->
                    % Double check that the token includes the same subject as
                    % the one stored in named token record (ignore subtype)
                    verify_token_auth(Token, AuthCtx, SupportedCaveats, Secret);
                _ ->
                    % The subjects are different - the token is invalid
                    ?ERROR_TOKEN_INVALID
            end;
        _ ->
            ?ERROR_TOKEN_INVALID
    end.

%% @private
-spec verify_token_auth(tokens:token(), aai:auth_ctx(), [caveats:type()], tokens:secret()) ->
    {ok, aai:auth()} | errors:error().
verify_token_auth(Token = #token{type = TokenType}, AuthCtx, SupportedCaveats, Secret) ->
    case tokens:verify(Token, Secret, AuthCtx, SupportedCaveats) of
        {ok, Auth} ->
            case check_against_token_type(TokenType, Auth, AuthCtx#auth_ctx.audience) of
                ok -> {ok, Auth};
                {error, _} = Err1 -> Err1
            end;
        {error, _} = Err2 ->
            Err2
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs additional checks that are dependant on token type.
%% @end
%%--------------------------------------------------------------------
-spec check_against_token_type(tokens:type(), aai:auth(), aai:audience()) -> ok | errors:error().
check_against_token_type(?ACCESS_TOKEN, #auth{subject = Subject}, Audience) ->
    case is_audience_allowed(Subject, Audience) of
        true -> ok;
        false -> ?ERROR_TOKEN_AUDIENCE_FORBIDDEN(Audience)
    end;
check_against_token_type(?GUI_ACCESS_TOKEN(SessionId), ?USER(UserId), Audience) ->
    % Only user auth is allowed in gui access tokens
    case {session:exists(SessionId), is_audience_allowed(?SUB(user, UserId), Audience)} of
        {{ok, true}, true} -> ok;
        {{ok, false}, _} -> ?ERROR_TOKEN_SESSION_INVALID;
        {_, false} -> ?ERROR_TOKEN_AUDIENCE_FORBIDDEN(Audience)
    end;
% Audience in invite tokens is always the subject consuming the token (making the request)
check_against_token_type(?INVITE_TOKEN(TokenType, _), ?USER(_), Audience) ->
    case {TokenType, Audience} of
        % Only provider audience is allowed in space support tokens
        {?SPACE_SUPPORT_TOKEN, ?AUD(?OP_WORKER, _)} -> ok;
        %% @TODO VFS-5727 support for registration as nobody should be discontinued
        % Nobody audience is allowed when creating a provider
        {?PROVIDER_REGISTRATION_TOKEN, _} -> ok;
        % User audience is allowed in all invite tokens
        % (including ?SPACE_SUPPORT_TOKEN, when supporting as cluster admin)
        {_, ?AUD(user, _)} -> ok;
        _ -> ?ERROR_TOKEN_AUDIENCE_FORBIDDEN(Audience)
    end;
check_against_token_type(_, _, _) ->
    ?ERROR_TOKEN_INVALID.


%% @private
-spec subject_exists(aai:subject()) -> boolean().
subject_exists(?SUB(user, UserId)) -> user_logic:exists(UserId);
subject_exists(?SUB(?ONEPROVIDER, ProviderId)) -> provider_logic:exists(ProviderId).
