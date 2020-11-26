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
-include_lib("ctool/include/logging.hrl").

%% API
-export([authenticate/2]).
-export([authenticate_for_rest_interface/1]).
-export([verify_access_token/2]).
-export([verify_identity_token/2]).
-export([verify_invite_token/3]).
-export([verify_service_token/2]).
-export([verify_consumer_token/2]).
-export([group_membership_checker/2]).
-export([validate_subject_and_service/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Tries to authenticate a client by an access token.
%%   {true, #auth{}} - the client was authenticated
%%   errors:unauthorized_error() - provided access token was invalid
%% @end
%%--------------------------------------------------------------------
-spec authenticate(tokens:serialized() | tokens:token(), aai:auth_ctx()) ->
    {true, aai:auth()} | errors:unauthorized_error().
authenticate(Serialized, AuthCtx) when is_binary(Serialized) ->
    case tokens:deserialize(Serialized) of
        {ok, Token} -> authenticate(Token, AuthCtx);
        {error, _} = Error -> ?ERROR_UNAUTHORIZED(Error)
    end;
authenticate(Token, AuthCtx) ->
    case verify_access_token(Token, AuthCtx) of
        {ok, Auth} -> {true, Auth};
        {error, _} = Error -> ?ERROR_UNAUTHORIZED(Error)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to authenticate a client by an access token based on a HTTP request.
%% The resulting #auth{} is valid only for REST interface.
%% Supports third party access tokens originating from Identity Providers.
%%   {true, #auth{}} - the client was authenticated
%%   false - access token was not found
%%   errors:unauthorized_error() - provided access token was invalid
%% @end
%%--------------------------------------------------------------------
-spec authenticate_for_rest_interface(cowboy_req:req()) ->
    {true, aai:auth()} | false | errors:unauthorized_error().
authenticate_for_rest_interface(Req) when is_map(Req) ->
    case tokens:parse_access_token_header(Req) of
        undefined ->
            false;
        Serialized ->
            case tokens:deserialize(Serialized) of
                {ok, Token} ->
                    authenticate_for_rest_interface(Req, Token);
                ?ERROR_BAD_TOKEN ->
                    case openid_protocol:authenticate_by_idp_access_token(Serialized) of
                        {true, {IdP, Attributes}} ->
                            LinkedAccount = attribute_mapping:map_attributes(IdP, Attributes),
                            {ok, #document{key = UserId}} = linked_accounts:acquire_user(LinkedAccount),
                            {true, ?USER(UserId)};
                        ?ERROR_UNAUTHORIZED(_) = AuthenticationError ->
                            AuthenticationError;
                        false ->
                            ?ERROR_UNAUTHORIZED(?ERROR_BAD_TOKEN)
                    end
            end
    end.

%% @private
-spec authenticate_for_rest_interface(cowboy_req:req(), tokens:token()) ->
    {true, aai:auth()} | errors:unauthorized_error().
authenticate_for_rest_interface(Req, Token) ->
    case {resolve_service_for_rest_interface(Req), resolve_consumer_for_rest_interface(Req)} of
        {{error, _} = Error, _} ->
            ?ERROR_UNAUTHORIZED(Error);
        {_, {error, _} = Error} ->
            ?ERROR_UNAUTHORIZED(Error);
        {{ok, Service}, {ok, Consumer}} ->
            {PeerIp, _} = cowboy_req:peer(Req),
            authenticate(Token, #auth_ctx{
                ip = PeerIp,
                interface = rest,
                service = Service,
                consumer = Consumer,
                % @TODO VFS-3817 Required until Onepanel uses GraphSync
                % with auth override for authenticating its clients.
                % Make sure to keep backward compatibility when it does!
                session_id = case Service of
                    % accept any session - op-worker and op-panel have no way of tracking
                    % client's session as they are hosted on different domain than Onezone
                    ?SERVICE(?OP_PANEL, _) ->
                        any;
                    _ ->
                        case gui_session:peek_session_id(Req) of
                            {ok, S} -> S;
                            _ -> undefined

                        end
                end
            })
    end.


%%--------------------------------------------------------------------
%% @doc
%% Verifies an access token, and if it's valid, returns the auth carried by that
%% token (auth object).
%% @end
%%--------------------------------------------------------------------
-spec verify_access_token(tokens:token(), aai:auth_ctx()) ->
    {ok, aai:auth()} | errors:error().
verify_access_token(#token{type = ?ACCESS_TOKEN} = Token, AuthCtx) ->
    verify_token(Token, AuthCtx);
verify_access_token(#token{type = ReceivedTokenType}, _AuthCtx) ->
    ?ERROR_NOT_AN_ACCESS_TOKEN(ReceivedTokenType).


%%--------------------------------------------------------------------
%% @doc
%% Verifies the identity carried by an identity token - verifies the token
%% itself and returns the token's subject.
%% @end
%%--------------------------------------------------------------------
-spec verify_identity_token(tokens:token(), aai:auth_ctx()) ->
    {ok, {aai:subject(), [caveats:caveat()]}} | errors:error().
verify_identity_token(#token{type = ?ACCESS_TOKEN(undefined) = Type} = Token, AuthCtx) ->
    %% @todo VFS-6098 access tokens are still accepted as identity tokens
    %% for backward compatibility with legacy providers
    case verify_token(Token, AuthCtx#auth_ctx{scope = identity_token}) of
        {ok, #auth{subject = ?SUB(?ONEPROVIDER, _) = Subject, caveats = Caveats}} ->
            {ok, {Subject, Caveats}};
        {ok, _} ->
            % user access tokens are never accepted as identity tokens
            ?ERROR_NOT_AN_IDENTITY_TOKEN(Type);
        {error, _} = Error ->
            Error
    end;
verify_identity_token(#token{type = ?IDENTITY_TOKEN} = Token, AuthCtx) ->
    case verify_token(Token, AuthCtx#auth_ctx{scope = identity_token}) of
        {ok, #auth{subject = Subject, caveats = Caveats}} ->
            {ok, {Subject, Caveats}};
        {error, _} = Error ->
            Error
    end;
verify_identity_token(#token{type = ReceivedTokenType}, _AuthCtx) ->
    ?ERROR_NOT_AN_IDENTITY_TOKEN(ReceivedTokenType).


%%--------------------------------------------------------------------
%% @doc
%% Verifies an invite token and checks against given expected invite token type
%% ('any' keyword can be used for any invite token). If it's valid, returns the
%% auth carried by that token (auth object).
%% @end
%%--------------------------------------------------------------------
-spec verify_invite_token(tokens:token(), any | token_type:invite_type(), aai:auth_ctx()) ->
    {ok, aai:auth()} | errors:error().
verify_invite_token(Token = #token{type = ReceivedType}, ExpectedType, AuthCtx) ->
    case tokens:is_invite_token(Token, ExpectedType) of
        true ->
            verify_token(Token, AuthCtx);
        false ->
            ?ERROR_NOT_AN_INVITE_TOKEN(ExpectedType, ReceivedType)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Verifies a service token and returns the service upon success.
%% The service token is a regular access token put in a proper header and sent
%% along with the subject's access token, however it allows less caveats than
%% a typical access token. It is used to prove the identity of token bearer,
%% which may be required to satisfy a service caveat of the subject's access
%% token.
%% @end
%%--------------------------------------------------------------------
-spec verify_service_token(tokens:token() | tokens:serialized(), aai:auth_ctx()) ->
    {ok, aai:service_spec()} | errors:error().
verify_service_token(SerializedServiceToken, AuthCtx) when is_binary(SerializedServiceToken) ->
    case tokens:deserialize(SerializedServiceToken) of
        {ok, ServiceToken} ->
            %% @todo VFS-6098 legacy onepanel sends its access token as service
            %% token with service indication, but since version 1 tokens do not
            %% carry inscribed subject, it is ignored during deserialization -
            %% detect such situation and adjust the resolved service.
            {IndicatedService, _} = tokens:check_for_oneprovider_service_indication(SerializedServiceToken),
            case {verify_service_token(ServiceToken, AuthCtx), IndicatedService} of
                {{ok, ?SERVICE(?OP_WORKER, ProviderId)}, ?OP_PANEL} ->
                    {ok, ?SERVICE(?OP_PANEL, ProviderId)};
                {OtherResult, _} ->
                    OtherResult
            end;
        {error, _} = Error ->
            ?ERROR_BAD_SERVICE_TOKEN(Error)
    end;
verify_service_token(ServiceToken, AuthCtx) ->
    case verify_identity_token(ServiceToken, AuthCtx) of
        {ok, {?SUB(?ONEPROVIDER, OpServiceType, ProviderId), _}} ->
            case OpServiceType of
                undefined -> {ok, ?SERVICE(?OP_WORKER, ProviderId)};
                _ -> {ok, ?SERVICE(OpServiceType, ProviderId)}
            end;
        {ok, _} ->
            ?ERROR_BAD_SERVICE_TOKEN(?ERROR_TOKEN_INVALID);
        {error, _} = Err2 ->
            ?ERROR_BAD_SERVICE_TOKEN(Err2)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Verifies a consumer token and returns the consumer upon success.
%% The consumer token is a regular access token put in a proper header and sent
%% along with the subject's access token, however it allows less caveats than
%% a typical access token. It is used to prove the identity of token bearer,
%% which may be required to satisfy a consumer caveat of the subject's access
%% token.
%% @end
%%--------------------------------------------------------------------
-spec verify_consumer_token(tokens:token() | tokens:serialized(), aai:auth_ctx()) ->
    {ok, aai:subject()} | errors:error().
verify_consumer_token(SerializedConsumerToken, AuthCtx) when is_binary(SerializedConsumerToken) ->
    case tokens:deserialize(SerializedConsumerToken) of
        {ok, ConsumerToken} ->
            verify_consumer_token(ConsumerToken, AuthCtx);
        {error, _} = Error ->
            ?ERROR_BAD_CONSUMER_TOKEN(Error)
    end;
verify_consumer_token(ConsumerToken, AuthCtx) ->
    case verify_identity_token(ConsumerToken, AuthCtx) of
        {ok, {Subject, _}} ->
            {ok, Subject};
        {error, _} = Err2 ->
            ?ERROR_BAD_CONSUMER_TOKEN(Err2)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Used as callback during caveat verification (provided in #auth_ctx{}).
%% @end
%%--------------------------------------------------------------------
-spec group_membership_checker(aai:subject(), od_group:id()) -> boolean().
group_membership_checker(?SUB(user, UserId), <<"*">>) ->
    {ok, Groups} = user_logic:get_groups(?ROOT, UserId),
    length(Groups) > 0;
group_membership_checker(?SUB(user, UserId), GroupId) ->
    group_logic:has_eff_user(GroupId, UserId);
group_membership_checker(_, _) ->
    false.


%%--------------------------------------------------------------------
%% @doc
%% Checks if the subject and service are compatible with each other and
%% valid for given token type.
%% @end
%%--------------------------------------------------------------------
-spec validate_subject_and_service(tokens:type(), aai:subject(), aai:service_spec()) ->
    ok | errors:error().
validate_subject_and_service(?ACCESS_TOKEN(SessId), Subject, Service) ->
    SessionValid = case {SessId, Subject} of
        {undefined, _} -> true;
        {_, ?SUB(user, UserId)} -> session:belongs_to_user(SessId, UserId);
        {_, _} -> false
    end,
    case {SessionValid, is_service_allowed(Subject, Service)} of
        {true, true} -> ok;
        {false, _} -> ?ERROR_TOKEN_SESSION_INVALID;
        {_, false} -> ?ERROR_TOKEN_SERVICE_FORBIDDEN(Service)
    end;
validate_subject_and_service(_, _, _) ->
    % Identity and invite tokens do not require additional checks as they do not
    % support the service caveat.
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines if the service is allowed for specific subject in an access token,
%% in other words if the service can use subject's tokens on their behalf.
%% @end
%%--------------------------------------------------------------------
-spec is_service_allowed(aai:subject(), aai:service_spec()) -> boolean().
is_service_allowed(?SUB(nobody), _) ->
    false; % Service is not applicable in case of nobody (unauthenticated client)

is_service_allowed(_Subject, ?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)) ->
    true; % Any user / provider can generate a token for Onezone
is_service_allowed(_Subject, ?SERVICE(?OZ_WORKER, <<"*">>)) ->
    true;

is_service_allowed(?SUB(user, _), ?SERVICE(_, <<"*">>)) ->
    true; % Always allowed when service id is a wildcard
is_service_allowed(?SUB(user, UserId), ?SERVICE(?OZ_PANEL, ?ONEZONE_CLUSTER_ID)) ->
    cluster_logic:has_eff_user(?ONEZONE_CLUSTER_ID, UserId);
is_service_allowed(?SUB(user, UserId), ?SERVICE(?OP_WORKER, ProviderId)) ->
    provider_logic:has_eff_user(ProviderId, UserId);
is_service_allowed(?SUB(user, UserId), ?SERVICE(?OP_PANEL, ProviderId)) ->
    cluster_logic:has_eff_user(ProviderId, UserId);

is_service_allowed(_, _) ->
    false.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if an service token is present in a HTTP (REST) request and if so,
%% verifies it. Returns undefined service if no token was provided, or the
%% token verification result (see verify_service_token/2).
%% @end
%%--------------------------------------------------------------------
-spec resolve_service_for_rest_interface(cowboy_req:req()) ->
    {ok, undefined | aai:service_spec()} | errors:error().
resolve_service_for_rest_interface(Req) ->
    case tokens:parse_service_token_header(Req) of
        undefined ->
            {ok, undefined};
        Serialized ->
            {PeerIp, _} = cowboy_req:peer(Req),
            verify_service_token(Serialized, #auth_ctx{ip = PeerIp, interface = rest})
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if an consumer token is present in a HTTP (REST) request and if so,
%% verifies it. Returns undefined consumer if no token was provided, or the
%% token verification result (see verify_consumer_token/2).
%% @end
%%--------------------------------------------------------------------
-spec resolve_consumer_for_rest_interface(cowboy_req:req()) ->
    {ok, undefined | aai:subject()} | errors:error().
resolve_consumer_for_rest_interface(Req) ->
    case tokens:parse_consumer_token_header(Req) of
        undefined ->
            {ok, undefined};
        Serialized ->
            {PeerIp, _} = cowboy_req:peer(Req),
            verify_consumer_token(Serialized, #auth_ctx{ip = PeerIp, interface = rest})
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Verifies a token according to the list of supported caveats and returns
%% the auth that it carries.
%% @end
%%--------------------------------------------------------------------
-spec verify_token(tokens:token(), aai:auth_ctx()) -> {ok, aai:auth()} | errors:error().
verify_token(Token = #token{persistence = {temporary, TokenGen}, subject = Subject}, AuthCtx) ->
    % This check is not required in case of named tokens, which are deleted
    % alongside the user / provider.
    case subject_exists(Subject) of
        false ->
            ?ERROR_TOKEN_INVALID;
        true ->
            case temporary_token_secret:get_for_subject(Subject) of
                {Secret, TokenGen} ->
                    verify_token(Token, AuthCtx, Secret);
                {_, NewerGen} when NewerGen > TokenGen ->
                    % Most probably, the token has been revoked (or forged, in
                    % such case the error might be confusing for the forger).
                    ?ERROR_TOKEN_REVOKED;
                {_, OlderGen} when OlderGen < TokenGen ->
                    % The token must have been forged (its generation is newer than actual).
                    ?ERROR_TOKEN_INVALID
            end
    end;
verify_token(Token = #token{persistence = named}, AuthCtx) ->
    case od_token:get(Token#token.id) of
        {ok, #document{value = #od_token{revoked = true}}} ->
            ?ERROR_TOKEN_REVOKED;
        {ok, #document{value = #od_token{secret = Secret, subject = ?SUB(SubType, _, SubId) = Subject}}} ->
            case Token of
                #token{version = 1} ->
                    % Legacy tokens do not include the subject - inject the one
                    % stored in named token record
                    verify_token(Token#token{subject = Subject}, AuthCtx, Secret);
                #token{subject = ?SUB(SubType, _, SubId)} ->
                    % Double check that the token includes the same subject as
                    % the one stored in named token record (ignore subtype)
                    verify_token(Token, AuthCtx, Secret);
                _ ->
                    % The subjects are different - the token is invalid
                    ?ERROR_TOKEN_INVALID
            end;
        {error, not_found} ->
            ?ERROR_TOKEN_INVALID
    end.

%% @private
-spec verify_token(tokens:token(), aai:auth_ctx(), tokens:secret()) ->
    {ok, aai:auth()} | errors:error().
verify_token(Token = #token{type = TokenType}, AuthCtx, Secret) ->
    Service = case AuthCtx#auth_ctx.service of
        undefined -> ?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID);
        Srv -> Srv
    end,
    CoalescedAuthCtx = AuthCtx#auth_ctx{
        current_timestamp = global_clock:timestamp_seconds(),
        service = Service,
        group_membership_checker = fun group_membership_checker/2
    },
    case tokens:verify(Token, Secret, CoalescedAuthCtx) of
        {ok, Auth = #auth{subject = Subject}} ->
            case validate_subject_and_service(TokenType, Subject, Service) of
                ok -> {ok, Auth};
                {error, _} = Err1 -> Err1
            end;
        {error, _} = Err2 ->
            Err2
    end.


%% @private
-spec subject_exists(aai:subject()) -> boolean().
subject_exists(?SUB(user, UserId)) -> user_logic:exists(UserId);
subject_exists(?SUB(?ONEPROVIDER, ProviderId)) -> provider_logic:exists(ProviderId).
