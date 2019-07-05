%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module present a high level API for creating and validating tokens
%%% issued for different GUIs in Onedata.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_tokens).
-author("Lukasz Opiola").

-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/api_errors.hrl").

% Timestamp (in seconds) when the token expires
-type expires() :: non_neg_integer().

-define(NOW(), time_utils:cluster_time_seconds()).
-define(GUI_TOKEN_TTL, oz_worker:get_env(gui_token_ttl, 600)).

-export([create/3]).
-export([verify/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new GUI token for given combination:
%%  * UserId of the client
%%  * Id of the user's session for which the token is issued
%%  * Audience (service type and id) for which the token will be valid
%% GUI tokens are temporary - time limited and not stored anywhere.
%% @end
%%--------------------------------------------------------------------
-spec create(od_user:id(), session:id(), aai:audience()) ->
    {ok, tokens:token(), expires()} | {error, token_audience_forbidden}.
create(UserId, SessionId, Audience) ->
    case is_audience_allowed(UserId, Audience) of
        true ->
            Secret = shared_token_secret:get(),
            Now = ?NOW(),
            TTL = ?GUI_TOKEN_TTL,
            Expires = Now + TTL,
            Prototype = #auth_token{
                onezone_domain = oz_worker:get_domain(),
                nonce = datastore_utils:gen_key(),
                persistent = false,
                subject = ?SUB(user, UserId),
                type = ?GUI_TOKEN(SessionId)
            },
            Token = tokens:construct(Prototype, Secret, [
                ?AUDIENCE_CAVEAT(Audience),
                ?TIME_CAVEAT(Now, TTL)
            ]),
            {ok, Token, Expires};
        false ->
            ?ERROR_TOKEN_AUDIENCE_FORBIDDEN
    end.


%%--------------------------------------------------------------------
%% @doc
%% Verifies if given GUI token carries valid user authorization, is related to
%% an existing session and is intended for given Audience.
%% @end
%%--------------------------------------------------------------------
-spec verify(tokens:token(), aai:audience()) ->
    {ok, od_user:id(), session:id()} | {error, term()}.
verify(AuthToken = #auth_token{type = ?GUI_TOKEN(SessionId)}, Audience) ->
    case session:exists(SessionId) of
        {ok, true} ->
            Secret = shared_token_secret:get(),
            CaveatVerifiers = lists:flatten([
                case Audience of
                    undefined -> [];
                    _ -> ?AUDIENCE_CAVEAT(Audience)
                end,
                ?TIME_CAVEAT(?NOW(), ?GUI_TOKEN_TTL)
            ]),
            case tokens:verify(AuthToken, Secret, [], CaveatVerifiers) of
                {ok, ?SUB(user, UserId) = Subject} ->
                    case is_audience_allowed(UserId, Audience) of
                        false -> ?ERROR_TOKEN_AUDIENCE_FORBIDDEN;
                        true -> {ok, #auth{
                            subject = Subject,
                            audience = Audience,
                            session_id = SessionId
                        }}
                    end;
                {error, _} = Error ->
                    Error
            end;
        _ ->
            ?ERROR_TOKEN_SESSION_INVALID
    end;
verify(_, _) ->
    ?ERROR_MACAROON_INVALID.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec is_audience_allowed(od_user:id(), undefined | aai:audience()) -> boolean().
is_audience_allowed(_UserId, undefined) ->
    true;
is_audience_allowed(_UserId, ?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)) ->
    true; % All users can generate a token for Onezone
is_audience_allowed(UserId, ?AUD(?OZ_PANEL, ?ONEZONE_CLUSTER_ID)) ->
    cluster_logic:has_eff_user(?ONEZONE_CLUSTER_ID, UserId);
is_audience_allowed(UserId, ?AUD(?OP_WORKER, ProviderId)) ->
    provider_logic:has_eff_user(ProviderId, UserId);
is_audience_allowed(UserId, ?AUD(?OP_PANEL, ProviderId)) ->
    cluster_logic:has_eff_user(ProviderId, UserId).
