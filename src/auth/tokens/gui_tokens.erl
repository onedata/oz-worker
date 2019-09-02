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

-define(NOW(), time_utils:cluster_time_seconds()).
-define(GUI_TOKEN_TTL, oz_worker:get_env(gui_token_ttl, 600)).
-define(SHARED_SECRET, shared_token_secret:get()).
-define(SUPPORTED_CAVEATS, [cv_time, cv_audience]).

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
    {ok, tokens:token(), time_utils:seconds()} | {error, token_audience_forbidden}.
create(UserId, SessionId, Audience) ->
    case is_audience_allowed(UserId, Audience) of
        true ->
            ValidUntil = ?NOW() + ?GUI_TOKEN_TTL,
            Prototype = #auth_token{
                onezone_domain = oz_worker:get_domain(),
                nonce = datastore_utils:gen_key(),
                persistent = false,
                subject = ?SUB(user, UserId),
                type = ?GUI_TOKEN(SessionId)
            },
            Token = tokens:construct(Prototype, ?SHARED_SECRET, [
                #cv_time{valid_until = ValidUntil},
                #cv_audience{audience = Audience}
            ]),
            {ok, Token, ValidUntil};
        false ->
            ?ERROR_TOKEN_AUDIENCE_FORBIDDEN
    end.


%%--------------------------------------------------------------------
%% @doc
%% Verifies if given GUI token carries valid user authorization, is related to
%% an existing session and is intended for given Audience.
%% @end
%%--------------------------------------------------------------------
-spec verify(tokens:token(), undefined | aai:audience()) -> {ok, aai:auth()} | {error, term()}.
verify(Token, Audience) ->
    AuthCtx = #auth_ctx{current_timestamp = ?NOW(), audience = Audience},
    case tokens:verify(Token, ?SHARED_SECRET, AuthCtx, ?SUPPORTED_CAVEATS) of
        {ok, ?USER(UserId, SessionId) = Auth} ->
            case {session:exists(SessionId), is_audience_allowed(UserId, Audience)} of
                {{ok, true}, true} -> {ok, Auth};
                {{ok, false}, _} -> ?ERROR_TOKEN_SESSION_INVALID;
                {_, false} -> ?ERROR_TOKEN_AUDIENCE_FORBIDDEN
            end;
        {error, _} = Error ->
            Error
    end.

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
