%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module presents a high level API for creating and validating access
%%% tokens issued by the Onezone service.
%%%
%%% @TODO VFS-5726 - this module is currently unused, but fully functional.
%%% It will be merged with the access tokens module
%%% when user access token upgrade procedure is implemented.
%%% @end
%%%-------------------------------------------------------------------
-module(new_access_tokens).
-author("Lukasz Opiola").

-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/api_errors.hrl").

-define(NOW(), time_utils:cluster_time_seconds()).
%% @TODO VFS-5726 switch to a single secret per token
-define(SHARED_SECRET, shared_token_secret:get()).
-define(SUPPORTED_CAVEATS, [
    cv_time, cv_audience, cv_ip, cv_asn, cv_country, cv_region,
    cv_api, cv_data_space, cv_data_access, cv_data_path, cv_data_objectid
]).

-export([create/2, verify/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec create(aai:subject(), [caveats:caveat()]) -> {ok, tokens:token()}.
create(Subject, Caveats) ->
    Prototype = #auth_token{
        onezone_domain = oz_worker:get_domain(),
        nonce = datastore_utils:gen_key(),
        persistent = false, %% @TODO VFS-5726 persistence for identifiers and secrets
        subject = Subject,
        type = ?ACCESS_TOKEN
    },
    {ok, tokens:construct(Prototype, ?SHARED_SECRET, Caveats)}.


-spec verify(tokens:token(), ip_utils:ip(), undefined | aai:audience()) -> {ok, aai:auth()} | {error, term()}.
verify(Token, PeerIp, Audience) ->
    AuthCtx = #auth_ctx{current_timestamp = ?NOW(), ip = PeerIp, audience = Audience},
    case tokens:verify(Token, ?SHARED_SECRET, AuthCtx, ?SUPPORTED_CAVEATS) of
        {ok, ?USER(UserId) = Auth} ->
            case is_audience_allowed(UserId, Audience) of
                true -> {ok, Auth};
                false -> ?ERROR_TOKEN_AUDIENCE_FORBIDDEN
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