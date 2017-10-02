%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This module implements auth_module_behaviour and handles signing in
%%% via RHEA KeyCloak OpenID Connect.
%%% @end
%%%-------------------------------------------------------------------
-module(auth_keycloak).
-behaviour(auth_module_behaviour).

-include_lib("ctool/include/logging.hrl").
-include("auth_common.hrl").
-include("datastore/oz_datastore_models.hrl").

-define(PROVIDER_ID, rhea).

%% API
-export([get_redirect_url/1, validate_login/0, get_user_info/1]).
-export([normalized_membership_specs/1, normalized_membership_spec/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns full URL, where the user will be redirected for authorization.
%% See function specification in auth_module_behaviour.
%% @end
%%--------------------------------------------------------------------
-spec get_redirect_url(boolean()) -> {ok, binary()} | {error, term()}.
get_redirect_url(ConnectAccount) ->
    auth_oauth2_common:get_redirect_url(
        ConnectAccount, ?PROVIDER_ID, ?MODULE
    ).


%%--------------------------------------------------------------------
%% @doc Validates login request that came back from the provider.
%% See function specification in auth_module_behaviour.
%% @end
%%--------------------------------------------------------------------
-spec validate_login() -> {ok, #linked_account{}} | {error, term()}.
validate_login() ->
    auth_oauth2_common:validate_login(
        ?PROVIDER_ID, secret_over_http_post, access_token_in_header
    ).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves user info from oauth provider based on access token.
%% @end
%%--------------------------------------------------------------------
-spec get_user_info(AccessToken :: binary()) ->
    {ok, #linked_account{}} | {error, bad_access_token}.
get_user_info(AccessToken) ->
    auth_oauth2_common:get_user_info(
        ?PROVIDER_ID, access_token_in_header, AccessToken
    ).

%%--------------------------------------------------------------------
%% @doc
%% Returns a string that represents user's group membership for given
%% IdP. They are strings complying with specification in idp_group_mapping
%% module. Returned values will be used to compute a diff in memberships
%% every time a user logs in, so he can be added to / removed from
%% certain groups. Because of this, the same values coming from IdP must always
%% be mapped to the same specs.
%% @end
%%--------------------------------------------------------------------
-spec normalized_membership_spec(Group :: binary()) ->
    idp_group_mapping:membership_spec().
normalized_membership_spec(Group) ->
    VoId = vo_id(),
    GroupTokens = binary:split(Group, <<"/">>, [global]),
    MappedTokens = [<<"tm:", T/binary>> || T <- GroupTokens],
    GroupSpec = str_utils:join_binary(MappedTokens, <<"/">>),
    <<"vo:", VoId/binary, "/", GroupSpec/binary, "/user:member">>.

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of strings that represent user's group memberships for given
%% IdP. They are strings complying with specification in idp_group_mapping
%% module. Returned values will be used to compute a diff in memberships
%% every time a user logs in, so he can be added to / removed from
%% certain groups. Because of this, the same values coming from IdP must always
%% be mapped to the same specs.
%% @end
%%--------------------------------------------------------------------
-spec normalized_membership_specs(proplists:proplist()) ->
    [idp_group_mapping:membership_spec()].
normalized_membership_specs(Props) ->
    VoId = vo_id(),
    Groups = proplists:get_value(<<"groups">>, Props, []),
    Roles = proplists:get_value(<<"roles">>, Props, []),
    NormalizedRoles = lists:map(
        fun(Role) ->
            <<"vo:", VoId/binary, "/rl:", Role/binary, "/user:member">>
        end, Roles),
    NormalizedGroups = lists:map(fun normalized_membership_spec/1, Groups),
    NormalizedRoles ++ NormalizedGroups.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the group Id for KeyCloak VO.
%% @end
%%--------------------------------------------------------------------
-spec vo_id() -> binary().
vo_id() ->
    GroupMappingConfig = auth_config:get_group_mapping_config(?PROVIDER_ID),
    case proplists:get_value(vo_group_id, GroupMappingConfig) of
        undefined -> throw(no_vo_group_id_specified_in_config);
        VoId -> VoId
    end.
