%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc: This module implements auth_module_behaviour and handles signing in
%%% via Indigo OpenID.
%%% @end
%%%-------------------------------------------------------------------
-module(auth_indigo).
-behaviour(auth_module_behaviour).
-behaviour(group_mapping_plugin_behaviour).

-include_lib("ctool/include/logging.hrl").
-include("auth_common.hrl").
-include("datastore/oz_datastore_models.hrl").

%% API
-export([get_redirect_url/2, validate_login/2, get_user_info/2]).
-export([normalized_membership_specs/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns full URL, where the user will be redirected for authorization.
%% See function specification in auth_module_behaviour.
%% @end
%%--------------------------------------------------------------------
-spec get_redirect_url(auth_utils:idp(), boolean()) -> {ok, binary()} | {error, term()}.
get_redirect_url(IdP, LinkAccount) ->
    auth_oauth2_common:get_redirect_url(LinkAccount, IdP).


%%--------------------------------------------------------------------
%% @doc Validates login request that came back from the provider.
%% See function specification in auth_module_behaviour.
%% @end
%%--------------------------------------------------------------------
-spec validate_login(auth_utils:idp(), QueryParams :: proplists:proplist()) ->
    {ok, #linked_account{}} | {error, term()}.
validate_login(IdP, QueryParams) ->
    auth_oauth2_common:validate_login(
        IdP, QueryParams, secret_over_http_basic, access_token_in_url
    ).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves user info from oauth provider based on access token.
%% @end
%%--------------------------------------------------------------------
-spec get_user_info(auth_utils:idp(), AccessToken :: binary()) ->
    {ok, #linked_account{}} | {error, bad_access_token}.
get_user_info(IdP, AccessToken) ->
    auth_oauth2_common:get_user_info(
        IdP, access_token_in_url, AccessToken
    ).


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
-spec normalized_membership_specs(auth_utils:idp(), maps:map()) ->
    [idp_group_mapping:membership_spec()].
normalized_membership_specs(IdP, Map) ->
    Groups = maps:get(<<"groups">>, Map, []),
    VoId = vo_id(IdP),
    lists:map(
        fun(Group) ->
            GroupTokens = binary:split(Group, <<"/">>, [global]),
            MappedTokens = [<<"tm:", T/binary>> || T <- GroupTokens],
            GroupSpec = str_utils:join_binary(MappedTokens, <<"/">>),
            <<"vo:", VoId/binary, "/", GroupSpec/binary, "/user:member">>
        end, Groups).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the group Id for KeyCloak VO.
%% @end
%%--------------------------------------------------------------------
-spec vo_id(auth_utils:idp()) -> binary().
vo_id(IdP) ->
    GroupMappingConfig = auth_config:get_group_mapping_config(IdP),
    case proplists:get_value(vo_group_id, GroupMappingConfig) of
        undefined -> throw(no_vo_group_id_specified_in_config);
        VoId -> VoId
    end.
