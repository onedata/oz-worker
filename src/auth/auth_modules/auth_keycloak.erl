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

%% API
-export([get_redirect_url/2, validate_login/2, get_user_info/2]).
-export([normalized_membership_specs/2, normalized_membership_spec/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns full URL, where the user will be redirected for authorization.
%% See function specification in auth_module_behaviour.
%% @end
%%--------------------------------------------------------------------
-spec get_redirect_url(auth_utils:idp(), boolean()) ->
    {ok, binary()} | {error, term()}.
get_redirect_url(IdP, LinkAccount) ->
    auth_oauth2_common:get_redirect_url(LinkAccount, IdP).


%%--------------------------------------------------------------------
%% @doc Validates login request that came back from the provider.
%% See function specification in auth_module_behaviour.
%% @end
%%--------------------------------------------------------------------
-spec validate_login(auth_utils:idp(), QueryParams :: proplists:proplist()) -> {ok, #linked_account{}} | {error, term()}.
validate_login(IdP, QueryParams) ->
    auth_oauth2_common:validate_login(
        IdP, QueryParams, secret_over_http_post, access_token_in_header
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
        IdP, access_token_in_header, AccessToken
    ).

%%--------------------------------------------------------------------
%% @doc
%% @equiv normalized_membership_spec(IdP, Group, team, {nested, <<"/">>})
%% @end
%%--------------------------------------------------------------------
-spec normalized_membership_spec(auth_utils:idp(), Group :: binary()) ->
    idp_group_mapping:membership_spec().
normalized_membership_spec(IdP, Group) ->
    normalized_membership_spec(IdP, Group, team, {nested, <<"/">>}).

%%--------------------------------------------------------------------
%% @doc
%% Returns a string that represents user's group membership for this
%% IdP, complying with specification in idp_group_mapping module.
%% @end
%%--------------------------------------------------------------------
-spec normalized_membership_spec(auth_utils:idp(), Group :: binary(), od_group:type(),
    Structure :: flat | {nested, SplitWith :: binary()}) ->
    idp_group_mapping:membership_spec().
normalized_membership_spec(IdP, Group, Type, Structure) ->
    VoId = vo_id(IdP),
    TypeStr = idp_group_mapping:type_to_str(Type),
    GroupSpec = case Structure of
        flat ->
            <<TypeStr/binary, ":", Group/binary>>;
        {nested, SplitWith} ->
            GroupTokens = binary:split(Group, SplitWith, [global, trim_all]),
            MappedTokens = [<<TypeStr/binary, ":", T/binary>> || T <- GroupTokens],
            str_utils:join_binary(MappedTokens, <<"/">>)
    end,
    <<"vo:", VoId/binary, "/", GroupSpec/binary, "/user:member">>.

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of strings that represent user's group memberships for this
%% IdP. They are strings complying with specification in idp_group_mapping
%% module. Returned values will be used to compute a diff in memberships
%% every time a user logs in, so he can be added to / removed from
%% certain groups. Because of this, the same values coming from IdP must always
%% be mapped to the same specs.
%% ===============================================
%% Mapping behaviour is configured in auth.config:
%% group_mapping -> attributes_to_map - which attributes sent by IdP should be
%% mapped to groups (each attribute value is expected to hold a list of strings).
%% Format: list of tuples {A, B, C}:
%%   A -> attribute key
%%   B -> derived group type (role | team | unit | organization)
%%   C -> expected group structure, one of:
%%     * flat -> all groups will be a direct child of the VO group,
%%       names of groups will be the same as in the attribute.
%%     * {nested, SplitWith} -> each group membership will be split
%%       into a hierarchical structure using the specified split pattern.
%%       E.g. for string <<"a/b/c">> and SplitWith = <<"/">>, three
%%       nested groups will be created, with user belonging to the
%%       last one: VO <- a <- b <- c <- user.
%% @end
%%--------------------------------------------------------------------
-spec normalized_membership_specs(auth_utils:idp(), maps:map()) ->
    [idp_group_mapping:membership_spec()].
normalized_membership_specs(IdP, Map) ->
    Config = auth_config:get_auth_config(IdP),
    GroupMappingConfig = proplists:get_value(group_mapping, Config, []),
    AttributesConfig = proplists:get_value(attributes_to_map, GroupMappingConfig, []),
    lists:flatmap(fun({Attr, Type, Structure}) ->
        Groups = maps:get(Attr, Map, []),
        lists:map(fun(Group) ->
            GroupWithoutSlashes = case Structure of
                {nested, <<"/">>} ->
                    Group;
                _ ->
                    % Otherwise we need to make sure that there are no slashes
                    % in the group name, as it would break the group spec format.
                    str_utils:join_binary(
                        binary:split(Group, <<"/">>, [global, trim_all]),
                        <<"-">>
                    )
            end,
            normalized_membership_spec(IdP, GroupWithoutSlashes, Type, Structure)
        end, Groups)
    end, AttributesConfig).


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
