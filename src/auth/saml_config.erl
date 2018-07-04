%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles configuration of SAML Service Provider and
%%% supported Identity Providers.
%%% @end
%%%-------------------------------------------------------------------
-module(saml_config).

-include("auth_common.hrl").
-include("http/gui_paths.hrl").
-include("registered_names.hrl").
-include_lib("esaml/include/esaml.hrl").
-include_lib("ctool/include/logging.hrl").

-export([
    get_sp_config/0,
    get_saml_cert_pem/0,
    get_supported_idps/0,
    get_idp_config/1,
    has_group_mapping_enabled/1,
    get_super_group/1,
    normalize_membership_specs/2,
    normalize_membership_spec/2
]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns the config of the Service Provider represented by this onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_sp_config() -> #esaml_sp{}.
get_sp_config() ->
    SAMLConfig = get_config(),
    SPConfig = maps:get(sp_config, SAMLConfig),

    RolloverNewCert = case maps:get(rollover_new_cert_file, SPConfig, undefined) of
        undefined -> undefined;
        CertPath -> esaml_util:load_certificate(CertPath)
    end,

    RolloverNewKey = case maps:get(rollover_new_key_file, SPConfig, undefined) of
        undefined -> undefined;
        KeyPath -> esaml_util:load_private_key(KeyPath)
    end,

    #esaml_sp{
        entity_id = maps:get(entity_id, SPConfig),
        certificate = esaml_util:load_certificate(maps:get(cert_file, SPConfig)),
        key = esaml_util:load_private_key(maps:get(key_file, SPConfig)),
        rollover_new_certificate = RolloverNewCert,
        rollover_new_key = RolloverNewKey,
        consume_uri = binary_to_list(oz_worker:get_uri(<<?SAML_CONSUME_PATH>>)),
        metadata_uri = binary_to_list(oz_worker:get_uri(<<?SAML_METADATA_PATH>>)),
        org = #esaml_org{
            name = maps:get(organization_name, SPConfig),
            displayname = maps:get(organization_display_name, SPConfig),
            url = binary_to_list(oz_worker:get_url())
        },
        tech = #esaml_contact{
            name = maps:get(tech_contact_name, SPConfig),
            email = maps:get(tech_contact_email, SPConfig)
        },
        sign_requests = maps:get(sign_requests, SPConfig),
        sign_metadata = maps:get(sign_metadata, SPConfig),
        want_assertions_signed = maps:get(want_assertions_signed, SPConfig)
    }.


%%--------------------------------------------------------------------
%% @doc
%% Returns the SAML certificate in PEM format. If a rollover certificate is
%% present, it is returned, otherwise the standard cert.
%% @end
%%--------------------------------------------------------------------
-spec get_saml_cert_pem() -> binary().
get_saml_cert_pem() ->
    SAMLConfig = get_config(),
    SPConfig = maps:get(sp_config, SAMLConfig),

    Default = maps:get(cert_file, SPConfig),
    CertificatePath = maps:get(rollover_new_cert_file, SPConfig, Default),
    {ok, CertPem} = file:read_file(CertificatePath),
    CertPem.


%%--------------------------------------------------------------------
%% @doc
%% Returns the list of all supported IdPs (their ids).
%% @end
%%--------------------------------------------------------------------
-spec get_supported_idps() -> [atom()].
get_supported_idps() ->
    SAMLConfig = get_config(),
    maps:keys(maps:get(supported_idps, SAMLConfig)).


%%--------------------------------------------------------------------
%% @doc
%% Returns IdP config based on its id.
%% @end
%%--------------------------------------------------------------------
-spec get_idp_config(IdPId :: atom()) -> #esaml_idp{}.
get_idp_config(IdPId) ->
    SAMLConfig = get_config(),
    SupportedIdPs = maps:get(supported_idps, SAMLConfig),
    IdPConfig = maps:get(IdPId, SupportedIdPs),
    #esaml_idp{
        metadata = esaml_util:load_metadata(maps:get(metadata_url, IdPConfig)),
        preferred_sso_binding = maps:get(preferred_sso_binding, IdPConfig, http_redirect),
        encrypts_assertions = maps:get(encrypts_assertions, IdPConfig),
        signs_assertions = maps:get(signs_assertions, IdPConfig),
        signs_envelopes = maps:get(signs_envelopes, IdPConfig),
        signs_logout_requests = false, % TODO Logout currently not supported
        attribute_mapping = maps:get(attribute_mapping, IdPConfig)
    }.


%%--------------------------------------------------------------------
%% @doc
%% Returns whether given IdP has enabled group mapping based on its config.
%% @end
%%--------------------------------------------------------------------
-spec has_group_mapping_enabled(IdPId :: atom()) -> boolean().
has_group_mapping_enabled(IdPId) ->
    SAMLConfig = get_config(),
    SupportedIdPs = maps:get(supported_idps, SAMLConfig),
    IdPConfig = maps:get(IdPId, SupportedIdPs, #{}),
    GroupMappingConfig = maps:get(group_mapping, IdPConfig, #{}),
    maps:get(enabled, GroupMappingConfig, false).


%%--------------------------------------------------------------------
%% @doc
%% Returns the super group for given IdP, if specified in config.
%% @end
%%--------------------------------------------------------------------
-spec get_super_group(IdPId :: atom()) ->
    undefined | idp_group_mapping:group_spec().
get_super_group(IdPId) ->
    SAMLConfig = get_config(),
    SupportedIdPs = maps:get(supported_idps, SAMLConfig),
    IdPConfig = maps:get(IdPId, SupportedIdPs),
    GroupMappingConfig = maps:get(group_mapping, IdPConfig, #{}),
    maps:get(super_group, GroupMappingConfig, undefined).

%%--------------------------------------------------------------------
%% @doc
%% Normalizes group membership spec for given IdP.
%% @end
%%--------------------------------------------------------------------
-spec normalize_membership_spec(IdPId :: atom(), Groups :: binary()) ->
    idp_group_mapping:membership_spec().
normalize_membership_spec(elixir, Group) ->
    normalize_elixir_membership_spec(Group);
normalize_membership_spec(_, Group) ->
    Group.

% TODO move this to a configurable plugin during auth system refactoring
%%--------------------------------------------------------------------
%% @doc
%% Normalizes group membership specs for given IdP.
%% @end
%%--------------------------------------------------------------------
-spec normalize_membership_specs(IdPId :: atom(), Groups :: [binary()]) ->
    [idp_group_mapping:membership_spec()].
normalize_membership_specs(elixir, Groups) ->
    lists:map(fun normalize_elixir_membership_spec/1, Groups);
normalize_membership_specs(_, Groups) ->
    Groups.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reads SAML config from file and returns it as a map.
%% TODO consider caching the config file
%% @end
%%--------------------------------------------------------------------
-spec get_config() -> maps:map().
get_config() ->
    {ok, SAMLConfigFile} = oz_worker:get_env(saml_config_file),
    {ok, [SAMLConfig]} = file:consult(SAMLConfigFile),
    SAMLConfig.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes group membership specs for Elixir.
%% @end
%%-------------------------------------------------------------------
-spec normalize_elixir_membership_spec(binary()) ->
    idp_group_mapping:membership_spec().
normalize_elixir_membership_spec(Group) ->
    [VO | Rest] = binary:split(Group, <<":">>, [global]),
    MappedTokens = [<<"vo:", VO/binary>>] ++
        [<<"tm:", Gr/binary>> || Gr <- Rest] ++
        [<<"user:member">>],
    str_utils:join_binary(MappedTokens, <<"/">>).