%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles configuration of supported Identity Providers
%%% (SAML and OpenId).
%%% @end
%%%-------------------------------------------------------------------
-module(auth_config).

-include("auth/auth_common.hrl").
-include("auth/auth_errors.hrl").
-include("http/gui_paths.hrl").
-include("registered_names.hrl").
-include_lib("esaml/include/esaml.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/logging.hrl").

-type config_v1() :: proplists:proplist().
-type saml_config_v1() :: #{atom() => term()}.
-type config_v2() :: #{atom() => term()}.
-type config_section() :: #{atom() => term()}.
-type idp() :: atom().
-type protocol() :: saml | openid | onepanelAuth | undefined.
-type parser_config() :: #{atom() => term()}.
-export_type([config_v1/0, saml_config_v1/0, config_v2/0, config_section/0]).
-export_type([idp/0, protocol/0, parser_config/0]).

% Defines the behaviour when given config parameter is not found:
%   * required - throws an error
%   * {default, Def} - returns Def
-type config_policy() :: required | {default, term()}.
% Expresses the nesting of config - consecutive keys in a nested map used to
% reach given parameter
-type nested_param_key() :: atom() | {proplist, atom()}.
-type nested_params() :: [nested_param_key()].
% Similar to nested_params/0, expresses the trace that means how the certain
% level of nesting was reached (through what nested keys).
-type trace() :: nested_params().

-export([
    get_supported_idps_in_gui_format/0,
    get_supported_idps_in_configuration_format/0,
    idp_exists/1,
    is_onepanel_auth_enabled/0,
    get_protocol/1, get_protocol/2,
    get_protocol_config/3, get_protocol_config/4,
    get_attribute_mapping/2,
    get_entitlement_mapping_config/3
]).
-export([
    get_authority_delegation_config/1,
    find_openid_idp_by_access_token/1,
    get_idps_with_offline_access/0,
    has_offline_access_enabled/1
]).
-export([
    get_saml_sp_config/0,
    get_saml_cert_pem/0,
    get_saml_idp_config/1
]).
-export([
    force_auth_config_reload/0
]).
-export([
    ensure_bin/1,
    ensure_str/1
]).

-define(AUTH_CONFIG_FILE, begin {ok, __Path} = oz_worker:get_env(auth_config_file), __Path end).
-define(TEST_AUTH_CONFIG_FILE, begin {ok, __Path} = oz_worker:get_env(test_auth_config_file), __Path end).
-define(CONFIG_CACHE_TTL, oz_worker:get_env(auth_config_cache_ttl, timer:minutes(1))).

-define(LEGACY_SAML_CONFIG_NAME, "saml.config").
-define(BACKUP_CFG_EXT, ".bak").

-define(DEFAULT_ICON_PATH, "/assets/images/auth-providers/default.svg").
-define(DEFAULT_ICON_BGCOLOR, "#333").


% Macros for accessing config entries
% Onepanel auth config
-define(CFG_ONEPANEL_AUTH_ENABLED, get_nested_cfg(
    [onepanelAuthConfig, enabled], {default, false}
)).
% OpenID config
-define(CFG_OPENID_ENABLED, get_nested_cfg(
    [openidConfig, enabled], {default, false}
)).
-define(CFG_OPENID_DEFAULT_PROTOCOL_CONFIG, get_nested_cfg(
    [openidConfig, defaultProtocolConfig], {default, #{}}
)).
% Supported IdPs config
-define(CFG_SUPPORTED_IDPS, get_nested_cfg(
    [supportedIdps], {default, []}
)).
-define(CFG_IDP(IdP), get_nested_cfg(
    [supportedIdps, {proplist, IdP}], {default, #{}}
)).
% SAML config
-define(CFG_SAML_ENABLED, get_nested_cfg(
    [samlConfig, enabled], {default, false}
)).
-define(CFG_SAML_DEFAULT_PROTOCOL_CONFIG, get_nested_cfg(
    [samlConfig, defaultProtocolConfig], {default, #{}}
)).
% SAML SP config
-define(CFG_SAML_SP_CERT, get_nested_cfg(
    [samlConfig, spConfig, certFile], required
)).
-define(CFG_SAML_SP_KEY, get_nested_cfg(
    [samlConfig, spConfig, keyFile], required
)).
-define(CFG_SAML_SP_ROLLOVER_NEW_CERT, get_nested_cfg(
    [samlConfig, spConfig, rolloverNewCertFile], {default, undefined}
)).
-define(CFG_SAML_SP_ROLLOVER_NEW_KEY, get_nested_cfg(
    [samlConfig, spConfig, rolloverNewKeyFile], {default, undefined}
)).
-define(CFG_SAML_SP_ENTITY_ID, get_nested_cfg(
    [samlConfig, spConfig, entityId], required
)).
-define(CFG_SAML_SP_CONSUME_URI,
    binary_to_list(oz_worker:get_uri(<<?SAML_CONSUME_PATH>>))
).
-define(CFG_SAML_SP_METADATA_URI,
    binary_to_list(oz_worker:get_uri(<<?SAML_METADATA_PATH>>))
).
-define(CFG_SAML_SP_ORGANIZATION_NAME, get_nested_cfg(
    [samlConfig, spConfig, organizationName], required
)).
-define(CFG_SAML_SP_ORGANIZATION_DISPLAY_NAME, get_nested_cfg(
    [samlConfig, spConfig, organizationDisplayName], required
)).
-define(CFG_SAML_SP_ORGANIZATION_URL,
    binary_to_list(oz_worker:get_url())
).
-define(CFG_SAML_SP_CONTACT_NAME, get_nested_cfg(
    [samlConfig, spConfig, techContactName], required
)).
-define(CFG_SAML_SP_CONTACT_EMAIL, get_nested_cfg(
    [samlConfig, spConfig, techContactEmail], required
)).
-define(CFG_SAML_SP_SIGN_METADATA, get_nested_cfg(
    [samlConfig, spConfig, signMetadata], required
)).
-define(CFG_SAML_SP_SIGN_REQUESTS, get_nested_cfg(
    [samlConfig, spConfig, signRequests], required
)).
-define(CFG_SAML_SP_WANT_ASSERTIONS_SIGNED, get_nested_cfg(
    [samlConfig, spConfig, wantAssertionsSigned], required
)).
% Specific IdP config
-define(CFG_IDP_DISPLAY_NAME(IdP, IdPConfig), ?bin(get_param(
    displayName, {default, IdP}, IdPConfig)
)).
-define(CFG_IDP_ICON_PATH(IdPConfig), ?bin(get_param(
    iconPath, {default, ?DEFAULT_ICON_PATH}, IdPConfig)
)).
-define(CFG_IDP_ICON_BACKGROUND(IdPConfig), ?bin(get_param(
    iconBackgroundColor, {default, ?DEFAULT_ICON_BGCOLOR}, IdPConfig)
)).
-define(CFG_IDP_ATTRIBUTE_MAPPING(IdP, Attribute), get_protocol_config(
    IdP, [attributeMapping, Attribute], {default, undefined}
)).
-define(CFG_IDP_ENTITLEMENT_MAPPING_CONFIG(IdP, NestedParams, Policy), get_protocol_config(
    IdP, [entitlementMapping | NestedParams], Policy
)).
-define(CFG_IDP_AUTHORITY_DELEGATION_CONFIG(IdP, IdPConfig), get_protocol_config(
    IdP, [authorityDelegation, enabled], {default, false}, IdPConfig
)).
-define(CFG_IDP_AUTHORITY_DELEGATION_TOKEN_PREFIX(IdP, IdPConfig), ?bin(get_protocol_config(
    IdP, [authorityDelegation, tokenPrefix], {default, atom_to_list(IdP) ++ ":"}, IdPConfig)
)).
-define(CFG_IDP_OFFLINE_ACCESS_ENABLED(IdP, IdPConfig), get_protocol_config(
    IdP, [offlineAccess], {default, false}, IdPConfig
)).
-define(CFG_IDP_SAML_METADATA_URL(IdP, IdPConfig), get_protocol_config(
    IdP, [metadataUrl], required, IdPConfig
)).
-define(CFG_IDP_SAML_PREFERRED_SSO_BINDING(IdP, IdPConfig), get_protocol_config(
    IdP, [preferredSsoBinding], {default, http_redirect}, IdPConfig
)).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns the list of all supported IdPs and their config formatted for GUI.
%% @end
%%--------------------------------------------------------------------
-spec get_supported_idps_in_gui_format() -> json_utils:json_term().
get_supported_idps_in_gui_format() ->
    [format_for_gui(IdP, IdPConfig) || {IdP, IdPConfig} <- get_supported_idps()].


%%--------------------------------------------------------------------
%% @doc
%% Returns the list of all supported IdPs and their config formatted for the
%% configuration endpoint.
%% @end
%%--------------------------------------------------------------------
-spec get_supported_idps_in_configuration_format() -> [json_utils:json_term()].
get_supported_idps_in_configuration_format() ->
    [format_for_configuration(IdP, IdPConfig) || {IdP, IdPConfig} <- get_supported_idps()].


%%--------------------------------------------------------------------
%% @doc
%% Returns the list of all supported IdPs and their config.
%% @end
%%--------------------------------------------------------------------
-spec get_supported_idps() -> [{idp(), config_section()}].
get_supported_idps() ->
    OnepanelAuthEnabled = ?CFG_ONEPANEL_AUTH_ENABLED,
    OpenIDEnabled = ?CFG_OPENID_ENABLED,
    SAMLEnabled = ?CFG_SAML_ENABLED,

    lists:filtermap(fun({IdP, IdPConfig}) ->
        Enabled = case get_protocol(IdP, IdPConfig) of
            onepanelAuth -> OnepanelAuthEnabled;
            openid -> OpenIDEnabled;
            saml -> SAMLEnabled;
            undefined -> false
        end,
        case Enabled of
            false -> false;
            true -> {true, {IdP, IdPConfig}}
        end
    end, ?CFG_SUPPORTED_IDPS).


%%--------------------------------------------------------------------
%% @doc
%% Predicate saying if given IdP appears in auth.config.
%% @end
%%--------------------------------------------------------------------
-spec idp_exists(idp()) -> boolean().
idp_exists(IdP) ->
    proplists:is_defined(IdP, ?CFG_SUPPORTED_IDPS).


-spec is_onepanel_auth_enabled() -> boolean().
is_onepanel_auth_enabled() ->
    ?CFG_ONEPANEL_AUTH_ENABLED.


%%--------------------------------------------------------------------
%% @doc
%% Returns the protocol used by given IdP.
%% @end
%%--------------------------------------------------------------------
-spec get_protocol(idp()) -> protocol().
get_protocol(IdP) ->
    get_protocol(IdP, ?CFG_IDP(IdP)).

-spec get_protocol(idp(), config_section()) -> protocol().
get_protocol(IdP, IdPConfig) ->
    get_param(protocol, {default, undefined}, IdPConfig, [supportedIdps, IdP]).


%%--------------------------------------------------------------------
%% @doc
%% Returns arbitrarily nested IdP's configuration concerning its protocol.
%% @end
%%--------------------------------------------------------------------
-spec get_protocol_config(idp(), nested_params(), config_policy()) -> term().
get_protocol_config(IdP, ProtocolNestedParams, Policy) ->
    get_protocol_config(IdP, ProtocolNestedParams, Policy, ?CFG_IDP(IdP)).

-spec get_protocol_config(idp(), nested_params(), config_policy(), config_section()) -> term().
get_protocol_config(IdP, ProtocolNestedParams, Policy, IdPConfig) ->
    NestedParams = [protocolConfig | ProtocolNestedParams],
    Trace = [supportedIdps, IdP, protocolConfig],
    case get_nested_cfg(NestedParams, {default, '$not_found'}, IdPConfig, Trace) of
        '$not_found' ->
            Protocol = get_protocol(IdP, IdPConfig),
            get_default_protocol_config(Protocol, ProtocolNestedParams, Policy, Trace);
        Value ->
            Value
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns configured attribute mapping for given IdP and Onedata attribute.
%% @end
%%--------------------------------------------------------------------
-spec get_attribute_mapping(idp(), attribute_mapping:onedata_attribute()) ->
    attribute_mapping:attribute_mapping().
get_attribute_mapping(IdP, Attribute) ->
    Mapping = ?CFG_IDP_ATTRIBUTE_MAPPING(IdP, Attribute),
    case {Attribute, Mapping} of
        {subjectId, {required, _}} ->
            ok;
        {subjectId, {plugin, _}} ->
            ok;
        {subjectId, _} ->
            ?alert(
                "Illegal subjectId attribute mapping for IdP '~p', must be "
                "'{required, _}' or  '{plugin, _}'", [IdP]
            ),
            throw(?ERROR_BAD_AUTH_CONFIG);
        {_, _} ->
            ok
    end,
    Mapping.


%%--------------------------------------------------------------------
%% @doc
%% Returns arbitrarily nested IdP's configuration concerning its entitlement mapping.
%% @end
%%--------------------------------------------------------------------
-spec get_entitlement_mapping_config(idp(), nested_params(), config_policy()) -> term().
get_entitlement_mapping_config(IdP, NestedParams, Policy) ->
    ?CFG_IDP_ENTITLEMENT_MAPPING_CONFIG(IdP, NestedParams, Policy).


%%--------------------------------------------------------------------
%% @doc
%% Returns arbitrarily nested IdP's configuration concerning its authority delegation.
%% @end
%%--------------------------------------------------------------------
-spec get_authority_delegation_config(idp()) -> false | {true, binary()}.
get_authority_delegation_config(IdP) ->
    get_authority_delegation_config(IdP, ?CFG_IDP(IdP)).

-spec get_authority_delegation_config(idp(), config_section()) -> false | {true, binary()}.
get_authority_delegation_config(IdP, IdPConfig) ->
    case ?CFG_IDP_AUTHORITY_DELEGATION_CONFIG(IdP, IdPConfig) of
        false ->
            false;
        true ->
            case get_protocol(IdP, IdPConfig) of
                openid ->
                    {true, ?CFG_IDP_AUTHORITY_DELEGATION_TOKEN_PREFIX(IdP, IdPConfig)};
                OtherProtocol ->
                    ?warning(
                        "Authority delegation can only be enabled for the openid protocol - "
                        "please adjust the auth.config entry for ~p (~p protocol)",
                        [IdP, OtherProtocol]
                    ),
                    false
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Tries to match access token's prefix to any IdP by its configured prefix.
%% @end
%%--------------------------------------------------------------------
-spec find_openid_idp_by_access_token(auth_logic:access_token()) -> false | {true, idp()}.
find_openid_idp_by_access_token(AccessTokenWithPrefix) ->
    find_openid_idp_by_access_token(AccessTokenWithPrefix, ?CFG_SUPPORTED_IDPS).

-spec find_openid_idp_by_access_token(auth_logic:access_token(), [{idp(), config_section()}]) ->
    false | {true, idp()}.
find_openid_idp_by_access_token(_, []) ->
    false;
find_openid_idp_by_access_token(AccessTokenWithPrefix, [{IdP, IdPConfig} | Rest]) ->
    case get_authority_delegation_config(IdP, IdPConfig) of
        false ->
            find_openid_idp_by_access_token(AccessTokenWithPrefix, Rest);
        {true, TokenPrefix} ->
            case binary:part(AccessTokenWithPrefix, 0, byte_size(TokenPrefix)) of
                TokenPrefix ->
                    {true, {IdP, TokenPrefix}};
                _ ->
                    find_openid_idp_by_access_token(AccessTokenWithPrefix, Rest)
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the list of all IdPs that have offline access enabled.
%% Because of technical reasons, only OpenID IdPs are supported.
%% @end
%%--------------------------------------------------------------------
-spec get_idps_with_offline_access() -> [idp()].
get_idps_with_offline_access() ->
    case ?CFG_OPENID_ENABLED of
        false ->
            [];
        true ->
            lists:filtermap(fun({IdP, IdPConfig}) ->
                case has_offline_access_enabled(IdP, IdPConfig) of
                    false -> false;
                    true -> {true, IdP}
                end
            end, ?CFG_SUPPORTED_IDPS)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns arbitrarily nested IdP's configuration concerning its authority delegation.
%% @end
%%--------------------------------------------------------------------
-spec has_offline_access_enabled(idp()) -> boolean().
has_offline_access_enabled(IdP) ->
    has_offline_access_enabled(IdP, ?CFG_IDP(IdP)).

-spec has_offline_access_enabled(idp(), config_section()) -> boolean().
has_offline_access_enabled(IdP, IdPConfig) ->
    case ?CFG_IDP_OFFLINE_ACCESS_ENABLED(IdP, IdPConfig) of
        false ->
            false;
        true ->
            case get_protocol(IdP, IdPConfig) of
                openid ->
                    true;
                OtherProtocol ->
                    ?warning(
                        "Offline access can only be enabled for the openid protocol - "
                        "please adjust the auth.config entry for ~p (~p protocol)",
                        [IdP, OtherProtocol]
                    ),
                    false
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the config of the SAML Service Provider represented by this Onezone.
%% @end
%%--------------------------------------------------------------------
-spec get_saml_sp_config() -> #esaml_sp{} | {error, saml_disabled}.
get_saml_sp_config() ->
    case ?CFG_SAML_ENABLED of
        false ->
            {error, saml_disabled};
        true ->
            CertPath = ?CFG_SAML_SP_CERT,
            KeyPath = ?CFG_SAML_SP_KEY,
            RolloverNewCertPath = ?CFG_SAML_SP_ROLLOVER_NEW_CERT,
            RolloverNewKeyPath = ?CFG_SAML_SP_ROLLOVER_NEW_KEY,

            ensure_file_exists(CertPath),
            ensure_file_exists(KeyPath),
            % Can be undefined (optional)
            is_binary(RolloverNewCertPath) andalso ensure_file_exists(RolloverNewCertPath),
            is_binary(RolloverNewKeyPath) andalso ensure_file_exists(RolloverNewKeyPath),

            Cert = esaml_util:load_certificate(CertPath),
            Key = esaml_util:load_private_key(KeyPath),
            RolloverNewCert = case RolloverNewCertPath of
                undefined -> undefined;
                _ -> esaml_util:load_certificate(RolloverNewCertPath)
            end,
            RolloverNewKey = case RolloverNewKeyPath of
                undefined -> undefined;
                _ -> esaml_util:load_private_key(RolloverNewKeyPath)
            end,


            #esaml_sp{
                entity_id = ?CFG_SAML_SP_ENTITY_ID,
                certificate = Cert,
                key = Key,
                rollover_new_certificate = RolloverNewCert,
                rollover_new_key = RolloverNewKey,
                consume_uri = ?CFG_SAML_SP_CONSUME_URI,

                metadata_uri = ?CFG_SAML_SP_METADATA_URI,
                org = #esaml_org{
                    name = ?CFG_SAML_SP_ORGANIZATION_NAME,
                    displayname = ?CFG_SAML_SP_ORGANIZATION_DISPLAY_NAME,
                    url = ?CFG_SAML_SP_ORGANIZATION_URL
                },
                tech = #esaml_contact{
                    name = ?CFG_SAML_SP_CONTACT_NAME,
                    email = ?CFG_SAML_SP_CONTACT_EMAIL
                },
                sign_metadata = ?CFG_SAML_SP_SIGN_METADATA,
                sign_requests = ?CFG_SAML_SP_SIGN_REQUESTS,
                want_assertions_signed = ?CFG_SAML_SP_WANT_ASSERTIONS_SIGNED
            }
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the SAML certificate in PEM format. If a rollover certificate is
%% present, it is returned, otherwise the standard cert.
%% @end
%%--------------------------------------------------------------------
-spec get_saml_cert_pem() -> binary() | {error, saml_disabled}.
get_saml_cert_pem() ->
    case ?CFG_SAML_ENABLED of
        false ->
            {error, saml_disabled};
        true ->
            CertificatePath = case ?CFG_SAML_SP_ROLLOVER_NEW_CERT of
                undefined -> ?CFG_SAML_SP_CERT;
                Path -> Path
            end,
            ensure_file_exists(CertificatePath),
            {ok, CertPem} = file:read_file(CertificatePath),
            CertPem
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns SAML IdP config based on its id.
%% @end
%%--------------------------------------------------------------------
-spec get_saml_idp_config(idp()) -> #esaml_idp{}.
get_saml_idp_config(IdP) ->
    IdPConfig = ?CFG_IDP(IdP),
    #esaml_idp{
        metadata = esaml_util:load_metadata(?CFG_IDP_SAML_METADATA_URL(IdP, IdPConfig)),
        preferred_sso_binding = ?CFG_IDP_SAML_PREFERRED_SSO_BINDING(IdP, IdPConfig)
    }.


-spec force_auth_config_reload() -> ok.
force_auth_config_reload() ->
    simple_cache:clear(cached_auth_config),
    get_auth_config(),
    ok.


-spec ensure_bin(term()) -> undefined | binary().
ensure_bin(undefined) -> undefined;
ensure_bin(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
ensure_bin(List) when is_list(List) -> str_utils:unicode_list_to_binary(List);
ensure_bin(Binary) when is_binary(Binary) -> Binary.


-spec ensure_str(term()) -> undefined | string().
ensure_str(undefined) -> undefined;
ensure_str(Atom) when is_atom(Atom) -> atom_to_list(Atom);
ensure_str(Binary) when is_binary(Binary) -> str_utils:binary_to_unicode_list(Binary);
ensure_str(Str) when is_list(Str) -> Str.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns IdP's config formatted for GUI.
%% @end
%%--------------------------------------------------------------------
-spec format_for_gui(idp(), config_section()) -> json_utils:json_term().
format_for_gui(IdP, IdPConfig) ->
    #{
        <<"id">> => IdP,
        <<"displayName">> => ?CFG_IDP_DISPLAY_NAME(IdP, IdPConfig),
        <<"iconPath">> => ?CFG_IDP_ICON_PATH(IdPConfig),
        <<"iconBackgroundColor">> => ?CFG_IDP_ICON_BACKGROUND(IdPConfig)
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns IdP's config formatted for the configuration endpoint.
%% @end
%%--------------------------------------------------------------------
-spec format_for_configuration(idp(), config_section()) -> json_utils:json_term().
format_for_configuration(IdP, IdPConfig) ->
    #{
        <<"id">> => IdP,
        <<"offlineAccess">> => has_offline_access_enabled(IdP, IdPConfig)
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reads auth config from file and returns it as a map.
%% The config is cached for configurable time.
%% @end
%%--------------------------------------------------------------------
-spec get_auth_config() -> config_v2().
get_auth_config() ->
    case auth_test_mode:process_is_test_mode_enabled() of
        false ->
            {ok, Cfg} = simple_cache:get(cached_auth_config, fun() ->
                {true, fetch_auth_config(), ?CONFIG_CACHE_TTL}
            end),
            Cfg;
        true ->
            get_test_auth_config()
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reads the auth config from file and returns it as a map. If an old version is
%% detected, upgrade is attempted.
%% @end
%%--------------------------------------------------------------------
-spec fetch_auth_config() -> config_v2().
fetch_auth_config() ->
    AuthConfigFile = ?AUTH_CONFIG_FILE,
    case file:consult(AuthConfigFile) of
        {ok, [Cfg = #{version := ?CURRENT_CONFIG_VERSION}]} ->
            simple_cache:put(cached_auth_config, Cfg, ?CONFIG_CACHE_TTL),
            Cfg;
        {ok, _} ->
            try
                UpgradedCfg = upgrade_auth_config(AuthConfigFile),
                simple_cache:put(cached_auth_config, UpgradedCfg, ?CONFIG_CACHE_TTL),
                UpgradedCfg
            catch Type:Reason ->
                ?alert_stacktrace("Failed to upgrade auth.config / saml.config, the "
                "login page will not work. Please upgrade the config manually. "
                "Reason - ~p:~p", [Type, Reason]),
                #{}
            end;
        {error, enoent} ->
            ?alert("auth.config was not found in ~s, the login page will "
            "not work correctly.", [
                AuthConfigFile
            ]),
            #{};
        {error, _} = Error ->
            ?alert("Cannot load auth.config due to ~p. The login page will "
            "not work correctly.", [
                Error
            ]),
            #{}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reads the test auth config from file and returns it as a map. The test auth
%% config can be used to test various IdP configurations on a production system
%% without interfering with the standard login page (the test login page is
%% under /#/test/login).
%% @end
%%--------------------------------------------------------------------
-spec get_test_auth_config() -> config_v2().
get_test_auth_config() ->
    TestAuthConfigFile = ?TEST_AUTH_CONFIG_FILE,
    case file:consult(TestAuthConfigFile) of
        {ok, [Cfg = #{version := ?CURRENT_CONFIG_VERSION}]} ->
            Cfg;
        {ok, _} ->
            ?alert("Badly formatted test.auth.config in ~s, the test "
            "login page will not work.", [TestAuthConfigFile]),
            #{};
        {error, enoent} ->
            ?debug("test.auth.config was not found in ~s, the test "
            "login page will not work.", [TestAuthConfigFile]),
            #{};
        {error, _} = Error ->
            ?alert("Cannot load test.auth.config due to ~p. The login "
            "page will not work.", [Error]),
            #{}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to upgrade the old auth.config file. Uses critical section
%% to ensure no race condition.
%% @end
%%--------------------------------------------------------------------
-spec upgrade_auth_config(file:filename_all()) -> config_v2().
upgrade_auth_config(AuthConfigFile) ->
    critical_section:run(auth_config_upgrade, fun() ->
        {ok, {LastSeen, LastUpgradeResult}} = simple_cache:get(
            previous_auth_cfg_upgrade, fun() -> {false, {<<"">>, #{}}} end
        ),
        case file_md5(AuthConfigFile) of
            LastSeen ->
                % Do not attempt upgrade of the same file multiple times
                LastUpgradeResult;
            _ ->
                % Save empty upgrade result, which will be overwritten by actual
                % result upon success, or will stay empty upon failure
                simple_cache:put(
                    previous_auth_cfg_upgrade, {file_md5(AuthConfigFile), #{}}
                ),
                {ok, [OldAuthCfg]} = file:consult(AuthConfigFile),
                UpgradedCfg = upgrade_auth_config(AuthConfigFile, OldAuthCfg),
                % Success - overwrite empty result
                simple_cache:put(
                    previous_auth_cfg_upgrade, {file_md5(AuthConfigFile), UpgradedCfg}
                ),
                UpgradedCfg
        end
    end).

%% @private
-spec upgrade_auth_config(file:filename_all(), config_v1()) -> config_v2().
upgrade_auth_config(AuthConfigFile, OldAuthCfg) ->
    ?notice("Deprecated auth.config found in ~s - attempting "
    "automatic upgrade to version ~B", [
        AuthConfigFile, ?CURRENT_CONFIG_VERSION
    ]),

    SamlCfgPath = filename:join(
        filename:dirname(AuthConfigFile),
        ?LEGACY_SAML_CONFIG_NAME
    ),
    SamlCfg = case file:consult(SamlCfgPath) of
        {ok, [SCfg]} ->
            ?notice("Deprecated saml.config found in ~s - attempting "
            "automatic upgrade to version ~B", [
                SamlCfgPath, ?CURRENT_CONFIG_VERSION
            ]),
            SCfg;
        _ ->
            #{}
    end,
    UpgradedCfg = auth_config_upgrader:upgrade(
        ?CURRENT_CONFIG_VERSION, OldAuthCfg, SamlCfg
    ),
    AuthConfigBak = AuthConfigFile ++ ?BACKUP_CFG_EXT,
    SamlConfigBak = SamlCfgPath ++ ?BACKUP_CFG_EXT,
    {ok, _} = file:copy(AuthConfigFile, AuthConfigBak),
    case filelib:is_regular(SamlCfgPath) of
        true -> ok = file:rename(SamlCfgPath, SamlConfigBak);
        false -> ok
    end,
    ok = file:write_file(AuthConfigFile, io_lib:format("~tp.~n", [UpgradedCfg])),
    ?notice("Upgrade completed, new config written to ~s. "
    "Moving deprecated auth/saml.config to '~s'", [AuthConfigFile, ?BACKUP_CFG_EXT]),
    ?alert("Make sure to manually update admin groups (fka super groups) in the "
    "config, as their format has changed (their names are now 1:1 with "
    "entitlements in the IdP)"),
    UpgradedCfg.


%% @private
-spec ensure_file_exists(file:filename_all()) -> file:filename_all() | no_return().
ensure_file_exists(Path) ->
    case filelib:is_regular(Path) of
        true ->
            Path;
        false ->
            ?alert("File specified in auth.config does not exist: ~s", [
                Path
            ]),
            throw(?ERROR_BAD_AUTH_CONFIG)
    end.


%% @private
-spec get_default_protocol_config(protocol(), nested_params(), config_policy(), trace()) ->
    term() | no_return().
get_default_protocol_config(Protocol, NestedParams, Policy, Trace) ->
    DefaultIdPConfig = case Protocol of
        openid -> ?CFG_OPENID_DEFAULT_PROTOCOL_CONFIG;
        saml -> ?CFG_SAML_DEFAULT_PROTOCOL_CONFIG;
        onepanelAuth -> #{};
        undefined -> #{}
    end,
    get_nested_cfg(NestedParams, Policy, DefaultIdPConfig, Trace).


%% @private
-spec get_nested_cfg(nested_params(), config_policy()) -> term() | no_return().
get_nested_cfg(NestedParams, Policy) ->
    get_nested_cfg(NestedParams, Policy, get_auth_config(), []).

-spec get_nested_cfg(nested_params(), config_policy(), config_section(), trace()) ->
    term() | no_return().
get_nested_cfg([Key], Policy, Config, Trace) ->
    get_param(Key, Policy, Config, Trace);
get_nested_cfg([Key | Rest], Policy, Config, Trace) ->
    DefaultNestedCfg = case Rest of
        [{proplist, _} | _] -> [];
        _ -> #{}
    end,
    NestedCfg = get_param(Key, {default, DefaultNestedCfg}, Config, Trace),
    get_nested_cfg(Rest, Policy, NestedCfg, Trace ++ [Key]).

-spec get_param(nested_param_key(), config_policy(), config_section()) ->
    term() | no_return().
get_param(Key, Policy, Config) ->
    get_param(Key, Policy, Config, []).

-spec get_param(nested_param_key(), config_policy(), config_section(), trace()) ->
    term() | no_return().
get_param({proplist, Key}, Policy, Config, Trace) ->
    case proplists:lookup(Key, Config) of
        {Key, Value} -> Value;
        none -> param_not_found(Key, Policy, Trace)
    end;
get_param(Key, Policy, Config, Trace) ->
    case maps:find(Key, Config) of
        {ok, Value} -> Value;
        error -> param_not_found(Key, Policy, Trace)
    end.


%% @private
-spec param_not_found(nested_param_key(), config_policy(), trace()) ->
    term() | no_return().
param_not_found(_Key, {default, Default}, _Trace) ->
    Default;
param_not_found(Key, required, Trace) ->
    TraceBinaries = [str_utils:format_bin("~s", [T]) || T <- Trace ++ [Key]],
    ConfigFile = case auth_test_mode:process_is_test_mode_enabled() of
        false -> "auth.config";
        true -> "test.auth.config"
    end,
    ?alert("Missing required parameter in ~s: ~s", [
        ConfigFile, str_utils:join_binary(TraceBinaries, <<" -> ">>)
    ]),
    throw(?ERROR_BAD_AUTH_CONFIG).


%% @private
-spec file_md5(FilePath :: string()) -> binary().
file_md5(FilePath) ->
    {ok, Bin} = file:read_file(FilePath),
    erlang:md5(Bin).
