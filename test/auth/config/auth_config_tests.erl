%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains eunit tests of flat_entitlement_parser module.
%%% @end
%%%-------------------------------------------------------------------
-module(auth_config_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("auth/auth_errors.hrl").
-include("http/gui_paths.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("esaml/include/esaml.hrl").

-define(DUMMY_AUTH_CONFIG_PATH, "/tmp/dummy-auth.config").

-define(DUMMY_SAML_CERT_PATH, "/tmp/dummy-saml-cert.pem").
-define(DUMMY_SAML_CERT, <<"saml-cert">>).

-define(DUMMY_SAML_KEY_PATH, "/tmp/dummy-saml-key.pem").
-define(DUMMY_SAML_KEY, <<"saml-key">>).

-define(DUMMY_SAML_ROLLOVER_CERT_PATH, "/tmp/dummy-saml-rollover-cert.pem").
-define(DUMMY_SAML_ROLLOVER_CERT, <<"saml-rollover-cert">>).

-define(DUMMY_SAML_ROLLOVER_KEY_PATH, "/tmp/dummy-saml-rollover-key.pem").
-define(DUMMY_SAML_ROLLOVER_KEY, <<"saml-rollover-key">>).

-define(ELIXIR_METADATA_URL, "https://elixir.org/sp.xml").
-define(CERN_METADATA_URL, "https://cern.org/sp.xml").

-define(DUMMY_SAML_METADATA(IdPStr), #esaml_idp_metadata{entity_id = IdPStr}).

-define(DUMMY_ONEZONE_DOMAIN, "onezone.org").


-define(TEST_CASES, [
    {"get SAML SP config", fun get_saml_sp_config/0},
    {"IdP exists", fun idp_exists/0},
    {"get SAML cert in PEM format", fun get_saml_cert_pem/0},
    {"get SAML IdP config", fun get_saml_idp_config/0},
    {"get supported IdPs in GUI format", fun get_supported_idps_in_gui_format/0},
    {"get supported IdPs in configuration format", fun get_supported_idps_in_configuration_format/0},
    {"get attribute mapping", fun get_attribute_mapping/0},
    {"get entitlement mapping config", fun get_entitlement_mapping_config/0},
    {"get authority delegation config", fun get_authority_delegation_config/0},
    {"get idps with offline access", fun get_idps_with_offline_access/0},
    {"has offline access enabled", fun has_offline_access_enabled/0}
]).


production_auth_config_test_() ->
    {foreach, local, % 'local' runs setup/teardown and the test in the same process
        fun setup_production_auth_config/0,
        fun teardown/1,
        ?TEST_CASES
    }.


test_auth_config_test_() ->
    {foreach, local, % 'local' runs setup/teardown and the test in the same process
        fun setup_test_auth_config/0,
        fun teardown/1,
        ?TEST_CASES
    }.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


setup_production_auth_config() ->
    setup(production).


setup_test_auth_config() ->
    setup(test).


%% Type :: production | test
setup(Type) ->
    meck:new(file, [unstick, passthrough]),
    meck:expect(file, consult, fun
        (?DUMMY_AUTH_CONFIG_PATH) -> {ok, [get_mock_config_file()]};
        (_) -> {error, enoent}
    end),
    meck:expect(file, read_file, fun
        F(?DUMMY_SAML_CERT_PATH) -> {ok, ?DUMMY_SAML_CERT};
        F(?DUMMY_SAML_ROLLOVER_CERT_PATH) -> {ok, ?DUMMY_SAML_ROLLOVER_CERT};
        F(?DUMMY_SAML_KEY_PATH) -> {ok, ?DUMMY_SAML_KEY};
        F(?DUMMY_SAML_ROLLOVER_KEY_PATH) -> {ok, ?DUMMY_SAML_ROLLOVER_KEY};
        F(List) when is_list(List) -> F(list_to_binary(List));
        F(_) -> {error, enoent}
    end),

    meck:new(filelib, [unstick, passthrough]),
    meck:expect(filelib, is_regular, fun
        F(?DUMMY_SAML_CERT_PATH) -> true;
        F(?DUMMY_SAML_ROLLOVER_CERT_PATH) -> true;
        F(?DUMMY_SAML_KEY_PATH) -> true;
        F(?DUMMY_SAML_ROLLOVER_KEY_PATH) -> true;
        F(List) when is_list(List) -> F(list_to_binary(List));
        F(_) -> false
    end),

    set_mock_config_file(mock_auth_config_file()),

    meck:new(esaml_util, []),
    meck:expect(esaml_util, load_certificate, fun
        F(?DUMMY_SAML_CERT_PATH) -> ?DUMMY_SAML_CERT;
        F(?DUMMY_SAML_ROLLOVER_CERT_PATH) -> ?DUMMY_SAML_ROLLOVER_CERT;
        F(List) when is_list(List) -> F(list_to_binary(List))
    end),
    meck:expect(esaml_util, load_private_key, fun
        F(?DUMMY_SAML_KEY_PATH) -> ?DUMMY_SAML_KEY;
        F(?DUMMY_SAML_ROLLOVER_KEY_PATH) -> ?DUMMY_SAML_ROLLOVER_KEY;
        F(List) when is_list(List) -> F(list_to_binary(List))
    end),
    meck:expect(esaml_util, load_metadata, fun
        (?ELIXIR_METADATA_URL) -> ?DUMMY_SAML_METADATA("elixir");
        (?CERN_METADATA_URL) -> ?DUMMY_SAML_METADATA("cern")
    end),

    case Type of
        production ->
            oz_worker:set_env(auth_config_file, ?DUMMY_AUTH_CONFIG_PATH);
        test ->
            oz_worker:set_env(test_auth_config_file, ?DUMMY_AUTH_CONFIG_PATH),
            auth_test_mode:process_enable_test_mode()
    end,
    oz_worker:set_env(auth_config_cache_ttl, -1),
    oz_worker:set_env(http_domain, ?DUMMY_ONEZONE_DOMAIN).

teardown(_) ->
    ?assert(meck:validate(file)),
    ok = meck:unload(file),
    ?assert(meck:validate(filelib)),
    ok = meck:unload(filelib),
    ?assert(meck:validate(esaml_util)),
    ok = meck:unload(esaml_util),
    auth_test_mode:process_disable_test_mode().

%%%===================================================================
%%% Test functions
%%%===================================================================

get_saml_sp_config() ->
    ExpectedSPConfig = #esaml_sp{
        entity_id = "https://onedata.org/sp",
        certificate = ?DUMMY_SAML_CERT,
        key = ?DUMMY_SAML_KEY,
        rollover_new_certificate = ?DUMMY_SAML_ROLLOVER_CERT,
        rollover_new_key = ?DUMMY_SAML_ROLLOVER_KEY,
        consume_uri = "https://" ++ ?DUMMY_ONEZONE_DOMAIN ++ ?SAML_CONSUME_PATH,
        metadata_uri = "https://" ++ ?DUMMY_ONEZONE_DOMAIN ++ ?SAML_METADATA_PATH,
        org = #esaml_org{
            name = "Onedata",
            displayname = "Onedata",
            url = "https://" ++ ?DUMMY_ONEZONE_DOMAIN
        },
        tech = #esaml_contact{
            name = "John Doe",
            email = "john.doe@onedata.org"
        },
        sign_metadata = false,
        sign_requests = true,
        want_assertions_signed = true
    },
    ?assertEqual(ExpectedSPConfig, auth_config:get_saml_sp_config()),

    % Check behavior when SAML is disabled
    modify_config_mock(fun(MockConfig = #{samlConfig := SamlConfig}) ->
        maps:put(samlConfig, SamlConfig#{enabled => false}, MockConfig)
    end),
    ?assertMatch({error, saml_disabled}, auth_config:get_saml_sp_config()),

    % Check behavior when SAML config is completely inexistent
    modify_config_mock(fun(MockConfig) ->
        maps:put(samlConfig, #{}, MockConfig)
    end),
    ?assertMatch({error, saml_disabled}, auth_config:get_saml_sp_config()).


idp_exists() ->
    ?assert(auth_config:idp_exists(onepanel)),
    ?assert(auth_config:idp_exists(my_idp)),
    ?assert(auth_config:idp_exists(egi)),
    ?assert(auth_config:idp_exists(elixir)),
    ?assert(auth_config:idp_exists(cern)),
    ?assertNot(auth_config:idp_exists(google)),
    ?assertNot(auth_config:idp_exists(facebook)).


get_saml_cert_pem() ->
    % If rollover cert is given, it should be returned
    ?assertEqual(?DUMMY_SAML_ROLLOVER_CERT, auth_config:get_saml_cert_pem()),

    % Check behavior when rollover cert is not given
    modify_config_mock(fun(MockConfig = #{samlConfig := SamlConfig = #{spConfig := SpConfig}}) ->
        maps:put(samlConfig, SamlConfig#{
            spConfig => SpConfig#{
                rolloverNewCertFile => undefined
            }
        }, MockConfig)
    end),
    ?assertMatch(?DUMMY_SAML_CERT, auth_config:get_saml_cert_pem()),

    % Check behavior when no valid cert is given
    modify_config_mock(fun(MockConfig = #{samlConfig := SamlConfig = #{spConfig := SpConfig}}) ->
        maps:put(samlConfig, SamlConfig#{
            spConfig => SpConfig#{
                certFile => undefined
            }
        }, MockConfig)
    end),

    ?assertException(throw, ?ERROR_BAD_AUTH_CONFIG, auth_config:get_saml_cert_pem()),

    % Check behavior when SAML is disabled
    modify_config_mock(fun(MockConfig = #{samlConfig := SamlConfig}) ->
        maps:put(samlConfig, SamlConfig#{enabled => false}, MockConfig)
    end),
    ?assertMatch({error, saml_disabled}, auth_config:get_saml_cert_pem()),

    % Check behavior when SAML config is completely inexistent
    modify_config_mock(fun(MockConfig) ->
        maps:put(samlConfig, #{}, MockConfig)
    end),
    ?assertMatch({error, saml_disabled}, auth_config:get_saml_cert_pem()).


get_saml_idp_config() ->
    ElixirIdPConfig = #esaml_idp{
        metadata = ?DUMMY_SAML_METADATA("elixir"),
        preferred_sso_binding = http_redirect
    },
    CERNIdPConfig = #esaml_idp{
        metadata = ?DUMMY_SAML_METADATA("cern"),
        preferred_sso_binding = http_post
    },

    ?assertMatch(ElixirIdPConfig, auth_config:get_saml_idp_config(elixir)),
    ?assertMatch(CERNIdPConfig, auth_config:get_saml_idp_config(cern)).


get_supported_idps_in_gui_format() ->
    Entry = fun(IdP, DisplayName, IconPath, IconBgColor) -> #{
        <<"id">> => IdP,
        <<"displayName">> => DisplayName,
        <<"iconPath">> => IconPath,
        <<"iconBackgroundColor">> => IconBgColor
    } end,
    OnepanelEntry = Entry(onepanel, <<"Onepanel account">>, <<"/assets/images/auth-providers/key.svg">>, <<"#4BD187">>),
    MyIdpEntry = Entry(my_idp, <<"My IdP">>, <<"/custom/auth-providers/my-idp.svg">>, <<"#1E2325">>),
    EgiEntry = Entry(egi, <<"EGI">>, <<"/assets/images/auth-providers/default.svg">>, <<"#333">>),
    ElixirEntry = Entry(elixir, <<"Elixir">>, <<"/assets/images/auth-providers/elixir.svg">>, <<"#FF7A04">>),
    CERNEntry = Entry(cern, <<"CERN">>, <<"/assets/images/auth-providers/cern.svg">>, <<"#0053A1">>),

    get_supported_idps_base(
        fun auth_config:get_supported_idps_in_gui_format/0,
        OnepanelEntry, MyIdpEntry, EgiEntry, ElixirEntry, CERNEntry
    ).


get_supported_idps_in_configuration_format() ->
    Entry = fun(IdP, OfflineAccess) -> #{
        <<"id">> => IdP,
        <<"offlineAccess">> => OfflineAccess
    } end,
    OnepanelEntry = Entry(onepanel, false),
    MyIdpEntry = Entry(my_idp, true),
    EgiEntry = Entry(egi, false),
    ElixirEntry = Entry(elixir, false),
    CERNEntry = Entry(cern, false),

    get_supported_idps_base(
        fun auth_config:get_supported_idps_in_configuration_format/0,
        OnepanelEntry, MyIdpEntry, EgiEntry, ElixirEntry, CERNEntry
    ).


get_supported_idps_base(GetIdPsFun, OnepanelEntry, MyIdpEntry, EgiEntry, ElixirEntry, CERNEntry) ->
    ?assertMatch([OnepanelEntry, MyIdpEntry, EgiEntry, ElixirEntry, CERNEntry], GetIdPsFun()),

    % Check if switching on/off certain protocols modifies IdPs list
    modify_config_mock(fun(MockConfig = #{samlConfig := SamlConfig}) ->
        maps:put(samlConfig, SamlConfig#{enabled => false}, MockConfig)
    end),
    ?assertMatch([OnepanelEntry, MyIdpEntry, EgiEntry], GetIdPsFun()),

    modify_config_mock(fun(MockConfig = #{openidConfig := SamlConfig}) ->
        maps:put(openidConfig, SamlConfig#{enabled => false}, MockConfig)
    end),
    ?assertMatch([OnepanelEntry], GetIdPsFun()),

    modify_config_mock(fun(MockConfig = #{onepanelAuthConfig := SamlConfig}) ->
        maps:put(onepanelAuthConfig, SamlConfig#{enabled => false}, MockConfig)
    end),
    ?assertMatch([], GetIdPsFun()),

    modify_config_mock(fun(MockConfig = #{openidConfig := SamlConfig}) ->
        maps:put(openidConfig, SamlConfig#{enabled => true}, MockConfig)
    end),
    ?assertMatch([MyIdpEntry, EgiEntry], GetIdPsFun()),

    modify_config_mock(fun(MockConfig = #{samlConfig := SamlConfig}) ->
        maps:put(samlConfig, SamlConfig#{enabled => true}, MockConfig)
    end),
    ?assertMatch([MyIdpEntry, EgiEntry, ElixirEntry, CERNEntry], GetIdPsFun()),

    modify_config_mock(fun(MockConfig = #{onepanelAuthConfig := SamlConfig}) ->
        maps:put(onepanelAuthConfig, SamlConfig#{enabled => true}, MockConfig)
    end),
    ?assertMatch([OnepanelEntry, MyIdpEntry, EgiEntry, ElixirEntry, CERNEntry], GetIdPsFun()),

    % Simulate no config file at all
    oz_worker:set_env(auth_config_file, "inexistent-file.config"),
    oz_worker:set_env(test_auth_config_file, "inexistent-file.config"),
    ?assertMatch([], GetIdPsFun()).


get_attribute_mapping() ->
    % Check if attribute mapping rules are properly inherited and overwritten
    ?assertEqual({required, "sub"}, auth_config:get_attribute_mapping(egi, subjectId)),
    ?assertEqual({required, {any, ["name", "login"]}}, auth_config:get_attribute_mapping(egi, name)),
    ?assertEqual(undefined, auth_config:get_attribute_mapping(egi, alias)),
    ?assertEqual(undefined, auth_config:get_attribute_mapping(egi, emails)),
    ?assertEqual({required, "groups"}, auth_config:get_attribute_mapping(egi, entitlements)),
    ?assertEqual({optional, "organization"}, auth_config:get_attribute_mapping(egi, custom)),

    ?assertEqual({required, "id"}, auth_config:get_attribute_mapping(my_idp, subjectId)),
    ?assertEqual({required, {any, ["name", "login"]}}, auth_config:get_attribute_mapping(my_idp, name)),
    ?assertEqual({optional, "login"}, auth_config:get_attribute_mapping(my_idp, alias)),
    ?assertEqual({optional, "email"}, auth_config:get_attribute_mapping(my_idp, emails)),
    ?assertEqual({optional, "edu_person_entitlements"}, auth_config:get_attribute_mapping(my_idp, entitlements)),
    ?assertEqual(undefined, auth_config:get_attribute_mapping(my_idp, custom)),

    ?assertEqual({required, eduPersonUniqueId}, auth_config:get_attribute_mapping(elixir, subjectId)),
    ?assertEqual({required, {any, [displayName, surName]}}, auth_config:get_attribute_mapping(elixir, name)),
    ?assertEqual({optional, eduPersonPrincipalName}, auth_config:get_attribute_mapping(elixir, alias)),
    ?assertEqual({optional, mail}, auth_config:get_attribute_mapping(elixir, emails)),
    ?assertEqual(undefined, auth_config:get_attribute_mapping(elixir, entitlements)),
    ?assertEqual({required, 'urn:oid:1.3.6.1.4.1.25178.1.2.9'}, auth_config:get_attribute_mapping(elixir, custom)),

    ?assertEqual({required, eduPersonTargetedID}, auth_config:get_attribute_mapping(cern, subjectId)),
    ?assertEqual({required, displayName}, auth_config:get_attribute_mapping(cern, name)),
    ?assertEqual({optional, eduPersonPrincipalName}, auth_config:get_attribute_mapping(cern, alias)),
    ?assertEqual({optional, mail}, auth_config:get_attribute_mapping(cern, emails)),
    ?assertEqual({optional, eduPersonEntitlement}, auth_config:get_attribute_mapping(cern, entitlements)),
    ?assertEqual({optional, eduPersonScopedAffiliation}, auth_config:get_attribute_mapping(cern, custom)),

    % Check if setting id mapping rule to other than required results in an error
    modify_config_mock(fun(MockConfig = #{supportedIdps := SupportedIdPs}) ->
        MyIdPCfg = #{protocolConfig := PC = #{attributeMapping := AM}} = proplists:get_value(my_idp, SupportedIdPs),
        NewMyIdPCfg = MyIdPCfg#{
            protocolConfig => PC#{
                attributeMapping => AM#{
                    subjectId => {optional, {any, ["id", "sub"]}}
                }
            }
        },
        maps:put(supportedIdps, lists:keyreplace(my_idp, 1, SupportedIdPs, {my_idp, NewMyIdPCfg}), MockConfig)
    end),

    ?assertException(throw, ?ERROR_BAD_AUTH_CONFIG, auth_config:get_attribute_mapping(my_idp, subjectId)).


get_entitlement_mapping_config() ->
    ?assertEqual(false, auth_config:get_entitlement_mapping_config(my_idp, [enabled], {default, false})),
    ?assertEqual(true, auth_config:get_entitlement_mapping_config(egi, [enabled], {default, false})),
    ?assertEqual(true, auth_config:get_entitlement_mapping_config(elixir, [enabled], {default, false})),
    ?assertEqual(true, auth_config:get_entitlement_mapping_config(cern, [enabled], {default, false})),

    ?assertEqual(undefined, auth_config:get_entitlement_mapping_config(my_idp, [voGroupName], {default, undefined})),
    ?assertEqual("EGI", auth_config:get_entitlement_mapping_config(egi, [voGroupName], {default, undefined})),
    ?assertEqual("Elixir", auth_config:get_entitlement_mapping_config(elixir, [voGroupName], {default, undefined})),
    ?assertEqual(undefined, auth_config:get_entitlement_mapping_config(cern, [voGroupName], {default, undefined})),

    ?assertEqual(undefined, auth_config:get_entitlement_mapping_config(my_idp, [adminGroup], {default, undefined})),
    ?assertEqual(undefined, auth_config:get_entitlement_mapping_config(egi, [adminGroup], {default, undefined})),
    ?assertEqual("tsi-admin", auth_config:get_entitlement_mapping_config(elixir, [adminGroup], {default, undefined})),
    ?assertEqual("admins", auth_config:get_entitlement_mapping_config(cern, [adminGroup], {default, undefined})),

    ?assertEqual(undefined, auth_config:get_entitlement_mapping_config(my_idp, [parser], {default, undefined})),
    ?assertEqual(custom_entitlement_parser, auth_config:get_entitlement_mapping_config(egi, [parser], {default, undefined})),
    ?assertEqual(nested_entitlement_parser, auth_config:get_entitlement_mapping_config(elixir, [parser], {default, undefined})),
    ?assertEqual(flat_entitlement_parser, auth_config:get_entitlement_mapping_config(cern, [parser], {default, undefined})),

    ElixirParserConfig = #{
        splitWith => "/",
        topGroupType => team,
        topGroupPrivilegesInVo => member,
        subGroupsType => team,
        subGroupsPrivilegesInParent => member,
        userPrivileges => member
    },
    CernParserConfig = #{
        groupType => team,
        groupPrivilegesInVo => member,
        userPrivileges => manager
    },

    ?assertEqual(#{}, auth_config:get_entitlement_mapping_config(my_idp, [parserConfig], {default, #{}})),
    ?assertEqual(#{}, auth_config:get_entitlement_mapping_config(egi, [parserConfig], {default, #{}})),
    ?assertEqual(ElixirParserConfig, auth_config:get_entitlement_mapping_config(elixir, [parserConfig], {default, #{}})),
    ?assertEqual(CernParserConfig, auth_config:get_entitlement_mapping_config(cern, [parserConfig], {default, #{}})).


get_authority_delegation_config() ->
    ?assertEqual(false, auth_config:get_authority_delegation_config(onepanel)),
    ?assertEqual(false, auth_config:get_authority_delegation_config(my_idp)),
    ?assertEqual({true, <<"egi:">>}, auth_config:get_authority_delegation_config(egi)),
    ?assertEqual(false, auth_config:get_authority_delegation_config(elixir)),
    ?assertEqual(false, auth_config:get_authority_delegation_config(cern)).


get_idps_with_offline_access() ->
    ?assertEqual([my_idp], auth_config:get_idps_with_offline_access()).


has_offline_access_enabled() ->
    ?assertEqual(false, auth_config:has_offline_access_enabled(onepanel)),
    ?assertEqual(true, auth_config:has_offline_access_enabled(my_idp)),
    ?assertEqual(false, auth_config:has_offline_access_enabled(egi)),
    ?assertEqual(false, auth_config:has_offline_access_enabled(elixir)),
    ?assertEqual(false, auth_config:has_offline_access_enabled(cern)).


%%%===================================================================
%%% Internal functions
%%%===================================================================

modify_config_mock(Fun) ->
    MockConfig = get_mock_config_file(),
    set_mock_config_file(Fun(MockConfig)).


set_mock_config_file(NewMockFile) ->
    oz_worker:set_env(mock_config_file, NewMockFile).


get_mock_config_file() ->
    oz_worker:get_env(mock_config_file, #{}).


mock_auth_config_file() ->
    #{
        version => 2,

        onepanelAuthConfig => #{
            enabled => true
        },

        samlConfig => #{
            enabled => true,
            spConfig => #{
                entityId => "https://onedata.org/sp",
                certFile => ?DUMMY_SAML_CERT_PATH,
                keyFile => ?DUMMY_SAML_KEY_PATH,
                rolloverNewCertFile => ?DUMMY_SAML_ROLLOVER_CERT_PATH,
                rolloverNewKeyFile => ?DUMMY_SAML_ROLLOVER_KEY_PATH,
                organizationName => "Onedata",
                organizationDisplayName => "Onedata",
                techContactName => "John Doe",
                techContactEmail => "john.doe@onedata.org",
                signMetadata => false,
                signRequests => true,
                wantAssertionsSigned => true
            },
            defaultProtocolConfig => #{
                preferredSsoBinding => http_redirect,
                attributeMapping => #{
                    subjectId => {required, eduPersonTargetedID},
                    name => {required, displayName},
                    alias => {optional, eduPersonPrincipalName},
                    emails => {optional, mail},
                    entitlements => {optional, eduPersonEntitlement},
                    custom => {optional, eduPersonScopedAffiliation}
                },
                entitlementMapping => #{
                    enabled => true,
                    voGroupName => undefined,
                    adminGroup => "admins",
                    parser => flat_entitlement_parser,
                    parserConfig => #{
                        groupType => team,
                        groupPrivilegesInVo => member,
                        userPrivileges => manager
                    }
                }
            }
        },
        openidConfig => #{
            enabled => true,
            defaultProtocolConfig => #{
                plugin => default_oidc_plugin,
                attributeMapping => #{
                    subjectId => {required, "sub"},
                    name => {required, {any, ["name", "login"]}},
                    alias => {optional, "login"},
                    emails => {optional, "email"},
                    entitlements => {optional, "edu_person_entitlements"},
                    custom => {optional, "organization"}
                },
                entitlementMapping => #{
                    enabled => false
                },
                authorityDelegation => #{
                    enabled => true
                }
            }
        },
        supportedIdps => [
            {onepanel, #{
                displayName => "Onepanel account",
                iconPath => "/assets/images/auth-providers/key.svg",
                iconBackgroundColor => "#4BD187",
                protocol => onepanelAuth
            }},
            {my_idp, #{
                displayName => "My IdP",
                iconPath => "/custom/auth-providers/my-idp.svg",
                iconBackgroundColor => "#1E2325",
                protocol => openid,
                protocolConfig => #{
                    plugin => my_idp_oidc_plugin,
                    pluginConfig => #{
                        clientId => "APP_ID",
                        clientSecret => "APP_SECRET",
                        endpoints => #{
                            xrds => "https://accounts.google.com/.well-known/openid-configuration"
                        }
                    },
                    authorityDelegation => #{
                        enabled => false
                    },
                    offlineAccess => true,
                    attributeMapping => #{
                        subjectId => {required, "id"},
                        custom => undefined
                    },
                    entitlementMapping => #{
                        enabled => false
                    }
                }
            }},
            {egi, #{
                displayName => "EGI",
                protocol => openid,
                protocolConfig => #{
                    pluginConfig => #{
                        clientId => "APP_ID",
                        clientSecret => "APP_SECRET",
                        endpoints => #{
                            xrds => "https://aai.egi.eu/oidc/.well-known/openid-configuration"
                        }
                    },
                    authorityDelegation => #{
                        tokenPrefix => "egi:"
                    },
                    attributeMapping => #{
                        alias => undefined,
                        emails => undefined,
                        entitlements => {required, "groups"}
                    },
                    entitlementMapping => #{
                        enabled => true,
                        voGroupName => "EGI",
                        parser => custom_entitlement_parser
                    }
                }
            }},
            {elixir, #{
                displayName => "Elixir",
                iconPath => "/assets/images/auth-providers/elixir.svg",
                iconBackgroundColor => "#FF7A04",
                protocol => saml,
                protocolConfig => #{
                    metadataUrl => ?ELIXIR_METADATA_URL,
                    attributeMapping => #{
                        subjectId => {required, eduPersonUniqueId},
                        name => {required, {any, [displayName, surName]}},
                        entitlements => undefined,
                        custom => {required, 'urn:oid:1.3.6.1.4.1.25178.1.2.9'}
                    },
                    entitlementMapping => #{
                        voGroupName => "Elixir",
                        adminGroup => "tsi-admin",
                        parser => nested_entitlement_parser,
                        parserConfig => #{
                            splitWith => "/",
                            topGroupType => team,
                            topGroupPrivilegesInVo => member,
                            subGroupsType => team,
                            subGroupsPrivilegesInParent => member,
                            userPrivileges => member
                        }
                    }
                }
            }},
            {cern, #{
                displayName => "CERN",
                iconPath => "/assets/images/auth-providers/cern.svg",
                iconBackgroundColor => "#0053A1",
                protocol => saml,
                protocolConfig => #{
                    metadataUrl => ?CERN_METADATA_URL,
                    preferredSsoBinding => http_post
                }
            }}
        ]
    }.


-endif.