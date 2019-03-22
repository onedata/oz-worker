%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains eunit tests of auth_config_upgrader module.
%%% @end
%%%-------------------------------------------------------------------
-module(auth_config_upgrader_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include_lib("auth/auth_common.hrl").

-define(AUTH_CONFIG_FILE, auth_config_file).
-define(SAML_CONFIG_FILE, saml_config_file).


idps_config_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"Upgrade auth.config, basic auth disabled", fun upgrade_auth_config_no_onepanel/0},
            {"Upgrade auth.config, basic auth enabled", fun upgrade_auth_config_with_onepanel/0},
            {"Upgrade auth/saml.config, basic auth disabled", fun upgrade_auth_saml_config_no_onepanel/0},
            {"Upgrade auth/saml.config, basic auth enabled", fun upgrade_auth_saml_config_with_onepanel/0},
            {"No upgrade needed", fun no_upgrade_needed/0}
        ]
    }.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    TempDir = mochitemp:mkdtemp(),
    oz_worker:set_env(test_tempdir, TempDir),
    AuthConfigPath = filename:join(TempDir, "auth.config"),
    SamlConfigPath = filename:join(TempDir, "saml.config"),
    oz_worker:set_env(?AUTH_CONFIG_FILE, AuthConfigPath),
    oz_worker:set_env(?SAML_CONFIG_FILE, SamlConfigPath).

teardown(_) ->
    TempDir = oz_worker:get_env(test_tempdir),
    mochitemp:rmtempdir(TempDir).


% Type :: ?AUTH_CONFIG_FILE | ?SAML_CONFIG_FILE
mock_file(Type, undefined) ->
    file:delete(get_path(Type));
mock_file(Type, Cfg) ->
    file:write_file(get_path(Type), io_lib:format("~tp.~n", [Cfg])).


% Type :: ?AUTH_CONFIG_FILE | ?SAML_CONFIG_FILE
get_path(Type) ->
    oz_worker:get_env(Type).


%%%===================================================================
%%% Test functions
%%%===================================================================

upgrade_auth_config_no_onepanel() ->
    OnepanelAuthEnabled = false,
    OpenidEnabled = true,
    SamlEnabled = false,
    check_upgrade_result(OnepanelAuthEnabled, OpenidEnabled, SamlEnabled).

upgrade_auth_config_with_onepanel() ->
    OnepanelAuthEnabled = true,
    OpenidEnabled = true,
    SamlEnabled = false,
    check_upgrade_result(OnepanelAuthEnabled, OpenidEnabled, SamlEnabled).

upgrade_auth_saml_config_no_onepanel() ->
    OnepanelAuthEnabled = false,
    OpenidEnabled = true,
    SamlEnabled = true,
    check_upgrade_result(OnepanelAuthEnabled, OpenidEnabled, SamlEnabled).

upgrade_auth_saml_config_with_onepanel() ->
    OnepanelAuthEnabled = true,
    OpenidEnabled = true,
    SamlEnabled = true,
    check_upgrade_result(OnepanelAuthEnabled, OpenidEnabled, SamlEnabled).

no_upgrade_needed() ->
    OnepanelAuthEnabled = true,
    OpenidEnabled = true,
    SamlEnabled = false,
    NewAuthConfig = expected_auth_config(OnepanelAuthEnabled, OpenidEnabled, SamlEnabled),
    mock_file(?AUTH_CONFIG_FILE, NewAuthConfig),
    auth_config:force_auth_config_reload(),
    ?assertEqual({ok, [NewAuthConfig]}, file:consult(get_path(?AUTH_CONFIG_FILE))).


%%%===================================================================
%%% Helper functions
%%%===================================================================

check_upgrade_result(OnepanelAuthEnabled, OpenidEnabled, SamlEnabled) ->
    OldAuthConfig = old_auth_config(OnepanelAuthEnabled, OpenidEnabled),
    mock_file(?AUTH_CONFIG_FILE, OldAuthConfig),

    OldSamlConfig = case SamlEnabled of
        true -> old_saml_config();
        false -> undefined
    end,
    mock_file(?SAML_CONFIG_FILE, OldSamlConfig),

    auth_config:force_auth_config_reload(),
    Expected = expected_auth_config(OnepanelAuthEnabled, OpenidEnabled, SamlEnabled),
    {ok, [Got]} = file:consult(get_path(?AUTH_CONFIG_FILE)),

    case Expected =:= Got of
        true ->
            true;
        false ->
            io:format(user, "~nExp: ~tp~n", [Expected]),
            io:format(user, "~nGot: ~tp~n", [Got]),
            ?assert(false)
    end,

    case SamlEnabled of
        false ->
            ok;
        true ->
            ?assertEqual({ok, [OldSamlConfig]}, file:consult(get_path(?SAML_CONFIG_FILE) ++ ".bak"))
    end,

    ?assertEqual({ok, [OldAuthConfig]}, file:consult(get_path(?AUTH_CONFIG_FILE) ++ ".bak")).


expected_auth_config(OnepanelAuthEnabled, OpenidEnabled, SamlEnabled) ->
    #{
        version => ?CURRENT_CONFIG_VERSION,
        onepanelAuthConfig =>  expected_onepanelAuthConfig(OnepanelAuthEnabled),
        samlConfig => expected_saml_config(SamlEnabled),
        openidConfig => expected_openidConfig(OpenidEnabled),
        supportedIdps => expected_idps(OnepanelAuthEnabled, OpenidEnabled, SamlEnabled)
    }.



expected_onepanelAuthConfig(Enabled) -> #{
    % Enable onepanelAuth login protocol
    enabled => Enabled
}.


expected_openidConfig(false) -> #{
    enabled => false
};
expected_openidConfig(true) -> #{
    enabled => true,
    defaultProtocolConfig => #{
        plugin => default_oidc_plugin,
        attributeMapping => #{
            subjectId => {required, "sub"},
            alias => {optional, "login"},
            name => {optional, {any, ["name", "login"]}},
            emails => {optional, "email"},
            entitlements => {optional, "groups"},
            custom => {optional, "name"}
        },
        entitlementMapping => #{
            enabled => false
        }
    }
}.


expected_saml_config(false) -> #{
    enabled => false
};
expected_saml_config(true) -> #{
    enabled => true,
    spConfig => #{
        entityId => "https://onedata.org/sp",
        certFile => "/etc/oz_worker/certs/saml_cert.pem",
        keyFile => "/etc/oz_worker/certs/saml_key.pem",
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
            subjectId => {required, "eduPersonTargetedID"},
            alias => {optional, "eduPersonPrincipalName"},
            name => {optional, {any, ["displayName", "surName"]}},
            emails => {optional, "mail"},
            entitlements => {optional, "eduPersonEntitlement"},
            custom => {optional, "eduPersonScopedAffiliation"}
        },
        entitlementMapping => #{
            enabled => false,
            voGroupName => undefined,
            adminGroup => undefined,
            parser => nested_entitlement_parser,
            parserConfig => #{
                splitWith => "/",
                topGroupType => unit,
                topGroupPrivilegesInVo => member,
                subGroupsType => team,
                subGroupsPrivilegesInParent => member,
                userPrivileges => member
            }
        }
    }
}.


expected_idps(OnepanelAuthEnabled, OpenidEnabled, SamlEnabled) ->
    lists:flatten(lists:flatten([
        case OnepanelAuthEnabled of true -> expected_onepanel_auth_idp(); false -> [] end,
        case OpenidEnabled of true -> expected_openid_idps(); false -> [] end,
        case SamlEnabled of true -> expected_saml_idps(); false -> [] end
    ])).


expected_onepanel_auth_idp() ->
    {onepanel, #{
        displayName => "Onepanel account",
        iconPath => "/assets/images/auth-providers/onepanel.svg",
        iconBackgroundColor => "#4BD187",
        protocol => onepanelAuth
    }}.


expected_saml_idps() -> [
    {cern, #{
        displayName => "CERN (eduGAIN)",
        iconBackgroundColor => "#0053A1",
        iconPath => "/assets/images/auth-providers/cern.svg",
        protocol => saml,
        protocolConfig => #{
            metadataUrl => "https://met.refeds.org/met/entity/https%3A//cern.ch/login/?viewxml=true&federation=incommon-federation",
            preferredSsoBinding => http_post,
            attributeMapping => #{
                subjectId => {required, "eduPersonUniqueID"},
                name => {optional, "displayName"},
                alias => {optional, "eduPersonPrincipalName"},
                email => {optional, "mail"},
                entitlements => {optional, "eduPersonAffiliation"},
                custom => undefined
            },
            entitlementMapping => #{
                enabled => false,
                adminGroup => undefined,
                voGroupName => undefined,
                parser => undefined,
                parserConfig => #{}
            }
        }
    }},
    {cnrs, #{
        displayName => "CNRS (eduGAIN)",
        iconBackgroundColor => "#FFF",
        iconPath => "/assets/images/auth-providers/cnrs.svg",
        protocol => saml,
        protocolConfig => #{
            metadataUrl => "https://met.refeds.org/met/entity/https%3A//janus.cnrs.fr/idp/?viewxml=true&federation=federation-education-recherche",
            preferredSsoBinding => http_redirect,
            attributeMapping => #{
                subjectId => {required, "eduPersonTargetedID"},
                name => {optional, "displayName"},
                alias => {optional, "eduPersonPrincipalName"},
                email => {optional, "mail"},
                entitlements => {optional, "eduPersonEntitlement"},
                custom => undefined
            },
            entitlementMapping => #{
                enabled => false,
                adminGroup => undefined,
                voGroupName => undefined,
                parser => undefined,
                parserConfig => #{}
            }
        }
    }},
    {desy, #{
        displayName => "DESY (eduGAIN)",
        iconBackgroundColor => "#FFF",
        iconPath => "/assets/images/auth-providers/desy.svg",
        protocol => saml,
        protocolConfig => #{
            metadataUrl => "https://idp1.desy.de/idp/shibboleth",
            preferredSsoBinding => http_redirect,
            attributeMapping => #{
                subjectId => {required, "eduPersonPrincipalName"},
                name => {optional, "displayName"},
                alias => {optional, "eduPersonPrincipalName"},
                email => {optional, "mail"},
                entitlements => {optional, "eduPersonEntitlement"},
                custom => undefined
            },
            entitlementMapping => #{
                enabled => false,
                adminGroup => undefined,
                voGroupName => undefined,
                parser => undefined,
                parserConfig => #{}
            }
        }
    }},
    {elixir, #{
        displayName => "Elixir",
        iconBackgroundColor => "#FF7A04",
        iconPath => "/assets/images/auth-providers/elixir.svg",
        protocol => saml,
        protocolConfig => #{
            metadataUrl => "https://login.elixir-czech.org/proxy/saml2/idp/metadata.php",
            preferredSsoBinding => http_redirect,
            attributeMapping => #{
                subjectId => {required, "eduPersonUniqueID"},
                name => {optional, "displayName"},
                alias => {optional, "eduPersonPrincipalName"},
                email => {optional, "mail"},
                entitlements => {optional, "eduPersonEntitlement"},
                custom => undefined
            },
            entitlementMapping => #{
                enabled => true,
                adminGroup => "vo:elixir_test/tm:HNSciCloud/tm:tsi-admin",
                voGroupName => undefined,
                parser => nested_entitlement_parser,
                parserConfig => #{
                    splitWith => ":",
                    topGroupType => unit,
                    topGroupPrivilegesInVo => member,
                    subGroupsType => team,
                    subGroupsPrivilegesInParent => member,
                    userPrivileges => member
                }
            }
        }
    }},
    {embl, #{
        displayName => "EMBL (eduGAIN)",
        iconBackgroundColor => "#FFF",
        iconPath => "/assets/images/auth-providers/embl.svg",
        protocol => saml,
        protocolConfig => #{
            metadataUrl => "https://idp.ebi.ac.uk/idp/shibboleth",
            preferredSsoBinding => http_redirect,
            attributeMapping => #{
                subjectId => {required, "eduPersonTargetedID"},
                name => {optional, "displayName"},
                alias => {optional, "eduPersonPrincipalName"},
                email => {optional, "mail"},
                entitlements => {optional, "eduPersonEntitlement"},
                custom => undefined
            },
            entitlementMapping => #{
                enabled => false,
                adminGroup => undefined,
                voGroupName => undefined,
                parser => undefined,
                parserConfig => #{}
            }
        }
    }},
    {esrf, #{
        displayName => "ESRF (eduGAIN)",
        iconBackgroundColor => "#FFF",
        iconPath => "/assets/images/auth-providers/esrf.svg",
        protocol => saml,
        protocolConfig => #{
            metadataUrl => "https://met.refeds.org/met/entity/https%3A//websso.esrf.fr/auth/realms/ESRF/?viewxml=true",
            preferredSsoBinding => http_redirect,
            attributeMapping => #{
                subjectId => {required, "eduPersonTargetedID"},
                name => {optional, "displayName"},
                alias => {optional, "eduPersonPrincipalName"},
                email => {optional, "mail"},
                entitlements => {optional, "eduPersonEntitlement"},
                custom => undefined
            },
            entitlementMapping => #{
                enabled => false,
                adminGroup => undefined,
                voGroupName => undefined,
                parser => undefined,
                parserConfig => #{}
            }
        }
    }},
    {ifae, #{
        displayName => "IFAE (eduGAIN)",
        iconBackgroundColor => "#FFF",
        iconPath => "/assets/images/auth-providers/ifae.jpg",
        protocol => saml,
        protocolConfig => #{
            metadataUrl => "https://met.refeds.org/met/entity/https%3A//www.rediris.es/sir/ifaeidp/?viewxml=true",
            preferredSsoBinding => http_redirect,
            attributeMapping => #{
                subjectId => {required, "eduPersonTargetedID"},
                name => {optional, "displayName"},
                alias => {optional, "eduPersonPrincipalName"},
                email => {optional, "mail"},
                entitlements => {optional, "eduPersonEntitlement"},
                custom => undefined
            },
            entitlementMapping => #{
                enabled => false,
                adminGroup => undefined,
                voGroupName => undefined,
                parser => undefined,
                parserConfig => #{}
            }
        }
    }},
    {infn, #{
        displayName => "INFN (eduGAIN)",
        iconBackgroundColor => "#FFF",
        iconPath => "/assets/images/auth-providers/infn.svg",
        protocol => saml,
        protocolConfig => #{
            metadataUrl => "https://idp.infn.it/saml2/idp/metadata.php",
            preferredSsoBinding => http_redirect,
            attributeMapping => #{
                subjectId => {required, "eduPersonTargetedID"},
                name => {optional, "displayName"},
                alias => {optional, "eduPersonPrincipalName"},
                email => {optional, "mail"},
                entitlements => {optional, "eduPersonEntitlement"},
                custom => undefined
            },
            entitlementMapping => #{
                enabled => false,
                adminGroup => undefined,
                voGroupName => undefined,
                parser => undefined,
                parserConfig => #{}
            }
        }
    }},
    {kit, #{
        displayName => "KIT (eduGAIN)",
        iconBackgroundColor => "#FFF",
        iconPath => "/assets/images/auth-providers/kit.svg",
        protocol => saml,
        protocolConfig => #{
            metadataUrl => "https://met.refeds.org/met/entity/https%3A//idp.scc.kit.edu/idp/shibboleth/?viewxml=true",
            preferredSsoBinding => http_redirect,
            attributeMapping => #{
                subjectId => {required, "eduPersonPrincipalName"},
                name => {optional, "surName"},
                alias => {optional, "eduPersonPrincipalName"},
                email => {optional, "mail"},
                entitlements => {optional, "eduPersonEntitlement"},
                custom => undefined
            },
            entitlementMapping => #{
                enabled => false,
                adminGroup => undefined,
                voGroupName => undefined,
                parser => undefined,
                parserConfig => #{}
            }
        }
    }},
    {stfc, #{
        displayName => "STFC (eduGAIN)",
        iconBackgroundColor => "#1C3764",
        iconPath => "/assets/images/auth-providers/stfc.svg",
        protocol => saml,
        protocolConfig => #{
            metadataUrl => "https://met.refeds.org/met/entity/https%3A//idp02.stfc.ac.uk/shibboleth/?viewxml=true",
            preferredSsoBinding => http_redirect,
            attributeMapping => #{
                subjectId => {required, "eduPersonTargetedID"},
                name => {optional, "displayName"},
                alias => {optional, "eduPersonPrincipalName"},
                email => {optional, "mail"},
                entitlements => {optional, "eduPersonEntitlement"},
                custom => undefined
            },
            entitlementMapping => #{
                enabled => false,
                adminGroup => undefined,
                voGroupName => undefined,
                parser => undefined,
                parserConfig => #{}
            }
        }
    }},
    {surfsara, #{
        displayName => "SURFSara (eduGAIN)",
        iconBackgroundColor => "#FFF",
        iconPath => "/assets/images/auth-providers/surfsara.svg",
        protocol => saml,
        protocolConfig => #{
            metadataUrl => "https://technical.edugain.org/api.php?action=show_entity&row_id=27218",
            preferredSsoBinding => http_redirect,
            attributeMapping => #{
                subjectId => {required, "eduPersonTargetedID"},
                name => {optional, "displayName"},
                alias => {optional, "eduPersonPrincipalName"},
                email => {optional, "mail"},
                entitlements => {optional, "eduPersonEntitlement"},
                custom => undefined
            },
            entitlementMapping => #{
                enabled => false,
                adminGroup => undefined,
                voGroupName => undefined,
                parser => undefined,
                parserConfig => #{}
            }
        }
    }},
    {unitedid, #{
        displayName => "UnitedID",
        iconBackgroundColor => "#ABDFF1",
        iconPath => "/assets/images/auth-providers/unitedid.png",
        protocol => saml,
        protocolConfig => #{
            metadataUrl => "http://md.unitedid.org/idp.xml",
            preferredSsoBinding => http_redirect,
            attributeMapping => #{
                subjectId => {required, "eduPersonTargetedID"},
                name => {optional, {join," ","displayName"}},
                alias => {optional, "eduPersonPrincipalName"},
                email => {optional, "eduPersonPrincipalName"},
                entitlements => undefined,
                custom => undefined
            },
            entitlementMapping => #{
                enabled => false,
                adminGroup => undefined,
                voGroupName => undefined,
                parser => undefined,
                parserConfig => #{ }
            }
        }
    }}
].


expected_openid_idps() -> [
    {google, #{
        displayName => "Google+",
        iconBackgroundColor => "#F1514F",
        iconPath => "/assets/images/auth-providers/google.svg",
        protocol => openid,
        protocolConfig => #{
            plugin => default_oidc_plugin,
            pluginConfig => #{
                clientId => "app-id-google",
                clientSecret => "app-secret-google",
                endpoints => #{
                    xrds => "https://accounts.google.com/.well-known/openid-configuration",
                    accessToken => {xrds, "token_endpoint"},
                    authorize => {xrds, "authorization_endpoint"},
                    userInfo => {xrds, "userinfo_endpoint"}
                },
                scope => "openid email profile",
                offlineAccess => false,
                prompt => undefined,
                accessTokenAcquireMethod => post,
                clientSecretPassMethod => urlencoded,
                accessTokenPassMethod => urlencoded,
                customData => #{}
            },
            authorityDelegation => #{
                enabled => false,
                tokenPrefix => "google:"
            },
            attributeMapping => #{
                subjectId => {required, "sub"},
                name => {optional, {any, ["name", "login"]}},
                alias => {optional, "login"},
                emails => {optional, "email"},
                entitlements => {optional, "groups"},
                custom => undefined
            },
            entitlementMapping => #{
                enabled => false,
                adminGroup => undefined,
                voGroupName => undefined,
                parser => undefined,
                parserConfig => #{}
            }
        }
    }},
    {facebook, #{
        displayName => "Facebook",
        iconBackgroundColor => "#5B87C5",
        iconPath => "/assets/images/auth-providers/facebook.svg",
        protocol => openid,
        protocolConfig => #{
            plugin => default_oidc_plugin,
            pluginConfig => #{
                clientId => "app-id-facebook",
                clientSecret => "app-secret-facebook",
                endpoints => #{
                    accessToken => "https://graph.facebook.com/oauth/access_token",
                    authorize => "https://www.facebook.com/dialog/oauth",
                    userInfo => "https://graph.facebook.com/me"
                },
                scope => "email",
                offlineAccess => false,
                prompt => undefined,
                accessTokenAcquireMethod => get,
                clientSecretPassMethod => urlencoded,
                accessTokenPassMethod => urlencoded,
                customData => #{
                    userInfo => #{
                        parameters => #{"fields" => "email,name"}
                    }
                }
            },
            authorityDelegation => #{
                enabled => false,
                tokenPrefix => "facebook:"
            },
            attributeMapping => #{
                subjectId => {required, "id"},
                name => {optional, {any, ["name", "login"]}},
                alias => {optional, "login"},
                emails => {optional, "email"},
                entitlements => {optional, "groups"},
                custom => undefined
            },
            entitlementMapping => #{
                enabled => false,
                adminGroup => undefined,
                voGroupName => undefined,
                parser => undefined,
                parserConfig => #{}
            }
        }
    }},
    {github, #{
        displayName => "GitHub",
        iconBackgroundColor => "#1E2325",
        iconPath => "/assets/images/auth-providers/github.svg",
        protocol => openid,
        protocolConfig => #{
            plugin => default_oidc_plugin,
            pluginConfig => #{
                accessTokenAcquireMethod => post,
                accessTokenPassMethod => urlencoded,
                clientId => "app-id-github",
                clientSecret => "app-secret-github",
                clientSecretPassMethod => urlencoded,
                customData => #{
                    userInfo => #{
                        headers => #{"User-Agent" => "Onedata"}
                    }
                },
                endpoints => #{
                    accessToken => "https://github.com/login/oauth/access_token",
                    authorize => "https://github.com/login/oauth/authorize",
                    userInfo => [
                        "https://api.github.com/user",
                        {"emails", "https://api.github.com/user/emails"}
                    ]
                },
                scope => "user,user:email",
                offlineAccess => false,
                prompt => undefined
            },
            authorityDelegation => #{
                enabled => false,
                tokenPrefix => "github:"
            },
            attributeMapping => #{
                alias => {optional, "login"},
                custom => undefined,
                emails => {optional, {nested, ["emails", {list, "email"}]}},
                entitlements => {optional, "groups"},
                subjectId => {required, "id"},
                name => {optional, {any, ["name", "login"]}}
            },
            entitlementMapping => #{
                enabled => false,
                adminGroup => undefined,
                voGroupName => undefined,
                parser => undefined,
                parserConfig => #{}
            }
        }
    }},
    {plgrid, #{
        displayName => "PLGrid OpenID",
        iconBackgroundColor => "#026381",
        iconPath => "/assets/images/auth-providers/plgrid.svg",
        protocol => openid,
        protocolConfig => #{
            plugin => plgrid_oidc_plugin,
            pluginConfig => #{
                xrds_endpoint => "https://openid.plgrid.pl/gateway"
            },
            attributeMapping => #{
                alias => {optional, "openid.sreg.nickname"},
                custom => undefined,
                emails => {optional, "openid.sreg.email"},
                entitlements => {optional, "openid.ext1.value.teams"},
                subjectId => {required, "openid.sreg.nickname"},
                name => {required, "openid.sreg.fullname"}
            },
            authorityDelegation => #{
                enabled => false,
                tokenPrefix => "plgrid:"
            },
            entitlementMapping => #{
                enabled => false,
                adminGroup => undefined,
                voGroupName => undefined,
                parser => flat_entitlement_parser,
                parserConfig => #{
                    groupPrivilegesInVo => member,
                    groupType => team,
                    userPrivileges => member
                }
            }
        }
    }},
    {indigo, #{
        displayName => "Indigo",
        iconBackgroundColor => "#341246",
        iconPath => "/assets/images/auth-providers/indigo.svg",
        protocol => openid,
        protocolConfig => #{
            plugin => default_oidc_plugin,
            pluginConfig => #{
                clientId => "app-id-indigo",
                clientSecret => "app-secret-indigo",
                endpoints => #{
                    accessToken => {xrds, "token_endpoint"},
                    authorize => {xrds, "authorization_endpoint"},
                    userInfo => {xrds, "userinfo_endpoint"},
                    xrds => "https://iam-test.indigo-datacloud.eu/.well-known/openid-configuration"
                },
                scope => "openid email profile",
                offlineAccess => false,
                prompt => undefined,
                accessTokenAcquireMethod => post,
                accessTokenPassMethod => urlencoded,
                clientSecretPassMethod => inAuthHeader,
                customData => #{}
            },
            authorityDelegation => #{
                enabled => true,
                tokenPrefix => "indigo:"
            },
            attributeMapping => #{
                subjectId => {required, "sub"},
                name => {required, {any, ["name", {concat, ["given_name", {str, " "}, "family_name"]}]}},
                alias => {optional, "preferred_username"},
                emails => {optional, "email"},
                entitlements => {optional, "groups"},
                custom => undefined
            },
            entitlementMapping => #{
                enabled => true,
                adminGroup => undefined,
                voGroupName => "INDIGO-DataCloud",
                parser => flat_entitlement_parser,
                parserConfig => #{
                    groupType => team,
                    groupPrivilegesInVo => member,
                    userPrivileges => member
                }
            }
        }
    }},
    {egi, #{
        displayName => "EGI",
        iconBackgroundColor => "#0455A1",
        iconPath => "/assets/images/auth-providers/egi.svg",
        protocol => openid,
        protocolConfig => #{
            plugin => default_oidc_plugin,
            pluginConfig => #{
                clientId => "app-id-egi",
                clientSecret => "app-secret-egi",
                endpoints => #{
                    accessToken => {xrds, "token_endpoint"},
                    authorize => {xrds, "authorization_endpoint"},
                    userInfo => {xrds, "userinfo_endpoint"},
                    xrds => "https://aai.egi.eu/oidc/.well-known/openid-configuration"
                },
                scope => "openid email profile",
                offlineAccess => false,
                prompt => undefined,
                accessTokenAcquireMethod => post,
                accessTokenPassMethod => urlencoded,
                clientSecretPassMethod => inAuthHeader,
                customData => #{}
            },
            authorityDelegation => #{
                enabled => false,
                tokenPrefix => "egi:"
            },
            attributeMapping => #{
                subjectId => {required, "sub"},
                name => {required, {any, ["name", {concat, ["given_name", {str, " "}, "family_name"]}]}},
                alias => {optional, "preferred_username"},
                emails => {optional, "email"},
                entitlements => {optional, "edu_person_entitlements"},
                custom => undefined
            },
            entitlementMapping => #{
                enabled => true,
                adminGroup => "admins",
                voGroupName => "EGI-Engage",
                parser => custom_entitlement_parser,
                parserConfig => #{}
            }
        }
    }},
    {rhea, #{
        displayName => "RHEA KeyCloak",
        iconBackgroundColor => "#B51017",
        iconPath => "/assets/images/auth-providers/rhea.svg",
        protocol => openid,
        protocolConfig => #{
            plugin => default_oidc_plugin,
            pluginConfig => #{
                clientId => "app-id-rhea",
                clientSecret => "app-secret-rhea",
                endpoints => #{
                    accessToken => {xrds, "token_endpoint"},
                    authorize => {xrds, "authorization_endpoint"},
                    userInfo => {xrds, "userinfo_endpoint"},
                    xrds => "https://fed-id.nuv.la/auth/realms/SixSq/.well-known/openid-configuration"
                },
                scope => "openid email profile",
                offlineAccess => false,
                prompt => undefined,
                accessTokenAcquireMethod => post,
                accessTokenPassMethod => inAuthHeader,
                clientSecretPassMethod => urlencoded,
                customData => #{}
            },
            authorityDelegation => #{
                enabled => true,
                tokenPrefix => "rhea/"
            },
            attributeMapping => #{
                subjectId => {required, "sub"},
                name => {optional, {any, ["name", "login"]}},
                alias => {optional, "username"},
                emails => {optional, "email"},
                entitlements => {optional, {append, ["groups", "roles"]}},
                custom => undefined
            },
            entitlementMapping => #{
                enabled => true,
                adminGroup => "vo:HNSciCloud-RHEA/rl:one-data-special-role",
                voGroupName => "HNSciCloud-RHEA",
                parser => nested_entitlement_parser,
                parserConfig => #{
                    splitWith => "/",
                    topGroupType => unit,
                    topGroupPrivilegesInVo => member,
                    subGroupsType => team,
                    subGroupsPrivilegesInParent => member,
                    userPrivileges => member
                }
            }
        }
    }}
].


% [cern,cnrs,desy,elixir,embl,esrf,ifae,infn,kit,stfc,surfsara,unitedid]
old_saml_config() -> #{
    sp_config => #{
        entity_id => "https://onedata.org/sp",
        cert_file => "/etc/oz_worker/certs/saml_cert.pem",
        key_file => "/etc/oz_worker/certs/saml_key.pem",
        organization_name => "Onedata",
        organization_display_name => "Onedata",
        tech_contact_name => "John Doe",
        tech_contact_email => "john.doe@onedata.org",
        sign_metadata => false,
        sign_requests => true,
        want_assertions_signed => true
    },

    supported_idps => #{

        cern => #{
            metadata_url => "https://met.refeds.org/met/entity/https%3A//cern.ch/login/?viewxml=true&federation=incommon-federation",
            preferred_sso_binding => http_post,
            attribute_mapping => #{
                subjectId => eduPersonUniqueId,
                login => eduPersonPrincipalName,
                name => displayName,
                email => mail,
                groups => eduPersonAffiliation
            }
        },

        cnrs => #{
            metadata_url => "https://met.refeds.org/met/entity/https%3A//janus.cnrs.fr/idp/?viewxml=true&federation=federation-education-recherche",
            preferred_sso_binding => http_redirect,
            attribute_mapping => #{
                subjectId => eduPersonTargetedID,
                login => eduPersonPrincipalName,
                name => displayName,
                email => mail,
                groups => eduPersonEntitlement
            }
        },

        desy => #{
            metadata_url => "https://idp1.desy.de/idp/shibboleth",
            preferred_sso_binding => http_redirect,
            attribute_mapping => #{
                subjectId => eduPersonPrincipalName,
                login => eduPersonPrincipalName,
                name => displayName,
                email => mail,
                groups => eduPersonEntitlement
            }
        },

        elixir => #{
            metadata_url => "https://login.elixir-czech.org/proxy/saml2/idp/metadata.php",
            preferred_sso_binding => http_redirect,
            attribute_mapping => #{
                subjectId => eduPersonUniqueId,
                login => eduPersonPrincipalName,
                name => displayName,
                email => mail,
                groups => eduPersonEntitlement
            },
            group_mapping => #{
                enabled => true,
                super_group => "vo:elixir_test/tm:HNSciCloud/tm:tsi-admin"
            }
        },

        embl => #{
            metadata_url => "https://idp.ebi.ac.uk/idp/shibboleth",
            preferred_sso_binding => http_redirect,
            attribute_mapping => #{
                subjectId => eduPersonTargetedID,
                login => eduPersonPrincipalName,
                name => displayName,
                email => mail,
                groups => eduPersonEntitlement
            }
        },

        esrf => #{
            metadata_url => "https://met.refeds.org/met/entity/https%3A//websso.esrf.fr/auth/realms/ESRF/?viewxml=true",
            preferred_sso_binding => http_redirect,
            attribute_mapping => #{
                subjectId => eduPersonTargetedID,
                login => eduPersonPrincipalName,
                name => displayName,
                email => mail,
                groups => eduPersonEntitlement
            }
        },

        ifae => #{
            metadata_url => "https://met.refeds.org/met/entity/https%3A//www.rediris.es/sir/ifaeidp/?viewxml=true",
            preferred_sso_binding => http_redirect,
            attribute_mapping => #{
                subjectId => eduPersonTargetedID,
                login => eduPersonPrincipalName,
                name => displayName,
                email => mail,
                groups => eduPersonEntitlement
            }
        },

        infn => #{
            metadata_url => "https://idp.infn.it/saml2/idp/metadata.php",
            preferred_sso_binding => http_redirect,
            attribute_mapping => #{
                subjectId => eduPersonTargetedID,
                login => eduPersonPrincipalName,
                name => displayName,
                email => mail,
                groups => eduPersonEntitlement
            }
        },

        kit => #{
            metadata_url => "https://met.refeds.org/met/entity/https%3A//idp.scc.kit.edu/idp/shibboleth/?viewxml=true",
            preferred_sso_binding => http_redirect,
            attribute_mapping => #{
                subjectId => eduPersonPrincipalName,
                login => eduPersonPrincipalName,
                name => surName,
                email => mail,
                groups => eduPersonEntitlement
            }
        },

        stfc => #{
            metadata_url => "https://met.refeds.org/met/entity/https%3A//idp02.stfc.ac.uk/shibboleth/?viewxml=true",
            preferred_sso_binding => http_redirect,
            attribute_mapping => #{
                subjectId => eduPersonTargetedID,
                login => eduPersonPrincipalName,
                name => displayName,
                email => mail,
                groups => eduPersonEntitlement
            }
        },

        surfsara => #{
            metadata_url => "https://technical.edugain.org/api.php?action=show_entity&row_id=27218",
            preferred_sso_binding => http_redirect,
            attribute_mapping => #{
                subjectId => eduPersonTargetedID,
                login => eduPersonPrincipalName,
                name => displayName,
                email => mail,
                groups => eduPersonEntitlement
            }
        },

        unitedid => #{
            metadata_url => "http://md.unitedid.org/idp.xml",
            preferred_sso_binding => http_redirect,
            attribute_mapping => #{
                subjectId => eduPersonTargetedID,
                login => eduPersonPrincipalName,
                email => eduPersonPrincipalName
            }
        }
    }
}.


old_auth_config(OnepanelAuthEnabled, OpenidEnabled) -> lists:flatten([
    case OnepanelAuthEnabled of
        true -> {basicAuth, []};
        false -> []
    end,
    case OpenidEnabled of
        true -> old_openid_idps();
        false -> []
    end
]).

old_openid_idps() -> [
    {google, [
        % Standard config
        {auth_module, auth_google},
        {app_id, <<"app-id-google">>},
        {app_secret, <<"app-secret-google">>},
        % Provider specific config
        {xrds_endpoint, <<"https://accounts.google.com/.well-known/openid-configuration">>}
    ]},

    {facebook, [
        % Standard config
        {auth_module, auth_facebook},
        {app_id, <<"app-id-facebook">>},
        {app_secret, <<"app-secret-facebook">>},
        % Provider specific config
        {authorize_endpoint, <<"https://www.facebook.com/dialog/oauth">>},
        {access_token_endpoint, <<"https://graph.facebook.com/oauth/access_token">>},
        {user_info_endpoint, <<"https://graph.facebook.com/me">>}
    ]},

    {github, [
        % Standard config
        {auth_module, auth_github},
        {app_id, <<"app-id-github">>},
        {app_secret, <<"app-secret-github">>},
        % Provider specific config
        {authorize_endpoint, <<"https://github.com/login/oauth/authorize">>},
        {access_token_endpoint, <<"https://github.com/login/oauth/access_token">>},
        {user_info_endpoint, <<"https://api.github.com/user">>},
        {user_emails_endpoint, <<"https://api.github.com/user/emails">>}
    ]},

    {plgrid, [
        % Standard config
        {auth_module, auth_plgrid},
        % Provider specific config
        {xrds_endpoint, <<"https://openid.plgrid.pl/gateway">>},
        {logout_endpoint, <<"https://openid.plgrid.pl/logout">>}
    ]},

    {indigo, [
        {auth_module, auth_indigo},
        {app_id, <<"app-id-indigo">>},
        {app_secret, <<"app-secret-indigo">>},
        % Provider specific config
        {xrds_endpoint, <<"https://iam-test.indigo-datacloud.eu/.well-known/openid-configuration">>},
        {authority_delegation, [
            {enabled, true},
            {token_prefix, <<"indigo:">>}
        ]},
        {group_mapping, [
            {enabled, true},
            {vo_group_id, <<"INDIGO-DataCloud">>},
            % Optional field
            {super_group, undefined}
        ]}
    ]},

    {egi, [
        % Standard config
        {auth_module, auth_egi},
        {app_id, <<"app-id-egi">>},
        {app_secret, <<"app-secret-egi">>},
        % Provider specific config
        {xrds_endpoint, <<"https://aai.egi.eu/oidc/.well-known/openid-configuration">>},
        {group_mapping, [
            {enabled, true},
            % Optional field
            {vo_group_id, "EGI-Engage"},
            {super_group, "admins"}
        ]}
    ]},

    {rhea, [
        % Standard config
        {auth_module, auth_keycloak},
        {app_id, <<"app-id-rhea">>},
        {app_secret, <<"app-secret-rhea">>},
        % Provider specific config
        {xrds_endpoint, <<"https://fed-id.nuv.la/auth/realms/SixSq/.well-known/openid-configuration">>},
        {authority_delegation, [
            {enabled, true},
            {token_prefix, <<"rhea/">>}
        ]},
        {group_mapping, [
            {enabled, true},
            % All groups from this IdP will belong to the below VO group.
            {vo_group_id, <<"HNSciCloud-RHEA">>},
            % attributes_to_map - which attributes sent by IdP should be mapped
            % to groups (each attribute value is expected to hold a list of strings).
            % Config format: list of tuples {A, B, C}:
            %   A -> attribute key
            %   B -> derived group type (role | team | unit | organization)
            %   C -> expected group structure, one of:
            %     * flat -> all groups will be a direct child of the VO group,
            %       names of groups will be the same as in the attribute.
            %     * {nested, SplitWith} -> each group membership will be split
            %       into a hierarchical structure using the specified split pattern.
            %       E.g. for string <<"a/b/c">> and SplitWith = <<"/">>, three
            %       nested groups will be created, with user belonging to the
            %       last one: VO <- a <- b <- c <- user.
            {attributes_to_map, [
                {<<"groups">>, team, {nested, <<"/">>}},
                {<<"roles">>, role, flat},
                {<<"entitlement">>, role, flat}
            ]},
            % Optional field
            {super_group, <<"vo:HNSciCloud-RHEA/rl:one-data-special-role">>}
        ]}
    ]}
].










-endif.