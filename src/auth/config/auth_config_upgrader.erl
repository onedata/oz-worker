%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles upgrades of the auth.config file.
%%% @end
%%%-------------------------------------------------------------------
-module(auth_config_upgrader).

-include_lib("auth/auth_common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([v1_to_v2/2]).
-export([v2_to_v3/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Performs auth.config upgrade from version 1 to 2. Merges auth.config and
%% saml.config (if present) in the process.
%% @end
%%--------------------------------------------------------------------
-spec v1_to_v2(auth_config:config_v1(), auth_config:saml_config_v1()) ->
    auth_config:config_v2_or_later().
v1_to_v2(AuthConfig, SamlConfig) ->
    #{
        version => 2,
        onepanelAuthConfig => onepanel_auth_config_entry(AuthConfig),
        samlConfig => saml_config_entry(SamlConfig),
        openidConfig => openid_config_entry(AuthConfig),
        supportedIdps => supported_idps(AuthConfig, SamlConfig)
    }.


%%--------------------------------------------------------------------
%% @doc
%% Performs auth.config upgrade from version 2 to 3.
%% @end
%%--------------------------------------------------------------------
-spec v2_to_v3(auth_config:config_v2_or_later()) -> auth_config:config_v2_or_later().
v2_to_v3(AuthConfig) ->
    % onepanelAuthConfig renamed to basicAuthConfig
    Cfg2 = rename_nested_key(AuthConfig, [onepanelAuthConfig], basicAuthConfig),

    % 'onepanel' special IdP renamed to 'basicAuth'
    % 'onepanelAuth' protocol renamed to 'basicAuth'
    SupportedIdPs = maps:get(supportedIdps, AuthConfig, []),
    BasicAuthIdP = case proplists:lookup(onepanel, SupportedIdPs) of
        none ->
            [];
        {onepanel, OnepanelIdP} ->
            [{basicAuth, OnepanelIdP#{
                displayName => "username & password",
                iconPath => "/assets/images/auth-providers/basicauth.svg",
                protocol => basicAuth
            }}]
    end,
    SupportedIdPs2 = BasicAuthIdP ++ proplists:delete(onepanel, SupportedIdPs),

    % 'alias' renamed to 'username' (attribute mapping)
    % 'name' renamed to 'fullName' (attribute mapping)
    Cfg3 = rename_nested_key(Cfg2, [openidConfig, defaultProtocolConfig, attributeMapping, alias], username),
    Cfg4 = rename_nested_key(Cfg3, [openidConfig, defaultProtocolConfig, attributeMapping, name], fullName),
    Cfg5 = rename_nested_key(Cfg4, [samlConfig, defaultProtocolConfig, attributeMapping, alias], username),
    Cfg6 = rename_nested_key(Cfg5, [samlConfig, defaultProtocolConfig, attributeMapping, name], fullName),
    Cfg6#{
        version => 3,
        supportedIdps => lists:map(fun({IdP, IdPCfg}) ->
            IdPCfg2 = rename_nested_key(IdPCfg, [protocolConfig, attributeMapping, alias], username),
            IdPCfg3 = rename_nested_key(IdPCfg2, [protocolConfig, attributeMapping, name], fullName),
            {IdP, IdPCfg3}
        end, SupportedIdPs2)
    }.

%%%===================================================================
%%% Helper functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Renames a key in an arbitrarily nested map, if exists.
%% @end
%%--------------------------------------------------------------------
-spec rename_nested_key(map(), NestedKeys :: [atom()], NewKey :: atom()) -> map().
rename_nested_key(Map, [NestedKey | RestKeys], NewKey) ->
    case maps:find(NestedKey, Map) of
        error ->
            Map;
        {ok, NestedValue} ->
            case RestKeys of
                [] ->
                    maps:remove(NestedKey, Map#{NewKey => NestedValue});
                _ ->
                    Map#{NestedKey => rename_nested_key(NestedValue, RestKeys, NewKey)}
            end
    end.

%%%===================================================================
%%% Functions concerning v1 to v2 upgrade
%%%===================================================================

-spec onepanel_auth_config_entry(auth_config:config_v1()) -> auth_config:config_section().
onepanel_auth_config_entry(AuthConfig) ->
    BasicAuthEnabled = case proplists:get_value(basicAuth, AuthConfig) of
        undefined -> false;
        _ -> true
    end,
    #{enabled => BasicAuthEnabled}.


-spec saml_config_entry(auth_config:saml_config_v1()) -> auth_config:config_section().
saml_config_entry(EmptyMap) when map_size(EmptyMap) == 0 -> #{
    enabled => false
};
saml_config_entry(SamlConfig) ->
    SpConfig = maps:get(sp_config, SamlConfig),
    #{
        enabled => true,
        spConfig => #{
            entityId => maps:get(entity_id, SpConfig),
            certFile => maps:get(cert_file, SpConfig),
            keyFile => maps:get(key_file, SpConfig),
            organizationName => maps:get(organization_name, SpConfig),
            organizationDisplayName => maps:get(organization_display_name, SpConfig),
            techContactName => maps:get(tech_contact_name, SpConfig),
            techContactEmail => maps:get(tech_contact_email, SpConfig),
            signMetadata => maps:get(sign_metadata, SpConfig),
            signRequests => maps:get(sign_requests, SpConfig),
            wantAssertionsSigned => maps:get(want_assertions_signed, SpConfig)
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


-spec openid_config_entry(auth_config:config_v1()) -> auth_config:config_section().
openid_config_entry(AuthConfig) ->
    % Check if there are any IdPs except basicAuth
    case proplists:delete(basicAuth, AuthConfig) /= [] of
        false ->
            #{
                enabled => false
            };
        true ->
            #{
                enabled => true,
                defaultProtocolConfig => #{
                    plugin => default_oidc_plugin,
                    attributeMapping => #{
                        subjectId => {required, "sub"},
                        alias => {optional, "username"},
                        name => {optional, {any, ["name", "fullName"]}},
                        emails => {optional, "email"},
                        entitlements => {optional, "groups"},
                        custom => {optional, "name"}
                    },
                    entitlementMapping => #{
                        enabled => false
                    }
                }
            }
    end.


-spec supported_idps(auth_config:config_v1(), auth_config:saml_config_v1()) ->
    [{auth_config:idp(), auth_config:config_section()}].
supported_idps(AuthConfig, SamlConfig) ->
    OnepanelAuthIdP = case proplists:get_value(basicAuth, AuthConfig) of
        undefined -> [];
        _ -> [onepanel_entry()]
    end,
    OidcIdPs = lists:map(fun({IdP, Config}) ->
        oidc_entry(IdP, Config)
    end, proplists:delete(basicAuth, AuthConfig)),
    SamlIdPs = lists:map(fun({IdP, Config}) ->
        saml_entry(IdP, Config)
    end, maps:to_list(maps:get(supported_idps, SamlConfig, #{}))),
    lists:flatten([OnepanelAuthIdP, OidcIdPs, SamlIdPs]).


-spec onepanel_entry() -> {auth_config:idp(), auth_config:config_section()}.
onepanel_entry() ->
    {onepanel, #{
        displayName => display_name(onepanel),
        iconPath => icon_path(onepanel),
        iconBackgroundColor => icon_background(onepanel),
        protocol => onepanelAuth
    }}.



-spec oidc_entry(auth_config:idp(), auth_config:config_v1()) ->
    {auth_config:idp(), auth_config:config_section()}.
oidc_entry(IdP, Config) ->
    IdPBin = atom_to_binary(IdP, utf8),
    AuthDelegationConfig = proplists:get_value(authority_delegation, Config, []),
    GroupMappingConfig = proplists:get_value(group_mapping, Config, []),
    ProtocolConfig = #{
        plugin => oidc_protocol_plugin(IdP),
        pluginConfig => oidc_protocol_plugin_config(IdP, Config),
        attributeMapping => oidc_attribute_mapping(IdP),
        authorityDelegation => #{
            enabled => proplists:get_value(enabled, AuthDelegationConfig, false),
            tokenPrefix => ?str(proplists:get_value(token_prefix, AuthDelegationConfig, <<IdPBin/binary, ":">>))
        },
        entitlementMapping => #{
            enabled => proplists:get_value(enabled, GroupMappingConfig, false),
            voGroupName => ?str(proplists:get_value(vo_group_id, GroupMappingConfig, undefined)),
            adminGroup => ?str(proplists:get_value(super_group, GroupMappingConfig, undefined)),
            parser => entitlement_parser(IdP),
            parserConfig => entitlement_parser_config(IdP)
        }
    },

    {IdP, #{
        displayName => display_name(IdP),
        iconPath => icon_path(IdP),
        iconBackgroundColor => icon_background(IdP),
        protocol => openid,
        protocolConfig => ProtocolConfig
    }}.


-spec oidc_protocol_plugin(auth_config:idp()) -> default_oidc_plugin | plgrid_oidc_plugin.
oidc_protocol_plugin(plgrid) -> plgrid_oidc_plugin;
oidc_protocol_plugin(_) -> default_oidc_plugin.


-spec oidc_protocol_plugin_config(auth_config:idp(), auth_config:config_v1()) -> auth_config:config_section().
oidc_protocol_plugin_config(plgrid, Config) -> #{
    xrds_endpoint => ?str(proplists:get_value(xrds_endpoint, Config, oidc_default_xrds_endpoint(plgrid)))
};
oidc_protocol_plugin_config(IdP, Config) -> #{
    clientId => ?str(proplists:get_value(app_id, Config, <<"unknown">>)),
    clientSecret => ?str(proplists:get_value(app_secret, Config, <<"unknown">>)),
    endpoints => oidc_endpoints(IdP, Config),
    scope => oidc_scope(IdP),
    offlineAccess => false,
    prompt => undefined,
    accessTokenAcquireMethod => oidc_access_token_acquire_method(IdP),
    clientSecretPassMethod => oidc_client_secret_pass_method(IdP),
    accessTokenPassMethod => oidc_access_token_pass_method(IdP),
    customData => oidc_custom_data(IdP)
}.


-spec oidc_endpoints(auth_config:idp(), auth_config:config_v1()) -> auth_config:config_section().
oidc_endpoints(facebook, Config) -> #{
    authorize => ?str(proplists:get_value(authorize_endpoint, Config, <<"https://www.facebook.com/dialog/oauth">>)),
    accessToken => ?str(proplists:get_value(access_token_endpoint, Config, <<"https://graph.facebook.com/oauth/access_token">>)),
    userInfo => ?str(proplists:get_value(user_info_endpoint, Config, <<"https://graph.facebook.com/me">>))
};
oidc_endpoints(github, Config) -> #{
    authorize => ?str(proplists:get_value(authorize_endpoint, Config, <<"https://github.com/login/oauth/authorize">>)),
    accessToken => ?str(proplists:get_value(access_token_endpoint, Config, <<"https://github.com/login/oauth/access_token">>)),
    userInfo => [
        ?str(proplists:get_value(user_info_endpoint, Config, <<"https://api.github.com/user">>)),
        {"emails", ?str(proplists:get_value(user_emails_endpoint, Config, <<"https://api.github.com/user/emails">>))}
    ]
};
oidc_endpoints(IdP, Config) -> #{
    xrds => ?str(proplists:get_value(xrds_endpoint, Config, oidc_default_xrds_endpoint(IdP))),
    authorize => {xrds, "authorization_endpoint"},
    accessToken => {xrds, "token_endpoint"},
    userInfo => {xrds, "userinfo_endpoint"}
}.


-spec oidc_default_xrds_endpoint(auth_config:idp()) -> binary().
oidc_default_xrds_endpoint(rhea) ->
    <<"https://fed-id.nuv.la/auth/realms/SixSq/.well-known/openid-configuration">>;
oidc_default_xrds_endpoint(google) ->
    <<"https://accounts.google.com/.well-known/openid-configuration">>;
oidc_default_xrds_endpoint(indigo) ->
    <<"https://iam-test.indigo-datacloud.eu/.well-known/openid-configuration">>;
oidc_default_xrds_endpoint(egi) ->
    <<"https://aai.egi.eu/oidc/.well-known/openid-configuration">>;
oidc_default_xrds_endpoint(plgrid) ->
    <<"https://openid.plgrid.pl/gateway">>;
oidc_default_xrds_endpoint(_) ->
    <<"https://localhost.local">>.


-spec oidc_scope(auth_config:idp()) -> string().
oidc_scope(facebook) -> "email";
oidc_scope(github) -> "user,user:email";
oidc_scope(_) -> "openid email profile".


-spec oidc_access_token_acquire_method(auth_config:idp()) -> post | get.
oidc_access_token_acquire_method(facebook) -> get;
oidc_access_token_acquire_method(_) -> post.


-spec oidc_client_secret_pass_method(auth_config:idp()) -> urlencoded | inAuthHeader.
oidc_client_secret_pass_method(indigo) -> inAuthHeader;
oidc_client_secret_pass_method(egi) -> inAuthHeader;
oidc_client_secret_pass_method(_) -> urlencoded.


-spec oidc_access_token_pass_method(auth_config:idp()) -> urlencoded | inAuthHeader.
oidc_access_token_pass_method(rhea) -> inAuthHeader;
oidc_access_token_pass_method(_) -> urlencoded.


-spec oidc_custom_data(auth_config:idp()) -> auth_config:config_section().
oidc_custom_data(facebook) -> #{
    userInfo => #{
        parameters => #{
            "fields" => "email,name"
        }
    }
};
oidc_custom_data(github) -> #{
    userInfo => #{
        headers => #{
            "User-Agent" => "Onedata"
        }
    }
};
oidc_custom_data(_) -> #{
}.


-spec oidc_attribute_mapping(auth_config:idp()) -> auth_config:config_section().
oidc_attribute_mapping(rhea) -> (oidc_attribute_mapping(default))#{
    alias => {optional, "username"},
    entitlements => {optional, {append, ["groups", "roles"]}}
};
oidc_attribute_mapping(indigo) -> (oidc_attribute_mapping(default))#{
    name => {required, {any, ["name", {concat, ["given_name", {str, " "}, "family_name"]}]}},
    alias => {optional, "preferred_username"}
};
oidc_attribute_mapping(facebook) -> (oidc_attribute_mapping(default))#{
    subjectId => {required, "id"}
};
oidc_attribute_mapping(github) -> (oidc_attribute_mapping(default))#{
    subjectId => {required, "id"},
    emails => {optional, {nested, ["emails", {list, "email"}]}}
};
oidc_attribute_mapping(egi) -> (oidc_attribute_mapping(default))#{
    name => {required, {any, ["name", {concat, ["given_name", {str, " "}, "family_name"]}]}},
    alias => {optional, "preferred_username"},
    entitlements => {optional, "edu_person_entitlements"}
};
oidc_attribute_mapping(plgrid) -> #{
    subjectId => {required, "openid.sreg.nickname"},
    name => {required, "openid.sreg.fullname"},
    alias => {optional, "openid.sreg.nickname"},
    emails => {optional, "openid.sreg.email"},
    entitlements => {optional, "openid.ext1.value.teams"},
    custom => undefined
};
oidc_attribute_mapping(_) -> #{
    subjectId => {required, "sub"},
    name => {optional, {any, ["name", "fullName"]}},
    alias => {optional, "username"},
    emails => {optional, "email"},
    entitlements => {optional, "groups"},
    custom => undefined
}.


-spec saml_entry(auth_config:idp(), auth_config:saml_config_v1()) ->
    {auth_config:idp(), auth_config:config_section()}.
saml_entry(IdP, Config) ->
    GroupMappingConfig = maps:get(group_mapping, Config, #{}),
    ProtocolConfig = #{
        metadataUrl => ?str(maps:get(metadata_url, Config, saml_default_metadata_url(IdP))),
        preferredSsoBinding => maps:get(preferred_sso_binding, Config, saml_preferred_sso_binding(IdP)),
        attributeMapping => saml_attribute_mapping(IdP, maps:get(attribute_mapping, Config, #{})),
        entitlementMapping => #{
            enabled => maps:get(enabled, GroupMappingConfig, false),
            voGroupName => ?str(maps:get(vo_group_id, GroupMappingConfig, undefined)),
            adminGroup => ?str(maps:get(super_group, GroupMappingConfig, undefined)),
            parser => entitlement_parser(IdP),
            parserConfig => entitlement_parser_config(IdP)
        }
    },

    {IdP, #{
        displayName => display_name(IdP),
        iconPath => icon_path(IdP),
        iconBackgroundColor => icon_background(IdP),
        protocol => saml,
        protocolConfig => ProtocolConfig
    }}.


-spec saml_default_metadata_url(auth_config:idp()) -> binary().
saml_default_metadata_url(elixir) ->
    <<"https://login.elixir-czech.org/proxy/saml2/idp/metadata.php">>;
saml_default_metadata_url(unitedid) ->
    <<"http://md.unitedid.org/idp.xml">>;
saml_default_metadata_url(cern) ->
    <<"https://met.refeds.org/met/entity/https%3A//cern.ch/login/?viewxml=true&federation=incommon-federation">>;
saml_default_metadata_url(cnrs) ->
    <<"https://met.refeds.org/met/entity/https%3A//janus.cnrs.fr/idp/?viewxml=true&federation=federation-education-recherche">>;
saml_default_metadata_url(desy) ->
    <<"https://idp1.desy.de/idp/shibboleth">>;
saml_default_metadata_url(embl) ->
    <<"https://idp.ebi.ac.uk/idp/shibboleth">>;
saml_default_metadata_url(esrf) ->
    <<"https://met.refeds.org/met/entity/https%3A//websso.esrf.fr/auth/realms/ESRF/?viewxml=true">>;
saml_default_metadata_url(ifae) ->
    <<"https://met.refeds.org/met/entity/https%3A//www.rediris.es/sir/ifaeidp/?viewxml=true">>;
saml_default_metadata_url(infn) ->
    <<"https://idp.infn.it/saml2/idp/metadata.php">>;
saml_default_metadata_url(kit) ->
    <<"https://met.refeds.org/met/entity/https%3A//idp.scc.kit.edu/idp/shibboleth/?viewxml=true">>;
saml_default_metadata_url(stfc) ->
    <<"https://met.refeds.org/met/entity/https%3A//idp02.stfc.ac.uk/shibboleth/?viewxml=true">>;
saml_default_metadata_url(surfsara) ->
    <<"https://technical.edugain.org/api.php?action=show_entity&row_id=27218">>;
saml_default_metadata_url(_) ->
    <<"https://localhost.local">>.


-spec saml_preferred_sso_binding(auth_config:idp()) -> http_redirect | http_post.
saml_preferred_sso_binding(cern) ->
    http_post;
saml_preferred_sso_binding(_) ->
    http_redirect.


-spec saml_attribute_mapping(auth_config:idp(), #{}) -> auth_config:config_section().
saml_attribute_mapping(elixir, AttrMappingConfig) -> (saml_attribute_mapping(default, AttrMappingConfig))#{
    subjectId => {required, saml_attr_v2(maps:get(id, AttrMappingConfig, eduPersonUniqueID))}
};
saml_attribute_mapping(unitedid, AttrMappingConfig) -> (saml_attribute_mapping(default, AttrMappingConfig))#{
    name => {optional, case maps:find(name, AttrMappingConfig) of
        error -> {join, " ", "displayName"};
        {ok, Attr} -> saml_attr_v2(Attr)
    end},
    email => {optional, saml_attr_v2(maps:get(email, AttrMappingConfig, eduPersonPrincipalName))},
    entitlements => undefined
};
saml_attribute_mapping(cern, AttrMappingConfig) -> (saml_attribute_mapping(default, AttrMappingConfig))#{
    subjectId => {required, saml_attr_v2(maps:get(id, AttrMappingConfig, eduPersonUniqueID))},
    entitlements => {optional, saml_attr_v2(maps:get(groups, AttrMappingConfig, eduPersonAffiliation))}
};
saml_attribute_mapping(desy, AttrMappingConfig) -> (saml_attribute_mapping(default, AttrMappingConfig))#{
    subjectId => {required, saml_attr_v2(maps:get(id, AttrMappingConfig, eduPersonPrincipalName))}
};
saml_attribute_mapping(kit, AttrMappingConfig) -> (saml_attribute_mapping(default, AttrMappingConfig))#{
    subjectId => {required, saml_attr_v2(maps:get(id, AttrMappingConfig, eduPersonPrincipalName))},
    name => {optional, saml_attr_v2(maps:get(name, AttrMappingConfig, surName))}
};
saml_attribute_mapping(_, AttrMappingConfig) -> #{
    subjectId => {required, saml_attr_v2(maps:get(id, AttrMappingConfig, eduPersonTargetedID))},
    name => {optional, saml_attr_v2(maps:get(name, AttrMappingConfig, displayName))},
    alias => {optional, saml_attr_v2(maps:get(login, AttrMappingConfig, eduPersonPrincipalName))},
    email => {optional, saml_attr_v2(maps:get(email, AttrMappingConfig, mail))},
    entitlements => {optional, saml_attr_v2(maps:get(groups, AttrMappingConfig, eduPersonEntitlement))},
    custom => undefined
}.


% Accounts changes in esaml
-spec saml_attr_v2(atom()) -> atom().
saml_attr_v2(eduPersonUniqueId) -> "eduPersonUniqueID";
saml_attr_v2(Other) -> ?str(Other).


-spec entitlement_parser(auth_config:idp()) -> module().
entitlement_parser(rhea) -> nested_entitlement_parser;
entitlement_parser(plgrid) -> flat_entitlement_parser;
entitlement_parser(indigo) -> flat_entitlement_parser;
entitlement_parser(egi) -> custom_entitlement_parser;
entitlement_parser(elixir) -> nested_entitlement_parser;
entitlement_parser(_) -> undefined.


-spec entitlement_parser_config(auth_config:idp()) -> auth_config:config_section().
entitlement_parser_config(egi) -> #{
};
entitlement_parser_config(rhea) ->
    entitlement_parser_config('$nested');
entitlement_parser_config(plgrid) ->
    entitlement_parser_config('$flat');
entitlement_parser_config(indigo) ->
    entitlement_parser_config('$flat');
entitlement_parser_config(elixir) ->
    (entitlement_parser_config('$nested'))#{
        splitWith => ":"
    };
entitlement_parser_config('$nested') -> #{
    splitWith => "/",
    topGroupType => unit,
    topGroupPrivilegesInVo => member,
    subGroupsType => team,
    subGroupsPrivilegesInParent => member,
    userPrivileges => member
};
entitlement_parser_config('$flat') -> #{
    groupType => team,
    groupPrivilegesInVo => member,
    userPrivileges => member
};
entitlement_parser_config(_) -> #{
}.


-spec display_name(auth_config:idp()) -> string().
display_name(onepanel) -> "Onepanel account";
display_name(elixir) -> "Elixir";
display_name(egi) -> "EGI";
display_name(facebook) -> "Facebook";
display_name(github) -> "GitHub";
display_name(google) -> "Google+";
display_name(indigo) -> "Indigo";
display_name(plgrid) -> "PLGrid OpenID";
display_name(rhea) -> "RHEA KeyCloak";
display_name(unitedid) -> "UnitedID";
display_name(cern) -> "CERN (eduGAIN)";
display_name(infn) -> "INFN (eduGAIN)";
display_name(desy) -> "DESY (eduGAIN)";
display_name(embl) -> "EMBL (eduGAIN)";
display_name(cnrs) -> "CNRS (eduGAIN)";
display_name(esrf) -> "ESRF (eduGAIN)";
display_name(ifae) -> "IFAE (eduGAIN)";
display_name(kit) -> "KIT (eduGAIN)";
display_name(stfc) -> "STFC (eduGAIN)";
display_name(surfsara) -> "SURFSara (eduGAIN)";
display_name(_) -> "<unknown>".


-spec icon_path(auth_config:idp()) -> string().
icon_path(ifae) -> "/assets/images/auth-providers/ifae.jpg";
icon_path(unitedid) -> "/assets/images/auth-providers/unitedid.png";
icon_path(IdP) ->
    IconFile = case lists:member(IdP, known_v1_idps()) of
        true -> atom_to_list(IdP) ++ ".svg";
        false -> "default.svg"
    end,
    "/assets/images/auth-providers/" ++ IconFile.


-spec icon_background(auth_config:idp()) -> string().
icon_background(cern) -> "#0053A1";
icon_background(egi) -> "#0455A1";
icon_background(elixir) -> "#FF7A04";
icon_background(indigo) -> "#341246";
icon_background(facebook) -> "#5B87C5";
icon_background(github) -> "#1E2325";
icon_background(google) -> "#F1514F";
icon_background(onepanel) -> "#4BD187";
icon_background(plgrid) -> "#026381";
icon_background(rhea) -> "#B51017";
icon_background(stfc) -> "#1C3764";
icon_background(unitedid) -> "#ABDFF1";
icon_background(_) -> "#FFF".


-spec known_v1_idps() -> [auth_config:idp()].
known_v1_idps() -> [
    onepanel,
    elixir,
    egi,
    facebook,
    github,
    google,
    indigo,
    plgrid,
    rhea,
    unitedid,
    cern,
    infn,
    desy,
    embl,
    cnrs,
    esrf,
    ifae,
    kit,
    stfc,
    surfsara
].
