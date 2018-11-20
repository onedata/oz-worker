%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning integration with OpenID servers.
%%% @end
%%%-------------------------------------------------------------------
-module(openid_test_SUITE).
-author("Lukasz Opiola").

-include("oidc_server_mock.hrl").
-include("registered_names.hrl").
-include("auth/auth_errors.hrl").
-include("auth/entitlement_mapping.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/global_definitions.hrl").

-define(DUMMY_IDP, dummyIdP).
-define(FIRST_IDP, firstIdP).
-define(ANOTHER_IDP, anotherIdP).
-define(DIFFERENT_IDP, differentIdP).

-define(DUMMY_CLIENT_ID, "123456789").
-define(DUMMY_CLIENT_SECRET, "abcdefghij").

% Shared by clients and the mock server
-define(CORRECT_OIDC_SPEC, #oidc_spec{
    endpoints = #{
        xrds => <<"https://example.com/well-known/openid">>,
        authorize => {xrds, "authorization_endpoint"},
        accessToken => {xrds, "token_endpoint"},
        userInfo => {xrds, "userinfo_endpoint"}
    },
    clientId = ?DUMMY_CLIENT_ID,
    clientSecret = ?DUMMY_CLIENT_SECRET,
    scope = "profile, email",
    customData = #{
        accessToken => #{
            headers => #{"A" => "B"},
            parameters => #{"param1" => "val1"}
        },
        userInfo => #{
            headers => #{"c" => "d"},
            parameters => #{"param2" => "val3"}
        }
    }
}).

%% API
-export([
    all/0,
    init_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    end_per_suite/1
]).
%% Tests
-export([
    get_login_endpoint/1,
    validate_correct_login/1,
    validate_correct_login_test_mode/1,
    authority_delegation/1,
    link_account/1,
    bad_oidc_config/1,
    bad_oidc_config_test_mode/1
]).
%% Exports for RPC
-export([validate_login_in_test_mode/1]).


all() -> ?ALL([
    get_login_endpoint,
    validate_correct_login,
    validate_correct_login_test_mode,
    authority_delegation,
    link_account,
    bad_oidc_config,
    bad_oidc_config_test_mode
]).


%%%===================================================================
%%% Setup/Teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils, rest_test_utils, oidc_server_mock]} | Config].


init_per_testcase(_, Config) ->
    oz_test_utils:delete_all_entities(Config),
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:set_env(Nodes, ?APP_NAME, openid_xrds_cache_ttl, -1),
    Config.

end_per_testcase(_, _Config) ->
    ok.

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().


%%%===================================================================
%%% Test functions
%%%===================================================================

get_login_endpoint(Config) ->
    OidcSpec = ?CORRECT_OIDC_SPEC,
    oidc_server_mock:mock(Config, OidcSpec),
    overwrite_auth_config(Config, false, [{?DUMMY_IDP, OidcSpec, #{}}]),

    ExpAuthorizeEndpoint = ?MOCK_ENDPOINT_FROM_XRDS("authorization_endpoint"),
    Len = byte_size(ExpAuthorizeEndpoint),
    ?assertMatch(
        {ok, #{
            <<"formData">> := null,
            <<"method">> := <<"get">>,
            <<"url">> := <<ExpAuthorizeEndpoint:Len/binary, _/binary>>
        }},
        oz_test_utils:call_oz(Config, auth_logic, get_login_endpoint, [
            ?DUMMY_IDP, false, <<"">>, false
        ])
    ).


validate_correct_login(Config) ->
    validate_correct_login_base(Config, false).

validate_correct_login_test_mode(Config) ->
    validate_correct_login_base(Config, true).

validate_correct_login_base(Config, TestMode) ->
    RedirectAfterLogin = <<"https://example.com/after-login-path">>,
    OidcSpec = ?CORRECT_OIDC_SPEC,
    oidc_server_mock:mock(Config, OidcSpec),
    overwrite_auth_config(Config, TestMode, [{?DUMMY_IDP, OidcSpec, #{
        attributeMapping => #{
            subjectId => {required, {replace, "c", "x", "id"}},
            name => {optional, {any, ["fullName", {join, " ", "nameTokens"}]}},
            alias => {optional, {nested, ["alias", "value"]}},
            emails => {required, {filter, ".*@my.org", {split, ",", "emails"}}},
            entitlements => {optional, {concat, [
                {str_list, ["a", "b", "c", "d"]},
                {str, ":"},
                "groups",
                {str, "/"},
                {str_list, ["1", "2", "3", "4"]}
            ]}},
            custom => {optional, {append, [
                "customAttrs",
                {keyValue, "organization"},
                {keyValue, "roles", {nested, ["roles", {list, "role"}, "displayName"]}}
            ]}}
        },
        entitlementMapping => #{
            enabled => true,
            voGroupName => "My-VO",
            adminGroup => "d:admins/4",
            parser => nested_entitlement_parser,
            parserConfig => #{
                splitWith => "/",
                topGroupType => unit,
                topGroupPrivilegesInVo => manager,
                subGroupsType => team,
                subGroupsPrivilegesInParent => member,
                userPrivileges => member
            }
        }
    }}]),

    {ok, #{<<"url">> := Url}} = oz_test_utils:call_oz(
        Config, auth_logic, get_login_endpoint, [?DUMMY_IDP, false, RedirectAfterLogin, TestMode]
    ),

    {ok, MockedCowboyReq, _} = ?assertMatch({ok, _, _}, oidc_server_mock:simulate_user_login(
        Config, OidcSpec, Url, #{
            <<"id">> => <<"abcdef1c2c3c4c">>,
            <<"nameTokens">> => [<<"John">>, <<"Doe">>, <<"Jr">>],
            <<"alias">> => #{
                <<"value">> => <<"jodoe">>
            },
            <<"groups">> => [
                <<"some">>,
                <<"entitlements">>,
                <<"idk">>,
                <<"admins">>
            ],
            <<"emails">> => <<"joedoe@example.com,john.doe@my.org">>,
            <<"organization">> => <<"My Organization">>,
            <<"customAttrs">> => #{
                <<"firstAttr">> => <<"firstValue">>,
                <<"secondAttr">> => [<<"second">>, <<"value">>],
                <<"thirdAttr">> => #{
                    <<"nested">> => <<"json">>
                },
                <<"fourthAttr">> => 17
            },
            <<"roles">> => [
                #{<<"role">> => #{<<"displayName">> => <<"role1">>}},
                #{<<"role">> => #{<<"displayName">> => <<"role2">>}},
                #{<<"role">> => #{<<"displayName">> => <<"role3">>}}
            ]
        }
    )),

    ExpSubjectId = <<"abxdef1x2x3x4x">>,  % {replace, "c", "x", "id"}
    ExpUserId = user_logic:idp_uid_to_system_uid(?DUMMY_IDP, ExpSubjectId),
    ExpName = <<"John Doe Jr">>,
    ExpAlias = <<"jodoe">>,
    ExpEmails = [<<"john.doe@my.org">>],
    ExpEntitlements = [
        <<"a:some/1">>,
        <<"b:entitlements/2">>,
        <<"c:idk/3">>,
        <<"d:admins/4">>
    ],
    ExpCustom = #{
        <<"firstAttr">> => <<"firstValue">>,
        <<"secondAttr">> => [<<"second">>, <<"value">>],
        <<"thirdAttr">> => #{
            <<"nested">> => <<"json">>
        },
        <<"fourthAttr">> => 17,
        <<"organization">> => <<"My Organization">>,
        <<"roles">> => [<<"role1">>, <<"role2">>, <<"role3">>]
    },

    Group1 = entitlement_mapping:gen_group_id(#idp_entitlement{idp = ?DUMMY_IDP, path = [
        #idp_group{name = <<"My-VO">>, type = organization},
        #idp_group{name = <<"a:some">>, type = unit},
        #idp_group{name = <<"1">>, type = team}
    ]}),
    Group2 = entitlement_mapping:gen_group_id(#idp_entitlement{idp = ?DUMMY_IDP, path = [
        #idp_group{name = <<"My-VO">>, type = organization},
        #idp_group{name = <<"b:entitlements">>, type = unit},
        #idp_group{name = <<"2">>, type = team}
    ]}),
    Group3 = entitlement_mapping:gen_group_id(#idp_entitlement{idp = ?DUMMY_IDP, path = [
        #idp_group{name = <<"My-VO">>, type = organization},
        #idp_group{name = <<"c:idk">>, type = unit},
        #idp_group{name = <<"3">>, type = team}
    ]}),
    Group4 = entitlement_mapping:gen_group_id(#idp_entitlement{idp = ?DUMMY_IDP, path = [
        #idp_group{name = <<"My-VO">>, type = organization},
        #idp_group{name = <<"d:admins">>, type = unit},
        #idp_group{name = <<"4">>, type = team}
    ]}),

    case TestMode of
        false ->
            {ok, UserId, _} = ?assertMatch(
                {ok, ExpUserId, RedirectAfterLogin},
                oz_test_utils:call_oz(Config, auth_logic, validate_login, [
                    <<"GET">>, MockedCowboyReq
                ])
            ),

            ?assertMatch(
                {ok, #od_user{
                    name = ExpName,
                    alias = ExpAlias,
                    emails = ExpEmails,
                    linked_accounts = [#linked_account{
                        subject_id = ExpSubjectId,
                        name = ExpName,
                        alias = ExpAlias,
                        emails = ExpEmails,
                        entitlements = ExpEntitlements,
                        custom = ExpCustom
                    }],
                    entitlements = [Group1, Group2, Group3, Group4]
                }},
                oz_test_utils:get_user(Config, UserId)
            ),

            % As a member of the admin group, the user should have effective admin rights
            % in all other groups from the IdP.
            AdminPrivs = privileges:group_admin(),
            ?assertMatch({ok, AdminPrivs}, oz_test_utils:group_get_eff_user_privileges(Config, Group1, UserId)),
            ?assertMatch({ok, AdminPrivs}, oz_test_utils:group_get_eff_user_privileges(Config, Group2, UserId)),
            ?assertMatch({ok, AdminPrivs}, oz_test_utils:group_get_eff_user_privileges(Config, Group3, UserId));

        true ->
            {ok, ExpUserId, UserData} = ?assertMatch(
                {ok, ExpUserId, _},
                oz_test_utils:call_oz(Config, ?MODULE, validate_login_in_test_mode, [
                    MockedCowboyReq
                ])
            ),

            ?assertMatch(
                #{
                    <<"name">> := ExpName,
                    <<"alias">> := ExpAlias,
                    <<"emails">> := ExpEmails,
                    <<"linkedAccounts">> := [#{
                        <<"subjectId">> := ExpSubjectId,
                        <<"name">> := ExpName,
                        <<"alias">> := ExpAlias,
                        <<"emails">> := ExpEmails,
                        <<"entitlements">> := ExpEntitlements,
                        <<"custom">> := ExpCustom
                    }],
                    <<"groups">> := [Group1, Group2, Group3, Group4]
                },
                UserData

            ),

            % No users or groups should be created in the process
            ?assertEqual({error, not_found}, oz_test_utils:call_oz(Config, od_user, get, [ExpUserId])),
            ?assertEqual({error, not_found}, oz_test_utils:call_oz(Config, od_group, get, [Group1])),
            ?assertEqual({error, not_found}, oz_test_utils:call_oz(Config, od_group, get, [Group2])),
            ?assertEqual({error, not_found}, oz_test_utils:call_oz(Config, od_group, get, [Group3])),
            ?assertEqual({error, not_found}, oz_test_utils:call_oz(Config, od_group, get, [Group4]))
    end.


authority_delegation(Config) ->
    SubjectId = <<"1233456734534">>,
    OidcSpec = ?CORRECT_OIDC_SPEC,
    oidc_server_mock:mock(Config, OidcSpec),
    overwrite_auth_config(Config, false, [{?DUMMY_IDP, OidcSpec, #{
        attributeMapping => #{
            subjectId => {required, "sub"}
        },
        authorityDelegation => #{
            enabled => true,
            tokenPrefix => "dummy/"
        }
    }}]),

    {ok, #{<<"url">> := Url}} = oz_test_utils:call_oz(
        Config, auth_logic, get_login_endpoint, [?DUMMY_IDP, false, <<"">>, false]
    ),
    {ok, _, AccessToken} = ?assertMatch({ok, _, _}, oidc_server_mock:simulate_user_login(
        Config, OidcSpec, Url, #{<<"sub">> => SubjectId}
    )),

    ExpUserId = user_logic:idp_uid_to_system_uid(?DUMMY_IDP, SubjectId),

    DelegationWorksSpec = fun(Success, AuthType) -> #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => case AuthType of
                bearer -> #{<<"Authorization">> => <<"Bearer dummy/", AccessToken/binary>>};
                xAuthToken -> #{<<"X-Auth-Token">> => <<"dummy/", AccessToken/binary>>}
            end
        },
        expect => case Success of
            true -> #{
                code => 200,
                body => {contains, #{<<"userId">> => ExpUserId}}
            };
            false -> #{
                code => 401
            }
        end
    } end,

    ?assert(rest_test_utils:check_rest_call(Config, DelegationWorksSpec(true, xAuthToken))),
    ?assert(rest_test_utils:check_rest_call(Config, DelegationWorksSpec(true, bearer))),

    % Turn off authority delegation and check if it does not work anymore.
    overwrite_auth_config(Config, false, [{?DUMMY_IDP, OidcSpec, #{
        attributeMapping => #{
            subjectId => {required, "sub"}
        },
        authorityDelegation => #{
            enabled => false,
            tokenPrefix => "dummy/"
        }
    }}]),

    ?assert(rest_test_utils:check_rest_call(Config, DelegationWorksSpec(false, xAuthToken))),
    ?assert(rest_test_utils:check_rest_call(Config, DelegationWorksSpec(false, bearer))).


link_account(Config) ->
    OidcSpec = ?CORRECT_OIDC_SPEC,

    FirstSubjectId = <<"123">>,
    FirstEmails = [<<"joedoe@first-example.com">>],

    AnotherSubjectId = <<"abc">>,
    AnotherEmails = [<<"joe@another-example.com">>],

    DifferentSubjectId = <<"xyz">>,
    DifferentEmails = [<<"john.doe@different-example.com">>],

    CommonProtocolConfig = #{
        attributeMapping => #{
            subjectId => {required, "sub"},
            emails => {required, "emails"}
        }
    },

    oidc_server_mock:mock(Config, OidcSpec),
    overwrite_auth_config(Config, false, [
        {?FIRST_IDP, OidcSpec, CommonProtocolConfig},
        {?ANOTHER_IDP, OidcSpec, CommonProtocolConfig},
        {?DIFFERENT_IDP, OidcSpec, CommonProtocolConfig}
    ]),

    % Log in using the first IdP
    ExpFirstUserId = user_logic:idp_uid_to_system_uid(?FIRST_IDP, FirstSubjectId),
    ?assertMatch(
        {ok, ExpFirstUserId, _},
        simulate_login_flow(Config, ?FIRST_IDP, false, false, OidcSpec, #{
            <<"sub">> => FirstSubjectId,
            <<"emails">> => FirstEmails
        })
    ),

    % Link the second account
    ?assertMatch(
        {ok, ExpFirstUserId, _}, % linking the account should yield the first user id
        simulate_login_flow(Config, ?ANOTHER_IDP, {true, ExpFirstUserId}, false, OidcSpec, #{
            <<"sub">> => AnotherSubjectId,
            <<"emails">> => AnotherEmails
        })
    ),

    FirstAndAnotherEmails = lists:usort(FirstEmails ++ AnotherEmails),
    ?assertMatch(
        {ok, #od_user{
            emails = FirstAndAnotherEmails,
            linked_accounts = [
                #linked_account{
                    subject_id = FirstSubjectId,
                    emails = FirstEmails
                },
                #linked_account{
                    subject_id = AnotherSubjectId,
                    emails = AnotherEmails
                }
            ]
        }},
        oz_test_utils:get_user(Config, ExpFirstUserId)
    ),

    % Link the third account
    ?assertMatch(
        {ok, ExpFirstUserId, _}, % linking the account should yield the first user id
        simulate_login_flow(Config, ?DIFFERENT_IDP, {true, ExpFirstUserId}, false, OidcSpec, #{
            <<"sub">> => DifferentSubjectId,
            <<"emails">> => DifferentEmails
        })
    ),

    AllEmails = lists:usort(FirstEmails ++ AnotherEmails ++ DifferentEmails),
    ?assertMatch(
        {ok, #od_user{
            emails = AllEmails,
            linked_accounts = [
                #linked_account{
                    subject_id = FirstSubjectId,
                    emails = FirstEmails
                },
                #linked_account{
                    subject_id = AnotherSubjectId,
                    emails = AnotherEmails
                },
                #linked_account{
                    subject_id = DifferentSubjectId,
                    emails = DifferentEmails
                }
            ]
        }},
        oz_test_utils:get_user(Config, ExpFirstUserId)
    ).


bad_oidc_config(Config) ->
    bad_oidc_config_base(Config, false).

bad_oidc_config_test_mode(Config) ->
    bad_oidc_config_base(Config, true).

bad_oidc_config_base(Config, TestMode) ->
    bad_xrds_endpoint(Config, TestMode),
    bad_authorize_endpoint(Config, TestMode),
    bad_access_token_endpoint(Config, TestMode),
    bad_userinfo_endpoint(Config, TestMode),
    bad_userinfo_endpoint_in_authority_delegation(Config, TestMode),
    bad_scope(Config, TestMode),
    bad_client_id(Config, TestMode),
    bad_client_secret(Config, TestMode),
    bad_access_token_acquire_method(Config, TestMode),
    bad_client_secret_pass_method(Config, TestMode),
    bad_access_token_pass_method(Config, TestMode),
    bad_access_token_pass_method_in_authority_delegation(Config, TestMode),
    bad_custom_data(Config, TestMode).


bad_xrds_endpoint(Config, TestMode) ->
    screw_up_config(Config, TestMode, #{}, fun(OidcSpec = #oidc_spec{endpoints = Endpoints}) ->
        OidcSpec#oidc_spec{
            endpoints = Endpoints#{
                xrds => <<"https://some.bad/xrds-url">>
            }
        }
    end),
    ?assertEqual(?ERROR_INTERNAL_SERVER_ERROR, oz_test_utils:call_oz(
        Config, auth_logic, get_login_endpoint, [?DUMMY_IDP, false, <<"">>, TestMode]
    )).


bad_authorize_endpoint(Config, TestMode) ->
    screw_up_config(Config, TestMode, #{}, fun(OidcSpec = #oidc_spec{endpoints = Endpoints}) ->
        OidcSpec#oidc_spec{
            endpoints = Endpoints#{
                authorize => {xrds, "authorization_blahblahblah"}
            }
        }
    end),
    ?assertEqual(?ERROR_INTERNAL_SERVER_ERROR, oz_test_utils:call_oz(
        Config, auth_logic, get_login_endpoint, [?DUMMY_IDP, false, <<"">>, TestMode]
    )).


bad_access_token_endpoint(Config, TestMode) ->
    CorrectOidcSpec = screw_up_config(Config, TestMode, #{}, fun(OidcSpec = #oidc_spec{endpoints = Endpoints}) ->
        OidcSpec#oidc_spec{
            endpoints = Endpoints#{
                accessToken => "https://bad-example.com/authorization_blahblahblah"
            }
        }
    end),
    ?assertMatch(
        {auth_error, ?ERROR_BAD_IDP_RESPONSE(_, 404, _, _), _, _},
        simulate_login_flow(Config, ?DUMMY_IDP, false, TestMode, CorrectOidcSpec, #{})
    ).


bad_userinfo_endpoint(Config, TestMode) ->
    CorrectOidcSpec = screw_up_config(Config, TestMode, #{}, fun(OidcSpec = #oidc_spec{endpoints = Endpoints}) ->
        OidcSpec#oidc_spec{
            endpoints = Endpoints#{
                userInfo => "https://bad-example.com/userinfo_blahblahblah"
            }
        }
    end),
    ?assertMatch(
        {auth_error, ?ERROR_BAD_IDP_RESPONSE(_, 404, _, _), _, _},
        simulate_login_flow(Config, ?DUMMY_IDP, false, TestMode, CorrectOidcSpec, #{})
    ).


bad_userinfo_endpoint_in_authority_delegation(Config, TestMode) ->
    CorrectOidcSpec = screw_up_config(Config, TestMode, #{
        authorityDelegation => #{
            enabled => true,
            tokenPrefix => "dummy/"
        }},
        fun(OidcSpec = #oidc_spec{endpoints = Endpoints}) ->
            OidcSpec#oidc_spec{
                endpoints = Endpoints#{
                    userInfo => "https://bad-example.com/userinfo_blahblahblah"
                }
            }
        end
    ),
    {ok, #{<<"url">> := Url}} = oz_test_utils:call_oz(
        Config, auth_logic, get_login_endpoint, [?DUMMY_IDP, false, <<"">>, TestMode]
    ),

    {ok, _, AccessToken} = ?assertMatch({ok, _, _}, oidc_server_mock:simulate_user_login(
        Config, CorrectOidcSpec, Url, #{}
    )),
    rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{<<"Authorization">> => <<"Bearer dummy/", AccessToken/binary>>}
        },
        expect => #{
            code => 500
        }

    }).


bad_scope(Config, TestMode) ->
    CorrectOidcSpec = screw_up_config(Config, TestMode, #{}, fun(OidcSpec) ->
        OidcSpec#oidc_spec{
            scope = "blahblahblah"
        }
    end),
    {ok, #{<<"url">> := Url}} = oz_test_utils:call_oz(
        Config, auth_logic, get_login_endpoint, [?DUMMY_IDP, false, <<"">>, TestMode]
    ),

    ?assertMatch(error, oidc_server_mock:simulate_user_login(
        Config, CorrectOidcSpec, Url, #{}
    )).


bad_client_id(Config, TestMode) ->
    CorrectOidcSpec = screw_up_config(Config, TestMode, #{}, fun(OidcSpec) ->
        OidcSpec#oidc_spec{
            clientId = "blahblahblah"
        }
    end),
    {ok, #{<<"url">> := Url}} = oz_test_utils:call_oz(
        Config, auth_logic, get_login_endpoint, [?DUMMY_IDP, false, <<"">>, TestMode]
    ),

    ?assertMatch(error, oidc_server_mock:simulate_user_login(
        Config, CorrectOidcSpec, Url, #{}
    )).


bad_client_secret(Config, TestMode) ->
    CorrectOidcSpec = screw_up_config(Config, TestMode, #{}, fun(OidcSpec) ->
        OidcSpec#oidc_spec{
            clientSecret = "blahblahblah"
        }
    end),
    ?assertMatch(
        {auth_error, ?ERROR_BAD_IDP_RESPONSE(_, 403, _, _), _, _},
        simulate_login_flow(Config, ?DUMMY_IDP, false, TestMode, CorrectOidcSpec, #{})
    ).


bad_access_token_acquire_method(Config, TestMode) ->
    CorrectOidcSpec = screw_up_config(Config, TestMode, #{}, fun(OidcSpec) ->
        OidcSpec#oidc_spec{
            accessTokenAcquireMethod = get
        }
    end),
    ?assertMatch(
        {auth_error, ?ERROR_BAD_IDP_RESPONSE(_, 404, _, _), _, _},
        simulate_login_flow(Config, ?DUMMY_IDP, false, TestMode, CorrectOidcSpec, #{})
    ).


bad_client_secret_pass_method(Config, TestMode) ->
    CorrectOidcSpec = screw_up_config(Config, TestMode, #{}, fun(OidcSpec) ->
        OidcSpec#oidc_spec{
            clientSecretPassMethod = inAuthHeader
        }
    end),
    ?assertMatch(
        {auth_error, ?ERROR_BAD_IDP_RESPONSE(_, 403, _, _), _, _},
        simulate_login_flow(Config, ?DUMMY_IDP, false, TestMode, CorrectOidcSpec, #{})
    ).


bad_access_token_pass_method(Config, TestMode) ->
    CorrectOidcSpec = screw_up_config(Config, TestMode, #{}, fun(OidcSpec) ->
        OidcSpec#oidc_spec{
            accessTokenPassMethod = urlencoded
        }
    end),
    ?assertMatch(
        {auth_error, ?ERROR_BAD_IDP_RESPONSE(_, 403, _, _), _, _},
        simulate_login_flow(Config, ?DUMMY_IDP, false, TestMode, CorrectOidcSpec, #{})
    ).


bad_access_token_pass_method_in_authority_delegation(Config, TestMode) ->
    CorrectOidcSpec = screw_up_config(Config, TestMode, #{
        authorityDelegation => #{
            enabled => true,
            tokenPrefix => "dummy/"
        }},
        fun(OidcSpec) ->
            OidcSpec#oidc_spec{
                accessTokenPassMethod = urlencoded
            }
        end
    ),
    {ok, #{<<"url">> := Url}} = oz_test_utils:call_oz(
        Config, auth_logic, get_login_endpoint, [?DUMMY_IDP, false, <<"">>, TestMode]
    ),

    {ok, _, AccessToken} = ?assertMatch({ok, _, _}, oidc_server_mock:simulate_user_login(
        Config, CorrectOidcSpec, Url, #{}
    )),
    rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => get,
            path => <<"/user">>,
            headers => #{<<"Authorization">> => <<"Bearer dummy/", AccessToken/binary>>}
        },
        expect => #{
            code => 500
        }

    }).


bad_custom_data(Config, TestMode) ->
    ScrewUpCustomData = fun
        (1, CustomData) -> CustomData#{userInfo => #{headers => undefined}};
        (2, CustomData) -> CustomData#{userInfo => #{parameters => undefined}};
        (3, CustomData) -> CustomData#{accessToken => #{headers => undefined}};
        (4, CustomData) -> CustomData#{accessToken => #{parameters => undefined}};
        (5, CustomData) -> CustomData#{userInfo => #{headers => #{}}};
        (6, CustomData) -> CustomData#{userInfo => #{parameters => #{}}};
        (7, CustomData) -> CustomData#{accessToken => #{headers => #{}}};
        (8, CustomData) -> CustomData#{accessToken => #{parameters => #{}}};
        (9, CustomData) -> CustomData#{userInfo => #{headers => #{"a" => "b"}}};
        (10, CustomData) -> CustomData#{userInfo => #{parameters => #{"a" => "b"}}};
        (11, CustomData) -> CustomData#{accessToken => #{headers => #{"a" => "b"}}};
        (12, CustomData) -> CustomData#{accessToken => #{parameters => #{"a" => "b"}}}
    end,

    lists:foreach(fun(TestNum) ->
        CorrectOidcSpec = screw_up_config(Config, TestMode, #{}, fun(OidcSpec = #oidc_spec{customData = CorrectData}) ->
            OidcSpec#oidc_spec{
                customData = ScrewUpCustomData(TestNum, CorrectData)
            }
        end),
        ?assertMatch(
            {auth_error, ?ERROR_BAD_IDP_RESPONSE(_, 400, _, _), _, _},
            simulate_login_flow(Config, ?DUMMY_IDP, false, TestMode, CorrectOidcSpec, #{})
        )
    end, lists:seq(1, 12)).


%%%===================================================================
%%% Auxiliary functions
%%%===================================================================

% Sets up DUMMY_IDP with default config, but overwrites auth.config with a modified
% version to introduce differences - ScrewingUpFun is used to spoil the config.
screw_up_config(Config, TestMode, ProtocolConfig, ScrewingUpFun) ->
    CorrectOidcSpec = ?CORRECT_OIDC_SPEC,
    IncorrectOidcSpec = ScrewingUpFun(CorrectOidcSpec),
    oidc_server_mock:mock(Config, CorrectOidcSpec),
    overwrite_auth_config(Config, TestMode, [{?DUMMY_IDP, IncorrectOidcSpec, ProtocolConfig}]),
    CorrectOidcSpec.


simulate_login_flow(Config, IdP, LinkAccount, TestMode, OidcSpec, UserAttributes) ->
    {ok, #{<<"url">> := Url}} = oz_test_utils:call_oz(
        Config, auth_logic, get_login_endpoint, [IdP, LinkAccount, <<"">>, TestMode]
    ),

    {ok, MockedCowboyReq, _} = oidc_server_mock:simulate_user_login(
        Config, OidcSpec, Url, UserAttributes
    ),
    oz_test_utils:call_oz(Config, auth_logic, validate_login, [
        <<"GET">>, MockedCowboyReq
    ]).


validate_login_in_test_mode(MockedCowboyReq) ->
    {ok, UserId, _} = auth_logic:validate_login(<<"GET">>, MockedCowboyReq),
    UserData = auth_test_mode:get_user_data(),
    {ok, UserId, UserData}.


%% IdPConfigs :: {IdP, OidcSpec, ProtocolConfig}
overwrite_auth_config(TestConfig, TestMode, IdPConfigs) ->
    ConfigData = #{
        openidConfig => #{
            enabled => true
        },
        supportedIdps => lists:map(fun({IdP, OidcSpec, ProtocolConfig}) ->
            {IdP, #{
                protocol => openid,
                protocolConfig => ProtocolConfig#{
                    plugin => default_oidc_plugin,
                    pluginConfig => #{
                        endpoints => OidcSpec#oidc_spec.endpoints,
                        clientId => OidcSpec#oidc_spec.clientId,
                        clientSecret => OidcSpec#oidc_spec.clientSecret,
                        scope => OidcSpec#oidc_spec.scope,
                        accessTokenAcquireMethod => OidcSpec#oidc_spec.accessTokenAcquireMethod,
                        clientSecretPassMethod => OidcSpec#oidc_spec.clientSecretPassMethod,
                        accessTokenPassMethod => OidcSpec#oidc_spec.accessTokenPassMethod,
                        customData => OidcSpec#oidc_spec.customData
                    }
                }
            }}
        end, IdPConfigs)
    },
    case TestMode of
        false -> oz_test_utils:overwrite_auth_config(TestConfig, ConfigData);
        true -> oz_test_utils:overwrite_test_auth_config(TestConfig, ConfigData)
    end.
