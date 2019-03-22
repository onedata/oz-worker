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
-module(attribute_mapping_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("auth/auth_errors.hrl").
-include("http/gui_paths.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("esaml/include/esaml.hrl").

-define(DUMMY_IDP, dummy_idp).


attribute_mapping_test_() ->
    {setup,
        fun setup/0,
        fun teardown/1,
        [?_test(map_attributes())]
    }.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    meck:new(auth_config, [passthrough]),
    meck:expect(auth_config, get_attribute_mapping, fun(?DUMMY_IDP, Attr) ->
        maps:get(Attr, get_mocked_attribute_mapping_cfg(), undefined)
    end),

    meck:new(my_attr_mapper, [non_strict]),
    meck:expect(my_attr_mapper, map_attribute, fun
        (?DUMMY_IDP, name, IdPAttributes) ->
            {ok, maps:get(<<"displayName">>, IdPAttributes, <<"">>)};
        (?DUMMY_IDP, emails, _IdPAttributes) ->
            {ok, []};
        (?DUMMY_IDP, custom, _IdPAttributes) ->
            {error, not_found}
    end),

    ok.

teardown(_) ->
    ?assert(meck:validate(auth_config)),
    ok = meck:unload(auth_config),
    ?assert(meck:validate(my_attr_mapper)),
    ok = meck:unload(my_attr_mapper).


get_mocked_attribute_mapping_cfg() ->
    oz_worker:get_env(mocked_attribute_mapping_cfg, #{}).


set_mocked_attribute_mapping_cfg(Cfg) ->
    oz_worker:set_env(mocked_attribute_mapping_cfg, Cfg).


%%%===================================================================
%%% Tests functions
%%%===================================================================

map_attributes() ->
    run_test_cases(1).


run_test_cases(CaseNum) ->
    case test_case(CaseNum, config) of
        finish ->
            ok;
        Config ->
            set_mocked_attribute_mapping_cfg(Config),
            Attributes = test_case(CaseNum, attributes),
            Expected = test_case(CaseNum, expected),
            Got = (catch attribute_mapping:map_attributes(?DUMMY_IDP, Attributes)),
            TestPassed = case Expected of
                Fun when is_function(Fun) -> Fun(Got);
                Val -> Val =:= Got
            end,
            case TestPassed of
                true ->
                    ok;
                false ->
                    io:format(user, "~nTest case ~B failed,~nexpected: ~p~ngot: ~p~n", [
                        CaseNum, Expected, Got
                    ]),
                    ?assert(false)
            end,
            run_test_cases(CaseNum + 1)
    end.


test_case(1, config) -> #{
    subjectId => {required, "id"},
    name => {optional, {any, ["name", "surName"]}},
    emails => {required, "mail"},
    entitlements => {optional, "groups"},
    custom => {optional, {keyValue, "custom"}}
};
test_case(1, attributes) -> #{
    <<"id">> => <<"abcdef">>,
    <<"name">> => <<"John">>,
    <<"surName">> => <<"Doe">>,
    <<"fullName">> => <<"John Doe">>,
    <<"login">> => <<"jodoe">>,
    <<"mail">> => <<"jodoe@example.com">>,
    <<"groups">> => [<<"some/nested/structure">>],
    <<"roles">> => <<"admins">>,
    <<"custom">> => [1, 2, 3, <<"a">>, <<"b">>, <<"c">>]
};
test_case(1, expected) -> #linked_account{
    idp = ?DUMMY_IDP,
    subject_id = <<"abcdef">>,
    name = <<"John">>,
    alias = undefined,
    emails = [<<"jodoe@example.com">>],
    entitlements = [<<"some/nested/structure">>],
    custom = #{<<"custom">> => [1, 2, 3, <<"a">>, <<"b">>, <<"c">>]}
};
%% ------------------------------------
%% ------------------------------------
test_case(2, config) -> #{
    subjectId => {required, {any, ["id", "uid", "eduPersonUniqueName"]}},
    name => {optional, {any, ["name", "surName"]}},
    alias => undefined,
    emails => {required, "mail"},
    entitlements => {optional, "groups"},
    custom => {optional, {keyValue, "custom"}}
};
test_case(2, attributes) -> #{
    <<"sub">> => <<"abcdef">>,
    <<"name">> => <<"John">>,
    <<"surName">> => <<"Doe">>,
    <<"fullName">> => <<"John Doe">>,
    <<"login">> => <<"jodoe">>,
    <<"mail">> => <<"jodoe@example.com">>,
    <<"groups">> => [<<"some/nested/structure">>],
    <<"roles">> => <<"admins">>,
    <<"custom">> => [1, 2, 3, <<"a">>, <<"b">>, <<"c">>]
};
test_case(2, expected) -> ?ERROR_CANNOT_RESOLVE_REQUIRED_ATTRIBUTE(subjectId);
%% ------------------------------------
%% ------------------------------------
test_case(3, config) -> #{
    subjectId => {required, "id"},
    name => {required, {any, ["name", "surName"]}},
    alias => undefined,
    emails => {required, "mail"},
    entitlements => {optional, "groups"},
    custom => {optional, {keyValue, "custom"}}
};
test_case(3, attributes) -> #{
    <<"id">> => <<"abcdef">>,
    <<"surName">> => [12345, <<"abc">>],
    <<"login">> => <<"jodoe">>,
    <<"mail">> => <<"jodoe@example.com">>,
    <<"groups">> => [<<"some/nested/structure">>],
    <<"roles">> => <<"admins">>,
    <<"custom">> => [1, 2, 3, <<"a">>, <<"b">>, <<"c">>]
};
test_case(3, expected) -> ?ERROR_BAD_ATTRIBUTE_TYPE(name, binary_or_undef);
%% ------------------------------------
%% ------------------------------------
test_case(4, config) -> #{
    subjectId => {required, "id"},
    % string cannot be appended to JSON
    name => {required, {append, ["name", {keyValue, "jsonKey", "custom"}]}},
    alias => undefined,
    emails => {required, "mail"},
    entitlements => {optional, "groups"},
    custom => {optional, {keyValue, "custom"}}
};
test_case(4, attributes) -> #{
    <<"id">> => <<"abcdef">>,
    <<"name">> => <<"John">>,
    <<"surName">> => <<"Doe">>,
    <<"fullName">> => <<"John Doe">>,
    <<"login">> => <<"jodoe">>,
    <<"mail">> => <<"jodoe@example.com">>,
    <<"groups">> => [<<"some/nested/structure">>],
    <<"roles">> => <<"admins">>,
    <<"custom">> => [1, 2, 3, <<"a">>, <<"b">>, <<"c">>]
};
test_case(4, expected) -> fun
    (?ERROR_ATTRIBUTE_MAPPING_ERROR(name, _, _, _, _)) -> true;
    (_) -> false
end;
%% ------------------------------------
%% ------------------------------------
test_case(5, config) -> #{
    subjectId => {required, {replace, "c", "d", "id"}},
    % Custom mapper (always returns {error, not_found}), see the setup/1 function
    custom => {plugin, my_attr_mapper}
};
test_case(5, attributes) -> #{
    <<"id">> => <<"abcdef">>,
    <<"name">> => [<<"John">>, <<"Doe">>],
    <<"groups">> => [
        <<"some">>,
        <<"entitlement">>,
        <<"from">>,
        <<"idp">>
    ],
    <<"attr1">> => #{
        <<"nestedAttrs">> => [
            #{<<"nestedListAttr">> => #{<<"value">> => <<"value1">>}},
            #{<<"nestedListAttr">> => #{<<"value">> => <<"value2">>}},
            #{<<"nestedListAttr">> => #{<<"value">> => <<"value3">>}}
        ]
    }
};
test_case(5, expected) ->
    ?ERROR_CANNOT_RESOLVE_REQUIRED_ATTRIBUTE(custom);
%% ------------------------------------
%% ------------------------------------
test_case(6, config) -> #{
    subjectId => {required, "id"},
    alias => undefined,
    emails => {required, {any, [{concat, [{str, "dummy-idp:"}, "mail"]}, "validMail"]}},
    custom => {required, {keyValue, "customAttrs", "custom"}}
};
test_case(6, attributes) -> #{
    <<"id">> => <<"abcdef">>,
    <<"name">> => <<"John">>,
    <<"surName">> => <<"Doe">>,
    <<"fullName">> => <<"John Doe">>,
    <<"login">> => <<"jodoe">>,
    <<"mail">> => #{<<"emailFormatThatWouldCrashTheMappingRules">> => 12345},
    <<"validMail">> => <<"jodoe@example.com">>,
    <<"groups">> => [<<"some/nested/structure">>],
    <<"roles">> => <<"admins">>,
    <<"custom">> => [1, 2, 3, <<"a">>, <<"b">>, <<"c">>]
};
test_case(6, expected) -> #linked_account{
    idp = ?DUMMY_IDP,
    subject_id = <<"abcdef">>,
    name = undefined,
    alias = undefined,
    emails = [<<"jodoe@example.com">>],
    entitlements = [],
    custom = #{<<"customAttrs">> => [1, 2, 3, <<"a">>, <<"b">>, <<"c">>]}
};
%% ------------------------------------
%% ------------------------------------
test_case(7, config) -> #{
    subjectId => {required, {any, ["sub", "id"]}},
    name => {optional, {concat, [{str, "Prefix "}, "name", {str, " "}, "surName"]}},
    alias => {required, {str, "literal-alias"}},
    emails => {required, {append, ["emails", "mainEmail"]}},
    entitlements => {optional, {filter, "^#.*", "groups"}}
};
test_case(7, attributes) -> #{
    <<"id">> => <<"abcdef">>,
    <<"name">> => <<"John">>,
    <<"surName">> => <<"Doe">>,
    <<"fullName">> => <<"John Doe">>,
    <<"login">> => <<"jodoe">>,
    <<"emails">> => [<<"j@example.com">>, <<"doe@example.com">>],
    <<"mainEmail">> => <<"jodoe@example.com">>,
    <<"groups">> => [
        <<"g1">>,
        <<"#g1-with-hash">>,
        <<"g2">>,
        <<"#g2-with-hash">>
    ],
    <<"roles">> => <<"admins">>,
    <<"custom">> => [1, 2, 3, <<"a">>, <<"b">>, <<"c">>]
};
test_case(7, expected) -> #linked_account{
    idp = ?DUMMY_IDP,
    subject_id = <<"abcdef">>,
    name = <<"Prefix John Doe">>,
    alias = <<"literal-alias">>,
    emails = [<<"j@example.com">>, <<"doe@example.com">>, <<"jodoe@example.com">>],
    entitlements = [<<"#g1-with-hash">>, <<"#g2-with-hash">>],
    custom = #{}
};
%% ------------------------------------
%% ------------------------------------
test_case(8, config) -> #{
    subjectId => {required, {any, ["sub", "id"]}},
    name => {optional, {replace, "'", "", "fullName"}},
    alias => {optional, {nested, ["alias", "value"]}},
    entitlements => {optional, {replace, "#", "", {filter, "^#.*", "groups"}}},
    custom => {required, {append, ["custom", "organization"]}}
};
test_case(8, attributes) -> #{
    <<"id">> => <<"abcdef">>,
    <<"name">> => <<"John">>,
    <<"surName">> => <<"Mc'Donald">>,
    <<"fullName">> => <<"John Mc'Donald">>,
    <<"alias">> => #{
        <<"value">> => <<"jodoe">>
    },
    <<"emails">> => [<<"j@example.com">>, <<"doe@example.com">>],
    <<"mainEmail">> => <<"jodoe@example.com">>,
    <<"groups">> => [
        <<"g1">>,
        <<"#g1-with-hash">>,
        <<"g2">>,
        <<"#g2-with-hash">>
    ],
    <<"roles">> => <<"admins">>,
    <<"custom">> => #{
        <<"customAttributes">> => [1, 2, 3, <<"a">>, <<"b">>, <<"c">>]
    },
    <<"organization">> => #{
        <<"name">> => <<"My Organization">>,
        <<"domain">> => <<"my.organization.org">>
    }
};
test_case(8, expected) -> #linked_account{
    idp = ?DUMMY_IDP,
    subject_id = <<"abcdef">>,
    name = <<"John McDonald">>,
    alias = <<"jodoe">>,
    emails = [],
    entitlements = [<<"g1-with-hash">>, <<"g2-with-hash">>],
    custom = #{
        <<"customAttributes">> => [1, 2, 3, <<"a">>, <<"b">>, <<"c">>],
        <<"name">> => <<"My Organization">>,
        <<"domain">> => <<"my.organization.org">>
    }
};
%% ------------------------------------
%% ------------------------------------
test_case(9, config) -> #{
    subjectId => {required, {any, ["uid", {concat, [{str, "dummy-idp:"}, "eduPersonUniqueId"]}]}},
    % Custom mapper (simply picks "displayName"), see the setup/1 function
    name => {plugin, my_attr_mapper},
    alias => {required, "eduPersonPrincipalName"},
    % Custom mapper (always returns an empty list), see the setup/1 function
    emails => {plugin, my_attr_mapper},
    entitlements => {optional, {append, [
        {concat, [{str, "group"}, {filter, "^#.*", "eduPersonEntitlement"}]},
        {concat, [{str, "role#"}, "urn:oid:2.5.4.25"]}
    ]}},
    custom => {optional, {append, ["urn:oid:1.3.6.1.4.1.5923.1.1.1.999", {keyValue, "schacHomeOrganization"}]}}
};
test_case(9, attributes) -> #{
    <<"eduPersonUniqueId">> => <<"abcdef">>,
    <<"eduPersonTargetedId">> => <<"123456">>,
    <<"displayName">> => <<"John">>,
    <<"eduPersonPrincipalName">> => <<"jodoe">>,
    <<"emails">> => [],
    <<"eduPersonEntitlement">> => [
        <<"g1">>,
        <<"#g1-with-hash">>,
        <<"g2">>,
        <<"#g2-with-hash">>
    ],
    <<"urn:oid:2.5.4.25">> => [
        <<"admin">>,
        <<"CTO">>
    ],
    <<"urn:oid:1.3.6.1.4.1.5923.1.1.1.999">> => #{
        <<"firstAttr">> => <<"firstValue">>,
        <<"secondAttr">> => [<<"second">>, <<"value">>],
        <<"thirdAttr">> => #{
            <<"nested">> => <<"json">>
        },
        <<"fourthAttr">> => 17
    },
    <<"schacHomeOrganization">> => <<"dummyIdP">>
};
test_case(9, expected) -> #linked_account{
    idp = ?DUMMY_IDP,
    subject_id = <<"dummy-idp:abcdef">>,
    name = <<"John">>,
    alias = <<"jodoe">>,
    emails = [],
    entitlements = [<<"group#g1-with-hash">>, <<"group#g2-with-hash">>, <<"role#admin">>, <<"role#CTO">>],
    custom = #{
        <<"firstAttr">> => <<"firstValue">>,
        <<"secondAttr">> => [<<"second">>, <<"value">>],
        <<"thirdAttr">> => #{
            <<"nested">> => <<"json">>
        },
        <<"fourthAttr">> => 17,
        <<"schacHomeOrganization">> => <<"dummyIdP">>
    }
};
%% ------------------------------------
%% ------------------------------------
test_case(10, config) -> #{
    subjectId => {required, {any, ["sub", "id"]}},
    name => {required, {join, " ", {str_list, ["John", "Doe,", "Jr."]}}},
    alias => undefined,
    emails => {optional, "emails"},
    entitlements => {required, {append, [
        {concat, [
            {str, "team:"},
            {nested, ["groups", "teams", {list, "teamName"}]},
            {str, "#"},
            {nested, ["groups", "teams", {list, "membersCount"}]}
        ]},
        {concat, [{str, "role:"}, {split, ",", {nested, ["groups", "roles"]}}]},
        {concat, [{str, "unit:"}, {split, "\n", {nested, ["groups", "units"]}}]}
    ]}},
    custom => undefined
};
test_case(10, attributes) -> #{
    <<"id">> => <<"abcdef">>,
    <<"name">> => <<"John">>,
    <<"groups">> => #{
        <<"teams">> => [
            #{<<"teamName">> => <<"team1">>, <<"membersCount">> => 76},
            #{<<"teamName">> => <<"team2">>, <<"membersCount">> => 13},
            #{<<"teamName">> => <<"team3">>, <<"membersCount">> => 1}
        ],
        <<"roles">> => [<<"user,admin,vmOperator">>, <<"manager">>],
        <<"units">> => <<"unit1\nunit2\nunit3">>
    },
    <<"custom">> => [1, 2, 3, <<"a">>, <<"b">>, <<"c">>]
};
test_case(10, expected) -> #linked_account{
    idp = ?DUMMY_IDP,
    subject_id = <<"abcdef">>,
    name = <<"John Doe, Jr.">>,
    alias = undefined,
    emails = [],
    entitlements = [
        <<"team:team1#76">>,
        <<"team:team2#13">>,
        <<"team:team3#1">>,
        <<"role:user">>,
        <<"role:admin">>,
        <<"role:vmOperator">>,
        <<"role:manager">>,
        <<"unit:unit1">>,
        <<"unit:unit2">>,
        <<"unit:unit3">>
    ],
    custom = #{}
};
%% ------------------------------------
%% ------------------------------------
test_case(11, config) -> #{
    subjectId => {required, {any, ["sub", "id"]}},
    name => {required, {join, " ", "name"}},
    alias => undefined,
    emails => {optional, "emails"},
    entitlements => undefined,
    custom => {required, {nested, ["attr1", "nestedAttrs", {list, "nestedListAttr"}, "value"]}}
};
test_case(11, attributes) -> #{
    <<"id">> => <<"abcdef">>,
    <<"name">> => [<<"John">>, <<"Doe">>],
    <<"groups">> => [],
    <<"attr1">> => #{
        <<"nestedAttrs">> => [
            #{<<"nestedListAttr">> => #{<<"value">> => <<"value1">>}},
            #{<<"nestedListAttr">> => #{<<"value">> => <<"value2">>}},
            #{<<"nestedListAttr">> => #{<<"value">> => <<"value3">>}}
        ]
    }
};
test_case(11, expected) -> #linked_account{
    idp = ?DUMMY_IDP,
    subject_id = <<"abcdef">>,
    name = <<"John Doe">>,
    alias = undefined,
    emails = [],
    entitlements = [],
    custom = [<<"value1">>, <<"value2">>, <<"value3">>]
};
%% ------------------------------------
%% ------------------------------------
test_case(12, config) -> #{
    subjectId => {required, {replace, "c", "d", "id"}},
    entitlements => {optional, {concat, [
        {str_list, ["a", "b", "c", "d"]},
        {str, ":"},
        "groups",
        {str, "/"},
        {str_list, ["1", "2", "3", "4"]}
    ]}}
};
test_case(12, attributes) -> #{
    <<"id">> => <<"abcdef">>,
    <<"name">> => [<<"John">>, <<"Doe">>],
    <<"groups">> => [
        <<"some">>,
        <<"entitlement">>,
        <<"from">>,
        <<"idp">>
    ],
    <<"attr1">> => #{
        <<"nestedAttrs">> => [
            #{<<"nestedListAttr">> => #{<<"value">> => <<"value1">>}},
            #{<<"nestedListAttr">> => #{<<"value">> => <<"value2">>}},
            #{<<"nestedListAttr">> => #{<<"value">> => <<"value3">>}}
        ]
    }
};
test_case(12, expected) -> #linked_account{
    idp = ?DUMMY_IDP,
    subject_id = <<"abddef">>,
    name = undefined,
    alias = undefined,
    emails = [],
    entitlements = [
        <<"a:some/1">>,
        <<"b:entitlement/2">>,
        <<"c:from/3">>,
        <<"d:idp/4">>
    ],
    custom = #{}
};
%% ------------------------------------
%% ------------------------------------
test_case(13, config) -> #{
    subjectId => {required, {replace, "c", "x", "id"}},
    name => {optional, {any, ["fullName", {join, " ", "nameTokens"}]}},
    alias => undefined,
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
};
test_case(13, attributes) -> #{
    <<"id">> => <<"abcdef1c2c3c4c">>,
    <<"nameTokens">> => [<<"John">>, <<"Doe">>, <<"Jr">>],
    <<"alias">> => <<"jodoe">>,
    <<"groups">> => [
        <<"some">>,
        <<"entitlement">>,
        <<"from">>,
        <<"idp">>
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
};
test_case(13, expected) -> #linked_account{
    idp = ?DUMMY_IDP,
    subject_id = <<"abxdef1x2x3x4x">>,
    name = <<"John Doe Jr">>,
    alias = undefined,
    emails = [<<"john.doe@my.org">>],
    entitlements = [
        <<"a:some/1">>,
        <<"b:entitlement/2">>,
        <<"c:from/3">>,
        <<"d:idp/4">>
    ],
    custom = #{
        <<"firstAttr">> => <<"firstValue">>,
        <<"secondAttr">> => [<<"second">>, <<"value">>],
        <<"thirdAttr">> => #{
            <<"nested">> => <<"json">>
        },
        <<"fourthAttr">> => 17,
        <<"organization">> => <<"My Organization">>,
        <<"roles">> => [<<"role1">>, <<"role2">>, <<"role3">>]
    }
};
%% ------------------------------------
%% ------------------------------------
test_case(_, _) -> finish.




-endif.

