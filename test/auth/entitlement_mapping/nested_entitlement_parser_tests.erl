%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains eunit tests of nested_entitlement_parser module.
%%% @end
%%%-------------------------------------------------------------------
-module(nested_entitlement_parser_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("auth/entitlement_mapping.hrl").


-define(DUMMY_IDP, idp).

%%%===================================================================
%%% Tests functions
%%%===================================================================

parser_test() ->
    run_test_cases(1).


run_test_cases(CaseNum) ->
    case test_case(CaseNum, input) of
        finish ->
            ok;
        Input ->
            Expected = test_case(CaseNum, expected),
            ParserCfg = test_case(CaseNum, parserCfg),
            ?assertEqual(Expected, nested_entitlement_parser:parse(?DUMMY_IDP, Input, ParserCfg)),
            run_test_cases(CaseNum + 1)
    end.


% Note - the test cases assume the default settings of nested_entitlement_parser:
% splitWith => "/"
% topGroupType => team
% topGroupPrivilegesInVo => member
% subGroupsType => team
% subGroupsPrivilegesInParent => member
% userPrivileges => member
test_case(1, input) -> <<"some-group">>;
test_case(1, parserCfg) -> #{};
test_case(1, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = team, name = <<"some-group">>, privileges = member}
], privileges = member};

test_case(2, input) -> <<"some-group">>;
test_case(2, parserCfg) -> #{topGroupType => organization, topGroupPrivilegesInVo => admin};
test_case(2, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = organization, name = <<"some-group">>, privileges = admin}
], privileges = member};

test_case(3, input) -> <<"some-group">>;
test_case(3, parserCfg) -> #{topGroupType => unit};
test_case(3, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = unit, name = <<"some-group">>, privileges = member}
], privileges = member};

test_case(4, input) -> <<"some-group">>;
test_case(4, parserCfg) -> #{topGroupType => team, topGroupPrivilegesInVo => manager};
test_case(4, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = team, name = <<"some-group">>, privileges = manager}
], privileges = member};

test_case(5, input) -> <<"some-group">>;
test_case(5, parserCfg) -> #{topGroupType => role_holder};
test_case(5, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = role_holder, name = <<"some-group">>, privileges = member}
], privileges = member};

test_case(6, input) -> <<"some-group">>;
test_case(6, parserCfg) -> #{topGroupType => organization, userPrivileges => member};
test_case(6, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = organization, name = <<"some-group">>, privileges = member}
], privileges = member};

test_case(7, input) -> <<"some-group">>;
test_case(7, parserCfg) -> #{topGroupType => organization, userPrivileges => manager};
test_case(7, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = organization, name = <<"some-group">>, privileges = member}
], privileges = manager};

test_case(8, input) -> <<"some-group">>;
test_case(8, parserCfg) -> #{topGroupType => organization, userPrivileges => admin};
test_case(8, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = organization, name = <<"some-group">>, privileges = member}
], privileges = admin};

test_case(9, input) -> <<"some-group/with/slashes">>;
test_case(9, parserCfg) -> #{topGroupType => unit, userPrivileges => admin};
test_case(9, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = unit, name = <<"some-group">>, privileges = member},
    #idp_group{type = team, name = <<"with">>, privileges = member},
    #idp_group{type = team, name = <<"slashes">>, privileges = member}
], privileges = admin};

test_case(10, input) -> <<"some-group/with/slashes">>;
test_case(10, parserCfg) -> #{splitWith => "-", topGroupType => organization, userPrivileges => manager};
test_case(10, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = organization, name = <<"some">>, privileges = member},
    #idp_group{type = team, name = <<"group/with/slashes">>, privileges = member}
], privileges = manager};

test_case(11, input) -> <<"a/b/c/d/e/f">>;
test_case(11, parserCfg) -> #{
    topGroupType => unit, topGroupPrivilegesInVo => admin,
    subGroupsType => role_holder, subGroupsPrivilegesInParent => manager,
    userPrivileges => admin
};
test_case(11, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = unit, name = <<"a">>, privileges = admin},
    #idp_group{type = role_holder, name = <<"b">>, privileges = manager},
    #idp_group{type = role_holder, name = <<"c">>, privileges = manager},
    #idp_group{type = role_holder, name = <<"d">>, privileges = manager},
    #idp_group{type = role_holder, name = <<"e">>, privileges = manager},
    #idp_group{type = role_holder, name = <<"f">>, privileges = manager}
], privileges = admin};

test_case(12, input) -> <<":a:b:c:d/e:f">>;
test_case(12, parserCfg) -> #{
    splitWith => ":",
    topGroupType => organization, topGroupPrivilegesInVo => manager,
    subGroupsType => unit, subGroupsPrivilegesInParent => member,
    userPrivileges => member
};
test_case(12, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = organization, name = <<"a">>, privileges = manager},
    #idp_group{type = unit, name = <<"b">>, privileges = member},
    #idp_group{type = unit, name = <<"c">>, privileges = member},
    #idp_group{type = unit, name = <<"d/e">>, privileges = member},
    #idp_group{type = unit, name = <<"f">>, privileges = member}
], privileges = member};

test_case(_, _) ->
    finish.


-endif.