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
-module(flat_entitlement_parser_tests).

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
            ?assertEqual(Expected, flat_entitlement_parser:parse(?DUMMY_IDP, Input, ParserCfg)),
            run_test_cases(CaseNum + 1)
    end.


% Note - the test cases assume the default settings of flat_entitlement_parser:
% groupType => team
% groupPrivilegesInVo => member
% userPrivileges => member
test_case(1, input) -> <<"some-group">>;
test_case(1, parserCfg) -> #{};
test_case(1, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = team, name = <<"some-group">>, privileges = member}
], privileges = member};

test_case(2, input) -> <<"some-group">>;
test_case(2, parserCfg) -> #{groupType => organization};
test_case(2, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = organization, name = <<"some-group">>, privileges = member}
], privileges = member};

test_case(3, input) -> <<"some-group">>;
test_case(3, parserCfg) -> #{groupType => unit, groupPrivilegesInVo => manager};
test_case(3, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = unit, name = <<"some-group">>, privileges = manager}
], privileges = member};

test_case(4, input) -> <<"some-group">>;
test_case(4, parserCfg) -> #{groupType => team};
test_case(4, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = team, name = <<"some-group">>, privileges = member}
], privileges = member};

test_case(5, input) -> <<"some-group">>;
test_case(5, parserCfg) -> #{groupType => role_holder, groupPrivilegesInVo => admin};
test_case(5, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = role_holder, name = <<"some-group">>, privileges = admin}
], privileges = member};

test_case(6, input) -> <<"some-group">>;
test_case(6, parserCfg) -> #{groupType => organization, userPrivileges => member};
test_case(6, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = organization, name = <<"some-group">>, privileges = member}
], privileges = member};

test_case(7, input) -> <<"some-group">>;
test_case(7, parserCfg) -> #{groupType => organization, userPrivileges => manager};
test_case(7, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = organization, name = <<"some-group">>, privileges = member}
], privileges = manager};

test_case(8, input) -> <<"some-group">>;
test_case(8, parserCfg) -> #{groupType => organization, userPrivileges => admin};
test_case(8, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = organization, name = <<"some-group">>, privileges = member}
], privileges = admin};

test_case(9, input) -> <<"some-group/with/slashes">>;
test_case(9, parserCfg) -> #{groupType => unit, groupPrivilegesInVo => manager, userPrivileges => admin};
test_case(9, expected) -> #idp_entitlement{idp = ?DUMMY_IDP, path = [
    #idp_group{type = unit, name = <<"some-group/with/slashes">>, privileges = manager}
], privileges = admin};

test_case(_, _) ->
    finish.

-endif.