%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Framework test scenarios used across different api SUITES.
%%% @end
%%%-------------------------------------------------------------------
-module(api_test_scenarios).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("api_test_utils.hrl").
-include("rest.hrl").
-include("entity_logic.hrl").
-include("errors.hrl").
-include_lib("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

-export([run_scenario/2]).
-export([get_relations/4]).
-export([get_privileges/5, get_privileges/6]).
-export([update_privileges/5]).

run_scenario(Function, Args) ->
    try
        erlang:apply(?MODULE, Function, Args),
        true
    catch
        throw:fail ->
            false;
        Type:Message ->
            ct:print(
                "Unexpected error in ~p:run_scenario - ~p:~p~nStacktrace: ~s",
                [?MODULE, Type, Message, lager:pr_stacktrace(erlang:get_stacktrace())]
            ),
            false
    end.


get_relations(Config, ListApiTestSpec, GetApiTestSpecGenerator, CreateRelationFuns) ->
    ExpectRelations = fun(Ids) ->
        #api_test_spec{
            rest_spec = RestSpec, logic_spec = LogicSpec
        } = ListApiTestSpec,
        NewRestSpec = case RestSpec of
            undefined ->
                undefined;
            _ ->
                RestExpBody = maps:map(
                    fun(_Key, _OldIds) ->
                        Ids
                    end, RestSpec#rest_spec.expected_body),
                RestSpec#rest_spec{
                    expected_body = RestExpBody
                }
        end,
        NewLogicSpec = case LogicSpec of
            undefined ->
                undefined;
            _ ->
                LogicSpec#logic_spec{
                    expected_result = ?OK_LIST(Ids)
                }
        end,
        ListApiTestSpec#api_test_spec{
            rest_spec = NewRestSpec,
            logic_spec = NewLogicSpec
        }
    end,
    % Start with empty list of relations
    assert(api_test_utils:run_tests(Config, ExpectRelations([]))),
    % Evaluate CreateRelationFuns one by one and check if expected relations
    % accumulate.
    % Functions can create relations that can be included or excluded.
    ExpectedRelations = lists:foldl(fun(CreateFun, AccMap) ->
        {ExcludeOrInclude, Map} = CreateFun(),
        NewAccMap = AccMap#{
            ExcludeOrInclude => maps:merge(maps:get(ExcludeOrInclude, AccMap), Map)
        },
        oz_test_utils:ensure_eff_graph_up_to_date(Config),
        assert(api_test_utils:run_tests(
            Config, ExpectRelations(maps:keys(maps:get(include, NewAccMap))))
        ),
        NewAccMap
    end, #{include => #{}, exclude => #{}}, CreateRelationFuns),
    % Check every entity one by one if it's data can be retrieved
    % Start with entities that should be reachable (included)
    maps:map(
        fun(EntityId, EntityData) ->
            ApiTestSpec = GetApiTestSpecGenerator(include, EntityId, EntityData),
            assert(api_test_utils:run_tests(Config, ApiTestSpec))
        end, maps:get(include, ExpectedRelations)),
    % Now entities that should be not reachable (excluded)
    maps:map(
        fun(EntityId, EntityData) ->
            ApiTestSpec = GetApiTestSpecGenerator(exclude, EntityId, EntityData),
            assert(api_test_utils:run_tests(Config, ApiTestSpec))
        end, maps:get(exclude, ExpectedRelations)).




get_privileges(Config, ApiTestSpec, SetPrivsFun, AllPrivs, InitialPrivs) ->
    get_privileges(Config, ApiTestSpec, SetPrivsFun, AllPrivs, InitialPrivs, []).


get_privileges(Config, ApiTestSpec, SetPrivsFun, AllPrivs, InitialPrivs, ConstantPrivs) ->
    ExpectPrivileges = fun(PrivilegesAtoms) ->
        PrivilegesBin = [atom_to_binary(P, utf8) || P <- PrivilegesAtoms],
        #api_test_spec{
            rest_spec = RestSpec, logic_spec = LogicSpec
        } = ApiTestSpec,
        NewRestSpec = case RestSpec of
            undefined ->
                undefined;
            _ ->
                RestSpec#rest_spec{
                    expected_body = #{<<"privileges">> => PrivilegesBin}
                }
        end,
        NewLogicSpec = case LogicSpec of
            undefined ->
                undefined;
            _ ->
                LogicSpec#logic_spec{
                    expected_result = ?OK_LIST(PrivilegesAtoms)
                }
        end,
        ApiTestSpec#api_test_spec{
            rest_spec = NewRestSpec,
            logic_spec = NewLogicSpec
        }
    end,
    % Check if endpoint returns InitialPrivileges as expected.
    assert(api_test_utils:run_tests(Config, ExpectPrivileges(InitialPrivs))),
    % Try setting all possible sublists of privileges and check if they are
    % correctly returned.
    Sublists = [lists:sublist(AllPrivs, I) || I <- lists:seq(0, length(AllPrivs))],
    lists:foreach(
        fun(PrivsSublist) ->
            SetPrivsFun(set, lists:usort(PrivsSublist ++ ConstantPrivs)),
            assert(api_test_utils:run_tests(
                Config, ExpectPrivileges(lists:usort(PrivsSublist ++ ConstantPrivs))
            ))
        end, Sublists).

% TODO
%% AllPrivs :: [atom()].
update_privileges(Config, ApiTestSpec, SetPrivsFun, AllPrivs, InitialPrivs) ->
    % Function that returns api_test_spec with given expected privileges.
    ExpectPrivileges = fun(TypeOfExpectation, PrivilegesAtoms) ->
        PrivilegesBin = [atom_to_binary(P, utf8) || P <- PrivilegesAtoms],
        {RestExpectation, LogicExpectation} = case TypeOfExpectation of
            exact -> {
                #{<<"privileges">> => PrivilegesBin},
                ?OK_LIST(PrivilegesAtoms)
            };
            contains -> {
                #{<<"privileges">> => {list_contains, PrivilegesBin}},
                ?OK_LIST_CONTAINS(PrivilegesAtoms)
            };
            doesnt_contain -> {
                #{<<"privileges">> => {list_doesnt_contain, PrivilegesBin}},
                ?OK_LIST_DOESNT_CONTAIN(PrivilegesAtoms)
            }
        end,
        #api_test_spec{
            rest_spec = RestSpec, logic_spec = LogicSpec
        } = ApiTestSpec,
        ApiTestSpec#api_test_spec{
            rest_spec = RestSpec#rest_spec{
                expected_body = RestExpectation
            },
            logic_spec = LogicSpec#logic_spec{
                expected_result = LogicExpectation
            }
        }
    end,
    % Check if endpoint returns InitialPrivileges as expected.
    assert(api_test_utils:run_tests(
        Config, ExpectPrivileges(exact, InitialPrivs))
    ),
    % Try SETing all possible sublists of privileges and check if they are
    % correctly returned.
    Sublists = [lists:sublist(AllPrivs, I) || I <- lists:seq(0, length(AllPrivs))],
    lists:foreach(
        fun(PrivsSublist) ->
            SetPrivsFun(set, PrivsSublist),
            assert(api_test_utils:run_tests(
                Config, ExpectPrivileges(exact, PrivsSublist))
            )
        end, Sublists),
    % Try GRANTing all sublists of privileges and check if they are included
    % in privileges.
    lists:foreach(
        fun(PrivsSublist) ->
            % First set random privileges and then grant a sublist of privileges
            % (it should not matter what privileges were set before, after
            % GRANTing new ones they should all be present)
            SetPrivsFun(set, lists:sublist(AllPrivs, rand:uniform(length(AllPrivs)))),
            SetPrivsFun(grant, PrivsSublist),
            assert(api_test_utils:run_tests(
                Config, ExpectPrivileges(contains, PrivsSublist))
            )
        end, Sublists),
    % Try REVOKing all sublists of privileges and check if they are not included
    % in privileges.
    lists:foreach(
        fun(PrivsSublist) ->
            % First set random privileges and then revoke a sublist of privileges
            % (it should not matter what privileges were set before, after
            % REVOKing new ones they should all be absent)
            SetPrivsFun(set, lists:sublist(AllPrivs, rand:uniform(length(AllPrivs)))),
            SetPrivsFun(revoke, PrivsSublist),
            assert(api_test_utils:run_tests(
                Config, ExpectPrivileges(doesnt_contain, PrivsSublist))
            )
        end, Sublists).


assert(true) ->
    ok;
assert(_) ->
    throw(fail).


