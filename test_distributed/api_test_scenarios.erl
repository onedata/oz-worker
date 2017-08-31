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
-include_lib("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("cluster_worker/include/api_errors.hrl").

-export([run_scenario/2]).
-export([delete_entity/4]).
-export([get_relations/4]).
-export([get_privileges/8]).
-export([update_privileges/7]).
-export([delete_privileges/7]).


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


delete_entity(Config, ApiTestSpec, InitFun, VerifyEndFun) ->
    %% run initial test configuration
    assert(api_test_utils:run_tests(
        Config, ApiTestSpec, InitFun, VerifyEndFun
    )),

    % prepare api_test_spec for entity cannot be deleted twice test
    #api_test_spec{
        client_spec = #client_spec{
            correct = Correct,
            unauthorized = Unauthorized,
            forbidden = Forbidden
        },
        rest_spec = RestSpec,
        logic_spec = #logic_spec{
            module = Module,
            function = Function,
            args = Args
        } = LogicSpec,
        gs_spec = GsSpec
    } = ApiTestSpec,

    NewRestSpec = case RestSpec of
                      undefined ->
                          undefined;
                      _ ->
                          RestSpec#rest_spec{
                              expected_code = ?HTTP_404_NOT_FOUND
                          }
                  end,
    NewGsSpec = case GsSpec of
                    undefined ->
                        undefined;
                    _ ->
                        GsSpec#gs_spec{
                            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
                        }
                end,

    NewApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = Correct ++ Unauthorized ++ Forbidden
        },
        rest_spec = NewRestSpec,
        logic_spec = LogicSpec#logic_spec{
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        },
        gs_spec = NewGsSpec
    },

    % check that entity cannot be deleted twice
    Env = InitFun(),
    MustSucceedArgs = lists:map(
        fun(client) -> ?ROOT;
            (Arg) -> maps:get(Arg, Env, Arg)
        end, Args
    ),
    assert(oz_test_utils:call_oz(
        Config, Module, Function, MustSucceedArgs
    )),

    assert(api_test_utils:run_tests(
        Config, NewApiTestSpec, fun() -> Env end, fun api_test_utils:end_env/2
    )).



get_privileges(
    Config, ApiTestSpec, SetPrivsFun, AllPrivs,
    InitialPrivs, ConstPrivs, Entity, ViewPriv
) ->
    % run tests with original api_test_spec without entity;
    % assert initial privileges are returned
    assert(api_test_utils:run_tests(
        Config, (get_privs_test_specs_gen(ApiTestSpec))(InitialPrivs)
    )),

    % tests without entity
    run_get_privs_scenarios(
        Config, get_privs_test_specs_gen(ApiTestSpec),
        SetPrivsFun, AllPrivs, ConstPrivs
    ),

    % tests with entity with view privilege
    EntityTestSpec = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{correct = [Entity]}
    },
    run_get_privs_scenarios(
        Config, get_privs_test_specs_gen(EntityTestSpec), SetPrivsFun,
        AllPrivs, lists:usort(ConstPrivs ++ [ViewPriv])
    ),

    % tests with entity without view privilege
    ForbiddenApiTestSpec = create_forbidden_test_spec(ApiTestSpec, Entity),
    run_get_privs_scenarios(
        Config, fun(_) -> ForbiddenApiTestSpec end, SetPrivsFun,
        AllPrivs -- [ViewPriv], ConstPrivs -- [ViewPriv]
    ).


run_get_privs_scenarios(
    Config, GenApiTestSpec, SetPrivsFun, AllPrivs, ConstPrivs
) ->
    lists:foreach(
        fun(PrivsSublist) ->
            Privs = lists:usort(PrivsSublist ++ ConstPrivs),

            InitFun =
                fun() ->
                    SetPrivsFun(set, Privs),
                    #{privs => Privs}
                end,

            assert(api_test_utils:run_tests(
                Config, GenApiTestSpec(Privs), InitFun,
                fun api_test_utils:end_env/2
            ))
        end, [
            lists:sublist(AllPrivs, I) || I <- lists:seq(0, length(AllPrivs))
        ]).


get_privs_test_specs_gen(ApiTestSpec) ->
    #api_test_spec{
        rest_spec = RestSpec,
        logic_spec = LogicSpec,
        gs_spec = GsSpec
    } = ApiTestSpec,

    fun(PrivilegesAtoms) ->
        PrivilegesBin = [atom_to_binary(P, utf8) || P <- PrivilegesAtoms],
        NewRestSpec = case RestSpec of
                          undefined ->
                              undefined;
                          _ ->
                              ExpBody = #{<<"privileges">> => PrivilegesBin},
                              RestSpec#rest_spec{
                                  expected_body = ExpBody
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
        NewGsSpec = case GsSpec of
                        undefined ->
                            undefined;
                        _ ->
                            GsSpec#gs_spec{
                                expected_result = ?OK_LIST(PrivilegesAtoms)
                            }
                    end,

        ApiTestSpec#api_test_spec{
            rest_spec = NewRestSpec,
            logic_spec = NewLogicSpec,
            gs_spec = NewGsSpec
        }
    end.


update_privileges(
    Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs, Entity, UpdatePriv
) ->
    VerifyEndFun =
        fun
            (#{correctData := true} = Env, Data) ->
                DataPrivs = lists:sort(maps:get(<<"privileges">>, Data)),
                Operation = maps:get(<<"operation">>, Data, set),
                InitialPrivs = maps:get(privs, Env),
                ActualPrivs = lists:sort(GetPrivsFun()),
                case Operation of
                    set ->
                        ?assertEqual(DataPrivs, ActualPrivs);
                    grant ->
                        ?assertEqual(
                            lists:usort(InitialPrivs ++ DataPrivs), ActualPrivs
                        );
                    revoke ->
                        ?assertEqual(DataPrivs -- ActualPrivs, DataPrivs)
                end;
            (_, _) ->
                ok
        end,

    % run tests with original api_test_spec without entity
    run_update_privs_scenarios(
        Config, ApiTestSpec, SetPrivsFun, VerifyEndFun, AllPrivs, []
    ),

    % run tests with entity with update privilege
    EntityTestSpec = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{correct = [Entity]}
    },
    run_update_privs_scenarios(
        Config, EntityTestSpec, SetPrivsFun,
        VerifyEndFun, AllPrivs, [UpdatePriv]
    ),

    % run tests with entity without update privilege
    % (it's not granted by accident during any run,
    % as it can be in original scenario)
    ForbiddenApiTestSpec = create_forbidden_test_spec(ApiTestSpec, Entity),
    ForbiddenVerifyEndFun =
        fun
            (#{correctData := true} = Env, _Data) ->
                ActualPrivs = lists:sort(GetPrivsFun()),
                InitialPrivs = maps:get(privs, Env),
                ?assertEqual(InitialPrivs, ActualPrivs);
            (_, _) ->
                ok
        end,
    run_update_privs_scenarios(
        Config, ForbiddenApiTestSpec, SetPrivsFun,
        ForbiddenVerifyEndFun, AllPrivs -- [UpdatePriv], []
    ).


% run scenarios for set/grant/revoke operations
run_update_privs_scenarios(
    Config, ApiTestSpec, SetPrivsFun, VerifyEndFun, AllPrivs, ConstPrivs
) ->
    run_set_privs_scenarios(
        Config, ApiTestSpec, SetPrivsFun,
        VerifyEndFun, AllPrivs, ConstPrivs
    ),
    run_grant_privs_scenarios(
        Config, ApiTestSpec, SetPrivsFun,
        VerifyEndFun, AllPrivs, ConstPrivs
    ),
    run_revoke_privs_scenarios(
        Config, ApiTestSpec, SetPrivsFun,
        VerifyEndFun, AllPrivs, ConstPrivs
    ).


run_set_privs_scenarios(
    Config, ApiTestSpec, SetPrivsFun, VerifyEndFun, AllPrivs, ConstPrivs
) ->
    SetInitFun =
        fun() ->
            SetPrivsFun(set, ConstPrivs),
            #{privs => ConstPrivs}
        end,
    lists:foreach(
        fun(PrivsSublist) ->
            NewApiTestSpec = ApiTestSpec#api_test_spec{
                data_spec = #data_spec{
                    required = [<<"privileges">>],
                    optional = [<<"operation">>],
                    correct_values = #{
                        <<"privileges">> => [PrivsSublist],
                        <<"operation">> => [set]
                    }
                }
            },
            assert(api_test_utils:run_tests(
                Config, NewApiTestSpec, SetInitFun, VerifyEndFun
            ))
        end, [lists:sublist(AllPrivs, I) || I <- lists:seq(0, length(AllPrivs))]
    ).


run_grant_privs_scenarios(
    Config, ApiTestSpec, SetPrivsFun, VerifyEndFun, AllPrivs, ConstPrivs
) ->
    GrantInitFun =
        fun() ->
            RandPrivs = lists:sublist(
                AllPrivs, rand:uniform(length(AllPrivs))
            ),
            InitialPrivs = lists:usort(ConstPrivs ++ RandPrivs),
            SetPrivsFun(set, InitialPrivs),
            #{privs => InitialPrivs}
        end,
    lists:foreach(
        fun(PrivsSublist) ->
            NewApiTestSpec = ApiTestSpec#api_test_spec{
                data_spec = #data_spec{
                    required = [<<"privileges">>, <<"operation">>],
                    correct_values = #{
                        <<"privileges">> => [PrivsSublist],
                        <<"operation">> => [grant]
                    }
                }
            },
            assert(api_test_utils:run_tests(
                Config, NewApiTestSpec, GrantInitFun, VerifyEndFun
            ))
        end, [lists:sublist(AllPrivs, I) || I <- lists:seq(0, length(AllPrivs))]
    ).


run_revoke_privs_scenarios(
    Config, ApiTestSpec, SetPrivsFun, VerifyEndFun, AllPrivs, _ConstPrivs
) ->
    RevokeInitFun =
        fun() ->
            SetPrivsFun(set, AllPrivs),
            #{privs => AllPrivs}
        end,
    lists:foreach(
        fun(PrivsSublist) ->
            NewApiTestSpec = ApiTestSpec#api_test_spec{
                data_spec = #data_spec{
                    required = [<<"privileges">>, <<"operation">>],
                    correct_values = #{
                        <<"privileges">> => [PrivsSublist],
                        <<"operation">> => [revoke]
                    }
                }
            },
            assert(api_test_utils:run_tests(
                Config, NewApiTestSpec, RevokeInitFun, VerifyEndFun
            ))
        end, [lists:sublist(AllPrivs, I) || I <- lists:seq(0, length(AllPrivs))]
    ).


delete_privileges(
    Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs, Entity, DeletePriv
) ->
    % run tests with original api_test_spec with entity
    InitFun =
        fun() ->
            SetPrivsFun(set, AllPrivs),
            #{privs => AllPrivs}
        end,
    VerifyEndFun =
        fun
            (#{correctData := true} = _Env, _Data) ->
                ActualPrivs = lists:sort(GetPrivsFun()),
                ?assertEqual([], ActualPrivs);
            (_, _) ->
                ok
        end,
    assert(api_test_utils:run_tests(
        Config, ApiTestSpec, InitFun, VerifyEndFun
    )),

    % run tests with entity without delete privilege
    ForbiddenApiTestSpec = create_forbidden_test_spec(ApiTestSpec, Entity),
    ForbiddenInitFun =
        fun() ->
            Privs = AllPrivs -- [DeletePriv],
            SetPrivsFun(set, Privs),
            #{privs => Privs}
        end,
    ForbiddenVerifyEndFun =
        fun
            (#{correctData := true} = Env, _Data) ->
                ActualPrivs = lists:sort(GetPrivsFun()),
                InitialPrivs = maps:get(privs, Env),
                ?assertEqual(InitialPrivs, ActualPrivs);
            (_, _) ->
                ok
        end,
    assert(api_test_utils:run_tests(
        Config, ForbiddenApiTestSpec, ForbiddenInitFun, ForbiddenVerifyEndFun
    )).


create_forbidden_test_spec(ApiTestSpec, Clients) when not is_list(Clients) ->
    create_forbidden_test_spec(ApiTestSpec, [Clients]);
create_forbidden_test_spec(ApiTestSpec, Clients) ->
    #api_test_spec{
        rest_spec = RestSpec,
        logic_spec = LogicSpec,
        gs_spec = GsSpec
    } = ApiTestSpec,

    NewRestSpec =
        case RestSpec of
            undefined ->
                undefined;
            _ ->
                RestSpec#rest_spec{
                    expected_code = ?HTTP_403_FORBIDDEN,
                    expected_body = undefined,
                    expected_headers = undefined
                }
        end,
    NewLogicSpec =
        case LogicSpec of
            undefined ->
                undefined;
            _ ->
                LogicSpec#logic_spec{
                    expected_result = ?ERROR_REASON(?ERROR_FORBIDDEN)
                }
        end,
    NewGsSpec =
        case GsSpec of
            undefined ->
                undefined;
            _ ->
                GsSpec#gs_spec{
                    expected_result = ?ERROR_REASON(?ERROR_FORBIDDEN)
                }
        end,

    ApiTestSpec#api_test_spec{
        client_spec = #client_spec{correct = Clients},
        rest_spec = NewRestSpec,
        logic_spec = NewLogicSpec,
        gs_spec = NewGsSpec
    }.


% TODO remove
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


assert(true) -> ok;
assert(ok) -> ok;
assert(_) -> throw(fail).
