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
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("cluster_worker/include/api_errors.hrl").

-export([run_scenario/2]).
-export([delete_entity/4, delete_entity/5]).
-export([get_relations/4]).
-export([get_privileges/7]).
-export([update_privileges/7]).
-export([delete_privileges/7]).

-export([
    create_eff_groups_env/1,
    create_eff_spaces_env/1,
    create_eff_providers_env/1,
    create_eff_handle_services_env/1,
    create_eff_handles_env/1
]).


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
                [
                    ?MODULE, Type, Message,
                    lager:pr_stacktrace(erlang:get_stacktrace())
                ]
            ),
            false
    end.


% Test that entity can be deleted, and every succeeding call results in 403.
% In case when entity can delete itself, it should not be listed in
% client spec, but additionally added as argument to this fun
delete_entity(Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun) ->
    delete_entity(Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, undefined).


delete_entity(Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, Entity) ->
    #api_test_spec{
        client_spec = #client_spec{
            correct = Correct,
            unauthorized = Unauthorized,
            forbidden = Forbidden
        } = ClientSpec,
        rest_spec = RestSpec,
        logic_spec = #logic_spec{
            module = Module,
            function = Function,
            args = Args
        } = LogicSpec,
        gs_spec = GsSpec
    } = ApiTestSpec,

    % In case of entity that can delete itself add it to correct clients
    NewApiTestSpec = case Entity of
                       undefined ->
                           ApiTestSpec;
                       _ ->
                           ApiTestSpec#api_test_spec{
                               client_spec = ClientSpec#client_spec{
                                   correct = [Entity | Correct]
                               }
                           }
                   end,

    assert(api_test_utils:run_tests(
        Config, NewApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % To check that entity cannot be deleted twice and every succeeding
    % call return ERROR_NOT_FOUND, it's first created with EnvSetUpFun,
    % deleted using logic (it is assumed there always will be logic fun
    % to delete entity), tests run against delete entity id.
    % Do not include entity (if entity deleted itself,
    % it cannot check if it's deleted, right?)
    EntityCanNotBeDeletedTwiceTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = Correct ++ Unauthorized ++ Forbidden
        },
        rest_spec = prepare_entity_not_found_rest_spec(RestSpec),
        logic_spec = LogicSpec#logic_spec{
            expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
        },
        gs_spec = prepare_entity_not_found_gs_spec(GsSpec)
    },

    % check that entity cannot be deleted twice
    Env = EnvSetUpFun(),
    PreparedArgs = lists:map(
        fun(client) -> ?ROOT;
            (Arg) -> maps:get(Arg, Env, Arg)
        end, Args
    ),
    ?assertMatch(ok, oz_test_utils:call_oz(
        Config, Module, Function, PreparedArgs
    )),

    assert(api_test_utils:run_tests(
        Config, EntityCanNotBeDeletedTwiceTestSpec,
        fun() -> Env end, undefined, undefined
    )).


prepare_entity_not_found_rest_spec(undefined) ->
    undefined;
prepare_entity_not_found_rest_spec(RestSpec) ->
    RestSpec#rest_spec{
        expected_code = ?HTTP_404_NOT_FOUND
    }.


prepare_entity_not_found_gs_spec(undefined) ->
    undefined;
prepare_entity_not_found_gs_spec(GsSpec) ->
    GsSpec#gs_spec{
        expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
    }.


% Set different privileges and run api test spec against them;
% Entity whose privileges will be changing during those tests
% (either normal or effective) should not be listed in client spec but
% provided additionally, so that spec can be adjusted for that case.
% Exception to this is when entity directly tries to get it's privileges,
% then it should be listed as only correct client and provided as argument
get_privileges(
    Config, ApiTestSpec, SetPrivsFun, AllPrivs, ConstPrivs, Entity, ViewPriv
) ->
    % Run original spec
    assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % In case of getting entity privileges for itself, skip this step,
    % as all it is doing is setting various combinations of privileges
    % and trying to get them (so it is valid only for clients that always
    % can get privileges)
    case ApiTestSpec#api_test_spec.client_spec#client_spec.correct of
        [Entity] ->
            ok;
        _ ->
            run_get_privs_tests(
                Config, ApiTestSpec, SetPrivsFun, AllPrivs, ConstPrivs
            )
    end,

    % Replace clients with entity and assert it can get privileges
    % when view priv is granted in environment setup
    EntityTestSpec = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{correct = [Entity]}
    },
    run_get_privs_tests(
        Config, EntityTestSpec, SetPrivsFun,
        AllPrivs, lists:usort(ConstPrivs ++ [ViewPriv])
    ),

    % Replace clients with entity and assert it can not get privileges
    % when all privileges but view one is set
    SetPrivsFun(set, AllPrivs -- [ViewPriv]),
    ForbiddenApiTestSpec = create_forbidden_test_spec(ApiTestSpec, Entity),
    api_test_utils:run_tests(Config, ForbiddenApiTestSpec).


run_get_privs_tests(Config, ApiTestSpec, SetPrivsFun, AllPrivs, ConstPrivs) ->
    lists:foreach(
        fun(PrivsSublist) ->
            Privs = lists:usort(PrivsSublist ++ ConstPrivs),
            EnvSetUpFun = fun() ->
                SetPrivsFun(set, Privs),
                #{privs => Privs}
            end,
            assert(api_test_utils:run_tests(
                Config, prepare_get_privs_api_spec(ApiTestSpec, Privs),
                EnvSetUpFun, undefined, undefined
            ))
        end, [
            lists:sublist(AllPrivs, I) || I <- lists:seq(0, length(AllPrivs))
        ]
    ).


prepare_get_privs_api_spec(ApiTestSpec, PrivsAtoms) ->
    #api_test_spec{
        rest_spec = RestSpec,
        logic_spec = LogicSpec,
        gs_spec = GsSpec
    } = ApiTestSpec,

    PrivsBin = [atom_to_binary(P, utf8) || P <- PrivsAtoms],

    ApiTestSpec#api_test_spec{
        rest_spec = prepare_get_privs_rest_spec(RestSpec, PrivsBin),
        logic_spec = prepare_get_privs_logic_spec(LogicSpec, PrivsAtoms),
        gs_spec = prepare_get_privs_gs_spec(GsSpec, PrivsAtoms)
    }.


prepare_get_privs_rest_spec(undefined, _Privs) ->
    undefined;
prepare_get_privs_rest_spec(RestSpec, Privs) ->
    RestSpec#rest_spec{
        expected_code = ?HTTP_200_OK,
        expected_body = #{<<"privileges">> => Privs}
    }.


prepare_get_privs_logic_spec(undefined, _Privs) ->
    undefined;
prepare_get_privs_logic_spec(LogicSpec, Privs) ->
    LogicSpec#logic_spec{
        expected_result = ?OK_LIST(Privs)
    }.


prepare_get_privs_gs_spec(undefined, _Privs) ->
    undefined;
prepare_get_privs_gs_spec(GsSpec, Privs) ->
    GsSpec#gs_spec{
        expected_result = ?OK_LIST(Privs)
    }.


% Scenario with entity whose privs will be changing during test run
update_privileges(
    Config, #api_test_spec{
        client_spec = #client_spec{correct = [Entity]}
    } = ApiTestSpec,
    SetPrivsFun, GetPrivsFun, AllPrivs, Entity, UpdatePriv
) ->
    % Run tests giving entity always update priv
    run_update_privs_tests(
        Config, ApiTestSpec, SetPrivsFun,
        prepare_update_privs_verify_fun(GetPrivsFun),
        AllPrivs, [UpdatePriv]
    ),

    % Run tests with entity without update privilege
    ForbiddenVerifyEndFun = fun(_, #{privs := InitialPrivs} = _Env, _Data) ->
        ActualPrivs = lists:sort(GetPrivsFun()),
        ?assertEqual(InitialPrivs, ActualPrivs)
    end,
    run_update_privs_tests(
        Config, create_forbidden_test_spec(ApiTestSpec, Entity),
        SetPrivsFun, ForbiddenVerifyEndFun, AllPrivs -- [UpdatePriv], []
    );
% Scenario when no client which eff privs will be affected during test
% run are listed in client spec, but given as argument
update_privileges(
    Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs, Entity, UpdatePriv
) ->
    % Run original spec without any constant privs
    run_update_privs_tests(
        Config, ApiTestSpec, SetPrivsFun,
        prepare_update_privs_verify_fun(GetPrivsFun),
        AllPrivs, []
    ),

    % Run scenario with entity, whose eff privs are affected during execution
    EntityTestSpec = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{correct = [Entity]}
    },
    update_privileges(
        Config, EntityTestSpec, SetPrivsFun, GetPrivsFun,
        AllPrivs, Entity, UpdatePriv
    ).


prepare_update_privs_verify_fun(GetPrivsFun) ->
    fun
        (true = _ShouldSucceed, #{privs := InitialPrivs} = _Env, Data) ->
            DataPrivs = lists:sort(maps:get(<<"privileges">>, Data)),
            Operation = maps:get(<<"operation">>, Data, set),
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
        (false = _ShouldSucceed, #{privs := InitialPrivs} = _Env, _) ->
            ActualPrivs = lists:sort(GetPrivsFun()),
            ?assertEqual(InitialPrivs, ActualPrivs)
    end.


run_update_privs_tests(
    Config, ApiTestSpec, SetPrivsFun, VerifyEndFun, AllPrivs, ConstPrivs
) ->
    run_set_privs_tests(
        Config, ApiTestSpec, SetPrivsFun, VerifyEndFun, AllPrivs, ConstPrivs
    ),
    run_grant_privs_tests(
        Config, ApiTestSpec, SetPrivsFun, VerifyEndFun, AllPrivs, ConstPrivs
    ),
    run_revoke_privs_tests(
        Config, ApiTestSpec, SetPrivsFun, VerifyEndFun, AllPrivs, ConstPrivs
    ).


run_set_privs_tests(
    Config, ApiTestSpec, SetPrivsFun, VerifyEndFun, AllPrivs, ConstPrivs
) ->
    SetPrivsEnvSetUpFun = fun() ->
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
                Config, NewApiTestSpec, SetPrivsEnvSetUpFun,
                undefined, VerifyEndFun
            ))
        end, [
            lists:sublist(AllPrivs, I) || I <- lists:seq(0, length(AllPrivs))
        ]
    ).


run_grant_privs_tests(
    Config, ApiTestSpec, SetPrivsFun, VerifyEndFun, AllPrivs, ConstPrivs
) ->
    GrantPrivsEnvSetUpFun = fun() ->
        RandPrivs = lists:sublist(AllPrivs, rand:uniform(length(AllPrivs))),
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
                Config, NewApiTestSpec, GrantPrivsEnvSetUpFun,
                undefined, VerifyEndFun
            ))
        end, [
            lists:sublist(AllPrivs, I) || I <- lists:seq(0, length(AllPrivs))
        ]
    ).


run_revoke_privs_tests(
    Config, ApiTestSpec, SetPrivsFun, VerifyEndFun, AllPrivs, _ConstPrivs
) ->
    RevokePrivsEnvSetUpFun = fun() ->
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
                Config, NewApiTestSpec, RevokePrivsEnvSetUpFun,
                undefined, VerifyEndFun
            ))
        end, [
            lists:sublist(AllPrivs, I) || I <- lists:seq(0, length(AllPrivs))
        ]
    ).


% Set all privileges and assert all correct clients can delete them;
% For entity whose privileges are affected (normal or effective ones)
% create also separate spec and set up all privileges but delete one,
% run tests and assert it cannot delete them
delete_privileges(
    Config, ApiTestSpec, SetPrivsFun, GetPrivsFun,
    AllPrivs, Entity, DeletePriv
) ->
    EnvSetUpFun = fun() ->
        SetPrivsFun(set, AllPrivs),
        #{privs => AllPrivs}
    end,
    VerifyEndFun =
        fun
            (true = _ShouldSucceed, _Env, _Data) ->
                ActualPrivs = lists:sort(GetPrivsFun()),
                ?assertEqual([], ActualPrivs);
            (false = _ShouldSucceed, _, _) ->
                ActualPrivs = lists:sort(GetPrivsFun()),
                ?assertEqual(AllPrivs, ActualPrivs)
        end,
    assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    ForbiddenApiTestSpec = create_forbidden_test_spec(ApiTestSpec, Entity),
    ForbiddenEnvSetUpFun = fun() ->
        Privs = AllPrivs -- [DeletePriv],
        SetPrivsFun(set, Privs),
        #{privs => Privs}
    end,
    ForbiddenVerifyEndFun = fun(_, #{privs := InitialPrivs} = _Env, _Data) ->
        ActualPrivs = lists:sort(GetPrivsFun()),
        ?assertEqual(InitialPrivs, ActualPrivs)
    end,
    assert(api_test_utils:run_tests(
        Config, ForbiddenApiTestSpec, ForbiddenEnvSetUpFun,
        undefined, ForbiddenVerifyEndFun
    )).


% Create spec for Clients where expected result is 403/{error, forbidden}
create_forbidden_test_spec(ApiTestSpec, Clients) when not is_list(Clients) ->
    create_forbidden_test_spec(ApiTestSpec, [Clients]);
create_forbidden_test_spec(ApiTestSpec, Clients) ->
    #api_test_spec{
        rest_spec = RestSpec,
        logic_spec = LogicSpec,
        gs_spec = GsSpec
    } = ApiTestSpec,

    ApiTestSpec#api_test_spec{
        client_spec = #client_spec{correct = Clients},
        rest_spec = create_forbidden_rest_spec(RestSpec),
        logic_spec = create_forbidden_logic_spec(LogicSpec),
        gs_spec = create_forbidden_gs_spec(GsSpec)
    }.


create_forbidden_rest_spec(undefined) ->
    undefined;
create_forbidden_rest_spec(RestSpec) ->
    RestSpec#rest_spec{
        expected_code = ?HTTP_403_FORBIDDEN,
        expected_body = undefined,
        expected_headers = undefined
    }.


create_forbidden_logic_spec(undefined) ->
    undefined;
create_forbidden_logic_spec(LogicSpec) ->
    LogicSpec#logic_spec{
        expected_result = ?ERROR_REASON(?ERROR_FORBIDDEN)
    }.


create_forbidden_gs_spec(undefined) ->
    undefined;
create_forbidden_gs_spec(GsSpec) ->
    GsSpec#gs_spec{
        expected_result = ?ERROR_REASON(?ERROR_FORBIDDEN)
    }.


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
assert(_) -> throw(fail).


%%%===================================================================
%%% Functions creating starting environment
%%%===================================================================


create_eff_groups_env(Config) ->
    %% Create environment with following relations:
    %%
    %%      Group5
    %%         \
    %%          \
    %%         Group4
    %%            \
    %%             \
    %%            Group3    Group2
    %%               \      /
    %%                \    /
    %%                Group1
    %%               /      \
    %%        [group_view]   \
    %%            /           \
    %%         User1          User2
    %%
    %%      <<user>>
    %%      NonAdmin

    Groups = [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _}] = lists:map(
        fun(Idx) ->
            IdxBin = integer_to_binary(Idx),
            GroupDetails = #{
                <<"name">> => <<"Group", IdxBin/binary>>,
                <<"type">> => lists:nth(
                    rand:uniform(4), [unit, organization, team, role]
                )
            },
            {ok, GroupId} = oz_test_utils:create_group(
                Config, ?ROOT, GroupDetails
            ),
            {GroupId, GroupDetails}
        end, lists:seq(1, 5)
    ),

    {ok, G4} = oz_test_utils:add_group_to_group(Config, G5, G4),
    {ok, G3} = oz_test_utils:add_group_to_group(Config, G4, G3),
    {ok, G1} = oz_test_utils:add_group_to_group(Config, G3, G1),
    {ok, G1} = oz_test_utils:add_group_to_group(Config, G2, G1),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, U1} = oz_test_utils:add_user_to_group(Config, G1, U1),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, set, [
        ?GROUP_VIEW
    ]),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, revoke, [
        ?GROUP_VIEW
    ]),

    {Groups, {U1, U2, NonAdmin}}.


create_eff_spaces_env(Config) ->
    %% Create environment with following relations:
    %%
    %%  Space4   Space5
    %%      \     /
    %%       \   /
    %%      Group5    Space3
    %%          \      /
    %%           \    /
    %%           Group4           Space2
    %%               \            /
    %%                \          /
    %%               Group3    Group2
    %%                  \      /        Space1
    %%                   \    /         /
    %%                   Group1 -------/
    %%                  /      \
    %%           [group_view]   \
    %%               /           \
    %%            User1          User2
    %%
    %%      <<user>>
    %%      NonAdmin

    {
        [{G1, _}, {G2, _}, _, {G4, _}, {G5, _}] = Groups, Users
    } = create_eff_groups_env(Config),

    Spaces = lists:map(
        fun({Idx, GroupId}) ->
            IdxBin = integer_to_binary(Idx),
            SpaceDetails = #{<<"name">> => <<"Space", IdxBin/binary>>},
            {ok, SpaceId} = oz_test_utils:create_space_for_group(
                Config, GroupId, SpaceDetails
            ),
            {SpaceId, SpaceDetails#{<<"providersSupports">> => #{}}}
        end, [{1, G1}, {2, G2}, {3, G4}, {4, G5}, {5, G5}]
    ),

    {Spaces, Groups, Users}.


create_eff_providers_env(Config) ->
    %% Create environment with following relations:
    %%
    %%
    %%  Prov3       Prov4
    %%    |          /
    %%    |         /
    %%  Space4   Space5
    %%      \     /
    %%       \   /
    %%      Group5    Space3 ----- Prov2
    %%          \      /             |
    %%           \    /              |
    %%           Group4           Space2
    %%               \            /
    %%                \          /
    %%               Group3    Group2
    %%                  \      /        Space1
    %%                   \    /         /   \
    %%                   Group1 -------/     \
    %%                  /      \            Prov1
    %%           [group_view]   \
    %%               /           \
    %%            User1          User2
    %%
    %%      <<user>>
    %%      NonAdmin

    {
        [{S1, _}, {S2, _}, {S3, _}, {S4, _}, {S5, _}] = Spaces, Groups, Users
    } = create_eff_spaces_env(Config),

    Providers = [{P1, _}, {P2, _}, {P3, _}, {P4, _}] = lists:map(
        fun(Idx) ->
            {_, CSRFile, _} = oz_test_utils:generate_provider_cert_files(),
            {ok, CSR} = file:read_file(CSRFile),
            ProvDetails = #{
                <<"name">> => <<"Prov", (Idx+48)/integer>>,
                <<"urls">> => [<<"127.0.0.1">>],
                <<"redirectionPoint">> => <<"https://127.0.0.1">>,
                <<"csr">> => CSR,
                <<"latitude">> => rand:uniform() * 90,
                <<"longitude">> => rand:uniform() * 180
            },
            {ok, {ProvId, _}} = oz_test_utils:create_provider(
                Config, ProvDetails
            ),
            {ProvId, maps:remove(<<"csr">>, ProvDetails)}
        end, lists:seq(1, 4)
    ),

    lists:foreach(
        fun({ProvId, SpaceId}) ->
            {ok, Macaroon} = oz_test_utils:space_invite_provider_token(
                Config, ?ROOT, SpaceId
            ),
            {ok, Token} = token_utils:serialize62(Macaroon),
            {ok, SpaceId} = oz_test_utils:support_space(
                Config, ?ROOT, ProvId, Token,
                oz_test_utils:minimum_support_size(Config)
            )
        end, [{P1, S1}, {P2, S2}, {P2, S3}, {P3, S4}, {P4, S5}]
    ),

    {Providers, Spaces, Groups, Users}.


create_eff_handle_services_env(Config) ->
    %% Create environment with following relations:
    %%
    %% HService4  HService5
    %%      \     /
    %%       \   /
    %%      Group5   HService3
    %%          \      /
    %%           \    /
    %%           Group4        HService2
    %%               \            /
    %%                \          /
    %%               Group3    Group2
    %%                  \      /       HService1
    %%                   \    /         /
    %%                   Group1 -------/
    %%                  /      \
    %%           [group_view]   \
    %%               /           \
    %%            User1          User2
    %%
    %%      <<user>>
    %%      NonAdmin

    {
        [{G1, _}, {G2, _}, _, {G4, _}, {G5, _}] = Groups, Users
    } = create_eff_groups_env(Config),

    HandleServices = lists:map(
        fun({Idx, GroupId}) ->
            Name = <<"HS", (Idx+48)/integer>>,
            ProxyEndpoint = <<"https://dot", (Idx+48)/integer, ".com">>,
            Properties = #{<<"asd">> => 1},
            {ok, HSid} = oz_test_utils:create_handle_service(
                Config, ?ROOT, Name, ProxyEndpoint, Properties
            ),
            HSDetails = #{
                <<"name">> => Name,
                <<"proxyEndpoint">> => ProxyEndpoint,
                <<"serviceProperties">> => Properties
            },
            {ok, GroupId} = oz_test_utils:add_group_to_handle_service(
                Config, HSid, GroupId
            ),
            {HSid, HSDetails}
        end, [{1, G1}, {2, G2}, {3, G4}, {4, G5}, {5, G5}]
    ),

    {HandleServices, Groups, Users}.


create_eff_handles_env(Config) ->
    %% Create environment with following relations:
    %%
    %% Handle4  Handle5
    %%      \     /
    %%       \   /
    %%      Group5   Handle3
    %%          \      /
    %%           \    /
    %%           Group4        Handle2
    %%               \            /
    %%                \          /
    %%               Group3    Group2
    %%                  \      /       Handle1
    %%                   \    /         /
    %%                   Group1 --------
    %%                  /      \
    %%           [group_view]   \
    %%               /           \
    %%            User1          User2
    %%
    %%      <<user>>
    %%      NonAdmin

    {
        [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _}] = Groups, Users
    } = create_eff_groups_env(Config),

    {ok, HandleServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, G1} = oz_test_utils:add_group_to_handle_service(
        Config, HandleServiceId, G1
    ),

    Handles = lists:map(
        fun({Idx, GroupId}) ->
            {ok, SpaceId} = oz_test_utils:create_space_for_group(
                Config, GroupId, <<"Space", (Idx+48)/integer>>
            ),
            UniqueInt = erlang:unique_integer([positive]),
            ShareId = <<"Share", (integer_to_binary(UniqueInt))/binary>>,
            {ok, ShareId} = oz_test_utils:create_share(
                Config, ?ROOT, ShareId, <<"share">>, <<"file">>, SpaceId
            ),
            HandleDetails = ?HANDLE(HandleServiceId, ShareId),
            {ok, HandleId} = oz_test_utils:create_handle(
                Config, ?ROOT, HandleDetails
            ),
            {ok, GroupId} = oz_test_utils:add_group_to_handle(
                Config, HandleId, GroupId
            ),
            {HandleId, HandleDetails}
        end, [{1, G1}, {2, G2}, {3, G3}, {4, G4}, {5, G5}]
    ),

    {Handles, Groups, Users}.
