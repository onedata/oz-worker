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
-include_lib("ctool/include/api_errors.hrl").

-export([run_scenario/2]).
-export([delete_entity/5]).
-export([get_relations/4]).
-export([get_privileges/7]).
-export([update_privileges/7]).
-export([delete_privileges/7]).

-export([
    collect_unique_tokens_fun/0
]).
-export([
    create_basic_group_env/2,
    create_basic_space_env/2,
    create_basic_doi_hservice_env/2,
    create_basic_handle_env/2
]).
-export([
    create_eff_parent_groups_env/1,
    create_eff_child_groups_env/1,
    create_space_eff_users_env/1,
    create_provider_eff_users_env/1,
    create_hservice_eff_users_env/1,
    create_handle_eff_users_env/1,
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


% Test that entity can be deleted, and every succeeding call results in 404.
% For that DeleteFun, that will get initialized Env and delete entity,
% is required.
delete_entity(Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteFun) ->
    #api_test_spec{
        client_spec = #client_spec{
            correct = Correct,
            unauthorized = Unauthorized,
            forbidden = Forbidden
        },
        rest_spec = RestSpec,
        logic_spec = LogicSpec,
        gs_spec = GsSpec
    } = ApiTestSpec,

    assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    % To check that entity cannot be deleted twice and every succeeding
    % call returns ERROR_NOT_FOUND, entity is first created with EnvSetUpFun,
    % deleted using DeleteEntityFun and tests run against deleted entity id.
    ApiTestSpec2 = #api_test_spec{
        client_spec = #client_spec{
            correct = Correct ++ Unauthorized ++ Forbidden
        },
        rest_spec = prepare_entity_not_found_rest_spec(RestSpec),
        logic_spec = prepare_entity_not_found_logic_spec(LogicSpec),
        gs_spec = prepare_entity_not_found_gs_spec(GsSpec)
    },

    Env = EnvSetUpFun(),
    DeleteFun(Env),
    assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, fun() -> Env end, undefined, undefined
    )).


prepare_entity_not_found_rest_spec(undefined) ->
    undefined;
prepare_entity_not_found_rest_spec(RestSpec) ->
    RestSpec#rest_spec{
        expected_code = ?HTTP_404_NOT_FOUND
    }.


prepare_entity_not_found_logic_spec(undefined) ->
    undefined;
prepare_entity_not_found_logic_spec(LogicSpec) ->
    LogicSpec#logic_spec{
        expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
    }.


prepare_entity_not_found_gs_spec(undefined) ->
    undefined;
prepare_entity_not_found_gs_spec(GsSpec) ->
    GsSpec#gs_spec{
        expected_result = ?ERROR_REASON(?ERROR_NOT_FOUND)
    }.


% Set different privileges, run tests and check they are returned.
% Entity whose eff privileges will be changing during those tests
% should not be listed in client spec but provided additionally.
% Exception to this is when entity directly tries to get it's privileges,
% then it should be listed as only correct client and provided as argument
get_privileges(
    Config, ApiTestSpec, SetPrivsFun, AllPrivs, ConstPrivs, Entity, ViewPriv
) ->
    % Run original spec
    assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % In case of getting privileges for Entity (mainly in user api), skip this
    % step because eff privileges of entity are affected during test run and
    % it will be able to sometimes get privileges and sometimes not
    case ApiTestSpec#api_test_spec.client_spec#client_spec.correct of
        [Entity] ->
            ok;
        _ ->
            run_get_privs_tests(
                Config, ApiTestSpec, SetPrivsFun, AllPrivs, ConstPrivs
            )
    end,

    % Replace clients with entity and check if it can get privileges
    % when view priv is set
    EntityTestSpec = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{correct = [Entity]}
    },
    run_get_privs_tests(
        Config, EntityTestSpec, SetPrivsFun,
        AllPrivs, lists:usort(ConstPrivs ++ [ViewPriv])
    ),

    % Replace clients with entity and check if it can not get privileges
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
    } = ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs, Entity, UpdatePriv
) ->
    % Run tests giving entity always update priv
    run_update_privs_tests(
        Config, ApiTestSpec, SetPrivsFun,
        update_privs_verify_fun(GetPrivsFun),
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
        update_privs_verify_fun(GetPrivsFun),
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


update_privs_verify_fun(GetPrivsFun) ->
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


% Grant all oz privileges and check that correct clients can delete them but
% forbidden and unauthorized cannot.
% For entity whose effective privileges are affected (and as such it should be
% listed in correct clients and as Entity arg) create separate spec and set all
% privileges but delete one, run tests and check that it cannot delete them
delete_privileges(
    Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs, Entity, DeletePriv
) ->
    EnvSetUpFun = fun() ->
        SetPrivsFun(set, AllPrivs),
        #{privs => AllPrivs}
    end,
    VerifyEndFun = fun(ShouldSucceed, _Env, _Data) ->
        ActualPrivs = lists:sort(GetPrivsFun()),
        case ShouldSucceed of
            true -> ?assertEqual([], ActualPrivs);
            false -> ?assertEqual(AllPrivs, ActualPrivs)
        end
    end,
    assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    ApiTestSpec2 = create_forbidden_test_spec(ApiTestSpec, Entity),
    EnvSetUpFun2 = fun() ->
        Privs = AllPrivs -- [DeletePriv],
        SetPrivsFun(set, Privs),
        #{privs => Privs}
    end,
    VerifyEndFun2 = fun(_, #{privs := InitialPrivs} = _Env, _Data) ->
        ActualPrivs = lists:sort(GetPrivsFun()),
        ?assertEqual(InitialPrivs, ActualPrivs)
    end,
    assert(api_test_utils:run_tests(
        Config, ApiTestSpec2, EnvSetUpFun2, undefined, VerifyEndFun2
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
%%% Miscellaneous functions used in api tests
%%%===================================================================


collect_unique_tokens_fun() ->
    CollectUniqueTokens = fun Loop(Tokens) ->
        receive
            {Pid, Token} ->
                case lists:member(Token, Tokens) of
                    false ->
                        Pid ! true,
                        Loop([Token | Tokens]);
                    true ->
                        Pid ! false,
                        Loop(Tokens)
                end
        end
    end,
    CollectTokensProc = spawn_link(fun() -> CollectUniqueTokens([]) end),

    fun(Token) ->
        CollectTokensProc ! {self(), Token},
        IsTokenUnique =
            receive
                Response -> Response
            after 1000 ->
                false
            end,
        ?assert(IsTokenUnique),
        true
    end.


%%%===================================================================
%%% Functions creating starting environment
%%%===================================================================


create_basic_group_env(Config, Privs) when not is_list(Privs) ->
    create_basic_group_env(Config, [Privs]);
create_basic_group_env(Config, Privs) ->
    %% Create environment with following relations:
    %%
    %%                  Group
    %%                 /     \
    %%                /       \
    %%       [~privileges]  [privileges]
    %%              /           \
    %%           User1         User2

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, Group} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    oz_test_utils:group_set_user_privileges(
        Config, Group, U1, revoke, Privs
    ),
    {ok, U2} = oz_test_utils:group_add_user(Config, Group, U2),
    oz_test_utils:group_set_user_privileges(Config, Group, U2, set, Privs),

    {Group, U1, U2}.


create_basic_space_env(Config, Privs) when not is_list(Privs) ->
    create_basic_space_env(Config, [Privs]);
create_basic_space_env(Config, Privs) ->
    %% Create environment with following relations:
    %%
    %%                  Space
    %%                 /     \
    %%                /       \
    %%       [~privileges]  [privileges]
    %%              /           \
    %%           User1         User2

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, Space} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(
        Config, Space, U1, revoke, Privs
    ),
    {ok, U2} = oz_test_utils:space_add_user(Config, Space, U2),
    oz_test_utils:space_set_user_privileges(Config, Space, U2, set, Privs),

    {Space, U1, U2}.


create_basic_doi_hservice_env(Config, Privs) when not is_list(Privs) ->
    create_basic_doi_hservice_env(Config, [Privs]);
create_basic_doi_hservice_env(Config, Privs) ->
    %% Create environment with following relations:
    %%
    %%              HandleService
    %%                 /      \
    %%                /        \
    %%       [~privileges]  [privileges]
    %%              /            \
    %%           User1         User2

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, set, [
        ?OZ_HANDLE_SERVICES_CREATE
    ]),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?USER(U1), ?DOI_SERVICE
    ),
    oz_test_utils:handle_service_set_user_privileges(
        Config, HService, U1, revoke, Privs
    ),
    {ok, U2} = oz_test_utils:handle_service_add_user(Config, HService, U2),
    oz_test_utils:handle_service_set_user_privileges(
        Config, HService, U2, set, Privs
    ),

    {HService, U1, U2}.


create_basic_handle_env(Config, Privs) when not is_list(Privs) ->
    create_basic_handle_env(Config, [Privs]);
create_basic_handle_env(Config, Privs) ->
    %% Create environment with following relations:
    %%
    %%                  Handle
    %%                 /      \
    %%                /        \
    %%       [~privileges]  [privileges]
    %%              /            \
    %%           User1         User2

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, set, [
        ?OZ_HANDLE_SERVICES_CREATE
    ]),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?USER(U1), ?DOI_SERVICE
    ),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ?SHARE_ID_1, ?SHARE_NAME1, ?ROOT_FILE_ID, S1
    ),

    HandleDetails = ?HANDLE(HService, ShareId),
    {ok, HandleId} = oz_test_utils:create_handle(
        Config, ?USER(U1), HandleDetails
    ),
    oz_test_utils:handle_set_user_privileges(
        Config, HandleId, U1, revoke, Privs
    ),
    {ok, U2} = oz_test_utils:handle_add_user(Config, HandleId, U2),
    oz_test_utils:handle_set_user_privileges(
        Config, HandleId, U2, set, Privs
    ),

    {HandleId, U1, U2}.


create_eff_parent_groups_env(Config) ->
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
    %%      [group_view]  [~group_view]
    %%            /           \
    %%         User1          User2
    %%
    %%      <<user>>
    %%      NonAdmin

    Groups = [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _}] = lists:map(
        fun(_) ->
            GroupDetails = ?GROUP_DETAILS(?UNIQUE_STRING),
            {ok, GroupId} = oz_test_utils:create_group(
                Config, ?ROOT, GroupDetails
            ),
            {GroupId, GroupDetails}
        end, lists:seq(1, 5)
    ),

    {ok, G4} = oz_test_utils:group_add_group(Config, G5, G4),
    {ok, G3} = oz_test_utils:group_add_group(Config, G4, G3),
    {ok, G1} = oz_test_utils:group_add_group(Config, G3, G1),
    {ok, G1} = oz_test_utils:group_add_group(Config, G2, G1),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, U1} = oz_test_utils:group_add_user(Config, G1, U1),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, set, [
        ?GROUP_VIEW
    ]),
    {ok, U2} = oz_test_utils:group_add_user(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set,
        oz_test_utils:all_group_privileges(Config) -- [?GROUP_VIEW]
    ),

    {Groups, {U1, U2, NonAdmin}}.


create_eff_child_groups_env(Config) ->
    %% Create environment with following relations:
    %%
    %%                  Group1
    %%                 /      \
    %%                /        \
    %%             Group6     Group2
    %%              /         /     \
    %%           User4       /     User1
    %%                    Group3
    %%                    /    \
    %%                   /      \
    %%                Group4  Group5
    %%                 /          \
    %%              User2        User3

    Users = [{U1, _}, {U2, _}, {U3, _}, {U4, _}] = lists:map(
        fun(_) ->
            Alias = Name = ?UNIQUE_STRING,
            UserDetails = #{
                <<"alias">> => Alias,
                <<"name">> => Name
            },
            {ok, UserId} = oz_test_utils:create_user(Config, #od_user{
                name = Name, alias = Alias
            }),
            {UserId, UserDetails}
        end, lists:seq(1, 4)
    ),

    Groups = [{G1, _}, {G2, _}, {G3, _}, {G4, _}, {G5, _}, {G6, _}] =
        lists:map(
            fun(_) ->
                GroupDetails = ?GROUP_DETAILS(?UNIQUE_STRING),
                {ok, GroupId} = oz_test_utils:create_group(
                    Config, ?ROOT, GroupDetails
                ),
                {GroupId, GroupDetails}
            end, lists:seq(1, 6)
        ),

    lists:foreach(
        fun({ParentGroup, ChildGroup}) ->
            oz_test_utils:group_add_group(Config, ParentGroup, ChildGroup)
        end, [{G1, G6}, {G1, G2}, {G2, G3}, {G3, G4}, {G3, G5}]
    ),
    lists:foreach(
        fun({Group, User}) ->
            oz_test_utils:group_add_user(Config, Group, User)
        end, [{G2, U1}, {G6, U4}, {G4, U2}, {G5, U3}]
    ),

    {Groups, Users}.


create_space_eff_users_env(Config) ->
    %% Create environment with following relations:
    %%
    %%                  Space
    %%                 /  |  \
    %%                /   |   \
    %%     [~space_view]  |  [space_view]
    %%           /        |        \
    %%        User1     Group1    User2
    %%                 /      \
    %%                /        \
    %%             Group6     Group2
    %%              /         /    \
    %%           User6       /     User3
    %%                    Group3
    %%                    /    \
    %%                   /      \
    %%                Group4  Group5
    %%                 /          \
    %%               User4      User5
    %%
    %%      <<user>>
    %%      NonAdmin

    {
        [{G1, _} | _] = Groups, Users
    } = create_eff_child_groups_env(Config),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, U1, revoke, [
        ?SPACE_VIEW
    ]),
    {ok, U2} = oz_test_utils:space_add_user(Config, S1, U2),
    oz_test_utils:space_set_user_privileges(Config, S1, U2, set, [
        ?SPACE_VIEW
    ]),
    {ok, G1} = oz_test_utils:space_add_group(Config, S1, G1),

    {S1, Groups, Users, {U1, U2, NonAdmin}}.


create_provider_eff_users_env(Config) ->
    %% Create environment with following relations:
    %%
    %%                Provider
    %%                    |
    %%                  Space
    %%                 /  |  \
    %%                /   |   \
    %%     [~space_view]  |  [space_view]
    %%           /        |        \
    %%        User1     Group1    User2
    %%                 /      \
    %%                /        \
    %%             Group6     Group2
    %%              /         /    \
    %%           User6       /     User3
    %%                    Group3
    %%                    /    \
    %%                   /      \
    %%                Group4  Group5
    %%                 /          \
    %%               User4      User5
    %%
    %%      <<user>>
    %%      NonAdmin

    {
        S1, Groups, Users, {U1, U2, NonAdmin}
    } = create_space_eff_users_env(Config),

    {ok, {P1, Macaroon}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),
    {ok, S1} = oz_test_utils:support_space(
        Config, P1, S1, oz_test_utils:minimum_support_size(Config)
    ),

    {{P1, Macaroon}, S1, Groups, Users, {U1, U2, NonAdmin}}.


create_hservice_eff_users_env(Config) ->
    %% Create environment with following relations:
    %%
    %%              HandleService
    %%                 /  |  \
    %%                /   |   \
    %%   [~hservice_view] |  [hservice_view]
    %%           /        |        \
    %%        User1     Group1    User2
    %%                 /      \
    %%                /        \
    %%             Group6     Group2
    %%              /         /    \
    %%           User6       /     User3
    %%                    Group3
    %%                    /    \
    %%                   /      \
    %%                Group4  Group5
    %%                 /          \
    %%               User4      User5
    %%
    %%      <<user>>
    %%      NonAdmin

    {
        [{G1, _} | _] = Groups, Users
    } = api_test_scenarios:create_eff_child_groups_env(Config),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, set, [
        ?OZ_HANDLE_SERVICES_CREATE
    ]),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?USER(U1), ?DOI_SERVICE
    ),
    oz_test_utils:handle_service_set_user_privileges(Config, HService, U1,
        revoke, [?HANDLE_SERVICE_VIEW]
    ),
    {ok, U2} = oz_test_utils:handle_service_add_user(Config, HService, U2),
    oz_test_utils:handle_service_set_user_privileges(Config, HService, U2,
        set, [?HANDLE_SERVICE_VIEW]
    ),
    {ok, G1} = oz_test_utils:handle_service_add_group(Config, HService, G1),

    {HService, Groups, Users, {U1, U2, NonAdmin}}.


create_handle_eff_users_env(Config) ->
    %% Create environment with following relations:
    %%
    %%                  Handle
    %%                 /  |   \
    %%                /   |    \
    %%   [~hservice_view] |   [hservice_view]
    %%           /        |         \
    %%        User1     Group1     User2
    %%                 /      \
    %%                /        \
    %%             Group6     Group2
    %%              /         /    \
    %%           User6       /     User3
    %%                    Group3
    %%                    /    \
    %%                   /      \
    %%                Group4  Group5
    %%                 /          \
    %%               User4      User5
    %%
    %%      <<user>>
    %%      NonAdmin

    {
        [{G1, _} | _] = Groups, Users
    } = api_test_scenarios:create_eff_child_groups_env(Config),

    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, U1, set, [
        ?OZ_HANDLE_SERVICES_CREATE
    ]),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?USER(U1), ?DOI_SERVICE
    ),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ?SHARE_ID_1, ?SHARE_NAME1, ?ROOT_FILE_ID, S1
    ),

    HandleDetails = ?HANDLE(HService, ShareId),
    {ok, HandleId} = oz_test_utils:create_handle(
        Config, ?USER(U1), HandleDetails
    ),
    oz_test_utils:handle_set_user_privileges(Config, HandleId, U1, revoke, [
        ?HANDLE_VIEW
    ]),
    {ok, U2} = oz_test_utils:handle_add_user(Config, HandleId, U2),
    oz_test_utils:handle_set_user_privileges(Config, HandleId, U2, set, [
        ?HANDLE_VIEW
    ]),
    {ok, G1} = oz_test_utils:handle_add_group(Config, HandleId, G1),

    {HandleId, Groups, Users, {U1, U2, NonAdmin}}.


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
    %%                   Group1 --------
    %%                  /      \
    %%          [group_view]  [~group_view]
    %%               /            \
    %%            User1          User2
    %%
    %%      <<user>>
    %%      NonAdmin

    {
        [{G1, _}, {G2, _}, _, {G4, _}, {G5, _}] = Groups, Users
    } = create_eff_parent_groups_env(Config),

    Spaces = lists:map(
        fun(GroupId) ->
            SpaceDetails = #{<<"name">> => ?UNIQUE_STRING},
            {ok, SpaceId} = oz_test_utils:group_create_space(
                Config, GroupId, SpaceDetails
            ),
            {SpaceId, SpaceDetails#{<<"providers">> => #{}}}
        end, [G1, G2, G4, G5, G5]
    ),

    {Spaces, Groups, Users}.


create_eff_providers_env(Config) ->
    %% Create environment with following relations:
    %%
    %%  Provider3   Provider4
    %%    |          /
    %%    |         /
    %%  Space4   Space5
    %%      \     /
    %%       \   /
    %%      Group5    Space3 ----- Prov2
    %%          \      /             |
    %%           \    /              |
    %%           Group4           Space2
    %%               \            /         Provider1
    %%                \          /           /
    %%               Group3    Group2       /
    %%                  \      /        Space1
    %%                   \    /         /
    %%                   Group1 --------
    %%                  /      \
    %%          [group_view]  [~group_view]
    %%               /           \
    %%            User1          User2
    %%
    %%      <<user>>
    %%      NonAdmin

    {
        [{S1, _}, {S2, _}, {S3, _}, {S4, _}, {S5, _}] = Spaces, Groups, Users
    } = create_eff_spaces_env(Config),

    Providers = [{P1, _}, {P2, _}, {P3, _}, {P4, _}] = lists:map(
        fun(_) ->
            ProviderName = ?UNIQUE_STRING,
            ProvDetails = ?PROVIDER_DETAILS(ProviderName),
            {ok, {ProvId, _}} = oz_test_utils:create_provider(
                Config, ProvDetails#{<<"subdomainDelegation">> => false}
            ),
            {ProvId, maps:remove(<<"adminEmail">>, ProvDetails#{
                <<"name">> => ProviderName,
                <<"online">> => false
            })}
        end, lists:seq(1, 4)
    ),

    lists:foreach(
        fun({ProvId, SpaceId}) ->
            {ok, Macaroon} = oz_test_utils:space_invite_provider_token(
                Config, ?ROOT, SpaceId
            ),
            {ok, Token} = onedata_macaroons:serialize(Macaroon),
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
    %%      [group_view]  [~group_view]
    %%               /           \
    %%            User1          User2
    %%
    %%      <<user>>
    %%      NonAdmin

    {
        [{G1, _}, {G2, _}, _, {G4, _}, {G5, _}] = Groups, Users
    } = create_eff_parent_groups_env(Config),

    HandleServices = lists:map(
        fun(GroupId) ->
            Name = ?UNIQUE_STRING,
            Properties = lists:nth(rand:uniform(2), [
                ?DOI_SERVICE_PROPERTIES, ?PID_SERVICE_PROPERTIES
            ]),
            HServiceDetails = #{
                <<"name">> => Name,
                <<"proxyEndpoint">> => ?PROXY_ENDPOINT,
                <<"serviceProperties">> => Properties
            },
            {ok, HSid} = oz_test_utils:create_handle_service(
                Config, ?ROOT, HServiceDetails
            ),
            {ok, GroupId} = oz_test_utils:handle_service_add_group(
                Config, HSid, GroupId
            ),
            {HSid, HServiceDetails}
        end, [G1, G2, G4, G5, G5]
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
    } = create_eff_parent_groups_env(Config),

    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, G1} = oz_test_utils:handle_service_add_group(Config, HService, G1),

    Handles = lists:map(
        fun(GroupId) ->
            {ok, SpaceId} = oz_test_utils:group_create_space(
                Config, GroupId, ?SPACE_NAME1
            ),
            ShareId = ?UNIQUE_STRING,
            {ok, ShareId} = oz_test_utils:create_share(
                Config, ?ROOT, ShareId, ?SHARE_NAME1, ?ROOT_FILE_ID, SpaceId
            ),
            HandleDetails = ?HANDLE(HService, ShareId),
            {ok, HandleId} = oz_test_utils:create_handle(
                Config, ?ROOT, HandleDetails
            ),
            {ok, GroupId} = oz_test_utils:handle_add_group(
                Config, HandleId, GroupId
            ),
            {HandleId, HandleDetails}
        end, [G1, G2, G3, G4, G5]
    ),

    {Handles, Groups, Users}.
