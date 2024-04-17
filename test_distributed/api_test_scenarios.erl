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
-include("http/rest.hrl").
-include("entity_logic.hrl").
-include_lib("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/errors.hrl").

-export([run_scenario/2]).
-export([delete_entity/5]).
-export([get_relations/4]).
-export([get_privileges/7]).
-export([get_privileges/8]).
-export([get_privileges/9]).
-export([update_privileges/7]).
-export([delete_privileges/7]).

-export([
    collect_unique_tokens_fun/0
]).
-export([
    create_basic_group_env/2,
    create_basic_space_env/2,
    create_basic_doi_hservice_env/2,
    create_basic_handle_env/2,
    create_basic_harvester_env/2,
    create_basic_cluster_env/2,
    create_basic_atm_inventory_env/1
]).
-export([
    create_eff_parent_groups_env/1,
    create_eff_child_groups_env/1,
    create_space_eff_users_env/1,
    create_provider_eff_users_env/1,
    create_hservice_eff_users_env/1,
    create_handle_eff_users_env/1,
    create_harvester_eff_users_env/1,
    create_cluster_eff_users_env/1,
    create_atm_inventory_eff_users_env/1,
    create_eff_spaces_env/1,
    create_eff_providers_env/1,
    create_harvester_eff_providers_env/1,
    create_eff_handle_services_env/1,
    create_eff_handles_env/1,
    create_eff_harvesters_env/1,
    create_eff_atm_inventories_env/1
]).


% Due to asynchronous entity graph recomputation, privileges converge after a while
-define(PRIVILEGES_CHECK_RETRIES, 600).
-define(PRIVILEGES_CHECK_INTERVAL, 100). % Interval * retries = 1 minute

-define(PRIVILEGES_RANDOMIZATION_REPEATS, 25).

-define(compare_privileges(__PrivsA, __PrivsB), begin
    __SortedPrivsA = lists:sort(__PrivsA),
    __SortedPrivsB = lists:sort(__PrivsB),
    ?assertEqual(
        __SortedPrivsA, __SortedPrivsB,
        ?PRIVILEGES_CHECK_RETRIES, ?PRIVILEGES_CHECK_INTERVAL
    )
end).


run_scenario(Function, Args) ->
    try
        erlang:apply(?MODULE, Function, Args),
        true
    catch
        throw:fail ->
            false;
        Type:Message:Stacktrace ->
            ct:pal(
                "Unexpected error in ~p:run_scenario - ~p:~p~nStacktrace: ~s",
                [
                    ?MODULE, Type, Message,
                    lager:pr_stacktrace(Stacktrace)
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
        expected_result_op = ?ERROR_REASON(?ERROR_NOT_FOUND)
    }.


% Set different privileges, run tests and check they are returned.
% Entity whose eff privileges will be changing during those tests
% should not be listed in client spec but provided additionally.
% Exception to this is when entity directly tries to get it's privileges,
% then it should be listed as only correct client and provided as argument
% SubjectUser is the one for whom privileges are being checked.
get_privileges(Config, ApiTestSpec, SetPrivsFun, AllPrivs, ConstPrivs, Entity, ViewPriv) ->
    get_privileges(Config, ApiTestSpec, SetPrivsFun, AllPrivs, ConstPrivs, Entity, ViewPriv, false, undefined).

get_privileges(Config, ApiTestSpec, SetPrivsFun, AllPrivs, ConstPrivs, Entity, ViewPriv, SkipEntity) ->
    get_privileges(Config, ApiTestSpec, SetPrivsFun, AllPrivs, ConstPrivs, Entity, ViewPriv, SkipEntity, undefined).

get_privileges(Config, ApiTestSpec, SetPrivsFun, AllPrivs, ConstPrivs, Entity, ViewPriv, SkipEntity, SubjectUser) ->
    % Run original spec
    assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % In case of getting privileges for Entity (mainly in user api), skip this
    % step because eff privileges of entity are affected during test run and
    % it will be able to sometimes get privileges and sometimes not
    case SkipEntity of
        true ->
            ok;
        _ ->
            run_get_privs_tests(
                Config, ApiTestSpec, SetPrivsFun, AllPrivs, ConstPrivs
            )
    end,

    % Replace clients with entity and check if it can get privileges
    % when view priv is set
    SetPrivsFun(lists:usort(ConstPrivs ++ [ViewPriv]), AllPrivs),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    EntityTestSpec = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{correct = [Entity]}
    },
    run_get_privs_tests(
        Config, EntityTestSpec, SetPrivsFun,
        AllPrivs, lists:usort(ConstPrivs ++ [ViewPriv])
    ),

    % Replace clients with entity and set all privileges but view one.
    SetPrivsFun(AllPrivs -- [ViewPriv], [ViewPriv]),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % If entity is the same as subject user then it should be possible to
    % get privileges even without view privilege. Otherwise entity should not
    % be able to get privileges.
    case Entity of
        {user, SubjectUser} ->
            run_get_privs_tests(
                Config, EntityTestSpec, SetPrivsFun, AllPrivs,
                lists:usort(ConstPrivs ++ [ViewPriv])
            );
        _ ->
            ForbiddenApiTestSpec = create_forbidden_test_spec(ApiTestSpec, Entity),
            assert(api_test_utils:run_tests(Config, ForbiddenApiTestSpec))
    end.


run_get_privs_tests(Config, ApiTestSpec, SetPrivsFun, AllPrivs, ConstPrivs) ->
    lists:foreach(
        fun(PrivsSublist) ->
            Privs = lists:usort(PrivsSublist ++ ConstPrivs),
            EnvSetUpFun = fun() ->
                SetPrivsFun(Privs, AllPrivs -- Privs),
                #{privs => Privs}
            end,
            assert(api_test_utils:run_tests(
                Config, prepare_get_privs_api_spec(ApiTestSpec, Privs),
                EnvSetUpFun, undefined, undefined
            ))
        end, generate_random_sublists_of_privs(AllPrivs)
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
        expected_result_op = ?OK_LIST(Privs)
    }.


% Scenario with entity whose privs will be changing during test run
update_privileges(
    Config, #api_test_spec{
        client_spec = #client_spec{correct = [Entity]}
    } = ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs, Entity, UpdatePriv
) ->
    SetPrivsFun([UpdatePriv], AllPrivs -- [UpdatePriv]),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    % Run tests giving entity always update priv
    run_update_privs_tests(
        Config, ApiTestSpec, SetPrivsFun,
        update_privs_verify_fun(GetPrivsFun),
        AllPrivs, [UpdatePriv]
    ),

    % Run tests with entity without update privilege
    SetPrivsFun([], AllPrivs),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    ForbiddenVerifyEndFun = fun(_, #{privs := InitialPrivs} = _Env, _Data) ->
        ?compare_privileges(InitialPrivs, GetPrivsFun())
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
            PrivsToGrant = maps:get(<<"grant">>, Data, []),
            PrivsToRevoke = maps:get(<<"revoke">>, Data, []),
            ActualPrivs = GetPrivsFun(),
            ?compare_privileges(
                privileges:union(PrivsToGrant, privileges:subtract(InitialPrivs, PrivsToRevoke)),
                ActualPrivs
            );
        (false = _ShouldSucceed, #{privs := InitialPrivs} = _Env, _) ->
            ActualPrivs = GetPrivsFun(),
            ?compare_privileges(InitialPrivs, ActualPrivs)
    end.


run_update_privs_tests(
    Config, ApiTestSpec, SetPrivsFun, VerifyEndFun, AllPrivs, ConstPrivs
) ->
    EnvSetUpFun = fun() ->
        RandPrivs = lists:sublist(AllPrivs, rand:uniform(length(AllPrivs))),
        InitialPrivs = lists:usort(ConstPrivs ++ RandPrivs),
        SetPrivsFun(InitialPrivs, AllPrivs -- InitialPrivs),
        #{privs => InitialPrivs}
    end,
    lists:foreach(fun(PrivsSublist) ->
        {PrivsToGrant, PrivsToRevoke} = case PrivsSublist of
            [] -> {[], []};
            L -> lists:split(rand:uniform(length(L)), L)
        end,
        NewApiTestSpec = ApiTestSpec#api_test_spec{
            data_spec = #data_spec{
                at_least_one = [<<"grant">>, <<"revoke">>],
                correct_values = #{
                    <<"grant">> => [PrivsToGrant],
                    <<"revoke">> => [PrivsToRevoke]
                }
            }
        },
        assert(api_test_utils:run_tests(
            Config, NewApiTestSpec, EnvSetUpFun,
            undefined, VerifyEndFun
        ))
    end, generate_random_sublists_of_privs(AllPrivs)).


generate_random_sublists_of_privs(AllPrivs) ->
    [lists_utils:random_sublist(AllPrivs) || _ <- lists:seq(1, ?PRIVILEGES_RANDOMIZATION_REPEATS)].

% Grant all oz privileges and check that correct clients can delete them but
% forbidden and unauthorized cannot.
% For entity whose effective privileges are affected (and as such it should be
% listed in correct clients and as Entity arg) create separate spec and set all
% privileges but delete one, run tests and check that it cannot delete them
delete_privileges(
    Config, ApiTestSpec, SetPrivsFun, GetPrivsFun, AllPrivs, Entity, DeletePriv
) ->
    EnvSetUpFun = fun() ->
        SetPrivsFun(AllPrivs, []),
        #{privs => AllPrivs}
    end,
    VerifyEndFun = fun(ShouldSucceed, _Env, _Data) ->
        ActualPrivs = GetPrivsFun(),
        case ShouldSucceed of
            true -> ?compare_privileges([], ActualPrivs);
            false -> ?compare_privileges(AllPrivs, ActualPrivs)
        end
    end,
    assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )),

    SetPrivsFun(AllPrivs -- [DeletePriv], [DeletePriv]),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ApiTestSpec2 = create_forbidden_test_spec(ApiTestSpec, Entity),
    EnvSetUpFun2 = fun() ->
        SetPrivsFun(AllPrivs -- [DeletePriv], [DeletePriv]),
        #{privs => AllPrivs -- [DeletePriv]}
    end,
    VerifyEndFun2 = fun(_, #{privs := InitialPrivs} = _Env, _Data) ->
        ActualPrivs = GetPrivsFun(),
        ?compare_privileges(InitialPrivs, ActualPrivs)
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
        expected_result_op = ?ERROR_REASON(?ERROR_FORBIDDEN)
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
    %% Create environment with the following relations:
    %%
    %%                  Group
    %%                 /     \
    %%                /       \
    %%       [~privileges]  [privileges]
    %%              /           \
    %%           User1         User2

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),

    AllGroupPrivs = privileges:group_privileges(),
    {ok, Group} = oz_test_utils:create_group(Config, ?USER(U1), ?GROUP_NAME1),
    oz_test_utils:group_set_user_privileges(
        Config, Group, U1, AllGroupPrivs -- Privs, Privs
    ),
    {ok, U2} = oz_test_utils:group_add_user(Config, Group, U2),
    oz_test_utils:group_set_user_privileges(Config, Group, U2, Privs, AllGroupPrivs -- Privs),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {Group, U1, U2}.


create_basic_space_env(Config, Privs) when not is_list(Privs) ->
    create_basic_space_env(Config, [Privs]);
create_basic_space_env(Config, Privs) ->
    %% Create environment with the following relations:
    %%
    %%                  Space
    %%                 /  |  \
    %%                /   |   \
    %%    [~privileges]   |   [privileges]
    %%              /     |     \
    %%          User1   Owner   User2

    {ok, Owner} = oz_test_utils:create_user(Config),
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    AllSpacePrivs = privileges:space_privileges(),

    % create a space with the Owner, but revoke some (randomly chosen) privileges
    % of the user - regardless of what they are, the owner should effectively have
    % all the privileges, and the tests using this env depend on that assumption
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(Owner), ?SPACE_NAME1),
    RandomPrivs = lists_utils:random_sublist(AllSpacePrivs),
    oz_test_utils:space_set_user_privileges(Config, Space, Owner, RandomPrivs, AllSpacePrivs -- RandomPrivs),

    {ok, U1} = oz_test_utils:space_add_user(Config, Space, U1),
    oz_test_utils:space_set_user_privileges(Config, Space, U1, AllSpacePrivs -- Privs, Privs),

    {ok, U2} = oz_test_utils:space_add_user(Config, Space, U2),
    oz_test_utils:space_set_user_privileges(Config, Space, U2, Privs, AllSpacePrivs -- Privs),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {Space, Owner, U1, U2}.


create_basic_doi_hservice_env(Config, Privs) when not is_list(Privs) ->
    create_basic_doi_hservice_env(Config, [Privs]);
create_basic_doi_hservice_env(Config, Privs) ->
    %% Create environment with the following relations:
    %%
    %%              HandleService
    %%                 /      \
    %%                /        \
    %%       [~privileges]  [privileges]
    %%              /            \
    %%           User1         User2

    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [
        ?OZ_HANDLE_SERVICES_CREATE
    ], []),
    {ok, U2} = oz_test_utils:create_user(Config),

    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?USER(U1), ?DOI_SERVICE
    ),

    AllHServicePrivs = privileges:handle_service_privileges(),
    oz_test_utils:handle_service_set_user_privileges(
        Config, HService, U1, [], Privs
    ),
    {ok, U2} = oz_test_utils:handle_service_add_user(Config, HService, U2),
    oz_test_utils:handle_service_set_user_privileges(
        Config, HService, U2, Privs, AllHServicePrivs -- Privs
    ),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {HService, U1, U2}.


create_basic_handle_env(Config, Privs) when not is_list(Privs) ->
    create_basic_handle_env(Config, [Privs]);
create_basic_handle_env(Config, Privs) ->
    %% Create environment with the following relations:
    %%
    %%                  Handle
    %%                 /      \
    %%                /        \
    %%       [~privileges]  [privileges]
    %%              /            \
    %%           User1         User2

    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [
        ?OZ_HANDLE_SERVICES_CREATE
    ], []),
    {ok, U2} = oz_test_utils:create_user(Config),

    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?USER(U1), ?DOI_SERVICE
    ),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ?SHARE_ID_1, ?SHARE_NAME1, S1
    ),

    AllHandlePrivs = privileges:handle_privileges(),
    HandleId = ozt_users:create_handle_for(U1, HService, ShareId),
    oz_test_utils:handle_set_user_privileges(
        Config, HandleId, U1, [], Privs
    ),
    {ok, U2} = oz_test_utils:handle_add_user(Config, HandleId, U2),
    oz_test_utils:handle_set_user_privileges(
        Config, HandleId, U2, Privs, AllHandlePrivs -- Privs
    ),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {HandleId, U1, U2}.


create_basic_harvester_env(Config, Privs) when not is_list(Privs) ->
    create_basic_harvester_env(Config, [Privs]);
create_basic_harvester_env(Config, Privs) ->
    %% Create environment with the following relations:
    %%
    %%                Harvester
    %%                 /     \
    %%                /       \
    %%       [~privileges]  [privileges]
    %%              /           \
    %%           User1         User2

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),

    AllHarvesterPrivs = privileges:harvester_privileges(),
    {ok, Harvester} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),
    {ok, _} = oz_test_utils:harvester_add_user(Config, Harvester, U1),

    oz_test_utils:harvester_set_user_privileges(
        Config, Harvester, U1, [], Privs
    ),
    {ok, U2} = oz_test_utils:harvester_add_user(Config, Harvester, U2),
    oz_test_utils:harvester_set_user_privileges(Config, Harvester, U2, Privs, AllHarvesterPrivs -- Privs),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {Harvester, U1, U2}.


create_basic_cluster_env(Config, Privs) when not is_list(Privs) ->
    create_basic_cluster_env(Config, [Privs]);
create_basic_cluster_env(Config, Privs) ->
    %% Create environment with the following relations:
    %%
    %%                  Cluster
    %%                 /     \
    %%                /       \
    %%       [~privileges]  [privileges]
    %%              /           \
    %%           User1         User2

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),

    AllClusterPrivs = privileges:cluster_privileges(),

    {ok, {ProviderId, ProviderToken}} = oz_test_utils:create_provider(
        Config, U1, ?PROVIDER_NAME1
    ),
    ClusterId = ProviderId,

    oz_test_utils:cluster_set_user_privileges(
        Config, ClusterId, U1, [], Privs
    ),
    {ok, U2} = oz_test_utils:cluster_add_user(Config, ClusterId, U2),
    oz_test_utils:cluster_set_user_privileges(Config, ClusterId, U2, Privs, AllClusterPrivs -- Privs),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ClusterId, U1, U2, {ProviderId, ProviderToken}}.


create_basic_atm_inventory_env(Privs) when not is_list(Privs) ->
    create_basic_atm_inventory_env([Privs]);
create_basic_atm_inventory_env(Privs) ->
    %% Create environment with the following relations:
    %%
    %%                  AtmInventory
    %%                    /     \
    %%                   /       \
    %%          [~privileges]  [privileges]
    %%                 /           \
    %% UserWithoutPrivileges  UserWithPrivileges

    UserWithoutPrivileges = ozt_users:create(),
    UserWithPrivileges = ozt_users:create(),

    AllAtmInventoryPrivs = privileges:atm_inventory_privileges(),
    AtmInventory = ozt_atm_inventories:create(),
    ozt_atm_inventories:add_user(AtmInventory, UserWithoutPrivileges, AllAtmInventoryPrivs -- Privs),
    ozt_atm_inventories:add_user(AtmInventory, UserWithPrivileges, Privs),
    ozt:reconcile_entity_graph(),

    {AtmInventory, UserWithoutPrivileges, UserWithPrivileges}.


create_eff_parent_groups_env(Config) ->
    %% Create environment with the following relations:
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

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllGroupPrivs = privileges:group_privileges(),
    {ok, U1} = oz_test_utils:group_add_user(Config, G1, U1),
    oz_test_utils:group_set_user_privileges(Config, G1, U1,
        [?GROUP_VIEW], AllGroupPrivs -- [?GROUP_VIEW]
    ),
    {ok, U2} = oz_test_utils:group_add_user(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2,
        AllGroupPrivs -- [?GROUP_VIEW], [?GROUP_VIEW]
    ),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {Groups, {U1, U2, NonAdmin}}.


create_eff_child_groups_env(Config) ->
    %% Create environment with the following relations:
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
            Username = ?UNIQUE_STRING,
            FullName = ?UNIQUE_STRING,
            UserDetails = #{
                <<"username">> => Username,
                <<"fullName">> => FullName
            },
            {ok, UserId} = oz_test_utils:create_user(Config, #{
                <<"fullName">> => FullName, <<"username">> => Username
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

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {Groups, Users}.


create_space_eff_users_env(Config) ->
    %% Create environment with the following relations:
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

    {ok, Owner} = oz_test_utils:create_user(Config),
    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllSpacePrivs = privileges:space_privileges(),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(Owner), ?SPACE_NAME1),
    {ok, U1} = oz_test_utils:space_add_user(Config, S1, U1),
    oz_test_utils:space_set_user_privileges(Config, S1, U1, [],
        [?SPACE_VIEW]
    ),
    {ok, U2} = oz_test_utils:space_add_user(Config, S1, U2),
    oz_test_utils:space_set_user_privileges(Config, S1, U2,
        [?SPACE_VIEW], AllSpacePrivs -- [?SPACE_VIEW]
    ),
    {ok, G1} = oz_test_utils:space_add_group(Config, S1, G1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {S1, Groups, Users, {Owner, U1, U2, NonAdmin}}.


create_provider_eff_users_env(Config) ->
    %% Create environment with the following relations:
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
        S1, Groups, Users, {Owner, U1, U2, NonAdmin}
    } = create_space_eff_users_env(Config),

    {ok, {P1, ProviderToken}} = oz_test_utils:create_provider(
        Config, ?PROVIDER_NAME2
    ),
    {ok, S1} = oz_test_utils:support_space_by_provider(Config, P1, S1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {{P1, ProviderToken}, S1, Groups, Users, {Owner, U1, U2, NonAdmin}}.


create_hservice_eff_users_env(Config) ->
    %% Create environment with the following relations:
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

    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [
        ?OZ_HANDLE_SERVICES_CREATE
    ], []),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllHServicePrivileges = privileges:handle_service_privileges(),
    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?USER(U1), ?DOI_SERVICE
    ),
    oz_test_utils:handle_service_set_user_privileges(Config, HService, U1,
        [], [?HANDLE_SERVICE_VIEW]
    ),
    {ok, U2} = oz_test_utils:handle_service_add_user(Config, HService, U2),
    oz_test_utils:handle_service_set_user_privileges(Config, HService, U2,
        [?HANDLE_SERVICE_VIEW], AllHServicePrivileges -- [?HANDLE_SERVICE_VIEW]
    ),
    {ok, G1} = oz_test_utils:handle_service_add_group(Config, HService, G1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {HService, Groups, Users, {U1, U2, NonAdmin}}.


create_handle_eff_users_env(Config) ->
    %% Create environment with the following relations:
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

    {ok, NonAdmin} = oz_test_utils:create_user(Config),
    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [
        ?OZ_HANDLE_SERVICES_CREATE
    ], []),
    {ok, U2} = oz_test_utils:create_user(Config),

    {ok, HService} = oz_test_utils:create_handle_service(
        Config, ?USER(U1), ?DOI_SERVICE
    ),
    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ?SHARE_ID_1, ?SHARE_NAME1, S1
    ),

    AllHandlePrivs = privileges:handle_privileges(),
    HandleId = ozt_users:create_handle_for(U1, HService, ShareId),
    oz_test_utils:handle_set_user_privileges(Config, HandleId, U1, [],
        [?HANDLE_VIEW]
    ),
    {ok, U2} = oz_test_utils:handle_add_user(Config, HandleId, U2),
    oz_test_utils:handle_set_user_privileges(Config, HandleId, U2,
        [?HANDLE_VIEW], AllHandlePrivs -- [?HANDLE_VIEW]
    ),
    {ok, G1} = oz_test_utils:handle_add_group(Config, HandleId, G1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {HandleId, Groups, Users, {U1, U2, NonAdmin}}.


create_harvester_eff_users_env(Config) ->
    %% Create environment with the following relations:
    %%
    %%                Harvester
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

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllHarvesterPrivs = privileges:harvester_privileges(),
    {ok, H1} = oz_test_utils:create_harvester(Config, ?ROOT, ?HARVESTER_CREATE_DATA),
    {ok, _} = oz_test_utils:harvester_add_user(Config, H1, U1),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U1, [],
        [?HARVESTER_VIEW]
    ),
    {ok, U2} = oz_test_utils:harvester_add_user(Config, H1, U2),
    oz_test_utils:harvester_set_user_privileges(Config, H1, U2,
        [?HARVESTER_VIEW], AllHarvesterPrivs -- [?HARVESTER_VIEW]
    ),
    {ok, G1} = oz_test_utils:harvester_add_group(Config, H1, G1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {H1, Groups, Users, {U1, U2, NonAdmin}}.


create_cluster_eff_users_env(Config) ->
    %% Create environment with the following relations:
    %%
    %%                  Cluster
    %%                 /  |  \
    %%                /   |   \
    %%     [~cluster_view]  |  [cluster_view]
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

    {ok, U1} = oz_test_utils:create_user(Config),
    {ok, U2} = oz_test_utils:create_user(Config),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    AllClusterPrivs = privileges:cluster_privileges(),


    {ok, {ProviderId, ProviderToken}} = oz_test_utils:create_provider(
        Config, U1, ?PROVIDER_NAME1
    ),
    ClusterId = ProviderId,

    oz_test_utils:cluster_set_user_privileges(Config, ClusterId, U1, [],
        [?CLUSTER_VIEW]
    ),
    {ok, U2} = oz_test_utils:cluster_add_user(Config, ClusterId, U2),
    oz_test_utils:cluster_set_user_privileges(Config, ClusterId, U2,
        [?CLUSTER_VIEW], AllClusterPrivs -- [?CLUSTER_VIEW]
    ),
    {ok, G1} = oz_test_utils:cluster_add_group(Config, ClusterId, G1),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ClusterId, Groups, Users, {U1, U2, NonAdmin}, {ProviderId, ProviderToken}}.


create_atm_inventory_eff_users_env(Config) ->
    %% Create environment with the following relations:
    %%
    %%                AtmInventory
    %%                 /  |  \
    %%                /   |   \
    %%   [~atm_inv_view]  |  [atm_inv_view]
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
        [{FirstGroup, _} | _] = Groups, Users
    } = create_eff_child_groups_env(Config),

    UserWithoutView = ozt_users:create(),
    UserWithView = ozt_users:create(),
    NonAdmin = ozt_users:create(),

    AllAtmInventoryPrivs = privileges:atm_inventory_privileges(),
    AtmInventory = ozt_atm_inventories:create(),
    ozt_atm_inventories:add_user(AtmInventory, UserWithoutView, AllAtmInventoryPrivs -- [?ATM_INVENTORY_VIEW]),
    ozt_atm_inventories:add_user(AtmInventory, UserWithView, [?ATM_INVENTORY_VIEW]),

    ozt_atm_inventories:add_group(AtmInventory, FirstGroup),

    ozt:reconcile_entity_graph(),

    {AtmInventory, Groups, Users, {UserWithoutView, UserWithView, NonAdmin}}.


create_eff_spaces_env(Config) ->
    %% Create environment with the following relations:
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

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {Spaces, Groups, Users}.


create_eff_providers_env(Config) ->
    %% Create environment with the following relations:
    %%
    %%  Provider3   Provider4
    %%    |          /
    %%    |         /
    %%  Space4   Space5
    %%      \     /
    %%       \   /
    %%      Group5    Space3 ----- Provider2
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
        [{S1, _}, {S2, _}, {S3, _}, {S4, _}, {S5, _}] = Spaces,
        Groups,
        {U1, _, _} = Users
    } = create_eff_spaces_env(Config),

    Providers = [{P1, _}, {P2, _}, {P3, _}, {P4, _}] = lists:map(
        fun(_) ->
            ProviderData = ?PROVIDER_DETAILS(?UNIQUE_STRING),
            {ok, {ProvId, _}} = oz_test_utils:create_provider(Config, U1, ProviderData),
            {ProvId, maps:remove(<<"adminEmail">>, ProviderData#{
                <<"online">> => false
            })}
        end, lists:seq(1, 4)
    ),

    lists:foreach(
        fun({ProvId, SpaceId}) ->
            {ok, SpaceId} = oz_test_utils:support_space_by_provider(Config, ProvId, SpaceId)
        end, [{P1, S1}, {P2, S2}, {P2, S3}, {P3, S4}, {P4, S5}]
    ),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {Providers, Spaces, Groups, Users}.


create_harvester_eff_providers_env(Config) ->
    %% Create environment with the following relations:
    %%
    %%  Provider1   Provider2  Provider3
    %%          \    /    \    /
    %%           \  /      \  /
    %%           Space1   Space2
    %%               \    /
    %%                \  /
    %%             Harvester1
    %%                 |
    %%               User1
    %%      <<user>>
    %%      NonAdmin

    {ok, U1} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, U1, [?OZ_HARVESTERS_CREATE], []),
    {ok, NonAdmin} = oz_test_utils:create_user(Config),

    Providers = [{P1, _}, {P2, _}, {P3, _}] = lists:map(fun(_) ->

        ProviderData = ?PROVIDER_DETAILS(?UNIQUE_STRING),
        {ok, {ProvId, PToken}} = oz_test_utils:create_provider(Config, U1, ProviderData),
        {ProvId, maps:remove(<<"adminEmail">>, ProviderData#{
            <<"online">> => false,
            <<"providerRootToken">> => PToken
        })}
    end, lists:seq(1, 3)),

    {ok, S1} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),
    {ok, S2} = oz_test_utils:create_space(Config, ?ROOT, ?SPACE_NAME1),

    oz_test_utils:support_space_by_provider(Config, P1, S1),
    oz_test_utils:support_space_by_provider(Config, P2, S1),
    oz_test_utils:support_space_by_provider(Config, P2, S2),
    oz_test_utils:support_space_by_provider(Config, P3, S2),

    {ok, H1} = oz_test_utils:create_harvester(Config, ?USER(U1), ?HARVESTER_CREATE_DATA),

    oz_test_utils:harvester_add_space(Config, H1, S1),
    oz_test_utils:harvester_add_space(Config, H1, S2),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {H1, Providers, [S1, S2], {U1, NonAdmin}}.


create_eff_handle_services_env(Config) ->
    %% Create environment with the following relations:
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

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {HandleServices, Groups, Users}.


create_eff_handles_env(Config) ->
    %% Create environment with the following relations:
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
                Config, ?ROOT, ShareId, ?SHARE_NAME1, SpaceId
            ),
            MetadataPrefix = ?RAND_ELEMENT(ozt_handles:supported_metadata_prefixes()),
            RawMetadata = ozt_handles:example_input_metadata(MetadataPrefix, ?RAND_INT(1, 10)),
            HandleData = #{
                <<"handleServiceId">> => HService,
                <<"resourceType">> => <<"Share">>,
                <<"resourceId">> => ShareId,
                <<"metadataPrefix">> => MetadataPrefix,
                <<"metadata">> => RawMetadata
            },
            {ok, HandleId} = oz_test_utils:create_handle(
                Config, ?ROOT, HandleData
            ),
            {ok, GroupId} = oz_test_utils:handle_add_group(
                Config, HandleId, GroupId
            ),
            {HandleId, HandleData}
        end, [G1, G2, G3, G4, G5]
    ),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {Handles, Groups, Users}.


create_eff_harvesters_env(Config) ->
    %% Create environment with the following relations:
    %%
    %%  Harvester4   Harvester5
    %%          \     /
    %%           \   /
    %%          Group5    Harvester3
    %%              \      /
    %%               \    /
    %%               Group4        Harvester2
    %%                   \            /
    %%                    \          /
    %%                   Group3    Group2
    %%                      \      /      Harvester1
    %%                       \    /         /
    %%                       Group1 --------
    %%                      /      \
    %%              [group_view]  [~group_view]
    %%                   /            \
    %%                User1          User2
    %%
    %%          <<user>>
    %%          NonAdmin

    {
        [{G1, _}, {G2, _}, _, {G4, _}, {G5, _}] = Groups, Users
    } = create_eff_parent_groups_env(Config),

    Harvesters = lists:map(
        fun(GroupId) ->
            Name = ?UNIQUE_STRING,
            {ok, HarvesterId} = oz_test_utils:group_create_harvester(
                Config, GroupId, ?HARVESTER_CREATE_DATA(Name)
            ),
            {HarvesterId, ?HARVESTER_CREATE_DATA(Name)}
        end, [G1, G2, G4, G5, G5]
    ),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {Harvesters, Groups, Users}.


create_eff_atm_inventories_env(Config) ->
    %% Create environment with the following relations:
    %%
    %%  AtmInventory4 AtmInventory5
    %%          \     /
    %%           \   /
    %%          Group5   AtmInventory3
    %%              \      /
    %%               \    /
    %%               Group4        AtmInventory2
    %%                   \            /
    %%                    \          /
    %%                   Group3    Group2
    %%                      \      /      AtmInventory1
    %%                       \    /         /
    %%                       Group1 --------
    %%                      /      \
    %%              [group_view]  [~group_view]
    %%                   /            \
    %%                User1          User2
    %%
    %%          <<user>>
    %%          NonAdmin

    {
        [{G1, _}, {G2, _}, _, {G4, _}, {G5, _}] = Groups, Users
    } = create_eff_parent_groups_env(Config),

    AtmInventories = lists:map(
        fun(GroupId) ->
            AtmInventoryData = #{<<"name">> => ?UNIQUE_STRING},
            AtmInventoryId = ozt_groups:create_atm_inventory_for(GroupId, AtmInventoryData),
            {AtmInventoryId, AtmInventoryData}
        end, [G1, G2, G4, G5, G5]
    ),

    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {AtmInventories, Groups, Users}.
