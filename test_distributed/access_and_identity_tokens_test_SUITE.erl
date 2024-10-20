%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This suite contains tests for access tokens, which check the behaviour of
%%% the system when access tokens are used to access arbitrarily chosen API
%%% operations. Each test is repeated several times, and each time the request
%%% context is randomized. Depending on the request context, different
%%% combinations of subjects, services, consumers, token caveats and interfaces 
%%% are randomly generated and tested if they return the expected results.
%%% @end
%%%-------------------------------------------------------------------
-module(access_and_identity_tokens_test_SUITE).
-author("Lukasz Opiola").

-include("ozt.hrl").
-include("api_test_utils.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

% Number of repeats of each test - each time, request context is randomized
-define(TEST_REPEATS, 3).
-define(CAVEAT_RANDOMIZATION_REPEATS, 3).

% Mocked connection and geo db data
-define(PEER_IP, {81, 213, 5, 17}).
-define(CORRECT_MASK_1, {{81, 213, 5, 0}, 24}).
-define(CORRECT_MASK_2, {{81, 0, 0, 0}, 8}).
-define(INCORRECT_MASK_1, {{1, 2, 3, 4}, 19}).
-define(INCORRECT_MASK_2, {{100, 78, 9, 0}, 22}).

% Examples of IP's GEO data - pairs of country code and regions
-define(GEO_EXAMPLES, [
    {<<"EG">>, [<<"Africa">>]},
    {<<"AQ">>, [<<"Antarctica">>]},
    {<<"CH">>, [<<"Asia">>]},
    {<<"PL">>, [<<"Europe">>, <<"EU">>]},
    {<<"MD">>, [<<"Europe">>]},
    {<<"US">>, [<<"NorthAmerica">>]},
    {<<"AU">>, [<<"Oceania">>]},
    {<<"BR">>, [<<"SouthAmerica">>]}
]).

-record(request_spec, {
    % List of subjects (clients) that are eligible to perform the operation.
    eligible_subjects :: [aai:subject()],
    operation :: entity_logic:operation(),
    gri :: gri:gri(),
    % Service caveats implicitly limit the allowed API - this list contains the list
    % of all services that are allowed to perform such operation in case service
    % caveat is present.
    services_allowed_to_call_this_api :: [aai:service()],
    % Specifies if such operation is available if the auth token contains a data_access_caveat
    available_with_data_access_caveats :: boolean(),
    % Args should typically start with 'auth' atom, which is substituted for client's auth
    logic_call_args :: {Module :: atom(), Function :: atom(), Args :: [term()]},
    rest_call_args :: {http_client:method(), ozt_http:urn_tokens(), entity_logic:data()},
    additional_graph_sync_args :: {gs_protocol:auth_hint(), entity_logic:data()}
}).

-record(request_context, {
    subject :: aai:subject(),
    token_type :: access_token | identity_token,
    current_timestamp :: time:seconds(),
    interface :: undefined | cv_interface:interface(),
    ip :: undefined | ip_utils:ip(),
    asn :: undefined | ip_utils:asn(),
    country :: undefined | ip_utils:country_code(),
    regions :: undefined | [ip_utils:region()],
    service :: aai:service_spec(),
    service_token = autogenerate :: autogenerate | tokens:token(),
    consumer :: aai:consumer_spec(),
    consumer_token = autogenerate :: autogenerate | tokens:token(),
    data_access_caveats_policy :: data_access_caveats:policy()
}).

%% Test API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    create_user/1,
    get_user/1,
    get_user_as_provider/1,
    get_group/1,
    get_group_as_provider/1,
    get_space/1,
    get_space_as_provider/1,
    update_user_privileges_in_cluster/1,
    delete_user_admin_privileges/1
]).

all() -> ?ALL([
    create_user,
    get_user,
    get_user_as_provider,
    get_group,
    get_group_as_provider,
    get_space,
    get_space_as_provider,
    update_user_privileges_in_cluster,
    delete_user_admin_privileges
]).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    application:ensure_all_started(hackney),
    ozt:init_per_suite(Config).

init_per_testcase(_, Config) ->
    ozt_mocks:freeze_time(),
    ozt_mocks:mock_gui_static(),
    ozt_mocks:mock_peer_ip_of_all_connections(?PEER_IP),
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unfreeze_time(),
    ozt_mocks:unmock_gui_static(),
    ozt_mocks:unmock_peer_ip_of_all_connections(),
    ok.

end_per_suite(_Config) ->
    application:stop(hackney),
    ssl:stop().

%%%===================================================================
%%% Test functions
%%%===================================================================

create_user(_Config) ->
    AdminUserId = ozt_users:create_admin([?OZ_USERS_CREATE]),
    ?assert(run_tests(#request_spec{
        eligible_subjects = [?SUB(user, AdminUserId)],
        operation = create,
        gri = ?GRI(od_user, undefined, instance, private),
        services_allowed_to_call_this_api = [?OZ_WORKER],
        available_with_data_access_caveats = false,
        logic_call_args = {user_logic, create, [auth]},
        rest_call_args = {post, <<"/users">>, #{}},
        additional_graph_sync_args = {undefined, #{}}
    })).


get_user(_Config) ->
    UserId = ozt_users:create(),
    ?assert(run_tests(#request_spec{
        eligible_subjects = [?SUB(user, UserId)],
        operation = get,
        gri = ?GRI(od_user, UserId, instance, protected),
        services_allowed_to_call_this_api = [?OZ_WORKER, ?OZ_PANEL, ?OP_WORKER, ?OP_PANEL],
        available_with_data_access_caveats = true,
        logic_call_args = {user_logic, get_protected_data, [auth, UserId]},
        rest_call_args = {get, [<<"/users/">>, UserId], #{}},
        additional_graph_sync_args = {undefined, #{}}
    })).


get_user_as_provider(_Config) ->
    UserId = ozt_users:create(),
    ProviderId = ozt_providers:create_as_support_for_user(UserId),
    ?assert(run_tests(#request_spec{
        eligible_subjects = [?SUB(?ONEPROVIDER, ProviderId)],
        operation = get,
        gri = ?GRI(od_user, UserId, instance, protected),
        services_allowed_to_call_this_api = [?OZ_WORKER, ?OP_WORKER, ?OP_PANEL],
        available_with_data_access_caveats = true,
        logic_call_args = {provider_logic, get_eff_user, [auth, ProviderId, UserId]},
        rest_call_args = {get, [<<"/providers/">>, ProviderId, <<"/effective_users/">>, UserId], #{}},
        additional_graph_sync_args = {?THROUGH_PROVIDER(ProviderId), #{}}
    })).


get_group(_Config) ->
    UserId = ozt_users:create(),
    GroupId = ozt_users:create_group_for(UserId),
    ?assert(run_tests(#request_spec{
        eligible_subjects = [?SUB(user, UserId)],
        operation = get,
        gri = ?GRI(od_group, GroupId, instance, protected),
        services_allowed_to_call_this_api = [?OZ_WORKER, ?OP_WORKER],
        available_with_data_access_caveats = false,
        logic_call_args = {group_logic, get_protected_data, [auth, GroupId]},
        rest_call_args = {get, [<<"/groups/">>, GroupId], #{}},
        additional_graph_sync_args = {undefined, #{}}
    })).


get_group_as_provider(_Config) ->
    UserId = ozt_users:create(),
    GroupId = ozt_users:create_group_for(UserId),
    SpaceOwner = ozt_users:create(),
    SpaceId = ozt_users:create_space_for(SpaceOwner),
    ozt_spaces:add_group(SpaceId, GroupId),
    ProviderId = ozt_providers:create(),
    ozt_providers:support_space(ProviderId, SpaceId),
    ?assert(run_tests(#request_spec{
        eligible_subjects = [?SUB(?ONEPROVIDER, ProviderId)],
        operation = get,
        gri = ?GRI(od_group, GroupId, instance, protected),
        services_allowed_to_call_this_api = [?OZ_WORKER, ?OP_WORKER],
        available_with_data_access_caveats = false,
        logic_call_args = {provider_logic, get_eff_group, [auth, ProviderId, GroupId]},
        rest_call_args = {get, [<<"/providers/">>, ProviderId, <<"/effective_groups/">>, GroupId], #{}},
        additional_graph_sync_args = {?THROUGH_PROVIDER(ProviderId), #{}}
    })).


get_space(_Config) ->
    UserId = ozt_users:create(),
    SpaceId = ozt_users:create_space_for(UserId),
    ProviderId = ozt_providers:create(),
    ozt_providers:support_space(ProviderId, SpaceId),
    ?assert(run_tests(#request_spec{
        eligible_subjects = [?SUB(user, UserId)],
        operation = get,
        gri = ?GRI(od_space, SpaceId, instance, protected),
        services_allowed_to_call_this_api = [?OZ_WORKER, ?OP_WORKER],
        available_with_data_access_caveats = true,
        logic_call_args = {space_logic, get_protected_data, [auth, SpaceId]},
        rest_call_args = {get, [<<"/spaces/">>, SpaceId], #{}},
        additional_graph_sync_args = {undefined, #{}}
    })).


get_space_as_provider(_Config) ->
    SpaceId = ozt_spaces:create(),
    ProviderId = ozt_providers:create_as_support_for_space(SpaceId),
    ?assert(run_tests(#request_spec{
        eligible_subjects = [?SUB(?ONEPROVIDER, ProviderId)],
        operation = get,
        gri = ?GRI(od_space, SpaceId, instance, protected),
        services_allowed_to_call_this_api = [?OZ_WORKER, ?OP_WORKER],
        available_with_data_access_caveats = true,
        logic_call_args = {provider_logic, get_eff_space, [auth, ProviderId, SpaceId]},
        rest_call_args = {get, [<<"/providers/">>, ProviderId, <<"/spaces/">>, SpaceId], #{}},
        additional_graph_sync_args = {?THROUGH_PROVIDER(ProviderId), #{}}
    })).


update_user_privileges_in_cluster(_Config) ->
    ClusterAdmin = ozt_users:create(),
    ProviderId = ozt_providers:create_for_admin_user(ClusterAdmin),
    ClusterId = ProviderId,
    ClusterMember = ozt_users:create(),
    ozt_clusters:add_user(ClusterId, ClusterMember),
    Data = #{<<"grant">> => lists_utils:random_sublist(privileges:cluster_admin())},
    ?assert(run_tests(#request_spec{
        eligible_subjects = [?SUB(user, ClusterAdmin), ?SUB(?ONEPROVIDER, ProviderId)],
        operation = update,
        gri = ?GRI(od_cluster, ClusterId, {user_privileges, ClusterMember}, private),
        services_allowed_to_call_this_api = [?OZ_WORKER],
        available_with_data_access_caveats = false,
        logic_call_args = {cluster_logic, update_user_privileges, [auth, ClusterId, ClusterMember, Data]},
        rest_call_args = {patch, [<<"/clusters/">>, ClusterId, <<"/users/">>, ClusterMember, <<"/privileges">>], Data},
        additional_graph_sync_args = {undefined, Data}
    })).


delete_user_admin_privileges(_Config) ->
    AdminUser = ozt_users:create_admin([?OZ_SET_PRIVILEGES]),
    TargetUser = ozt_users:create(),
    ?assert(run_tests(#request_spec{
        eligible_subjects = [?SUB(user, AdminUser)],
        operation = delete,
        gri = ?GRI(od_user, TargetUser, oz_privileges, private),
        services_allowed_to_call_this_api = [?OZ_WORKER],
        available_with_data_access_caveats = false,
        logic_call_args = {user_logic, delete_oz_privileges, [auth, TargetUser]},
        rest_call_args = {delete, [<<"/users/">>, TargetUser, <<"/privileges">>], #{}},
        additional_graph_sync_args = {undefined, #{}}
    })).

%%%===================================================================
%%% Testing framework
%%%===================================================================

run_tests(RequestSpec) ->
    lists:all(fun(Num) -> run_test_repeat(RequestSpec, Num) end, lists:seq(1, ?TEST_REPEATS)).

run_test_repeat(RequestSpec, RepeatNum) ->
    try
        check_bad_token_scenarios(RequestSpec),
        check_forbidden_service_when_creating_scenarios(RequestSpec),
        check_forbidden_service_when_consuming_scenarios(RequestSpec),
        check_unauthorized_scenarios(RequestSpec),
        check_authorized_scenarios(RequestSpec),
        check_user_blocking(RequestSpec),
        check_temporary_token_revocation(RequestSpec),
        check_named_token_revocation(RequestSpec),
        check_token_caveats_handling(RequestSpec),
        check_service_token_caveats_handling(RequestSpec),
        check_consumer_token_caveats_handling(RequestSpec),
        % This must be run with last repeat as it deletes the eligible subjects
        RepeatNum == ?TEST_REPEATS andalso check_subject_deleted_scenarios(RequestSpec),
        true
    catch Type:Reason:Stacktrace ->
        ct:pal("Access tokens test failed due to ~tp:~tp~nStacktrace: ~ts", [
            Type, Reason, lager:pr_stacktrace(Stacktrace)
        ]),
        false
    end.


check_bad_token_scenarios(RequestSpec) ->
    DummyProvider = ozt_providers:create(),
    DummyUser = ozt_users:create(),
    SessId = ozt_http:simulate_login(DummyUser),

    BadToken = <<"this-is-a-bad-token-that-cannot-be-decoded">>,
    ?assertEqual(
        ?ERROR_UNAUTHORIZED(?ERROR_BAD_TOKEN),
        make_request_with_random_context(RequestSpec, ?SUB(user, DummyUser), {token, BadToken})
    ),

    InviteToken = ozt_tokens:create(
        named, ?SUB(?ONEPROVIDER, DummyProvider),
        ?INVITE_TOKEN(?USER_JOIN_CLUSTER, DummyProvider)
    ),
    ?assertEqual(
        ?ERROR_UNAUTHORIZED(?ERROR_NOT_AN_ACCESS_TOKEN(?INVITE_TOKEN(?USER_JOIN_CLUSTER, DummyProvider))),
        make_request_with_random_context(RequestSpec, ?SUB(user, DummyUser), {token, InviteToken})
    ),

    GuiAccessToken = ozt_tokens:create_access_token_for_gui(DummyUser, SessId, ?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)),
    Error = ?assertMatch(
        {error, _},
        make_request_with_random_context(RequestSpec, ?SUB(user, DummyUser), {token, GuiAccessToken})
    ),
    % The error depends on the request context, which is randomized with every request
    ?assert(lists:member(Error, [
        ?ERROR_FORBIDDEN,
        ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SESSION_INVALID),
        ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_service{whitelist = [?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]}))
    ])),

    lists:foreach(fun(EligibleSubject) ->
        Prototype = #token{
            onezone_domain = ozt:get_domain(),
            id = <<"123123123123">>,
            subject = EligibleSubject,
            type = ?ACCESS_TOKEN,
            persistence = named
        },

        ForgedTokenAlpha = tokens:construct(Prototype, <<"secret">>, []),
        ?assertEqual(
            ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_INVALID),
            make_request_with_random_context(RequestSpec, EligibleSubject, {token, ForgedTokenAlpha})
        ),

        {ok, CurrentGeneration} = case EligibleSubject of
            ?SUB(user, UserId) -> ozt:rpc(token_logic, get_user_temporary_token_generation, [?ROOT, UserId]);
            ?SUB(?ONEPROVIDER, PrId) -> ozt:rpc(token_logic, get_provider_temporary_token_generation, [?ROOT, PrId])
        end,

        % If the token generation is older than actual subject's token generation,
        % it is considered revoked. This error might be confusing in regard to forged
        % tokens, but useful for subjects with good intentions.
        ForgedTokenBeta = tokens:construct(Prototype#token{persistence = {temporary, CurrentGeneration - 1}}, <<"secret">>, []),
        ?assertEqual(
            ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_REVOKED),
            make_request_with_random_context(RequestSpec, EligibleSubject, {token, ForgedTokenBeta})
        ),

        % If the generation is the same, forged token is considered invalid.
        ForgedTokenGamma = tokens:construct(Prototype#token{persistence = {temporary, CurrentGeneration}}, <<"secret">>, []),
        ?assertEqual(
            ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_INVALID),
            make_request_with_random_context(RequestSpec, EligibleSubject, {token, ForgedTokenGamma})
        ),

        % Future generation implies that the token must have been forged.
        ForgedTokenDelta = tokens:construct(Prototype#token{persistence = {temporary, 857346}}, <<"secret">>, []),
        ?assertEqual(
            ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_INVALID),
            make_request_with_random_context(RequestSpec, EligibleSubject, {token, ForgedTokenDelta})
        )
    end, RequestSpec#request_spec.eligible_subjects).


check_forbidden_service_when_creating_scenarios(RequestSpec) ->
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            lists:foreach(fun(ForbiddenService) ->
                ?assertEqual(
                    ?ERROR_TOKEN_SERVICE_FORBIDDEN(ForbiddenService),
                    ozt_tokens:try_create(Persistence, EligibleSubject, #{
                        <<"type">> => ?ACCESS_TOKEN,
                        <<"caveats">> => [#cv_service{whitelist = [ForbiddenService]}]
                    })
                ),
                Token = ozt_tokens:create(Persistence, EligibleSubject),
                ?assertEqual(
                    ?ERROR_TOKEN_SERVICE_FORBIDDEN(ForbiddenService),
                    ozt_tokens:confine(Token, [#cv_service{whitelist = [ForbiddenService]}])
                )
            end, gen_forbidden_service_specs(EligibleSubject))
        end, RequestSpec#request_spec.eligible_subjects)
    end, [named, temporary]).


check_forbidden_service_when_consuming_scenarios(RequestSpec) ->
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            RequestContext = randomize_request_context(EligibleSubject),
            lists:foreach(fun(ForbiddenService) ->
                ClientAuth = gen_client_auth(EligibleSubject, Persistence),
                RequestContextWithService = RequestContext#request_context{
                    service = ForbiddenService
                },
                ?assertMatch(
                    ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SERVICE_FORBIDDEN(ForbiddenService)),
                    make_request(RequestSpec, RequestContextWithService, ClientAuth)
                )
            end, gen_forbidden_service_specs(EligibleSubject))
        end, RequestSpec#request_spec.eligible_subjects)
    end, [named, temporary]).


check_unauthorized_scenarios(RequestSpec) ->
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(SubjectType) ->
            % Each operation requires certain privileges or memberships, so
            % freshly created clients should not be authorized.
            UnauthorizedSubject = create_subject_by_type(SubjectType),
            ExpectedError = case SubjectType of
                nobody -> ?ERROR_UNAUTHORIZED;
                _ -> ?ERROR_FORBIDDEN
            end,
            ClientAuth = gen_client_auth(UnauthorizedSubject, Persistence),
            ?assertMatch(ExpectedError, make_request_with_random_context(
                RequestSpec, UnauthorizedSubject, ClientAuth
            ))
        end, all_subject_types())
    end, [named, temporary]).


check_authorized_scenarios(RequestSpec) ->
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            ClientAuth = gen_client_auth(EligibleSubject, Persistence),
            ?assertMatch(ok, make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuth))
        end, RequestSpec#request_spec.eligible_subjects)
    end, [named, temporary]).


check_user_blocking(RequestSpec) ->
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            case EligibleSubject of
                ?SUB(user, UserId) ->
                    ClientAuth = gen_client_auth(EligibleSubject, Persistence),
                    % context must be randomized before blocking access as it
                    % may need to create some invitations on user's behalf
                    % (which is not possible when the user is blocked)
                    RequestContext = randomize_request_context(EligibleSubject),
                    ozt_users:toggle_access_block(UserId, true),
                    ?assertMatch(
                        ?ERROR_UNAUTHORIZED(?ERROR_USER_BLOCKED),
                        make_request(RequestSpec, RequestContext, ClientAuth)
                    ),
                    ozt_users:toggle_access_block(UserId, false),
                    ?assertMatch(ok, make_request(RequestSpec, RequestContext, ClientAuth));
                _ ->
                    ok
            end
        end, RequestSpec#request_spec.eligible_subjects)
    end, [named, temporary]).


check_temporary_token_revocation(RequestSpec) ->
    lists:foreach(fun(EligibleSubject) ->
        ClientAuth = gen_client_auth(EligibleSubject, temporary),
        ?assertMatch(ok, make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuth)),
        ozt_tokens:revoke_all_temporary_tokens(EligibleSubject),
        ?assertMatch(
            ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_REVOKED),
            make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuth)
        )
    end, RequestSpec#request_spec.eligible_subjects).


check_named_token_revocation(RequestSpec) ->
    lists:foreach(fun(EligibleSubject) ->
        ClientAuthAlpha = {token, TokenAlpha} = gen_client_auth(EligibleSubject, named),
        ClientAuthBeta = {token, TokenBeta} = gen_client_auth(EligibleSubject, named),
        ?assertMatch(ok, make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuthAlpha)),
        ?assertMatch(ok, make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuthBeta)),

        ozt_tokens:toggle_revoked(TokenAlpha, true),
        ?assertMatch(
            ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_REVOKED),
            make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuthAlpha))
        ,
        ?assertMatch(ok, make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuthBeta)),

        ozt_tokens:toggle_revoked(TokenAlpha, false),
        ozt_tokens:toggle_revoked(TokenBeta, true),
        ?assertMatch(ok, make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuthAlpha)),
        ?assertMatch(
            ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_REVOKED),
            make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuthBeta)
        )
    end, RequestSpec#request_spec.eligible_subjects).


check_token_caveats_handling(RequestSpec) ->
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            lists:foreach(fun(_) ->
                RequestContext = randomize_request_context(EligibleSubject),
                RandCorrectCaveats = lists_utils:random_sublist(gen_correct_caveats(RequestSpec, RequestContext)),
                RandUnverifiedCaveats = lists_utils:random_sublist(gen_unverified_caveats(RequestSpec, RequestContext)),
                ClientAuth = gen_client_auth(EligibleSubject, Persistence, RandCorrectCaveats ++ RandUnverifiedCaveats),
                % force entity graph reconciliation as some tests depend on
                % effective memberships/privileges in groups/spaces etc,
                % and some of these may be set up during caveats generation and be not yet recomputed
                ozt:reconcile_entity_graph(),
                Result = make_request(RequestSpec, RequestContext, ClientAuth),
                % These tests cover only a subset of combinations and possibly
                % there are some combinations wrongly handled by the test framework
                % or backend implementation (hopefully, the former).
                % If the result is not as expected, dump some verbose information to help in later debugging.
                ShouldRequestSucceed = (RandUnverifiedCaveats =:= []),
                TestPassed = case {ShouldRequestSucceed, Result} of
                    {true, ok} ->
                        true;
                    {true, _UnsuccessfulResult} ->
                        false;
                    {false, ok} ->
                        false;
                    {false, ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_CAVEAT_UNVERIFIED(_))} ->
                        ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_CAVEAT_UNVERIFIED(UnverifiedCaveat)) = Result,
                        lists:member(UnverifiedCaveat, RandUnverifiedCaveats)
                end,
                case TestPassed of
                    true ->
                        ok;
                    false ->
                        ct:pal("Token caveats handling test failed!"),
                        ct:pal("RequestSpec: ~tp", [RequestSpec]),
                        ct:pal("Persistence: ~tp", [Persistence]),
                        ct:pal("EligibleSubject: ~tp", [EligibleSubject]),
                        ct:pal("EligibleSubjectData: ~tp", [case EligibleSubject of
                            ?SUB(user, UserId) ->
                                ozt_users:get(UserId);
                            ?SUB(?ONEPROVIDER, ProviderId) ->
                                ozt_providers:get(ProviderId);
                            _ ->
                                <<"none">>
                        end ]),
                        ct:pal("RequestContext: ~tp", [RequestContext]),
                        ct:pal("RandCorrectCaveats: ~tp", [RandCorrectCaveats]),
                        ct:pal("RandUnverifiedCaveats: ~tp", [RandUnverifiedCaveats]),
                        ct:pal("ClientAuth: ~tp", [ClientAuth]),
                        ct:pal("Result: ~tp", [Result]),
                        error(test_failed)
                end
            end, lists:seq(1, ?CAVEAT_RANDOMIZATION_REPEATS))
        end, RequestSpec#request_spec.eligible_subjects)
    end, [named, temporary]).


check_service_token_caveats_handling(RequestSpec) ->
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            ValidServiceTokenHolders = eligible_service_types(EligibleSubject) -- [?OZ_WORKER, ?OZ_PANEL],
            lists:foreach(fun(ServiceType) ->
                lists:foreach(fun(_) ->
                    % Check only services that can present an service token
                    Service = set_up_service(EligibleSubject, ServiceType),
                    RequestContext = randomize_request_context(EligibleSubject, Service, random),
                    ClientAuth = gen_client_auth(EligibleSubject, Persistence),

                    ServiceRequestContext = RequestContext#request_context{
                        subject = case Service of
                            ?SERVICE(?OP_WORKER, PrId) -> ?SUB(?ONEPROVIDER, ?OP_WORKER, PrId);
                            ?SERVICE(?OP_PANEL, PrId) -> ?SUB(?ONEPROVIDER, ?OP_PANEL, PrId)
                        end,
                        % Identity tokens cannot have service caveats and cannot allow data access caveats
                        token_type = identity_token,
                        service = undefined,
                        service_token = undefined,
                        consumer = undefined,
                        consumer_token = undefined,
                        data_access_caveats_policy = disallow_data_access_caveats
                    },
                    RandCorrectCaveats = lists_utils:random_sublist(gen_correct_caveats(RequestSpec, ServiceRequestContext)),
                    RandUnverifiedCaveats = lists_utils:random_sublist(gen_unverified_caveats(RequestSpec, ServiceRequestContext)),
                    ServiceToken = create_service_token_if_applicable(
                        Service, lists_utils:shuffle(RandCorrectCaveats ++ RandUnverifiedCaveats)
                    ),
                    RequestContextWithServiceToken = RequestContext#request_context{service_token = ServiceToken},
                    % Service tokens are not supported in GraphSync - the service is
                    % inferred from the channel owner, after the owner connects with
                    % a regular access token. Use only logic and REST methods.
                    % Cases with service caveats via GraphSync are tested in
                    % 'check_token_caveats_handling'.
                    Result = make_request(RequestSpec, RequestContextWithServiceToken, ClientAuth, [logic, rest]),
                    case RandUnverifiedCaveats of
                        [] ->
                            ?assertMatch(ok, Result);
                        _ ->
                            % The service token may be used to init GS connection, in such
                            % case the returned error will not be wrapped in ?ERROR_BAD_SERVICE_TOKEN
                            TokenError = case Result of
                                ?ERROR_UNAUTHORIZED(?ERROR_BAD_SERVICE_TOKEN(Error)) -> ?ERROR_UNAUTHORIZED(Error);
                                Error -> Error
                            end,
                            ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_CAVEAT_UNVERIFIED(_)), TokenError),
                            ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_CAVEAT_UNVERIFIED(UnverifiedCaveat)) = TokenError,
                            ?assert(lists:member(UnverifiedCaveat, RandUnverifiedCaveats))
                    end
                end, lists:seq(1, ?CAVEAT_RANDOMIZATION_REPEATS))
            end, ValidServiceTokenHolders)
        end, RequestSpec#request_spec.eligible_subjects)
    end, [named, temporary]).


check_consumer_token_caveats_handling(RequestSpec) ->
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            lists:foreach(fun(ConsumerType) ->
                lists:foreach(fun(_) ->
                    Consumer = set_up_consumer(ConsumerType),
                    RequestContext = randomize_request_context(EligibleSubject, random, Consumer),
                    ClientAuth = gen_client_auth(EligibleSubject, Persistence),

                    ConsumerRequestContext = RequestContext#request_context{
                        % Identity tokens cannot have service caveats and cannot allow data access caveats
                        subject = Consumer,
                        token_type = identity_token,
                        service = undefined,
                        service_token = undefined,
                        consumer = undefined,
                        consumer_token = undefined,
                        data_access_caveats_policy = disallow_data_access_caveats
                    },
                    RandCorrectCaveats = lists_utils:random_sublist(gen_correct_caveats(RequestSpec, ConsumerRequestContext)),
                    RandUnverifiedCaveats = lists_utils:random_sublist(gen_unverified_caveats(RequestSpec, ConsumerRequestContext)),
                    ConsumerToken = create_consumer_token_if_applicable(
                        Consumer, lists_utils:shuffle(RandCorrectCaveats ++ RandUnverifiedCaveats)
                    ),
                    RequestContextWithConsumerToken = RequestContext#request_context{consumer_token = ConsumerToken},
                    Result = make_request(RequestSpec, RequestContextWithConsumerToken, ClientAuth),
                    case RandUnverifiedCaveats of
                        [] ->
                            ?assertMatch(ok, Result);
                        _ ->
                            ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_BAD_CONSUMER_TOKEN(?ERROR_TOKEN_CAVEAT_UNVERIFIED(_))), Result),
                            ?ERROR_UNAUTHORIZED(?ERROR_BAD_CONSUMER_TOKEN(?ERROR_TOKEN_CAVEAT_UNVERIFIED(UnverifiedCaveat))) = Result,
                            ?assert(lists:member(UnverifiedCaveat, RandUnverifiedCaveats))
                    end
                end, lists:seq(1, ?CAVEAT_RANDOMIZATION_REPEATS))
            end, valid_consumer_token_holders())
        end, RequestSpec#request_spec.eligible_subjects)
    end, [named, temporary]).


check_subject_deleted_scenarios(RequestSpec) ->
    RequestsToCheck = lists:flatmap(fun(Persistence) ->
        lists:flatmap(fun(EligibleSubject) ->
            [{randomize_request_context(EligibleSubject), gen_client_auth(EligibleSubject, Persistence)}]
        end, RequestSpec#request_spec.eligible_subjects)
    end, [named, temporary]),

    lists:foreach(fun
        (?SUB(user, UserId)) -> ozt_users:delete(UserId);
        (?SUB(?ONEPROVIDER, PrId)) -> ozt_providers:delete(PrId)
    end, RequestSpec#request_spec.eligible_subjects),

    lists:foreach(fun({RequestContext, ClientAuth}) ->
        ?assertMatch(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_INVALID), make_request(RequestSpec, RequestContext, ClientAuth))
    end, RequestsToCheck).


%%%===================================================================
%%% Helper functions
%%%===================================================================

all_subject_types() -> [user, ?ONEPROVIDER, nobody].


valid_consumer_token_holders() -> [user, ?ONEPROVIDER].


create_subject_by_type(nobody) ->
    ?SUB(nobody);
create_subject_by_type(user) ->
    ?SUB(user, ozt_users:create());
create_subject_by_type(?ONEPROVIDER) ->
    ?SUB(?ONEPROVIDER, ozt_providers:create()).


gen_client_auth(SubjectType, Persistence) ->
    gen_client_auth(SubjectType, Persistence, []).

gen_client_auth(?SUB(nobody), _Persistence, _Caveats) ->
    nobody;
gen_client_auth(Subject, Persistence, Caveats) ->
    {token, ozt_tokens:create(Persistence, Subject, ?ACCESS_TOKEN, Caveats)}.


make_request_with_random_context(RequestSpec, Subject, ClientAuth) ->
    RequestContext = randomize_request_context(Subject),
    make_request(RequestSpec, RequestContext, ClientAuth).


make_request(RequestSpec, RequestContext, ClientAuth) ->
    make_request(RequestSpec, RequestContext, ClientAuth, [logic, rest, gs, op_gs_with_override]).


make_request(RequestSpec, RequestContext, ClientAuth, CompatibleMethods) ->
    AvailableMethods = lists_utils:intersect(CompatibleMethods, available_methods_for_ctx(RequestContext)),
    case lists_utils:random_element(AvailableMethods) of
        logic -> request_via_logic(RequestSpec, RequestContext, ClientAuth);
        rest -> request_via_rest(RequestSpec, RequestContext, ClientAuth);
        gs -> request_via_gs(RequestSpec, RequestContext, ClientAuth);
        op_gs_with_override -> request_via_op_gs_with_override(RequestSpec, RequestContext, ClientAuth)
    end.


% Only some request methods can yield an auth context with specified IP,
% interface, subject, service, consumer and data access caveats policy:
%   * logic accepts any context as it is injected
%   * oz-worker's REST and GraphSync endpoints automatically set the policy to
%       disallow_data_access_caveats
%   * op-worker and op-panel can use the auth_override as user on the GraphSync
%       channel to request authorization in requested context, providing arbitrary
%       context: IP, interface, consumer token and data access caveats policy
%   * allow_data_access_caveats policy and oneclient interface can appear only
%       on Oneprovider's GraphSync channel with auth_override
%   * undefined IP address can only be achieved in logic calls
% Returns one of: logic | rest | gs | op_gs_with_override
available_methods_for_ctx(RequestContext) ->
    PeerIp = RequestContext#request_context.ip,
    Interface = RequestContext#request_context.interface,
    ?SUB(SubjectType) = RequestContext#request_context.subject,
    ServiceType = case RequestContext#request_context.service of
        undefined -> undefined;
        ?SERVICE(SType, _) -> SType
    end,
    ConsumerType = case RequestContext#request_context.consumer of
        undefined -> undefined;
        ?SUB(CType, _) -> CType
    end,
    DataAccessCaveatsPolicy = RequestContext#request_context.data_access_caveats_policy,
    AvailableMethods = case {PeerIp, Interface} of
        {undefined, _} -> [logic];
        {_, undefined} -> [logic];
        {_, rest} -> methods_for_rest_ctx(SubjectType, ServiceType, ConsumerType, DataAccessCaveatsPolicy);
        {_, oneclient} -> methods_for_oneclient_ctx(SubjectType, ServiceType, ConsumerType, DataAccessCaveatsPolicy);
        {_, graphsync} -> methods_for_graphsync_ctx(SubjectType, ServiceType, ConsumerType, DataAccessCaveatsPolicy)
    end,
    case {RequestContext#request_context.consumer, RequestContext#request_context.service} of
        {undefined, _} ->
            % In case the consumer is not defined in the RequestContext, the request should not
            % include any consumer indication. However, when op_gs_with_override is used and no
            % consumer token is presented in auth override, the requesting Oneprovider becomes
            % the consumer, which conflicts with the original assumption.
            lists:delete(op_gs_with_override, AvailableMethods);
        {?SUB(?ONEPROVIDER, ProviderId), ?SERVICE(ST, ProviderId)} when ST == ?OP_WORKER; ST == ?OP_PANEL ->
            % For the same reason as above, if the requesting service is a Oneprovider service,
            % the GS channel with auth override can be used to implicitly become the target consumer
            lists_utils:union([op_gs_with_override], AvailableMethods);
        _ ->
            AvailableMethods
    end.


methods_for_rest_ctx(_, ?OZ_PANEL, _, _) -> [logic];
methods_for_rest_ctx(_, ?OZ_WORKER, _, allow_data_access_caveats) -> [logic];
methods_for_rest_ctx(_, ?OZ_WORKER, _, disallow_data_access_caveats) -> [logic, rest];
methods_for_rest_ctx(_, undefined, _, allow_data_access_caveats) -> [logic];
methods_for_rest_ctx(_, undefined, _, disallow_data_access_caveats) -> [logic, rest];
methods_for_rest_ctx(nobody, _, _, disallow_data_access_caveats) -> [logic, rest];
methods_for_rest_ctx(user, _, ?ONEPROVIDER, allow_data_access_caveats) -> [logic];
methods_for_rest_ctx(user, _, ?ONEPROVIDER, disallow_data_access_caveats) -> [logic, rest];
methods_for_rest_ctx(user, _, _, allow_data_access_caveats) -> [logic, op_gs_with_override];
methods_for_rest_ctx(user, _, _, disallow_data_access_caveats) -> [logic, rest, op_gs_with_override];
methods_for_rest_ctx(?ONEPROVIDER, ?OP_WORKER, _, disallow_data_access_caveats) -> [logic, rest];
methods_for_rest_ctx(?ONEPROVIDER, ?OP_PANEL, _, disallow_data_access_caveats) -> [logic, rest];
methods_for_rest_ctx(_, _, _, _) -> [logic].


methods_for_oneclient_ctx(user, ?OP_WORKER, ?ONEPROVIDER, _) -> [logic];
methods_for_oneclient_ctx(user, ?OP_WORKER, _, _) -> [logic, op_gs_with_override];
methods_for_oneclient_ctx(_, _, _, _) -> [logic].


methods_for_graphsync_ctx(_, ?OZ_PANEL, _, _) -> [logic];
methods_for_graphsync_ctx(_, ?OZ_WORKER, undefined, disallow_data_access_caveats) -> [logic, gs];
methods_for_graphsync_ctx(_, ?OZ_WORKER, _, _) -> [logic];
methods_for_graphsync_ctx(_, undefined, undefined, disallow_data_access_caveats) -> [logic, gs];
methods_for_graphsync_ctx(_, undefined, _, _) -> [logic];
methods_for_graphsync_ctx(user, _, ?ONEPROVIDER, _) -> [logic];
methods_for_graphsync_ctx(user, _, _, _) -> [logic, op_gs_with_override];
methods_for_graphsync_ctx(_, _, _, _) -> [logic].


% Logic takes an #auth{} record that is a result of verifying the access and
% optionally service/consumer tokens. This code first verifies all tokens and
% upon success, calls the logic function with resulting #auth{}.
request_via_logic(RequestSpec, Rc, ClientAuth) ->
    try
        case get_service_token_for_request(Rc) of
            undefined ->
                ok;
            ServiceToken ->
                ServiceAuthCtx = #auth_ctx{
                    ip = Rc#request_context.ip,
                    interface = Rc#request_context.interface
                },
                case ozt_tokens:verify_service_token(ServiceToken, ServiceAuthCtx) of
                    {ok, Service} ->
                        ?assertEqual(Service, Rc#request_context.service);
                    {error, _} = Err1 ->
                        throw(?ERROR_UNAUTHORIZED(Err1))
                end
        end,

        case get_consumer_token_for_request(Rc) of
            undefined ->
                ok;
            ConsumerToken ->
                ConsumerAuthCtx = #auth_ctx{
                    ip = Rc#request_context.ip,
                    interface = Rc#request_context.interface
                },
                case ozt_tokens:verify_consumer_token(ConsumerToken, ConsumerAuthCtx) of
                    {ok, Consumer} ->
                        ?assertEqual(Consumer, Rc#request_context.consumer);
                    {error, _} = Err2 ->
                        throw(?ERROR_UNAUTHORIZED(Err2))
                end
        end,

        Auth = case ClientAuth of
            nobody ->
                ?NOBODY;
            {token, Token} ->
                AuthCtx = #auth_ctx{
                    ip = Rc#request_context.ip,
                    interface = Rc#request_context.interface,
                    service = Rc#request_context.service,
                    consumer = Rc#request_context.consumer,
                    data_access_caveats_policy = Rc#request_context.data_access_caveats_policy
                },
                case ozt_tokens:authenticate(Token, AuthCtx) of
                    {true, A} ->
                        A;
                    {error, _} = Err3 ->
                        throw(Err3)
                end
        end,
        {Module, Function, Args} = RequestSpec#request_spec.logic_call_args,
        ArgsWithAuth = lists:map(fun
            (auth) -> Auth;
            (Arg) -> Arg
        end, Args),
        check_success(ozt:rpc(Module, Function, ArgsWithAuth))
    catch
        throw:{error, _} = Error -> Error
    end.


request_via_rest(#request_spec{rest_call_args = {Method, UrnTokens, DataJson}}, RequestContext, ClientAuth) ->
    ServiceToken = get_service_token_for_request(RequestContext),
    ConsumerToken = get_consumer_token_for_request(RequestContext),
    check_success(ozt_http:rest_call(ClientAuth, ServiceToken, ConsumerToken, Method, UrnTokens, DataJson)).


request_via_gs(RequestSpec, RequestContext, ClientAuth) ->
    Endpoint = case RequestContext#request_context.subject of
        ?SUB(?ONEPROVIDER) -> oneprovider;
        _ -> gui
    end,
    request_via_gs(RequestSpec, Endpoint, ClientAuth, undefined).

request_via_gs(RequestSpec, Endpoint, ClientAuth, AuthOverride) ->
    {AuthHint, Data} = RequestSpec#request_spec.additional_graph_sync_args,
    Req = #gs_req{
        subtype = graph,
        auth_override = AuthOverride,
        request = #gs_req_graph{
            gri = RequestSpec#request_spec.gri,
            operation = RequestSpec#request_spec.operation,
            data = Data,
            subscribe = false,
            auth_hint = AuthHint
        }
    },
    check_success(ozt_gs:connect_and_request(Endpoint, ClientAuth, Req)).

request_via_op_gs_with_override(RequestSpec, RequestContext, ClientAuth) ->
    ?SERVICE(OpService, ProviderId) = RequestContext#request_context.service,
    ProviderToken = ozt_tokens:create(temporary, ?SUB(?ONEPROVIDER, ProviderId)),
    OpServiceToken = tokens:add_oneprovider_service_indication(OpService, ozt_tokens:ensure_serialized(ProviderToken)),
    ConsumerToken = case get_consumer_token_for_request(RequestContext) of
        undefined -> undefined;
        Token -> ozt_tokens:ensure_serialized(Token)
    end,
    AuthOverride = #auth_override{
        client_auth = ozt_gs:normalize_client_auth(ClientAuth),
        interface = RequestContext#request_context.interface,
        peer_ip = RequestContext#request_context.ip,
        consumer_token = case RequestContext#request_context.consumer of
            ?SUB(?ONEPROVIDER, ProviderId) ->
                % If the requesting Oneprovider is the same as the target consumer, test also
                % the possibility of becoming the consumer implicitly without presenting a token
                % (which is the default behaviour on the GS channel with auth override).
                lists_utils:random_element([undefined, ConsumerToken]);
            _ ->
                ConsumerToken
        end,
        data_access_caveats_policy = RequestContext#request_context.data_access_caveats_policy
    },
    request_via_gs(RequestSpec, oneprovider, {token, OpServiceToken}, AuthOverride).


check_success(ok) -> ok;
check_success({ok, _}) -> ok;
check_success({error, _} = Error) -> Error.


get_service_token_for_request(#request_context{service = undefined}) ->
    undefined;
get_service_token_for_request(#request_context{service = Service, service_token = autogenerate}) ->
    create_service_token_if_applicable(Service);
get_service_token_for_request(#request_context{service = _, service_token = ServiceToken}) ->
    ServiceToken.


get_consumer_token_for_request(#request_context{consumer = undefined}) ->
    undefined;
get_consumer_token_for_request(#request_context{consumer = Consumer, consumer_token = autogenerate}) ->
    create_consumer_token_if_applicable(Consumer);
get_consumer_token_for_request(#request_context{consumer = _, consumer_token = ConsumerToken}) ->
    ConsumerToken.


create_service_token_if_applicable(Subject) ->
    create_service_token_if_applicable(Subject, []).

create_service_token_if_applicable(?SERVICE(?OP_WORKER, PrId), Caveats) ->
    ProviderToken = ozt_tokens:create(temporary, ?SUB(?ONEPROVIDER, PrId), ?IDENTITY_TOKEN, Caveats),
    tokens:add_oneprovider_service_indication(?OP_WORKER, ozt_tokens:ensure_serialized(ProviderToken));
create_service_token_if_applicable(?SERVICE(?OP_PANEL, PrId), Caveats) ->
    ProviderToken = ozt_tokens:create(temporary, ?SUB(?ONEPROVIDER, PrId), ?IDENTITY_TOKEN, Caveats),
    tokens:add_oneprovider_service_indication(?OP_PANEL, ozt_tokens:ensure_serialized(ProviderToken));
create_service_token_if_applicable(_, _) ->
    undefined.


create_consumer_token_if_applicable(Subject) ->
    create_consumer_token_if_applicable(Subject, []).

create_consumer_token_if_applicable(?SUB(user, UserId), Caveats) ->
    UserToken = ozt_tokens:create(temporary, ?SUB(user, UserId), ?IDENTITY_TOKEN, Caveats),
    ozt_tokens:ensure_serialized(UserToken);
create_consumer_token_if_applicable(?SUB(?ONEPROVIDER, Subtype, PrId), Caveats) ->
    ProviderToken = ozt_tokens:create(temporary, ?SUB(?ONEPROVIDER, PrId), ?IDENTITY_TOKEN, Caveats),
    Serialized = ozt_tokens:ensure_serialized(ProviderToken),
    case Subtype of
        ?OP_WORKER -> tokens:add_oneprovider_service_indication(?OP_WORKER, Serialized);
        ?OP_PANEL -> tokens:add_oneprovider_service_indication(?OP_PANEL, Serialized);
        undefined -> Serialized
    end;
create_consumer_token_if_applicable(_, _) ->
    undefined.


randomize_request_context(Subject) ->
    randomize_request_context(Subject, random, random).

% Service and Consumer can be 'random' or given explicitly
randomize_request_context(Subject, Service, Consumer) ->
    % ~ 1/5 of test repeats should have undefined IP
    IP = lists_utils:random_element([undefined, ?PEER_IP, ?PEER_IP, ?PEER_IP, ?PEER_IP]),
    Asn = case IP of
        undefined -> undefined;
        _ -> rand:uniform(10000)
    end,
    {Country, Regions} = case IP of
        undefined -> {undefined, undefined};
        _ -> lists_utils:random_element(?GEO_EXAMPLES)
    end,
    case IP of
        undefined -> ok;
        _ -> ozt_mocks:mock_geo_db_entry_for_all_ips(Asn, Country, Regions)
    end,
    ResolvedService = case Service of
        random ->
            case rand:uniform(2) of
                1 -> undefined;
                2 -> set_up_service(Subject, lists_utils:random_element(eligible_service_types(Subject)))
            end;
        _ ->
            Service
    end,
    ResolvedConsumer = case Consumer of
        random ->
            case rand:uniform(2) of
                1 -> undefined;
                2 -> set_up_consumer(lists_utils:random_element(valid_consumer_token_holders()))
            end;
        _ ->
            Consumer
    end,
    DataAccessCaveatsPolicy = lists_utils:random_element([
        % ~ 1/4 of test repeats should allow data access caveats
        disallow_data_access_caveats, disallow_data_access_caveats, disallow_data_access_caveats,
        allow_data_access_caveats
    ]),
    OneclientInterfaceEligible = case ResolvedService of
        ?SERVICE(?OP_WORKER, _) -> true;
        _ -> false
    end,
    EligibleInterfaces = case OneclientInterfaceEligible of
        true -> cv_interface:valid_interfaces();
        false -> cv_interface:valid_interfaces() -- [oneclient]
    end,
    #request_context{
        subject = Subject,
        token_type = access_token,
        current_timestamp = ozt:timestamp_seconds(),
        interface = lists_utils:random_element([undefined | EligibleInterfaces]),
        ip = IP,
        asn = Asn,
        country = Country,
        regions = Regions,
        service = ResolvedService,
        consumer = ResolvedConsumer,
        data_access_caveats_policy = DataAccessCaveatsPolicy
    }.


eligible_service_types(?SUB(user)) ->
    [?OZ_WORKER, ?OZ_PANEL, ?OP_WORKER, ?OP_PANEL];
eligible_service_types(_) ->
    [?OZ_WORKER].


% Substantiate a service of given type. If needed, create required relations
% between the subject and the service.
set_up_service(_Subject, undefined) ->
    undefined;
set_up_service(_Subject, ?OZ_WORKER) ->
    ?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID);
set_up_service(?SUB(user, UserId), ?OZ_PANEL) ->
    ozt_clusters:ensure_member(?ONEZONE_CLUSTER_ID, UserId),
    ?SERVICE(?OZ_PANEL, ?ONEZONE_CLUSTER_ID);
set_up_service(?SUB(user, UserId), ?OP_WORKER) ->
    ProviderId = ozt_providers:create_as_support_for_user(UserId),
    ?SERVICE(?OP_WORKER, ProviderId);
set_up_service(?SUB(?ONEPROVIDER), ?OP_WORKER) ->
    OtherProviderId = ozt_providers:create(),
    ?SERVICE(?OP_WORKER, OtherProviderId);
set_up_service(?SUB(user, UserId), ?OP_PANEL) ->
    ProviderId = ozt_providers:create_for_admin_user(UserId),
    ?SERVICE(?OP_PANEL, ProviderId).


% Substantiate a consumer of given type.
set_up_consumer(nobody) ->
    undefined;
set_up_consumer(user) ->
    ?SUB(user, ozt_users:create());
set_up_consumer(?ONEPROVIDER) ->
    ?SUB(?ONEPROVIDER, ozt_providers:create()).


gen_correct_caveats(RequestSpec, RC) -> lists:flatten([
    #cv_time{valid_until = RC#request_context.current_timestamp + 10},
    case RC#request_context.ip of
        undefined -> [];
        _ -> gen_correct_ip_based_caveats(RC)
    end,
    case gen_correct_service_caveat(RequestSpec, RC) of
        undefined -> [];
        ServiceCaveat -> ServiceCaveat
    end,
    case RC#request_context.consumer of
        undefined -> [];
        _ -> gen_correct_consumer_caveat(RC)
    end,
    case RC#request_context.interface of
        undefined ->
            [];
        oneclient ->
            % oneclient interface caveat causes API limitations
            case RequestSpec#request_spec.available_with_data_access_caveats of
                false -> [];
                true -> #cv_interface{interface = oneclient}
            end;
        Interface ->
            #cv_interface{interface = Interface}
    end,
    case RC#request_context.token_type of
        access_token -> gen_correct_api_caveat(RequestSpec);
        identity_token -> [] % Identity tokens cannot have API caveats
    end,
    gen_correct_data_access_caveats(RequestSpec, RC)
]).


gen_unverified_caveats(RequestSpec, RC) -> lists:flatten([
    #cv_time{valid_until = RC#request_context.current_timestamp - 1},
    case gen_unverified_service_caveat(RequestSpec, RC) of
        undefined -> [];
        ServiceCaveat -> ServiceCaveat
    end,
    gen_unverified_consumer_caveat(RC),
    case RC#request_context.ip of
        undefined ->
            {Country, Regions} = lists_utils:random_element(?GEO_EXAMPLES),
            gen_correct_ip_based_caveats(RC#request_context{
                ip = ?PEER_IP,
                asn = rand:uniform(10000),
                country = Country,
                regions = Regions
            });
        _ ->
            gen_unverified_ip_based_caveats(RC)
    end,
    #cv_interface{interface = lists_utils:random_element(case RC#request_context.interface of
        undefined ->
            cv_interface:valid_interfaces();
        oneclient ->
            case {RC#request_context.token_type, RequestSpec#request_spec.available_with_data_access_caveats} of
                % oneclient interface caveat causes API limitations, but only for access tokens
                {access_token, false} -> cv_interface:valid_interfaces();
                _ -> cv_interface:valid_interfaces() -- [oneclient]
            end;
        Interface ->
            cv_interface:valid_interfaces() -- [Interface]
    end)},
    case RC#request_context.token_type of
        access_token -> gen_unverified_api_caveat(RequestSpec);
        identity_token -> gen_correct_api_caveat(RequestSpec) % Identity tokens cannot have API caveats
    end,
    gen_unverified_data_access_caveats(RequestSpec, RC)
]).


gen_correct_service_caveat(_RequestSpec, #request_context{service = undefined}) ->
    undefined;
gen_correct_service_caveat(RequestSpec, Rc = #request_context{subject = Subject, service = ?SERVICE(Type, _Id)}) ->
    UnverifiedServiceSpecs = gen_unverified_service_specs(RequestSpec, Rc),
    case lists:member(Type, RequestSpec#request_spec.services_allowed_to_call_this_api) of
        false ->
            % if the request context service is not allowed to call the API, there has
            % to be another service on the whitelist that is allowed, or there can be
            % no service caveat at all for the request to succeed
            case rand:uniform(2) of
                1 ->
                    undefined;
                2 ->
                    APIAllowedService = set_up_service(Subject, lists_utils:random_element(
                        RequestSpec#request_spec.services_allowed_to_call_this_api
                    )),
                    #cv_service{whitelist = lists_utils:shuffle(lists:flatten([
                        lists_utils:random_sublist(gen_correct_service_specs(Rc), 1, all),
                        lists_utils:random_sublist(gen_correct_service_specs(APIAllowedService), 1, all),
                        lists_utils:random_sublist(UnverifiedServiceSpecs)
                    ]))}
            end;
        true ->
            #cv_service{whitelist = lists_utils:shuffle(lists:flatten([
                lists_utils:random_sublist(gen_correct_service_specs(Rc), 1, all),
                lists_utils:random_sublist(UnverifiedServiceSpecs)
            ]))}
    end.

gen_unverified_service_caveat(RequestSpec, Rc) ->
    case gen_unverified_service_specs(RequestSpec, Rc) of
        [] ->
            undefined;
        UnverifiedServiceSpecs ->
            #cv_service{whitelist = lists_utils:random_sublist(UnverifiedServiceSpecs, 1, all)}
    end.


gen_correct_service_specs(#request_context{service = Service}) ->
    gen_correct_service_specs(Service);
gen_correct_service_specs(?SERVICE(Type, Id)) ->
    [
        ?SERVICE(Type, Id),
        ?SERVICE(Type, <<"*">>)
    ].

gen_unverified_service_specs(RequestSpec, #request_context{subject = Subject, service = RequestService}) ->
    EligibleTypes = eligible_service_types(Subject),
    UnverifiedTypes = case RequestService of
        undefined ->
            % If not specified, service defaults to OZ_WORKER
            EligibleTypes -- [?OZ_WORKER];
        ?SERVICE(Type, _) ->
            case rand:uniform(2) of
                1 ->
                    % all services that do not match the service in request context
                    EligibleTypes -- [Type];
                2 ->
                    % all services that are forbidden to call the API
                    EligibleTypes -- RequestSpec#request_spec.services_allowed_to_call_this_api
            end
    end,
    lists:map(fun(ServiceType) ->
        Service = set_up_service(Subject, ServiceType),
        case rand:uniform(2) of
            1 -> Service;
            2 -> Service#service_spec{id = <<"*">>}
        end
    end, UnverifiedTypes).


gen_forbidden_service_specs(?SUB(user, UserId)) ->
    ProviderId = ozt_providers:create(),
    ozt_clusters:ensure_not_a_member(?ONEZONE_CLUSTER_ID, UserId),
    [
        ?SERVICE(?OZ_PANEL, ?ONEZONE_CLUSTER_ID),
        ?SERVICE(?OP_WORKER, ProviderId),
        ?SERVICE(?OP_PANEL, ProviderId)
    ];
gen_forbidden_service_specs(?SUB(?ONEPROVIDER)) ->
    OtherProviderId = ozt_providers:create(),
    [
        ?SERVICE(?OZ_PANEL, ?ONEZONE_CLUSTER_ID),
        ?SERVICE(?OP_PANEL, OtherProviderId)
    ].


gen_correct_consumer_caveat(RequestContext) ->
    CorrectConsumers = gen_correct_consumers(RequestContext),
    UnverifiedConsumers = gen_unverified_consumers(RequestContext),
    #cv_consumer{whitelist = lists_utils:shuffle(lists:flatten([
        lists_utils:random_sublist(CorrectConsumers, 1, all),
        lists_utils:random_sublist(UnverifiedConsumers)
    ]))}.

gen_unverified_consumer_caveat(Rc) ->
    UnverifiedConsumers = gen_unverified_consumers(Rc),
    #cv_consumer{whitelist = lists_utils:random_sublist(UnverifiedConsumers, 1, all)}.

gen_correct_consumers(#request_context{consumer = ?SUB(user, UserId)}) ->
    GroupId = ozt_users:create_group_for(UserId),
    [
        ?SUB(user, UserId),
        ?SUB(user, <<"*">>),
        ?SUB(group, GroupId),
        ?SUB(group, <<"*">>)
    ];
gen_correct_consumers(#request_context{consumer = ?SUB(Type, Id)}) ->
    [
        ?SUB(Type, Id),
        ?SUB(Type, <<"*">>)
    ].

gen_unverified_consumers(#request_context{consumer = RequestConsumer}) ->
    ValidConsumerTypes = valid_consumer_token_holders(),
    UnverifiedTypes = case RequestConsumer of
        undefined -> ValidConsumerTypes;
        ?SUB(Type, _) -> ValidConsumerTypes -- [Type]
    end,
    lists:map(fun(ConsumerType) ->
        Consumer = set_up_consumer(ConsumerType),
        case rand:uniform(2) of
            1 -> Consumer;
            2 -> Consumer#subject{id = <<"*">>}
        end
    end, UnverifiedTypes).


gen_correct_ip_based_caveats(RC) -> [
    #cv_ip{whitelist = [?CORRECT_MASK_1]},
    #cv_ip{whitelist = [?CORRECT_MASK_2]},
    #cv_ip{whitelist = [{{0, 0, 0, 0}, 8}, ?CORRECT_MASK_1, ?CORRECT_MASK_2]},
    #cv_asn{whitelist = [RC#request_context.asn]},
    #cv_asn{whitelist = [1, 2, 3, RC#request_context.asn]},
    #cv_country{type = whitelist, list = [RC#request_context.country]},
    #cv_country{type = whitelist, list = [<<"DE">>, RC#request_context.country, <<"FR">>]},
    #cv_country{type = blacklist, list = [<<"DE">>, <<"FR">>] -- [RC#request_context.country]},
    #cv_region{type = whitelist, list = RC#request_context.regions},
    #cv_region{type = whitelist, list = [<<"Europe">> | RC#request_context.regions]},
    #cv_region{type = blacklist, list = [<<"Europe">>, <<"Asia">>] -- RC#request_context.regions}
].

gen_unverified_ip_based_caveats(RC) -> [
    #cv_ip{whitelist = [?INCORRECT_MASK_1]},
    #cv_ip{whitelist = [?INCORRECT_MASK_2]},
    #cv_asn{whitelist = [1, RC#request_context.asn - 1, RC#request_context.asn + 1]},
    #cv_country{type = whitelist, list = [<<"DE">>, <<"FR">>] -- [RC#request_context.country]},
    #cv_country{type = blacklist, list = [RC#request_context.country]},
    #cv_country{type = blacklist, list = [<<"DE">>, RC#request_context.country, <<"FR">>]},
    #cv_region{type = whitelist, list = [<<"Europe">>, <<"Asia">>] -- RC#request_context.regions},
    #cv_region{type = blacklist, list = RC#request_context.regions},
    #cv_region{type = blacklist, list = [<<"Europe">>, <<"SouthAmerica">> | RC#request_context.regions]}
].


gen_correct_api_caveat(#request_spec{operation = Operation, gri = GRI}) ->
    Correct = lists:map(fun(_) ->
        gen_correct_api_matchspec(Operation, GRI)
    end, lists:seq(1, rand:uniform(5))), % 1 - 5 entries
    Unverified = lists:map(fun(_) ->
        gen_unverified_api_matchspec(Operation, GRI)
    end, lists:seq(2, rand:uniform(5))), % 0 - 4 entries
    #cv_api{whitelist = lists_utils:shuffle(Correct ++ Unverified)}.

gen_unverified_api_caveat(#request_spec{operation = Operation, gri = GRI}) ->
    Unverified = lists:map(fun(_) ->
        gen_unverified_api_matchspec(Operation, GRI)
    end, lists:seq(1, rand:uniform(5))), % 1 - 5 entries
    #cv_api{whitelist = lists_utils:shuffle(Unverified)}.

gen_correct_api_matchspec(Operation, #gri{type = Type, id = Id, aspect = Aspect, scope = Scope}) ->
    ServicePattern = lists_utils:random_element([all, ?OZ_WORKER]),
    OperationPattern = lists_utils:random_element([all, Operation]),
    {ServicePattern, OperationPattern, #gri_pattern{
        type = lists_utils:random_element([Type, '*']),
        id = lists_utils:random_element([Id, <<"*">>]),
        aspect = lists_utils:random_element(case Aspect of
            {A, B} -> [
                <<"*">>,
                {str_utils:to_binary(A), str_utils:to_binary(B)},
                {<<"*">>, str_utils:to_binary(B)},
                {str_utils:to_binary(A), <<"*">>},
                {<<"*">>, <<"*">>}
            ];
            A -> [
                str_utils:to_binary(A),
                <<"*">>
            ]
        end),
        scope = lists_utils:random_element([Scope, '*'])
    }}.

gen_unverified_api_matchspec(Operation, GRI = #gri{type = Type, aspect = Aspect, scope = Scope}) ->
    % Randomize which fields should be unverified (at least one)
    UnverifiedFields = lists_utils:random_sublist([service, operation, type, id, aspect, scope], 1, all),
    {CorrectServicePattern, CorrectOperationPattern, CorrectGriPattern} = gen_correct_api_matchspec(Operation, GRI),

    ServicePattern = case lists:member(service, UnverifiedFields) of
        true -> lists_utils:random_element([?OZ_PANEL, ?OP_WORKER, ?OP_PANEL]);
        false -> CorrectServicePattern
    end,

    OperationPattern = case lists:member(operation, UnverifiedFields) of
        true -> lists_utils:random_element([create, get, update, delete] -- [Operation]);
        false -> CorrectOperationPattern
    end,

    OrigAspectPattern = case Aspect of
        {A, B} -> {str_utils:to_binary(A), str_utils:to_binary(B)};
        A -> str_utils:to_binary(A)
    end,

    GriPattern = #gri_pattern{
        type = case lists:member(type, UnverifiedFields) of
            true -> lists_utils:random_element([od_user, od_group, od_space, od_share] -- [Type]);
            false -> CorrectGriPattern#gri_pattern.type
        end,
        id = case lists:member(id, UnverifiedFields) of
            true -> datastore_key:new();
            false -> CorrectGriPattern#gri_pattern.id
        end,
        aspect = case lists:member(aspect, UnverifiedFields) of
            true -> lists_utils:random_element([<<"instance">>, <<"users">>, <<"eff_groups">>] -- [OrigAspectPattern]);
            false -> CorrectGriPattern#gri_pattern.aspect
        end,
        scope = case lists:member(scope, UnverifiedFields) of
            true -> lists_utils:random_element([private, protected, shared, public] -- [Scope]);
            false -> CorrectGriPattern#gri_pattern.scope
        end
    },
    {ServicePattern, OperationPattern, GriPattern}.


gen_correct_data_access_caveats(RequestSpec, RequestContext) ->
    gen_data_access_caveats(correct, RequestSpec, RequestContext).

gen_unverified_data_access_caveats(RequestSpec, RequestContext) ->
    gen_data_access_caveats(unverified, RequestSpec, RequestContext).

gen_data_access_caveats(Verifiability, RequestSpec, RequestContext) ->
    RequestedSpace = case RequestSpec#request_spec.gri of
        ?GRI(od_space, undefined, instance, _) -> undefined;
        ?GRI(od_space, SpaceId, instance, _) -> SpaceId;
        ?GRI(_, _, _, _) -> undefined
    end,
    gen_data_access_caveats(Verifiability, RequestSpec, RequestContext, RequestedSpace).

gen_data_access_caveats(correct, RequestSpec, RequestContext, RequestedSpace) ->
    case are_data_access_caveats_allowed(RequestSpec, RequestContext) of
        false ->
            [];
        true ->
            gen_data_access_caveats(RequestedSpace)
    end;
gen_data_access_caveats(unverified, RequestSpec, RequestContext, RequestedSpace) ->
    case are_data_access_caveats_allowed(RequestSpec, RequestContext) of
        false ->
            gen_data_access_caveats(RequestedSpace);
        true ->
            case RequestedSpace of
                undefined ->
                    [];
                _ ->
                    % Specific space was requested - generate caveats allowing
                    % to only view another one. Readonly caveat does not depend
                    % on the space, so it should always be correct if policy
                    % allows data access caveats.
                    gen_data_access_caveats(<<"another-space">>) -- [#cv_data_readonly{}]
            end
    end.

gen_data_access_caveats(RequestedSpace) ->
    % If requested space is undefined, it means that the request is not related
    % to any space and the path/objectid caveats do not have any impact on it.
    % Otherwise, if asking for a specific space when data access caveats are
    % present, the space must be whitelisted in path/objectid caveats.
    SpaceId = case RequestedSpace of
        undefined -> str_utils:rand_hex(16);
        SId -> SId
    end,
    [
        #cv_data_readonly{},
        #cv_data_path{whitelist = [
            <<"/", SpaceId/binary, "/b/c/d">>,
            <<"/", SpaceId/binary, "/dir/file.txt">>
        ]},
        #cv_data_objectid{whitelist = [
            ?RAND_OBJECTID(SpaceId),
            ?RAND_OBJECTID(SpaceId),
            ?RAND_OBJECTID(SpaceId)
        ]}
    ].


% Checks if data access caveats are allowed (verifiable) for given request and
% specific request context.
are_data_access_caveats_allowed(RequestSpec, RequestContext) ->
    OperationAvailable = RequestSpec#request_spec.available_with_data_access_caveats,
    Policy = RequestContext#request_context.data_access_caveats_policy,
    {true, allow_data_access_caveats} == {OperationAvailable, Policy}.
