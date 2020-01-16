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
%%% operations. Each test is repeated, and each time the request context is
%%% randomized. Depending on the context, different combinations of subjects,
%%% tokens caveats and interfaces are tested if they return the expected results.
%%% @end
%%%-------------------------------------------------------------------
-module(access_tokens_test_SUITE).
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
-define(TEST_REPEATS, 4).
-define(CAVEAT_RANDOMIZATION_REPEATS, 4).

% Mocked connection and geo db data
-define(PEER_IP, {81, 213, 5, 17}).
-define(CORRECT_MASK_1, {{81, 213, 5, 0}, 24}).
-define(CORRECT_MASK_2, {{81, 0, 0, 0}, 8}).
-define(INCORRECT_MASK_1, {{1, 2, 3, 4}, 19}).
-define(INCORRECT_MASK_2, {{100, 78, 9, 0}, 22}).

-define(RAND_OBJECTID(SpaceId),
    element(2, {ok, _} = file_id:guid_to_objectid(file_id:pack_guid(str_utils:rand_hex(4), SpaceId)))
).

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
    % Specifies if such operation is available if the auth token contains a data_access_caveat
    available_with_data_access_caveats :: boolean(),
    % Args should typically start with 'auth' atom, which is substituted for client's auth
    logic_call_args :: {Module :: atom(), Function :: atom(), Args :: [term()]},
    rest_call_args :: {http_client:method(), ozt_http:urn_tokens(), entity_logic:data()},
    additional_graph_sync_args :: {gs_protocol:auth_hint(), entity_logic:data()}
}).

-record(request_context, {
    subject :: aai:subject(),
    % Denotes if the context concerns an audience token
    is_audience_token :: boolean(),
    current_timestamp :: time_utils:seconds(),
    interface :: undefined | cv_interface:interface(),
    ip :: undefined | ip_utils:ip(),
    asn :: undefined | ip_utils:asn(),
    country :: undefined | ip_utils:country_code(),
    regions :: undefined | [ip_utils:region()],
    audience :: aai:audience(),
    data_access_caveats_policy :: data_access_caveats:policy(),
    % Supporting provider that will be used to perform requests on behalf of
    % the user on Oneprovider's GraphSync channel with auth_override.
    % Applicable only if subject is a user.
    provider_for_gs_with_auth_override :: not_applicable | od_provider:id()
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
    hackney:start(),
    ozt:init_per_suite(Config).

init_per_testcase(_, Config) ->
    ozt_mocks:mock_time(),
    ozt_mocks:mock_gui_static(),
    ozt_mocks:mock_peer_ip_of_all_connections(?PEER_IP),
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unmock_time(),
    ozt_mocks:unmock_gui_static(),
    ozt_mocks:unmock_peer_ip_of_all_connections(),
    ok.

end_per_suite(_Config) ->
    hackney:stop(),
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
        available_with_data_access_caveats = false,
        logic_call_args = {group_logic, get_protected_data, [auth, GroupId]},
        rest_call_args = {get, [<<"/groups/">>, GroupId], #{}},
        additional_graph_sync_args = {undefined, #{}}
    })).


get_group_as_provider(_Config) ->
    UserId = ozt_users:create(),
    GroupId = ozt_users:create_group_for(UserId),
    SpaceId = ozt_groups:create_space_for(GroupId),
    ProviderId = ozt_providers:create(),
    ozt_providers:support_space(ProviderId, SpaceId),
    ?assert(run_tests(#request_spec{
        eligible_subjects = [?SUB(?ONEPROVIDER, ProviderId)],
        operation = get,
        gri = ?GRI(od_group, GroupId, instance, protected),
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
        check_bad_audience_when_creating_scenarios(RequestSpec),
        check_bad_audience_when_consuming_scenarios(RequestSpec),
        check_unauthorized_scenarios(RequestSpec),
        check_authorized_scenarios(RequestSpec),
        check_temporary_token_revocation(RequestSpec),
        check_named_token_revocation(RequestSpec),
        check_token_caveats_handling(RequestSpec),
        check_audience_token_caveats_handling(RequestSpec),
        % This must be run with last repeat as it deletes the eligible subjects
        RepeatNum == ?TEST_REPEATS andalso check_subject_deleted_scenarios(RequestSpec),
        true
    catch Type:Reason ->
        ct:pal("Access tokens test failed due to ~p:~p~nStacktrace: ~s", [
            Type, Reason, lager:pr_stacktrace(erlang:get_stacktrace())
        ]),
        false
    end.


check_bad_token_scenarios(RequestSpec) ->
    DummyProvider = ozt_providers:create(),
    DummyUser = ozt_users:create(),
    SessId = ozt_http:simulate_login(DummyUser),

    BadToken = <<"this-is-a-bad-token-that-cannot-be-decoded">>,
    ?assertEqual(
        ?ERROR_BAD_TOKEN,
        make_request_with_random_context(RequestSpec, ?SUB(user, DummyUser), {token, BadToken})
    ),

    InviteToken = ozt_tokens:create(
        named, ?SUB(?ONEPROVIDER, DummyProvider),
        ?INVITE_TOKEN(?USER_JOIN_CLUSTER, DummyProvider)
    ),
    ?assertEqual(
        ?ERROR_NOT_AN_ACCESS_TOKEN(?INVITE_TOKEN(?USER_JOIN_CLUSTER, DummyProvider)),
        make_request_with_random_context(RequestSpec, ?SUB(user, DummyUser), {token, InviteToken})
    ),

    GuiAccessToken = ozt_tokens:create_gui_access_token(DummyUser, SessId, ?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)),
    Error = ?assertMatch(
        {error, _},
        make_request_with_random_context(RequestSpec, ?SUB(user, DummyUser), {token, GuiAccessToken})
    ),
    % The error depends on the request context, which is randomized with every request
    ?assert(lists:member(Error, [
        ?ERROR_FORBIDDEN,
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_audience{whitelist = [?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]})
    ])),

    lists:foreach(fun(EligibleSubject) ->
        ForgedToken = tokens:construct(#token{
            onezone_domain = ozt:get_domain(),
            id = <<"123123123123">>,
            subject = EligibleSubject,
            type = ?ACCESS_TOKEN,
            persistent = false
        }, <<"secret">>, []),
        ?assertEqual(
            ?ERROR_TOKEN_INVALID,
            make_request_with_random_context(RequestSpec, EligibleSubject, {token, ForgedToken})
        )
    end, RequestSpec#request_spec.eligible_subjects).


check_bad_audience_when_creating_scenarios(RequestSpec) ->
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            lists:foreach(fun(ForbiddenAudience) ->
                ?assertEqual(
                    ?ERROR_TOKEN_AUDIENCE_FORBIDDEN(ForbiddenAudience),
                    ozt_tokens:try_create(Persistence, EligibleSubject, #{
                        <<"type">> => ?ACCESS_TOKEN,
                        <<"caveats">> =>  [#cv_audience{whitelist = [ForbiddenAudience]}]
                    })
                ),
                Token = ozt_tokens:create(Persistence, EligibleSubject),
                ?assertEqual(
                    ?ERROR_TOKEN_AUDIENCE_FORBIDDEN(ForbiddenAudience),
                    ozt_tokens:confine(Token, [#cv_audience{whitelist = [ForbiddenAudience]}])
                )
            end, gen_forbidden_audiences(EligibleSubject))
        end, RequestSpec#request_spec.eligible_subjects)
    end, [named, temporary]).


check_bad_audience_when_consuming_scenarios(RequestSpec) ->
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            RequestContext = randomize_request_context(EligibleSubject),
            lists:foreach(fun(ForbiddenAudience) ->
                ClientAuth = gen_client_auth(EligibleSubject, Persistence),
                RequestContextWithAudience = RequestContext#request_context{
                    audience = ForbiddenAudience
                },
                ?assertEqual(
                    ?ERROR_TOKEN_AUDIENCE_FORBIDDEN(ForbiddenAudience),
                    make_request(RequestSpec, RequestContextWithAudience, ClientAuth)
                )
            end, gen_forbidden_audiences(EligibleSubject))
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


check_temporary_token_revocation(RequestSpec) ->
    lists:foreach(fun(EligibleSubject) ->
        ClientAuth = gen_client_auth(EligibleSubject, temporary),
        ?assertMatch(ok, make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuth)),
        ozt_tokens:revoke_all_temporary_tokens(EligibleSubject),
        ?assertMatch(?ERROR_TOKEN_INVALID, make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuth))
    end, RequestSpec#request_spec.eligible_subjects).


check_named_token_revocation(RequestSpec) ->
    lists:foreach(fun(EligibleSubject) ->
        ClientAuthAlpha = {token, TokenAlpha} = gen_client_auth(EligibleSubject, named),
        ClientAuthBeta = {token, TokenBeta} = gen_client_auth(EligibleSubject, named),
        ?assertMatch(ok, make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuthAlpha)),
        ?assertMatch(ok, make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuthBeta)),

        ozt_tokens:toggle_revoked(TokenAlpha, true),
        ?assertMatch(?ERROR_TOKEN_REVOKED, make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuthAlpha)),
        ?assertMatch(ok, make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuthBeta)),

        ozt_tokens:toggle_revoked(TokenAlpha, false),
        ozt_tokens:toggle_revoked(TokenBeta, true),
        ?assertMatch(ok, make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuthAlpha)),
        ?assertMatch(?ERROR_TOKEN_REVOKED, make_request_with_random_context(RequestSpec, EligibleSubject, ClientAuthBeta))
    end, RequestSpec#request_spec.eligible_subjects).


check_token_caveats_handling(RequestSpec) ->
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            lists:foreach(fun(_) ->
                RequestContext = randomize_request_context(EligibleSubject),
                RandCorrectCaveats = lists_utils:random_sublist(gen_correct_caveats(RequestSpec, RequestContext)),
                RandUnverifiedCaveats = lists_utils:random_sublist(gen_unverified_caveats(RequestSpec, RequestContext)),
                ClientAuth = gen_client_auth(EligibleSubject, Persistence, RandCorrectCaveats ++ RandUnverifiedCaveats),
                Result = make_request(RequestSpec, RequestContext, ClientAuth),
                case RandUnverifiedCaveats of
                    [] ->
                        ?assertMatch(ok, Result);
                    _ ->
                        ?assertMatch(?ERROR_TOKEN_CAVEAT_UNVERIFIED(_), Result),
                        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(UnverifiedCaveat) = Result,
                        ?assert(lists:member(UnverifiedCaveat, RandUnverifiedCaveats))
                end
            end, lists:seq(1, ?CAVEAT_RANDOMIZATION_REPEATS))
        end, RequestSpec#request_spec.eligible_subjects)
    end, [named, temporary]).


check_audience_token_caveats_handling(RequestSpec) ->
    lists:foreach(fun(Persistence) ->
        lists:foreach(fun(EligibleSubject) ->
            lists:foreach(fun(_) ->
                % Check only audiences that can present an audience token
                EligibleAudienceTypes = eligible_audience_types(EligibleSubject),
                ValidAudienceTokenHolders = EligibleAudienceTypes -- [?OZ_WORKER, ?OZ_PANEL],
                Audience = set_up_audience(EligibleSubject, lists_utils:random_element(ValidAudienceTokenHolders)),
                RequestContext = randomize_request_context(EligibleSubject, Audience),
                ClientAuth = gen_client_auth(EligibleSubject, Persistence),

                % Audience tokens cannot have an audience and cannot allow data access caveats
                AudienceRequestContext = RequestContext#request_context{
                    subject = case Audience of
                        ?AUD(user, UserId) -> ?SUB(user, UserId);
                        ?AUD(?OP_WORKER, PrId) -> ?SUB(?ONEPROVIDER, PrId);
                        ?AUD(?OP_PANEL, PrId) -> ?SUB(?ONEPROVIDER, PrId)
                    end,
                    is_audience_token = true,
                    audience = undefined,
                    data_access_caveats_policy = disallow_data_access_caveats,
                    provider_for_gs_with_auth_override = not_applicable
                },
                RandCorrectCaveats = lists_utils:random_sublist(gen_correct_caveats(RequestSpec, AudienceRequestContext)),
                RandUnverifiedCaveats = lists_utils:random_sublist(gen_unverified_caveats(RequestSpec, AudienceRequestContext)),
                AudienceToken = create_audience_token_if_applicable(
                    Audience, lists_utils:shuffle(RandCorrectCaveats ++ RandUnverifiedCaveats)
                ),

                AvailableRequestMethods = available_methods_for_ctx(RequestContext),
                Result = case lists_utils:random_element(AvailableRequestMethods) of
                    logic ->
                        request_via_logic_with_audience_token(
                            RequestSpec, RequestContext, ClientAuth, AudienceToken
                        );
                    rest ->
                        request_via_rest_with_audience_token(
                            RequestSpec, ClientAuth, AudienceToken
                        );
                    op_gs_with_override ->
                        request_via_op_gs_with_override_and_audience_token(
                            RequestSpec, RequestContext, ClientAuth, AudienceToken
                        )
                end,
                case RandUnverifiedCaveats of
                    [] ->
                        ?assertMatch(ok, Result);
                    _ ->
                        ?assertMatch(?ERROR_BAD_AUDIENCE_TOKEN(?ERROR_TOKEN_CAVEAT_UNVERIFIED(_)), Result),
                        ?ERROR_BAD_AUDIENCE_TOKEN(?ERROR_TOKEN_CAVEAT_UNVERIFIED(UnverifiedCaveat)) = Result,
                        ?assert(lists:member(UnverifiedCaveat, RandUnverifiedCaveats))
                end
            end, lists:seq(1, ?CAVEAT_RANDOMIZATION_REPEATS))
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
        ?assertMatch(?ERROR_TOKEN_INVALID, make_request(RequestSpec, RequestContext, ClientAuth))
    end, RequestsToCheck).


%%%===================================================================
%%% Helper functions
%%%===================================================================

all_subject_types() -> [
    user, ?ONEPROVIDER, nobody
].


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
    AvailableRequestMethods = available_methods_for_ctx(RequestContext),
    case lists_utils:random_element(AvailableRequestMethods) of
        logic -> request_via_logic(RequestSpec, RequestContext, ClientAuth);
        rest -> request_via_rest(RequestSpec, RequestContext, ClientAuth);
        gs -> request_via_gs(RequestSpec, RequestContext, ClientAuth);
        op_gs_with_override -> request_via_op_gs_with_override(RequestSpec, RequestContext, ClientAuth)
    end.


% Only some request methods can yield an auth context with specified IP,
% interface, subject, audience and data access caveats policy:
%   * logic accepts any context as it is injected
%   * oz-worker's REST and GraphSync endpoints automatically set the policy to
%       disallow_data_access_caveats
%   * Oneprovider can use the auth_override as user on the GraphSync channel to
%       request authorization in requested context, providing arbitrary context:
%       IP, interface, audience and data access caveats policy
%   * allow_data_access_caveats policy and oneclient interface can appear only
%       on Oneprovider's GraphSync channel with auth_override
%   * undefined IP address can only be achieved in logic calls
% Returns one of: logic | rest | gs | op_gs_with_override
available_methods_for_ctx(RequestContext) ->
    PeerIp = RequestContext#request_context.ip,
    Interface = RequestContext#request_context.interface,
    ?SUB(SubjectType) = RequestContext#request_context.subject,
    AudienceType = case RequestContext#request_context.audience of
        undefined -> undefined;
        ?AUD(Type, _) -> Type
    end,
    DataAccessCaveatsPolicy = RequestContext#request_context.data_access_caveats_policy,
    case {PeerIp, Interface} of
        {undefined, _} -> [logic];
        {_, undefined} -> [logic];
        {_, rest} -> methods_for_rest_ctx(SubjectType, AudienceType, DataAccessCaveatsPolicy);
        {_, oneclient} -> methods_for_oneclient_ctx(SubjectType, AudienceType, DataAccessCaveatsPolicy);
        {_, graphsync} -> methods_for_graphsync_ctx(SubjectType, AudienceType, DataAccessCaveatsPolicy)
    end.


methods_for_rest_ctx(_, ?OZ_PANEL, _) -> [logic];
methods_for_rest_ctx(?ONEPROVIDER, _, allow_data_access_caveats) -> [logic];
methods_for_rest_ctx(?ONEPROVIDER, _, disallow_data_access_caveats) -> [logic, rest];
methods_for_rest_ctx(nobody, _, allow_data_access_caveats) -> [logic];
methods_for_rest_ctx(nobody, _, disallow_data_access_caveats) -> [logic, rest];
methods_for_rest_ctx(user, user, allow_data_access_caveats) -> [logic, op_gs_with_override];
methods_for_rest_ctx(user, user, disallow_data_access_caveats) -> [logic, rest, op_gs_with_override];
methods_for_rest_ctx(user, ?OP_WORKER, allow_data_access_caveats) -> [logic, op_gs_with_override];
methods_for_rest_ctx(user, ?OP_WORKER, disallow_data_access_caveats) -> [logic, rest, op_gs_with_override];
methods_for_rest_ctx(user, _, allow_data_access_caveats) -> [logic];
methods_for_rest_ctx(user, _, disallow_data_access_caveats) -> [logic, rest].


methods_for_oneclient_ctx(user, user, _) -> [logic, op_gs_with_override];
methods_for_oneclient_ctx(user, ?OP_WORKER, _) -> [logic, op_gs_with_override];
methods_for_oneclient_ctx(_, _, _) -> [logic].


methods_for_graphsync_ctx(_, undefined, allow_data_access_caveats) -> [logic];
methods_for_graphsync_ctx(_, undefined, disallow_data_access_caveats) -> [logic, gs];
methods_for_graphsync_ctx(_, ?OZ_WORKER, allow_data_access_caveats) -> [logic];
methods_for_graphsync_ctx(_, ?OZ_WORKER, disallow_data_access_caveats) -> [logic, gs];
methods_for_graphsync_ctx(user, user, _) -> [logic, op_gs_with_override];
methods_for_graphsync_ctx(user, ?OP_WORKER, _) -> [logic, op_gs_with_override];
methods_for_graphsync_ctx(_, _, _) -> [logic].


request_via_logic(RequestSpec, RequestContext, ClientAuth) ->
    AuthenticateResult = case ClientAuth of
        nobody ->
            {true, ?NOBODY};
        {token, Token} ->
            AuthCtx = ozt_tokens:build_auth_ctx([
                RequestContext#request_context.interface,
                RequestContext#request_context.ip,
                RequestContext#request_context.audience,
                RequestContext#request_context.data_access_caveats_policy
            ]),
            ozt_tokens:authenticate(Token, AuthCtx)
    end,

    case AuthenticateResult of
        {error, _} = Error ->
            Error;
        {true, Auth} ->
            {Module, Function, Args} = RequestSpec#request_spec.logic_call_args,
            ArgsWithAuth = lists:map(fun
                (auth) -> Auth;
                (Arg) -> Arg
            end, Args),
            check_success(ozt:rpc(Module, Function, ArgsWithAuth))
    end.

request_via_logic_with_audience_token(RequestSpec, RequestContext, ClientAuth, AudienceToken) ->
    case AudienceToken of
        undefined ->
            request_via_logic(RequestSpec, RequestContext, ClientAuth);
        _ ->
            AuthCtx = ozt_tokens:build_auth_ctx([
                RequestContext#request_context.interface,
                RequestContext#request_context.ip
            ]),
            case ozt_tokens:verify_audience_token(AudienceToken, AuthCtx) of
                {ok, Audience} ->
                    ?assertEqual(Audience, RequestContext#request_context.audience),
                    request_via_logic(RequestSpec, RequestContext, ClientAuth);
                {error, _} = Error ->
                    Error
            end
    end.


request_via_rest(RequestSpec, RequestContext, ClientAuth) ->
    AudienceToken = create_audience_token_if_applicable(RequestContext#request_context.audience),
    request_via_rest_with_audience_token(RequestSpec, ClientAuth, AudienceToken).

request_via_rest_with_audience_token(RequestSpec, ClientAuth, AudienceToken) ->
    #request_spec{rest_call_args = {Method, UrnTokens, DataJson}} = RequestSpec,
    check_success(ozt_http:rest_call(ClientAuth, AudienceToken, Method, UrnTokens, DataJson)).


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
    AudienceToken = case create_audience_token_if_applicable(RequestContext#request_context.audience) of
        undefined -> undefined;
        Token -> ozt_tokens:ensure_serialized(Token)
    end,
    request_via_op_gs_with_override_and_audience_token(RequestSpec, RequestContext, ClientAuth, AudienceToken).

request_via_op_gs_with_override_and_audience_token(RequestSpec, RequestContext, ClientAuth, AudienceToken) ->
    ProviderId = RequestContext#request_context.provider_for_gs_with_auth_override,
    OneproviderToken = ozt_tokens:create(temporary, ?SUB(?ONEPROVIDER, ProviderId)),
    AuthOverride = #auth_override{
        client_auth = ozt_gs:normalize_client_auth(ClientAuth),
        interface = RequestContext#request_context.interface,
        peer_ip = RequestContext#request_context.ip,
        audience_token = AudienceToken,
        data_access_caveats_policy = RequestContext#request_context.data_access_caveats_policy
    },
    request_via_gs(RequestSpec, oneprovider, {token, OneproviderToken}, AuthOverride).


check_success(ok) -> ok;
check_success({ok, _}) -> ok;
check_success({error, _} = Error) -> Error.


create_audience_token_if_applicable(Subject) ->
    create_audience_token_if_applicable(Subject, []).

create_audience_token_if_applicable(?AUD(user, UserId), Caveats) ->
    UserToken = ozt_tokens:create(temporary, ?SUB(user, UserId), ?ACCESS_TOKEN, Caveats),
    ozt_tokens:ensure_serialized(UserToken);
create_audience_token_if_applicable(?AUD(?OP_WORKER, PrId), Caveats) ->
    ProviderToken = ozt_tokens:create(temporary, ?SUB(?ONEPROVIDER, PrId), ?ACCESS_TOKEN, Caveats),
    tokens:build_service_access_token(?OP_WORKER, ozt_tokens:ensure_serialized(ProviderToken));
create_audience_token_if_applicable(?AUD(?OP_PANEL, PrId), Caveats) ->
    ProviderToken = ozt_tokens:create(temporary, ?SUB(?ONEPROVIDER, PrId), ?ACCESS_TOKEN, Caveats),
    tokens:build_service_access_token(?OP_PANEL, ozt_tokens:ensure_serialized(ProviderToken));
create_audience_token_if_applicable(_, _) ->
    undefined.


randomize_request_context(Subject) ->
    Audience = case rand:uniform(2) of
        1 -> undefined;
        2 -> set_up_audience(Subject, lists_utils:random_element(eligible_audience_types(Subject)))
    end,
    randomize_request_context(Subject, Audience).

randomize_request_context(Subject, Audience) ->
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
    DataAccessCaveatsPolicy = lists_utils:random_element([
        % ~ 1/4 of test repeats should allow data access caveats
        disallow_data_access_caveats, disallow_data_access_caveats, disallow_data_access_caveats,
        allow_data_access_caveats
    ]),
    ProviderForGsWithAuthOverride = case Subject of
        ?SUB(user, UserId) ->
            case ozt_users:get_eff_providers(UserId) of
                [] -> ozt_providers:create_as_support_for_user(UserId);
                [ProviderId | _] -> ProviderId
            end;
        _ ->
            not_applicable
    end,
    OneclientInterfaceEligible = case Audience of
        ?AUD(user, _) -> true;
        ?AUD(?OP_WORKER, _) -> true;
        _ -> false
    end,
    EligibleInterfaces = case OneclientInterfaceEligible of
        true -> cv_interface:valid_interfaces();
        false -> cv_interface:valid_interfaces() -- [oneclient]
    end,
    #request_context{
        subject = Subject,
        is_audience_token = false,
        current_timestamp = ozt:cluster_time_seconds(),
        interface = lists_utils:random_element([undefined | EligibleInterfaces]),
        ip = IP,
        asn = Asn,
        country = Country,
        regions = Regions,
        audience = Audience,
        data_access_caveats_policy = DataAccessCaveatsPolicy,
        provider_for_gs_with_auth_override = ProviderForGsWithAuthOverride
    }.


eligible_audience_types(?SUB(nobody)) ->
    [user, ?OZ_WORKER];
eligible_audience_types(?SUB(user)) ->
    [user, ?OZ_WORKER, ?OZ_PANEL, ?OP_WORKER, ?OP_PANEL];
eligible_audience_types(?SUB(?ONEPROVIDER)) ->
    [user, ?OZ_WORKER, ?OP_WORKER].


% Substantiate an audience of given type. If needed, create required relations
% between the subject and the audience.
set_up_audience(_Subject, undefined) ->
    undefined;
set_up_audience(_Subject, user) ->
    ?AUD(user, ozt_users:create());
set_up_audience(_Subject, ?OZ_WORKER) ->
    ?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID);
set_up_audience(?SUB(user, UserId), ?OZ_PANEL) ->
    ozt_clusters:ensure_member(?ONEZONE_CLUSTER_ID, UserId),
    ?AUD(?OZ_PANEL, ?ONEZONE_CLUSTER_ID);
set_up_audience(?SUB(user, UserId), ?OP_WORKER) ->
    ProviderId = ozt_providers:create_as_support_for_user(UserId),
    ?AUD(?OP_WORKER, ProviderId);
set_up_audience(?SUB(?ONEPROVIDER), ?OP_WORKER) ->
    OtherProviderId = ozt_providers:create(),
    ?AUD(?OP_WORKER, OtherProviderId);
set_up_audience(?SUB(user, UserId), ?OP_PANEL) ->
    ProviderId = ozt_providers:create_for_admin_user(UserId),
    ?AUD(?OP_PANEL, ProviderId).


gen_correct_caveats(RequestSpec, RC) -> lists:flatten([
    #cv_time{valid_until = RC#request_context.current_timestamp + 10},
    case RC#request_context.audience of
        undefined -> [];
        _ -> gen_correct_audience_caveat(RC)
    end,
    case RC#request_context.ip of
        undefined -> [];
        _ -> gen_correct_ip_based_caveats(RC)
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
    case RC#request_context.is_audience_token of
        true -> []; % Audience tokens cannot have API caveats
        false -> gen_correct_api_caveat(RequestSpec)
    end,
    gen_correct_data_access_caveats(RequestSpec, RC)
]).


gen_unverified_caveats(RequestSpec, RC) -> lists:flatten([
    #cv_time{valid_until = RC#request_context.current_timestamp - 1},
    #cv_authorization_none{},
    gen_unverified_audience_caveat(RC),
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
            % oneclient interface caveat causes API limitations
            case RequestSpec#request_spec.available_with_data_access_caveats of
                false -> cv_interface:valid_interfaces();
                true -> cv_interface:valid_interfaces() -- [oneclient]
            end;
        Interface ->
            cv_interface:valid_interfaces() -- [Interface]
    end)},
    case RC#request_context.is_audience_token of
        true -> gen_correct_api_caveat(RequestSpec); % Audience tokens cannot have API caveats
        false -> gen_unverified_api_caveat(RequestSpec)
    end,
    gen_unverified_data_access_caveats(RequestSpec, RC)
]).


gen_correct_audience_caveat(RequestContext) ->
    CorrectAudiences = gen_correct_audiences(RequestContext),
    UnverifiedAudiences = gen_unverified_audiences(RequestContext),
    #cv_audience{whitelist = lists_utils:shuffle(lists:flatten([
        lists_utils:random_sublist(CorrectAudiences, 1, all),
        lists_utils:random_sublist(UnverifiedAudiences)
    ]))}.

gen_unverified_audience_caveat(Rc) ->
    UnverifiedAudiences = gen_unverified_audiences(Rc),
    #cv_audience{whitelist = lists_utils:shuffle(
        lists_utils:random_sublist(UnverifiedAudiences, 1, all)
    )}.

gen_correct_audiences(#request_context{audience = ?AUD(user, UserId)}) ->
    GroupId = ozt_users:create_group_for(UserId),
    [
        ?AUD(user, UserId),
        ?AUD(user, <<"*">>),
        ?AUD(group, GroupId),
        ?AUD(group, <<"*">>)
    ];
gen_correct_audiences(#request_context{audience = ?AUD(Type, Id)}) ->
    [
        ?AUD(Type, Id),
        ?AUD(Type, <<"*">>)
    ].

gen_unverified_audiences(#request_context{subject = Subject, audience = RequestAudience}) ->
    EligibleTypes = eligible_audience_types(Subject),
    UnverifiedTypes = case RequestAudience of
        undefined -> EligibleTypes -- [?OZ_WORKER]; % If not specified, audience defaults to OZ_WORKER
        ?AUD(Type, _) -> EligibleTypes -- [Type]
    end,
    GroupId = ozt_groups:create(),
    [?AUD(group, GroupId)] ++ lists:map(fun(AudienceType) ->
        Audience = set_up_audience(Subject, AudienceType),
        case rand:uniform(2) of
            1 -> Audience;
            2 -> Audience#audience{id = <<"*">>}
        end
    end, UnverifiedTypes).


gen_forbidden_audiences(?SUB(user, UserId)) ->
    ProviderId = ozt_providers:create(),
    ozt_clusters:ensure_not_a_member(?ONEZONE_CLUSTER_ID, UserId),
    [
        ?AUD(?OZ_PANEL, ?ONEZONE_CLUSTER_ID),
        ?AUD(?OP_WORKER, ProviderId),
        ?AUD(?OP_PANEL, ProviderId)
    ];
gen_forbidden_audiences(?SUB(?ONEPROVIDER)) ->
    OtherProviderId = ozt_providers:create(),
    [
        ?AUD(?OZ_PANEL, ?ONEZONE_CLUSTER_ID),
        ?AUD(?OP_PANEL, OtherProviderId)
    ].


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
    ServiceSpec = lists_utils:random_element([all, ?OZ_WORKER]),
    OperationSpec = lists_utils:random_element([all, Operation]),
    {ServiceSpec, OperationSpec, #gri_pattern{
        type = lists_utils:random_element([Type, '*']),
        id = lists_utils:random_element([Id, '*']),
        aspect = lists_utils:random_element([Aspect] ++ case Aspect of
            {A, B} -> ['*', {'*', B}, {A, '*'}, {'*', '*'}];
            _ -> ['*']
        end),
        scope = lists_utils:random_element([Scope, '*'])
    }}.

gen_unverified_api_matchspec(Operation, GRI = #gri{type = Type, aspect = Aspect, scope = Scope}) ->
    % Randomize which fields should be unverified (at least one)
    UnverifiedFields = lists_utils:random_sublist([service, operation, type, id, aspect, scope], 1, all),
    {CorrectServiceSpec, CorrectOperationSpec, CorrectGriPattern} = gen_correct_api_matchspec(Operation, GRI),

    ServiceSpec = case lists:member(service, UnverifiedFields) of
        true -> lists_utils:random_element([?OZ_PANEL, ?OP_WORKER, ?OP_PANEL]);
        false -> CorrectServiceSpec
    end,

    OperationSpec = case lists:member(operation, UnverifiedFields) of
        true -> lists_utils:random_element([create, get, update, delete] -- [Operation]);
        false -> CorrectOperationSpec
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
            true -> lists_utils:random_element([instance, users, eff_groups] -- [Aspect]);
            false -> CorrectGriPattern#gri_pattern.aspect
        end,
        scope = case lists:member(scope, UnverifiedFields) of
            true -> lists_utils:random_element([private, protected, shared, public] -- [Scope]);
            false -> CorrectGriPattern#gri_pattern.scope
        end
    },
    {ServiceSpec, OperationSpec, GriPattern}.


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
