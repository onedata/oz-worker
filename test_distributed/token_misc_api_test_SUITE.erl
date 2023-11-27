%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% CT tests concerning miscellaneous token API
%%% (operations other than named/temporary token API).
%%% @end
%%%-------------------------------------------------------------------
-module(token_misc_api_test_SUITE).
-author("Lukasz Opiola").

-include("ozt.hrl").

%% API
-export([
    groups/0, all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    examine/1,
    confine/1,
    verify_access_token/1,
    verify_identity_token/1,
    verify_invite_token/1,
    infer_access_token_scope/1,

    % sequential tests
    create_offline_user_access_token/1
]).

groups() -> [
    {parallel_tests, [parallel], [
        examine,
        confine,
        verify_access_token,
        verify_identity_token,
        verify_invite_token,
        infer_access_token_scope
    ]},
    {sequential_tests, [sequential], [
        create_offline_user_access_token
    ]}
].

all() -> [
    {group, parallel_tests}
].


% Context used for token verification with IP with all context's parameters
%    any       - any value should be accepted for the parameter
%    undefined - the parameter is not known (e.g. unknown interface),
%                will fail against a specific interface caveat
%    <value>   - specific parameter value
-record(access_token_ctx, {
    peer_ip = any :: any | undefined | ip_utils:ip(),
    interface = any :: any | undefined | cv_interface:interface(),
    service = any :: any | undefined | aai:service_spec(),
    consumer = any :: any | undefined | aai:consumer_spec(),
    allow_data_access_caveats = any :: any | undefined | boolean()
}).


%%%===================================================================
%%% Test functions
%%%===================================================================

examine(_Config) ->
    User = ozt_users:create(),
    ProviderAdmin = ozt_users:create(),
    Provider = ozt_providers:create_for_admin_user(ProviderAdmin),
    AllClients = [{user, User}, {provider, Provider}, {user, ProviderAdmin}],

    TimeCaveat = #cv_time{valid_until = ozt:timestamp_seconds() + 3600},
    TokenAlpha = create_user_temporary_token(User, ?ACCESS_TOKEN, [TimeCaveat]),
    examine_base(AllClients, TokenAlpha, #{
        <<"onezoneDomain">> => ozt:get_domain(),
        <<"id">> => TokenAlpha#token.id,
        <<"persistence">> => <<"temporary">>,
        <<"subject">> => ?SUB(user, User),
        <<"type">> => ?ACCESS_TOKEN,
        <<"caveats">> => [TimeCaveat]
    }),

    TokenBeta = create_provider_named_token(
        Provider, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, Provider), [
            #cv_consumer{whitelist = [?SUB(user, <<"123456789">>)]},
            #cv_ip{whitelist = [{{181, 115, 16, 8}, 32}, {{181, 115, 16, 9}, 32}]},
            #cv_asn{whitelist = [854]},
            #cv_country{type = blacklist, list = [<<"PL">>, <<"PT">>, <<"ES">>]},
            #cv_region{type = whitelist, list = [<<"EU">>]}
        ]
    ),
    examine_base(AllClients, TokenBeta, #{
        <<"onezoneDomain">> => ozt:get_domain(),
        <<"id">> => TokenBeta#token.id,
        <<"persistence">> => <<"named">>,
        <<"subject">> => ?SUB(?ONEPROVIDER, Provider),
        <<"type">> => ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, Provider),
        <<"caveats">> => [
            #cv_consumer{whitelist = [?SUB(user, <<"123456789">>)]},
            #cv_ip{whitelist = [{{181, 115, 16, 8}, 32}, {{181, 115, 16, 9}, 32}]},
            #cv_asn{whitelist = [854]},
            #cv_country{type = blacklist, list = [<<"PL">>, <<"PT">>, <<"ES">>]},
            #cv_region{type = whitelist, list = [<<"EU">>]}
        ]
    }).

examine_base(AllClients, Token, ExpResult) ->
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), #api_test_spec{
        client_spec = #client_spec{
            correct = AllClients ++ [
                nobody,
                root
            ]
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = examine,
            args = [auth, data],
            expected_result = ?OK_MAP(ExpResult)
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/tokens/examine">>,
            expected_code = ?HTTP_200_OK,
            expected_body = ExpResult#{
                <<"subject">> => aai:subject_to_json(maps:get(<<"subject">>, ExpResult)),
                <<"type">> => token_type:to_json(maps:get(<<"type">>, ExpResult)),
                <<"caveats">> => [caveats:to_json(C) || C <- maps:get(<<"caveats">>, ExpResult)]
            }
        },
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{
                <<"token">> => [ozt_tokens:ensure_serialized(Token)]
            },
            bad_values = [
                {<<"token">>, <<"1234">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)}
            ]
        }
    })).


confine(_Config) ->
    User = ozt_users:create(),
    ProviderAdmin = ozt_users:create(),
    Provider = ozt_providers:create_for_admin_user(ProviderAdmin),
    AllClients = [{user, User}, {provider, Provider}, {user, ProviderAdmin}],

    InitialCaveatsAlpha = [
        #cv_time{valid_until = ozt:timestamp_seconds() + 3600}
    ],
    TokenAlpha = create_user_temporary_token(User, ?ACCESS_TOKEN, InitialCaveatsAlpha),
    confine_combinations(AllClients, TokenAlpha, InitialCaveatsAlpha, [
        #cv_scope{scope = identity_token},
        #cv_service{whitelist = [?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]},
        #cv_interface{interface = rest},
        #cv_api{whitelist = [{oz_worker, get, ?GRI_PATTERN(od_space, <<"*">>, <<"*">>, private)}]},
        #cv_data_readonly{},
        #cv_data_path{whitelist = [<<"/a/b/c/d">>]},
        #cv_data_objectid{whitelist = [?RAND_OBJECTID, ?RAND_OBJECTID, ?RAND_OBJECTID]}
    ]),

    InitialCaveatsBeta = [
        #cv_time{valid_until = ozt:timestamp_seconds() + 3600},
        #cv_ip{whitelist = [{{181, 115, 16, 8}, 32}, {{181, 115, 16, 9}, 32}]},
        #cv_country{type = blacklist, list = [<<"PL">>, <<"PT">>, <<"ES">>]},
        #cv_region{type = whitelist, list = [<<"EU">>]}
    ],
    TokenBeta = create_provider_named_token(
        Provider, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, Provider), InitialCaveatsBeta
    ),
    confine_combinations(AllClients, TokenBeta, InitialCaveatsBeta, [
        #cv_time{valid_until = ozt:timestamp_seconds() + 1800},
        #cv_consumer{whitelist = [?SUB(user, <<"123456789">>), ?SUB(group, <<"abderg">>)]},
        #cv_asn{whitelist = [854]}
    ]).

confine_combinations(AllClients, TokenAlpha, InitialCaveats, CaveatsToAdd) ->
    Combinations = [lists:sublist(CaveatsToAdd, L) || L <- lists:seq(1, length(CaveatsToAdd))],
    lists:foreach(fun(Combination) ->
        confine_base(AllClients, TokenAlpha, InitialCaveats, Combination)
    end, Combinations).

confine_base(AllClients, Token, InitialCaveats, CaveatsToAdd) ->
    VerifyFun = fun(ResultToken) ->
        ?assertEqual(InitialCaveats ++ CaveatsToAdd, tokens:get_caveats(ResultToken)),
        true
    end,

    ?assert(api_test_utils:run_tests(ozt:get_test_config(), #api_test_spec{
        client_spec = #client_spec{
            correct = AllClients ++ [
                nobody,
                root
            ]
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = confine,
            args = [auth, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/tokens/confine">>,
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Serialized}) ->
                VerifyFun(ozt_tokens:ensure_deserialized(Serialized))
            end
        },
        data_spec = #data_spec{
            required = [<<"token">>, <<"caveats">>],
            correct_values = #{
                <<"token">> => [ozt_tokens:ensure_serialized(Token)],
                <<"caveats">> => [[caveats:to_json(C) || C <- CaveatsToAdd]]
            },
            bad_values = [
                {<<"token">>, <<"1234">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"caveats">>, <<"1234">>, ?ERROR_BAD_DATA(<<"caveats">>)},
                {<<"caveats">>, 1234, ?ERROR_BAD_DATA(<<"caveats">>)},
                {<<"caveats">>, [#{<<"a">> => <<"b">>}], ?ERROR_BAD_VALUE_CAVEAT(#{<<"a">> => <<"b">>})},
                {<<"caveats">>, [#{<<"type">> => <<"unknownCaveat">>}],
                    ?ERROR_BAD_VALUE_CAVEAT(#{<<"type">> => <<"unknownCaveat">>})}
            ]
        }
    })).


verify_access_token(_Config) ->
    User = ozt_users:create(),
    ProviderAdmin = ozt_users:create(),
    SessionId = ozt_http:simulate_login(ProviderAdmin),
    Provider = ozt_providers:create_for_admin_user(ProviderAdmin),
    Space = ozt_users:create_space_for(User),
    ozt_providers:support_space(Provider, Space),
    ozt_clusters:add_user(?ONEZONE_CLUSTER_ID, User),
    ozt:reconcile_entity_graph(),

    AllClients = [
        nobody,
        root,
        {user, User},
        {provider, Provider},
        {user, ProviderAdmin}
    ],

    TokenAlpha = create_user_temporary_token(User, ?ACCESS_TOKEN, [
        #cv_time{valid_until = ozt:timestamp_seconds() + 3600}
    ]),
    verify_access_token_base(
        AllClients, TokenAlpha, #access_token_ctx{},
        true, {?SUB(user, User), 3600}
    ),
    verify_access_token_base(
        AllClients, TokenAlpha, #access_token_ctx{interface = undefined},
        true, {?SUB(user, User), 3600}
    ),

    TokenBeta = create_provider_named_token(Provider, ?ACCESS_TOKEN, [
        #cv_time{valid_until = ozt:timestamp_seconds() + 2700}
    ]),
    verify_access_token_base(
        AllClients, TokenBeta, #access_token_ctx{interface = rest},
        true, {?SUB(?ONEPROVIDER, Provider), 2700}
    ),
    verify_access_token_base(
        AllClients, TokenBeta, #access_token_ctx{interface = graphsync},
        true, {?SUB(?ONEPROVIDER, Provider), 2700}
    ),

    TokenGamma = create_user_temporary_token(ProviderAdmin, ?ACCESS_TOKEN(SessionId), [
        #cv_time{valid_until = ozt:timestamp_seconds() + 1800},
        #cv_consumer{whitelist = [?SUB(?ONEPROVIDER, Provider)]}
    ]),
    verify_access_token_base(
        AllClients, TokenGamma, #access_token_ctx{consumer = ?SUB(?ONEPROVIDER, Provider)},
        true, {?SUB(user, ProviderAdmin), 1800}
    ),
    verify_access_token_base(
        AllClients, TokenGamma, #access_token_ctx{consumer = ?SUB(?ONEPROVIDER, Provider), allow_data_access_caveats = false},
        true, {?SUB(user, ProviderAdmin), 1800}
    ),
    verify_access_token_base(
        AllClients -- [{provider, Provider}], TokenGamma, #access_token_ctx{consumer = undefined},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_consumer{whitelist = [?SUB(?ONEPROVIDER, Provider)]})
    ),
    % the consumer defaults to the authenticated client
    verify_access_token_base(
        [{provider, Provider}], TokenGamma, #access_token_ctx{consumer = undefined},
        true, {?SUB(user, ProviderAdmin), 1800}
    ),

    TokenDelta = create_user_named_token(User, ?ACCESS_TOKEN, [
        #cv_time{valid_until = ozt:timestamp_seconds() - 1}
    ]),
    verify_access_token_base(
        AllClients, TokenDelta, #access_token_ctx{interface = oneclient, allow_data_access_caveats = true},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_time{valid_until = ozt:timestamp_seconds() - 1}
        )
    ),
    verify_access_token_base(
        AllClients, TokenDelta, #access_token_ctx{interface = rest},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_time{valid_until = ozt:timestamp_seconds() - 1}
        )
    ),

    TokenZeta = create_user_temporary_token(User, ?ACCESS_TOKEN, [
        #cv_interface{interface = rest},
        #cv_service{whitelist = [?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID), ?SERVICE(?OP_WORKER, Provider)]}
    ]),
    verify_access_token_base(
        AllClients, TokenZeta, #access_token_ctx{interface = undefined},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_interface{interface = rest})
    ),
    verify_access_token_base(
        AllClients, TokenZeta, #access_token_ctx{interface = graphsync},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_interface{interface = rest})
    ),
    verify_access_token_base(
        AllClients, TokenZeta, #access_token_ctx{interface = oneclient},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_interface{interface = rest}
        )
    ),
    verify_access_token_base(
        AllClients, TokenZeta, #access_token_ctx{interface = rest, service = ?SERVICE(?OP_PANEL, Provider)},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_service{whitelist = [?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID), ?SERVICE(?OP_WORKER, Provider)]}
        )
    ),
    verify_access_token_base(
        AllClients, TokenZeta, #access_token_ctx{interface = rest, service = ?SERVICE(?OP_WORKER, Provider)},
        true, {?SUB(user, User), ?DEFAULT_TEMP_CAVEAT_TTL}
    ),
    % Service defaults to ?OZ_WORKER if undefined
    verify_access_token_base(
        AllClients, TokenZeta, #access_token_ctx{interface = rest, service = undefined},
        true, {?SUB(user, User), ?DEFAULT_TEMP_CAVEAT_TTL}
    ),

    TokenKappa = create_user_named_token(User, ?ACCESS_TOKEN, [
        #cv_service{whitelist = [?SERVICE(?OZ_PANEL, ?ONEZONE_CLUSTER_ID)]}
    ]),
    % It is not possible to create a token for OZ_PANEL and hence verify it
    % using the API
    verify_access_token_base(
        AllClients, TokenKappa, #access_token_ctx{},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_service{whitelist = [?SERVICE(?OZ_PANEL, ?ONEZONE_CLUSTER_ID)]}
        )
    ),
    % However, the token is verifiable using internal logic (which is RPC called from onepanel)
    ?assertMatch(
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_service{whitelist = [?SERVICE(?OZ_PANEL, ?ONEZONE_CLUSTER_ID)]}),
        ozt:rpc(token_auth, verify_access_token, [TokenKappa, #auth_ctx{}])
    ),
    ?assertMatch(
        {ok, ?USER(User)},
        ozt:rpc(token_auth, verify_access_token, [TokenKappa, #auth_ctx{service = ?SERVICE(?OZ_PANEL, ?ONEZONE_CLUSTER_ID)}])
    ),

    TokenSigma = create_user_temporary_token(ProviderAdmin, ?INVITE_TOKEN(?USER_JOIN_CLUSTER, Provider)),
    verify_access_token_base(
        AllClients, TokenSigma, #access_token_ctx{},
        false, ?ERROR_NOT_AN_ACCESS_TOKEN(?INVITE_TOKEN(?USER_JOIN_CLUSTER, Provider))
    ),

    TokenEpsilon = create_provider_temporary_token(Provider, ?IDENTITY_TOKEN),
    verify_access_token_base(
        AllClients, TokenEpsilon, #access_token_ctx{},
        false, ?ERROR_NOT_AN_ACCESS_TOKEN(?IDENTITY_TOKEN)
    ),

    TokenTheta = create_provider_temporary_token(Provider, ?ACCESS_TOKEN, [
        #cv_ip{whitelist = [{{134, 93, 0, 0}, 16}]}
    ]),
    verify_access_token_base(
        AllClients, TokenTheta, #access_token_ctx{peer_ip = undefined},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_ip{whitelist = [{{134, 93, 0, 0}, 16}]})
    ),
    verify_access_token_base(
        AllClients, TokenTheta, #access_token_ctx{peer_ip = <<"133.93.1.182">>},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_ip{whitelist = [{{134, 93, 0, 0}, 16}]})
    ),
    verify_access_token_base(
        AllClients, TokenTheta, #access_token_ctx{peer_ip = <<"134.93.7.18">>},
        true, {?SUB(?ONEPROVIDER, Provider), ?DEFAULT_TEMP_CAVEAT_TTL}
    ),
    verify_access_token_base(
        AllClients, TokenTheta, #access_token_ctx{peer_ip = <<"134.93.1.206">>},
        true, {?SUB(?ONEPROVIDER, Provider), ?DEFAULT_TEMP_CAVEAT_TTL}
    ),

    TokenOmega = create_provider_named_token(Provider, ?ACCESS_TOKEN, [
        #cv_ip{whitelist = [{get_testmaster_ip(), 18}]}
    ]),
    verify_access_token_base(
        AllClients, TokenOmega, #access_token_ctx{peer_ip = <<"133.93.1.182">>},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_ip{whitelist = [{get_testmaster_ip(), 18}]})
    ),
    % the IP defaults to the requesting client's IP
    verify_access_token_base(
        AllClients, TokenOmega, #access_token_ctx{peer_ip = undefined},
        true, {?SUB(?ONEPROVIDER, Provider), undefined}
    ),

    TokenTau = create_user_named_token(User, ?ACCESS_TOKEN, [
        #cv_data_readonly{}
    ]),
    verify_access_token_base(
        AllClients, TokenTau, #access_token_ctx{allow_data_access_caveats = false},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_data_readonly{})
    ),
    verify_access_token_base(
        AllClients, TokenTau, #access_token_ctx{allow_data_access_caveats = true},
        true, {?SUB(user, User), undefined}
    ).


verify_access_token_base(AllClients, Token, AccessTokenCtx, ShouldSucceed, ExpResult) ->
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), #api_test_spec{
        client_spec = #client_spec{
            correct = AllClients,
            unauthorized = [],
            forbidden = []
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = verify_access_token,
            args = fun prepare_token_verification_logic_args/3,
            expected_result = case ShouldSucceed of
                true ->
                    ?OK_TERM(fun(#{<<"subject">> := Subject, <<"ttl">> := TTL}) ->
                        {ExpSubject, ExpTTL} = ExpResult,
                        ?assertEqual(Subject, ExpSubject),
                        ?assertEqual(TTL, ExpTTL),
                        true
                    end);
                false ->
                    ?ERROR_REASON(ExpResult)
            end
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/tokens/verify_access_token">>,
            expected_code = case ShouldSucceed of
                true -> ?HTTP_200_OK;
                false -> errors:to_http_code(ExpResult)
            end,
            expected_body = case ShouldSucceed of
                true ->
                    fun(#{<<"subject">> := Subject, <<"ttl">> := TTL}) ->
                        {ExpSubject, ExpTTL} = ExpResult,
                        ?assertEqual(Subject, aai:subject_to_json(ExpSubject)),
                        ?assertEqual(TTL, utils:undefined_to_null(ExpTTL)),
                        true
                    end;
                false ->
                    undefined
            end
        },
        data_spec = access_token_verification_data_spec(Token, AccessTokenCtx)
    })).


% Context used for token verification with IP with all context's parameters
%    any       - any value should be accepted for the parameter
%    undefined - the parameter is not known (e.g. unknown interface),
%                will fail against a specific interface caveat
%    <value>   - specific parameter value
-record(identity_token_ctx, {
    peer_ip = any :: any | undefined | ip_utils:ip(),
    interface = any :: any | undefined | cv_interface:interface(),
    consumer = any :: any | undefined | aai:consumer_spec()
}).
verify_identity_token(_Config) ->
    User = ozt_users:create(),
    ProviderAdmin = ozt_users:create(),
    SessionId = ozt_http:simulate_login(ProviderAdmin),
    Provider = ozt_providers:create_for_admin_user(ProviderAdmin),
    Space = ozt_users:create_space_for(User),
    ozt_providers:support_space(Provider, Space),
    ozt_clusters:add_user(?ONEZONE_CLUSTER_ID, User),
    ozt:reconcile_entity_graph(),

    AllClients = [
        nobody,
        root,
        {user, User},
        {provider, Provider},
        {user, ProviderAdmin}
    ],

    TokenAlpha = create_user_temporary_token(User, ?IDENTITY_TOKEN, [
        #cv_time{valid_until = ozt:timestamp_seconds() + 3600}
    ]),
    verify_identity_token_base(
        AllClients, TokenAlpha, #identity_token_ctx{},
        true, {?SUB(user, User), 3600}
    ),
    verify_identity_token_base(
        AllClients, TokenAlpha, #identity_token_ctx{interface = undefined},
        true, {?SUB(user, User), 3600}
    ),

    TokenBeta = create_provider_named_token(Provider, ?IDENTITY_TOKEN, [
        #cv_time{valid_until = ozt:timestamp_seconds() + 2700}
    ]),
    verify_identity_token_base(
        AllClients, TokenBeta, #identity_token_ctx{interface = rest},
        true, {?SUB(?ONEPROVIDER, Provider), 2700}
    ),
    verify_identity_token_base(
        AllClients, TokenBeta, #identity_token_ctx{interface = graphsync},
        true, {?SUB(?ONEPROVIDER, Provider), 2700}
    ),

    TokenGamma = create_user_temporary_token(ProviderAdmin, ?IDENTITY_TOKEN, [
        #cv_time{valid_until = ozt:timestamp_seconds() + 1800},
        #cv_consumer{whitelist = [?SUB(user, User)]}
    ]),
    verify_identity_token_base(
        AllClients, TokenGamma, #identity_token_ctx{consumer = ?SUB(user, User)},
        true, {?SUB(user, ProviderAdmin), 1800}
    ),
    verify_identity_token_base(
        AllClients -- [{user, User}], TokenGamma, #identity_token_ctx{consumer = undefined},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_consumer{whitelist = [?SUB(user, User)]})
    ),
    % the consumer defaults to the authenticated client
    verify_identity_token_base(
        [{user, User}], TokenGamma, #identity_token_ctx{consumer = undefined},
        true, {?SUB(user, ProviderAdmin), 1800}
    ),

    TokenDelta = create_user_named_token(User, ?IDENTITY_TOKEN, [
        #cv_time{valid_until = ozt:timestamp_seconds() - 1}
    ]),
    verify_identity_token_base(
        AllClients, TokenDelta, #identity_token_ctx{interface = rest},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_time{valid_until = ozt:timestamp_seconds() - 1}
        )
    ),

    TokenZeta = create_user_temporary_token(User, ?IDENTITY_TOKEN, [
        #cv_interface{interface = rest}
    ]),
    verify_identity_token_base(
        AllClients, TokenZeta, #identity_token_ctx{interface = undefined},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_interface{interface = rest})
    ),
    verify_identity_token_base(
        AllClients, TokenZeta, #identity_token_ctx{interface = graphsync},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_interface{interface = rest})
    ),
    verify_identity_token_base(
        AllClients, TokenZeta, #identity_token_ctx{interface = oneclient},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_interface{interface = rest})
    ),
    verify_identity_token_base(
        AllClients, TokenZeta, #identity_token_ctx{interface = rest},
        true, {?SUB(user, User), ?DEFAULT_TEMP_CAVEAT_TTL}
    ),

    TokenSigma = create_user_temporary_token(ProviderAdmin, ?INVITE_TOKEN(?USER_JOIN_CLUSTER, Provider)),
    verify_identity_token_base(
        AllClients, TokenSigma, #identity_token_ctx{},
        false, ?ERROR_NOT_AN_IDENTITY_TOKEN(?INVITE_TOKEN(?USER_JOIN_CLUSTER, Provider))
    ),

    TokenTheta = create_provider_temporary_token(Provider, ?IDENTITY_TOKEN, [
        #cv_ip{whitelist = [{{134, 93, 0, 0}, 16}]}
    ]),
    verify_identity_token_base(
        AllClients, TokenTheta, #identity_token_ctx{peer_ip = undefined},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_ip{whitelist = [{{134, 93, 0, 0}, 16}]})
    ),
    verify_identity_token_base(
        AllClients, TokenTheta, #identity_token_ctx{peer_ip = <<"133.93.1.182">>},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_ip{whitelist = [{{134, 93, 0, 0}, 16}]})
    ),
    verify_identity_token_base(
        AllClients, TokenTheta, #identity_token_ctx{peer_ip = <<"134.93.7.18">>},
        true, {?SUB(?ONEPROVIDER, Provider), ?DEFAULT_TEMP_CAVEAT_TTL}
    ),
    verify_identity_token_base(
        AllClients, TokenTheta, #identity_token_ctx{peer_ip = <<"134.93.1.182">>},
        true, {?SUB(?ONEPROVIDER, Provider), ?DEFAULT_TEMP_CAVEAT_TTL}
    ),

    TokenTau = create_user_temporary_token(User, ?IDENTITY_TOKEN, [
        #cv_ip{whitelist = [{get_testmaster_ip(), 13}]}
    ]),
    verify_identity_token_base(
        AllClients, TokenTau, #identity_token_ctx{peer_ip = <<"133.93.1.132">>},
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_ip{whitelist = [{get_testmaster_ip(), 13}]})
    ),
    % the IP defaults to the requesting client's IP
    verify_identity_token_base(
        AllClients, TokenTau, #identity_token_ctx{peer_ip = undefined},
        true, {?SUB(user, User), ?DEFAULT_TEMP_CAVEAT_TTL}
    ),

    %% @todo VFS-6098 for backward compatibility - legacy provider access tokens should be
    %% accepted as identity tokens for backward compatibility with old providers
    TokenRho = ozt_tokens:create_legacy_access_token(?SUB(?ONEPROVIDER, Provider)),
    verify_identity_token_base(
        AllClients, TokenRho, #identity_token_ctx{},
        true, {?SUB(?ONEPROVIDER, Provider), undefined}
    ),

    TokenRhoAuthNone = ozt_tokens:confine_with_legacy_auth_none_caveat(TokenRho),
    verify_identity_token_base(
        AllClients, TokenRhoAuthNone, #identity_token_ctx{},
        true, {?SUB(?ONEPROVIDER, Provider), undefined}
    ),

    %% @todo VFS-6098 for backward compatibility - when a legacy provider is
    %% registered in a modern zone, it gets a modern token but uses it in legacy
    %% way (presenting the access token as its identity proof) - this should
    %% also be supported
    TokenOmega = create_provider_named_token(Provider, ?ACCESS_TOKEN, [
        #cv_scope{scope = identity_token}
    ]),
    verify_identity_token_base(
        AllClients, TokenOmega, #identity_token_ctx{},
        true, {?SUB(?ONEPROVIDER, Provider), undefined}
    ),

    % user access tokens should never be accepted, regardless if legacy or modern
    TokenXi = ozt_tokens:create_legacy_access_token(?SUB(user, User)),
    verify_identity_token_base(
        AllClients, TokenXi, #identity_token_ctx{},
        false, ?ERROR_NOT_AN_IDENTITY_TOKEN(?ACCESS_TOKEN)
    ),

    TokenPi = create_user_named_token(User, ?ACCESS_TOKEN, [
        #cv_scope{scope = identity_token}
    ]),
    verify_identity_token_base(
        AllClients, TokenPi, #identity_token_ctx{},
        false, ?ERROR_NOT_AN_IDENTITY_TOKEN(?ACCESS_TOKEN)
    ),

    TokenKappa = create_user_temporary_token(ProviderAdmin, ?ACCESS_TOKEN(SessionId)),
    verify_identity_token_base(
        AllClients, TokenKappa, #identity_token_ctx{},
        false, ?ERROR_NOT_AN_IDENTITY_TOKEN(?ACCESS_TOKEN(SessionId))
    ).


verify_identity_token_base(AllClients, Token, IdentityTokenCtx, ShouldSucceed, ExpResult) ->
    #identity_token_ctx{
        peer_ip = PeerIp,
        interface = Interface,
        consumer = Consumer
    } = IdentityTokenCtx,

    ConsumerToken = case Consumer of
        any ->
            User = ozt_users:create(),
            ozt_tokens:ensure_serialized(ozt_tokens:create(temporary, ?SUB(user, User), ?IDENTITY_TOKEN));
        undefined ->
            undefined;
        _ ->
            ozt_tokens:ensure_serialized(ozt_tokens:create(temporary, Consumer, ?IDENTITY_TOKEN))
    end,

    ?assert(api_test_utils:run_tests(ozt:get_test_config(), #api_test_spec{
        client_spec = #client_spec{
            correct = AllClients,
            unauthorized = [],
            forbidden = []
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = verify_identity_token,
            args = fun prepare_token_verification_logic_args/3,
            expected_result = case ShouldSucceed of
                true ->
                    ?OK_TERM(fun(#{<<"subject">> := Subject, <<"ttl">> := TTL}) ->
                        {ExpSubject, ExpTTL} = ExpResult,
                        ?assertEqual(Subject, ExpSubject),
                        ?assertEqual(TTL, ExpTTL),
                        true
                    end);
                false ->
                    ?ERROR_REASON(ExpResult)
            end
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/tokens/verify_identity_token">>,
            expected_code = case ShouldSucceed of
                true -> ?HTTP_200_OK;
                false -> errors:to_http_code(ExpResult)
            end,
            expected_body = case ShouldSucceed of
                true ->
                    fun(#{<<"subject">> := Subject, <<"ttl">> := TTL}) ->
                        {ExpSubject, ExpTTL} = ExpResult,
                        ?assertEqual(Subject, aai:subject_to_json(ExpSubject)),
                        ?assertEqual(TTL, utils:undefined_to_null(ExpTTL)),
                        true
                    end;
                false ->
                    undefined
            end
        },
        data_spec = #data_spec{
            % Peer IP, interface, service, consumer and allowDataAccessCaveats
            % flag can be required to verify a token, but generally they are
            % optional in the API.
            required = lists:flatten([
                <<"token">>,
                case PeerIp of
                    any -> [];
                    undefined -> [];
                    _ -> [<<"peerIp">>]
                end,
                case Interface of
                    any -> [];
                    undefined -> [];
                    _ -> [<<"interface">>]
                end,
                case ConsumerToken of
                    undefined -> [];
                    _ -> [<<"consumerToken">>]
                end
            ]),
            optional = lists:flatten([
                case PeerIp of
                    any -> [<<"peerIp">>];
                    undefined -> [];
                    _ -> []
                end,
                case Interface of
                    any -> [<<"interface">>];
                    undefined -> [];
                    _ -> []
                end,
                case Consumer of
                    any -> [<<"consumerToken">>];
                    _ -> []
                end
            ]),
            correct_values = #{
                <<"token">> => [ozt_tokens:ensure_serialized(Token)],
                <<"peerIp">> => case PeerIp of
                    any -> [<<"1.2.3.4">>, <<"5.6.7.8">>];
                    undefined -> [];
                    _ -> [utils:undefined_to_null(PeerIp)]
                end,
                <<"interface">> => case Interface of
                    any -> cv_interface:valid_interfaces();
                    undefined -> [];
                    _ -> [Interface]
                end,
                <<"consumerToken">> => case ConsumerToken of
                    undefined -> [];
                    _ -> [ConsumerToken]
                end
            },
            bad_values = lists:flatten([
                {<<"token">>, <<"1234">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"peerIp">>, <<"1234.6.78.19">>, ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
                {<<"peerIp">>, 1234, ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
                {<<"peerIp">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
                {<<"interface">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_ATOM(<<"interface">>)},
                {<<"interface">>, <<"graphSync">>,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"interface">>, cv_interface:valid_interfaces())},
                {<<"consumerToken">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_TOKEN(<<"consumerToken">>, ?ERROR_BAD_TOKEN)},
                {<<"consumerToken">>, <<"graphSync">>, ?ERROR_BAD_VALUE_TOKEN(<<"consumerToken">>, ?ERROR_BAD_TOKEN)}
            ])
        }
    })).


verify_invite_token(_Config) ->
    User = ozt_users:create(),
    ProviderAdmin = ozt_users:create(),
    SessionId = ozt_http:simulate_login(ProviderAdmin),
    Provider = ozt_providers:create_for_admin_user(ProviderAdmin),
    Space = ozt_users:create_space_for(User),
    Group = ozt_users:create_group_for(User),
    ozt_providers:support_space(Provider, Space),
    ozt:reconcile_entity_graph(),

    AllClients = [
        nobody,
        root,
        {provider, Provider},
        {user, User},
        {user, ProviderAdmin}
    ],

    TokenAlpha = create_user_temporary_token(User, ?INVITE_TOKEN(?USER_JOIN_SPACE, Space), [
        #cv_time{valid_until = ozt:timestamp_seconds() + 3600}
    ]),
    verify_invite_token_base(
        AllClients, TokenAlpha, any, undefined, any,
        true, {?SUB(user, User), 3600}
    ),
    verify_invite_token_base(
        AllClients, TokenAlpha, undefined, any, ?USER_JOIN_SPACE,
        true, {?SUB(user, User), 3600}
    ),
    verify_invite_token_base(
        AllClients, TokenAlpha, any, any, ?GROUP_JOIN_SPACE,
        false, ?ERROR_NOT_AN_INVITE_TOKEN(?GROUP_JOIN_SPACE, ?INVITE_TOKEN(?USER_JOIN_SPACE, Space))
    ),

    TokenBeta = create_provider_named_token(Provider, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, Provider), [
        #cv_time{valid_until = ozt:timestamp_seconds() + 2700}
    ]),
    verify_invite_token_base(
        AllClients, TokenBeta, undefined, undefined, any,
        true, {?SUB(?ONEPROVIDER, Provider), 2700}
    ),
    verify_invite_token_base(
        AllClients, TokenBeta, any, any, ?GROUP_JOIN_CLUSTER,
        true, {?SUB(?ONEPROVIDER, Provider), 2700}
    ),

    TokenGamma = create_user_temporary_token(User, ?INVITE_TOKEN(?SUPPORT_SPACE, Space), [
        #cv_time{valid_until = ozt:timestamp_seconds() + 1200},
        #cv_consumer{whitelist = [?SUB(?ONEPROVIDER, Provider)]}
    ]),
    verify_invite_token_base(
        AllClients, TokenGamma, undefined, ?SUB(?ONEPROVIDER, Provider), ?SUPPORT_SPACE,
        true, {?SUB(user, User), 1200}
    ),
    verify_invite_token_base(
        AllClients -- [{provider, Provider}], TokenGamma, any, undefined, any,
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_consumer{whitelist = [?SUB(?ONEPROVIDER, Provider)]})
    ),
    % the consumer defaults to the authenticated client
    verify_invite_token_base(
        [{provider, Provider}], TokenGamma, any, undefined, any,
        true, {?SUB(user, User), 1200}
    ),

    TokenDelta = create_user_named_token(User, ?INVITE_TOKEN(?GROUP_JOIN_GROUP, Group), [
        #cv_time{valid_until = ozt:timestamp_seconds() - 1}
    ]),
    verify_invite_token_base(
        AllClients, TokenDelta, undefined, any, any,
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_time{valid_until = ozt:timestamp_seconds() - 1})
    ),

    TokenLambda = create_user_temporary_token(User, ?INVITE_TOKEN(?USER_JOIN_GROUP, Group), [
        #cv_ip{whitelist = [{{134, 93, 0, 0}, 16}]}
    ]),
    verify_invite_token_base(
        AllClients, TokenLambda, any, any, any,
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_ip{whitelist = [{{134, 93, 0, 0}, 16}]})
    ),
    verify_invite_token_base(
        AllClients, TokenLambda, <<"133.93.1.182">>, any, ?USER_JOIN_GROUP,
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_ip{whitelist = [{{134, 93, 0, 0}, 16}]})
    ),
    verify_invite_token_base(
        AllClients, TokenLambda, <<"134.93.7.18">>, any, any,
        true, {?SUB(user, User), ?DEFAULT_TEMP_CAVEAT_TTL}
    ),
    verify_invite_token_base(
        AllClients, TokenLambda, <<"134.93.1.182">>, any, ?USER_JOIN_GROUP,
        true, {?SUB(user, User), ?DEFAULT_TEMP_CAVEAT_TTL}
    ),

    TokenKappa = create_user_named_token(User, ?INVITE_TOKEN(?GROUP_JOIN_SPACE, Space), [
        #cv_ip{whitelist = [{get_testmaster_ip(), 32}]}
    ]),
    verify_invite_token_base(
        AllClients, TokenKappa, <<"133.93.1.182">>, any, any,
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_ip{whitelist = [{get_testmaster_ip(), 32}]})
    ),
    % the IP defaults to the requesting client's IP
    verify_invite_token_base(
        AllClients, TokenKappa, undefined, any, any,
        true, {?SUB(user, User), undefined}
    ),

    TokenSigma = create_user_temporary_token(ProviderAdmin, ?ACCESS_TOKEN(SessionId)),
    verify_invite_token_base(
        AllClients, TokenSigma, undefined, any, any,
        false, ?ERROR_NOT_AN_INVITE_TOKEN(any, ?ACCESS_TOKEN(SessionId))
    ),

    TokenTheta = create_user_temporary_token(User, ?ACCESS_TOKEN),
    verify_invite_token_base(
        AllClients, TokenTheta, any, undefined, ?SPACE_JOIN_HARVESTER,
        false, ?ERROR_NOT_AN_INVITE_TOKEN(?SPACE_JOIN_HARVESTER, ?ACCESS_TOKEN)
    ),

    TokenOmega = create_provider_named_token(Provider, ?INVITE_TOKEN(?USER_JOIN_CLUSTER, Provider), [
        #cv_scope{scope = identity_token}
    ]),
    verify_invite_token_base(
        AllClients, TokenOmega, undefined, any, any,
        false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_scope{scope = identity_token})
    ).


verify_invite_token_base(AllClients, Token, PeerIp, Consumer, ExpType, ShouldSucceed, ExpResult) ->
    ConsumerToken = case Consumer of
        any ->
            User = ozt_users:create(),
            ozt_tokens:ensure_serialized(ozt_tokens:create(temporary, ?SUB(user, User), ?IDENTITY_TOKEN));
        undefined ->
            undefined;
        _ ->
            ozt_tokens:ensure_serialized(ozt_tokens:create(temporary, Consumer, ?IDENTITY_TOKEN))
    end,

    ?assert(api_test_utils:run_tests(ozt:get_test_config(), #api_test_spec{
        client_spec = #client_spec{
            correct = AllClients,
            unauthorized = [],
            forbidden = []
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = verify_invite_token,
            args = fun prepare_token_verification_logic_args/3,
            expected_result = case ShouldSucceed of
                true ->
                    ?OK_TERM(fun(#{<<"subject">> := Subject, <<"ttl">> := TTL}) ->
                        {ExpSubject, ExpTTL} = ExpResult,
                        ?assertEqual(Subject, ExpSubject),
                        ?assertEqual(TTL, ExpTTL),
                        true
                    end);
                false ->
                    ?ERROR_REASON(ExpResult)
            end
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/tokens/verify_invite_token">>,
            expected_code = case ShouldSucceed of
                true -> ?HTTP_200_OK;
                false -> errors:to_http_code(ExpResult)
            end,
            expected_body = case ShouldSucceed of
                true ->
                    fun(#{<<"subject">> := Subject, <<"ttl">> := TTL}) ->
                        {ExpSubject, ExpTTL} = ExpResult,
                        ?assertEqual(Subject, aai:subject_to_json(ExpSubject)),
                        ?assertEqual(TTL, utils:undefined_to_null(ExpTTL)),
                        true
                    end;
                false ->
                    undefined
            end
        },
        data_spec = #data_spec{
            % Peer IP can be required to verify a token, but is generally optional in the API.
            required = lists:flatten([
                <<"token">>,
                % Do not include expected type in the request in case of 'any'
                case ExpType of any -> []; _ -> <<"expectedInviteType">> end,
                case PeerIp of
                    any -> [];
                    undefined -> [];
                    _ -> [<<"peerIp">>]
                end,
                case ConsumerToken of
                    undefined -> [];
                    _ -> [<<"consumerToken">>]
                end
            ]),
            optional = lists:flatten([
                case PeerIp of
                    any -> [<<"peerIp">>];
                    undefined -> [];
                    _ -> []
                end,
                case Consumer of
                    any -> [<<"consumerToken">>];
                    _ -> []
                end
            ]),
            correct_values = #{
                <<"token">> => [ozt_tokens:ensure_serialized(Token)],
                <<"peerIp">> => case PeerIp of
                    any -> [<<"1.2.3.4">>, <<"5.6.7.8">>];
                    undefined -> [];
                    _ -> [utils:undefined_to_null(PeerIp)]
                end,
                <<"consumerToken">> => case ConsumerToken of
                    undefined -> [];
                    _ -> [ConsumerToken]
                end,
                <<"expectedInviteType">> => case ExpType of
                    any -> [];
                    _ -> [token_type:invite_type_to_str(ExpType)]
                end
            },
            bad_values = [
                {<<"token">>, <<"1234">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"peerIp">>, <<"1234.6.78.19">>, ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
                {<<"peerIp">>, 1234, ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
                {<<"peerIp">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
                {<<"consumerToken">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_TOKEN(<<"consumerToken">>, ?ERROR_BAD_TOKEN)},
                {<<"consumerToken">>, <<"graphSync">>, ?ERROR_BAD_VALUE_TOKEN(<<"consumerToken">>, ?ERROR_BAD_TOKEN)},
                {<<"expectedInviteType">>, <<"userJoinUser">>,
                    ?ERROR_BAD_VALUE_INVITE_TYPE(<<"expectedInviteType">>)},
                {<<"expectedInviteType">>, 1234,
                    ?ERROR_BAD_VALUE_INVITE_TYPE(<<"expectedInviteType">>)},
                {<<"expectedInviteType">>, #{<<"a">> => <<"b">>},
                    ?ERROR_BAD_VALUE_INVITE_TYPE(<<"expectedInviteType">>)}
            ]
        }
    })).


-record(infer_access_token_scope_test_spec, {
    subject_user :: od_user:id(),
    providers :: [od_provider:id()],
    space_supports :: #{od_space:id() => #{od_provider:id() => Readonly :: boolean()}},
    token_caveats = [] :: [caveats:caveat()],
    consumer_token = undefined,
    exp_general_result = success :: success | errors:error(),
    exp_allowed_spaces = [] :: [atom()],
    exp_allowed_providers = [] :: [atom()]
}).

infer_access_token_scope(_Config) ->
    SubjectUser = ozt_users:create(),
    ConsumerUser = ozt_users:create(),

    Krakow = ozt_providers:create_for_admin_user(SubjectUser),
    Paris = ozt_providers:create_for_admin_user(SubjectUser),
    Bari = ozt_providers:create_for_admin_user(SubjectUser),
    Lisbon = ozt_providers:create_for_admin_user(SubjectUser),

    Alpha = ozt_users:create_space_for(SubjectUser, <<"Alpha">>),
    Beta = ozt_users:create_space_for(SubjectUser, <<"Beta">>),
    Gamma = ozt_users:create_space_for(SubjectUser, <<"Gamma">>),
    Delta = ozt_users:create_space_for(SubjectUser, <<"Delta">>),
    Kappa = ozt_users:create_space_for(SubjectUser, <<"Kappa">>),

    % provider's support should be treated as readonly if all supporting storages of a provider are readonly
    add_support_with_new_storage(Alpha, Krakow, #{<<"readonly">> => true}),
    add_support_with_new_storage(Alpha, Krakow, #{<<"readonly">> => false}),

    add_support_with_new_storage(Beta, Krakow, #{<<"readonly">> => false}),
    add_support_with_new_storage(Beta, Paris, #{<<"readonly">> => true}),
    add_support_with_new_storage(Beta, Paris, #{<<"readonly">> => true}),
    add_support_with_new_storage(Beta, Bari, #{<<"readonly">> => false}),

    add_support_with_new_storage(Gamma, Krakow, #{<<"readonly">> => false}),
    add_support_with_new_storage(Gamma, Krakow, #{<<"readonly">> => false}),
    add_support_with_new_storage(Gamma, Paris, #{<<"readonly">> => true}),
    add_support_with_new_storage(Gamma, Bari, #{<<"readonly">> => true}),
    add_support_with_new_storage(Gamma, Lisbon, #{<<"readonly">> => false}),
    add_support_with_new_storage(Gamma, Lisbon, #{<<"readonly">> => false}),
    add_support_with_new_storage(Gamma, Lisbon, #{<<"readonly">> => true}),

    add_support_with_new_storage(Delta, Paris, #{<<"readonly">> => true}),
    add_support_with_new_storage(Delta, Lisbon, #{<<"readonly">> => true}),

    CommonSpec = #infer_access_token_scope_test_spec{
        subject_user = SubjectUser,
        providers = [Krakow, Paris, Bari, Lisbon],
        space_supports = #{
            Alpha => #{Krakow => false},
            Beta => #{Krakow => false, Paris => true, Bari => false},
            Gamma => #{Krakow => false, Paris => true, Bari => true, Lisbon => false},
            Delta => #{Paris => true, Lisbon => true},
            Kappa => #{}
        }
    },

    ?assert(run_infer_access_token_scope_test(CommonSpec#infer_access_token_scope_test_spec{
        token_caveats = [],
        exp_allowed_spaces = [Alpha, Beta, Gamma, Delta, Kappa],
        exp_allowed_providers = [Krakow, Paris, Bari, Lisbon]
    })),
    ?assert(run_infer_access_token_scope_test(CommonSpec#infer_access_token_scope_test_spec{
        token_caveats = [
            #cv_service{whitelist = [?SERVICE(?OP_WORKER, Krakow)]}
        ],
        exp_allowed_spaces = [Alpha, Beta, Gamma, Delta, Kappa],
        exp_allowed_providers = [Krakow]
    })),
    ?assert(run_infer_access_token_scope_test(CommonSpec#infer_access_token_scope_test_spec{
        token_caveats = [
            #cv_service{whitelist = [?SERVICE(?OP_WORKER, Krakow)]},
            #cv_service{whitelist = [?SERVICE(?OP_WORKER, Krakow), ?SERVICE(?OP_WORKER, Bari), ?SERVICE(?OZ_WORKER, <<"*">>)]}
        ],
        exp_allowed_spaces = [Alpha, Beta, Gamma, Delta, Kappa],
        exp_allowed_providers = [Krakow]
    })),
    ?assert(run_infer_access_token_scope_test(CommonSpec#infer_access_token_scope_test_spec{
        token_caveats = [#cv_service{whitelist = [?SERVICE(?OP_PANEL, Krakow)]}],
        exp_allowed_spaces = [Alpha, Beta, Gamma, Delta, Kappa],
        exp_allowed_providers = []
    })),
    ?assert(run_infer_access_token_scope_test(CommonSpec#infer_access_token_scope_test_spec{
        token_caveats = [
            #cv_data_path{whitelist = [
                ?RAND_CANONICAL_PATH(Alpha),
                ?RAND_CANONICAL_PATH(Beta),
                ?RAND_CANONICAL_PATH(Kappa)
            ]}
        ],
        exp_allowed_spaces = [Alpha, Beta, Kappa],
        exp_allowed_providers = [Krakow, Paris, Bari]
    })),
    ?assert(run_infer_access_token_scope_test(CommonSpec#infer_access_token_scope_test_spec{
        token_caveats = [
            #cv_service{whitelist = [?SERVICE(?OP_WORKER, <<"*">>)]},
            #cv_data_objectid{whitelist = [
                ?RAND_OBJECTID(Beta),
                ?RAND_OBJECTID(Gamma),
                ?RAND_OBJECTID(Delta)
            ]}
        ],
        exp_allowed_spaces = [Beta, Gamma, Delta],
        exp_allowed_providers = [Krakow, Paris, Bari, Lisbon]
    })),
    ?assert(run_infer_access_token_scope_test(CommonSpec#infer_access_token_scope_test_spec{
        token_caveats = [
            #cv_service{whitelist = [?SERVICE(?OP_WORKER, Paris), ?SERVICE(?OP_WORKER, Bari)]},
            #cv_data_path{whitelist = [
                ?RAND_CANONICAL_PATH(Alpha),
                ?RAND_CANONICAL_PATH(Beta)
            ]},
            #cv_data_objectid{whitelist = [
                ?RAND_OBJECTID(Beta),
                ?RAND_OBJECTID(Kappa)
            ]}
        ],
        exp_allowed_spaces = [Beta],
        exp_allowed_providers = [Paris, Bari]
    })),
    ?assert(run_infer_access_token_scope_test(CommonSpec#infer_access_token_scope_test_spec{
        token_caveats = [
            #cv_data_path{whitelist = [
                ?RAND_CANONICAL_PATH(Alpha)
            ]},
            #cv_data_objectid{whitelist = [
                ?RAND_OBJECTID(Kappa)
            ]}
        ],
        exp_allowed_spaces = [],
        exp_allowed_providers = []
    })),
    ?assert(run_infer_access_token_scope_test(CommonSpec#infer_access_token_scope_test_spec{
        token_caveats = [
            #cv_data_objectid{whitelist = [
                ?RAND_OBJECTID(?RAND_STR())
            ]}
        ],
        exp_allowed_spaces = [],
        exp_allowed_providers = []
    })),
    ?assert(run_infer_access_token_scope_test(CommonSpec#infer_access_token_scope_test_spec{
        token_caveats = [
            #cv_service{whitelist = [?SERVICE(?OP_WORKER, Krakow)]},
            #cv_data_path{whitelist = [?RAND_CANONICAL_PATH(Gamma)]},
            #cv_consumer{whitelist = [?SUB(user, ConsumerUser)]},
            #cv_ip{whitelist = [{{123, 4, 5, 6}, 24}, {get_testmaster_ip(), 32}]}
        ],
        consumer_token = ozt_tokens:ensure_serialized(create_user_temporary_token(ConsumerUser, ?IDENTITY_TOKEN)),
        exp_allowed_spaces = [Gamma],
        exp_allowed_providers = [Krakow]
    })),

    lists:foreach(fun(UnverifiableCaveat) ->
        ?assert(run_infer_access_token_scope_test(CommonSpec#infer_access_token_scope_test_spec{
            token_caveats = ?SHUFFLED([UnverifiableCaveat] ++ ?RAND_SUBLIST([
                #cv_service{whitelist = [?SERVICE(?OP_WORKER, Krakow)]},
                #cv_data_path{whitelist = [?RAND_CANONICAL_PATH(Alpha)]},
                #cv_data_objectid{whitelist = [?RAND_OBJECTID(Kappa)]},
                #cv_data_readonly{},
                #cv_interface{interface = ?RAND_CHOICE(rest, oneclient, graphsync)},
                #cv_api{whitelist = [{oz_worker, get, ?GRI_PATTERN(od_space, <<"*">>, <<"*">>, private)}]}
            ])),
            exp_general_result = ?ERROR_TOKEN_CAVEAT_UNVERIFIED(UnverifiableCaveat)
        }))
    end, [
        #cv_time{valid_until = ozt:timestamp_seconds() - 999999},
        #cv_ip{whitelist = [{{8, 8, 8, 8}, 32}]},
        #cv_asn{whitelist = [997]},
        #cv_country{type = whitelist, list = [<<"XX">>]},
        #cv_region{type = whitelist, list = [<<"Antarctica">>]},
        #cv_scope{scope = identity_token},
        #cv_consumer{whitelist = [?SUB(user, <<"123456789">>)]}
    ]),

    % passing a provider token should result in null data access scope
    ProviderToken = create_provider_named_token(Krakow, ?ACCESS_TOKEN, [#cv_time{valid_until = 17171717171}]),
    ?assertEqual(
        {ok, #{<<"validUntil">> => 17171717171, <<"dataAccessScope">> => null}},
        ozt:rpc(token_logic, infer_access_token_scope, [?NOBODY, #{<<"token">> => ProviderToken}])
    ),
    % passing a wrong token type should result in proper error
    IdentityToken = create_user_named_token(SubjectUser, ?IDENTITY_TOKEN, []),
    InviteToken = create_user_temporary_token(SubjectUser, ?INVITE_TOKEN(?USER_JOIN_SPACE, Alpha), []),
    ?assertEqual(
        ?ERROR_NOT_AN_ACCESS_TOKEN(?IDENTITY_TOKEN),
        ozt:rpc(token_logic, infer_access_token_scope, [?NOBODY, #{<<"token">> => IdentityToken}])
    ),
    ?assertEqual(
        ?ERROR_NOT_AN_ACCESS_TOKEN(?INVITE_TOKEN(?USER_JOIN_SPACE, Alpha)),
        ozt:rpc(token_logic, infer_access_token_scope, [?NOBODY, #{<<"token">> => InviteToken}])
    ),

    % make sure that immediately after adding a user to a space, the space shows up in the results
    lists_utils:pforeach(fun(_) ->
        NewUser = ozt_users:create(),
        NewUserToken = create_user_temporary_token(NewUser, ?ACCESS_TOKEN),
        ozt_spaces:add_user(Delta, NewUser),
        ?assertMatch(
            {ok, #{<<"dataAccessScope">> := #{
                <<"spaces">> := #{Delta := #{<<"supports">> := #{Paris := _, Lisbon := _}}},
                <<"providers">> := #{Paris := _, Lisbon := _}
            }}},
            ozt:rpc(token_logic, infer_access_token_scope, [?NOBODY, #{<<"token">> => NewUserToken}])
        )
    end, lists:seq(1, 50)).


run_infer_access_token_scope_test(#infer_access_token_scope_test_spec{
    subject_user = SubjectUser,
    providers = Providers,
    space_supports = SpaceSupports,
    token_caveats = TokenCaveats,
    consumer_token = ConsumerToken,
    exp_general_result = ExpGeneralResult,
    exp_allowed_spaces = ExpAllowedSpaces,
    exp_allowed_providers = ExpAllowedProviders
}) ->
    ProviderToVersion = maps_utils:generate_from_list(fun(ProviderId) ->
        ReleaseVersion = <<"21.02.", (integer_to_binary(?RAND_INT(1, 20)))/binary>>,
        ozt_providers:simulate_version(ProviderId, ReleaseVersion),
        ReleaseVersion
    end, Providers),

    TokenType = case ?RAND_BOOL() of
        true -> ?ACCESS_TOKEN;
        false -> ?ACCESS_TOKEN(ozt_http:simulate_login(SubjectUser))
    end,
    Token = case ?RAND_BOOL() of
        true -> create_user_named_token(SubjectUser, TokenType, TokenCaveats);
        false -> create_user_temporary_token(SubjectUser, TokenType, TokenCaveats)
    end,

    ExpectedSuccessResult = #{
        <<"validUntil">> => utils:undefined_to_null(caveats:infer_expiration_time(tokens:get_caveats(Token))),
        <<"dataAccessScope">> => #{
            <<"readonly">> => lists:member(#cv_data_readonly{}, TokenCaveats),
            <<"spaces">> => maps_utils:generate_from_list(fun(SpaceId) ->
                ProviderToReadonly = maps:get(SpaceId, SpaceSupports),
                AllowedProvidersForSpace = lists_utils:intersect(maps:keys(ProviderToReadonly), ExpAllowedProviders),
                {SpaceId, #{
                    <<"name">> => (ozt_spaces:get(SpaceId))#od_space.name,
                    <<"supports">> => maps:map(fun(_, Readonly) ->
                        #{<<"readonly">> => Readonly}
                    end, maps:with(AllowedProvidersForSpace, ProviderToReadonly))
                }}
            end, ExpAllowedSpaces),
            <<"providers">> => maps_utils:generate_from_list(fun(ProviderId) ->
                ProviderRecord = ozt_providers:get(ProviderId),
                {ProviderId, #{
                    <<"name">> => ProviderRecord#od_provider.name,
                    <<"domain">> => ProviderRecord#od_provider.domain,
                    <<"version">> => maps:get(ProviderId, ProviderToVersion),
                    <<"online">> => ozt:rpc(provider_connections, is_online, [ProviderId])
                }}
            end, ExpAllowedProviders)
        }
    },

    api_test_utils:run_tests(ozt:get_test_config(), #api_test_spec{
        client_spec = #client_spec{
            correct = [
                nobody,
                root,
                {provider, ozt_providers:create()},
                {user, SubjectUser},
                {user, ozt_users:create()}
            ],
            unauthorized = [],
            forbidden = []
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = infer_access_token_scope,
            args = fun prepare_token_verification_logic_args/3,
            expected_result = case ExpGeneralResult of
                success -> ?OK_MAP(ExpectedSuccessResult);
                {error, _} = Error -> ?ERROR_REASON(Error)
            end
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/tokens/infer_access_token_scope">>,
            expected_code = case ExpGeneralResult of
                success -> ?HTTP_200_OK;
                {error, _} = Error -> errors:to_http_code(Error)
            end,
            expected_body = case ExpGeneralResult of
                success ->
                    fun(Result) ->
                        ?assertEqual(ExpectedSuccessResult, Result),
                        true
                    end;
                {error, _} ->
                    undefined
            end
        },
        data_spec = #data_spec{
            required = lists:flatten([
                <<"token">>,
                case ConsumerToken of
                    undefined -> [];
                    _ -> [<<"consumerToken">>]
                end
            ]),
            correct_values = #{
                <<"token">> => [ozt_tokens:ensure_serialized(Token)],
                <<"consumerToken">> => case ConsumerToken of
                    undefined -> [];
                    _ -> [ConsumerToken]
                end
            },
            bad_values = [
                {<<"token">>, <<"1234">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"consumerToken">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_TOKEN(<<"consumerToken">>, ?ERROR_BAD_TOKEN)},
                {<<"consumerToken">>, <<"graphSync">>, ?ERROR_BAD_VALUE_TOKEN(<<"consumerToken">>, ?ERROR_BAD_TOKEN)}
            ]
        }
    }).


create_offline_user_access_token(_Config) ->
    SubjectUser = ozt_users:create(),
    SessionId = ozt_http:simulate_login(SubjectUser),
    RequestingProvider = ozt_providers:create(),
    Space = ozt_users:create_space_for(SubjectUser),
    ozt_providers:support_space(RequestingProvider, Space),

    AnotherUser = ozt_users:create(),
    AnotherProvider = ozt_providers:create(),

    TokenAlpha = create_user_temporary_token(SubjectUser, ?ACCESS_TOKEN, [
        #cv_time{valid_until = ozt:timestamp_seconds() + 36000}
    ]),
    create_offline_user_access_token_base(
        RequestingProvider, SubjectUser, TokenAlpha, #access_token_ctx{},
        ok
    ),

    TokenBeta = create_user_temporary_token(SubjectUser, ?ACCESS_TOKEN(SessionId), [
        #cv_time{valid_until = ozt:timestamp_seconds() + 18000},
        #cv_consumer{whitelist = [?SUB(user, AnotherUser)]}
    ]),
    create_offline_user_access_token_base(
        RequestingProvider, SubjectUser, TokenBeta, #access_token_ctx{},
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_consumer{whitelist = [?SUB(user, AnotherUser)]})
    ),
    create_offline_user_access_token_base(
        RequestingProvider, SubjectUser, TokenBeta, #access_token_ctx{consumer = ?SUB(user, AnotherUser)},
        ok
    ),

    TokenEpsilon = create_user_named_token(SubjectUser, ?ACCESS_TOKEN, [
        #cv_consumer{whitelist = [?SUB(?ONEPROVIDER, RequestingProvider)]}
    ]),
    create_offline_user_access_token_base(
        RequestingProvider, SubjectUser, TokenEpsilon, #access_token_ctx{consumer = ?SUB(user, AnotherUser)},
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_consumer{whitelist = [?SUB(?ONEPROVIDER, RequestingProvider)]})
    ),
    % the consumer defaults to the authenticated client
    create_offline_user_access_token_base(
        RequestingProvider, SubjectUser, TokenEpsilon, #access_token_ctx{consumer = undefined},
        ok
    ),

    TokenGamma = create_provider_named_token(AnotherProvider, ?ACCESS_TOKEN, [
        #cv_time{valid_until = ozt:timestamp_seconds() + 27000}
    ]),
    create_offline_user_access_token_base(
        RequestingProvider, SubjectUser, TokenGamma, #access_token_ctx{},
        ?ERROR_TOKEN_SUBJECT_INVALID
    ),

    TokenDelta = create_user_temporary_token(AnotherUser, ?ACCESS_TOKEN, [
        #cv_time{valid_until = ozt:timestamp_seconds() + 36000}
    ]),
    create_offline_user_access_token_base(
        RequestingProvider, SubjectUser, TokenDelta, #access_token_ctx{},
        % the subject of provided token must match the user for whom offline access is requested
        ?ERROR_TOKEN_SUBJECT_INVALID
    ),

    TokenKappa = create_user_named_token(SubjectUser, ?ACCESS_TOKEN, [
        #cv_ip{whitelist = [{{146, 193, 14, 0}, 24}]}
    ]),
    create_offline_user_access_token_base(
        RequestingProvider, SubjectUser, TokenKappa, #access_token_ctx{},
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_ip{whitelist = [{{146, 193, 14, 0}, 24}]})
    ),
    create_offline_user_access_token_base(
        RequestingProvider, SubjectUser, TokenKappa, #access_token_ctx{peer_ip = <<"146.193.14.86">>},
        ok
    ),

    TokenTau = create_user_temporary_token(SubjectUser, ?ACCESS_TOKEN, [
        #cv_ip{whitelist = [{get_testmaster_ip(), 13}]}
    ]),
    create_offline_user_access_token_base(
        RequestingProvider, SubjectUser, TokenTau, #access_token_ctx{peer_ip = <<"133.93.1.132">>},
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_ip{whitelist = [{get_testmaster_ip(), 13}]})
    ),
    % the IP defaults to the requesting client's IP
    create_offline_user_access_token_base(
        RequestingProvider, SubjectUser, TokenTau, #access_token_ctx{peer_ip = undefined},
        ok
    ),

    TokenOmega = create_user_named_token(SubjectUser, ?ACCESS_TOKEN, [
        #cv_interface{interface = rest},
        #cv_service{whitelist = [?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID), ?SERVICE(?OP_WORKER, RequestingProvider)]},
        #cv_data_readonly{}
    ]),
    create_offline_user_access_token_base(
        RequestingProvider, SubjectUser, TokenOmega, #access_token_ctx{
            interface = undefined, service = ?SERVICE(?OP_WORKER, RequestingProvider), allow_data_access_caveats = true
        },
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_interface{interface = rest})
    ),
    create_offline_user_access_token_base(
        RequestingProvider, SubjectUser, TokenOmega, #access_token_ctx{
            interface = rest, allow_data_access_caveats = true
        },
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_service{whitelist = [
            ?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID), ?SERVICE(?OP_WORKER, RequestingProvider)
        ]})
    ),

    create_offline_user_access_token_base(
        RequestingProvider, SubjectUser, TokenOmega, #access_token_ctx{
            interface = rest, service = ?SERVICE(?OP_WORKER, RequestingProvider), allow_data_access_caveats = undefined
        },
        ?ERROR_TOKEN_CAVEAT_UNVERIFIED(#cv_data_readonly{})
    ),
    create_offline_user_access_token_base(
        RequestingProvider, SubjectUser, TokenOmega, #access_token_ctx{
            interface = rest, service = ?SERVICE(?OP_WORKER, RequestingProvider), allow_data_access_caveats = true
        },
        ok
    ).

create_offline_user_access_token_base(RequestingProvider, SubjectUser, Token, AccessTokenCtx, ExpectedResult) ->
    ProviderAdmin = ozt_users:create(),
    ozt_clusters:add_user(RequestingProvider, ProviderAdmin, privileges:cluster_admin()),
    ozt_clusters:add_user(?ONEZONE_CLUSTER_ID, ProviderAdmin),
    AnotherProvider = ozt_providers:create(),
    ozt:reconcile_entity_graph(),

    ExpectedTtl = ozt:get_env(offline_access_token_ttl),

    ProviderIdentityToken = tokens:add_oneprovider_service_indication(
        ?OP_WORKER,
        ozt_tokens:ensure_serialized(ozt_tokens:create(temporary, ?SUB(?ONEPROVIDER, RequestingProvider), ?IDENTITY_TOKEN))
    ),

    VerifyFun = fun(OfflineAccessToken) ->
        OfflineAuthContextWithoutInterface = #{
            <<"token">> => OfflineAccessToken,
            <<"serviceToken">> => ProviderIdentityToken,
            <<"consumerToken">> => ProviderIdentityToken,
            <<"allowDataAccessCaveats">> => case AccessTokenCtx#access_token_ctx.allow_data_access_caveats of
                Bool when is_boolean(Bool) -> Bool;
                _ -> false
            end
        },
        OfflineAuthContext = maps_utils:put_if_defined(
            OfflineAuthContextWithoutInterface,
            <<"interface">>,
            case AccessTokenCtx#access_token_ctx.interface of
                any -> lists_utils:random_element([undefined | cv_interface:valid_interfaces()]);
                Other -> Other
            end
        ),
        ?assertEqual({ok, #{<<"subject">> => ?SUB(user, SubjectUser), <<"ttl">> => ExpectedTtl}}, ozt:rpc(
            token_logic, verify_access_token, [?NOBODY, OfflineAuthContext#{<<"token">> => OfflineAccessToken}]
        )),
        % create another offline access token based off this token and check it too
        ozt_mocks:simulate_seconds_passing(100),
        {ok, ConsecutiveToken} = ?assertMatch({ok, _}, ozt:rpc(
            token_logic, create_offline_user_access_token, [
                ?PROVIDER(RequestingProvider), SubjectUser, OfflineAuthContext#{<<"token">> => OfflineAccessToken}
            ])
        ),
        ?assertEqual({ok, #{<<"subject">> => ?SUB(user, SubjectUser), <<"ttl">> => ExpectedTtl}}, ozt:rpc(
            token_logic, verify_access_token, [?NOBODY, OfflineAuthContext#{<<"token">> => ConsecutiveToken}]
        ))
    end,

    ?assert(api_test_utils:run_tests(ozt:get_test_config(), #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {provider, RequestingProvider}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, ProviderAdmin},
                {user, SubjectUser},
                {provider, AnotherProvider}
            ]
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = create_offline_user_access_token,
            args = fun(Auth, Data, Env) ->
                [PreparedAuth, PreparedData] = prepare_token_verification_logic_args(Auth, Data, Env),
                [PreparedAuth, SubjectUser, PreparedData]
            end,
            expected_result = case ExpectedResult of
                ok -> ?OK_TERM(VerifyFun);
                {error, _} = Error -> ?ERROR_REASON(Error)
            end
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_token, id = undefined, aspect = {offline_user_access_token, SubjectUser}},
            expected_result_op = case ExpectedResult of
                ok -> ?OK_TERM(VerifyFun);
                {error, _} = Error -> ?ERROR_REASON(Error)
            end
        },
        data_spec = access_token_verification_data_spec(Token, AccessTokenCtx)
    })).

%%%===================================================================
%%% Helper functions
%%%===================================================================

create_user_named_token(UserId, Type, Caveats) ->
    ozt_tokens:create(named, ?SUB(user, UserId), #{
        <<"name">> => ?UNIQUE_STRING, <<"type">> => Type, <<"caveats">> => Caveats
    }).

create_provider_named_token(ProviderId, Type, Caveats) ->
    ozt_tokens:create(named, ?SUB(?ONEPROVIDER, ProviderId), #{
        <<"name">> => ?UNIQUE_STRING, <<"type">> => Type, <<"caveats">> => Caveats
    }).


create_user_temporary_token(UserId, Type) ->
    create_user_temporary_token(UserId, Type, []).
create_user_temporary_token(UserId, Type, Caveats) ->
    ozt_tokens:create(temporary, ?SUB(user, UserId), Type, Caveats).

create_provider_temporary_token(ProviderId, Type) ->
    create_provider_temporary_token(ProviderId, Type, []).
create_provider_temporary_token(ProviderId, Type, Caveats) ->
    ozt_tokens:create(temporary, ?SUB(?ONEPROVIDER, ProviderId), Type, Caveats).


get_testmaster_ip() ->
    {ok, [{IP, _, _} | _]} = inet:getif(),
    IP.


add_support_with_new_storage(SpaceId, ProviderId, StorageData) ->
    StorageId = ozt_providers:create_storage(ProviderId, StorageData),
    ozt_providers:support_space(ProviderId, StorageId, SpaceId).


prepare_token_verification_logic_args(Auth, Data, _Env) ->
    [Auth#auth{peer_ip = get_testmaster_ip()}, Data].


access_token_verification_data_spec(Token, AccessTokenCtx) ->
    #access_token_ctx{
        peer_ip = PeerIp,
        interface = Interface,
        service = Service,
        consumer = Consumer,
        allow_data_access_caveats = AllowDataAccessCaveats
    } = AccessTokenCtx,

    ConsumerToken = case Consumer of
        any ->
            User = ozt_users:create(),
            ozt_tokens:ensure_serialized(ozt_tokens:create(temporary, ?SUB(user, User), ?IDENTITY_TOKEN));
        undefined ->
            undefined;
        _ ->
            ozt_tokens:ensure_serialized(ozt_tokens:create(temporary, Consumer, ?IDENTITY_TOKEN))
    end,

    ServiceToken = case Service of
        any ->
            case Token#token.subject of
                ?SUB(user, UserId) ->
                    ProvId = ozt_providers:create_as_support_for_user(UserId),
                    ozt_tokens:ensure_serialized(ozt_tokens:create(temporary, ?SUB(?ONEPROVIDER, ProvId), ?IDENTITY_TOKEN));
                ?SUB(?ONEPROVIDER) ->
                    undefined
            end;
        ?SERVICE(OpService, ProvId) when OpService == ?OP_WORKER orelse OpService == ?OP_PANEL ->
            tokens:add_oneprovider_service_indication(
                OpService,
                ozt_tokens:ensure_serialized(ozt_tokens:create(temporary, ?SUB(?ONEPROVIDER, ProvId), ?IDENTITY_TOKEN))
            );
        _ ->
            undefined
    end,
    #data_spec{
        % Peer IP, interface, service, consumer and allowDataAccessCaveats
        % flag can be required to verify a token, but generally they are
        % optional in the API.
        required = lists:flatten([
            <<"token">>,
            case PeerIp of
                any -> [];
                undefined -> [];
                _ -> [<<"peerIp">>]
            end,
            case Interface of
                any -> [];
                undefined -> [];
                _ -> [<<"interface">>]
            end,
            case ServiceToken of
                undefined -> [];
                _ -> [<<"serviceToken">>]
            end,
            case ConsumerToken of
                undefined -> [];
                _ -> [<<"consumerToken">>]
            end,
            case AllowDataAccessCaveats of
                any -> [];
                undefined -> [];
                _ -> [<<"allowDataAccessCaveats">>]
            end
        ]),
        optional = lists:flatten([
            case PeerIp of
                any -> [<<"peerIp">>];
                undefined -> [];
                _ -> []
            end,
            case Interface of
                any -> [<<"interface">>];
                undefined -> [];
                _ -> []
            end,
            case {Service, ServiceToken} of
                {any, undefined} -> [];
                {any, _} -> [<<"serviceToken">>];
                _ -> []
            end,
            case Consumer of
                any -> [<<"consumerToken">>];
                _ -> []
            end,
            case AllowDataAccessCaveats of
                any -> [<<"allowDataAccessCaveats">>];
                undefined -> [];
                _ -> []
            end
        ]),
        correct_values = #{
            <<"token">> => [ozt_tokens:ensure_serialized(Token)],
            <<"peerIp">> => case PeerIp of
                any -> [<<"1.2.3.4">>, <<"5.6.7.8">>];
                undefined -> [];
                _ -> [utils:undefined_to_null(PeerIp)]
            end,
            <<"interface">> => case Interface of
                any -> cv_interface:valid_interfaces();
                undefined -> [];
                _ -> [Interface]
            end,
            <<"serviceToken">> => case ServiceToken of
                undefined -> [];
                _ -> [ServiceToken]
            end,
            <<"consumerToken">> => case ConsumerToken of
                undefined -> [];
                _ -> [ConsumerToken]
            end,
            <<"allowDataAccessCaveats">> => case AllowDataAccessCaveats of
                any -> [true, false];
                undefined -> [];
                _ -> [AllowDataAccessCaveats]
            end
        },
        bad_values = lists:flatten([
            {<<"token">>, <<"1234">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
            {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
            {<<"token">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
            {<<"peerIp">>, <<"1234.6.78.19">>, ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
            {<<"peerIp">>, 1234, ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
            {<<"peerIp">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
            {<<"interface">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_ATOM(<<"interface">>)},
            {<<"interface">>, <<"graphSync">>,
                ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"interface">>, cv_interface:valid_interfaces())},
            {<<"serviceToken">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_TOKEN(<<"serviceToken">>, ?ERROR_BAD_TOKEN)},
            {<<"serviceToken">>, <<"graphSync">>, ?ERROR_BAD_VALUE_TOKEN(<<"serviceToken">>, ?ERROR_BAD_TOKEN)},
            {<<"consumerToken">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_TOKEN(<<"consumerToken">>, ?ERROR_BAD_TOKEN)},
            {<<"consumerToken">>, <<"graphSync">>, ?ERROR_BAD_VALUE_TOKEN(<<"consumerToken">>, ?ERROR_BAD_TOKEN)},
            {<<"allowDataAccessCaveats">>, <<"1234">>, ?ERROR_BAD_VALUE_BOOLEAN(<<"allowDataAccessCaveats">>)},
            {<<"allowDataAccessCaveats">>, 1234, ?ERROR_BAD_VALUE_BOOLEAN(<<"allowDataAccessCaveats">>)},
            {<<"allowDataAccessCaveats">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_BOOLEAN(<<"allowDataAccessCaveats">>)}
        ])
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    application:ensure_all_started(hackney),
    ozt:init_per_suite(Config).


end_per_suite(_Config) ->
    application:stop(hackney),
    ssl:stop().


init_per_group(_Group, Config) ->
    ozt_mocks:freeze_time(),
    ozt_mocks:mock_harvesting_backends(),
    Config.


end_per_group(_Group, Config) ->
    ozt_mocks:unfreeze_time(),
    ozt_mocks:unmock_harvesting_backends(),
    Config.


init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.
