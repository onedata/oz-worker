%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning token API (logic).
%%% @todo VFS-5727 REST to be implemented
%%% @end
%%%-------------------------------------------------------------------
-module(token_api_test_SUITE).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/graph_sync/graph_sync.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-include("api_test_utils.hrl").

%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    create_user_named_token/1,
    create_provider_named_token/1,
    create_user_temporary_token/1,
    create_provider_temporary_token/1,
    create_gui_access_token/1,
    list/1,
    list_user_named_tokens/1,
    list_provider_named_tokens/1,
    get_named_token_by_nonce/1,
    get_user_named_token/1,
    get_provider_named_token/1,
    update_named_token_metadata_by_nonce/1,
    update_user_named_token_metadata/1,
    update_provider_named_token_metadata/1,
    toggle_named_token_revoked_by_nonce/1,
    toggle_user_named_token_revoked/1,
    toggle_provider_named_token_revoked/1
]).

all() ->
    ?ALL([
        create_user_named_token,
        create_provider_named_token,
        create_user_temporary_token,
        create_provider_temporary_token,
        create_gui_access_token,
        list,
        list_user_named_tokens,
        list_provider_named_tokens,
        get_named_token_by_nonce,
        get_user_named_token,
        get_provider_named_token,
        update_named_token_metadata_by_nonce,
        update_user_named_token_metadata,
        update_provider_named_token_metadata,
        toggle_named_token_revoked_by_nonce,
        toggle_user_named_token_revoked,
        toggle_provider_named_token_revoked
    ]).

%%%===================================================================
%%% Common macros and verification functions
%%%===================================================================

-define(USER_ALPHA, userAlpha).
-define(USER_BETA, userBeta).
-define(PROVIDER_GAMMA, providerGamma).
-define(PROVIDER_DELTA, providerDelta).

-define(ROOT_TOKEN(Provider), {root_token, Provider}).
-define(TOKENS_OF(UserOrProvider), {tokens_of, UserOrProvider}).

% Record used for clearer test code
-record(named_token_data, {
    name :: od_token:name(),
    token :: tokens:serialized() | tokens:token()
}).

% Common test spec used in run_token_tests/2 for more concise code.
% The framework relies on above macros to express users / providers and their
% tokens. In each test, two users and two providers are created, along with a
% set of tokens for each of them.
% Each test implicitly checks that root/admin can perform all operations, and
% nobody is unauthorized to do anything.
-record(token_api_test_spec, {
    % A list of tokens (will be flattened) of all tokens to check within this test.
    % ?TOKENS_OF(UserOrProvider) bindings will be expanded to actual list of user/provider tokens.
    % ?ROOT_TOKEN(Provider) bindings will be expanded to the root token of given provider.
    tokens_to_check = [] :: [#named_token_data{} | {tokens_of, atom()} | {root_token, atom()}],
    % Decides if there is one test for all tokens_to_check, or a test for each
    % of the tokens
    testing_strategy :: one_token_at_a_time | all_tokens_at_the_same_time,
    % Clients that are allowed to perform the operation expressed by macro bindings
    % (?USER_ALPHA, ?PROVIDER_GAMMA...)
    correct_clients = [] :: atom(),
    % token_logic function to call
    logic_function :: atom(),
    % Returns logic function call args based on resolved ClientId (UserId / ProviderId)
    % and the expanded list of tokens to check
    logic_args_generator = fun(_, _) -> [] end :: logic_args_generator(),
    % Returns logic_expectation based on the expanded list of tokens to check
    logic_expectation_generator = fun(_) -> ?OK end :: logic_expectation_generator(),
    % Returns data_spec based on the expanded list of tokens to check
    data_spec_generator = fun(_) -> undefined end :: data_spec_generator()
}).

% Depends on the testing strategy: one_token_at_a_time | all_tokens_at_the_same_time
-type token_or_tokens() :: #named_token_data{} | [#named_token_data{}].
-type logic_args_generator() :: fun((ClientId :: binary(), token_or_tokens()) -> logic_expectation()).
-type logic_expectation_generator() :: fun((token_or_tokens()) -> logic_expectation()).
-type data_spec_generator() :: fun((token_or_tokens()) -> #data_spec{}).

-define(INVITE_TOKEN_TYPE_EXAMPLES, [
    ?INVITE_TOKEN(?GROUP_INVITE_USER_TOKEN, <<"123">>),
    ?INVITE_TOKEN(?GROUP_INVITE_GROUP_TOKEN, <<"123">>),
    ?INVITE_TOKEN(?SPACE_INVITE_USER_TOKEN, <<"123">>),
    ?INVITE_TOKEN(?SPACE_INVITE_GROUP_TOKEN, <<"123">>),
    ?INVITE_TOKEN(?SPACE_SUPPORT_TOKEN, <<"123">>),
    ?INVITE_TOKEN(?CLUSTER_INVITE_USER_TOKEN, <<"123">>),
    ?INVITE_TOKEN(?CLUSTER_INVITE_GROUP_TOKEN, <<"123">>),
    ?INVITE_TOKEN(?PROVIDER_REGISTRATION_TOKEN, <<"123">>),
    ?INVITE_TOKEN(?HARVESTER_INVITE_USER_TOKEN, <<"123">>),
    ?INVITE_TOKEN(?HARVESTER_INVITE_GROUP_TOKEN, <<"123">>),
    ?INVITE_TOKEN(?HARVESTER_INVITE_SPACE_TOKEN, <<"123">>)
]).

-define(PROVIDER_ALLOWED_INVITE_TOKEN_TYPES, [
    ?INVITE_TOKEN(?CLUSTER_INVITE_USER_TOKEN, <<"123">>),
    ?INVITE_TOKEN(?CLUSTER_INVITE_GROUP_TOKEN, <<"123">>)
]).

-define(CAVEATS_EXAMPLES_FOR_NAMED_TOKENS(Config), [
    [],
    [
        #cv_time{valid_until = infinity},
        #cv_audience{whitelist = [?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]}
    ],
    [
        #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) + 560},
        #cv_api{whitelist = [
            {?OZ_WORKER, all, ?GRI(od_user, <<"123">>, instance, private)}
        ]},
        #cv_data_space{whitelist = [<<"124234234">>, <<"opmxv7wenrfapfsa">>]},
        #cv_data_access{type = read},
        #cv_data_path{whitelist = [<<"/ab/cdef/g">>, <<"/1/2">>]},
        #cv_data_objectid{whitelist = [<<"12345">>, <<"67890">>]}
    ]
]).

% Temporary tokens must have a sane time caveat
-define(CAVEATS_EXAMPLES_FOR_TEMPORARY_TOKENS(Now, MaxTtl), [
    [
        #cv_time{valid_until = Now + 917}
    ],
    [
        #cv_time{valid_until = Now + MaxTtl - 7},
        #cv_audience{whitelist = [?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]}
    ],
    [
        #cv_time{valid_until = Now + 156},
        #cv_api{whitelist = [
            {?OP_PANEL, create, ?GRI('*', <<"*">>, '*', '*')},
            {all, get, ?GRI(od_user, <<"*">>, '*', private)}
        ]},
        #cv_data_space{whitelist = [<<"8y978qtwda67d12g3">>]},
        #cv_data_access{type = write},
        #cv_data_path{whitelist = [<<84, 71, 3, 250, 218, 190, 67, 67, 45, 13>>]},
        #cv_data_objectid{whitelist = [<<"000779645781987512837492813641234234">>]}
    ]
]).

-define(METADATA_EXAMPLES, [
    #{},
    #{
        <<"key">> => #{
            <<"nested">> => <<"value">>
        }
    },
    #{
        <<"key">> => #{
            <<"int">> => 123,
            <<"nested">> => #{
                <<"123">> => 16.5
            },
            <<"bool">> => true,
            <<"boolButNot">> => false
        }}
]).

-define(BAD_TYPE_VALUES_FOR_USER, [
    {<<"type">>, <<>>, ?ERROR_BAD_VALUE_TOKEN_TYPE(<<"type">>)},
    {<<"type">>, 123123, ?ERROR_BAD_VALUE_TOKEN_TYPE(<<"type">>)},
    {<<"type">>, <<"dsfdsfasdf">>, ?ERROR_BAD_VALUE_TOKEN_TYPE(<<"type">>)}
]).

-define(BAD_TYPE_VALUES_FOR_PROVIDER, ?BAD_TYPE_VALUES_FOR_USER ++ lists:map(fun(Type) ->
    {<<"type">>, Type, ?ERROR_BAD_VALUE_TOKEN_TYPE(<<"type">>)}
end, ?INVITE_TOKEN_TYPE_EXAMPLES -- ?PROVIDER_ALLOWED_INVITE_TOKEN_TYPES)).

-define(BAD_CAVEATS_VALUES, [
    {<<"caveats">>, <<>>, ?ERROR_BAD_VALUE_CAVEATS},
    {<<"caveats">>, 345345, ?ERROR_BAD_VALUE_CAVEATS},
    {<<"caveats">>, 1.56, ?ERROR_BAD_VALUE_CAVEATS},
    {<<"caveats">>, <<"string literal">>, ?ERROR_BAD_VALUE_CAVEATS},
    {<<"caveats">>, #{}, ?ERROR_BAD_VALUE_CAVEATS},
    {<<"caveats">>, [1, 2, 3], ?ERROR_BAD_VALUE_CAVEATS},
    {<<"caveats">>, [<<"a">>, <<"b">>, <<"c">>], ?ERROR_BAD_VALUE_CAVEATS},
    {<<"caveats">>, [<<"time > 1234">>], ?ERROR_BAD_VALUE_CAVEATS},
    {<<"caveats">>, [<<"ip = 1.2.3.4.5">>], ?ERROR_BAD_VALUE_CAVEATS}
]).

-define(BAD_METADATA_VALUES, [
    {<<"metadata">>, <<>>, ?ERROR_BAD_VALUE_JSON(<<"metadata">>)},
    {<<"metadata">>, 345345, ?ERROR_BAD_VALUE_JSON(<<"metadata">>)},
    {<<"metadata">>, 1.56, ?ERROR_BAD_VALUE_JSON(<<"metadata">>)},
    {<<"metadata">>, <<"string literal">>, ?ERROR_BAD_VALUE_JSON(<<"metadata">>)}
]).

-define(BAD_REVOKED_VALUES, [
    {<<"revoked">>, <<>>, ?ERROR_BAD_VALUE_BOOLEAN(<<"revoked">>)},
    {<<"revoked">>, 345345, ?ERROR_BAD_VALUE_BOOLEAN(<<"revoked">>)},
    {<<"revoked">>, 1.56, ?ERROR_BAD_VALUE_BOOLEAN(<<"revoked">>)},
    {<<"revoked">>, <<"string literal">>, ?ERROR_BAD_VALUE_BOOLEAN(<<"revoked">>)}
]).

-spec verify_token_fun(Config :: term(), aai:subject(), named | temporary) ->
    fun((term()) -> boolean()).
verify_token_fun(Config, Subject, Persistence) ->
    verify_token_fun(Config, Subject, Persistence, ?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)).

-spec verify_token_fun(Config :: term(), aai:subject(), named | temporary, undefined | aai:audience()) ->
    fun((term()) -> boolean()).
verify_token_fun(Config, Subject, Persistence, Audience) ->
    fun(Result) ->
        Token = #token{nonce = Nonce} = case is_binary(Result) of
            true -> element(2, {ok, _} = tokens:deserialize(Result));
            false -> Result
        end,
        case Token#token.type of
            ?INVITE_TOKEN(_, _) ->
                ok;
            _ ->
                % ACCESS_TOKEN, GUI_ACCESS_TOKEN
                case oz_test_utils:call_oz(Config, token_auth, check_token_auth, [Token, undefined, Audience]) of
                    {true, _} -> ok;
                    _ -> ct:print("Audience: ~p", [Audience])  % @fixme
                end,
                ?assertMatch(
                    {true, #auth{subject = Subject}},
                    oz_test_utils:call_oz(Config, token_auth, check_token_auth, [Token, undefined, Audience])
                )
        end,
        case Persistence of
            named ->
                {ok, #document{value = NamedToken}} = oz_test_utils:call_oz(Config, od_token, get, [Nonce]),
                ?assertEqual(Token, oz_test_utils:call_oz(Config, token_logic, named_token_to_token, [
                    Nonce, NamedToken
                ])),
                true;
            temporary ->
                true
        end
    end.


token_data_to_token(#named_token_data{token = Token}) ->
    token_data_to_token(Token);
token_data_to_token(Serialized) when is_binary(Serialized) ->
    {ok, Token} = tokens:deserialize(Serialized),
    Token;
token_data_to_token(Token = #token{}) ->
    Token.


token_data_to_nonce(TokenData) ->
    #token{nonce = Nonce} = token_data_to_token(TokenData),
    Nonce.


token_data_to_name(#named_token_data{name = Name}) ->
    Name.


token_data_to_name_and_nonce(#named_token_data{token = Token, name = Name}) ->
    {Name, token_data_to_nonce(Token)}.


create_user_named_token(Config, UserId, Type) ->
    Name = ?UNIQUE_STRING,
    {ok, Token} = oz_test_utils:call_oz(Config, token_logic, create_user_named_token, [
        ?USER(UserId), UserId, Name, #{<<"type">> => Type}
    ]),
    #named_token_data{token = Token, name = Name}.


create_provider_named_token(Config, ProviderId, Type) ->
    Name = ?UNIQUE_STRING,
    {ok, Token} = oz_test_utils:call_oz(Config, token_logic, create_provider_named_token, [
        ?PROVIDER(ProviderId), ProviderId, Name, #{<<"type">> => Type}
    ]),
    #named_token_data{token = Token, name = Name}.


create_user_temporary_token(Config, UserId, Type) ->
    Now = oz_test_utils:cluster_time_seconds(Config),
    {ok, Token} = oz_test_utils:call_oz(Config, token_logic, create_user_temporary_token, [
        ?USER(UserId), UserId, #{<<"type">> => Type, <<"caveats">> => [#cv_time{valid_until = Now + 176}]}
    ]),
    Token.


create_provider_temporary_token(Config, ProviderId, Type) ->
    Now = oz_test_utils:cluster_time_seconds(Config),
    {ok, Token} = oz_test_utils:call_oz(Config, token_logic, create_provider_temporary_token, [
        ?PROVIDER(ProviderId), ProviderId, #{<<"type">> => Type, <<"caveats">> => [#cv_time{valid_until = Now + 176}]}
    ]),
    Token.


% Creates some named and temporary tokens for given users and providers.
% Returns the named tokens represented by #named_token_data{} records.
-spec create_some_tokens(Config :: term(), od_user:id(), od_user:id(), od_provider:id(), od_provider:id()) ->
    #{atom => [#named_token_data{}]}.
create_some_tokens(Config, UserAlpha, UserBeta, ProviderGamma, ProviderDelta) ->
    {ok, {SessionAlpha, _}} = oz_test_utils:log_in(Config, UserAlpha),
    {ok, {SessionBeta, _}} = oz_test_utils:log_in(Config, UserBeta),

    {ok, SpaceAlpha} = oz_test_utils:create_space(Config, ?USER(UserAlpha)),
    {ok, GroupBeta} = oz_test_utils:create_space(Config, ?USER(UserBeta)),

    UAlphaT1 = create_user_named_token(Config, UserAlpha, ?ACCESS_TOKEN),
    UAlphaT2 = create_user_named_token(Config, UserAlpha, ?ACCESS_TOKEN),
    UAlphaT3 = create_user_named_token(Config, UserAlpha, ?INVITE_TOKEN(?SPACE_INVITE_USER_TOKEN, SpaceAlpha)),
    UAlphaT4 = create_user_named_token(Config, UserAlpha, ?INVITE_TOKEN(?SPACE_INVITE_GROUP_TOKEN, SpaceAlpha)),
    UAlphaT5 = create_user_named_token(Config, UserAlpha, ?INVITE_TOKEN(?SPACE_INVITE_GROUP_TOKEN, SpaceAlpha)),

    UBetaT1 = create_user_named_token(Config, UserBeta, ?ACCESS_TOKEN),
    UBetaT2 = create_user_named_token(Config, UserBeta, ?ACCESS_TOKEN),
    UBetaT3 = create_user_named_token(Config, UserBeta, ?ACCESS_TOKEN),
    UBetaT4 = create_user_named_token(Config, UserBeta, ?INVITE_TOKEN(?GROUP_INVITE_USER_TOKEN, GroupBeta)),
    UBetaT5 = create_user_named_token(Config, UserBeta, ?INVITE_TOKEN(?GROUP_INVITE_USER_TOKEN, GroupBeta)),
    UBetaT6 = create_user_named_token(Config, UserBeta, ?INVITE_TOKEN(?GROUP_INVITE_USER_TOKEN, GroupBeta)),
    UBetaT7 = create_user_named_token(Config, UserBeta, ?INVITE_TOKEN(?GROUP_INVITE_GROUP_TOKEN, GroupBeta)),

    PGammaT1 = create_provider_named_token(Config, ProviderGamma, ?ACCESS_TOKEN),
    PGammaT2 = create_provider_named_token(Config, ProviderGamma, ?INVITE_TOKEN(?CLUSTER_INVITE_USER_TOKEN, ProviderGamma)),

    PDeltaT1 = create_provider_named_token(Config, ProviderDelta, ?ACCESS_TOKEN),
    PDeltaT2 = create_provider_named_token(Config, ProviderDelta, ?INVITE_TOKEN(?CLUSTER_INVITE_GROUP_TOKEN, ProviderDelta)),

    % Create some temporary tokens, which should not be listed
    create_user_temporary_token(Config, UserAlpha, ?ACCESS_TOKEN),
    create_user_temporary_token(Config, UserAlpha, ?GUI_ACCESS_TOKEN(SessionAlpha)),
    create_user_temporary_token(Config, UserAlpha, ?INVITE_TOKEN(?SPACE_INVITE_USER_TOKEN, SpaceAlpha)),
    create_user_temporary_token(Config, UserAlpha, ?INVITE_TOKEN(?SPACE_INVITE_GROUP_TOKEN, SpaceAlpha)),
    create_user_temporary_token(Config, UserAlpha, ?INVITE_TOKEN(?SPACE_INVITE_GROUP_TOKEN, SpaceAlpha)),

    create_user_temporary_token(Config, UserBeta, ?ACCESS_TOKEN),
    create_user_temporary_token(Config, UserBeta, ?GUI_ACCESS_TOKEN(SessionBeta)),
    create_user_temporary_token(Config, UserBeta, ?INVITE_TOKEN(?GROUP_INVITE_USER_TOKEN, GroupBeta)),
    create_user_temporary_token(Config, UserBeta, ?INVITE_TOKEN(?GROUP_INVITE_USER_TOKEN, GroupBeta)),
    create_user_temporary_token(Config, UserBeta, ?INVITE_TOKEN(?GROUP_INVITE_USER_TOKEN, GroupBeta)),
    create_user_temporary_token(Config, UserBeta, ?INVITE_TOKEN(?GROUP_INVITE_GROUP_TOKEN, GroupBeta)),

    create_provider_temporary_token(Config, ProviderGamma, ?ACCESS_TOKEN),
    create_provider_temporary_token(Config, ProviderGamma, ?INVITE_TOKEN(?CLUSTER_INVITE_USER_TOKEN, ProviderGamma)),

    create_provider_temporary_token(Config, ProviderDelta, ?ACCESS_TOKEN),
    create_provider_temporary_token(Config, ProviderDelta, ?INVITE_TOKEN(?CLUSTER_INVITE_GROUP_TOKEN, ProviderDelta)),

    #{
        UserAlpha => [UAlphaT1, UAlphaT2, UAlphaT3, UAlphaT4, UAlphaT5],
        UserBeta => [UBetaT1, UBetaT2, UBetaT3, UBetaT4, UBetaT5, UBetaT6, UBetaT7],
        ProviderGamma => [PGammaT1, PGammaT2],
        ProviderDelta => [PDeltaT1, PDeltaT2]
    }.


% See the #token_api_test_spec{} record for description.
run_token_tests(Config, TestSpec) ->
    {ok, UserAlpha} = oz_test_utils:create_user(Config),
    {ok, UserBeta} = oz_test_utils:create_user(Config),
    {ok, {ProviderGamma, PGammaToken}} = oz_test_utils:create_provider(Config),
    {ok, {ProviderDelta, PDeltaToken}} = oz_test_utils:create_provider(Config),

    #{
        UserAlpha := UserAlphaNamedTokens,
        UserBeta := UserBetaNamedTokens,
        ProviderGamma := ProviderGammaNamedTokens,
        ProviderDelta := ProviderDeltaNamedTokens
    } = create_some_tokens(Config, UserAlpha, UserBeta, ProviderGamma, ProviderDelta),

    #token_api_test_spec{
        tokens_to_check = TokensToCheckBindings,
        testing_strategy = TestingStrategy,
        correct_clients = CorrectClientsBindings,
        logic_function = LogicFunction,
        logic_args_generator = LogicArgsGenerator,
        logic_expectation_generator = LogicExpectationGenerator,
        data_spec_generator = DataSpecGenerator
    } = TestSpec,

    MapClient = fun
        (?USER_ALPHA) -> {user, UserAlpha};
        (?USER_BETA) -> {user, UserBeta};
        (?PROVIDER_GAMMA) -> {provider, ProviderGamma, PGammaToken};
        (?PROVIDER_DELTA) -> {provider, ProviderDelta, PDeltaToken}
    end,

    CorrectClients = lists:map(MapClient, CorrectClientsBindings),
    ForbiddenClients = [
        {user, UserAlpha},
        {user, UserBeta},
        {provider, ProviderGamma, PGammaToken},
        {provider, ProviderDelta, PDeltaToken}
    ] -- CorrectClients,

    TokensToCheck = lists:flatmap(fun
        (?ROOT_TOKEN(?PROVIDER_GAMMA)) ->
            [#named_token_data{name = ?PROVIDER_ROOT_TOKEN_NAME, token = PGammaToken}];
        (?ROOT_TOKEN(?PROVIDER_DELTA)) ->
            [#named_token_data{name = ?PROVIDER_ROOT_TOKEN_NAME, token = PDeltaToken}];
        (?TOKENS_OF(?USER_ALPHA)) ->
            UserAlphaNamedTokens;
        (?TOKENS_OF(?USER_BETA)) ->
            UserBetaNamedTokens;
        (?TOKENS_OF(?PROVIDER_GAMMA)) ->
            ProviderGammaNamedTokens;
        (?TOKENS_OF(?PROVIDER_DELTA)) ->
            ProviderDeltaNamedTokens;
        (#named_token_data{} = Token) ->
            [Token]
    end, TokensToCheckBindings),

    Testcases = case TestingStrategy of
        one_token_at_a_time -> TokensToCheck;
        all_tokens_at_the_same_time -> [TokensToCheck]
    end,

    lists:all(fun(CurrentClientBinding) ->
        lists:all(fun(TokenOrTokens) ->
            CurrentClient = MapClient(CurrentClientBinding),
            CurrentClientId = case CurrentClient of
                {user, UserId} -> UserId;
                {provider, ProviderId, _} -> ProviderId
            end,
            api_test_utils:run_tests(Config, #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {admin, [?OZ_MANAGE_TOKENS]},
                        CurrentClient
                    ],
                    unauthorized = [nobody],
                    forbidden = ForbiddenClients
                },
                logic_spec = #logic_spec{
                    module = token_logic,
                    function = LogicFunction,
                    args = LogicArgsGenerator(CurrentClientId, TokenOrTokens),
                    expected_result = LogicExpectationGenerator(TokenOrTokens)
                },
                data_spec = DataSpecGenerator(TokenOrTokens)
            })
        end, Testcases)
    end, CorrectClientsBindings).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_user_named_token(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config),
    AlreadyExistingToken = <<"alreadyExistingName">>,

    % Create a token to later check if name duplicates are disallowed
    {ok, _} = oz_test_utils:call_oz(Config, token_logic, create_user_named_token, [
        ?USER(User), User, AlreadyExistingToken, #{}
    ]),

    Testcases = lists:flatten([
        {fun() -> ?UNIQUE_STRING end, ?OK_TERM(verify_token_fun(Config, ?SUB(user, User), named))},
        {AlreadyExistingToken, ?ERROR_REASON(?ERROR_ALREADY_EXISTS)},
        [{BadName, ?ERROR_REASON(Err)} || {_, BadName, Err} <- ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)]
    ]),

    lists:foreach(fun({TokenName, ExpResult}) ->
        ?assert(api_test_utils:run_tests(Config, #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {user, User},
                    {admin, [?OZ_MANAGE_TOKENS, ?OZ_PROVIDERS_INVITE]}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {provider, Provider, ProviderToken}
                ]
            },
            logic_spec = #logic_spec{
                module = token_logic,
                function = create_user_named_token,
                args = [auth, User, TokenName, data],
                expected_result = ExpResult
            },
            data_spec = #data_spec{
                optional = [<<"type">>, <<"caveats">>, <<"metadata">>],
                correct_values = #{
                    <<"type">> => [
                        ?ACCESS_TOKEN |
                        ?INVITE_TOKEN_TYPE_EXAMPLES
                    ],
                    <<"caveats">> => ?CAVEATS_EXAMPLES_FOR_NAMED_TOKENS(Config),
                    <<"metadata">> => ?METADATA_EXAMPLES
                },
                % If the expected result is an error, do no feed bad values
                bad_values = case ExpResult of
                    ?OK_TERM(_) -> ?BAD_TYPE_VALUES_FOR_USER ++ ?BAD_CAVEATS_VALUES ++ ?BAD_METADATA_VALUES;
                    ?ERROR_REASON(_) -> []
                end
            }
        }))
    end, Testcases).


create_provider_named_token(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config),
    AlreadyExistingToken = <<"subdubpubsubbub">>,

    % Create a token to later check if name duplicates are disallowed
    {ok, _} = oz_test_utils:call_oz(Config, token_logic, create_provider_named_token, [
        ?PROVIDER(Provider), Provider, AlreadyExistingToken, #{}
    ]),

    Testcases = lists:flatten([
        {fun() -> ?UNIQUE_STRING end, ?OK_TERM(verify_token_fun(Config, ?SUB(?ONEPROVIDER, Provider), named))},
        {AlreadyExistingToken, ?ERROR_REASON(?ERROR_ALREADY_EXISTS)},
        [{BadName, ?ERROR_REASON(Err)} || {_, BadName, Err} <- ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)]
    ]),

    lists:foreach(fun({TokenName, ExpResult}) ->
        ?assert(api_test_utils:run_tests(Config, #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {provider, Provider, ProviderToken},
                    {admin, [?OZ_MANAGE_TOKENS, ?OZ_PROVIDERS_INVITE]}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {user, User}
                ]
            },
            logic_spec = #logic_spec{
                module = token_logic,
                function = create_provider_named_token,
                args = [auth, Provider, TokenName, data],
                expected_result = ExpResult
            },
            data_spec = #data_spec{
                optional = [<<"type">>, <<"caveats">>, <<"metadata">>],
                correct_values = #{
                    <<"type">> => [
                        ?ACCESS_TOKEN |
                        ?PROVIDER_ALLOWED_INVITE_TOKEN_TYPES
                    ],
                    <<"caveats">> => ?CAVEATS_EXAMPLES_FOR_NAMED_TOKENS(Config),
                    <<"metadata">> => ?METADATA_EXAMPLES
                },
                bad_values = case ExpResult of
                    ?OK_TERM(_) -> ?BAD_TYPE_VALUES_FOR_PROVIDER ++ ?BAD_CAVEATS_VALUES ++ ?BAD_METADATA_VALUES;
                    ?ERROR_REASON(_) -> []
                end
            }
        }))
    end, Testcases).


create_user_temporary_token(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, {SessionId, _}} = oz_test_utils:log_in(Config, User),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config),

    Now = oz_test_utils:cluster_time_seconds(Config),
    MaxTtl = oz_test_utils:get_env(Config, max_temporary_token_ttl),

    ?assert(api_test_utils:run_tests(Config, #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User},
                {admin, [?OZ_MANAGE_TOKENS, ?OZ_PROVIDERS_INVITE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {provider, Provider, ProviderToken}
            ]
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = create_user_temporary_token,
            args = [auth, User, data],
            expected_result = ?OK_TERM(verify_token_fun(Config, ?SUB(user, User), temporary))
        },
        data_spec = #data_spec{
            % Caveats are required because of a mandatory time caveat
            required = [<<"caveats">>],
            optional = [<<"type">>],
            correct_values = #{
                <<"type">> => [
                    ?ACCESS_TOKEN,
                    ?GUI_ACCESS_TOKEN(SessionId) |
                    ?INVITE_TOKEN_TYPE_EXAMPLES
                ],
                <<"caveats">> => ?CAVEATS_EXAMPLES_FOR_TEMPORARY_TOKENS(Now, MaxTtl)
            },
            bad_values = ?BAD_TYPE_VALUES_FOR_USER ++ ?BAD_CAVEATS_VALUES ++ [
                {<<"caveats">>, [
                    #cv_time{valid_until = infinity}
                ], ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl)},
                {<<"caveats">>, [
                    #cv_audience{whitelist = [?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]},
                    #cv_time{valid_until = Now + MaxTtl + 1}
                ], ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl)}
            ]
        }
    })).


create_provider_temporary_token(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, {SessionId, _}} = oz_test_utils:log_in(Config, User),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config),

    Now = oz_test_utils:cluster_time_seconds(Config),
    MaxTtl = oz_test_utils:get_env(Config, max_temporary_token_ttl),

    ?assert(api_test_utils:run_tests(Config, #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, Provider, ProviderToken},
                {admin, [?OZ_MANAGE_TOKENS, ?OZ_PROVIDERS_INVITE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, User}
            ]
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = create_provider_temporary_token,
            args = [auth, Provider, data],
            expected_result = ?OK_TERM(verify_token_fun(Config, ?SUB(?ONEPROVIDER, Provider), temporary))
        },
        data_spec = #data_spec{
            % Caveats are required because of a mandatory time caveat
            required = [<<"caveats">>],
            optional = [<<"type">>],
            correct_values = #{
                <<"type">> => [
                    ?ACCESS_TOKEN |
                    ?PROVIDER_ALLOWED_INVITE_TOKEN_TYPES
                ],
                <<"caveats">> => ?CAVEATS_EXAMPLES_FOR_TEMPORARY_TOKENS(Now, MaxTtl)
            },
            bad_values = ?BAD_TYPE_VALUES_FOR_PROVIDER ++ ?BAD_CAVEATS_VALUES ++ [
                {<<"type">>, ?GUI_ACCESS_TOKEN(SessionId), ?ERROR_BAD_VALUE_TOKEN_TYPE(<<"type">>)},
                {<<"caveats">>, [
                    #cv_time{valid_until = infinity}
                ], ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl)},
                {<<"caveats">>, [
                    #cv_audience{whitelist = [?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]},
                    #cv_time{valid_until = Now + MaxTtl + 50}
                ], ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl)}
            ]
        }
    })).


create_gui_access_token(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, {SessionId, _}} = oz_test_utils:log_in(Config, User),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(User)),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config, User, ?UNIQUE_STRING),
    oz_test_utils:support_space(Config, Provider, Space),
    oz_test_utils:cluster_add_user(Config, ?ONEZONE_CLUSTER_ID, User),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, {AnotherProvider, _}} = oz_test_utils:create_provider(Config),

    VerifyFun = fun({Token, _Ttl}) ->
        [#cv_audience{whitelist = [Audience]}] = caveats:filter([cv_audience], tokens:get_caveats(Token)),
        TokenVerifyFun = verify_token_fun(Config, ?SUB(user, User), temporary, Audience),
        TokenVerifyFun(Token)
    end,

    Testcases = [
        {?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID),
            ?OK_TERM(VerifyFun)},
        {?AUD(?OZ_PANEL, ?ONEZONE_CLUSTER_ID),
            ?OK_TERM(VerifyFun)},
        {?AUD(?OP_WORKER, Provider),
            ?OK_TERM(VerifyFun)},
        {?AUD(?OP_PANEL, Provider),
            ?OK_TERM(VerifyFun)},
        {?AUD(?OZ_WORKER, <<"123">>),
            ?ERROR_REASON(?ERROR_TOKEN_AUDIENCE_FORBIDDEN(?AUD(?OZ_WORKER, <<"123">>)))},
        {?AUD(?OZ_PANEL, <<"123">>),
            ?ERROR_REASON(?ERROR_TOKEN_AUDIENCE_FORBIDDEN(?AUD(?OZ_PANEL, <<"123">>)))},
        {?AUD(?OP_WORKER, AnotherProvider),
            ?ERROR_REASON(?ERROR_TOKEN_AUDIENCE_FORBIDDEN(?AUD(?OP_WORKER, AnotherProvider)))},
        {?AUD(?OP_PANEL, AnotherProvider),
            ?ERROR_REASON(?ERROR_TOKEN_AUDIENCE_FORBIDDEN(?AUD(?OP_PANEL, AnotherProvider)))}
    ],

    lists:foreach(fun({Audience, ExpResult}) ->
        ?assert(api_test_utils:run_tests(Config, #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {user, User},
                    {admin, [?OZ_MANAGE_TOKENS]}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {provider, Provider, ProviderToken}
                ]
            },
            logic_spec = #logic_spec{
                module = token_logic,
                function = create_gui_access_token,
                args = [auth, User, SessionId, Audience],
                expected_result = ExpResult
            }
        }))
    end, Testcases).


list(Config) ->
    ?assert(run_token_tests(Config, #token_api_test_spec{
        tokens_to_check = [
            ?ROOT_TOKEN(?PROVIDER_GAMMA),
            ?ROOT_TOKEN(?PROVIDER_DELTA),
            ?TOKENS_OF(?USER_ALPHA), ?TOKENS_OF(?USER_BETA),
            ?TOKENS_OF(?PROVIDER_GAMMA), ?TOKENS_OF(?PROVIDER_DELTA)
        ],
        testing_strategy = all_tokens_at_the_same_time,
        correct_clients = [], % only root & admin are authorized
        logic_function = list,
        logic_args_generator = fun(_, _) -> [auth] end,
        logic_expectation_generator = fun(TokensToCheck) ->
            ?OK_LIST([token_data_to_nonce(T) || T <- TokensToCheck])
        end
    })).


list_user_named_tokens(Config) ->
    list_user_named_tokens(Config, ?USER_ALPHA),
    list_user_named_tokens(Config, ?USER_BETA).

list_user_named_tokens(Config, User) ->
    ?assert(run_token_tests(Config, #token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(User)],
        testing_strategy = all_tokens_at_the_same_time,
        correct_clients = [User],
        logic_function = list_user_named_tokens,
        logic_args_generator = fun(UserId, _) -> [auth, UserId] end,
        logic_expectation_generator = fun(TokensToCheck) ->
            ?OK_LIST([token_data_to_name_and_nonce(T) || T <- TokensToCheck])
        end
    })).


list_provider_named_tokens(Config) ->
    list_provider_named_tokens(Config, ?PROVIDER_GAMMA),
    list_provider_named_tokens(Config, ?PROVIDER_DELTA).

list_provider_named_tokens(Config, Provider) ->
    ?assert(run_token_tests(Config, #token_api_test_spec{
        tokens_to_check = [?ROOT_TOKEN(Provider), ?TOKENS_OF(Provider)],
        testing_strategy = all_tokens_at_the_same_time,
        correct_clients = [Provider],
        logic_function = list_provider_named_tokens,
        logic_args_generator = fun(ProviderId, _) -> [auth, ProviderId] end,
        logic_expectation_generator = fun(TokensToCheck) ->
            ?OK_LIST([token_data_to_name_and_nonce(T) || T <- TokensToCheck])
        end
    })).


get_named_token_by_nonce(Config) ->
    get_named_token_by_nonce(Config, ?USER_ALPHA),
    get_named_token_by_nonce(Config, ?USER_BETA),
    get_named_token_by_nonce(Config, ?PROVIDER_GAMMA),
    get_named_token_by_nonce(Config, ?PROVIDER_DELTA).

get_named_token_by_nonce(Config, UserOrProvider) ->
    ?assert(run_token_tests(Config, #token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(UserOrProvider)],
        testing_strategy = one_token_at_a_time,
        correct_clients = [UserOrProvider],
        logic_function = get_named_token_by_nonce,
        logic_args_generator = fun(UserOrProviderId, TokenToCheck) ->
            [auth, UserOrProviderId, token_data_to_nonce(TokenToCheck)]
        end,
        logic_expectation_generator = fun(TokenToCheck) ->
            Token = #token{
                subject = Subject,
                type = Type
            } = token_data_to_token(TokenToCheck),
            Caveats = tokens:get_caveats(Token),
            TokenName = token_data_to_name(TokenToCheck),
            ?OK_TERM(fun(TokenRecord) ->
                ?assertMatch(#od_token{
                    name = TokenName,
                    version = ?CURRENT_TOKEN_VERSION,
                    subject = Subject,
                    caveats = Caveats,
                    type = Type,
                    revoked = false
                }, TokenRecord)
            end)
        end
    })).


get_user_named_token(Config) ->
    get_user_named_token(Config, ?USER_ALPHA),
    get_user_named_token(Config, ?USER_BETA).

get_user_named_token(Config, User) ->
    ?assert(run_token_tests(Config, #token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(User)],
        testing_strategy = one_token_at_a_time,
        correct_clients = [User],
        logic_function = get_user_named_token,
        logic_args_generator = fun(UserId, TokenToCheck) ->
            [auth, UserId, token_data_to_name(TokenToCheck)]
        end,
        logic_expectation_generator = fun(TokenToCheck) ->
            Token = #token{
                subject = Subject,
                type = Type
            } = token_data_to_token(TokenToCheck),
            Caveats = tokens:get_caveats(Token),
            TokenName = token_data_to_name(TokenToCheck),
            ?OK_TERM(fun(TokenRecord) ->
                ?assertMatch(#od_token{
                    name = TokenName,
                    version = ?CURRENT_TOKEN_VERSION,
                    subject = Subject,
                    caveats = Caveats,
                    type = Type,
                    revoked = false
                }, TokenRecord)
            end)
        end
    })).


get_provider_named_token(Config) ->
    get_provider_named_token(Config, ?PROVIDER_GAMMA),
    get_provider_named_token(Config, ?PROVIDER_DELTA).

get_provider_named_token(Config, Provider) ->
    ?assert(run_token_tests(Config, #token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(Provider)],
        testing_strategy = one_token_at_a_time,
        correct_clients = [Provider],
        logic_function = get_provider_named_token,
        logic_args_generator = fun(ProviderId, TokenToCheck) ->
            [auth, ProviderId, token_data_to_name(TokenToCheck)]
        end,
        logic_expectation_generator = fun(TokenToCheck) ->
            Token = #token{
                subject = Subject,
                type = Type
            } = token_data_to_token(TokenToCheck),
            Caveats = tokens:get_caveats(Token),
            TokenName = token_data_to_name(TokenToCheck),
            ?OK_TERM(fun(TokenRecord) ->
                ?assertMatch(#od_token{
                    name = TokenName,
                    version = ?CURRENT_TOKEN_VERSION,
                    subject = Subject,
                    caveats = Caveats,
                    type = Type,
                    revoked = false
                }, TokenRecord)
            end)
        end
    })).


update_named_token_metadata_by_nonce(Config) ->
    {ok, UserAlpha} = oz_test_utils:create_user(Config),
    {ok, UserBeta} = oz_test_utils:create_user(Config),
    {ok, {ProviderGamma, PGammaToken}} = oz_test_utils:create_provider(Config),
    {ok, {ProviderDelta, PDeltaToken}} = oz_test_utils:create_provider(Config),

    UserAlphaToken = create_user_named_token(Config, UserAlpha, ?ACCESS_TOKEN),
    UserBetaToken = create_user_named_token(Config, UserBeta, ?ACCESS_TOKEN),
    ProviderGammaToken = create_provider_named_token(Config, ProviderGamma, ?ACCESS_TOKEN),
    ProviderDeltaToken = create_provider_named_token(Config, ProviderDelta, ?ACCESS_TOKEN),

    AllClients = [
        {user, UserAlpha},
        {user, UserBeta},
        {provider, ProviderGamma, PGammaToken},
        {provider, ProviderDelta, PDeltaToken}
    ],

    Testcases = [
        {{user, UserAlpha}, token_data_to_nonce(UserAlphaToken)},
        {{user, UserBeta}, token_data_to_nonce(UserBetaToken)},
        {{provider, ProviderGamma, PGammaToken}, token_data_to_nonce(ProviderGammaToken)},
        {{provider, ProviderDelta, PDeltaToken}, token_data_to_nonce(ProviderDeltaToken)}
    ],

    lists:foreach(fun({Client, TokenNonce}) ->
        update_named_token_metadata_base(
            Config, AllClients, Client, TokenNonce,
            update_named_token_metadata_by_nonce, [auth, TokenNonce, data]
        )
    end, Testcases).


update_user_named_token_metadata(Config) ->
    {ok, UserAlpha} = oz_test_utils:create_user(Config),
    {ok, UserBeta} = oz_test_utils:create_user(Config),
    {ok, {ProviderGamma, PGammaToken}} = oz_test_utils:create_provider(Config),
    {ok, {ProviderDelta, PDeltaToken}} = oz_test_utils:create_provider(Config),

    UserAlphaToken = create_user_named_token(Config, UserAlpha, ?ACCESS_TOKEN),
    UserBetaToken = create_user_named_token(Config, UserBeta, ?ACCESS_TOKEN),

    AllClients = [
        {user, UserAlpha},
        {user, UserBeta},
        {provider, ProviderGamma, PGammaToken},
        {provider, ProviderDelta, PDeltaToken}
    ],

    Testcases = [
        {{user, UserAlpha}, token_data_to_name_and_nonce(UserAlphaToken)},
        {{user, UserBeta}, token_data_to_name_and_nonce(UserBetaToken)}
    ],

    lists:foreach(fun({Client = {user, User}, {TokenName, TokenNonce}}) ->
        update_named_token_metadata_base(
            Config, AllClients, Client, TokenNonce,
            update_user_named_token_metadata, [auth, User, TokenName, data]
        )
    end, Testcases).


update_provider_named_token_metadata(Config) ->
    {ok, UserAlpha} = oz_test_utils:create_user(Config),
    {ok, UserBeta} = oz_test_utils:create_user(Config),
    {ok, {ProviderGamma, PGammaToken}} = oz_test_utils:create_provider(Config),
    {ok, {ProviderDelta, PDeltaToken}} = oz_test_utils:create_provider(Config),

    ProviderGammaToken = create_provider_named_token(Config, ProviderGamma, ?ACCESS_TOKEN),
    ProviderDeltaToken = create_provider_named_token(Config, ProviderDelta, ?ACCESS_TOKEN),

    AllClients = [
        {user, UserAlpha},
        {user, UserBeta},
        {provider, ProviderGamma, PGammaToken},
        {provider, ProviderDelta, PDeltaToken}
    ],

    Testcases = [
        {{provider, ProviderGamma, PGammaToken}, token_data_to_name_and_nonce(ProviderGammaToken)},
        {{provider, ProviderDelta, PDeltaToken}, token_data_to_name_and_nonce(ProviderDeltaToken)}
    ],

    lists:foreach(fun({Client = {provider, Provider, _}, {TokenName, TokenNonce}}) ->
        update_named_token_metadata_base(
            Config, AllClients, Client, TokenNonce,
            update_provider_named_token_metadata, [auth, Provider, TokenName, data]
        )
    end, Testcases).


update_named_token_metadata_base(Config, AllClients, CorrectClient, TokenNonce, Function, Args) ->
    EnvSetUpFun = fun() ->
        InitialMetadata = case rand:uniform(2) of
            1 -> #{};
            % Internal metadata section is dedicated for internal logic and cannot be modified.
            2 -> #{<<"$$$_internal_$$$">> => <<"reserved">>}
        end,
        % Reset the metadata to the initial value
        oz_test_utils:call_oz(Config, od_token, update, [TokenNonce, fun(Token) ->
            {ok, Token#od_token{metadata = InitialMetadata}}
        end]),
        #{initialMetadata => InitialMetadata}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{initialMetadata := InitialMetadata}, Data) ->
        ExpMetadata = case ShouldSucceed of
            false -> InitialMetadata;
            % Merge so that ExpMetadata always includes the internal values from InitialMetadata
            true -> maps:merge(Data, InitialMetadata)
        end,
        ct:print("ExpMetadata: ~p", [ExpMetadata]),  % @fixme
        ?assertMatch(
            {ok, #od_token{metadata = ExpMetadata}},
            oz_test_utils:call_oz(Config, token_logic, get_named_token_by_nonce, [?ROOT, TokenNonce])
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_MANAGE_TOKENS]},
                CorrectClient
            ],
            unauthorized = [nobody],
            forbidden = AllClients -- [CorrectClient]
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = Function,
            args = Args,
            expected_result = ?OK
        },
        data_spec = #data_spec{
            required = [<<"metadata">>],
            correct_values = #{
                <<"metadata">> => [
                    #{<<"$$$_internal_$$$">> => <<"overwriting-should-not-be-possible">>} |
                    ?METADATA_EXAMPLES
                ]
            },
            bad_values = ?BAD_METADATA_VALUES
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)).


toggle_named_token_revoked_by_nonce(Config) ->
    {ok, UserAlpha} = oz_test_utils:create_user(Config),
    {ok, UserBeta} = oz_test_utils:create_user(Config),
    {ok, {ProviderGamma, PGammaToken}} = oz_test_utils:create_provider(Config),
    {ok, {ProviderDelta, PDeltaToken}} = oz_test_utils:create_provider(Config),

    UserAlphaToken = create_user_named_token(Config, UserAlpha, ?ACCESS_TOKEN),
    UserBetaToken = create_user_named_token(Config, UserBeta, ?ACCESS_TOKEN),
    ProviderGammaToken = create_provider_named_token(Config, ProviderGamma, ?ACCESS_TOKEN),
    ProviderDeltaToken = create_provider_named_token(Config, ProviderDelta, ?ACCESS_TOKEN),

    AllClients = [
        {user, UserAlpha},
        {user, UserBeta},
        {provider, ProviderGamma, PGammaToken},
        {provider, ProviderDelta, PDeltaToken}
    ],

    Testcases = [
        {{user, UserAlpha}, token_data_to_nonce(UserAlphaToken)},
        {{user, UserBeta}, token_data_to_nonce(UserBetaToken)},
        {{provider, ProviderGamma, PGammaToken}, token_data_to_nonce(ProviderGammaToken)},
        {{provider, ProviderDelta, PDeltaToken}, token_data_to_nonce(ProviderDeltaToken)}
    ],

    lists:foreach(fun({Client, TokenNonce}) ->
        toggle_named_token_revoked_base(
            Config, AllClients, Client, TokenNonce,
            toggle_named_token_revoked_by_nonce, [auth, TokenNonce, data]
        )
    end, Testcases).


toggle_user_named_token_revoked(Config) ->
    {ok, UserAlpha} = oz_test_utils:create_user(Config),
    {ok, UserBeta} = oz_test_utils:create_user(Config),
    {ok, {ProviderGamma, PGammaToken}} = oz_test_utils:create_provider(Config),
    {ok, {ProviderDelta, PDeltaToken}} = oz_test_utils:create_provider(Config),

    UserAlphaToken = create_user_named_token(Config, UserAlpha, ?ACCESS_TOKEN),
    UserBetaToken = create_user_named_token(Config, UserBeta, ?ACCESS_TOKEN),

    AllClients = [
        {user, UserAlpha},
        {user, UserBeta},
        {provider, ProviderGamma, PGammaToken},
        {provider, ProviderDelta, PDeltaToken}
    ],

    Testcases = [
        {{user, UserAlpha}, token_data_to_name_and_nonce(UserAlphaToken)},
        {{user, UserBeta}, token_data_to_name_and_nonce(UserBetaToken)}
    ],

    lists:foreach(fun({Client = {user, User}, {TokenName, TokenNonce}}) ->
        toggle_named_token_revoked_base(
            Config, AllClients, Client, TokenNonce,
            toggle_user_named_token_revoked, [auth, User, TokenName, data]
        )
    end, Testcases).


toggle_provider_named_token_revoked(Config) ->
    {ok, UserAlpha} = oz_test_utils:create_user(Config),
    {ok, UserBeta} = oz_test_utils:create_user(Config),
    {ok, {ProviderGamma, PGammaToken}} = oz_test_utils:create_provider(Config),
    {ok, {ProviderDelta, PDeltaToken}} = oz_test_utils:create_provider(Config),

    ProviderGammaToken = create_provider_named_token(Config, ProviderGamma, ?ACCESS_TOKEN),
    ProviderDeltaToken = create_provider_named_token(Config, ProviderDelta, ?ACCESS_TOKEN),

    AllClients = [
        {user, UserAlpha},
        {user, UserBeta},
        {provider, ProviderGamma, PGammaToken},
        {provider, ProviderDelta, PDeltaToken}
    ],

    Testcases = [
        {{provider, ProviderGamma, PGammaToken}, token_data_to_name_and_nonce(ProviderGammaToken)},
        {{provider, ProviderDelta, PDeltaToken}, token_data_to_name_and_nonce(ProviderDeltaToken)}
    ],

    lists:foreach(fun({Client = {provider, Provider, _}, {TokenName, TokenNonce}}) ->
        toggle_named_token_revoked_base(
            Config, AllClients, Client, TokenNonce,
            toggle_provider_named_token_revoked, [auth, Provider, TokenName, data]
        )
    end, Testcases).


toggle_named_token_revoked_base(Config, AllClients, CorrectClient, TokenNonce, Function, Args) ->
    EnvSetUpFun = fun() ->
        {ok, #od_token{revoked = Revoked}} = oz_test_utils:call_oz(
            Config, token_logic, get_named_token_by_nonce, [?ROOT, TokenNonce]
        ),
        #{previousRevokedState => Revoked}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{previousRevokedState := PreviousRevokedState}, Data) ->
        ExpRevoked = case ShouldSucceed of
            false -> PreviousRevokedState;
            true -> maps:get(<<"revoked">>, Data)
        end,
        ?assertMatch(
            {ok, #od_token{revoked = ExpRevoked}},
            oz_test_utils:call_oz(Config, token_logic, get_named_token_by_nonce, [?ROOT, TokenNonce])
        )
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_MANAGE_TOKENS]},
                CorrectClient
            ],
            unauthorized = [nobody],
            forbidden = AllClients -- [CorrectClient]
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = Function,
            args = Args,
            expected_result = ?OK
        },
        data_spec = #data_spec{
            required = [<<"revoked">>],
            correct_values = #{
                <<"revoked">> => [true, false]
            },
            bad_values = ?BAD_REVOKED_VALUES
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    [{?LOAD_MODULES, [oz_test_utils]} | Config].


end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().


init_per_testcase(_, Config) ->
    Config.


end_per_testcase(_, Config) ->
    oz_test_utils:delete_all_entities(Config),
    ok.
