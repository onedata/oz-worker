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
    toggle_provider_named_token_revoked/1,
    delete_named_token_by_nonce/1,
    delete_user_named_token/1,
    delete_provider_named_token/1,
    delete_all_user_named_tokens/1,
    delete_all_provider_named_tokens/1
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
        toggle_provider_named_token_revoked,
        delete_named_token_by_nonce,
        delete_user_named_token,
        delete_provider_named_token,
        delete_all_user_named_tokens,
        delete_all_provider_named_tokens
    ]).

%%%===================================================================
%%% Common macros and verification functions
%%%===================================================================

-define(USER_ALPHA, userAlpha).
-define(USER_BETA, userBeta).
-define(PROV_GAMMA, providerGamma).
-define(PROV_DELTA, providerDelta).

-define(ROOT_TOKEN(Provider), {root_token, Provider}).
-define(TOKENS_OF(UserOrProvider), {tokens_of, UserOrProvider}).
-define(ADMIN_OF(Provider), {admin_of, Provider}).

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
    % Id of the subject whose tokens are tested - macro binding
    % (?USER_ALPHA, ?PROV_DELTA etc.). It is resolved and fed to logic_args_generator)
    subject_id = undefined,
    % Clients that are allowed to perform the operation expressed by macro bindings
    % (?USER_ALPHA, ?PROVIDER_GAMMA...) or ?ADMIN_OF (which resolves to the provider's
    % cluster creator).
    correct_clients = [] :: atom(),
    % token_logic function to call
    logic_function :: atom(),
    % Returns logic function call args based on resolved subject_id (UserId / ProviderId)
    % and the expanded list of tokens to check
    logic_args_generator = fun(_, _) -> [] end :: logic_args_generator(),
    % Returns logic_expectation based on the expanded list of tokens to check
    logic_expectation_generator = fun(_) -> ?OK end :: logic_expectation_generator(),
    % Returns data_spec based on the expanded list of tokens to check
    data_spec = undefined :: #data_spec{},
    env_set_up_fun_generator = fun(_) -> undefined end :: env_set_up_fun_generator(),
    verify_end_fun_generator = fun(_) -> undefined end :: verify_end_fun_generator()
}).

% Record holding information about a basic env composed of two providers with two
% corresponding cluster admins and two regular users.
-record(basic_env, {
    user_alpha :: od_user:id(),
    user_beta :: od_user:id(),
    prov_gamma :: od_provider:id(),
    prov_gamma_token :: tokens:serialized(),
    prov_gamma_admin :: od_user:id(),
    prov_delta :: od_provider:id(),
    prov_delta_token :: tokens:serialized(),
    prov_delta_admin :: od_user:id()
}).

% Depends on the testing strategy: one_token_at_a_time | all_tokens_at_the_same_time
-type token_or_tokens() :: #named_token_data{} | [#named_token_data{}].
-type logic_args_generator() :: fun((SubjectId :: binary(), token_or_tokens()) -> logic_expectation()).
-type logic_expectation_generator() :: fun((token_or_tokens()) -> logic_expectation()).
-type env_set_up_fun_generator() :: fun((token_or_tokens()) -> undefined | api_test_utils:env_setup_fun()).
-type verify_end_fun_generator() :: fun((token_or_tokens()) -> undefined | api_test_utils:verify_fun()).

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
    {<<"metadata">>, <<"string literal">>, ?ERROR_BAD_VALUE_JSON(<<"metadata">>)},
    {<<"metadata">>, #{<<"$$$_internal_$$$">> => <<"this-is-forbidden">>}, ?ERROR_BAD_DATA(<<"metadata">>)}
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


% Creates some named and temporary tokens for users and providers from a basic env setup.
% Returns the named tokens represented by #named_token_data{} records.
-spec create_some_tokens(Config :: term(), #basic_env{}) ->
    #{gri:entity_id() => [#named_token_data{}]}.
create_some_tokens(Config, BasicEnv) ->
    #basic_env{
        user_alpha = UserAlpha, user_beta = UserBeta,
        prov_gamma = PrGamma, prov_gamma_admin = PrGammaAdmin,
        prov_delta = PrDelta, prov_delta_admin = PrDeltaAdmin
    } = BasicEnv,

    {ok, {SessionAlpha, _}} = oz_test_utils:log_in(Config, UserAlpha),
    {ok, {SessionBeta, _}} = oz_test_utils:log_in(Config, UserBeta),

    {ok, SpaceAlpha} = oz_test_utils:create_space(Config, ?USER(UserAlpha)),
    {ok, GroupBeta} = oz_test_utils:create_group(Config, ?USER(UserBeta)),

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

    PrGammaT1 = create_provider_named_token(Config, PrGamma, ?ACCESS_TOKEN),
    PrGammaT2 = create_provider_named_token(Config, PrGamma, ?INVITE_TOKEN(?CLUSTER_INVITE_USER_TOKEN, PrGamma)),

    PrGammaAdminT1 = create_user_named_token(Config, PrGammaAdmin, ?INVITE_TOKEN(?CLUSTER_INVITE_USER_TOKEN, PrGamma)),
    PrGammaAdminT2 = create_user_named_token(Config, PrGammaAdmin, ?ACCESS_TOKEN),

    PrDeltaT1 = create_provider_named_token(Config, PrDelta, ?ACCESS_TOKEN),
    PrDeltaT2 = create_provider_named_token(Config, PrDelta, ?INVITE_TOKEN(?CLUSTER_INVITE_GROUP_TOKEN, PrDelta)),

    PrDeltaAdminT1 = create_user_named_token(Config, PrDeltaAdmin, ?INVITE_TOKEN(?CLUSTER_INVITE_GROUP_TOKEN, PrDelta)),
    PrDeltaAdminT2 = create_user_named_token(Config, PrDeltaAdmin, ?ACCESS_TOKEN),

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

    create_provider_temporary_token(Config, PrGamma, ?ACCESS_TOKEN),
    create_provider_temporary_token(Config, PrGamma, ?INVITE_TOKEN(?CLUSTER_INVITE_USER_TOKEN, PrGamma)),

    create_provider_temporary_token(Config, PrDelta, ?ACCESS_TOKEN),
    create_provider_temporary_token(Config, PrDelta, ?INVITE_TOKEN(?CLUSTER_INVITE_GROUP_TOKEN, PrDelta)),

    #{
        UserAlpha => [UAlphaT1, UAlphaT2, UAlphaT3, UAlphaT4, UAlphaT5],
        UserBeta => [UBetaT1, UBetaT2, UBetaT3, UBetaT4, UBetaT5, UBetaT6, UBetaT7],
        PrGamma => [PrGammaT1, PrGammaT2],
        PrGammaAdmin => [PrGammaAdminT1, PrGammaAdminT2],
        PrDelta => [PrDeltaT1, PrDeltaT2],
        PrDeltaAdmin => [PrDeltaAdminT1, PrDeltaAdminT2]
    }.


create_basic_env(Config) ->
    {ok, UserAlpha} = oz_test_utils:create_user(Config),
    {ok, UserBeta} = oz_test_utils:create_user(Config),
    {ok, PrGammaAdmin} = oz_test_utils:create_user(Config),
    {ok, PrDeltaAdmin} = oz_test_utils:create_user(Config),
    {ok, {PrGamma, PrGammaToken}} = oz_test_utils:create_provider(Config, PrGammaAdmin, ?UNIQUE_STRING),
    {ok, {PrDelta, PrDeltaToken}} = oz_test_utils:create_provider(Config, PrDeltaAdmin, ?UNIQUE_STRING),
    #basic_env{
        user_alpha = UserAlpha,
        user_beta = UserBeta,
        prov_gamma = PrGamma,
        prov_gamma_token = PrGammaToken,
        prov_gamma_admin = PrGammaAdmin,
        prov_delta = PrDelta,
        prov_delta_token = PrDeltaToken,
        prov_delta_admin = PrDeltaAdmin
    }.


map_client(#basic_env{user_alpha = Id}, ?USER_ALPHA) -> {user, Id};
map_client(#basic_env{user_beta = Id}, ?USER_BETA) -> {user, Id};
map_client(#basic_env{prov_gamma = Id, prov_gamma_token = Tk}, ?PROV_GAMMA) -> {provider, Id, Tk};
map_client(#basic_env{prov_gamma_admin = Id}, ?ADMIN_OF(?PROV_GAMMA)) -> {user, Id};
map_client(#basic_env{prov_delta = Id, prov_delta_token = Tk}, ?PROV_DELTA) -> {provider, Id, Tk};
map_client(#basic_env{prov_delta_admin = Id}, ?ADMIN_OF(?PROV_DELTA)) -> {user, Id}.


all_clients(BasicEnv) ->
    ClientBindings = [
        ?USER_ALPHA, ?USER_BETA,
        ?PROV_GAMMA, ?PROV_DELTA,
        ?ADMIN_OF(?PROV_GAMMA), ?ADMIN_OF(?PROV_DELTA)
    ],
    [map_client(BasicEnv, C) || C <- ClientBindings].


% Common function for running test related to tokens.
% Covers get / update / delete operations.
% See the #token_api_test_spec{} record for description.
run_token_tests(Config, TestSpec) ->
    #basic_env{
        user_alpha = UserAlpha,
        user_beta = UserBeta,
        prov_gamma = PrGamma,
        prov_gamma_token = PrGammaToken,
        prov_gamma_admin = PrGammaAdmin,
        prov_delta = PrDelta,
        prov_delta_token = PrDeltaToken,
        prov_delta_admin = PrDeltaAdmin
    } = BasicEnv = create_basic_env(Config),

    #{
        UserAlpha := UserAlphaNamedTokens,
        UserBeta := UserBetaNamedTokens,
        PrGamma := PrGammaNamedTokens,
        PrGammaAdmin := PrGammaAdminNamedTokens,
        PrDelta := PrDeltaNamedTokens,
        PrDeltaAdmin := PrDeltaAdminNamedTokens
    } = create_some_tokens(Config, BasicEnv),

    #token_api_test_spec{
        tokens_to_check = TokensToCheckBindings,
        testing_strategy = TestingStrategy,
        subject_id = SubjectIdBinding,
        correct_clients = CorrectClientsBindings,
        logic_function = LogicFunction,
        logic_args_generator = LogicArgsGenerator,
        logic_expectation_generator = LogicExpectationGenerator,
        data_spec = DataSpec,
        env_set_up_fun_generator = EnvSetUpFunGenerator,
        verify_end_fun_generator = VerifyEndFunGenerator
    } = TestSpec,

    AllClients = all_clients(BasicEnv),
    CorrectClients = [map_client(BasicEnv, C) || C <- CorrectClientsBindings],
    ForbiddenClients = AllClients -- CorrectClients,
    SubjectId = case SubjectIdBinding of
        undefined ->
            undefined;
        _ ->
            case map_client(BasicEnv, SubjectIdBinding) of
                {user, UserId} -> UserId;
                {provider, ProviderId, _} -> ProviderId
            end
    end,

    TokensToCheck = lists:flatmap(fun
        (?ROOT_TOKEN(?PROV_GAMMA)) ->
            [#named_token_data{name = ?PROVIDER_ROOT_TOKEN_NAME, token = PrGammaToken}];
        (?ROOT_TOKEN(?PROV_DELTA)) ->
            [#named_token_data{name = ?PROVIDER_ROOT_TOKEN_NAME, token = PrDeltaToken}];
        (?TOKENS_OF(?USER_ALPHA)) ->
            UserAlphaNamedTokens;
        (?TOKENS_OF(?USER_BETA)) ->
            UserBetaNamedTokens;
        (?TOKENS_OF(?PROV_GAMMA)) ->
            PrGammaNamedTokens;
        (?TOKENS_OF(?ADMIN_OF(?PROV_GAMMA))) ->
            PrGammaAdminNamedTokens;
        (?TOKENS_OF(?PROV_DELTA)) ->
            PrDeltaNamedTokens;
        (?TOKENS_OF(?ADMIN_OF(?PROV_DELTA))) ->
            PrDeltaAdminNamedTokens;
        (#named_token_data{} = Token) ->
            [Token]
    end, TokensToCheckBindings),

    Testcases = case TestingStrategy of
        one_token_at_a_time -> TokensToCheck;
        all_tokens_at_the_same_time -> [TokensToCheck]
    end,

    lists:all(fun(TokenOrTokens) ->
        EnvSetUpFun = EnvSetUpFunGenerator(TokenOrTokens),
        VerifyEndFun = VerifyEndFunGenerator(TokenOrTokens),
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_MANAGE_TOKENS]} |
                    CorrectClients
                ],
                unauthorized = [nobody],
                forbidden = ForbiddenClients
            },
            logic_spec = #logic_spec{
                module = token_logic,
                function = LogicFunction,
                args = LogicArgsGenerator(SubjectId, TokenOrTokens),
                expected_result = LogicExpectationGenerator(TokenOrTokens)
            },
            data_spec = DataSpec
        },
        api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)
    end, Testcases).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_user_named_token(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, ProviderAdmin} = oz_test_utils:create_user(Config),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config, ProviderAdmin, ?UNIQUE_STRING),
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
                    {provider, Provider, ProviderToken},
                    {user, ProviderAdmin}
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
    {ok, ProviderAdmin} = oz_test_utils:create_user(Config),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config, ProviderAdmin, ?UNIQUE_STRING),
    {ok, {AnotherProvider, AnotherProviderToken}} = oz_test_utils:create_provider(Config),
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
                    {user, ProviderAdmin},
                    {admin, [?OZ_MANAGE_TOKENS, ?OZ_PROVIDERS_INVITE]}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {user, User},
                    {provider, AnotherProvider, AnotherProviderToken}
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
    {ok, ProviderAdmin} = oz_test_utils:create_user(Config),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config, ProviderAdmin, ?UNIQUE_STRING),

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
                {provider, Provider, ProviderToken},
                {user, ProviderAdmin}
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
    {ok, ProviderAdmin} = oz_test_utils:create_user(Config),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config, ProviderAdmin, ?UNIQUE_STRING),
    {ok, {AnotherProvider, AnotherProviderToken}} = oz_test_utils:create_provider(Config),

    Now = oz_test_utils:cluster_time_seconds(Config),
    MaxTtl = oz_test_utils:get_env(Config, max_temporary_token_ttl),

    ?assert(api_test_utils:run_tests(Config, #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, Provider, ProviderToken},
                {user, ProviderAdmin},
                {admin, [?OZ_MANAGE_TOKENS, ?OZ_PROVIDERS_INVITE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, User},
                {provider, AnotherProvider, AnotherProviderToken}
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
    {ok, AnotherUser} = oz_test_utils:create_user(Config),
    {ok, {SessionId, _}} = oz_test_utils:log_in(Config, User),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(User)),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config, User, ?UNIQUE_STRING),
    oz_test_utils:support_space_by_provider(Config, Provider, Space),
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
                    {provider, Provider, ProviderToken},
                    {user, AnotherUser}
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
            ?ROOT_TOKEN(?PROV_GAMMA),
            ?ROOT_TOKEN(?PROV_DELTA),
            ?TOKENS_OF(?USER_ALPHA), ?TOKENS_OF(?USER_BETA),
            ?TOKENS_OF(?PROV_GAMMA), ?TOKENS_OF(?ADMIN_OF(?PROV_GAMMA)),
            ?TOKENS_OF(?PROV_DELTA), ?TOKENS_OF(?ADMIN_OF(?PROV_DELTA))
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
    list_user_named_tokens(Config, ?USER_BETA),
    list_user_named_tokens(Config, ?ADMIN_OF(?PROV_GAMMA)),
    list_user_named_tokens(Config, ?ADMIN_OF(?PROV_DELTA)).

list_user_named_tokens(Config, User) ->
    ?assert(run_token_tests(Config, #token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(User)],
        testing_strategy = all_tokens_at_the_same_time,
        subject_id = User,
        correct_clients = [User],
        logic_function = list_user_named_tokens,
        logic_args_generator = fun(UserId, _) -> [auth, UserId] end,
        logic_expectation_generator = fun(TokensToCheck) ->
            ?OK_LIST([token_data_to_name_and_nonce(T) || T <- TokensToCheck])
        end
    })).


list_provider_named_tokens(Config) ->
    list_provider_named_tokens(Config, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    list_provider_named_tokens(Config, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]).

list_provider_named_tokens(Config, Provider, CorrectClients) ->
    ?assert(run_token_tests(Config, #token_api_test_spec{
        tokens_to_check = [?ROOT_TOKEN(Provider), ?TOKENS_OF(Provider)],
        testing_strategy = all_tokens_at_the_same_time,
        subject_id = Provider,
        correct_clients = CorrectClients,
        logic_function = list_provider_named_tokens,
        logic_args_generator = fun(ProviderId, _) -> [auth, ProviderId] end,
        logic_expectation_generator = fun(TokensToCheck) ->
            ?OK_LIST([token_data_to_name_and_nonce(T) || T <- TokensToCheck])
        end
    })).


get_named_token_by_nonce(Config) ->
    get_named_token_by_nonce(Config, ?USER_ALPHA, [?USER_ALPHA]),
    get_named_token_by_nonce(Config, ?USER_BETA, [?USER_BETA]),
    get_named_token_by_nonce(Config, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    get_named_token_by_nonce(Config, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]),
    get_named_token_by_nonce(Config, ?ADMIN_OF(?PROV_GAMMA), [?ADMIN_OF(?PROV_GAMMA)]),
    get_named_token_by_nonce(Config, ?ADMIN_OF(?PROV_DELTA), [?ADMIN_OF(?PROV_DELTA)]).

get_named_token_by_nonce(Config, UserOrProvider, CorrectClients) ->
    ?assert(run_token_tests(Config, #token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(UserOrProvider)],
        testing_strategy = one_token_at_a_time,
        correct_clients = CorrectClients,
        logic_function = get_named_token_by_nonce,
        logic_args_generator = fun(_, TokenToCheck) ->
            [auth, token_data_to_nonce(TokenToCheck)]
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
    get_user_named_token(Config, ?USER_BETA),
    get_user_named_token(Config, ?ADMIN_OF(?PROV_GAMMA)),
    get_user_named_token(Config, ?ADMIN_OF(?PROV_DELTA)).

get_user_named_token(Config, User) ->
    ?assert(run_token_tests(Config, #token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(User)],
        testing_strategy = one_token_at_a_time,
        subject_id = User,
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
    get_provider_named_token(Config, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    get_provider_named_token(Config, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]).

get_provider_named_token(Config, Provider, CorrectClients) ->
    ?assert(run_token_tests(Config, #token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(Provider)],
        testing_strategy = one_token_at_a_time,
        subject_id = Provider,
        correct_clients = CorrectClients,
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
    update_named_token_metadata_by_nonce(Config, ?USER_ALPHA, [?USER_ALPHA]),
    update_named_token_metadata_by_nonce(Config, ?USER_BETA, [?USER_BETA]),
    update_named_token_metadata_by_nonce(Config, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    update_named_token_metadata_by_nonce(Config, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]),
    update_named_token_metadata_by_nonce(Config, ?ADMIN_OF(?PROV_GAMMA), [?ADMIN_OF(?PROV_GAMMA)]),
    update_named_token_metadata_by_nonce(Config, ?ADMIN_OF(?PROV_DELTA), [?ADMIN_OF(?PROV_DELTA)]).

update_named_token_metadata_by_nonce(Config, ProviderOrUser, CorrectClients) ->
    update_named_token_metadata_base(
        Config, CorrectClients, ProviderOrUser, update_named_token_metadata_by_nonce, fun(_, TokenToCheck) ->
            [auth, token_data_to_nonce(TokenToCheck), data]
        end
    ).


update_user_named_token_metadata(Config) ->
    update_user_named_token_metadata(Config, ?USER_ALPHA),
    update_user_named_token_metadata(Config, ?USER_BETA),
    update_user_named_token_metadata(Config, ?ADMIN_OF(?PROV_GAMMA)),
    update_user_named_token_metadata(Config, ?ADMIN_OF(?PROV_DELTA)).

update_user_named_token_metadata(Config, User) ->
    update_named_token_metadata_base(
        Config, [User], User, update_user_named_token_metadata, fun(SubjectId, TokenToCheck) ->
            [auth, SubjectId, token_data_to_name(TokenToCheck), data]
        end
    ).


update_provider_named_token_metadata(Config) ->
    update_provider_named_token_metadata(Config, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    update_provider_named_token_metadata(Config, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]).

update_provider_named_token_metadata(Config, Provider, CorrectClients) ->
    update_named_token_metadata_base(
        Config, CorrectClients, Provider, update_provider_named_token_metadata, fun(SubjectId, TokenToCheck) ->
            [auth, SubjectId, token_data_to_name(TokenToCheck), data]
        end
    ).


update_named_token_metadata_base(Config, CorrectClients, SubjectId, LogicFunction, LogicArgsGenerator) ->
    ?assert(run_token_tests(Config, #token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(SubjectId)],
        testing_strategy = one_token_at_a_time,
        subject_id = SubjectId,
        correct_clients = CorrectClients,
        logic_function = LogicFunction,
        logic_args_generator = LogicArgsGenerator,
        logic_expectation_generator = fun(_) -> ?OK end,
        data_spec = #data_spec{
            required = [<<"metadata">>],
            correct_values = #{
                <<"metadata">> => ?METADATA_EXAMPLES

            },
            bad_values = ?BAD_METADATA_VALUES
        },
        env_set_up_fun_generator = fun(TokenToCheck) ->
            fun() ->
                TokenNonce = token_data_to_nonce(TokenToCheck),
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
            end
        end,
        verify_end_fun_generator = fun(TokenToCheck) ->
            fun(ShouldSucceed, #{initialMetadata := InitialMetadata}, Data) ->
                TokenNonce = token_data_to_nonce(TokenToCheck),
                ExpMetadata = case ShouldSucceed of
                    false ->
                        InitialMetadata;
                    true ->
                        MetadataParam = maps:get(<<"metadata">>, Data, #{}),
                        MetadataParamWithoutInternal = maps:remove(<<"$$$_internal_$$$">>, MetadataParam),
                        maps:merge(MetadataParamWithoutInternal, InitialMetadata)
                end,
                ?assertMatch(
                    {ok, #document{value = #od_token{metadata = ExpMetadata}}},
                    oz_test_utils:call_oz(Config, od_token, get, [TokenNonce])
                ),
                ?assertMatch(
                    {ok, #od_token{metadata = ExpMetadata}},
                    oz_test_utils:call_oz(Config, token_logic, get_named_token_by_nonce, [?ROOT, TokenNonce])
                )
            end
        end
    })).


toggle_named_token_revoked_by_nonce(Config) ->
    toggle_named_token_revoked_by_nonce(Config, ?USER_ALPHA, [?USER_ALPHA]),
    toggle_named_token_revoked_by_nonce(Config, ?USER_BETA, [?USER_BETA]),
    toggle_named_token_revoked_by_nonce(Config, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    toggle_named_token_revoked_by_nonce(Config, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]),
    toggle_named_token_revoked_by_nonce(Config, ?ADMIN_OF(?PROV_GAMMA), [?ADMIN_OF(?PROV_GAMMA)]),
    toggle_named_token_revoked_by_nonce(Config, ?ADMIN_OF(?PROV_DELTA), [?ADMIN_OF(?PROV_DELTA)]).

toggle_named_token_revoked_by_nonce(Config, ProviderOrUser, CorrectClients) ->
    toggle_named_token_revoked_base(
        Config, CorrectClients, ProviderOrUser, toggle_named_token_revoked_by_nonce, fun(_, TokenToCheck) ->
            [auth, token_data_to_nonce(TokenToCheck), data]
        end
    ).


toggle_user_named_token_revoked(Config) ->
    toggle_user_named_token_revoked(Config, ?USER_ALPHA),
    toggle_user_named_token_revoked(Config, ?USER_BETA),
    toggle_user_named_token_revoked(Config, ?ADMIN_OF(?PROV_GAMMA)),
    toggle_user_named_token_revoked(Config, ?ADMIN_OF(?PROV_DELTA)).

toggle_user_named_token_revoked(Config, User) ->
    toggle_named_token_revoked_base(
        Config, [User], User, toggle_user_named_token_revoked, fun(SubjectId, TokenToCheck) ->
            [auth, SubjectId, token_data_to_name(TokenToCheck), data]
        end
    ).


toggle_provider_named_token_revoked(Config) ->
    toggle_provider_named_token_revoked(Config, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    toggle_provider_named_token_revoked(Config, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]).

toggle_provider_named_token_revoked(Config, Provider, CorrectClients) ->
    toggle_named_token_revoked_base(
        Config, CorrectClients, Provider, toggle_provider_named_token_revoked, fun(SubjectId, TokenToCheck) ->
            [auth, SubjectId, token_data_to_name(TokenToCheck), data]
        end
    ).


toggle_named_token_revoked_base(Config, CorrectClients, SubjectId, LogicFunction, LogicArgsGenerator) ->
    ?assert(run_token_tests(Config, #token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(SubjectId)],
        testing_strategy = one_token_at_a_time,
        subject_id = SubjectId,
        correct_clients = CorrectClients,
        logic_function = LogicFunction,
        logic_args_generator = LogicArgsGenerator,
        logic_expectation_generator = fun(_) -> ?OK end,
        data_spec = #data_spec{
            required = [<<"revoked">>],
            correct_values = #{
                <<"revoked">> => [true, false]
            },
            bad_values = ?BAD_REVOKED_VALUES
        },
        env_set_up_fun_generator = fun(TokenToCheck) ->
            fun() ->
                TokenNonce = token_data_to_nonce(TokenToCheck),
                {ok, #od_token{revoked = Revoked}} = oz_test_utils:call_oz(
                    Config, token_logic, get_named_token_by_nonce, [?ROOT, TokenNonce]
                ),
                #{previousRevokedState => Revoked}
            end
        end,
        verify_end_fun_generator = fun(TokenToCheck) ->
            TokenNonce = token_data_to_nonce(TokenToCheck),
            fun(ShouldSucceed, #{previousRevokedState := PreviousRevokedState}, Data) ->
                ExpRevoked = case ShouldSucceed of
                    false -> PreviousRevokedState;
                    true -> maps:get(<<"revoked">>, Data)
                end,
                ?assertMatch(
                    {ok, #od_token{revoked = ExpRevoked}},
                    oz_test_utils:call_oz(Config, token_logic, get_named_token_by_nonce, [?ROOT, TokenNonce])
                )
            end
        end
    })).


delete_named_token_by_nonce(Config) ->
    BasicEnv = create_basic_env(Config),
    delete_named_token_by_nonce(Config, BasicEnv, ?USER_ALPHA, [?USER_ALPHA]),
    delete_named_token_by_nonce(Config, BasicEnv, ?USER_BETA, [?USER_BETA]),
    delete_named_token_by_nonce(Config, BasicEnv, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    delete_named_token_by_nonce(Config, BasicEnv, ?ADMIN_OF(?PROV_GAMMA), [?ADMIN_OF(?PROV_GAMMA)]),
    delete_named_token_by_nonce(Config, BasicEnv, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]),
    delete_named_token_by_nonce(Config, BasicEnv, ?ADMIN_OF(?PROV_DELTA), [?ADMIN_OF(?PROV_DELTA)]).

delete_named_token_by_nonce(Config, BasicEnv, SubjectIdBinding, CorrectClientsBindings) ->
    delete_named_token_base(
        Config, BasicEnv, CorrectClientsBindings, SubjectIdBinding, delete_named_token_by_nonce, [auth, nonce]
    ).


delete_user_named_token(Config) ->
    BasicEnv = create_basic_env(Config),
    delete_user_named_token(Config, BasicEnv, ?USER_ALPHA),
    delete_user_named_token(Config, BasicEnv, ?USER_BETA),
    delete_user_named_token(Config, BasicEnv, ?ADMIN_OF(?PROV_GAMMA)),
    delete_user_named_token(Config, BasicEnv, ?ADMIN_OF(?PROV_DELTA)).

delete_user_named_token(Config, BasicEnv, UserId) ->
    delete_named_token_base(
        Config, BasicEnv, [UserId], UserId, delete_user_named_token, [auth, subjectId, name]
    ).


delete_provider_named_token(Config) ->
    BasicEnv = create_basic_env(Config),
    delete_provider_named_token(Config, BasicEnv, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    delete_provider_named_token(Config, BasicEnv, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]).

delete_provider_named_token(Config, BasicEnv, PrId, CorrectClientsBindings) ->
    delete_named_token_base(
        Config, BasicEnv, CorrectClientsBindings, PrId, delete_provider_named_token, [auth, subjectId, name]
    ).


delete_named_token_base(Config, BasicEnv, CorrectClientsBindings, SubjectIdBinding, LogicFunction, LogicArgs) ->
    CorrectClients = [map_client(BasicEnv, C) || C <- CorrectClientsBindings],
    EnvSetUpFun = fun() ->
        #named_token_data{token = Token, name = TokenName} = case map_client(BasicEnv, SubjectIdBinding) of
            {user, UserId} -> create_user_named_token(Config, UserId, ?ACCESS_TOKEN);
            {provider, ProviderId, _} -> create_provider_named_token(Config, ProviderId, ?ACCESS_TOKEN)
        end,
        ?SUB(_, SubjectId) = Token#token.subject,
        #{
            token => Token,
            % name, nonce and subjectId are used in logic args
            name => TokenName,
            nonce => Token#token.nonce,
            subjectId => SubjectId
        }
    end,

    VerifyEndFun = fun(ShouldSucceed, #{token := Token, name := TokenName}, _Data) ->
        assert_token_deleted(ShouldSucceed, Config, Token, TokenName)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_MANAGE_TOKENS]} |
                CorrectClients
            ],
            unauthorized = [nobody],
            forbidden = all_clients(BasicEnv) -- CorrectClients
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = LogicFunction,
            args = LogicArgs,
            expected_result = ?OK
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)).


delete_all_user_named_tokens(Config) ->
    BasicEnv = create_basic_env(Config),
    delete_all_user_named_tokens(Config, BasicEnv, ?USER_ALPHA),
    delete_all_user_named_tokens(Config, BasicEnv, ?USER_BETA),
    delete_all_user_named_tokens(Config, BasicEnv, ?ADMIN_OF(?PROV_GAMMA)),
    delete_all_user_named_tokens(Config, BasicEnv, ?ADMIN_OF(?PROV_DELTA)).

delete_all_user_named_tokens(Config, BasicEnv, UserIdBinding) ->
    {user, UserId} = map_client(BasicEnv, UserIdBinding),

    EnvSetUpFun = fun() ->
        #{UserId := UserTokens} = create_some_tokens(Config, BasicEnv),
        #{userTokens => UserTokens}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{userTokens := UserTokens}, _Data) ->
        lists:foreach(fun(#named_token_data{name = TokenName, token = Token}) ->
            assert_token_deleted(ShouldSucceed, Config, Token, TokenName)
        end, UserTokens)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_MANAGE_TOKENS]},
                {user, UserId}
            ],
            unauthorized = [nobody],
            forbidden = all_clients(BasicEnv) -- [{user, UserId}]
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = delete_all_user_named_tokens,
            args = [auth, UserId],
            expected_result = ?OK
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)).


delete_all_provider_named_tokens(Config) ->
    BasicEnv = create_basic_env(Config),
    delete_all_provider_named_tokens(Config, BasicEnv, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    delete_all_provider_named_tokens(Config, BasicEnv, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]).

delete_all_provider_named_tokens(Config, BasicEnv, ProviderIdBinding, CorrectClientsBindings) ->
    {provider, ProviderId, _} = map_client(BasicEnv, ProviderIdBinding),
    CorrectClients = [map_client(BasicEnv, C) || C <- CorrectClientsBindings],

    EnvSetUpFun = fun() ->
        #{ProviderId := ProviderTokens} = create_some_tokens(Config, BasicEnv),
        #{providerTokens => ProviderTokens}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{providerTokens := ProviderTokens}, _Data) ->
        lists:foreach(fun(#named_token_data{name = TokenName, token = Token}) ->
            assert_token_deleted(ShouldSucceed, Config, Token, TokenName)
        end, ProviderTokens)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_MANAGE_TOKENS]} |
                CorrectClients
            ],
            unauthorized = [nobody],
            forbidden = all_clients(BasicEnv) -- CorrectClients
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = delete_all_provider_named_tokens,
            args = [auth, ProviderId],
            expected_result = ?OK
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)).


assert_token_deleted(true, Config, Token, TokenName) ->
    #token{subject = Subject, nonce = TokenNonce, type = Type} = Token,
    ?assertMatch(
        {error, not_found},
        oz_test_utils:call_oz(Config, od_token, get, [TokenNonce])
    ),
    ?assertMatch(
        {error, not_found},
        oz_test_utils:call_oz(Config, named_tokens, get, [Subject, TokenName])
    ),
    case Type of
        ?ACCESS_TOKEN ->
            ?assertEqual(
                ?ERROR_TOKEN_INVALID,
                oz_test_utils:call_oz(Config, token_auth, check_token_auth, [Token, undefined, undefined])
            );
        _ ->
            ok
    end;
assert_token_deleted(false, Config, Token, TokenName) ->
    #token{subject = Subject, nonce = TokenNonce, type = Type} = Token,
    ?assertMatch(
        {ok, _},
        oz_test_utils:call_oz(Config, od_token, get, [TokenNonce])
    ),
    ?assertMatch(
        {ok, _},
        oz_test_utils:call_oz(Config, named_tokens, get, [Subject, TokenName])
    ),
    case Type of
        ?ACCESS_TOKEN ->
            ?assertEqual(
                {true, #auth{subject = Subject}},
                oz_test_utils:call_oz(Config, token_auth, check_token_auth, [Token, undefined, undefined])
            );
        _ ->
            ok
    end.


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
