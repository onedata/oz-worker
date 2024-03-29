%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019-2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% CT tests concerning named/temporary token API.
%%% @end
%%%-------------------------------------------------------------------
-module(token_named_temp_api_test_SUITE).
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
    create_user_named_token/1,
    create_provider_named_token/1,
    create_user_temporary_token/1,
    create_provider_temporary_token/1,
    create_access_token_for_gui/1,
    list_user_named_tokens/1,
    list_provider_named_tokens/1,
    get_named_token/1,
    get_named_token_status/1,
    get_user_named_token_by_name/1,
    get_provider_named_token_by_name/1,
    get_user_temporary_token_generation/1,
    get_provider_temporary_token_generation/1,
    update_named_token/1,
    delete_named_token/1,
    delete_all_user_named_tokens/1,
    delete_all_provider_named_tokens/1,
    revoke_all_user_temporary_tokens/1,
    revoke_all_provider_temporary_tokens/1,

    % sequential tests
    list/1
]).

groups() -> [
    {parallel_tests, [parallel], [
        create_user_named_token,
        create_provider_named_token,
        create_user_temporary_token,
        create_provider_temporary_token,
        create_access_token_for_gui,
        list_user_named_tokens,
        list_provider_named_tokens,
        get_named_token,
        get_named_token_status,
        get_user_named_token_by_name,
        get_provider_named_token_by_name,
        get_user_temporary_token_generation,
        get_provider_temporary_token_generation,
        update_named_token,
        delete_named_token,
        delete_all_user_named_tokens,
        delete_all_provider_named_tokens,
        revoke_all_user_temporary_tokens,
        revoke_all_provider_temporary_tokens
    ]},
    {sequential_tests, [sequential], [
        list
    ]}
].

all() -> [
    {group, parallel_tests},
    {group, sequential_tests}
].

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


%%%===================================================================
%%% Common macros and verification functions
%%%===================================================================

-define(USER_ALPHA, userAlpha).
-define(USER_BETA, userBeta).
-define(PROV_GAMMA, providerGamma).
-define(PROV_DELTA, providerDelta).

-define(ROOT_TOKEN(Provider), {root_token, Provider}).
-define(TOKENS_OF(UserOrProvider), {tokens_of, UserOrProvider}).
% Cluster member with admin privileges
-define(ADMIN_OF(Cluster), {admin_of, Cluster}).
% Cluster member without admin privileges
-define(MEMBER_OF(Cluster), {member_of, Cluster}).
% A user that has a space supported by the provider
-define(SUPPORTED_USER(Provider), {supported_user, Provider}).

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
    tokens_to_check = [] :: [#named_token_data{} | {tokens_of, client_binding()} | {root_token, client_binding()}],
    % Decides if there is one test for all tokens_to_check, or a test for each
    % of the tokens
    testing_strategy :: one_token_at_a_time | all_tokens_at_the_same_time,
    % Id of the subject whose tokens are tested - macro binding
    % (?USER_ALPHA, ?PROV_DELTA etc.). It is resolved and fed to logic_args_generator)
    subject_id = undefined,
    % Clients that are allowed to perform the operation expressed by macro bindings
    % (?USER_ALPHA, ?PROVIDER_GAMMA...), ?ADMIN_OF, ?MEMBER_OF or ?SUPPORTED_USER
    correct_clients = [] :: client_binding(),
    % token_logic function to call
    logic_function :: atom(),
    % Returns logic function call args based on resolved subject_id (UserId / ProviderId)
    % and the expanded list of tokens to check
    logic_args_generator = fun(_, _) -> [] end :: logic_args_generator(),
    % Returns logic_expectation based on the expanded list of tokens to check
    logic_expectation_generator = fun(_) -> ?OK end :: logic_expectation_generator(),
    % HTTP method of the REST call
    rest_method = get :: http_client:method(),
    % Return the REST path to call based on resolved subject_id (UserId / ProviderId)
    % and the expanded list of tokens to check
    rest_path_generator = fun(_, _) -> <<"/">> end :: rest_path_generator(),
    % Indicates a REST path where given client can ask for his own resources
    rest_current_client_path_generator = undefined :: undefined | rest_current_client_path_generator(),
    % Returns the expected HTTP code and body based on the expanded list of tokens to check
    rest_expectation_generator = fun(_) -> {?HTTP_200_OK, undefined} end :: rest_expectation_generator(),
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
    prov_gamma_cluster_admin :: od_user:id(),
    prov_gamma_cluster_member :: od_user:id(),
    prov_gamma_supported_user :: od_user:id(),
    prov_delta :: od_provider:id(),
    prov_delta_token :: tokens:serialized(),
    prov_delta_cluster_admin :: od_user:id(),
    prov_delta_cluster_member :: od_user:id(),
    prov_delta_supported_user :: od_user:id()
}).

% Clients are represented by macro bindings (which are atoms underneath) and mapped
% to actual clients in the test run logic.
-type client_binding() :: atom().
% Depends on the testing strategy: one_token_at_a_time | all_tokens_at_the_same_time
-type token_or_tokens() :: #named_token_data{} | [#named_token_data{}].

-type logic_args_generator() :: fun((SubjectId :: binary(), token_or_tokens()) -> [term()]).
-type logic_expectation_generator() :: fun((token_or_tokens()) -> logic_expectation()).

-type rest_path_generator() :: fun((SubjectId :: binary(), token_or_tokens()) -> binary() | [binary()]).
-type rest_current_client_path_generator() :: fun((token_or_tokens()) -> {client_binding(), binary() | [binary()]}).
-type rest_expectation_generator() :: fun((token_or_tokens()) -> {HttpCode :: integer(), rest_expectation()}).

-type env_set_up_fun_generator() :: fun((token_or_tokens()) -> undefined | api_test_utils:env_setup_fun()).
-type verify_end_fun_generator() :: fun((token_or_tokens()) -> undefined | api_test_utils:verify_fun()).

-define(TYPES_TO_JSON(Types), [token_type:to_json(T) || T <- Types]).
-define(CAVEATS_TO_JSON(Caveats), [caveats:to_json(C) || C <- Caveats]).

-define(INVITE_TOKEN_TYPE_EXAMPLES(GroupId, SpaceId, AdminUserId, ClusterId, HarvesterId), [
    ?INVITE_TOKEN(?USER_JOIN_GROUP, GroupId),
    ?INVITE_TOKEN(?GROUP_JOIN_GROUP, GroupId),
    ?INVITE_TOKEN(?USER_JOIN_SPACE, SpaceId),
    ?INVITE_TOKEN(?GROUP_JOIN_SPACE, SpaceId),
    ?INVITE_TOKEN(?SUPPORT_SPACE, SpaceId),
    ?INVITE_TOKEN(?REGISTER_ONEPROVIDER, AdminUserId),
    ?INVITE_TOKEN(?USER_JOIN_CLUSTER, ClusterId),
    ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, ClusterId),
    ?INVITE_TOKEN(?USER_JOIN_HARVESTER, HarvesterId),
    ?INVITE_TOKEN(?GROUP_JOIN_HARVESTER, HarvesterId),
    ?INVITE_TOKEN(?SPACE_JOIN_HARVESTER, HarvesterId)
]).

-define(PROVIDER_ALLOWED_INVITE_TOKEN_TYPES(ClusterId), [
    ?INVITE_TOKEN(?USER_JOIN_CLUSTER, ClusterId),
    ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, ClusterId)
]).

-define(CAVEATS_EXAMPLES_FOR_NAMED_TOKENS(), [
    [],
    ?CAVEATS_TO_JSON([
        #cv_time{valid_until = ozt:timestamp_seconds() + 130},
        #cv_service{whitelist = [?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]}
    ]),
    ?CAVEATS_TO_JSON([
        #cv_time{valid_until = ozt:timestamp_seconds() + 560},
        #cv_api{whitelist = [
            {?OZ_WORKER, all, ?GRI_PATTERN(od_user, <<"123">>, <<"instance">>, private)}
        ]},
        #cv_data_readonly{},
        #cv_data_path{whitelist = [<<"/ab/cdef/g">>, <<"/1/2">>]},
        #cv_data_objectid{whitelist = [?RAND_OBJECTID, ?RAND_OBJECTID]}
    ])
]).

% Temporary tokens must have a sane time caveat
-define(CAVEATS_EXAMPLES_FOR_TEMPORARY_TOKENS(Now, MaxTtl), [
    ?CAVEATS_TO_JSON([
        #cv_time{valid_until = Now + 917}
    ]),
    ?CAVEATS_TO_JSON([
        #cv_time{valid_until = Now + MaxTtl - 7},
        #cv_service{whitelist = [?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]}
    ]),
    ?CAVEATS_TO_JSON([
        #cv_time{valid_until = Now + 156},
        #cv_api{whitelist = [
            {?OP_PANEL, create, ?GRI_PATTERN('*', <<"*">>, <<"*">>, '*')},
            {all, get, ?GRI_PATTERN(od_user, <<"*">>, <<"*">>, private)}
        ]},
        #cv_data_readonly{},
        #cv_data_path{whitelist = [<<"/767t2r3g6asd78asd/13123/dir/file.txt">>]},
        #cv_data_objectid{whitelist = [?RAND_OBJECTID]}
    ])
]).

-define(CUSTOM_METADATA_EXAMPLES, [
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

-define(BAD_TYPE_VALUES, [
    {<<"type">>, <<>>, ?ERROR_BAD_VALUE_TOKEN_TYPE(<<"type">>)},
    {<<"type">>, 123123, ?ERROR_BAD_VALUE_TOKEN_TYPE(<<"type">>)},
    {<<"type">>, <<"dsfdsfasdf">>, ?ERROR_BAD_VALUE_TOKEN_TYPE(<<"type">>)},
    {<<"type">>, token_type:to_json(?INVITE_TOKEN(?USER_JOIN_GROUP, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, token_type:to_json(?INVITE_TOKEN(?GROUP_JOIN_GROUP, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, token_type:to_json(?INVITE_TOKEN(?USER_JOIN_SPACE, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, token_type:to_json(?INVITE_TOKEN(?GROUP_JOIN_SPACE, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, token_type:to_json(?INVITE_TOKEN(?SUPPORT_SPACE, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, token_type:to_json(?INVITE_TOKEN(?REGISTER_ONEPROVIDER, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, token_type:to_json(?INVITE_TOKEN(?USER_JOIN_CLUSTER, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, token_type:to_json(?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, token_type:to_json(?INVITE_TOKEN(?USER_JOIN_HARVESTER, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, token_type:to_json(?INVITE_TOKEN(?GROUP_JOIN_HARVESTER, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, token_type:to_json(?INVITE_TOKEN(?SPACE_JOIN_HARVESTER, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)}
]).

-define(BAD_TYPE_VALUES_FOR_PROVIDER(GroupId, SpaceId, AdminUserId, ClusterId, HarvesterId),
    lists:map(
        fun(Type) ->
            {<<"type">>, token_type:to_json(Type), ?ERROR_INVITE_TOKEN_SUBJECT_NOT_AUTHORIZED}
        end,
        ?INVITE_TOKEN_TYPE_EXAMPLES(GroupId, SpaceId, AdminUserId, ClusterId, HarvesterId) --
        ?PROVIDER_ALLOWED_INVITE_TOKEN_TYPES(ClusterId)
    )
).

-define(BAD_CAVEATS_VALUES, [
    {<<"caveats">>, <<>>, ?ERROR_BAD_DATA(<<"caveats">>)},
    {<<"caveats">>, 345345, ?ERROR_BAD_DATA(<<"caveats">>)},
    {<<"caveats">>, 1.56, ?ERROR_BAD_DATA(<<"caveats">>)},
    {<<"caveats">>, <<"string literal">>, ?ERROR_BAD_DATA(<<"caveats">>)},
    {<<"caveats">>, #{}, ?ERROR_BAD_DATA(<<"caveats">>)},
    {<<"caveats">>, [#{}], ?ERROR_BAD_VALUE_CAVEAT(#{})},
    {<<"caveats">>, [#{<<"a">> => <<"b">>}], ?ERROR_BAD_VALUE_CAVEAT(#{<<"a">> => <<"b">>})},
    {<<"caveats">>, [#{<<"type">> => <<"tiem">>}], ?ERROR_BAD_VALUE_CAVEAT(#{<<"type">> => <<"tiem">>})},
    {<<"caveats">>, [1, 2, 3], ?ERROR_BAD_VALUE_CAVEAT(<<"1">>)},
    {<<"caveats">>, [<<"a">>, <<"b">>, <<"c">>], ?ERROR_BAD_VALUE_CAVEAT(<<"a">>)},
    {<<"caveats">>, [<<"time > 1234">>], ?ERROR_BAD_VALUE_CAVEAT(<<"time > 1234">>)},
    {<<"caveats">>, [<<"ip = 1.2.3.4.5">>], ?ERROR_BAD_VALUE_CAVEAT(<<"ip = 1.2.3.4.5">>)}
]).

-define(BAD_CUSTOM_METADATA_VALUES, [
    {<<"customMetadata">>, <<>>, ?ERROR_BAD_VALUE_JSON(<<"customMetadata">>)},
    {<<"customMetadata">>, 345345, ?ERROR_BAD_VALUE_JSON(<<"customMetadata">>)},
    {<<"customMetadata">>, 1.56, ?ERROR_BAD_VALUE_JSON(<<"customMetadata">>)},
    {<<"customMetadata">>, <<"string literal">>, ?ERROR_BAD_VALUE_JSON(<<"customMetadata">>)}
]).

-define(BAD_REVOKED_VALUES, [
    {<<"revoked">>, <<>>, ?ERROR_BAD_VALUE_BOOLEAN(<<"revoked">>)},
    {<<"revoked">>, 345345, ?ERROR_BAD_VALUE_BOOLEAN(<<"revoked">>)},
    {<<"revoked">>, 1.56, ?ERROR_BAD_VALUE_BOOLEAN(<<"revoked">>)},
    {<<"revoked">>, <<"string literal">>, ?ERROR_BAD_VALUE_BOOLEAN(<<"revoked">>)}
]).


-spec new_token_verify_fun(aai:subject(), named | temporary) ->
    fun((term()) -> boolean()).
new_token_verify_fun(Subject, Persistence) ->
    new_token_verify_fun(Subject, Persistence, ?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)).

-spec new_token_verify_fun(aai:subject(), named | temporary, undefined | aai:service_spec()) ->
    fun((term()) -> boolean()).
new_token_verify_fun(Subject, Persistence, Service) ->
    % Accepts all types of operation result:
    %   1) #token{} record (returned from logic)
    %   2) Serialized token
    %   3) REST response for temporary token (object with <<"token">> key)
    %   4) REST response for named token (object with <<"token">> and <<"tokenId">> keys)
    fun
        Fun(#{<<"token">> := Serialized, <<"tokenId">> := TokenId}) ->
            ?assertMatch({ok, #token{id = TokenId}}, tokens:deserialize(Serialized)),
            Fun(Serialized);

        Fun(#{<<"token">> := Serialized}) ->
            Fun(Serialized);

        Fun(Serialized) when is_binary(Serialized) ->
            {ok, Token} = tokens:deserialize(Serialized),
            Fun(Token);

        Fun(Token = #token{id = TokenId}) ->
            case Token#token.type of
                ?ACCESS_TOKEN ->
                    ?assertMatch({true, #auth{subject = Subject}}, ozt_tokens:authenticate(Token, #auth_ctx{
                        service = Service, data_access_caveats_policy = allow_data_access_caveats, session_id = any
                    }));
                _ ->
                    % IDENTITY_TOKEN, INVITE_TOKEN
                    ok
            end,
            case Persistence of
                named ->
                    {ok, #document{value = NamedToken}} = ozt:rpc(od_token, get, [TokenId]),
                    ?assertEqual(Token, ozt:rpc(od_token, named_token_to_token, [TokenId, NamedToken])),
                    true;
                temporary ->
                    true
            end
    end.


token_data_to_token(#named_token_data{token = Token}) ->
    token_data_to_token(Token);
token_data_to_token(Token) ->
    ozt_tokens:ensure_deserialized(Token).


token_data_to_id(TokenData) ->
    #token{id = TokenId} = token_data_to_token(TokenData),
    TokenId.


token_data_to_name(#named_token_data{name = Name}) ->
    Name.


create_user_named_token(UserId, Type) ->
    create_user_named_token(UserId, Type, []).
create_user_named_token(UserId, Type, Caveats) ->
    create_user_named_token(UserId, ?UNIQUE_STRING, Type, Caveats).
create_user_named_token(UserId, Name, Type, Caveats) ->
    Token = ozt_tokens:create(named, ?SUB(user, UserId), #{
        <<"name">> => Name, <<"type">> => Type, <<"caveats">> => Caveats
    }),
    #named_token_data{token = Token, name = Name}.


create_provider_named_token(ProviderId, Type) ->
    create_provider_named_token(ProviderId, Type, []).
create_provider_named_token(ProviderId, Type, Caveats) ->
    create_provider_named_token(ProviderId, ?UNIQUE_STRING, Type, Caveats).
create_provider_named_token(ProviderId, Name, Type, Caveats) ->
    Token = ozt_tokens:create(named, ?SUB(?ONEPROVIDER, ProviderId), #{
        <<"name">> => Name, <<"type">> => Type, <<"caveats">> => Caveats
    }),
    #named_token_data{token = Token, name = Name}.


create_user_temporary_token(UserId, Type) ->
    create_user_temporary_token(UserId, Type, []).
create_user_temporary_token(UserId, Type, Caveats) ->
    ozt_tokens:create(temporary, ?SUB(user, UserId), Type, Caveats).


create_provider_temporary_token(ProviderId, Type) ->
    create_provider_temporary_token(ProviderId, Type, []).
create_provider_temporary_token(ProviderId, Type, Caveats) ->
    ozt_tokens:create(temporary, ?SUB(?ONEPROVIDER, ProviderId), Type, Caveats).


% Creates some named and temporary tokens for users and providers from a basic env setup.
% Returns the named tokens represented by #named_token_data{} records.
-spec create_some_tokens(#basic_env{}) ->
    #{gri:entity_id() => [#named_token_data{}]}.
create_some_tokens(BasicEnv) ->
    #basic_env{
        user_alpha = UserAlpha, user_beta = UserBeta,

        prov_gamma = PrGamma, prov_gamma_cluster_admin = PrGammaAdmin,
        prov_gamma_cluster_member = PrGammaMember, prov_gamma_supported_user = SupportedByPrGamma,

        prov_delta = PrDelta, prov_delta_cluster_admin = PrDeltaAdmin,
        prov_delta_cluster_member = PrDeltaMember, prov_delta_supported_user = SupportedByPrDelta
    } = BasicEnv,

    SessionAlpha = ozt_http:simulate_login(UserAlpha),
    SessionBeta = ozt_http:simulate_login(UserBeta),

    SpaceAlpha = ozt_users:create_space_for(UserAlpha),
    GroupBeta = ozt_users:create_group_for(UserBeta),

    UAlphaT1 = create_user_named_token(UserAlpha, ?ACCESS_TOKEN),
    UAlphaT2 = create_user_named_token(UserAlpha, ?ACCESS_TOKEN),
    UAlphaT3 = create_user_named_token(UserAlpha, ?INVITE_TOKEN(?USER_JOIN_SPACE, SpaceAlpha)),
    UAlphaT4 = create_user_named_token(UserAlpha, ?INVITE_TOKEN(?GROUP_JOIN_SPACE, SpaceAlpha)),
    UAlphaT5 = create_user_named_token(UserAlpha, ?INVITE_TOKEN(?GROUP_JOIN_SPACE, SpaceAlpha)),

    UBetaT1 = create_user_named_token(UserBeta, ?ACCESS_TOKEN),
    UBetaT2 = create_user_named_token(UserBeta, ?ACCESS_TOKEN),
    UBetaT3 = create_user_named_token(UserBeta, ?ACCESS_TOKEN),
    UBetaT4 = create_user_named_token(UserBeta, ?INVITE_TOKEN(?USER_JOIN_GROUP, GroupBeta)),
    UBetaT5 = create_user_named_token(UserBeta, ?INVITE_TOKEN(?USER_JOIN_GROUP, GroupBeta)),
    UBetaT6 = create_user_named_token(UserBeta, ?INVITE_TOKEN(?USER_JOIN_GROUP, GroupBeta)),
    UBetaT7 = create_user_named_token(UserBeta, ?INVITE_TOKEN(?GROUP_JOIN_GROUP, GroupBeta)),

    PrGammaT1 = create_provider_named_token(PrGamma, ?ACCESS_TOKEN),
    PrGammaT2 = create_provider_named_token(PrGamma, ?INVITE_TOKEN(?USER_JOIN_CLUSTER, PrGamma)),

    PrGammaAdminT1 = create_user_named_token(PrGammaAdmin, ?INVITE_TOKEN(?USER_JOIN_CLUSTER, PrGamma)),
    PrGammaAdminT2 = create_user_named_token(PrGammaAdmin, ?ACCESS_TOKEN),

    PrGammaMemberT1 = create_user_named_token(PrGammaMember, ?ACCESS_TOKEN),

    SupportedByPrGammaT1 = create_user_named_token(SupportedByPrGamma, ?ACCESS_TOKEN),

    PrDeltaT1 = create_provider_named_token(PrDelta, ?ACCESS_TOKEN),
    PrDeltaT2 = create_provider_named_token(PrDelta, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, PrDelta)),

    PrDeltaAdminT1 = create_user_named_token(PrDeltaAdmin, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, PrDelta)),
    PrDeltaAdminT2 = create_user_named_token(PrDeltaAdmin, ?ACCESS_TOKEN),

    PrDeltaMemberT1 = create_user_named_token(PrDeltaMember, ?ACCESS_TOKEN),

    SupportedByPrDeltaT1 = create_user_named_token(SupportedByPrDelta, ?ACCESS_TOKEN),

    % Create some temporary tokens, which should not be listed
    create_user_temporary_token(UserAlpha, ?ACCESS_TOKEN),
    create_user_temporary_token(UserAlpha, ?ACCESS_TOKEN(SessionAlpha)),
    create_user_temporary_token(UserAlpha, ?INVITE_TOKEN(?USER_JOIN_SPACE, SpaceAlpha)),
    create_user_temporary_token(UserAlpha, ?INVITE_TOKEN(?GROUP_JOIN_SPACE, SpaceAlpha)),
    create_user_temporary_token(UserAlpha, ?INVITE_TOKEN(?GROUP_JOIN_SPACE, SpaceAlpha)),

    create_user_temporary_token(UserBeta, ?ACCESS_TOKEN),
    create_user_temporary_token(UserBeta, ?ACCESS_TOKEN(SessionBeta)),
    create_user_temporary_token(UserBeta, ?INVITE_TOKEN(?USER_JOIN_GROUP, GroupBeta)),
    create_user_temporary_token(UserBeta, ?INVITE_TOKEN(?USER_JOIN_GROUP, GroupBeta)),
    create_user_temporary_token(UserBeta, ?INVITE_TOKEN(?USER_JOIN_GROUP, GroupBeta)),
    create_user_temporary_token(UserBeta, ?INVITE_TOKEN(?GROUP_JOIN_GROUP, GroupBeta)),

    create_provider_temporary_token(PrGamma, ?ACCESS_TOKEN),
    create_provider_temporary_token(PrGamma, ?INVITE_TOKEN(?USER_JOIN_CLUSTER, PrGamma)),

    create_provider_temporary_token(PrDelta, ?ACCESS_TOKEN),
    create_provider_temporary_token(PrDelta, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, PrDelta)),

    create_user_temporary_token(PrGammaMember, ?ACCESS_TOKEN),
    create_user_temporary_token(PrDeltaMember, ?ACCESS_TOKEN),
    create_user_temporary_token(SupportedByPrGamma, ?ACCESS_TOKEN),
    create_user_temporary_token(SupportedByPrDelta, ?ACCESS_TOKEN),

    #{
        UserAlpha => [UAlphaT1, UAlphaT2, UAlphaT3, UAlphaT4, UAlphaT5],
        UserBeta => [UBetaT1, UBetaT2, UBetaT3, UBetaT4, UBetaT5, UBetaT6, UBetaT7],
        PrGamma => [PrGammaT1, PrGammaT2],
        PrGammaAdmin => [PrGammaAdminT1, PrGammaAdminT2],
        PrGammaMember => [PrGammaMemberT1],
        SupportedByPrGamma => [SupportedByPrGammaT1],
        PrDelta => [PrDeltaT1, PrDeltaT2],
        PrDeltaAdmin => [PrDeltaAdminT1, PrDeltaAdminT2],
        PrDeltaMember => [PrDeltaMemberT1],
        SupportedByPrDelta => [SupportedByPrDeltaT1]
    }.


create_basic_env() ->
    UserAlpha = ozt_users:create(),
    UserBeta = ozt_users:create(),
    PrGammaClusterAdmin = ozt_users:create(),
    PrDeltaClusterAdmin = ozt_users:create(),
    PrGammaClusterMember = ozt_users:create(),
    PrDeltaClusterMember = ozt_users:create(),
    SupportedByPrGamma = ozt_users:create(),
    SupportedByPrDelta = ozt_users:create(),
    PrGamma = ozt_providers:create_for_admin_user(PrGammaClusterAdmin),
    PrDelta = ozt_providers:create_for_admin_user(PrDeltaClusterAdmin),
    ozt_clusters:add_user(PrGamma, PrGammaClusterMember, privileges:cluster_member()),
    ozt_clusters:add_user(PrDelta, PrDeltaClusterMember, privileges:cluster_member()),
    ozt_providers:set_up_support_for_user(PrGamma, SupportedByPrGamma),
    ozt_providers:set_up_support_for_user(PrDelta, SupportedByPrDelta),
    #basic_env{
        user_alpha = UserAlpha,
        user_beta = UserBeta,
        prov_gamma = PrGamma,
        prov_gamma_token = ozt_providers:get_root_token(PrGamma),
        prov_gamma_cluster_admin = PrGammaClusterAdmin,
        prov_gamma_cluster_member = PrGammaClusterMember,
        prov_gamma_supported_user = SupportedByPrGamma,
        prov_delta = PrDelta,
        prov_delta_token = ozt_providers:get_root_token(PrDelta),
        prov_delta_cluster_admin = PrDeltaClusterAdmin,
        prov_delta_cluster_member = PrDeltaClusterMember,
        prov_delta_supported_user = SupportedByPrDelta
    }.


map_client(#basic_env{user_alpha = Id}, ?USER_ALPHA) -> {user, Id};
map_client(#basic_env{user_beta = Id}, ?USER_BETA) -> {user, Id};
map_client(#basic_env{prov_gamma = Id}, ?PROV_GAMMA) -> {provider, Id};
map_client(#basic_env{prov_gamma_cluster_admin = Id}, ?ADMIN_OF(?PROV_GAMMA)) -> {user, Id};
map_client(#basic_env{prov_gamma_cluster_member = Id}, ?MEMBER_OF(?PROV_GAMMA)) -> {user, Id};
map_client(#basic_env{prov_gamma_supported_user = Id}, ?SUPPORTED_USER(?PROV_GAMMA)) -> {user, Id};
map_client(#basic_env{prov_delta = Id}, ?PROV_DELTA) -> {provider, Id};
map_client(#basic_env{prov_delta_cluster_admin = Id}, ?ADMIN_OF(?PROV_DELTA)) -> {user, Id};
map_client(#basic_env{prov_delta_cluster_member = Id}, ?MEMBER_OF(?PROV_DELTA)) -> {user, Id};
map_client(#basic_env{prov_delta_supported_user = Id}, ?SUPPORTED_USER(?PROV_DELTA)) -> {user, Id}.


all_clients(BasicEnv) ->
    ClientBindings = [
        ?USER_ALPHA, ?USER_BETA,
        ?PROV_GAMMA, ?PROV_DELTA,
        ?ADMIN_OF(?PROV_GAMMA), ?ADMIN_OF(?PROV_DELTA),
        ?MEMBER_OF(?PROV_GAMMA), ?MEMBER_OF(?PROV_DELTA),
        ?SUPPORTED_USER(?PROV_GAMMA), ?SUPPORTED_USER(?PROV_DELTA)
    ],
    [map_client(BasicEnv, C) || C <- ClientBindings].


% Common function for running test related to tokens.
% Covers get / update / delete operations.
% See the #token_api_test_spec{} record for description.
run_token_tests(TestSpec) ->
    #basic_env{
        user_alpha = UserAlpha,
        user_beta = UserBeta,
        prov_gamma = PrGamma,
        prov_gamma_token = PrGammaToken,
        prov_gamma_cluster_admin = PrGammaClusterAdmin,
        prov_gamma_cluster_member = PrGammaClusterMember,
        prov_gamma_supported_user = SupportedByPrGamma,
        prov_delta = PrDelta,
        prov_delta_token = PrDeltaToken,
        prov_delta_cluster_admin = PrDeltaClusterAdmin,
        prov_delta_cluster_member = PrDeltaClusterMember,
        prov_delta_supported_user = SupportedByPrDelta
    } = BasicEnv = create_basic_env(),

    #{
        UserAlpha := UserAlphaNamedTokens,
        UserBeta := UserBetaNamedTokens,
        PrGamma := PrGammaNamedTokens,
        PrGammaClusterAdmin := PrGammaAdminNamedTokens,
        PrGammaClusterMember := PrGammaMemberNamedTokens,
        SupportedByPrGamma := SupportedByPrGammaNamedTokens,
        PrDelta := PrDeltaNamedTokens,
        PrDeltaClusterAdmin := PrDeltaAdminNamedTokens,
        PrDeltaClusterMember := PrDeltaMemberNamedTokens,
        SupportedByPrDelta := SupportedByPrDeltaNamedTokens
    } = create_some_tokens(BasicEnv),

    #token_api_test_spec{
        tokens_to_check = TokensToCheckBindings,
        testing_strategy = TestingStrategy,
        subject_id = SubjectIdBinding,
        correct_clients = CorrectClientsBindings,
        logic_function = LogicFunction,
        logic_args_generator = LogicArgsGenerator,
        logic_expectation_generator = LogicExpectationGenerator,
        rest_method = RestMethod,
        rest_path_generator = RestPathGenerator,
        rest_current_client_path_generator = RestCurrentClientPathGenerator,
        rest_expectation_generator = RestExpectationGenerator,
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
                {provider, ProviderId} -> ProviderId
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
        (?TOKENS_OF(?MEMBER_OF(?PROV_GAMMA))) ->
            PrGammaMemberNamedTokens;
        (?TOKENS_OF(?SUPPORTED_USER(?PROV_GAMMA))) ->
            SupportedByPrGammaNamedTokens;
        (?TOKENS_OF(?PROV_DELTA)) ->
            PrDeltaNamedTokens;
        (?TOKENS_OF(?ADMIN_OF(?PROV_DELTA))) ->
            PrDeltaAdminNamedTokens;
        (?TOKENS_OF(?MEMBER_OF(?PROV_DELTA))) ->
            PrDeltaMemberNamedTokens;
        (?TOKENS_OF(?SUPPORTED_USER(?PROV_DELTA))) ->
            SupportedByPrDeltaNamedTokens;
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
        {ExpCode, ExpBody} = RestExpectationGenerator(TokenOrTokens),
        ApiTestSpec = #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {admin, [?OZ_TOKENS_MANAGE]} |
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
            rest_spec = RestSpec = #rest_spec{
                method = RestMethod,
                path = RestPathGenerator(SubjectId, TokenOrTokens),
                expected_code = ExpCode,
                expected_body = ExpBody
            },
            data_spec = DataSpec
        },
        Result = api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun),

        case RestCurrentClientPathGenerator of
            undefined ->
                Result;
            _ ->
                {ClientBinding, Path} = RestCurrentClientPathGenerator(TokenOrTokens),
                CurrentClientApiTestSpec = ApiTestSpec#api_test_spec{
                    client_spec = #client_spec{
                        correct = [map_client(BasicEnv, ClientBinding)]
                    },
                    logic_spec = undefined,
                    rest_spec = RestSpec#rest_spec{
                        path = Path
                    }
                },
                Result andalso api_test_utils:run_tests(
                    ozt:get_test_config(), CurrentClientApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
                )
        end
    end, Testcases).

%%%===================================================================
%%% Test functions
%%%===================================================================

create_user_named_token(_Config) ->
    User = ozt_users:create(),
    ProviderAdmin = ozt_users:create(),
    Provider = ozt_providers:create_for_admin_user(ProviderAdmin),
    Group = ozt_users:create_group_for(User),
    Space = ozt_users:create_space_for(User),
    Harvester = ozt_users:create_harvester_for(User),
    Cluster = Provider,
    ozt_clusters:add_user(Cluster, User, privileges:cluster_admin()),
    ozt:reconcile_entity_graph(),

    AlreadyExistingName = <<"alreadyExistingName">>,
    create_user_named_token(User, AlreadyExistingName, ?ACCESS_TOKEN, []),

    VerifyFun = new_token_verify_fun(?SUB(user, User), named),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User},
                {admin, [?OZ_TOKENS_MANAGE, ?OZ_PROVIDERS_INVITE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {provider, Provider},
                {user, ProviderAdmin}
            ]
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = create_user_named_token,
            args = [auth, User, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        rest_spec = RestSpec = #rest_spec{
            method = post,
            path = [<<"/users/">>, User, <<"/tokens/named/">>],
            expected_code = ?HTTP_201_CREATED,
            expected_body = VerifyFun
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            optional = [<<"type">>, <<"caveats">>, <<"metadata">>],
            correct_values = #{
                <<"name">> => [fun() -> ?UNIQUE_STRING end],
                <<"type">> => ?TYPES_TO_JSON([
                    ?ACCESS_TOKEN |
                    ?INVITE_TOKEN_TYPE_EXAMPLES(Group, Space, User, Cluster, Harvester)
                ]),
                <<"caveats">> => ?CAVEATS_EXAMPLES_FOR_NAMED_TOKENS(),
                <<"metadata">> => ?CUSTOM_METADATA_EXAMPLES
            },
            % If the expected result is an error, do no feed bad values
            bad_values = lists:flatten([
                {<<"name">>, AlreadyExistingName, ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"name">>)},
                ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME),
                ?BAD_TYPE_VALUES,
                ?BAD_CAVEATS_VALUES,
                ?BAD_CUSTOM_METADATA_VALUES
            ])
        }
    },
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec)),
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{user, User}]
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/user/tokens/named/">>
        }
    })).

create_provider_named_token(_Config) ->
    User = ozt_users:create(),
    ProviderAdmin = ozt_users:create(),
    Provider = ozt_providers:create_for_admin_user(ProviderAdmin),
    AnotherProvider = ozt_providers:create(),
    Group = ozt_users:create_group_for(User),
    Space = ozt_users:create_space_for(User),
    Harvester = ozt_users:create_harvester_for(User),
    Cluster = Provider,

    AlreadyExistingName = <<"subdubpubsubbub">>,
    create_provider_named_token(Provider, AlreadyExistingName, ?ACCESS_TOKEN, []),

    VerifyFun = new_token_verify_fun(?SUB(?ONEPROVIDER, Provider), named),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, Provider},
                {user, ProviderAdmin},
                {admin, [?OZ_TOKENS_MANAGE, ?OZ_PROVIDERS_INVITE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, User},
                {provider, AnotherProvider}
            ]
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = create_provider_named_token,
            args = [auth, Provider, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        rest_spec = RestSpec = #rest_spec{
            method = post,
            path = [<<"/providers/">>, Provider, <<"/tokens/named/">>],
            expected_code = ?HTTP_201_CREATED,
            expected_body = VerifyFun
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            optional = [<<"type">>, <<"caveats">>, <<"metadata">>],
            correct_values = #{
                <<"name">> => [fun() -> ?UNIQUE_STRING end],
                <<"type">> => ?TYPES_TO_JSON([
                    ?ACCESS_TOKEN |
                    ?PROVIDER_ALLOWED_INVITE_TOKEN_TYPES(Cluster)
                ]),
                <<"caveats">> => ?CAVEATS_EXAMPLES_FOR_NAMED_TOKENS(),
                <<"metadata">> => ?CUSTOM_METADATA_EXAMPLES
            },
            bad_values = lists:flatten([
                {<<"name">>, AlreadyExistingName, ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"name">>)},
                ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME),
                ?BAD_TYPE_VALUES,
                ?BAD_TYPE_VALUES_FOR_PROVIDER(Group, Space, User, Cluster, Harvester),
                ?BAD_CAVEATS_VALUES,
                ?BAD_CUSTOM_METADATA_VALUES
            ])
        }
    },

    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec)),
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, Provider}]
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/provider/tokens/named/">>
        }
    })).


create_user_temporary_token(_Config) ->
    User = ozt_users:create(),
    SessionId = ozt_http:simulate_login(User),
    ProviderAdmin = ozt_users:create(),
    Provider = ozt_providers:create_for_admin_user(ProviderAdmin),
    Group = ozt_users:create_group_for(User),
    Space = ozt_users:create_space_for(User),
    Harvester = ozt_users:create_harvester_for(User),
    Cluster = Provider,
    ozt_clusters:add_user(Cluster, User, privileges:cluster_admin()),
    ozt:reconcile_entity_graph(),

    Now = ozt:timestamp_seconds(),
    MaxTtl = ozt:get_env(max_temporary_token_ttl),

    VerifyFun = new_token_verify_fun(?SUB(user, User), temporary),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User},
                {admin, [?OZ_TOKENS_MANAGE, ?OZ_PROVIDERS_INVITE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {provider, Provider},
                {user, ProviderAdmin}
            ]
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = create_user_temporary_token,
            args = [auth, User, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        rest_spec = RestSpec = #rest_spec{
            method = post,
            path = [<<"/users/">>, User, <<"/tokens/temporary/">>],
            expected_code = ?HTTP_201_CREATED,
            expected_body = VerifyFun
        },
        data_spec = #data_spec{
            % Caveats are required because of a mandatory time caveat
            required = [<<"caveats">>],
            optional = [<<"type">>],
            correct_values = #{
                <<"type">> => ?TYPES_TO_JSON([
                    ?ACCESS_TOKEN,
                    ?ACCESS_TOKEN(SessionId) |
                    ?INVITE_TOKEN_TYPE_EXAMPLES(Group, Space, User, Cluster, Harvester)
                ]),
                <<"caveats">> => ?CAVEATS_EXAMPLES_FOR_TEMPORARY_TOKENS(Now, MaxTtl)
            },
            bad_values = ?BAD_TYPE_VALUES ++ ?BAD_CAVEATS_VALUES ++ [
                {<<"caveats">>, [], ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl)},
                {<<"caveats">>, ?CAVEATS_TO_JSON([
                    #cv_service{whitelist = [?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]},
                    #cv_time{valid_until = Now + MaxTtl + 1}
                ]), ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl)}
            ]
        }
    },

    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec)),
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{user, User}]
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/user/tokens/temporary/">>
        }
    })).


create_provider_temporary_token(_Config) ->
    User = ozt_users:create(),
    SessionId = ozt_http:simulate_login(User),
    ProviderAdmin = ozt_users:create(),
    Provider = ozt_providers:create_for_admin_user(ProviderAdmin),
    AnotherProvider = ozt_providers:create(),
    Group = ozt_users:create_group_for(User),
    Space = ozt_users:create_space_for(User),
    Harvester = ozt_users:create_harvester_for(User),
    Cluster = Provider,

    Now = ozt:timestamp_seconds(),
    MaxTtl = ozt:get_env(max_temporary_token_ttl),

    VerifyFun = new_token_verify_fun(?SUB(?ONEPROVIDER, Provider), temporary),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, Provider},
                {user, ProviderAdmin},
                {admin, [?OZ_TOKENS_MANAGE, ?OZ_PROVIDERS_INVITE]}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, User},
                {provider, AnotherProvider}
            ]
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = create_provider_temporary_token,
            args = [auth, Provider, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        rest_spec = RestSpec = #rest_spec{
            method = post,
            path = [<<"/providers/">>, Provider, <<"/tokens/temporary/">>],
            expected_code = ?HTTP_201_CREATED,
            expected_body = VerifyFun
        },
        data_spec = #data_spec{
            % Caveats are required because of a mandatory time caveat
            required = [<<"caveats">>],
            optional = [<<"type">>],
            correct_values = #{
                <<"type">> => ?TYPES_TO_JSON([
                    ?ACCESS_TOKEN |
                    ?PROVIDER_ALLOWED_INVITE_TOKEN_TYPES(Cluster)
                ]),
                <<"caveats">> => ?CAVEATS_EXAMPLES_FOR_TEMPORARY_TOKENS(Now, MaxTtl)
            },
            bad_values = lists:flatten([
                ?BAD_TYPE_VALUES,
                ?BAD_TYPE_VALUES_FOR_PROVIDER(Group, Space, User, Cluster, Harvester),
                ?BAD_CAVEATS_VALUES,
                [
                    {<<"type">>, token_type:to_json(?ACCESS_TOKEN(SessionId)), ?ERROR_TOKEN_SESSION_INVALID},
                    {<<"caveats">>, [], ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl)},
                    {<<"caveats">>, ?CAVEATS_TO_JSON([
                        #cv_service{whitelist = [?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]},
                        #cv_time{valid_until = Now + MaxTtl + 50}
                    ]), ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl)}
                ]
            ])
        }
    },

    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec)),
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, Provider}]
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/provider/tokens/temporary/">>
        }
    })).


create_access_token_for_gui(_Config) ->
    ProviderAdmin = ozt_users:create(),
    AnotherUser = ozt_users:create(),
    SessionId = ozt_http:simulate_login(ProviderAdmin),
    Space = ozt_users:create_space_for(ProviderAdmin),
    Provider = ozt_providers:create_for_admin_user(ProviderAdmin),
    ozt_providers:support_space(Provider, Space),
    ozt_clusters:add_user(?ONEZONE_CLUSTER_ID, ProviderAdmin),
    ozt:reconcile_entity_graph(),

    AnotherProvider = ozt_providers:create(),

    VerifyFun = fun({Token, _Ttl}) ->
        [#cv_service{whitelist = [Service]}] = caveats:filter([cv_service], tokens:get_caveats(Token)),
        TokenVerifyFun = new_token_verify_fun(?SUB(user, ProviderAdmin), temporary, Service),
        TokenVerifyFun(Token)
    end,

    Testcases = [
        {?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID),
            ?OK_TERM(VerifyFun)},
        {?SERVICE(?OZ_PANEL, ?ONEZONE_CLUSTER_ID),
            ?OK_TERM(VerifyFun)},
        {?SERVICE(?OP_WORKER, Provider),
            ?OK_TERM(VerifyFun)},
        {?SERVICE(?OP_PANEL, Provider),
            ?OK_TERM(VerifyFun)},
        {?SERVICE(?OZ_WORKER, <<"123">>),
            ?ERROR_REASON(?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OZ_WORKER, <<"123">>)))},
        {?SERVICE(?OZ_PANEL, <<"123">>),
            ?ERROR_REASON(?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OZ_PANEL, <<"123">>)))},
        {?SERVICE(?OP_WORKER, AnotherProvider),
            ?ERROR_REASON(?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_WORKER, AnotherProvider)))},
        {?SERVICE(?OP_PANEL, AnotherProvider),
            ?ERROR_REASON(?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, AnotherProvider)))}
    ],

    lists:foreach(fun({Service, ExpResult}) ->
        ?assert(api_test_utils:run_tests(ozt:get_test_config(), #api_test_spec{
            client_spec = #client_spec{
                correct = [
                    root,
                    {user, ProviderAdmin},
                    {admin, [?OZ_TOKENS_MANAGE]}
                ],
                unauthorized = [nobody],
                forbidden = [
                    {provider, Provider},
                    {user, AnotherUser}
                ]
            },
            logic_spec = #logic_spec{
                module = token_logic,
                function = create_access_token_for_gui,
                args = [auth, ProviderAdmin, SessionId, Service],
                expected_result = ExpResult
            }
        }))
    end, Testcases).


list(_Config) ->
    ozt:delete_all_entities(),
    ?assert(run_token_tests(#token_api_test_spec{
        tokens_to_check = [
            ?ROOT_TOKEN(?PROV_GAMMA),
            ?ROOT_TOKEN(?PROV_DELTA),
            ?TOKENS_OF(?USER_ALPHA), ?TOKENS_OF(?USER_BETA),

            ?TOKENS_OF(?PROV_GAMMA), ?TOKENS_OF(?ADMIN_OF(?PROV_GAMMA)),
            ?TOKENS_OF(?MEMBER_OF(?PROV_GAMMA)), ?TOKENS_OF(?SUPPORTED_USER(?PROV_GAMMA)),

            ?TOKENS_OF(?PROV_DELTA), ?TOKENS_OF(?ADMIN_OF(?PROV_DELTA)),
            ?TOKENS_OF(?MEMBER_OF(?PROV_DELTA)), ?TOKENS_OF(?SUPPORTED_USER(?PROV_DELTA))
        ],
        testing_strategy = all_tokens_at_the_same_time,
        correct_clients = [], % only root & admin are authorized
        logic_function = list,
        logic_args_generator = fun(_, _) -> [auth] end,
        logic_expectation_generator = fun(TokensToCheck) ->
            ?OK_LIST([token_data_to_id(T) || T <- TokensToCheck])
        end,
        rest_method = get,
        rest_path_generator = fun(_, _) -> <<"/tokens/named">> end,
        rest_expectation_generator = fun(TokensToCheck) ->
            {?HTTP_200_OK, #{<<"tokens">> => [token_data_to_id(T) || T <- TokensToCheck]}}
        end
    })).


list_user_named_tokens(_Config) ->
    list_user_named_tokens_base(?USER_ALPHA),
    list_user_named_tokens_base(?USER_BETA),
    list_user_named_tokens_base(?ADMIN_OF(?PROV_GAMMA)),
    list_user_named_tokens_base(?ADMIN_OF(?PROV_DELTA)),
    list_user_named_tokens_base(?MEMBER_OF(?PROV_GAMMA)),
    list_user_named_tokens_base(?MEMBER_OF(?PROV_DELTA)),
    list_user_named_tokens_base(?SUPPORTED_USER(?PROV_GAMMA)),
    list_user_named_tokens_base(?SUPPORTED_USER(?PROV_DELTA)).

list_user_named_tokens_base(User) ->
    ?assert(run_token_tests(#token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(User)],
        testing_strategy = all_tokens_at_the_same_time,
        subject_id = User,
        correct_clients = [User],
        logic_function = list_user_named_tokens,
        logic_args_generator = fun(UserId, _) -> [auth, UserId] end,
        logic_expectation_generator = fun(TokensToCheck) ->
            ?OK_LIST([token_data_to_id(T) || T <- TokensToCheck])
        end,
        rest_method = get,
        rest_path_generator = fun(UserId, _) -> [<<"/users/">>, UserId, <<"/tokens/named">>] end,
        rest_current_client_path_generator = fun(_) -> {User, <<"/user/tokens/named">>} end,
        rest_expectation_generator = fun(TokensToCheck) ->
            {?HTTP_200_OK, #{<<"tokens">> => [token_data_to_id(T) || T <- TokensToCheck]}}
        end
    })).


list_provider_named_tokens(_Config) ->
    list_provider_named_tokens(?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    list_provider_named_tokens(?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]).

list_provider_named_tokens(Provider, CorrectClients) ->
    ?assert(run_token_tests(#token_api_test_spec{
        tokens_to_check = [?ROOT_TOKEN(Provider), ?TOKENS_OF(Provider)],
        testing_strategy = all_tokens_at_the_same_time,
        subject_id = Provider,
        correct_clients = CorrectClients,
        logic_function = list_provider_named_tokens,
        logic_args_generator = fun(ProviderId, _) -> [auth, ProviderId] end,
        logic_expectation_generator = fun(TokensToCheck) ->
            ?OK_LIST([token_data_to_id(T) || T <- TokensToCheck])
        end,
        rest_method = get,
        rest_path_generator = fun(PrId, _) -> [<<"/providers/">>, PrId, <<"/tokens/named">>] end,
        rest_current_client_path_generator = fun(_) -> {Provider, <<"/provider/tokens/named">>} end,
        rest_expectation_generator = fun(TokensToCheck) ->
            {?HTTP_200_OK, #{<<"tokens">> => [token_data_to_id(T) || T <- TokensToCheck]}}
        end
    })).


get_named_token(_Config) ->
    get_named_token(?USER_ALPHA, [?USER_ALPHA]),
    get_named_token(?USER_BETA, [?USER_BETA]),
    get_named_token(?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    get_named_token(?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]),
    get_named_token(?ADMIN_OF(?PROV_GAMMA), [?ADMIN_OF(?PROV_GAMMA)]),
    get_named_token(?ADMIN_OF(?PROV_DELTA), [?ADMIN_OF(?PROV_DELTA)]),
    get_named_token(?MEMBER_OF(?PROV_GAMMA), [?MEMBER_OF(?PROV_GAMMA)]),
    get_named_token(?MEMBER_OF(?PROV_DELTA), [?MEMBER_OF(?PROV_DELTA)]),
    get_named_token(?SUPPORTED_USER(?PROV_GAMMA), [?SUPPORTED_USER(?PROV_GAMMA)]),
    get_named_token(?SUPPORTED_USER(?PROV_DELTA), [?SUPPORTED_USER(?PROV_DELTA)]).

get_named_token(UserOrProvider, CorrectClients) ->
    ?assert(run_token_tests(#token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(UserOrProvider)],
        testing_strategy = one_token_at_a_time,
        correct_clients = CorrectClients,
        logic_function = get_named_token,
        logic_args_generator = fun(_, TokenToCheck) ->
            [auth, token_data_to_id(TokenToCheck)]
        end,
        logic_expectation_generator = fun(TokenToCheck) ->
            ?OK_TERM(fun(Result) ->
                verify_named_token_data(logic, Result, TokenToCheck)
            end)
        end,
        rest_method = get,
        rest_path_generator = fun(_, TokenToCheck) -> [<<"/tokens/named/">>, token_data_to_id(TokenToCheck)] end,
        rest_expectation_generator = fun(TokenToCheck) ->
            {?HTTP_200_OK, fun(Result) ->
                verify_named_token_data(rest, Result, TokenToCheck)
            end}
        end
    })).


get_named_token_status(_Config) ->
    get_named_token_status(?USER_ALPHA, [?USER_ALPHA]),
    get_named_token_status(?USER_BETA, [?USER_BETA]),
    get_named_token_status(?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA), ?MEMBER_OF(?PROV_GAMMA)]),
    get_named_token_status(?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA), ?MEMBER_OF(?PROV_DELTA)]),
    get_named_token_status(?ADMIN_OF(?PROV_GAMMA), [?ADMIN_OF(?PROV_GAMMA)]),
    get_named_token_status(?ADMIN_OF(?PROV_DELTA), [?ADMIN_OF(?PROV_DELTA)]),
    get_named_token_status(?MEMBER_OF(?PROV_GAMMA), [?MEMBER_OF(?PROV_GAMMA)]),
    get_named_token_status(?MEMBER_OF(?PROV_DELTA), [?MEMBER_OF(?PROV_DELTA)]),
    get_named_token_status(?SUPPORTED_USER(?PROV_GAMMA), [?SUPPORTED_USER(?PROV_GAMMA), ?PROV_GAMMA]),
    get_named_token_status(?SUPPORTED_USER(?PROV_DELTA), [?SUPPORTED_USER(?PROV_DELTA), ?PROV_DELTA]).

get_named_token_status(UserOrProvider, CorrectClients) ->
    VerifyFun = fun(Result) ->
        ?assertEqual(#{<<"revoked">> => false}, Result),
        true
    end,
    ?assert(run_token_tests(#token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(UserOrProvider)],
        testing_strategy = one_token_at_a_time,
        correct_clients = CorrectClients,
        logic_function = get_named_token_status,
        logic_args_generator = fun(_, TokenToCheck) ->
            [auth, token_data_to_id(TokenToCheck)]
        end,
        logic_expectation_generator = fun(_TokenToCheck) -> ?OK_TERM(VerifyFun) end,
        rest_method = get,
        rest_path_generator = fun(_, TokenToCheck) ->
            [<<"/tokens/named/">>, token_data_to_id(TokenToCheck), <<"/status">>]
        end,
        rest_expectation_generator = fun(_TokenToCheck) -> {?HTTP_200_OK, VerifyFun} end
    })).


get_user_named_token_by_name(_Config) ->
    get_user_named_token_by_name_base(?USER_ALPHA),
    get_user_named_token_by_name_base(?USER_BETA),
    get_user_named_token_by_name_base(?ADMIN_OF(?PROV_GAMMA)),
    get_user_named_token_by_name_base(?ADMIN_OF(?PROV_DELTA)),
    get_user_named_token_by_name_base(?MEMBER_OF(?PROV_GAMMA)),
    get_user_named_token_by_name_base(?MEMBER_OF(?PROV_DELTA)),
    get_user_named_token_by_name_base(?SUPPORTED_USER(?PROV_GAMMA)),
    get_user_named_token_by_name_base(?SUPPORTED_USER(?PROV_DELTA)).

get_user_named_token_by_name_base(User) ->
    ?assert(run_token_tests(#token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(User)],
        testing_strategy = one_token_at_a_time,
        subject_id = User,
        correct_clients = [User],
        logic_function = get_user_named_token_by_name,
        logic_args_generator = fun(UserId, TokenToCheck) ->
            [auth, UserId, token_data_to_name(TokenToCheck)]
        end,
        logic_expectation_generator = fun(TokenToCheck) ->
            ?OK_TERM(fun(Result) ->
                verify_named_token_data(logic, Result, TokenToCheck)
            end)
        end,
        rest_method = get,
        rest_path_generator = fun(UserId, TokenToCheck) ->
            [<<"/users/">>, UserId, <<"/tokens/named/name/">>, token_data_to_name(TokenToCheck)]
        end,
        rest_current_client_path_generator = fun(TokenToCheck) ->
            {User, [<<"/user/tokens/named/name/">>, token_data_to_name(TokenToCheck)]}
        end,
        rest_expectation_generator = fun(TokenToCheck) ->
            {?HTTP_200_OK, fun(Result) ->
                verify_named_token_data(rest, Result, TokenToCheck)
            end}
        end
    })).


get_provider_named_token_by_name(_Config) ->
    get_provider_named_token_by_name(?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    get_provider_named_token_by_name(?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]).

get_provider_named_token_by_name(Provider, CorrectClients) ->
    ?assert(run_token_tests(#token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(Provider)],
        testing_strategy = one_token_at_a_time,
        subject_id = Provider,
        correct_clients = CorrectClients,
        logic_function = get_provider_named_token_by_name,
        logic_args_generator = fun(ProviderId, TokenToCheck) ->
            [auth, ProviderId, token_data_to_name(TokenToCheck)]
        end,
        logic_expectation_generator = fun(TokenToCheck) ->
            ?OK_TERM(fun(Result) ->
                verify_named_token_data(logic, Result, TokenToCheck)
            end)
        end,
        rest_method = get,
        rest_path_generator = fun(PrId, TokenToCheck) ->
            [<<"/providers/">>, PrId, <<"/tokens/named/name/">>, token_data_to_name(TokenToCheck)]
        end,
        rest_current_client_path_generator = fun(TokenToCheck) ->
            {Provider, [<<"/provider/tokens/named/name/">>, token_data_to_name(TokenToCheck)]}
        end,
        rest_expectation_generator = fun(TokenToCheck) ->
            {?HTTP_200_OK, fun(Result) ->
                verify_named_token_data(rest, Result, TokenToCheck)
            end}
        end
    })).


get_user_temporary_token_generation(_Config) ->
    BasicEnv = create_basic_env(),
    get_user_temporary_token_generation(BasicEnv, ?USER_ALPHA, [?USER_ALPHA]),
    get_user_temporary_token_generation(BasicEnv, ?USER_BETA, [?USER_BETA]),
    get_user_temporary_token_generation(BasicEnv, ?ADMIN_OF(?PROV_GAMMA), [?ADMIN_OF(?PROV_GAMMA)]),
    get_user_temporary_token_generation(BasicEnv, ?ADMIN_OF(?PROV_DELTA), [?ADMIN_OF(?PROV_DELTA)]),
    get_user_temporary_token_generation(BasicEnv, ?MEMBER_OF(?PROV_GAMMA), [?MEMBER_OF(?PROV_GAMMA)]),
    get_user_temporary_token_generation(BasicEnv, ?MEMBER_OF(?PROV_DELTA), [?MEMBER_OF(?PROV_DELTA)]),
    get_user_temporary_token_generation(BasicEnv, ?SUPPORTED_USER(?PROV_GAMMA), [?SUPPORTED_USER(?PROV_GAMMA), ?PROV_GAMMA]),
    get_user_temporary_token_generation(BasicEnv, ?SUPPORTED_USER(?PROV_DELTA), [?SUPPORTED_USER(?PROV_DELTA), ?PROV_DELTA]).

get_user_temporary_token_generation(BasicEnv, UserIdBinding, CorrectClientsBindings) ->
    {user, UserId} = map_client(BasicEnv, UserIdBinding),

    CorrectClients = [map_client(BasicEnv, C) || C <- CorrectClientsBindings],
    ForbiddenClients = all_clients(BasicEnv) -- CorrectClients,

    {ok, CurrentGeneration} = ozt:rpc(token_logic, get_user_temporary_token_generation, [?ROOT, UserId]),
    RevocationCount = rand:uniform(15),
    lists:foreach(fun(_) ->
        ozt_tokens:revoke_all_temporary_tokens(?SUB(user, UserId))
    end, lists:seq(1, RevocationCount)),
    ExpGeneration = CurrentGeneration + RevocationCount,

    ?assertMatch(
        #token{persistence = {temporary, ExpGeneration}},
        create_user_temporary_token(UserId, ?ACCESS_TOKEN)
    ),

    VerifyFun = fun(Generation) ->
        ?assertEqual(Generation, ExpGeneration),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_TOKENS_MANAGE]} |
                replace_temporary_tokens_with_named_for_clients(CorrectClients)
            ],
            unauthorized = [nobody],
            forbidden = replace_temporary_tokens_with_named_for_clients(ForbiddenClients)
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = get_user_temporary_token_generation,
            args = [auth, UserId],
            expected_result = ?OK_TERM(VerifyFun)
        },
        rest_spec = RestSpec = #rest_spec{
            method = get,
            path = [<<"/users/">>, UserId, <<"/tokens/temporary/">>],
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"generation">> := Generation}) ->
                VerifyFun(Generation)
            end
        }
    },
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec)),

    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, UserId}
            ]
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/user/tokens/temporary/">>
        }
    },
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec2)).


get_provider_temporary_token_generation(_Config) ->
    BasicEnv = create_basic_env(),
    get_provider_temporary_token_generation(BasicEnv, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA), ?MEMBER_OF(?PROV_GAMMA)]),
    get_provider_temporary_token_generation(BasicEnv, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA), ?MEMBER_OF(?PROV_DELTA)]).

get_provider_temporary_token_generation(BasicEnv, ProviderIdBinding, CorrectClientsBindings) ->
    {provider, ProviderId} = map_client(BasicEnv, ProviderIdBinding),

    CorrectClients = [map_client(BasicEnv, C) || C <- CorrectClientsBindings],
    ForbiddenClients = all_clients(BasicEnv) -- CorrectClients,

    {ok, CurrentGeneration} = ozt:rpc(token_logic, get_provider_temporary_token_generation, [?ROOT, ProviderId]),
    RevocationCount = rand:uniform(15),
    lists:foreach(fun(_) ->
        ozt_tokens:revoke_all_temporary_tokens(?SUB(?ONEPROVIDER, ProviderId))
    end, lists:seq(1, RevocationCount)),
    ExpGeneration = CurrentGeneration + RevocationCount,

    ?assertMatch(
        #token{persistence = {temporary, ExpGeneration}},
        create_provider_temporary_token(ProviderId, ?ACCESS_TOKEN)
    ),

    VerifyFun = fun(Generation) ->
        ?assertEqual(Generation, ExpGeneration),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_TOKENS_MANAGE]} |
                replace_temporary_tokens_with_named_for_clients(CorrectClients)
            ],
            unauthorized = [nobody],
            forbidden = replace_temporary_tokens_with_named_for_clients(ForbiddenClients)
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = get_provider_temporary_token_generation,
            args = [auth, ProviderId],
            expected_result = ?OK_TERM(VerifyFun)
        },
        rest_spec = RestSpec = #rest_spec{
            method = get,
            path = [<<"/providers/">>, ProviderId, <<"/tokens/temporary/">>],
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"generation">> := Generation}) ->
                VerifyFun(Generation)
            end
        }
    },
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec)),

    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                {provider, ProviderId}
            ]
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/provider/tokens/temporary/">>
        }
    },
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec2)).


verify_named_token_data(API, Result, TokenToCheck) when API == logic orelse API == rest ->
    Token = #token{
        subject = Subject,
        type = Type
    } = token_data_to_token(TokenToCheck),
    TokenId = token_data_to_id(TokenToCheck),
    Caveats = tokens:get_caveats(Token),
    TokenName = token_data_to_name(TokenToCheck),
    true = rest_test_utils:contains_map(Result, #{
        <<"name">> => TokenName,
        <<"id">> => TokenId,
        <<"subject">> => case API of
            logic -> Subject;
            rest -> aai:subject_to_json(Subject)
        end,
        <<"type">> => case API of
            logic -> Type;
            rest -> token_type:to_json(Type)
        end,
        <<"caveats">> => case API of
            logic -> Caveats;
            rest -> [caveats:to_json(C) || C <- Caveats]
        end,
        <<"revoked">> => false,
        <<"token">> => case API of
            logic -> Token;
            rest -> ozt_tokens:ensure_serialized(Token)
        end
    }),
    Metadata = maps:get(<<"metadata">>, Result),
    Now = ozt:timestamp_seconds(),
    case Type of
        ?INVITE_TOKEN ->
            ?assertMatch(#{
                <<"creationTime">> := Now,
                <<"custom">> := #{},
                <<"privileges">> := _,
                <<"usageLimit">> := <<"infinity">>,
                <<"usageCount">> := 0
            }, Metadata);
        _ ->
            ?assertMatch(#{
                <<"creationTime">> := Now,
                <<"custom">> := #{}
            }, Metadata)
    end,
    true.


update_named_token(_Config) ->
    update_named_token(?USER_ALPHA, [?USER_ALPHA]),
    update_named_token(?USER_BETA, [?USER_BETA]),
    update_named_token(?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    update_named_token(?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]),
    update_named_token(?ADMIN_OF(?PROV_GAMMA), [?ADMIN_OF(?PROV_GAMMA)]),
    update_named_token(?ADMIN_OF(?PROV_DELTA), [?ADMIN_OF(?PROV_DELTA)]),
    update_named_token(?MEMBER_OF(?PROV_GAMMA), [?MEMBER_OF(?PROV_GAMMA)]),
    update_named_token(?MEMBER_OF(?PROV_DELTA), [?MEMBER_OF(?PROV_DELTA)]),
    update_named_token(?SUPPORTED_USER(?PROV_GAMMA), [?SUPPORTED_USER(?PROV_GAMMA)]),
    update_named_token(?SUPPORTED_USER(?PROV_DELTA), [?SUPPORTED_USER(?PROV_DELTA)]).


update_named_token(SubjectId, CorrectClients) ->
    AlreadyExistingName = <<"alreadyExistingName123">>,
    ?assert(run_token_tests(#token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(SubjectId)],
        testing_strategy = one_token_at_a_time,
        subject_id = SubjectId,
        correct_clients = CorrectClients,
        logic_function = update_named_token,
        logic_args_generator = fun(_, TokenToCheck) ->
            [auth, token_data_to_id(TokenToCheck), data]
        end,
        logic_expectation_generator = fun(_) -> ?OK end,
        rest_method = patch,
        rest_path_generator = fun(_, TokenToCheck) ->
            [<<"/tokens/named/">>, token_data_to_id(TokenToCheck)]
        end,
        rest_expectation_generator = fun(_) ->
            {?HTTP_204_NO_CONTENT, undefined}
        end,
        data_spec = #data_spec{
            at_least_one = [<<"name">>, <<"customMetadata">>, <<"revoked">>],
            correct_values = #{
                <<"name">> => [fun() -> ?UNIQUE_STRING end],
                <<"customMetadata">> => ?CUSTOM_METADATA_EXAMPLES,
                <<"revoked">> => [true, false]
            },
            bad_values = lists:flatten([
                ?BAD_CUSTOM_METADATA_VALUES,
                ?BAD_REVOKED_VALUES,
                ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
            ])
        },
        env_set_up_fun_generator = fun(TokenToCheck) ->
            fun() ->
                TokenId = token_data_to_id(TokenToCheck),
                {ok, #{
                    <<"name">> := Name,
                    <<"subject">> := Subject,
                    <<"revoked">> := Revoked,
                    <<"metadata">> := Metadata
                }} = ozt:rpc(token_logic, get_named_token, [?ROOT, TokenId]),
                % Create a token with AlreadyExistingName to check that other tokens
                % cannot be renamed to that name
                ozt_tokens:try_create(named, Subject, #{<<"name">> => AlreadyExistingName}),
                % Randomize the custom part of metadata
                MetadataWithRandomizedCustom = case rand:uniform(3) of
                    1 ->
                        Metadata;
                    2 ->
                        Metadata#{<<"custom">> => #{}};
                    3 ->
                        Metadata#{<<"custom">> => #{
                            <<"attr1">> => 123, <<"attr2">> => #{
                                <<"attr">> => <<"str">>
                            }
                        }}
                end,
                ozt:rpc(od_token, update, [TokenId, fun(Token) ->
                    {ok, Token#od_token{metadata = MetadataWithRandomizedCustom}}
                end]),

                #{
                    previousName => Name,
                    previousMetadata => MetadataWithRandomizedCustom,
                    previousRevoked => Revoked
                }
            end
        end,
        verify_end_fun_generator = fun(TokenToCheck) ->
            TokenId = token_data_to_id(TokenToCheck),
            fun(ShouldSucceed, Env, Data) ->
                PreviousName = maps:get(previousName, Env),
                PreviousMetadata = maps:get(previousMetadata, Env),
                PreviousRevoked = maps:get(previousRevoked, Env),
                ExpName = case ShouldSucceed of
                    false -> PreviousName;
                    true -> maps:get(<<"name">>, Data, PreviousName)
                end,
                ExpMetadata = case ShouldSucceed of
                    false ->
                        PreviousMetadata;
                    true ->
                        case maps:find(<<"customMetadata">>, Data) of
                            {ok, NewCustom} -> PreviousMetadata#{<<"custom">> => NewCustom};
                            error -> PreviousMetadata
                        end
                end,
                ExpRevoked = case ShouldSucceed of
                    false -> PreviousRevoked;
                    true -> maps:get(<<"revoked">>, Data, PreviousRevoked)
                end,
                ?assertMatch({ok, #{
                    <<"name">> := ExpName,
                    <<"revoked">> := ExpRevoked,
                    <<"metadata">> := ExpMetadata
                }}, ozt:rpc(token_logic, get_named_token, [?ROOT, TokenId])),
                true
            end
        end
    })).


delete_named_token(_Config) ->
    BasicEnv = create_basic_env(),
    delete_named_token(BasicEnv, ?USER_ALPHA, [?USER_ALPHA]),
    delete_named_token(BasicEnv, ?USER_BETA, [?USER_BETA]),
    delete_named_token(BasicEnv, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    delete_named_token(BasicEnv, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]),
    delete_named_token(BasicEnv, ?ADMIN_OF(?PROV_GAMMA), [?ADMIN_OF(?PROV_GAMMA)]),
    delete_named_token(BasicEnv, ?ADMIN_OF(?PROV_DELTA), [?ADMIN_OF(?PROV_DELTA)]),
    delete_named_token(BasicEnv, ?MEMBER_OF(?PROV_GAMMA), [?MEMBER_OF(?PROV_GAMMA)]),
    delete_named_token(BasicEnv, ?MEMBER_OF(?PROV_DELTA), [?MEMBER_OF(?PROV_DELTA)]),
    delete_named_token(BasicEnv, ?SUPPORTED_USER(?PROV_GAMMA), [?SUPPORTED_USER(?PROV_GAMMA)]),
    delete_named_token(BasicEnv, ?SUPPORTED_USER(?PROV_DELTA), [?SUPPORTED_USER(?PROV_DELTA)]).

delete_named_token(BasicEnv, SubjectIdBinding, CorrectClientsBindings) ->
    CorrectClients = [map_client(BasicEnv, C) || C <- CorrectClientsBindings],
    EnvSetUpFun = fun() ->
        #named_token_data{token = Token, name = TokenName} = case map_client(BasicEnv, SubjectIdBinding) of
            {user, UserId} -> create_user_named_token(UserId, ?ACCESS_TOKEN);
            {provider, ProviderId} -> create_provider_named_token(ProviderId, ?ACCESS_TOKEN)
        end,
        ?SUB(_, SubjectId) = Token#token.subject,
        #{
            token => Token,
            % name, tokenId and subjectId are used in logic args
            tokenName => TokenName,
            tokenId => Token#token.id,
            subjectId => SubjectId
        }
    end,

    VerifyEndFun = fun(ShouldSucceed, #{token := Token, tokenName := TokenName}, _Data) ->
        assert_token_deleted(ShouldSucceed, Token, TokenName)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_TOKENS_MANAGE]} |
                CorrectClients
            ],
            unauthorized = [nobody],
            forbidden = all_clients(BasicEnv) -- CorrectClients
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = delete_named_token,
            args = [auth, tokenId],
            expected_result = ?OK
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/tokens/named/">>, tokenId],
            expected_code = ?HTTP_204_NO_CONTENT
        }
    },
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)).


delete_all_user_named_tokens(_Config) ->
    BasicEnv = create_basic_env(),
    delete_all_user_named_tokens(BasicEnv, ?USER_ALPHA),
    delete_all_user_named_tokens(BasicEnv, ?USER_BETA),
    delete_all_user_named_tokens(BasicEnv, ?ADMIN_OF(?PROV_GAMMA)),
    delete_all_user_named_tokens(BasicEnv, ?ADMIN_OF(?PROV_DELTA)),
    delete_all_user_named_tokens(BasicEnv, ?MEMBER_OF(?PROV_GAMMA)),
    delete_all_user_named_tokens(BasicEnv, ?MEMBER_OF(?PROV_DELTA)),
    delete_all_user_named_tokens(BasicEnv, ?SUPPORTED_USER(?PROV_GAMMA)),
    delete_all_user_named_tokens(BasicEnv, ?SUPPORTED_USER(?PROV_DELTA)).

delete_all_user_named_tokens(BasicEnv, UserIdBinding) ->
    {user, UserId} = map_client(BasicEnv, UserIdBinding),

    EnvSetUpFun = fun() ->
        #{UserId := UserTokens} = create_some_tokens(BasicEnv),
        #{userTokens => UserTokens}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{userTokens := UserTokens}, _Data) ->
        lists:foreach(fun(#named_token_data{name = TokenName, token = Token}) ->
            assert_token_deleted(ShouldSucceed, Token, TokenName)
        end, UserTokens)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_TOKENS_MANAGE]},
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
        },
        rest_spec = RestSpec = #rest_spec{
            method = delete,
            path = [<<"/users/">>, UserId, <<"/tokens/named/">>],
            expected_code = ?HTTP_204_NO_CONTENT
        }
    },
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)),

    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{user, UserId}]
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/user/tokens/named/">>
        }
    },
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun)).


delete_all_provider_named_tokens(_Config) ->
    BasicEnv = create_basic_env(),
    delete_all_provider_named_tokens(BasicEnv, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    delete_all_provider_named_tokens(BasicEnv, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]).

delete_all_provider_named_tokens(BasicEnv, ProviderIdBinding, CorrectClientsBindings) ->
    {provider, ProviderId} = map_client(BasicEnv, ProviderIdBinding),

    CorrectClients = [map_client(BasicEnv, C) || C <- CorrectClientsBindings],
    ForbiddenClients = all_clients(BasicEnv) -- CorrectClients,

    EnvSetUpFun = fun() ->
        #{ProviderId := ProviderTokens} = create_some_tokens(BasicEnv),
        #{providerTokens => ProviderTokens}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{providerTokens := ProviderTokens}, _Data) ->
        lists:foreach(fun(#named_token_data{name = TokenName, token = Token}) ->
            assert_token_deleted(ShouldSucceed, Token, TokenName)
        end, ProviderTokens)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_TOKENS_MANAGE]} |
                CorrectClients
            ],
            unauthorized = [nobody],
            forbidden = ForbiddenClients
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = delete_all_provider_named_tokens,
            args = [auth, ProviderId],
            expected_result = ?OK
        },
        rest_spec = RestSpec = #rest_spec{
            method = delete,
            path = [<<"/providers/">>, ProviderId, <<"/tokens/named/">>],
            expected_code = ?HTTP_204_NO_CONTENT
        }
    },
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)),

    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                {provider, ProviderId}
            ]
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/provider/tokens/named/">>
        }
    },
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun)).


assert_token_deleted(true, Token, TokenName) ->
    #token{subject = Subject, id = TokenId, type = Type} = Token,
    ?assertMatch({error, not_found}, ozt:rpc(od_token, get, [TokenId])),
    ?assertMatch({error, not_found}, ozt:rpc(token_names, lookup, [Subject, TokenName])),
    case Type of
        ?ACCESS_TOKEN ->
            ?assertEqual(?ERROR_UNAUTHORIZED(?ERROR_TOKEN_INVALID), ozt_tokens:authenticate(Token));
        _ ->
            ok
    end;
assert_token_deleted(false, Token, TokenName) ->
    #token{subject = Subject, id = TokenId, type = Type} = Token,
    ?assertMatch({ok, _}, ozt:rpc(od_token, get, [TokenId])),
    ?assertMatch({ok, TokenId}, ozt:rpc(token_names, lookup, [Subject, TokenName])),
    case Type of
        ?ACCESS_TOKEN ->
            ?assertEqual({true, #auth{subject = Subject}}, ozt_tokens:authenticate(Token));
        _ ->
            ok
    end.


revoke_all_user_temporary_tokens(_Config) ->
    BasicEnv = create_basic_env(),
    revoke_all_user_temporary_tokens(BasicEnv, ?USER_ALPHA),
    revoke_all_user_temporary_tokens(BasicEnv, ?USER_BETA),
    revoke_all_user_temporary_tokens(BasicEnv, ?ADMIN_OF(?PROV_GAMMA)),
    revoke_all_user_temporary_tokens(BasicEnv, ?ADMIN_OF(?PROV_DELTA)),
    revoke_all_user_temporary_tokens(BasicEnv, ?MEMBER_OF(?PROV_GAMMA)),
    revoke_all_user_temporary_tokens(BasicEnv, ?MEMBER_OF(?PROV_DELTA)),
    revoke_all_user_temporary_tokens(BasicEnv, ?SUPPORTED_USER(?PROV_GAMMA)),
    revoke_all_user_temporary_tokens(BasicEnv, ?SUPPORTED_USER(?PROV_DELTA)).

revoke_all_user_temporary_tokens(BasicEnv, UserIdBinding) ->
    {user, UserId} = map_client(BasicEnv, UserIdBinding),
    SessionId = ozt_http:simulate_login(UserId),

    CorrectClients = [{user, UserId}],
    ForbiddenClients = all_clients(BasicEnv) -- CorrectClients,

    EnvSetUpFun = fun() ->
        Space = ozt_users:create_space_for(UserId),
        T1 = create_user_temporary_token(UserId, ?ACCESS_TOKEN),
        T2 = create_user_temporary_token(UserId, ?ACCESS_TOKEN(SessionId)),
        T3 = create_user_temporary_token(UserId, ?INVITE_TOKEN(?USER_JOIN_SPACE, Space)),
        #{userTokens => [T1, T2, T3]}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{userTokens := UserTokens}, _Data) ->
        lists:foreach(fun(Token) ->
            assert_temporary_token_revoked(ShouldSucceed, Token)
        end, UserTokens)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_TOKENS_MANAGE]} |
                % Use a named token for authorizing the user as temporary tokens
                % get deleted in this test
                replace_temporary_tokens_with_named_for_clients(CorrectClients)
            ],
            unauthorized = [nobody],
            forbidden = replace_temporary_tokens_with_named_for_clients(ForbiddenClients)
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = revoke_all_user_temporary_tokens,
            args = [auth, UserId],
            expected_result = ?OK
        },
        rest_spec = RestSpec = #rest_spec{
            method = delete,
            path = [<<"/users/">>, UserId, <<"/tokens/temporary/">>],
            expected_code = ?HTTP_204_NO_CONTENT
        }
    },
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)),

    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = replace_temporary_tokens_with_named_for_clients([{user, UserId}])
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/user/tokens/temporary/">>
        }
    },
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun)).



revoke_all_provider_temporary_tokens(_Config) ->
    BasicEnv = create_basic_env(),
    revoke_all_provider_temporary_tokens(BasicEnv, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    revoke_all_provider_temporary_tokens(BasicEnv, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]).

revoke_all_provider_temporary_tokens(BasicEnv, ProviderIdBinding, CorrectClientsBindings) ->
    {provider, ProviderId} = map_client(BasicEnv, ProviderIdBinding),

    CorrectClients = [map_client(BasicEnv, C) || C <- CorrectClientsBindings],
    ForbiddenClients = all_clients(BasicEnv) -- CorrectClients,

    EnvSetUpFun = fun() ->
        T1 = create_provider_temporary_token(ProviderId, ?ACCESS_TOKEN),
        T2 = create_provider_temporary_token(ProviderId, ?ACCESS_TOKEN),
        T3 = create_provider_temporary_token(ProviderId, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, ProviderId)),
        #{providerTokens => [T1, T2, T3]}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{providerTokens := ProviderTokens}, _Data) ->
        lists:foreach(fun(Token) ->
            assert_temporary_token_revoked(ShouldSucceed, Token)
        end, ProviderTokens)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_TOKENS_MANAGE]} |
                replace_temporary_tokens_with_named_for_clients(CorrectClients)
            ],
            unauthorized = [nobody],
            forbidden = replace_temporary_tokens_with_named_for_clients(ForbiddenClients)
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = revoke_all_provider_temporary_tokens,
            args = [auth, ProviderId],
            expected_result = ?OK
        },
        rest_spec = RestSpec = #rest_spec{
            method = delete,
            path = [<<"/providers/">>, ProviderId, <<"/tokens/temporary/">>],
            expected_code = ?HTTP_204_NO_CONTENT
        }
    },
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)),

    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = replace_temporary_tokens_with_named_for_clients([{provider, ProviderId}])
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/provider/tokens/temporary/">>
        }
    },
    ?assert(api_test_utils:run_tests(ozt:get_test_config(), ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun)).


% Required in tests that revoke temporary tokens during the testing procedure,
% which are by default used by the test framework for REST calls.
replace_temporary_tokens_with_named_for_clients(Clients) ->
    lists:map(fun
        ({provider, PrId}) ->
            ProviderRootToken = ozt_providers:get_root_token(PrId),
            {provider, PrId, ozt_tokens:ensure_serialized(ProviderRootToken)};
        ({user, UId}) ->
            #named_token_data{token = UserNamedToken} = create_user_named_token(UId, ?ACCESS_TOKEN),
            {user, UId, ozt_tokens:ensure_serialized(UserNamedToken)}
    end, Clients).


assert_temporary_token_revoked(IsRevoked, Token = #token{subject = Subject}) ->
    VerificationResult = ozt_tokens:verify(Token),
    case IsRevoked of
        false -> ?assertMatch({ok, #{<<"subject">> := Subject}}, VerificationResult);
        true -> ?assertMatch(?ERROR_TOKEN_REVOKED, VerificationResult)
    end.
