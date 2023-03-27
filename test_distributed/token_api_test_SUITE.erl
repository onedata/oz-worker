%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning token API (logic).
%%% @end
%%%-------------------------------------------------------------------
-module(token_api_test_SUITE).
-author("Lukasz Opiola").

-include("ozt.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
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
    examine/1,
    confine/1,
    verify_access_token/1,
    verify_identity_token/1,
    verify_invite_token/1,
    create_user_named_token/1,
    create_provider_named_token/1,
    create_user_temporary_token/1,
    create_provider_temporary_token/1,
    create_access_token_for_gui/1,
    create_offline_user_access_token/1,
    list/1,
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
    revoke_all_provider_temporary_tokens/1
]).

all() ->
    ?ALL([
        examine,
        confine,
        verify_access_token,
        verify_identity_token,
        verify_invite_token,
        create_user_named_token,
        create_provider_named_token,
        create_user_temporary_token,
        create_provider_temporary_token,
        create_access_token_for_gui,
        create_offline_user_access_token,
        list,
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
    ]).

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


init_per_testcase(_, Config) ->
    ozt_mocks:freeze_time(),
    ozt_mocks:mock_harvesting_backends(),
    Config.


end_per_testcase(_, _Config) ->
    ozt_mocks:unfreeze_time(),
    ozt_mocks:unmock_harvesting_backends(),
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

-define(RAND_OBJECTID,
    element(2, {ok, _} = file_id:guid_to_objectid(
        file_id:pack_guid(str_utils:rand_hex(6), str_utils:rand_hex(6))
    ))
).

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

    #named_token_data{token = TokenBeta} = create_provider_named_token(
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
    #named_token_data{token = TokenBeta} = create_provider_named_token(
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

    #named_token_data{token = TokenBeta} = create_provider_named_token(
        Provider, ?ACCESS_TOKEN, [
            #cv_time{valid_until = ozt:timestamp_seconds() + 2700}
        ]
    ),
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

    #named_token_data{token = TokenDelta} = create_user_named_token(
        User, ?ACCESS_TOKEN, [
            #cv_time{valid_until = ozt:timestamp_seconds() - 1}
        ]
    ),
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

    #named_token_data{token = TokenKappa} = create_user_named_token(
        User, ?ACCESS_TOKEN, [
            #cv_service{whitelist = [?SERVICE(?OZ_PANEL, ?ONEZONE_CLUSTER_ID)]}
        ]
    ),
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

    #named_token_data{token = TokenOmega} = create_provider_named_token(Provider, ?ACCESS_TOKEN, [
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

    #named_token_data{token = TokenTau} = create_user_named_token(User, ?ACCESS_TOKEN, [
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

    #named_token_data{token = TokenBeta} = create_provider_named_token(
        Provider, ?IDENTITY_TOKEN, [
            #cv_time{valid_until = ozt:timestamp_seconds() + 2700}
        ]
    ),
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

    #named_token_data{token = TokenDelta} = create_user_named_token(
        User, ?IDENTITY_TOKEN, [
            #cv_time{valid_until = ozt:timestamp_seconds() - 1}
        ]
    ),
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
    #named_token_data{token = TokenOmega} = create_provider_named_token(Provider, ?ACCESS_TOKEN, [
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

    #named_token_data{token = TokenPi} = create_user_named_token(User, ?ACCESS_TOKEN, [
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

    #named_token_data{token = TokenBeta} = create_provider_named_token(
        Provider, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, Provider), [
            #cv_time{valid_until = ozt:timestamp_seconds() + 2700}
        ]
    ),
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

    #named_token_data{token = TokenDelta} = create_user_named_token(
        User, ?INVITE_TOKEN(?GROUP_JOIN_GROUP, Group), [
            #cv_time{valid_until = ozt:timestamp_seconds() - 1}
        ]
    ),
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

    #named_token_data{token = TokenKappa} = create_user_named_token(User, ?INVITE_TOKEN(?GROUP_JOIN_SPACE, Space), [
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

    #named_token_data{token = TokenOmega} = create_provider_named_token(
        Provider, ?INVITE_TOKEN(?USER_JOIN_CLUSTER, Provider), [
            #cv_scope{scope = identity_token}
        ]
    ),
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

    #named_token_data{token = TokenEpsilon} = create_user_named_token(SubjectUser, ?ACCESS_TOKEN, [
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

    #named_token_data{token = TokenGamma} = create_provider_named_token(
        AnotherProvider, ?ACCESS_TOKEN, [
            #cv_time{valid_until = ozt:timestamp_seconds() + 27000}
        ]
    ),
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

    #named_token_data{token = TokenKappa} = create_user_named_token(
        SubjectUser, ?ACCESS_TOKEN, [
            #cv_ip{whitelist = [{{146, 193, 14, 0}, 24}]}
        ]
    ),
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

    #named_token_data{token = TokenOmega} = create_user_named_token(
        SubjectUser, ?ACCESS_TOKEN, [
            #cv_interface{interface = rest},
            #cv_service{whitelist = [?SERVICE(?OZ_WORKER, ?ONEZONE_CLUSTER_ID), ?SERVICE(?OP_WORKER, RequestingProvider)]},
            #cv_data_readonly{}
        ]
    ),
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


get_testmaster_ip() ->
    {ok, [{IP, _, _} | _]} = inet:getif(),
    IP.


prepare_token_verification_logic_args(Auth, Data, _Env) ->
    [Auth#auth{peer_ip = get_testmaster_ip()}, Data].
