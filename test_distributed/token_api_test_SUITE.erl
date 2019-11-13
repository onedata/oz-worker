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
    examine/1,
    confine/1,
    verify_access_or_identity_token/1,
    verify_invite_token/1,
    create_user_named_token/1,
    create_provider_named_token/1,
    create_user_temporary_token/1,
    create_provider_temporary_token/1,
    create_gui_access_token/1,
    list/1,
    list_user_named_tokens/1,
    list_provider_named_tokens/1,
    get_named_token/1,
    get_user_named_token_by_name/1,
    get_provider_named_token_by_name/1,
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
        verify_access_or_identity_token,
        verify_invite_token,
        create_user_named_token,
        create_provider_named_token,
        create_user_temporary_token,
        create_provider_temporary_token,
        create_gui_access_token,
        list,
        list_user_named_tokens,
        list_provider_named_tokens,
        get_named_token,
        get_user_named_token_by_name,
        get_provider_named_token_by_name,
        update_named_token,
        delete_named_token,
        delete_all_user_named_tokens,
        delete_all_provider_named_tokens,
        revoke_all_user_temporary_tokens,
        revoke_all_provider_temporary_tokens
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
    tokens_to_check = [] :: [#named_token_data{} | {tokens_of, client_binding()} | {root_token, client_binding()}],
    % Decides if there is one test for all tokens_to_check, or a test for each
    % of the tokens
    testing_strategy :: one_token_at_a_time | all_tokens_at_the_same_time,
    % Id of the subject whose tokens are tested - macro binding
    % (?USER_ALPHA, ?PROV_DELTA etc.). It is resolved and fed to logic_args_generator)
    subject_id = undefined,
    % Clients that are allowed to perform the operation expressed by macro bindings
    % (?USER_ALPHA, ?PROVIDER_GAMMA...) or ?ADMIN_OF (which resolves to the provider's
    % cluster creator).
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
    prov_gamma_admin :: od_user:id(),
    prov_delta :: od_provider:id(),
    prov_delta_token :: tokens:serialized(),
    prov_delta_admin :: od_user:id()
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

-define(TYPES_TO_JSON(Types), [tokens:type_to_json(T) || T <- Types]).
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

-define(CAVEATS_EXAMPLES_FOR_NAMED_TOKENS(Config), [
    [],
    ?CAVEATS_TO_JSON([
        #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) + 130},
        #cv_audience{whitelist = [?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]}
    ]),
    ?CAVEATS_TO_JSON([
        #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) + 560},
        #cv_api{whitelist = [
            {?OZ_WORKER, all, ?GRI(od_user, <<"123">>, instance, private)}
        ]},
        #cv_data_space{whitelist = [<<"124234234">>, <<"opmxv7wenrfapfsa">>]},
        #cv_data_access{type = read},
        #cv_data_path{whitelist = [<<"/ab/cdef/g">>, <<"/1/2">>]},
        #cv_data_objectid{whitelist = [<<"12345">>, <<"67890">>]}
    ])
]).

% Temporary tokens must have a sane time caveat
-define(CAVEATS_EXAMPLES_FOR_TEMPORARY_TOKENS(Now, MaxTtl), [
    ?CAVEATS_TO_JSON([
        #cv_time{valid_until = Now + 917}
    ]),
    ?CAVEATS_TO_JSON([
        #cv_time{valid_until = Now + MaxTtl - 7},
        #cv_audience{whitelist = [?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]}
    ]),
    ?CAVEATS_TO_JSON([
        #cv_time{valid_until = Now + 156},
        #cv_api{whitelist = [
            {?OP_PANEL, create, ?GRI('*', <<"*">>, '*', '*')},
            {all, get, ?GRI(od_user, <<"*">>, '*', private)}
        ]},
        #cv_data_space{whitelist = [<<"8y978qtwda67d12g3">>]},
        #cv_data_access{type = write},
        #cv_data_path{whitelist = [<<84, 71, 3, 250, 218, 190, 67, 67, 45, 13>>]},
        #cv_data_objectid{whitelist = [<<"000779645781987512837492813641234234">>]}
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
    {<<"type">>, tokens:type_to_json(?INVITE_TOKEN(?USER_JOIN_GROUP, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, tokens:type_to_json(?INVITE_TOKEN(?GROUP_JOIN_GROUP, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, tokens:type_to_json(?INVITE_TOKEN(?USER_JOIN_SPACE, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, tokens:type_to_json(?INVITE_TOKEN(?GROUP_JOIN_SPACE, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, tokens:type_to_json(?INVITE_TOKEN(?SUPPORT_SPACE, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, tokens:type_to_json(?INVITE_TOKEN(?REGISTER_ONEPROVIDER, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, tokens:type_to_json(?INVITE_TOKEN(?USER_JOIN_CLUSTER, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, tokens:type_to_json(?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, tokens:type_to_json(?INVITE_TOKEN(?USER_JOIN_HARVESTER, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, tokens:type_to_json(?INVITE_TOKEN(?GROUP_JOIN_HARVESTER, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)},
    {<<"type">>, tokens:type_to_json(?INVITE_TOKEN(?SPACE_JOIN_HARVESTER, <<"123">>)), ?ERROR_INVITE_TOKEN_TARGET_ID_INVALID(<<"123">>)}
]).

-define(BAD_TYPE_VALUES_FOR_PROVIDER(GroupId, SpaceId, AdminUserId, ClusterId, HarvesterId),
    lists:map(
        fun(Type) ->
            {<<"type">>, tokens:type_to_json(Type), ?ERROR_INVITE_TOKEN_CREATOR_NOT_AUTHORIZED}
        end,
        ?INVITE_TOKEN_TYPE_EXAMPLES(GroupId, SpaceId, AdminUserId, ClusterId, HarvesterId) --
        ?PROVIDER_ALLOWED_INVITE_TOKEN_TYPES(ClusterId)
    )
).

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

-spec new_token_verify_fun(Config :: term(), aai:subject(), named | temporary) ->
    fun((term()) -> boolean()).
new_token_verify_fun(Config, Subject, Persistence) ->
    new_token_verify_fun(Config, Subject, Persistence, ?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)).

-spec new_token_verify_fun(Config :: term(), aai:subject(), named | temporary, undefined | aai:audience()) ->
    fun((term()) -> boolean()).
new_token_verify_fun(Config, Subject, Persistence, Audience) ->
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
                    {ok, #document{value = NamedToken}} = oz_test_utils:call_oz(Config, od_token, get, [TokenId]),
                    ?assertEqual(Token, oz_test_utils:call_oz(Config, od_token, named_token_to_token, [
                        TokenId, NamedToken
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


token_data_to_id(TokenData) ->
    #token{id = TokenId} = token_data_to_token(TokenData),
    TokenId.


token_data_to_name(#named_token_data{name = Name}) ->
    Name.


create_user_named_token(Config, UserId, Type) ->
    create_user_named_token(Config, UserId, Type, []).
create_user_named_token(Config, UserId, Type, Caveats) ->
    create_user_named_token(Config, UserId, ?UNIQUE_STRING, Type, Caveats).
create_user_named_token(Config, UserId, Name, Type, Caveats) ->
    {ok, Token} = oz_test_utils:call_oz(Config, token_logic, create_user_named_token, [
        ?USER(UserId), UserId, #{<<"name">> => Name, <<"type">> => Type, <<"caveats">> => Caveats}
    ]),
    #named_token_data{token = Token, name = Name}.


create_provider_named_token(Config, ProviderId, Type) ->
    create_provider_named_token(Config, ProviderId, Type, []).
create_provider_named_token(Config, ProviderId, Type, Caveats) ->
    create_provider_named_token(Config, ProviderId, ?UNIQUE_STRING, Type, Caveats).
create_provider_named_token(Config, ProviderId, Name, Type, Caveats) ->
    {ok, Token} = oz_test_utils:call_oz(Config, token_logic, create_provider_named_token, [
        ?PROVIDER(ProviderId), ProviderId, #{<<"name">> => Name, <<"type">> => Type, <<"caveats">> => Caveats}
    ]),
    #named_token_data{token = Token, name = Name}.


create_user_temporary_token(Config, UserId, Type) ->
    create_user_temporary_token(Config, UserId, Type, []).
create_user_temporary_token(Config, UserId, Type, Caveats) ->
    Now = oz_test_utils:cluster_time_seconds(Config),
    {ok, Token} = oz_test_utils:call_oz(Config, token_logic, create_user_temporary_token, [
        ?USER(UserId), UserId, #{
            <<"type">> => Type, <<"caveats">> => case caveats:find(cv_time, Caveats) of
                % Time caveat is required in temporary tokens - add one if the isn't any
                false -> [#cv_time{valid_until = Now + 3600} | Caveats];
                {true, _} -> Caveats
            end
        }
    ]),
    Token.


create_provider_temporary_token(Config, ProviderId, Type) ->
    create_provider_temporary_token(Config, ProviderId, Type, []).
create_provider_temporary_token(Config, ProviderId, Type, Caveats) ->
    Now = oz_test_utils:cluster_time_seconds(Config),
    {ok, Token} = oz_test_utils:call_oz(Config, token_logic, create_provider_temporary_token, [
        ?PROVIDER(ProviderId), ProviderId, #{
            <<"type">> => Type, <<"caveats">> => case caveats:find(cv_time, Caveats) of
                % Time caveat is required in temporary tokens - add one if there isn't any
                false -> [#cv_time{valid_until = Now + 3600} | Caveats];
                {true, _} -> Caveats
            end
        }
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
    UAlphaT3 = create_user_named_token(Config, UserAlpha, ?INVITE_TOKEN(?USER_JOIN_SPACE, SpaceAlpha)),
    UAlphaT4 = create_user_named_token(Config, UserAlpha, ?INVITE_TOKEN(?GROUP_JOIN_SPACE, SpaceAlpha)),
    UAlphaT5 = create_user_named_token(Config, UserAlpha, ?INVITE_TOKEN(?GROUP_JOIN_SPACE, SpaceAlpha)),

    UBetaT1 = create_user_named_token(Config, UserBeta, ?ACCESS_TOKEN),
    UBetaT2 = create_user_named_token(Config, UserBeta, ?ACCESS_TOKEN),
    UBetaT3 = create_user_named_token(Config, UserBeta, ?ACCESS_TOKEN),
    UBetaT4 = create_user_named_token(Config, UserBeta, ?INVITE_TOKEN(?USER_JOIN_GROUP, GroupBeta)),
    UBetaT5 = create_user_named_token(Config, UserBeta, ?INVITE_TOKEN(?USER_JOIN_GROUP, GroupBeta)),
    UBetaT6 = create_user_named_token(Config, UserBeta, ?INVITE_TOKEN(?USER_JOIN_GROUP, GroupBeta)),
    UBetaT7 = create_user_named_token(Config, UserBeta, ?INVITE_TOKEN(?GROUP_JOIN_GROUP, GroupBeta)),

    PrGammaT1 = create_provider_named_token(Config, PrGamma, ?ACCESS_TOKEN),
    PrGammaT2 = create_provider_named_token(Config, PrGamma, ?INVITE_TOKEN(?USER_JOIN_CLUSTER, PrGamma)),

    PrGammaAdminT1 = create_user_named_token(Config, PrGammaAdmin, ?INVITE_TOKEN(?USER_JOIN_CLUSTER, PrGamma)),
    PrGammaAdminT2 = create_user_named_token(Config, PrGammaAdmin, ?ACCESS_TOKEN),

    PrDeltaT1 = create_provider_named_token(Config, PrDelta, ?ACCESS_TOKEN),
    PrDeltaT2 = create_provider_named_token(Config, PrDelta, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, PrDelta)),

    PrDeltaAdminT1 = create_user_named_token(Config, PrDeltaAdmin, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, PrDelta)),
    PrDeltaAdminT2 = create_user_named_token(Config, PrDeltaAdmin, ?ACCESS_TOKEN),

    % Create some temporary tokens, which should not be listed
    create_user_temporary_token(Config, UserAlpha, ?ACCESS_TOKEN),
    create_user_temporary_token(Config, UserAlpha, ?GUI_ACCESS_TOKEN(SessionAlpha)),
    create_user_temporary_token(Config, UserAlpha, ?INVITE_TOKEN(?USER_JOIN_SPACE, SpaceAlpha)),
    create_user_temporary_token(Config, UserAlpha, ?INVITE_TOKEN(?GROUP_JOIN_SPACE, SpaceAlpha)),
    create_user_temporary_token(Config, UserAlpha, ?INVITE_TOKEN(?GROUP_JOIN_SPACE, SpaceAlpha)),

    create_user_temporary_token(Config, UserBeta, ?ACCESS_TOKEN),
    create_user_temporary_token(Config, UserBeta, ?GUI_ACCESS_TOKEN(SessionBeta)),
    create_user_temporary_token(Config, UserBeta, ?INVITE_TOKEN(?USER_JOIN_GROUP, GroupBeta)),
    create_user_temporary_token(Config, UserBeta, ?INVITE_TOKEN(?USER_JOIN_GROUP, GroupBeta)),
    create_user_temporary_token(Config, UserBeta, ?INVITE_TOKEN(?USER_JOIN_GROUP, GroupBeta)),
    create_user_temporary_token(Config, UserBeta, ?INVITE_TOKEN(?GROUP_JOIN_GROUP, GroupBeta)),

    create_provider_temporary_token(Config, PrGamma, ?ACCESS_TOKEN),
    create_provider_temporary_token(Config, PrGamma, ?INVITE_TOKEN(?USER_JOIN_CLUSTER, PrGamma)),

    create_provider_temporary_token(Config, PrDelta, ?ACCESS_TOKEN),
    create_provider_temporary_token(Config, PrDelta, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, PrDelta)),

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
        Result = api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun),

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
                    Config, CurrentClientApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
                )
        end
    end, Testcases).

%%%===================================================================
%%% Test functions
%%%===================================================================

examine(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, ProviderAdmin} = oz_test_utils:create_user(Config),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config, ProviderAdmin, ?UNIQUE_STRING),
    AllClients = [{user, User}, {provider, Provider, ProviderToken}, {user, ProviderAdmin}],

    TimeCaveat = #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) + 3600},
    TokenAlpha = create_user_temporary_token(Config, User, ?ACCESS_TOKEN, [TimeCaveat]),
    examine_base(Config, AllClients, TokenAlpha, #{
        <<"onezoneDomain">> => oz_test_utils:oz_domain(Config),
        <<"id">> => TokenAlpha#token.id,
        <<"persistence">> => <<"temporary">>,
        <<"subject">> => ?SUB(user, User),
        <<"type">> => ?ACCESS_TOKEN,
        <<"caveats">> => [TimeCaveat]
    }),

    #named_token_data{token = TokenBeta} = create_provider_named_token(
        Config, Provider, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, Provider), [
            #cv_audience{whitelist = [?AUD(user, <<"123456789">>)]},
            #cv_ip{whitelist = [{{181, 115, 16, 8}, 32}, {{181, 115, 16, 9}, 32}]},
            #cv_asn{whitelist = [854]},
            #cv_country{type = blacklist, list = [<<"PL">>, <<"PT">>, <<"ES">>]},
            #cv_region{type = whitelist, list = [<<"EU">>]}
        ]
    ),
    examine_base(Config, AllClients, TokenBeta, #{
        <<"onezoneDomain">> => oz_test_utils:oz_domain(Config),
        <<"id">> => TokenBeta#token.id,
        <<"persistence">> => <<"named">>,
        <<"subject">> => ?SUB(?ONEPROVIDER, Provider),
        <<"type">> => ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, Provider),
        <<"caveats">> => [
            #cv_audience{whitelist = [?AUD(user, <<"123456789">>)]},
            #cv_ip{whitelist = [{{181, 115, 16, 8}, 32}, {{181, 115, 16, 9}, 32}]},
            #cv_asn{whitelist = [854]},
            #cv_country{type = blacklist, list = [<<"PL">>, <<"PT">>, <<"ES">>]},
            #cv_region{type = whitelist, list = [<<"EU">>]}
        ]
    }).

examine_base(Config, AllClients, Token, ExpResult) ->
    ?assert(api_test_utils:run_tests(Config, #api_test_spec{
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
                <<"type">> => tokens:type_to_json(maps:get(<<"type">>, ExpResult)),
                <<"caveats">> => [caveats:to_json(C) || C <- maps:get(<<"caveats">>, ExpResult)]
            }
        },
        data_spec = #data_spec{
            required = [<<"token">>],
            correct_values = #{
                <<"token">> => [element(2, {ok, _} = tokens:serialize(Token))]
            },
            bad_values = [
                {<<"token">>, <<"1234">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)}
            ]
        }
    })).


confine(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, ProviderAdmin} = oz_test_utils:create_user(Config),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config, ProviderAdmin, ?UNIQUE_STRING),
    AllClients = [{user, User}, {provider, Provider, ProviderToken}, {user, ProviderAdmin}],

    InitialCaveatsAlpha = [
        #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) + 3600}
    ],
    TokenAlpha = create_user_temporary_token(Config, User, ?ACCESS_TOKEN, InitialCaveatsAlpha),
    confine_combinations(Config, AllClients, TokenAlpha, InitialCaveatsAlpha, [
        #cv_authorization_none{},
        #cv_audience{whitelist = [?AUD(group, <<"abderg">>), ?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]},
        #cv_api{whitelist = [{oz_worker, get, ?GRI(od_space, <<"*">>, '*', private)}]},
        #cv_data_space{whitelist = [<<"123">>, <<"456">>, <<"678">>]},
        #cv_data_access{type = read},
        #cv_data_path{whitelist = [base64:encode(<<"a/b/c/d">>)]},
        #cv_data_objectid{whitelist = [<<"abcdef">>, <<"ghijk">>]}
    ]),

    InitialCaveatsBeta = [
        #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) + 3600},
        #cv_ip{whitelist = [{{181, 115, 16, 8}, 32}, {{181, 115, 16, 9}, 32}]},
        #cv_country{type = blacklist, list = [<<"PL">>, <<"PT">>, <<"ES">>]},
        #cv_region{type = whitelist, list = [<<"EU">>]}
    ],
    #named_token_data{token = TokenBeta} = create_provider_named_token(
        Config, Provider, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, Provider), InitialCaveatsBeta
    ),
    confine_combinations(Config, AllClients, TokenBeta, InitialCaveatsBeta, [
        #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) + 1800},
        #cv_audience{whitelist = [?AUD(user, <<"123456789">>)]},
        #cv_asn{whitelist = [854]}
    ]).

confine_combinations(Config, AllClients, TokenAlpha, InitialCaveats, CaveatsToAdd) ->
    Combinations = [lists:sublist(CaveatsToAdd, L) || L <- lists:seq(1, length(CaveatsToAdd))],
    lists:foreach(fun(Combination) ->
        confine_base(Config, AllClients, TokenAlpha, InitialCaveats, Combination)
    end, Combinations).

confine_base(Config, AllClients, Token, InitialCaveats, CaveatsToAdd) ->
    VerifyFun = fun(ResultToken) ->
        ?assertEqual(InitialCaveats ++ CaveatsToAdd, tokens:get_caveats(ResultToken)),
        true
    end,

    ?assert(api_test_utils:run_tests(Config, #api_test_spec{
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
                Deserialized = element(2, {ok, _} = tokens:deserialize(Serialized)),
                VerifyFun(Deserialized)
            end
        },
        data_spec = #data_spec{
            required = [<<"token">>, <<"caveats">>],
            correct_values = #{
                <<"token">> => [element(2, {ok, _} = tokens:serialize(Token))],
                <<"caveats">> => [[caveats:to_json(C) || C <- CaveatsToAdd]]
            },
            bad_values = [
                {<<"token">>, <<"1234">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"caveats">>, <<"1234">>, ?ERROR_BAD_VALUE_CAVEATS},
                {<<"caveats">>, 1234, ?ERROR_BAD_VALUE_CAVEATS},
                {<<"caveats">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_CAVEATS},
                {<<"caveats">>, [#{<<"type">> => <<"unknownCaveat">>}], ?ERROR_BAD_VALUE_CAVEATS}
            ]
        }
    })).


verify_access_or_identity_token(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, ProviderAdmin} = oz_test_utils:create_user(Config),
    {ok, {SessionId, _}} = oz_test_utils:log_in(Config, ProviderAdmin),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config, ProviderAdmin, ?UNIQUE_STRING),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(User)),
    oz_test_utils:support_space(Config, Provider, Space),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AllClients = [
        nobody,
        root,
        {user, User},
        {provider, Provider, ProviderToken},
        {user, ProviderAdmin}
    ],

    TokenAlpha = create_user_temporary_token(Config, User, ?ACCESS_TOKEN, [
        #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) + 3600}
    ]),
    verify_access_or_identity_token_base(
        Config, access, AllClients, TokenAlpha, undefined, true, ?SUB(user, User)
    ),
    verify_access_or_identity_token_base(
        Config, identity, AllClients, TokenAlpha, undefined, true, ?SUB(user, User)
    ),

    #named_token_data{token = TokenBeta} = create_provider_named_token(
        Config, Provider, ?ACCESS_TOKEN, [
            #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) + 3600}
        ]
    ),
    verify_access_or_identity_token_base(
        Config, access, AllClients, TokenBeta, undefined, true, ?SUB(?ONEPROVIDER, Provider)
    ),
    verify_access_or_identity_token_base(
        Config, identity, AllClients, TokenBeta, undefined, true, ?SUB(?ONEPROVIDER, Provider)
    ),

    TokenGamma = create_user_temporary_token(Config, ProviderAdmin, ?GUI_ACCESS_TOKEN(SessionId), [
        #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) + 3600},
        #cv_audience{whitelist = [?AUD(?OP_PANEL, Provider)]}
    ]),
    OpPanelClient = {op_panel, Provider, ProviderToken},
    verify_access_or_identity_token_base(
        Config, access, [OpPanelClient], TokenGamma, undefined, true, ?SUB(user, ProviderAdmin)
    ),
    verify_access_or_identity_token_base(
        Config, identity, [OpPanelClient], TokenGamma, undefined, true, ?SUB(user, ProviderAdmin)
    ),
    verify_access_or_identity_token_base(
        Config, access, AllClients -- [OpPanelClient], TokenGamma, undefined, false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_audience{whitelist = [?AUD(?OP_PANEL, Provider)]}
        )
    ),
    verify_access_or_identity_token_base(
        Config, identity, AllClients -- [OpPanelClient], TokenGamma, undefined, false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_audience{whitelist = [?AUD(?OP_PANEL, Provider)]}
        )
    ),

    #named_token_data{token = TokenDelta} = create_user_named_token(
        Config, User, ?ACCESS_TOKEN, [
            #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) - 1}
        ]
    ),
    verify_access_or_identity_token_base(
        Config, access, AllClients, TokenDelta, undefined, false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) - 1}
        )
    ),
    verify_access_or_identity_token_base(
        Config, identity, AllClients, TokenDelta, undefined, false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) - 1}
        )
    ),

    TokenSigma = create_user_temporary_token(Config, ProviderAdmin, ?INVITE_TOKEN(?USER_JOIN_CLUSTER, Provider)),
    verify_access_or_identity_token_base(
        Config, access, AllClients, TokenSigma, undefined,
        false, ?ERROR_NOT_AN_ACCESS_TOKEN(?INVITE_TOKEN(?USER_JOIN_CLUSTER, Provider))
    ),
    verify_access_or_identity_token_base(
        Config, identity, AllClients, TokenSigma, undefined,
        false, ?ERROR_NOT_AN_ACCESS_TOKEN(?INVITE_TOKEN(?USER_JOIN_CLUSTER, Provider))
    ),

    TokenTheta = create_provider_temporary_token(Config, Provider, ?ACCESS_TOKEN, [
        #cv_ip{whitelist = [{{134, 93, 0, 0}, 16}]}
    ]),
    verify_access_or_identity_token_base(
        Config, access, AllClients, TokenTheta, undefined, false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_ip{whitelist = [{{134, 93, 0, 0}, 16}]}
        )
    ),
    verify_access_or_identity_token_base(
        Config, identity, AllClients, TokenTheta, <<"133.93.1.182">>, false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_ip{whitelist = [{{134, 93, 0, 0}, 16}]}
        )
    ),
    verify_access_or_identity_token_base(
        Config, access, AllClients, TokenTheta, <<"134.93.7.18">>, true, ?SUB(?ONEPROVIDER, Provider)
    ),
    verify_access_or_identity_token_base(
        Config, identity, AllClients, TokenTheta, <<"134.93.1.182">>, true, ?SUB(?ONEPROVIDER, Provider)
    ),

    #named_token_data{token = TokenOmega} = create_provider_named_token(
        Config, Provider, ?ACCESS_TOKEN, [
            #cv_authorization_none{}
        ]
    ),
    verify_access_or_identity_token_base(
        Config, access, AllClients, TokenOmega, undefined, false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_authorization_none{}
        )
    ),
    verify_access_or_identity_token_base(
        Config, identity, AllClients, TokenOmega, undefined, true, ?SUB(?ONEPROVIDER, Provider)
    ).


verify_access_or_identity_token_base(Config, AccessOrIdentity, AllClients, Token, PeerIp, ShouldSucceed, ExpResult) ->
    ?assert(api_test_utils:run_tests(Config, #api_test_spec{
        client_spec = #client_spec{
            correct = AllClients,
            unauthorized = [],
            forbidden = []
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = case AccessOrIdentity of
                access -> verify_access_token;
                identity -> verify_identity_token
            end,
            args = [auth, data],
            expected_result = case ShouldSucceed of
                true ->
                    ?OK_TERM(fun(Result) ->
                        ?assertEqual(Result, ExpResult),
                        true
                    end);
                false ->
                    ?ERROR_REASON(ExpResult)
            end
        },
        rest_spec = #rest_spec{
            method = post,
            path = case AccessOrIdentity of
                access -> <<"/tokens/verify_access_token">>;
                identity -> <<"/tokens/verify_identity_token">>
            end,
            expected_code = case ShouldSucceed of
                true -> ?HTTP_200_OK;
                false -> errors:to_http_code(ExpResult)
            end,
            expected_body = case ShouldSucceed of
                true ->
                    fun(#{<<"subject">> := Subject}) ->
                        ?assertEqual(Subject, aai:subject_to_json(ExpResult)),
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
                case PeerIp of undefined -> []; _ -> [<<"peerIp">>] end
            ]),
            optional = case PeerIp of undefined -> [<<"peerIp">>]; _ -> [] end,
            correct_values = #{
                <<"token">> => [element(2, {ok, _} = tokens:serialize(Token))],
                <<"peerIp">> => [gs_protocol:undefined_to_null(PeerIp)]
            },
            bad_values = [
                {<<"token">>, <<"1234">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"peerIp">>, <<"1234.6.78.19">>, ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
                {<<"peerIp">>, 1234, ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
                {<<"peerIp">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)}
            ]
        }
    })).


verify_invite_token(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, ProviderAdmin} = oz_test_utils:create_user(Config),
    {ok, {SessionId, _}} = oz_test_utils:log_in(Config, ProviderAdmin),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config, ProviderAdmin, ?UNIQUE_STRING),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(User)),
    {ok, Group} = oz_test_utils:create_group(Config, ?USER(User)),
    oz_test_utils:support_space(Config, Provider, Space),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    ProviderClient = {provider, Provider, ProviderToken},
    % Only space support token can be consumed by a provider, so this will be
    % a typical list of allowed clients
    AllClientsButProvider = [
        nobody,
        root,
        {user, User},
        {user, ProviderAdmin}
    ],
    AllClients = [ProviderClient | AllClientsButProvider],

    TokenAlpha = create_user_temporary_token(Config, User, ?INVITE_TOKEN(?USER_JOIN_SPACE, Space), [
        #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) + 3600}
    ]),
    verify_invite_token_base(
        Config, AllClients, TokenAlpha, undefined, any, true, ?SUB(user, User)
    ),
    verify_invite_token_base(
        Config, AllClients, TokenAlpha, undefined, ?USER_JOIN_SPACE, true, ?SUB(user, User)
    ),
    verify_invite_token_base(
        Config, AllClients, TokenAlpha, undefined, ?GROUP_JOIN_SPACE, false, ?ERROR_NOT_AN_INVITE_TOKEN(
            ?GROUP_JOIN_SPACE, ?INVITE_TOKEN(?USER_JOIN_SPACE, Space)
        )
    ),

    #named_token_data{token = TokenBeta} = create_provider_named_token(
        Config, Provider, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, Provider), [
            #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) + 3600}
        ]
    ),
    verify_invite_token_base(
        Config, AllClients, TokenBeta, undefined, any, true, ?SUB(?ONEPROVIDER, Provider)
    ),
    verify_invite_token_base(
        Config, AllClients, TokenBeta, undefined, ?GROUP_JOIN_CLUSTER, true, ?SUB(?ONEPROVIDER, Provider)
    ),

    TokenGamma = create_user_temporary_token(Config, User, ?INVITE_TOKEN(?SUPPORT_SPACE, Space), [
        #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) + 3600},
        #cv_audience{whitelist = [?AUD(?OP_WORKER, Provider)]}
    ]),
    verify_invite_token_base(
        Config, [ProviderClient], TokenGamma, undefined, ?SUPPORT_SPACE, true, ?SUB(user, User)
    ),
    verify_invite_token_base(
        Config, AllClientsButProvider, TokenGamma, undefined, any, false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_audience{whitelist = [?AUD(?OP_WORKER, Provider)]}
        )
    ),

    #named_token_data{token = TokenDelta} = create_user_named_token(
        Config, User, ?INVITE_TOKEN(?GROUP_JOIN_GROUP, Group), [
            #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) - 1}
        ]
    ),
    verify_invite_token_base(
        Config, AllClients, TokenDelta, undefined, any, false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_time{valid_until = oz_test_utils:cluster_time_seconds(Config) - 1}
        )
    ),

    TokenLambda = create_user_temporary_token(Config, User, ?INVITE_TOKEN(?USER_JOIN_GROUP, Group), [
        #cv_ip{whitelist = [{{134, 93, 0, 0}, 16}]}
    ]),
    verify_invite_token_base(
        Config, AllClients, TokenLambda, undefined, any, false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_ip{whitelist = [{{134, 93, 0, 0}, 16}]}
        )
    ),
    verify_invite_token_base(
        Config, AllClients, TokenLambda, <<"133.93.1.182">>, ?USER_JOIN_GROUP, false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_ip{whitelist = [{{134, 93, 0, 0}, 16}]}
        )
    ),
    verify_invite_token_base(
        Config, AllClients, TokenLambda, <<"134.93.7.18">>, any, true, ?SUB(user, User)
    ),
    verify_invite_token_base(
        Config, AllClients, TokenLambda, <<"134.93.1.182">>, ?USER_JOIN_GROUP, true, ?SUB(user, User)
    ),

    TokenSigma = create_user_temporary_token(Config, ProviderAdmin, ?GUI_ACCESS_TOKEN(SessionId)),
    verify_invite_token_base(
        Config, AllClients, TokenSigma, undefined, any, false, ?ERROR_NOT_AN_INVITE_TOKEN(
            any, ?GUI_ACCESS_TOKEN(SessionId)
        )
    ),

    TokenTheta = create_user_temporary_token(Config, User, ?ACCESS_TOKEN),
    verify_invite_token_base(
        Config, AllClients, TokenTheta, undefined, ?SPACE_JOIN_HARVESTER, false, ?ERROR_NOT_AN_INVITE_TOKEN(
            ?SPACE_JOIN_HARVESTER, ?ACCESS_TOKEN
        )
    ),

    #named_token_data{token = TokenOmega} = create_provider_named_token(
        Config, Provider, ?INVITE_TOKEN(?USER_JOIN_CLUSTER, Provider), [
            #cv_authorization_none{}
        ]
    ),
    verify_invite_token_base(
        Config, AllClients, TokenOmega, undefined, any, false, ?ERROR_TOKEN_CAVEAT_UNVERIFIED(
            #cv_authorization_none{}
        )
    ).


verify_invite_token_base(Config, AllClients, Token, PeerIp, ExpType, ShouldSucceed, ExpResult) ->
    ?assert(api_test_utils:run_tests(Config, #api_test_spec{
        client_spec = #client_spec{
            correct = AllClients,
            unauthorized = [],
            forbidden = []
        },
        logic_spec = #logic_spec{
            module = token_logic,
            function = verify_invite_token,
            args = [auth, data],
            expected_result = case ShouldSucceed of
                true ->
                    ?OK_TERM(fun(Result) ->
                        ?assertEqual(Result, ExpResult),
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
                    fun(#{<<"subject">> := Subject}) ->
                        ?assertEqual(Subject, aai:subject_to_json(ExpResult)),
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
                case ExpType of any -> []; _ -> <<"expectedInviteTokenType">> end,
                case PeerIp of undefined -> []; _ -> [<<"peerIp">>] end
            ]),
            optional = lists:flatten([
                case PeerIp of undefined -> [<<"peerIp">>]; _ -> [] end
            ]),
            correct_values = #{
                <<"token">> => [element(2, {ok, _} = tokens:serialize(Token))],
                <<"peerIp">> => [gs_protocol:undefined_to_null(PeerIp)],
                <<"expectedInviteTokenType">> => case ExpType of
                    any -> [];
                    _ -> [tokens:invite_token_type_to_str(ExpType)]
                end
            },
            bad_values = [
                {<<"token">>, <<"1234">>, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, 1234, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"token">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_TOKEN(<<"token">>, ?ERROR_BAD_TOKEN)},
                {<<"peerIp">>, <<"1234.6.78.19">>, ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
                {<<"peerIp">>, 1234, ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
                {<<"peerIp">>, #{<<"a">> => <<"b">>}, ?ERROR_BAD_VALUE_IPV4_ADDRESS(<<"peerIp">>)},
                {<<"expectedInviteTokenType">>, <<"userJoinUser">>,
                    ?ERROR_BAD_VALUE_INVITE_TOKEN_TYPE(<<"expectedInviteTokenType">>)},
                {<<"expectedInviteTokenType">>, 1234,
                    ?ERROR_BAD_VALUE_INVITE_TOKEN_TYPE(<<"expectedInviteTokenType">>)},
                {<<"expectedInviteTokenType">>, #{<<"a">> => <<"b">>},
                    ?ERROR_BAD_VALUE_INVITE_TOKEN_TYPE(<<"expectedInviteTokenType">>)}
            ]
        }
    })).


create_user_named_token(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, ProviderAdmin} = oz_test_utils:create_user(Config),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config, ProviderAdmin, ?UNIQUE_STRING),
    {ok, Group} = oz_test_utils:create_group(Config, ?USER(User)),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(User)),
    oz_test_utils:user_set_oz_privileges(Config, User, [?OZ_HARVESTERS_CREATE], []),
    {ok, Harvester} = oz_test_utils:create_harvester(Config, ?USER(User)),
    Cluster = Provider,
    oz_test_utils:cluster_add_user(Config, Cluster, User),
    oz_test_utils:cluster_set_user_privileges(Config, Cluster, User, privileges:cluster_admin(), []),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    AlreadyExistingName = <<"alreadyExistingName">>,
    create_user_named_token(Config, User, AlreadyExistingName, ?ACCESS_TOKEN, []),

    VerifyFun = new_token_verify_fun(Config, ?SUB(user, User), named),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User},
                {admin, [?OZ_TOKENS_MANAGE, ?OZ_PROVIDERS_INVITE]}
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
                <<"caveats">> => ?CAVEATS_EXAMPLES_FOR_NAMED_TOKENS(Config),
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
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{user, User}]
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/user/tokens/named/">>
        }
    })).

create_provider_named_token(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, ProviderAdmin} = oz_test_utils:create_user(Config),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config, ProviderAdmin, ?UNIQUE_STRING),
    {ok, {AnotherProvider, AnotherProviderToken}} = oz_test_utils:create_provider(Config),
    {ok, Group} = oz_test_utils:create_group(Config, ?USER(User)),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(User)),
    oz_test_utils:user_set_oz_privileges(Config, User, [?OZ_HARVESTERS_CREATE], []),
    {ok, Harvester} = oz_test_utils:create_harvester(Config, ?USER(User)),
    Cluster = Provider,

    AlreadyExistingName = <<"subdubpubsubbub">>,
    create_provider_named_token(Config, Provider, AlreadyExistingName, ?ACCESS_TOKEN, []),

    VerifyFun = new_token_verify_fun(Config, ?SUB(?ONEPROVIDER, Provider), named),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, Provider, ProviderToken},
                {user, ProviderAdmin},
                {admin, [?OZ_TOKENS_MANAGE, ?OZ_PROVIDERS_INVITE]}
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
                <<"caveats">> => ?CAVEATS_EXAMPLES_FOR_NAMED_TOKENS(Config),
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

    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, Provider, ProviderToken}]
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/provider/tokens/named/">>
        }
    })).


create_user_temporary_token(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, {SessionId, _}} = oz_test_utils:log_in(Config, User),
    {ok, ProviderAdmin} = oz_test_utils:create_user(Config),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config, ProviderAdmin, ?UNIQUE_STRING),
    {ok, Group} = oz_test_utils:create_group(Config, ?USER(User)),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(User)),
    oz_test_utils:user_set_oz_privileges(Config, User, [?OZ_HARVESTERS_CREATE], []),
    {ok, Harvester} = oz_test_utils:create_harvester(Config, ?USER(User)),
    Cluster = Provider,
    oz_test_utils:cluster_add_user(Config, Cluster, User),
    oz_test_utils:cluster_set_user_privileges(Config, Cluster, User, privileges:cluster_admin(), []),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    Now = oz_test_utils:cluster_time_seconds(Config),
    MaxTtl = oz_test_utils:get_env(Config, max_temporary_token_ttl),

    VerifyFun = new_token_verify_fun(Config, ?SUB(user, User), temporary),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User},
                {admin, [?OZ_TOKENS_MANAGE, ?OZ_PROVIDERS_INVITE]}
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
                    ?GUI_ACCESS_TOKEN(SessionId) |
                    ?INVITE_TOKEN_TYPE_EXAMPLES(Group, Space, User, Cluster, Harvester)
                ]),
                <<"caveats">> => ?CAVEATS_EXAMPLES_FOR_TEMPORARY_TOKENS(Now, MaxTtl)
            },
            bad_values = ?BAD_TYPE_VALUES ++ ?BAD_CAVEATS_VALUES ++ [
                {<<"caveats">>, [], ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl)},
                {<<"caveats">>, ?CAVEATS_TO_JSON([
                    #cv_audience{whitelist = [?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]},
                    #cv_time{valid_until = Now + MaxTtl + 1}
                ]), ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl)}
            ]
        }
    },

    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{user, User}]
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/user/tokens/temporary/">>
        }
    })).


create_provider_temporary_token(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, {SessionId, _}} = oz_test_utils:log_in(Config, User),
    {ok, ProviderAdmin} = oz_test_utils:create_user(Config),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config, ProviderAdmin, ?UNIQUE_STRING),
    {ok, {AnotherProvider, AnotherProviderToken}} = oz_test_utils:create_provider(Config),
    {ok, Group} = oz_test_utils:create_group(Config, ?USER(User)),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(User)),
    oz_test_utils:user_set_oz_privileges(Config, User, [?OZ_HARVESTERS_CREATE], []),
    {ok, Harvester} = oz_test_utils:create_harvester(Config, ?USER(User)),
    Cluster = Provider,

    Now = oz_test_utils:cluster_time_seconds(Config),
    MaxTtl = oz_test_utils:get_env(Config, max_temporary_token_ttl),

    VerifyFun = new_token_verify_fun(Config, ?SUB(?ONEPROVIDER, Provider), temporary),
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {provider, Provider, ProviderToken},
                {user, ProviderAdmin},
                {admin, [?OZ_TOKENS_MANAGE, ?OZ_PROVIDERS_INVITE]}
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
                    {<<"type">>, tokens:type_to_json(?GUI_ACCESS_TOKEN(SessionId)), ?ERROR_BAD_VALUE_TOKEN_TYPE(<<"type">>)},
                    {<<"caveats">>, [], ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl)},
                    {<<"caveats">>, ?CAVEATS_TO_JSON([
                        #cv_audience{whitelist = [?AUD(?OZ_WORKER, ?ONEZONE_CLUSTER_ID)]},
                        #cv_time{valid_until = Now + MaxTtl + 50}
                    ]), ?ERROR_TOKEN_TIME_CAVEAT_REQUIRED(MaxTtl)}
                ]
            ])
        }
    },

    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, Provider, ProviderToken}]
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/provider/tokens/temporary/">>
        }
    })).


create_gui_access_token(Config) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, AnotherUser} = oz_test_utils:create_user(Config),
    {ok, {SessionId, _}} = oz_test_utils:log_in(Config, User),
    {ok, Space} = oz_test_utils:create_space(Config, ?USER(User)),
    {ok, {Provider, ProviderToken}} = oz_test_utils:create_provider(Config, User, ?UNIQUE_STRING),
    oz_test_utils:support_space(Config, Provider, Space),
    oz_test_utils:cluster_add_user(Config, ?ONEZONE_CLUSTER_ID, User),
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),

    {ok, {AnotherProvider, _}} = oz_test_utils:create_provider(Config),

    VerifyFun = fun({Token, _Ttl}) ->
        [#cv_audience{whitelist = [Audience]}] = caveats:filter([cv_audience], tokens:get_caveats(Token)),
        TokenVerifyFun = new_token_verify_fun(Config, ?SUB(user, User), temporary, Audience),
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
                    {admin, [?OZ_TOKENS_MANAGE]}
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
            ?OK_LIST([token_data_to_id(T) || T <- TokensToCheck])
        end,
        rest_method = get,
        rest_path_generator = fun(_, _) -> <<"/tokens/named">> end,
        rest_expectation_generator = fun(TokensToCheck) ->
            {?HTTP_200_OK, #{<<"tokens">> => [token_data_to_id(T) || T <- TokensToCheck]}}
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
            ?OK_LIST([token_data_to_id(T) || T <- TokensToCheck])
        end,
        rest_method = get,
        rest_path_generator = fun(UserId, _) -> [<<"/users/">>, UserId, <<"/tokens/named">>] end,
        rest_current_client_path_generator = fun(_) -> {User, <<"/user/tokens/named">>} end,
        rest_expectation_generator = fun(TokensToCheck) ->
            {?HTTP_200_OK, #{<<"tokens">> => [token_data_to_id(T) || T <- TokensToCheck]}}
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
            ?OK_LIST([token_data_to_id(T) || T <- TokensToCheck])
        end,
        rest_method = get,
        rest_path_generator = fun(PrId, _) -> [<<"/providers/">>, PrId, <<"/tokens/named">>] end,
        rest_current_client_path_generator = fun(_) -> {Provider, <<"/provider/tokens/named">>} end,
        rest_expectation_generator = fun(TokensToCheck) ->
            {?HTTP_200_OK, #{<<"tokens">> => [token_data_to_id(T) || T <- TokensToCheck]}}
        end
    })).


get_named_token(Config) ->
    get_named_token(Config, ?USER_ALPHA, [?USER_ALPHA]),
    get_named_token(Config, ?USER_BETA, [?USER_BETA]),
    get_named_token(Config, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    get_named_token(Config, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]),
    get_named_token(Config, ?ADMIN_OF(?PROV_GAMMA), [?ADMIN_OF(?PROV_GAMMA)]),
    get_named_token(Config, ?ADMIN_OF(?PROV_DELTA), [?ADMIN_OF(?PROV_DELTA)]).

get_named_token(Config, UserOrProvider, CorrectClients) ->
    ?assert(run_token_tests(Config, #token_api_test_spec{
        tokens_to_check = [?TOKENS_OF(UserOrProvider)],
        testing_strategy = one_token_at_a_time,
        correct_clients = CorrectClients,
        logic_function = get_named_token,
        logic_args_generator = fun(_, TokenToCheck) ->
            [auth, token_data_to_id(TokenToCheck)]
        end,
        logic_expectation_generator = fun(TokenToCheck) ->
            ?OK_TERM(fun(Result) ->
                verify_named_token_data(Config, logic, Result, TokenToCheck)
            end)
        end,
        rest_method = get,
        rest_path_generator = fun(_, TokenToCheck) -> [<<"/tokens/named/">>, token_data_to_id(TokenToCheck)] end,
        rest_expectation_generator = fun(TokenToCheck) ->
            {?HTTP_200_OK, fun(Result) ->
                verify_named_token_data(Config, rest, Result, TokenToCheck)
            end}
        end
    })).


get_user_named_token_by_name(Config) ->
    get_user_named_token_by_name(Config, ?USER_ALPHA),
    get_user_named_token_by_name(Config, ?USER_BETA),
    get_user_named_token_by_name(Config, ?ADMIN_OF(?PROV_GAMMA)),
    get_user_named_token_by_name(Config, ?ADMIN_OF(?PROV_DELTA)).

get_user_named_token_by_name(Config, User) ->
    ?assert(run_token_tests(Config, #token_api_test_spec{
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
                verify_named_token_data(Config, logic, Result, TokenToCheck)
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
                verify_named_token_data(Config, rest, Result, TokenToCheck)
            end}
        end
    })).


get_provider_named_token_by_name(Config) ->
    get_provider_named_token_by_name(Config, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    get_provider_named_token_by_name(Config, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]).

get_provider_named_token_by_name(Config, Provider, CorrectClients) ->
    ?assert(run_token_tests(Config, #token_api_test_spec{
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
                verify_named_token_data(Config, logic, Result, TokenToCheck)
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
                verify_named_token_data(Config, rest, Result, TokenToCheck)
            end}
        end
    })).


verify_named_token_data(Config, API, Result, TokenToCheck) when API == logic orelse API == rest ->
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
            rest -> tokens:type_to_json(Type)
        end,
        <<"caveats">> => case API of
            logic -> Caveats;
            rest -> [caveats:to_json(C) || C <- Caveats]
        end,
        <<"revoked">> => false,
        <<"token">> => case API of
            logic -> Token;
            rest -> element(2, {ok, _} = tokens:serialize(Token))
        end
    }),
    Metadata = maps:get(<<"metadata">>, Result),
    Now = oz_test_utils:get_mocked_time(Config),
    case Type of
        ?INVITE_TOKEN(_, _) ->
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


update_named_token(Config) ->
    update_named_token(Config, ?USER_ALPHA, [?USER_ALPHA]),
    update_named_token(Config, ?USER_BETA, [?USER_BETA]),
    update_named_token(Config, ?ADMIN_OF(?PROV_GAMMA), [?ADMIN_OF(?PROV_GAMMA)]),
    update_named_token(Config, ?ADMIN_OF(?PROV_DELTA), [?ADMIN_OF(?PROV_DELTA)]),
    update_named_token(Config, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    update_named_token(Config, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]).


update_named_token(Config, SubjectId, CorrectClients) ->
    AlreadyExistingName = <<"alreadyExistingName123">>,
    ?assert(run_token_tests(Config, #token_api_test_spec{
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
                }} = oz_test_utils:call_oz(Config, token_logic, get_named_token, [?ROOT, TokenId]),
                % Create a token with AlreadyExistingName to check that other tokens
                % cannot be renamed to that name
                case Subject of
                    ?SUB(user, UserId) ->
                        oz_test_utils:call_oz(Config, token_logic, create_user_named_token, [
                            ?USER(UserId), UserId, #{<<"name">> => AlreadyExistingName}
                        ]);
                    ?SUB(?ONEPROVIDER, PrId) ->
                        oz_test_utils:call_oz(Config, token_logic, create_provider_named_token, [
                            ?PROVIDER(PrId), PrId, #{<<"name">> => AlreadyExistingName}
                        ])
                end,
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
                oz_test_utils:call_oz(Config, od_token, update, [TokenId, fun(Token) ->
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
                }}, oz_test_utils:call_oz(Config, token_logic, get_named_token, [?ROOT, TokenId])),
                true
            end
        end
    })).


delete_named_token(Config) ->
    BasicEnv = create_basic_env(Config),
    delete_named_token(Config, BasicEnv, ?USER_ALPHA, [?USER_ALPHA]),
    delete_named_token(Config, BasicEnv, ?USER_BETA, [?USER_BETA]),
    delete_named_token(Config, BasicEnv, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    delete_named_token(Config, BasicEnv, ?ADMIN_OF(?PROV_GAMMA), [?ADMIN_OF(?PROV_GAMMA)]),
    delete_named_token(Config, BasicEnv, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]),
    delete_named_token(Config, BasicEnv, ?ADMIN_OF(?PROV_DELTA), [?ADMIN_OF(?PROV_DELTA)]).

delete_named_token(Config, BasicEnv, SubjectIdBinding, CorrectClientsBindings) ->
    CorrectClients = [map_client(BasicEnv, C) || C <- CorrectClientsBindings],
    EnvSetUpFun = fun() ->
        #named_token_data{token = Token, name = TokenName} = case map_client(BasicEnv, SubjectIdBinding) of
            {user, UserId} -> create_user_named_token(Config, UserId, ?ACCESS_TOKEN);
            {provider, ProviderId, _} -> create_provider_named_token(Config, ProviderId, ?ACCESS_TOKEN)
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
        assert_token_deleted(ShouldSucceed, Config, Token, TokenName)
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
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)),

    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{user, UserId}]
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/user/tokens/named/">>
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun)).


delete_all_provider_named_tokens(Config) ->
    BasicEnv = create_basic_env(Config),
    delete_all_provider_named_tokens(Config, BasicEnv, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    delete_all_provider_named_tokens(Config, BasicEnv, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]).

delete_all_provider_named_tokens(Config, BasicEnv, ProviderIdBinding, CorrectClientsBindings) ->
    {provider, ProviderId, _} = map_client(BasicEnv, ProviderIdBinding),

    % This test deletes providers' named tokens, in which case temporary tokens
    % must be used to authorize REST operations. This function replaces the
    % tokens in provider client tuples.
    ReplaceTokensForProviders = fun(Clients) ->
        lists:map(fun
            ({provider, PrId, _RootToken}) ->
                TempToken = create_provider_temporary_token(Config, PrId, ?ACCESS_TOKEN),
                {provider, PrId, element(2, {ok, _} = tokens:serialize(TempToken))};
            (Other) ->
                Other
        end, Clients)
    end,

    CorrectClients = [map_client(BasicEnv, C) || C <- CorrectClientsBindings],
    ForbiddenClients = all_clients(BasicEnv) -- CorrectClients,

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
                {admin, [?OZ_TOKENS_MANAGE]} |
                ReplaceTokensForProviders(CorrectClients)
            ],
            unauthorized = [nobody],
            forbidden = ReplaceTokensForProviders(ForbiddenClients)
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
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)),

    {ok, ProviderTempToken} = tokens:serialize(
        create_provider_temporary_token(Config, ProviderId, ?ACCESS_TOKEN)
    ),
    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [
                {provider, ProviderId, ProviderTempToken}
            ]
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/provider/tokens/named/">>
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun)).


assert_token_deleted(true, Config, Token, TokenName) ->
    #token{subject = Subject, id = TokenId, type = Type} = Token,
    ?assertMatch(
        {error, not_found},
        oz_test_utils:call_oz(Config, od_token, get, [TokenId])
    ),
    ?assertMatch(
        {error, not_found},
        oz_test_utils:call_oz(Config, token_names, lookup, [Subject, TokenName])
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
    #token{subject = Subject, id = TokenId, type = Type} = Token,
    ?assertMatch(
        {ok, _},
        oz_test_utils:call_oz(Config, od_token, get, [TokenId])
    ),
    ?assertMatch(
        {ok, TokenId},
        oz_test_utils:call_oz(Config, token_names, lookup, [Subject, TokenName])
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


revoke_all_user_temporary_tokens(Config) ->
    BasicEnv = create_basic_env(Config),
    revoke_all_user_temporary_tokens(Config, BasicEnv, ?USER_ALPHA),
    revoke_all_user_temporary_tokens(Config, BasicEnv, ?USER_BETA),
    revoke_all_user_temporary_tokens(Config, BasicEnv, ?ADMIN_OF(?PROV_GAMMA)),
    revoke_all_user_temporary_tokens(Config, BasicEnv, ?ADMIN_OF(?PROV_DELTA)).

revoke_all_user_temporary_tokens(Config, BasicEnv, UserIdBinding) ->
    {user, UserId} = map_client(BasicEnv, UserIdBinding),
    {ok, {SessionId, _}} = oz_test_utils:log_in(Config, UserId),

    % This test deletes users' temporary tokens, which are by default used by the
    % test framework in REST calls. This function generates named tokens and
    % inserts them into the user client tuples.
    GenNamedTokensForUsers = fun(Clients) ->
        lists:map(fun
            ({user, UId}) ->
                #named_token_data{token = UserNamedToken} = create_user_named_token(Config, UId, ?ACCESS_TOKEN),
                {user, UId, element(2, {ok, _} = tokens:serialize(UserNamedToken))};
            (Other) ->
                Other
        end, Clients)
    end,

    CorrectClients = [{user, UserId}],
    ForbiddenClients = all_clients(BasicEnv) -- CorrectClients,

    EnvSetUpFun = fun() ->
        {ok, Space} = oz_test_utils:create_space(Config, ?USER(UserId)),
        T1 = create_user_temporary_token(Config, UserId, ?ACCESS_TOKEN),
        T2 = create_user_temporary_token(Config, UserId, ?GUI_ACCESS_TOKEN(SessionId)),
        T3 = create_user_temporary_token(Config, UserId, ?INVITE_TOKEN(?USER_JOIN_SPACE, Space)),
        #{userTokens => [T1, T2, T3]}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{userTokens := UserTokens}, _Data) ->
        lists:foreach(fun(Token) ->
            assert_token_verifies(Config, not ShouldSucceed, Token)
        end, UserTokens)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_TOKENS_MANAGE]} |
                % Use a named token for authorizing the user as temporary tokens
                % get deleted in this test
                GenNamedTokensForUsers(CorrectClients)
            ],
            unauthorized = [nobody],
            forbidden = GenNamedTokensForUsers(ForbiddenClients)
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
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)),

    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{user, UserId}]
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/user/tokens/temporary/">>
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun)).



revoke_all_provider_temporary_tokens(Config) ->
    BasicEnv = create_basic_env(Config),
    revoke_all_provider_temporary_tokens(Config, BasicEnv, ?PROV_GAMMA, [?PROV_GAMMA, ?ADMIN_OF(?PROV_GAMMA)]),
    revoke_all_provider_temporary_tokens(Config, BasicEnv, ?PROV_DELTA, [?PROV_DELTA, ?ADMIN_OF(?PROV_DELTA)]).

revoke_all_provider_temporary_tokens(Config, BasicEnv, ProviderIdBinding, CorrectClientsBindings) ->
    {provider, ProviderId, ProviderToken} = map_client(BasicEnv, ProviderIdBinding),

    CorrectClients = [map_client(BasicEnv, C) || C <- CorrectClientsBindings],
    ForbiddenClients = all_clients(BasicEnv) -- CorrectClients,

    EnvSetUpFun = fun() ->
        T1 = create_provider_temporary_token(Config, ProviderId, ?ACCESS_TOKEN),
        T2 = create_provider_temporary_token(Config, ProviderId, ?ACCESS_TOKEN),
        T3 = create_provider_temporary_token(Config, ProviderId, ?INVITE_TOKEN(?GROUP_JOIN_CLUSTER, ProviderId)),
        #{providerTokens => [T1, T2, T3]}
    end,

    VerifyEndFun = fun(ShouldSucceed, #{providerTokens := ProviderTokens}, _Data) ->
        lists:foreach(fun(Token) ->
            assert_token_verifies(Config, not ShouldSucceed, Token)
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
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun)),

    ApiTestSpec2 = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [{provider, ProviderId, ProviderToken}]
        },
        logic_spec = undefined,
        rest_spec = RestSpec#rest_spec{
            path = <<"/provider/tokens/temporary/">>
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec2, EnvSetUpFun, undefined, VerifyEndFun)).


assert_token_verifies(Config, ShouldSucceed, Token = #token{type = Type, subject = Subject}) ->
    VerificationResult = case Type of
        ?ACCESS_TOKEN ->
            oz_test_utils:call_oz(Config, token_logic, verify_access_token, [?NOBODY, #{<<"token">> => Token}]);
        ?GUI_ACCESS_TOKEN(_) ->
            oz_test_utils:call_oz(Config, token_logic, verify_access_token, [?NOBODY, #{<<"token">> => Token}]);
        ?INVITE_TOKEN(_, _) ->
            oz_test_utils:call_oz(Config, token_logic, verify_invite_token, [?NOBODY, #{<<"token">> => Token}])
    end,
    case ShouldSucceed of
        true -> ?assertMatch({ok, Subject}, VerificationResult);
        false -> ?assertMatch(?ERROR_TOKEN_INVALID, VerificationResult)
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
    oz_test_utils:mock_time(Config),
    oz_test_utils:mock_harvester_plugins(Config, ?HARVESTER_MOCK_PLUGIN),
    Config.


end_per_testcase(_, Config) ->
    oz_test_utils:unmock_time(Config),
    oz_test_utils:unmock_harvester_plugins(Config, ?HARVESTER_MOCK_PLUGIN),
    oz_test_utils:delete_all_entities(Config),
    ok.
