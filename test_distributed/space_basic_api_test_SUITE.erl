%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C): 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning space basic API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(space_basic_api_test_SUITE).
-author("Bartosz Walkowicz").

-include("rest.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("cluster_worker/include/api_errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1
]).
-export([
    create_test/1,
    list_test/1,
    get_test/1,
    update_test/1,
    delete_test/1,

    list_shares_test/1,
    get_share_test/1,

    list_providers_test/1,
    create_provider_support_token/1,
    get_provider_test/1,
    leave_provider_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        get_test,
        update_test,
        delete_test,

        list_shares_test,
        get_share_test,

        list_providers_test,
        create_provider_support_token,
        get_provider_test,
        leave_provider_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


create_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),

    VerifyFun = fun(SpaceId) ->
        {ok, Space} = oz_test_utils:get_space(Config, SpaceId),
        ?assertEqual(?SPACE_NAME1, Space#od_space.name),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
            ],
            unauthorized = [nobody]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/spaces">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"location">> := Location} = _Headers) ->
                <<"/spaces/", SpaceId/binary>> = Location,
                VerifyFun(SpaceId)
            end
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = create,
            args = [client, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_space, aspect = instance},
            auth_hint = ?AS_USER(U1),
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"name">> => ?SPACE_NAME1,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    VerifyFun(Id)
                end
            })
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            correct_values = #{<<"name">> => [?SPACE_NAME1]},
            bad_values = [
                {<<"name">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"name">>)},
                {<<"name">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"name">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


list_test(Config) ->
    % Make sure that spaces created in other tests are deleted.
    oz_test_utils:delete_all_entities(Config),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_SPACES_LIST
    ]),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S3} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S4} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S5} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    ExpSpaces = [S1, S2, S3, S4, S5],

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/spaces">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"spaces">> => ExpSpaces}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = list,
            args = [client],
            expected_result = ?OK_LIST(ExpSpaces)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also space_logic:exist function
    lists:foreach(
        fun(SpaceId) ->
            ?assert(oz_test_utils:call_oz(
                Config, space_logic, exists, [SpaceId])
            )
        end, ExpSpaces
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, space_logic, exists, [<<"asdiucyaie827346w">>])
    ).


get_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, grant, [
        ?OZ_SPACES_LIST
    ]),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, U1, revoke, [
        ?SPACE_VIEW
    ]),
    oz_test_utils:add_user_to_space(Config, S1, U2),
    oz_test_utils:space_set_user_privileges(Config, S1, U2, set, [
        ?SPACE_VIEW
    ]),

    {ok, {P1, KeyFile, CertFile}} = oz_test_utils:create_provider_and_certs(
        Config, ?PROVIDER_NAME
    ),
    SupportSize = oz_test_utils:minimum_support_size(Config),
    {ok, S1} = oz_test_utils:support_space(
        Config, P1, S1, SupportSize
    ),

    AllPrivs = oz_test_utils:get_space_privileges(Config),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    % Get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2},
                {provider, P1, KeyFile, CertFile}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, Admin},
                {user, NonAdmin},
                {user, U1}
            ]
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get,
            args = [client, S1],
            expected_result = ?OK_TERM(
                fun(#od_space{
                    name = Name, users = Users, groups = #{},
                    providers = Providers, shares = [],
                    eff_users = EffUsers, eff_groups = #{},
                    eff_providers = EffProviders,
                    top_down_dirty = false, bottom_up_dirty = false
                }) ->
                    ?assertEqual(?SPACE_NAME1, Name),
                    ?assertEqual(Users, #{
                        U1 => AllPrivs -- [?SPACE_VIEW],
                        U2 => [?SPACE_VIEW]}
                    ),
                    ?assertEqual(EffUsers, #{
                        U1 => {AllPrivs -- [?SPACE_VIEW], [{od_space, S1}]},
                        U2 => {[?SPACE_VIEW], [{od_space, S1}]}
                    }),
                    ?assertEqual(Providers, #{P1 => SupportSize}),
                    ?assertEqual(EffProviders, #{P1 => [{od_space, S1}]})
                end
            )
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_space, id = S1, aspect = instance},
            expected_result = ?OK_MAP(#{
                <<"name">> => ?SPACE_NAME1,
                <<"users">> => #{
                    U1 => AllPrivsBin -- [<<"space_view">>],
                    U2 => [<<"space_view">>]
                },
                <<"groups">> => #{},
                <<"shares">> => [],
                <<"providers">> => #{P1 => SupportSize},
                <<"effectiveUsers">> => #{
                    U1 => AllPrivsBin -- [<<"space_view">>],
                    U2 => [<<"space_view">>]
                },
                <<"effectiveGroups">> => #{},
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(S1, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPrivateDataApiTestSpec)),

    % Get and check protected data
    GetSharedDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {user, U1},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {provider, P1, KeyFile, CertFile}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, S1],
            expected_code = ?HTTP_200_OK,
            expected_body = #{
                <<"spaceId">> => S1,
                <<"name">> => ?SPACE_NAME1,
                <<"providersSupports">> => #{P1 => SupportSize}
            }
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_protected_data,
            args = [client, S1],
            expected_result = ?OK_MAP(#{
                <<"name">> => ?SPACE_NAME1,
                <<"providersSupports">> => #{P1 => SupportSize}
            })
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_space, id = S1, aspect = instance, scope = protected
            },
            expected_result = ?OK_MAP(#{
                <<"name">> => ?SPACE_NAME1,
                <<"providersSupports">> => #{P1 => SupportSize},
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(S1, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetSharedDataApiTestSpec)).


update_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
        oz_test_utils:space_set_user_privileges(
            Config, S1, U1, revoke, [?SPACE_UPDATE]
        ),
        oz_test_utils:add_user_to_space(Config, S1, U2),
        oz_test_utils:space_set_user_privileges(
            Config, S1, U2, set, [?SPACE_UPDATE]
        ),
        #{spaceId => S1}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, Data) ->
        {ok, Space} = oz_test_utils:get_space(Config, SpaceId),
        ExpName = case ShouldSucceed of
            false -> ?SPACE_NAME1;
            true -> maps:get(<<"name">>, Data, ?SPACE_NAME1)
        end,
        ?assertEqual(ExpName, Space#od_space.name)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = patch,
            path = [<<"/spaces/">>, spaceId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = update,
            args = [client, spaceId, data],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_space, id = spaceId, aspect = instance},
            expected_result = ?OK
        },
        data_spec = #data_spec{
            at_least_one = [<<"name">>],
            correct_values = #{
                <<"name">> => [fun() -> ?UNIQUE_NAME(<<"space">>) end]
            },
            bad_values = [
                {<<"name">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"name">>)},
                {<<"name">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"name">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
        oz_test_utils:space_set_user_privileges(
            Config, S1, U1, revoke, [?SPACE_DELETE]
        ),
        oz_test_utils:add_user_to_space(Config, S1, U2),
        oz_test_utils:space_set_user_privileges(
            Config, S1, U2, set, [?SPACE_DELETE]
        ),
        #{spaceId => S1}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{spaceId := SpaceId} = _Env, _) ->
        {ok, Spaces} = oz_test_utils:list_spaces(Config),
        ?assertEqual(lists:member(SpaceId, Spaces), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/spaces/">>, spaceId],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = delete,
            args = [client, spaceId],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_space, id = spaceId, aspect = instance},
            expected_result = ?OK
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun]
    )).


list_shares_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, User, set, []),

    ExpShares = lists:map(
        fun(_) ->
            ShareId = ?UNIQUE_NAME(<<"Share">>),
            {ok, ShareId} = oz_test_utils:create_share(
                Config, ?ROOT, ShareId, ShareId, ?ROOT_FILE_ID, S1
            ),
            ShareId
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, S1, <<"/shares">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"shares">> => ExpShares}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_shares,
            args = [client, S1],
            expected_result = ?OK_LIST(ExpShares)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_share_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, User, set, []),

    ShareName = <<"Share">>,
    ShareId = ?UNIQUE_NAME(ShareName),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ShareId, ShareName, ?ROOT_FILE_ID, S1
    ),

    {ok, ZoneDomain} = oz_test_utils:get_domain(Config),
    SharePublicUrl = ?SHARE_PUBLIC_URL(ZoneDomain, ShareId),

    ExpShareDetails = #{
        <<"name">> => ShareName,
        <<"spaceId">> => S1,
        <<"rootFileId">> => ?ROOT_FILE_ID,
        <<"handleId">> => <<"undefined">>,
        <<"publicUrl">> => SharePublicUrl
    },
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, User}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, S1, <<"/shares/">>, ShareId],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpShareDetails#{<<"shareId">> => ShareId}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_share,
            args = [client, S1, ShareId],
            expected_result = ?OK_TERM(
                fun(#od_share{
                    name = Name,
                    public_url = PublicUrl,
                    space = SpaceId,
                    handle = undefined,
                    root_file = RootFile
                }) ->
                    ?assertEqual(Name, ShareName),
                    ?assertEqual(PublicUrl, SharePublicUrl),
                    ?assertEqual(SpaceId, S1),
                    ?assertEqual(RootFile, ?ROOT_FILE_ID)
                end)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_share, id = ShareId,
                aspect = instance, scope = private
            },
            auth_hint = ?THROUGH_SPACE(S1),
            expected_result = ?OK_MAP(ExpShareDetails#{
                <<"handleId">> => null,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(Id, ShareId)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


list_providers_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, set, [
        ?OZ_SPACES_LIST_PROVIDERS
    ]),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, U1, revoke, [
        ?SPACE_VIEW
    ]),
    {ok, U2} = oz_test_utils:add_user_to_space(Config, S1, U2),
    oz_test_utils:space_set_user_privileges(Config, S1, U2, set, [
        ?SPACE_VIEW
    ]),

    SupportSize = oz_test_utils:minimum_support_size(Config),
    ExpProviders = lists:map(
        fun(_) ->
            {ok, {ProviderId, _, _}} = oz_test_utils:create_provider_and_certs(
                Config, ?PROVIDER_NAME
            ),
            {ok, S1} = oz_test_utils:support_space(
                Config, ProviderId, S1, SupportSize
            ),
            ProviderId
        end, lists:seq(1, 5)
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin},
                {user, U1}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, S1, <<"/providers">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"providers">> => ExpProviders}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_providers,
            args = [client, S1],
            expected_result = ?OK_LIST(ExpProviders)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also space_logic:has_provider function
    lists:foreach(
        fun(ProviderId) ->
            ?assert(oz_test_utils:call_oz(
                Config, space_logic, has_provider, [S1, ProviderId])
            )
        end, ExpProviders
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, space_logic, has_provider, [S1, <<"asdiucyaie827346w">>])
    ).


create_provider_support_token(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, U1, revoke, [
        ?SPACE_INVITE_PROVIDER
    ]),
    {ok, U2} = oz_test_utils:add_user_to_space(Config, S1, U2),
    oz_test_utils:space_set_user_privileges(Config, S1, U2, set, [
        ?SPACE_INVITE_PROVIDER
    ]),

    % We will keep all tokens generated until now in a process, we will query
    GeneratedTokens = fun Loop(Tokens) ->
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
    TokensProc = spawn_link(fun() -> GeneratedTokens([]) end),

    VerifyFun = fun(Token) ->
        TokensProc ! {self(), Token},
        IsTokenUnique =
            receive
                Response -> Response
            after 1000 ->
                false
            end,
        ?assert(IsTokenUnique),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/spaces/">>, S1, <<"/providers/token">>],
            expected_code = ?HTTP_200_OK,
            expected_body = fun(#{<<"token">> := Token}) -> VerifyFun(Token) end
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = create_provider_invite_token,
            args = [client, S1],
            expected_result = ?OK_TERM(VerifyFun)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_provider_test(Config) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:set_user_oz_privileges(Config, Admin, set, [
        ?OZ_SPACES_LIST_PROVIDERS
    ]),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, User, set, []),

    {_, CSRFile, _} = oz_test_utils:generate_provider_cert_files(),
    {ok, CSR} = file:read_file(CSRFile),
    ProviderDetails = ?PROVIDER_DETAILS(?PROVIDER_NAME),

    {ok, {P1, _}} = oz_test_utils:create_provider(
        Config, ProviderDetails#{<<"csr">> => CSR}
    ),
    {ok, S1} = oz_test_utils:support_space(
        Config, P1, S1, oz_test_utils:minimum_support_size(Config)
    ),

    ExpProvidersDetails = ProviderDetails#{
        <<"clientName">> => ?PROVIDER_NAME
    },
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {user, User}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, S1, <<"/providers/">>, P1],
            expected_code = ?HTTP_200_OK,
            expected_body = ExpProvidersDetails#{
                <<"providerId">> => P1
            }
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_provider,
            args = [client, S1, P1],
            expected_result = ?OK_MAP(ExpProvidersDetails)
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_provider, id = P1,
                aspect = instance, scope = protected
            },
            auth_hint = ?THROUGH_SPACE(S1),
            expected_result = ?OK_MAP(ExpProvidersDetails#{
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(Id, P1)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


leave_provider_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    oz_test_utils:space_set_user_privileges(Config, S1, U1, revoke, [
        ?SPACE_REMOVE_PROVIDER
    ]),
    {ok, U2} = oz_test_utils:add_user_to_space(Config, S1, U2),
    oz_test_utils:space_set_user_privileges(Config, S1, U2, set, [
        ?SPACE_REMOVE_PROVIDER
    ]),

    EnvSetUpFun = fun() ->
        {ok, {ProviderId, _, _}} = oz_test_utils:create_provider_and_certs(
            Config, ?PROVIDER_NAME
        ),
        {ok, S1} = oz_test_utils:support_space(
            Config, ProviderId, S1, oz_test_utils:minimum_support_size(Config)
        ),
        #{providerId => ProviderId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{providerId := ProviderId} = _Env, _) ->
        {ok, Providers} = oz_test_utils:get_space_providers(Config, S1),
        ?assertEqual(lists:member(ProviderId, Providers), not ShouldSucceed)
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U2}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = delete,
            path = [<<"/spaces/">>, S1, <<"/providers/">>, providerId],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = leave_provider,
            args = [client, S1, providerId],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun]
    )).


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
