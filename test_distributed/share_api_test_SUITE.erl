%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning share API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(share_api_test_SUITE).
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
-include_lib("ctool/include/api_errors.hrl").

-include("api_test_utils.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1
]).
-export([
    list_test/1,
    create_test/1,
    get_test/1,
    update_test/1,
    delete_test/1
]).

all() ->
    ?ALL([
        create_test,
        list_test,
        get_test,
        update_test,
        delete_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_test(Config) ->
    % Make sure that shares created in other tests are deleted.
    oz_test_utils:delete_all_entities(Config),

    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, set, [
        ?OZ_SHARES_LIST
    ]),

    {ok, S1} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME1),
    {ok, S2} = oz_test_utils:create_space(Config, ?USER(U1), ?SPACE_NAME2),

    ExpShares = lists:map(
        fun(SpaceId) ->
            ShareId = ?UNIQUE_STRING,
            {ok, ShareId} = oz_test_utils:create_share(
                Config, ?ROOT, ShareId, ?SHARE_NAME1, ?ROOT_FILE_ID, SpaceId
            ),
            ShareId
        end, [S1, S1, S1, S2, S2]
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = <<"/shares">>,
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"shares">> => ExpShares}
        },
        logic_spec = #logic_spec{
            module = share_logic,
            function = list,
            args = [client],
            expected_result = ?OK_LIST(ExpShares)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also share_logic:exist function
    lists:foreach(
        fun(ShareId) ->
            ?assert(oz_test_utils:call_oz(
                Config, share_logic, exists, [ShareId])
            )
        end, ExpShares
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, share_logic, exists, [<<"asdiucyaie827346w">>])
    ).


create_test(Config) ->
    % create space with 3 users:
    %   U3 gets the SPACE_MANAGE_SHARES privilege
    %   U2 gets the SPACE_MANAGE_SHARES privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_MANAGE_SHARES
    ),
    {ok, U3} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U3} = oz_test_utils:space_add_user(Config, S1, U3),
    oz_test_utils:space_set_user_privileges(Config, S1, U3, set, [
        ?SPACE_MANAGE_SHARES
    ]),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    ExpShareDetails = #od_share{
        name = ?CORRECT_NAME, space = S1, root_file = ?ROOT_FILE_ID
    },
    VerifyFun = fun(ShareId) ->
        {ok, Share} = oz_test_utils:get_share(Config, ShareId),
        PublicURL = oz_test_utils:get_share_public_url(Config, ShareId),
        ?assertEqual(ExpShareDetails#od_share{public_url = PublicURL}, Share),
        true
    end,

    BadDataValues = [
        {<<"shareId">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"shareId">>)},
        {<<"shareId">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"shareId">>)},
        {<<"rootFileId">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"rootFileId">>)},
        {<<"rootFileId">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"rootFileId">>)}
    ],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U2},
                {user, U3}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U1},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = <<"/shares">>,
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"Location">> := Location} = _Headers) ->
                BaseURL = ?URL(Config, [<<"/shares/">>]),
                [ShareId] = binary:split(Location, [BaseURL], [global, trim_all]),
                VerifyFun(ShareId)
            end
        },
        logic_spec = LogicSpec = #logic_spec{
            module = share_logic,
            function = create,
            args = [client, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_share, aspect = instance},
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"handleId">> => null,
                <<"name">> => ?CORRECT_NAME,
                <<"rootFileId">> => ?ROOT_FILE_ID,
                <<"spaceId">> => S1,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    VerifyFun(Id)
                end
            })
        },
        data_spec = DataSpec = #data_spec{
            required = [
                <<"shareId">>, <<"name">>, <<"rootFileId">>, <<"spaceId">>
            ],
            correct_values = #{
                <<"shareId">> => [fun() -> ?UNIQUE_STRING end],
                <<"name">> => [?CORRECT_NAME],
                <<"rootFileId">> => [?ROOT_FILE_ID],
                <<"spaceId">> => [S1]
            },
            bad_values = lists:append([
                [{<<"spaceId">>, <<"">>, ?ERROR_FORBIDDEN},
                {<<"spaceId">>, <<"asdq4ewfs">>, ?ERROR_FORBIDDEN}],
                BadDataValues,
                ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)])
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Root client bypasses authorization checks,
    % hence wrong values of handleServiceId or resourceId
    % cause validation errors rather than authorization errors.
    RootApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [root]
        },
        logic_spec = LogicSpec,
        data_spec = DataSpec#data_spec{
            bad_values = [
                {<<"spaceId">>, <<"">>, ?ERROR_BAD_VALUE_EMPTY(<<"spaceId">>)},
                {<<"spaceId">>, 1234, ?ERROR_BAD_VALUE_BINARY(<<"spaceId">>)}
                | BadDataValues
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, RootApiTestSpec)).


get_test(Config) ->
    % create space with 2 users:
    %   U2 gets the SPACE_MANAGE_SHARES privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_MANAGE_SHARES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Admin} = oz_test_utils:create_user(Config, #od_user{}),
    oz_test_utils:user_set_oz_privileges(Config, Admin, set, [
        ?OZ_SHARES_LIST
    ]),

    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, ?SHARE_ID_1, ?SHARE_NAME1, ?ROOT_FILE_ID, S1
    ),
    SharePublicURL = oz_test_utils:get_share_public_url(Config, ShareId),
    SharePublicDetails = #{
        <<"name">> => ?SHARE_NAME1, <<"publicUrl">> => SharePublicURL,
        <<"rootFileId">> => ?ROOT_FILE_ID, <<"handleId">> => undefined
    },
    SharePrivateDetails = SharePublicDetails#{<<"spaceId">> => S1},

    % Get and check private data
    GetPrivateDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, Admin},
                {user, U2},
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/shares/">>, ShareId],
            expected_code = ?HTTP_200_OK,
            expected_body = SharePrivateDetails#{
                % TODO VFS-3922
                <<"handleId">> => <<"undefined">>,
                <<"shareId">> => ShareId
            }
        },
        logic_spec = #logic_spec{
            module = share_logic,
            function = get,
            args = [client, ShareId],
            expected_result = ?OK_TERM(
                fun(#od_share{
                    name = ShareName, public_url = PublicURL,
                    space = Space, handle = undefined, root_file = RootFile
                }) ->
                    ?assertEqual(?SHARE_NAME1, ShareName),
                    ?assertEqual(?ROOT_FILE_ID, RootFile),
                    ?assertEqual(Space, S1),
                    ?assertEqual(SharePublicURL, PublicURL)
                end
            )
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_share, id = ShareId, aspect = instance},
            expected_result = ?OK_MAP(SharePrivateDetails#{
                <<"handleId">> => null,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = Id} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    ?assertEqual(ShareId, Id)
                end
            })
        }
    },
    ?assert(api_test_utils:run_tests(Config, GetPrivateDataApiTestSpec)),

    % Get and check public data
    GetSharedDataApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                nobody,
                {user, Admin},
                {user, NonAdmin},
                {user, U1},
                {user, U2}
            ]
        },
        logic_spec = #logic_spec{
            module = share_logic,
            function = get_public_data,
            args = [client, ShareId],
            expected_result = ?OK_MAP(SharePublicDetails)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, GetSharedDataApiTestSpec)).


update_test(Config) ->
    % create space with 2 users:
    %   U2 gets the SPACE_MANAGE_SHARES privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_MANAGE_SHARES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        ShareId = ?UNIQUE_STRING,
        {ok, ShareId} = oz_test_utils:create_share(
            Config, ?ROOT, ShareId, ?SHARE_NAME1, ?ROOT_FILE_ID, S1
        ),
        #{shareId => ShareId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{shareId := ShareId}, Data) ->
        {ok, Share} = oz_test_utils:get_share(Config, ShareId),
        ExpName = case ShouldSucceed of
            false -> ?SHARE_NAME1;
            true -> maps:get(<<"name">>, Data)
        end,
        ?assertEqual(ExpName, Share#od_share.name)
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
            path = [<<"/shares/">>, shareId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = share_logic,
            function = update,
            args = [client, shareId, data],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = update,
            gri = #gri{type = od_share, id = shareId, aspect = instance},
            expected_result = ?OK
        },
        data_spec = #data_spec{
            required = [<<"name">>],
            correct_values = #{<<"name">> => [?CORRECT_NAME]},
            bad_values = ?BAD_VALUES_NAME(?ERROR_BAD_VALUE_NAME)
        }
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


delete_test(Config) ->
    % create space with 2 users:
    %   U2 gets the SPACE_MANAGE_SHARES privilege
    %   U1 gets all remaining privileges
    {S1, U1, U2} = api_test_scenarios:create_basic_space_env(
        Config, ?SPACE_MANAGE_SHARES
    ),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    EnvSetUpFun = fun() ->
        ShareId = ?UNIQUE_STRING,
        {ok, ShareId} = oz_test_utils:create_share(
            Config, ?ROOT, ShareId, ?SHARE_NAME1, ?ROOT_FILE_ID, S1
        ),
        #{shareId => ShareId}
    end,
    DeleteEntityFun = fun(#{shareId := ShareId} = _Env) ->
        oz_test_utils:delete_share(Config, ShareId)
    end,
    VerifyEndFun = fun(ShouldSucceed, #{shareId := ShareId} = _Env, _) ->
        {ok, Shares} = oz_test_utils:list_shares(Config),
        ?assertEqual(lists:member(ShareId, Shares), not ShouldSucceed)
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
            path = [<<"/shares/">>, shareId],
            expected_code = ?HTTP_204_NO_CONTENT
        },
        logic_spec = #logic_spec{
            module = share_logic,
            function = delete,
            args = [client, shareId],
            expected_result = ?OK
        },
        gs_spec = #gs_spec{
            operation = delete,
            gri = #gri{type = od_share, id = shareId, aspect = instance},
            expected_result = ?OK
        }
    },
    ?assert(api_test_scenarios:run_scenario(delete_entity,
        [Config, ApiTestSpec, EnvSetUpFun, VerifyEndFun, DeleteEntityFun]
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
