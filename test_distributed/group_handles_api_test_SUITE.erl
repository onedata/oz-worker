%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C): 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning groups handles API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(group_handles_api_test_SUITE).
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


%% API
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    list_handles_test/1,
    create_handle_test/1,
    get_handle_details_test/1,
    leave_handle_test/1,
    list_eff_handles_test/1,
    get_eff_handle_details_test/1
]).

all() ->
    ?ALL([
        list_handles_test,
        create_handle_test,
        get_handle_details_test,
        leave_handle_test,
        list_eff_handles_test,
        get_eff_handle_details_test
    ]).


%%%===================================================================
%%% Test functions
%%%===================================================================


list_handles_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, revoke, [
        ?GROUP_VIEW
    ]),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),

    {ok, S1} = oz_test_utils:create_space_for_group(Config, G1, <<"S1">>),
    {ok, S2} = oz_test_utils:create_space_for_group(Config, G1, <<"S2">>),

    {ok, HServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),

    % Create 3 handles, 2 for S1 and 1 for S2
    ExpHandles = lists:map(
        fun({Idx, SpaceId}) ->
            ShareId = <<"shareId", (Idx+48)/integer>>,
            {ok, ShareId} = oz_test_utils:create_share(
                Config, ?ROOT, ShareId, <<"share">>, <<"file">>, SpaceId
            ),
            {ok, HandleId} = oz_test_utils:create_handle(
                Config, ?ROOT, ?HANDLE(HServiceId, ShareId)
            ),
            {ok, G1} = oz_test_utils:add_group_to_handle(Config, HandleId, G1),
            HandleId
        end, [{1, S1}, {2, S1}, {3, S2}]
    ),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
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
            path = [<<"/groups/">>, G1, <<"/handles">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handles">> => ExpHandles}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_handles,
            args = [client, G1],
            expected_result = ?OK_LIST(ExpHandles)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


create_handle_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    {ok, S1} = oz_test_utils:create_space_for_group(Config, G1, <<"S1">>),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, <<"share1">>, <<"share">>, <<"file">>, S1
    ),

    {ok, HServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, G1} = oz_test_utils:add_group_to_handle_service(
        Config, HServiceId, G1
    ),

    AllPrivs = oz_test_utils:call_oz(
        Config, privileges, handle_privileges, []
    ),
    AllPrivsBin = [atom_to_binary(Priv, utf8) || Priv <- AllPrivs],

    ExpResourceType = <<"Share">>,
    VerifyFun = fun(HandleId) ->
        {ok, Handle} = oz_test_utils:get_handle(Config, HandleId),
        ?assertEqual(ExpResourceType, Handle#od_handle.resource_type),
        ?assertEqual(ShareId, Handle#od_handle.resource_id),
        ?assertEqual(HServiceId, Handle#od_handle.handle_service),
        ?assertEqual(#{G1 => AllPrivs}, Handle#od_handle.groups),
        ?assertEqual(
            #{G1 => {AllPrivs, [{od_handle, HandleId}]}},
            Handle#od_handle.eff_groups
        ),
        ?assertEqual(#{}, Handle#od_handle.users),
        ?assertEqual(
            #{U1 => {AllPrivs, [{od_group, G1}]}},
            Handle#od_handle.eff_users
        ),
        true
    end,

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = post,
            path = [<<"/groups/">>, G1, <<"/handles">>],
            expected_code = ?HTTP_201_CREATED,
            expected_headers = fun(#{<<"location">> := Location} = _Headers) ->
                [GroupId, HandleId] = binary:split(
                    Location,
                    [<<"/groups/">>, <<"/handles/">>],
                    [trim_all, global]
                ),
                ?assertEqual(GroupId, G1),
                VerifyFun(HandleId)
            end
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = create_handle,
            args = [client, G1, data],
            expected_result = ?OK_TERM(VerifyFun)
        },
        gs_spec = #gs_spec{
            operation = create,
            gri = #gri{type = od_handle, aspect = instance},
            auth_hint = ?AS_GROUP(G1),
            expected_result = ?OK_MAP_CONTAINS(#{
                <<"effectiveGroups">> => #{G1 => AllPrivsBin},
                <<"effectiveUsers">> => #{U1 => AllPrivsBin},
                <<"metadata">> => ?DC_METADATA,
                <<"handleServiceId">> => HServiceId,
                <<"resourceType">> => ExpResourceType,
                <<"resourceId">> => ShareId,
                <<"gri">> => fun(EncodedGri) ->
                    #gri{id = HandleId} = oz_test_utils:decode_gri(
                        Config, EncodedGri
                    ),
                    VerifyFun(HandleId)
                end
            })
        },
        data_spec = DataSpec = #data_spec{
            required = [
                <<"handleServiceId">>,
                <<"resourceType">>,
                <<"resourceId">>,
                <<"metadata">>
            ],
            correct_values = #{
                <<"handleServiceId">> => [HServiceId],
                <<"resourceType">> => [<<"Share">>],
                <<"resourceId">> => [ShareId],
                <<"metadata">> => [?DC_METADATA]
            },
            bad_values = [
                % One cannot check privileges of hs if it does not exist so 403
                {<<"handleServiceId">>, <<"">>, ?ERROR_FORBIDDEN},
                {<<"handleServiceId">>, 1234, ?ERROR_FORBIDDEN},
                {<<"resourceType">>, <<"">>,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"resourceType">>,
                        [<<"Share">>])},
                {<<"resourceType">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"resourceType">>)},
                % one cannot check privileges of resource
                % if it does not exist so 403
                {<<"resourceId">>, <<"">>, ?ERROR_FORBIDDEN},
                {<<"resourceId">>, 1234, ?ERROR_FORBIDDEN},
                {<<"metadata">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"metadata">>)},
                {<<"metadata">>, <<"">>,
                    ?ERROR_BAD_VALUE_EMPTY(<<"metadata">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % Root bypass authorize so malformed data result in errors
    % in validation step not authorize
    RootApiTestSpec = ApiTestSpec#api_test_spec{
        client_spec = #client_spec{
            correct = [root]
        },
        rest_spec = undefined,
        gs_spec = undefined,
        data_spec = DataSpec#data_spec{
            bad_values = [
                % one cannot check privileges of hs if it does not exist so 403
                {<<"handleServiceId">>, <<"">>,
                    ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"handleServiceId">>)},
                {<<"handleServiceId">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"handleServiceId">>)},
                {<<"resourceType">>, <<"">>,
                    ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"resourceType">>,
                        [<<"Share">>])},
                {<<"resourceType">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"resourceType">>)},
                % one cannot check privileges of resource
                % if it does not exist so 403
                {<<"resourceId">>, <<"">>,
                    ?ERROR_BAD_VALUE_ID_NOT_FOUND(<<"resourceId">>)},
                {<<"resourceId">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"resourceId">>)},
                {<<"metadata">>, 1234,
                    ?ERROR_BAD_VALUE_BINARY(<<"metadata">>)},
                {<<"metadata">>, <<"">>,
                    ?ERROR_BAD_VALUE_EMPTY(<<"metadata">>)}
            ]
        }
    },
    ?assert(api_test_utils:run_tests(Config, RootApiTestSpec)).


get_handle_details_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, revoke, [
        ?GROUP_VIEW
    ]),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_VIEW
    ]),

    {ok, HServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, G1} = oz_test_utils:add_group_to_handle_service(
        Config, HServiceId, G1
    ),

    {ok, S1} = oz_test_utils:create_space_for_group(Config, G1, <<"S1">>),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, <<"share1">>, <<"share">>, <<"file">>, S1
    ),

    HandleDetails = ?HANDLE(HServiceId, ShareId),
    {ok, HandleId} = oz_test_utils:create_handle(
        Config, ?ROOT, HandleDetails
    ),
    {ok, G1} = oz_test_utils:add_group_to_handle(
        Config, HandleId, G1
    ),

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
            method = get,
            path = [<<"/groups/">>, G1, <<"/handles/">>, HandleId],
            expected_code = ?HTTP_200_OK,
            expected_body = {contains, HandleDetails}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_handle,
            args = [client, G1, HandleId],
            expected_result = ?OK_MAP_CONTAINS(HandleDetails)
        }
        % TODO gs
%%        gs_spec = #gs_spec{
%%            operation = get,
%%            gri = #gri{
%%                type = od_handle, id = HandleId,
%%                aspect = instance, scope = protected
%%            },
%%            auth_hint = ?THROUGH_GROUP(G1),
%%            expected_result = ?OK_MAP_CONTAINS(HandleDetails#{
%%                fun(EncodedGri) ->
%%                    #gri{id = Hid} = oz_test_utils:decode_gri(
%%                        Config, EncodedGri
%%                    ),
%%                    ?assertEqual(Hid, HandleId)
%%                end
%%            })
%%        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


leave_handle_test(Config) ->
    {ok, U1} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, U2} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, NonAdmin} = oz_test_utils:create_user(Config, #od_user{}),

    {ok, G1} = oz_test_utils:create_group(Config, ?USER(U1), <<"G1">>),
    oz_test_utils:group_set_user_privileges(Config, G1, U1, revoke, [
        ?GROUP_UPDATE
    ]),
    {ok, U2} = oz_test_utils:add_user_to_group(Config, G1, U2),
    oz_test_utils:group_set_user_privileges(Config, G1, U2, set, [
        ?GROUP_UPDATE
    ]),

    {ok, HServiceId} = oz_test_utils:create_handle_service(
        Config, ?ROOT, ?DOI_SERVICE
    ),
    {ok, G1} = oz_test_utils:add_group_to_handle_service(
        Config, HServiceId, G1
    ),

    {ok, S1} = oz_test_utils:create_space_for_group(Config, G1, <<"S1">>),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?ROOT, <<"share1">>, <<"share">>, <<"file">>, S1
    ),

    EnvSetUpFun = fun() ->
        {ok, HandleId} = oz_test_utils:create_handle(
            Config, ?ROOT, ?HANDLE(HServiceId, ShareId)
        ),
        {ok, G1} = oz_test_utils:add_group_to_handle(Config, HandleId, G1),
        #{handleId => HandleId}
    end,
    VerifyEndFun = fun(ShouldSucceed, #{handleId := HandleId} = _Env, _) ->
        {ok, Groups} = oz_test_utils:get_handle_groups(Config, HandleId),
        ?assertEqual(lists:member(G1, Groups), not ShouldSucceed)
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
            path = [<<"/groups/">>, G1, <<"/handles/">>, handleId],
            expected_code = ?HTTP_202_ACCEPTED
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = leave_handle,
            args = [client, G1, handleId],
            expected_result = ?OK
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(
        Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyEndFun
    )).


list_eff_handles_test(Config) ->
    {
        [{H1, _}, {H2, _}, {H3, _}, {H4, _}, {H5, _}],
        [{G1, _} | _Groups], {U1, U2, NonAdmin}
    } = api_test_scenarios:create_eff_handles_env(Config),

    ExpHandles = [H1, H2, H3, H4, H5],
    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {user, U1}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, U2},
                {user, NonAdmin}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/groups/">>, G1, <<"/effective_handles">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"handles">> => ExpHandles}
        },
        logic_spec = #logic_spec{
            module = group_logic,
            function = get_eff_handles,
            args = [client, G1],
            expected_result = ?OK_LIST(ExpHandles)
        }
        % TODO gs
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)),

    % check also group_logic:has_eff_handle function
    lists:foreach(
        fun(Handle) ->
            ?assert(oz_test_utils:call_oz(
                Config, group_logic, has_eff_handle, [G1, Handle])
            )
        end, ExpHandles
    ),
    ?assert(not oz_test_utils:call_oz(
        Config, group_logic, has_eff_handle, [G1, <<"asdiucyaie827346w">>])
    ).


get_eff_handle_details_test(Config) ->
    {
        EffHandlesList, [{G1, _} | _Groups], {U1, _U2, NonAdmin}
    } = api_test_scenarios:create_eff_handles_env(Config),

    lists:foreach(
        fun({HandleId, HandleDetails}) ->
            ApiTestSpec = #api_test_spec{
                client_spec = #client_spec{
                    correct = [
                        root,
                        {user, U1}
                    ],
                    unauthorized = [nobody],
                    forbidden = [
                        {user, NonAdmin}
                    ]
                },
                rest_spec = #rest_spec{
                    method = get,
                    path = [
                        <<"/groups/">>, G1, <<"/effective_handles/">>, HandleId
                    ],
                    expected_code = ?HTTP_200_OK,
                    expected_body = {
                        contains, HandleDetails#{<<"handleId">> => HandleId}
                    }
                },
                logic_spec = #logic_spec{
                    module = group_logic,
                    function = get_eff_handle,
                    args = [client, G1, HandleId],
                    expected_result = ?OK_MAP_CONTAINS(HandleDetails)
                }
                % TODO gs
%%                gs_spec = #gs_spec{
%%                    operation = get,
%%                    gri = #gri{
%%                        type = od_handle, id = HandleId,
%%                        aspect = instance, scope = protected
%%                    },
%%                    auth_hint = ?THROUGH_GROUP(G5),
%%                    expected_result = ?OK_MAP(HandleDetails#{
%%                        <<"gri">> =>
%%                        fun(EncodedGri) ->
%%                            #gri{id = Id} = oz_test_utils:decode_gri(
%%                                Config, EncodedGri
%%                            ),
%%                            ?assertEqual(Id, HandleId)
%%                        end
%%                    })
%%                }
            },
            ?assert(api_test_utils:run_tests(Config, ApiTestSpec))

        end, EffHandlesList
    ).


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
    oz_test_utils:mock_handle_proxy(Config),
    Config.


end_per_testcase(_, Config) ->
    oz_test_utils:delete_all_entities(Config),
    oz_test_utils:unmock_handle_proxy(Config).
