%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% CT tests concerning space shares API (REST + logic + gs).
%%% @end
%%%-------------------------------------------------------------------
-module(space_shares_api_test_SUITE).
-author("Lukasz Opiola").

-include("ozt.hrl").


-export([
    groups/0, all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    end_per_testcase/2
]).
-export([
    get_shares_test/1,
    get_share_test/1,
    list_shares_test/1
]).


groups() -> [
    {parallel_tests, [parallel], [
        get_shares_test,
        get_share_test,
        list_shares_test
    ]}
].

all() -> [
    {group, parallel_tests}
].


%%%===================================================================
%%% Test functions
%%%===================================================================


get_shares_test(Config) ->
    Owner = ozt_users:create(),
    PrivilegedMember = ozt_users:create(),
    UnprivilegedMember = ozt_users:create(),
    NonMember = ozt_users:create(),

    SpaceId = ozt_users:create_space_for(Owner),
    ProviderId = ozt_providers:create_as_support_for_space(SpaceId),

    ozt_spaces:add_user(SpaceId, PrivilegedMember, [?SPACE_VIEW]),
    ozt_spaces:add_user(SpaceId, UnprivilegedMember, privileges:space_admin() -- [?SPACE_VIEW]),

    ExpShares = lists_utils:generate(fun(_) -> ozt_shares:create(SpaceId) end, 10),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_LIST_RELATIONSHIPS]},
                {user, Owner},
                {user, PrivilegedMember},
                {provider, ProviderId}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UnprivilegedMember},
                {user, NonMember}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, SpaceId, <<"/shares">>],
            expected_code = ?HTTP_200_OK,
            expected_body = #{<<"shares">> => ExpShares}
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_shares,
            args = [auth, SpaceId],
            expected_result = ?OK_LIST(ExpShares)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


get_share_test(Config) ->
    Owner = ozt_users:create(),
    PrivilegedMember = ozt_users:create(),
    UnprivilegedMember = ozt_users:create(),
    NonMember = ozt_users:create(),

    SpaceId = ozt_users:create_space_for(Owner),
    ProviderId = ozt_providers:create_as_support_for_space(SpaceId),

    ozt_spaces:add_user(SpaceId, PrivilegedMember, [?SPACE_VIEW]),
    ozt_spaces:add_user(SpaceId, UnprivilegedMember, privileges:space_admin() -- [?SPACE_VIEW]),

    ozt:reconcile_entity_graph(),

    ShareId = datastore_key:new(),
    ShareData = #{
        <<"name">> => str_utils:rand_hex(12),
        <<"description">> => <<"## Share description">>,
        <<"fileType">> => lists_utils:random_element([?REGULAR_FILE_TYPE, ?DIRECTORY_TYPE]),
        <<"spaceId">> => SpaceId,
        <<"shareId">> => ShareId,
        <<"rootFileId">> => ?GEN_ROOT_FILE_GUID(SpaceId, ShareId)
    },
    ozt_shares:create(?USER(Owner), SpaceId, ShareData),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SHARES_VIEW]},
                {user, Owner},
                {user, PrivilegedMember},
                {provider, ProviderId}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, UnprivilegedMember},
                {user, NonMember}
            ]
        },
        rest_spec = #rest_spec{
            method = get,
            path = [<<"/spaces/">>, SpaceId, <<"/shares/">>, ShareId],
            expected_code = ?HTTP_200_OK,
            expected_body = api_test_expect:private_share(rest, ShareId, ShareData, ?SUB(user, Owner))
        },
        logic_spec = #logic_spec{
            module = space_logic,
            function = get_share,
            args = [auth, SpaceId, ShareId],
            expected_result = api_test_expect:private_share(logic, ShareId, ShareData, ?SUB(user, Owner))
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{
                type = od_share, id = ShareId,
                aspect = instance, scope = private
            },
            auth_hint = ?THROUGH_SPACE(SpaceId),
            expected_result_op = api_test_expect:private_share(gs, ShareId, ShareData, ?SUB(user, Owner))
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).


list_shares_test(Config) ->
    OwnerAlpha = ozt_users:create(),
    SpaceAlpha = ozt_users:create_space_for(OwnerAlpha),
    list_shares_test_base(Config, OwnerAlpha, SpaceAlpha, ozt_shares:gen_shares_for_space(SpaceAlpha, 2222)),

    OwnerBeta = ozt_users:create(),
    SpaceBeta = ozt_users:create_space_for(OwnerBeta),
    list_shares_test_base(Config, OwnerBeta, SpaceBeta, ozt_shares:gen_shares_for_space(SpaceBeta, 333)),

    OwnerZeroShares = ozt_users:create(),
    SpaceZeroShares = ozt_users:create_space_for(OwnerZeroShares),
    list_shares_test_base(Config, OwnerZeroShares, SpaceZeroShares, []),

    % deleting a space should cause all its shares to be deleted
    ozt_spaces:delete(SpaceAlpha),
    ozt_spaces:delete(SpaceBeta),
    ozt_spaces:delete(SpaceZeroShares),
    ?assertEqual([], ozt:rpc(share_registry, list_entries, [SpaceAlpha, #{limit => infinity}])),
    ?assertEqual([], ozt:rpc(share_registry, list_entries, [SpaceBeta, #{limit => infinity}])),
    ?assertEqual([], ozt:rpc(share_registry, list_entries, [SpaceZeroShares, #{limit => infinity}])).

list_shares_test_base(Config, Owner, SpaceId, ExpAllShareEntries) ->
    PrivilegedMember = ozt_users:create(),
    UnprivilegedMember = ozt_users:create(),
    NonMember = ozt_users:create(),

    ProviderId = ozt_providers:create_as_support_for_space(SpaceId),

    ozt_spaces:add_user(SpaceId, PrivilegedMember, [?SPACE_VIEW]),
    ozt_spaces:add_user(SpaceId, UnprivilegedMember, privileges:space_admin() -- [?SPACE_VIEW]),

    ExpShareEntriesListing = fun(Aspect, ListingOpts) ->
        Limit = maps:get(<<"limit">>, ListingOpts, 1000),
        RequestedOffset = maps:get(<<"offset">>, ListingOpts, 0),
        StartIndex = maps:get(<<"index">>, ListingOpts, <<>>),

        StartingPos = lists_utils:foldl_while(fun(ShareEntry, AccCurrentPos) ->
            case StartIndex > maps:get(<<"index">>, ShareEntry) of
                false -> {halt, AccCurrentPos};
                true -> {cont, AccCurrentPos + 1}
            end
        end, 1, ExpAllShareEntries),

        % the offset might be negative
        ExpEntries = lists:sublist(ExpAllShareEntries, max(1, StartingPos + RequestedOffset), Limit),

        ExpData = case Aspect of
            list_shares -> [maps:get(<<"shareId">>, E) || E <- ExpEntries];
            list_shares_with_data -> ExpEntries
        end,
        {ExpData, length(ExpEntries) < Limit}
    end,

    RunTests = fun(ClientSpec, GsSpec, CorrectValues, BadValues, Aspect) ->
        api_test_utils:run_tests(Config, #api_test_spec{
            client_spec = ClientSpec,
            logic_spec = #logic_spec{
                module = space_logic,
                function = Aspect,
                args = [auth, SpaceId, data],
                expected_result = ?OK_ENV(fun(_, Data) ->
                    ExpResult = ExpShareEntriesListing(Aspect, Data),
                    ?OK_TERM(fun(Result) -> ?assertEqual(ExpResult, Result) end)
                end)
            },
            gs_spec = GsSpec,
            data_spec = #data_spec{
                optional = [
                    <<"index">>,
                    <<"offset">>,
                    <<"limit">>
                ],
                correct_values = CorrectValues,
                bad_values = BadValues
            }
        })
    end,

    ClientSpecQuick = #client_spec{
        correct = [
            {user, Owner}
        ]
    },
    ClientSpecAll = #client_spec{
        correct = [
            root,
            {admin, [?OZ_SPACES_LIST_RELATIONSHIPS]},
            {user, Owner},
            {user, PrivilegedMember}
        ],
        unauthorized = [nobody],
        forbidden = [
            {user, UnprivilegedMember},
            {user, NonMember},
            {provider, ProviderId}
        ]
    },

    GsSpec = fun(Aspect) ->
        #gs_spec{
            operation = create,
            gri = #gri{type = od_space, id = SpaceId, aspect = Aspect, scope = private},
            expected_result_gui = ?OK_ENV(fun(_Env, Data) ->
                {ExpEntries, IsLast} = ExpShareEntriesListing(Aspect, Data),
                ?OK_MAP(#{<<"list">> => ExpEntries, <<"isLast">> => IsLast})
            end)
        }
    end,

    CorrectValuesQuick = #{
        <<"index">> => [<<"0">>],
        <<"offset">> => [0],
        <<"limit">> => [1000]
    },
    CorrectValuesCombinations = #{
        <<"index">> => [
            <<>>,
            null,
            rand_utf8_string(1),
            rand_utf8_string(4)
        ],
        <<"offset">> => [0, ?RAND_INT(-1000, 1000)],
        <<"limit">> => [?RAND_INT(1, 1000)]
    },

    BadValues = [
        {<<"index">>, 10, ?ERROR_BAD_VALUE_BINARY(<<"index">>)},
        {<<"offset">>, <<"a">>, ?ERROR_BAD_VALUE_INTEGER(<<"offset">>)},
        {<<"limit">>, <<"a">>, ?ERROR_BAD_VALUE_INTEGER(<<"limit">>)},
        {<<"limit">>, 0, ?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"limit">>, 1, 1000)}
    ],

    % those two runs focus on auth checks, bad value checks and Logic + GraphSync interfaces
    ?assert(RunTests(ClientSpecAll, GsSpec(list_shares), CorrectValuesQuick, BadValues, list_shares)),
    ?assert(RunTests(ClientSpecAll, GsSpec(list_shares_with_data), CorrectValuesQuick, BadValues, list_shares_with_data)),
    % those two - on correct listing results depending on listing opts, purely on the logic calls
    ?assert(RunTests(ClientSpecQuick, undefined, CorrectValuesCombinations, [], list_shares)),
    ?assert(RunTests(ClientSpecQuick, undefined, CorrectValuesCombinations, [], list_shares_with_data)),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec rand_utf8_string(pos_integer()) -> binary().
rand_utf8_string(Length) ->
    Tail = case Length of
        1 -> <<>>;
        _ -> rand_utf8_string(Length - 1)
    end,
    Bin = <<(?RAND_INT(0, 255)), Tail/binary>>,
    try
        % some generated combinations of chars may be invalid JSON strings
        json_utils:encode(Bin),
        Bin
    catch _:_ ->
        rand_utf8_string(Length)
    end.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================


init_per_suite(Config) ->
    ssl:start(),
    application:ensure_all_started(hackney),
    ozt:init_per_suite(Config, fun() ->
        ozt_mocks:mock_handle_proxy()
    end).

end_per_suite(_Config) ->
    ozt_mocks:unmock_handle_proxy(),
    application:stop(hackney),
    ssl:stop().

init_per_group(_Group, Config) ->
    ozt_mailer:mock(),
    ozt_mocks:freeze_time(),
    Config.

end_per_group(_Group, Config) ->
    ozt_mailer:unmock(),
    ozt_mocks:unfreeze_time(),
    Config.

end_per_testcase(_, Config) ->
    Config.

