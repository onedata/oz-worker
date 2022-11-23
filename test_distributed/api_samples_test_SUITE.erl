%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Tests of GraphSync API for fetching API samples for different resources.
%%% @end
%%%-------------------------------------------------------------------
-module(api_samples_test_SUITE).
-author("Lukasz Opiola").

-include("http/rest.hrl").
-include("entity_logic.hrl").
-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/automation/automation.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/rest_api_samples_test_utils.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/errors.hrl").

-include("api_test_utils.hrl").
-include("ozt.hrl").

-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    space_api_samples_test/1
]).

all() ->
    ?ALL([
        space_api_samples_test
    ]).


-record(space_ctx, {
    space_id :: od_space:id(),
    tested_user :: od_user:id(),
    tested_group :: od_group:id()
}).

-define(GRANTED_SPACE_PRIVS, [?SPACE_UPDATE, ?SPACE_MANAGE_ATM_WORKFLOW_EXECUTIONS, ?SPACE_VIEW_QOS]).
-define(REVOKED_SPACE_PRIVS, [?SPACE_DELETE, ?SPACE_VIEW_TRANSFERS, ?SPACE_ADD_GROUP]).


%%%===================================================================
%%% Test functions
%%%===================================================================


space_api_samples_test(Config) ->
    Creator = ozt_users:create(),
    SpaceId = ozt_users:create_space_for(Creator),

    AnotherMember = ozt_users:create(),
    TestedUser = ozt_users:create(),
    ozt_spaces:add_user(SpaceId, AnotherMember, []),
    ozt_spaces:add_user(SpaceId, TestedUser, lists_utils:random_sublist(privileges:space_privileges())),

    TestedGroup = ozt_groups:create(),
    ozt_spaces:add_group(SpaceId, TestedGroup, lists_utils:random_sublist(privileges:space_privileges())),

    NonMember = ozt_users:create(),

    ApiTestSpec = #api_test_spec{
        client_spec = #client_spec{
            correct = [
                root,
                {admin, [?OZ_SPACES_VIEW]},
                {user, Creator},
                {user, AnotherMember}
            ],
            unauthorized = [nobody],
            forbidden = [
                {user, NonMember}
            ]
        },
        gs_spec = #gs_spec{
            operation = get,
            gri = #gri{type = od_space, id = SpaceId, aspect = api_samples, scope = private},
            expected_result = ?OK_TERM(fun(ResultBody) ->
                ExpectedApiRoot = str_utils:format_bin("https://~s/api/v3/onezone", [ozt:get_domain()]),
                rest_api_samples_test_utils:verify_structure(
                    ResultBody, ExpectedApiRoot, exp_operation_list()
                ),
                rest_api_samples_test_utils:test_samples(
                    ResultBody,
                    ozt_tokens:ensure_serialized(ozt_tokens:create(?SUB(user, Creator))),
                    [{ssl_options, ozt_http:ssl_opts()}],
                    #space_ctx{space_id = SpaceId, tested_user = TestedUser, tested_group = TestedGroup},
                    fun build_sample_test_spec/1
                )
            end)
        }
    },
    ?assert(api_test_utils:run_tests(Config, ApiTestSpec)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec build_sample_test_spec(rest_api_request_sample:name()) -> rest_api_samples_test_utils:sample_test_spec().
build_sample_test_spec(<<"Get space details">>) -> #sample_test_spec{
    verify_fun = fun(#space_ctx{space_id = SpaceId}, ResultBody) ->
        #od_space{name = ExpName} = ozt_spaces:get(SpaceId),
        ?assertMatch(#{<<"name">> := ExpName}, json_utils:decode(ResultBody))
    end
};
build_sample_test_spec(<<"List all space privileges">>) -> #sample_test_spec{
    verify_fun = fun(_Ctx, ResultBody) ->
        ?assertMatch(#{<<"admin">> := _, <<"manager">> := _, <<"member">> := _}, json_utils:decode(ResultBody))
    end
};
build_sample_test_spec(<<"List direct space users">>) -> #sample_test_spec{
    substitute_placeholder_fun = fun(#space_ctx{tested_user = UserId}, <<"$USER_ID">>) -> UserId end,
    verify_fun = fun(#space_ctx{space_id = SpaceId}, ResultBody) ->
        #{<<"users">> := Users} = ?assertMatch(#{<<"users">> := _}, json_utils:decode(ResultBody)),
        ?assertEqual(lists:sort(ozt_spaces:get_users(SpaceId)), lists:sort(Users))
    end
};
build_sample_test_spec(<<"List effective space users">>) -> #sample_test_spec{
    substitute_placeholder_fun = fun(#space_ctx{tested_user = UserId}, <<"$USER_ID">>) -> UserId end,
    verify_fun = fun(#space_ctx{space_id = SpaceId}, ResultBody) ->
        #{<<"users">> := Users} = ?assertMatch(#{<<"users">> := _}, json_utils:decode(ResultBody)),
        ?assertEqual(lists:sort(ozt_spaces:get_eff_users(SpaceId)), lists:sort(Users))
    end
};
build_sample_test_spec(<<"Get effective space user details">>) -> #sample_test_spec{
    substitute_placeholder_fun = fun(#space_ctx{tested_user = UserId}, <<"$USER_ID">>) -> UserId end,
    verify_fun = fun(#space_ctx{tested_user = UserId}, ResultBody) ->
        #od_user{full_name = ExpFullName} = ozt_users:get(UserId),
        ?assertMatch(#{<<"fullName">> := ExpFullName}, json_utils:decode(ResultBody))
    end
};
build_sample_test_spec(<<"List user's direct space privileges">>) -> #sample_test_spec{
    substitute_placeholder_fun = fun(#space_ctx{tested_user = UserId}, <<"$USER_ID">>) -> UserId end,
    verify_fun = fun(#space_ctx{space_id = SpaceId, tested_user = UserId}, ResultBody) ->
        ExpPrivs = [atom_to_binary(P) || P <- ozt_spaces:get_user_privileges(SpaceId, UserId)],
        ?assertEqual(#{<<"privileges">> => ExpPrivs}, json_utils:decode(ResultBody))
    end
};
build_sample_test_spec(<<"List user's effective space privileges">>) -> #sample_test_spec{
    substitute_placeholder_fun = fun(#space_ctx{tested_user = UserId}, <<"$USER_ID">>) -> UserId end,
    verify_fun = fun(#space_ctx{space_id = SpaceId, tested_user = UserId}, ResultBody) ->
        ExpPrivs = [atom_to_binary(P) || P <- ozt_spaces:get_eff_user_privileges(SpaceId, UserId)],
        ?assertEqual(#{<<"privileges">> => ExpPrivs}, json_utils:decode(ResultBody))
    end
};
build_sample_test_spec(<<"Update user's space privileges">>) -> #sample_test_spec{
    substitute_placeholder_fun = fun
        (#space_ctx{tested_user = UserId}, <<"$USER_ID">>) -> UserId;
        (_, <<"$PRIVS_TO_GRANT_LIST">>) -> json_utils:encode(?GRANTED_SPACE_PRIVS);
        (_, <<"$PRIVS_TO_REVOKE_LIST">>) -> json_utils:encode(?REVOKED_SPACE_PRIVS)
    end,
    verify_fun = fun(#space_ctx{space_id = SpaceId, tested_user = UserId}, _ResultBody) ->
        CurrentPrivs = ozt_spaces:get_eff_user_privileges(SpaceId, UserId),
        ?assert(lists_utils:is_subset(?GRANTED_SPACE_PRIVS, CurrentPrivs)),
        ?assertNot(lists_utils:is_subset(?REVOKED_SPACE_PRIVS, CurrentPrivs))
    end
};
build_sample_test_spec(<<"List direct space groups">>) -> #sample_test_spec{
    substitute_placeholder_fun = fun(#space_ctx{tested_group = GroupId}, <<"$GROUP_ID">>) -> GroupId end,
    verify_fun = fun(#space_ctx{space_id = SpaceId}, ResultBody) ->
        #{<<"groups">> := Groups} = ?assertMatch(#{<<"groups">> := _}, json_utils:decode(ResultBody)),
        ?assertEqual(lists:sort(ozt_spaces:get_groups(SpaceId)), lists:sort(Groups))
    end
};
build_sample_test_spec(<<"List effective space groups">>) -> #sample_test_spec{
    substitute_placeholder_fun = fun(#space_ctx{tested_group = GroupId}, <<"$GROUP_ID">>) -> GroupId end,
    verify_fun = fun(#space_ctx{space_id = SpaceId}, ResultBody) ->
        #{<<"groups">> := Groups} = ?assertMatch(#{<<"groups">> := _}, json_utils:decode(ResultBody)),
        ?assertEqual(lists:sort(ozt_spaces:get_eff_groups(SpaceId)), lists:sort(Groups))
    end
};
build_sample_test_spec(<<"Get effective space group details">>) -> #sample_test_spec{
    substitute_placeholder_fun = fun(#space_ctx{tested_group = GroupId}, <<"$GROUP_ID">>) -> GroupId end,
    verify_fun = fun(#space_ctx{tested_group = GroupId}, ResultBody) ->
        #od_group{name = ExpName} = ozt_groups:get(GroupId),
        ?assertMatch(#{<<"name">> := ExpName}, json_utils:decode(ResultBody))
    end
};
build_sample_test_spec(<<"List group's direct space privileges">>) -> #sample_test_spec{
    substitute_placeholder_fun = fun(#space_ctx{tested_group = GroupId}, <<"$GROUP_ID">>) -> GroupId end,
    verify_fun = fun(#space_ctx{space_id = SpaceId, tested_group = GroupId}, ResultBody) ->
        ExpPrivs = [atom_to_binary(P) || P <- ozt_spaces:get_group_privileges(SpaceId, GroupId)],
        ?assertEqual(#{<<"privileges">> => ExpPrivs}, json_utils:decode(ResultBody))
    end
};
build_sample_test_spec(<<"List group's effective space privileges">>) -> #sample_test_spec{
    substitute_placeholder_fun = fun(#space_ctx{tested_group = GroupId}, <<"$GROUP_ID">>) -> GroupId end,
    verify_fun = fun(#space_ctx{space_id = SpaceId, tested_group = GroupId}, ResultBody) ->
        ExpPrivs = [atom_to_binary(P) || P <- ozt_spaces:get_eff_group_privileges(SpaceId, GroupId)],
        ?assertEqual(#{<<"privileges">> => ExpPrivs}, json_utils:decode(ResultBody))
    end
};
build_sample_test_spec(<<"Update group's space privileges">>) -> #sample_test_spec{
    substitute_placeholder_fun = fun
        (#space_ctx{tested_group = GroupId}, <<"$GROUP_ID">>) -> GroupId;
        (_, <<"$PRIVS_TO_GRANT_LIST">>) -> json_utils:encode(?GRANTED_SPACE_PRIVS);
        (_, <<"$PRIVS_TO_REVOKE_LIST">>) -> json_utils:encode(?REVOKED_SPACE_PRIVS)
    end,
    verify_fun = fun(#space_ctx{space_id = SpaceId, tested_group = GroupId}, _ResultBody) ->
        CurrentPrivs = ozt_spaces:get_eff_group_privileges(SpaceId, GroupId),
        ?assert(lists_utils:is_subset(?GRANTED_SPACE_PRIVS, CurrentPrivs)),
        ?assertNot(lists_utils:is_subset(?REVOKED_SPACE_PRIVS, CurrentPrivs))
    end
};
build_sample_test_spec(<<"List space shares">>) -> #sample_test_spec{
    verify_fun = fun(#space_ctx{space_id = SpaceId}, ResultBody) ->
        #{<<"shares">> := Shares} = ?assertMatch(#{<<"shares">> := _}, json_utils:decode(ResultBody)),
        ?assertEqual(lists:sort(ozt_spaces:get_shares(SpaceId)), lists:sort(Shares))
    end
}.


%% @private
-spec exp_operation_list() -> rest_api_samples_test_utils:operation_listing().
exp_operation_list() -> [
    {'GET', <<"get_space">>, <<"Get space details">>},
    {'GET', <<"list_space_privileges">>, <<"List all space privileges">>},

    {'GET', <<"list_space_users">>, <<"List direct space users">>},
    {'GET', <<"list_effective_space_users">>, <<"List effective space users">>},
    {'GET', <<"get_effective_space_user">>, <<"Get effective space user details">>},
    {'GET', <<"list_user_space_privileges">>, <<"List user's direct space privileges">>},
    {'GET', <<"list_effective_user_space_privileges">>, <<"List user's effective space privileges">>},
    {'PATCH', <<"update_user_space_privileges">>, <<"Update user's space privileges">>},

    {'GET', <<"list_space_groups">>, <<"List direct space groups">>},
    {'GET', <<"list_effective_space_groups">>, <<"List effective space groups">>},
    {'GET', <<"get_effective_space_group">>, <<"Get effective space group details">>},
    {'GET', <<"list_group_space_privileges">>, <<"List group's direct space privileges">>},
    {'GET', <<"list_effective_group_space_privileges">>, <<"List group's effective space privileges">>},
    {'PATCH', <<"update_group_space_privileges">>, <<"Update group's space privileges">>},

    {'GET', <<"list_space_shares">>, <<"List space shares">>}
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

init_per_testcase(_, Config) ->
    ozt_mocks:freeze_time(),
    Config.

end_per_testcase(_, _Config) ->
    ozt_mocks:unfreeze_time().
