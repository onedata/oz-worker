%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This test verifies if database documents are correctly upgraded through
%%% all versions.
%%% @end
%%%-------------------------------------------------------------------
-module(model_upgrade_test_SUITE).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

-define(DUMMY_TIMESTAMP, 1539770225).
-define(OZ_NODES(Config), ?config(oz_worker_nodes, Config)).

%% API
-export([
    all/0, init_per_suite/1, end_per_suite/1
]).
-export([
    user_upgrade_test/1,
    group_upgrade_test/1,
    space_upgrade_test/1,
    share_upgrade_test/1,
    provider_upgrade_test/1,
    handle_service_upgrade_test/1,
    handle_upgrade_test/1,
    harvester_upgrade_test/1,
    cluster_upgrade_test/1,
    dns_state_upgrade_test/1,
    token_upgrade_test/1,
    macaroon_auth_upgrade_test/1,
    generate_cluster_for_a_legacy_provider_test/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    user_upgrade_test,
    group_upgrade_test,
    space_upgrade_test,
    share_upgrade_test,
    provider_upgrade_test,
    handle_service_upgrade_test,
    handle_upgrade_test,
    harvester_upgrade_test,
    cluster_upgrade_test,
    dns_state_upgrade_test,
    token_upgrade_test,
    macaroon_auth_upgrade_test,
    generate_cluster_for_a_legacy_provider_test
]).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    Posthook = fun(NewConfig) ->
        Nodes = ?config(oz_worker_nodes, NewConfig),
        ok = test_utils:mock_new(Nodes, time_utils, [passthrough]),
        ok = test_utils:mock_expect(Nodes, time_utils, system_time_seconds, fun() ->
            ?DUMMY_TIMESTAMP
        end),
        NewConfig
    end,
    [{env_up_posthook, Posthook}, {?LOAD_MODULES, [oz_test_utils]} | Config].

end_per_suite(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_unload(Nodes, [time_utils]),
    ok.


%%%===================================================================
%%% Tests
%%%===================================================================

user_upgrade_test(Config) ->
    test_record_upgrade(Config, od_user).


group_upgrade_test(Config) ->
    test_record_upgrade(Config, od_group).


space_upgrade_test(Config) ->
    test_record_upgrade(Config, od_space).


share_upgrade_test(Config) ->
    test_record_upgrade(Config, od_share).


provider_upgrade_test(Config) ->
    test_record_upgrade(Config, od_provider).


handle_service_upgrade_test(Config) ->
    test_record_upgrade(Config, od_handle_service).


handle_upgrade_test(Config) ->
    test_record_upgrade(Config, od_handle).


harvester_upgrade_test(Config) ->
    test_record_upgrade(Config, od_harvester).


cluster_upgrade_test(Config) ->
    test_record_upgrade(Config, od_cluster).


dns_state_upgrade_test(Config) ->
    test_record_upgrade(Config, dns_state).


token_upgrade_test(Config) ->
    test_record_upgrade(Config, token).


macaroon_auth_upgrade_test(Config) ->
    test_record_upgrade(Config, macaroon_auth).


generate_cluster_for_a_legacy_provider_test(Config) ->
    Provider1 = datastore_utils:gen_key(),
    Cluster1 = Provider1,
    ?assertMatch({ok, false}, oz_test_utils:call_oz(Config, od_cluster, exists, [Cluster1])),
    LegacyProviderDoc1 = #document{key = Provider1, value = #od_provider{
        name = <<"dummy1">>
    }},
    ?assertMatch({ok, _}, oz_test_utils:call_oz(Config, od_provider, save, [LegacyProviderDoc1])),
    ?assertMatch({ok, true}, oz_test_utils:call_oz(Config, od_cluster, exists, [Cluster1])),

    Provider2 = datastore_utils:gen_key(),
    Cluster2 = Provider2,
    ?assertMatch({error, not_found}, oz_test_utils:call_oz(Config, od_cluster, get, [Cluster2])),
    LegacyProviderDoc2 = #document{key = Provider2, value = #od_provider{
        name = <<"dummy2">>
    }},
    ?assertMatch({ok, _}, oz_test_utils:call_oz(Config, od_provider, save, [LegacyProviderDoc2])),
    ?assertMatch({ok, #document{
        key = Provider2, value = #od_cluster{}}
    }, oz_test_utils:call_oz(Config, od_cluster, get, [Cluster2])).


%%%===================================================================
%%% Helper functions
%%%===================================================================

test_record_upgrade(Config, Model) ->
    CurrentVersion = oz_test_utils:call_oz(Config, Model, get_record_version, []),
    test_record_upgrade(Config, Model, lists:seq(1, CurrentVersion)).

test_record_upgrade(Config, RecordType, Versions) ->
    Nodes = ?OZ_NODES(Config),
    ok = test_utils:mock_new(Nodes, RecordType, [passthrough]),

    % Force saving data to disc
    % Due to that record will be upgraded when fetching
    MockCtx = #{model => RecordType, memory_driver => undefined},
    Key = datastore_utils:gen_key(),

    % Simulate record in first version
    mock_record_version(Config, RecordType, hd(Versions)),

    Doc = #document{
        key = Key,
        value = get_record(RecordType, hd(Versions))
    },
    oz_test_utils:call_oz(Config, datastore_model, save, [MockCtx, Doc]),

    lists:foreach(fun(Version) ->
        % Simulate new record version
        mock_record_version(Config, RecordType, Version),

        % Fetch record which should cause upgrade to the new version
        Result = ?assertMatch({ok, _Doc}, oz_test_utils:call_oz(
            Config, datastore_model, get, [MockCtx, Key]
        )),
        {ok, #document{value = NewRecord, version = NewVersion}} = Result,

        {ExpAfterUpgrade, NextIteration} = case get_record(RecordType, Version) of
            {Rec1, Rec2} -> {Rec1, Rec2};
            Rec1 -> {Rec1, Rec1}
        end,
        ?assertEqual(
            {NewVersion, NewRecord},
            {Version, ExpAfterUpgrade}
        ),
        % Allow changing of the records between versions to enable testing evolution
        % of fields that were added in its lifetime
        oz_test_utils:call_oz(Config, datastore_model, save, [MockCtx, #document{
            key = Key,
            value = NextIteration
        }])
    end, tl(Versions)),

    test_utils:mock_unload(Nodes, RecordType).


mock_record_version(Config, RecordType, Version) ->
    Nodes = ?OZ_NODES(Config),
    ok = test_utils:mock_expect(Nodes, RecordType, get_record_version,
        fun() ->
            Version
        end
    ).

%%%===================================================================
%%% Record definitions
%%%===================================================================

get_record(od_user, 1) -> {od_user,
    <<"name">>,
    <<"username">>,
    true,   % basic_auth_enabled
    <<"alias">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    [
        {oauth_account,
            google,  % provider_id
            <<"user_id1">>,
            <<"username1">>,
            <<"name1">>,
            [<<"email1@email.com">>]
        },
        {oauth_account,
            github,  % provider_id
            <<"user_id2">>,
            <<"username2">>,
            <<"name2">>,
            [<<"email2@email.com">>]
        }
    ],
    <<"default_space">>,
    <<"default_provider">>,
    <<"chosen_provider">>,
    [<<"token1">>, <<"token2">>],   % client_tokens
    #{    % space_aliases
        <<"sp1">> => <<"sp1Name">>,
        <<"sp2">> => <<"sp2Name">>
    },
    [  % oz_privileges
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST,
        oz_groups_list, oz_groups_list_users, oz_groups_list_groups, oz_groups_add_members, oz_groups_remove_members,
        oz_spaces_list, oz_spaces_list_users, oz_spaces_list_groups, oz_spaces_list_providers, oz_spaces_add_members, oz_spaces_remove_members,
        oz_providers_list, oz_providers_list_users, oz_providers_list_groups, oz_providers_list_spaces
    ],
    [],  % eff_oz_privileges
    [<<"group1">>, <<"group2">>, <<"group3">>],
    [<<"space1">>, <<"space2">>, <<"space3">>],
    [<<"hservice1">>, <<"hservice2">>, <<"hservice3">>],
    [<<"handle1">>, <<"handle2">>, <<"handle3">>],
    [<<"eff_group1">>, <<"eff_group2">>, <<"eff_group3">>, <<"eff_group4">>],
    [],  % eff_groups
    [],  % eff_spaces
    [],  % eff_providers
    [],  % eff_handle_services
    [],  % eff_handles
    false  % top_down_dirty
};
get_record(od_user, 2) -> {od_user,
    <<"name">>,
    <<"username">>,
    <<"alias">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    true,
    [
        {oauth_account,
            google,
            <<"user_id1">>,
            <<"username1">>,
            <<"name1">>,
            [<<"email1@email.com">>]
        },
        {oauth_account,
            github,
            <<"user_id2">>,
            <<"username2">>,
            <<"name2">>,
            [<<"email2@email.com">>]
        }
    ],
    <<"default_space">>,
    <<"default_provider">>,
    <<"chosen_provider">>,
    [<<"token1">>, <<"token2">>],
    #{
        <<"sp1">> => <<"sp1Name">>,
        <<"sp2">> => <<"sp2Name">>
    },
    [
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST,
        oz_groups_list, oz_groups_list_users, oz_groups_list_groups, oz_groups_add_members, oz_groups_remove_members,
        oz_spaces_list, oz_spaces_list_users, oz_spaces_list_groups, oz_spaces_list_providers, oz_spaces_add_members, oz_spaces_remove_members,
        oz_providers_list, oz_providers_list_users, oz_providers_list_groups, oz_providers_list_spaces
    ],
    [],
    [<<"group1">>, <<"group2">>, <<"group3">>],
    [<<"space1">>, <<"space2">>, <<"space3">>],
    [<<"hservice1">>, <<"hservice2">>, <<"hservice3">>],
    [<<"handle1">>, <<"handle2">>, <<"handle3">>],
    #{},
    #{},
    #{},
    #{},
    #{},
    true
};
get_record(od_user, 3) -> {od_user,
    <<"name">>,
    <<"username">>,
    <<"alias">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    true,
    [
        {linked_account,
            google,
            <<"user_id1">>,
            <<"username1">>,
            <<"name1">>,
            [<<"email1@email.com">>]
        },
        {linked_account,
            github,
            <<"user_id2">>,
            <<"username2">>,
            <<"name2">>,
            [<<"email2@email.com">>]
        }
    ],
    <<"default_space">>,
    <<"default_provider">>,
    <<"chosen_provider">>,
    [<<"token1">>, <<"token2">>],
    #{
        <<"sp1">> => <<"sp1Name">>,
        <<"sp2">> => <<"sp2Name">>
    },
    [
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST,
        oz_groups_list, oz_groups_list_users, oz_groups_list_groups, oz_groups_add_members, oz_groups_remove_members,
        oz_spaces_list, oz_spaces_list_users, oz_spaces_list_groups, oz_spaces_list_providers, oz_spaces_add_members, oz_spaces_remove_members,
        oz_providers_list, oz_providers_list_users, oz_providers_list_groups, oz_providers_list_spaces
    ],
    [],
    [<<"group1">>, <<"group2">>, <<"group3">>],
    [<<"space1">>, <<"space2">>, <<"space3">>],
    [<<"hservice1">>, <<"hservice2">>, <<"hservice3">>],
    [<<"handle1">>, <<"handle2">>, <<"handle3">>],
    #{},
    #{},
    #{},
    #{},
    #{},
    true
};
get_record(od_user, 4) -> {od_user,
    <<"name">>,
    <<"username">>,
    <<"alias">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    true,
    [
        {linked_account,
            google,
            <<"user_id1">>,
            <<"username1">>,
            <<"name1">>,
            [<<"email1@email.com">>],
            []
        },
        {linked_account,
            github,
            <<"user_id2">>,
            <<"username2">>,
            <<"name2">>,
            [<<"email2@email.com">>],
            []
        }
    ],
    <<"default_space">>,
    <<"default_provider">>,
    <<"chosen_provider">>,
    [<<"token1">>, <<"token2">>],
    #{
        <<"sp1">> => <<"sp1Name">>,
        <<"sp2">> => <<"sp2Name">>
    },
    [
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST,
        oz_groups_list, oz_groups_list_users, oz_groups_list_groups, oz_groups_add_members, oz_groups_remove_members,
        oz_spaces_list, oz_spaces_list_users, oz_spaces_list_groups, oz_spaces_list_providers, oz_spaces_add_members, oz_spaces_remove_members,
        oz_providers_list, oz_providers_list_users, oz_providers_list_groups, oz_providers_list_spaces
    ],
    [],
    [<<"group1">>, <<"group2">>, <<"group3">>],
    [<<"space1">>, <<"space2">>, <<"space3">>],
    [<<"hservice1">>, <<"hservice2">>, <<"hservice3">>],
    [<<"handle1">>, <<"handle2">>, <<"handle3">>],
    #{},
    #{},
    #{},
    #{},
    #{},
    true
};
get_record(od_user, 5) -> {od_user,
    <<"name">>,
    <<"username">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    true,
    [
        {linked_account,
            google,
            <<"user_id1">>,
            <<"username1">>,
            <<"name1">>,
            [<<"email1@email.com">>],
            []
        },
        {linked_account,
            github,
            <<"user_id2">>,
            <<"username2">>,
            <<"name2">>,
            [<<"email2@email.com">>],
            []
        }
    ],
    <<"default_space">>,
    <<"default_provider">>,
    <<"chosen_provider">>,
    [<<"token1">>, <<"token2">>],
    #{
        <<"sp1">> => <<"sp1Name">>,
        <<"sp2">> => <<"sp2Name">>
    },
    [
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST,
        oz_groups_list, oz_groups_list_users, oz_groups_list_groups, oz_groups_add_members, oz_groups_remove_members,
        oz_spaces_list, oz_spaces_list_users, oz_spaces_list_groups, oz_spaces_list_providers, oz_spaces_add_members, oz_spaces_remove_members,
        oz_providers_list, oz_providers_list_users, oz_providers_list_groups, oz_providers_list_spaces
    ],
    [],
    [<<"group1">>, <<"group2">>, <<"group3">>],
    [<<"space1">>, <<"space2">>, <<"space3">>],
    [<<"hservice1">>, <<"hservice2">>, <<"hservice3">>],
    [<<"handle1">>, <<"handle2">>, <<"handle3">>],
    #{},
    #{},
    #{},
    #{},
    #{},
    true
};
get_record(od_user, 6) -> {od_user,
    <<"name">>,
    <<"username">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    true,
    [
        {linked_account,
            google,
            <<"user_id1">>,
            <<"username1">>,
            <<"name1">>,
            [<<"email1@email.com">>],
            []
        },
        {linked_account,
            github,
            <<"user_id2">>,
            <<"username2">>,
            <<"name2">>,
            [<<"email2@email.com">>],
            []
        }
    ],
    <<"default_space">>,
    <<"default_provider">>,
    [<<"token1">>, <<"token2">>],
    #{
        <<"sp1">> => <<"sp1Name">>,
        <<"sp2">> => <<"sp2Name">>
    },
    [
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST,
        oz_groups_list, oz_groups_list_users, oz_groups_list_groups, oz_groups_add_members, oz_groups_remove_members,
        oz_spaces_list, oz_spaces_list_users, oz_spaces_list_groups, oz_spaces_list_providers, oz_spaces_add_members, oz_spaces_remove_members,
        oz_providers_list, oz_providers_list_users, oz_providers_list_groups, oz_providers_list_spaces
    ],
    [],
    [<<"group1">>, <<"group2">>, <<"group3">>],
    [<<"space1">>, <<"space2">>, <<"space3">>],
    [<<"hservice1">>, <<"hservice2">>, <<"hservice3">>],
    [<<"handle1">>, <<"handle2">>, <<"handle3">>],
    #{},
    #{},
    #{},
    #{},
    #{},
    true
};
get_record(od_user, 7) -> {od_user,
    <<"name">>,
    <<"username">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    true,
    [
        {linked_account,
            google,
            <<"user_id1">>,
            <<"username1">>,
            <<"name1">>,
            [<<"email1@email.com">>],
            []
        },
        {linked_account,
            github,
            <<"user_id2">>,
            <<"username2">>,
            <<"name2">>,
            [<<"email2@email.com">>],
            []
        }
    ],
    <<"default_space">>,
    <<"default_provider">>,
    [<<"token1">>, <<"token2">>],
    #{
        <<"sp1">> => <<"sp1Name">>,
        <<"sp2">> => <<"sp2Name">>
    },
    [
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST,
        oz_groups_list, oz_groups_list_users, oz_groups_list_groups, oz_groups_add_members, oz_groups_remove_members,
        oz_spaces_list, oz_spaces_list_users, oz_spaces_list_groups, oz_spaces_list_providers, oz_spaces_add_members, oz_spaces_remove_members,
        oz_providers_list, oz_providers_list_users, oz_providers_list_groups, oz_providers_list_spaces
    ],
    [],
    [<<"group1">>, <<"group2">>, <<"group3">>],
    [<<"space1">>, <<"space2">>, <<"space3">>],
    [<<"hservice1">>, <<"hservice2">>, <<"hservice3">>],
    [<<"handle1">>, <<"handle2">>, <<"handle3">>],
    #{},
    #{},
    #{},
    #{},
    #{},
    true
};
get_record(od_user, 8) -> {od_user,
    <<"name">>,
    <<"username">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    true,
    [
        {linked_account,
            google,
            <<"user_id1">>,
            <<"name1">>,
            <<"username1">>,
            [<<"email1@email.com">>],
            [],
            #{}
        },
        {linked_account,
            github,
            <<"user_id2">>,
            <<"name2">>,
            <<"username2">>,
            [<<"email2@email.com">>],
            [],
            #{}
        }
    ],
    [],

    <<"default_space">>,
    <<"default_provider">>,

    [<<"token1">>, <<"token2">>],
    #{
        <<"sp1">> => <<"sp1Name">>,
        <<"sp2">> => <<"sp2Name">>
    },

    [
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST,
        oz_groups_list, oz_groups_list_users, oz_groups_list_groups, oz_groups_add_members, oz_groups_remove_members,
        oz_spaces_list, oz_spaces_list_users, oz_spaces_list_groups, oz_spaces_list_providers, oz_spaces_add_members, oz_spaces_remove_members,
        oz_providers_list, oz_providers_list_users, oz_providers_list_groups, oz_providers_list_spaces
    ],
    [],

    [<<"group1">>, <<"group2">>, <<"group3">>],
    [<<"space1">>, <<"space2">>, <<"space3">>],
    [<<"hservice1">>, <<"hservice2">>, <<"hservice3">>],
    [<<"handle1">>, <<"handle2">>, <<"handle3">>],

    #{},
    #{},
    #{},
    #{},
    #{},
    true
};
get_record(od_user, 9) -> {od_user,
    <<"name">>,
    <<"username">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    true,

    [
        {linked_account,
            google,
            <<"user_id1">>,
            <<"name1">>,
            <<"username1">>,
            [<<"email1@email.com">>],
            [],
            #{},
            {undefined, 0},
            undefined
        },
        {linked_account,
            github,
            <<"user_id2">>,
            <<"name2">>,
            <<"username2">>,
            [<<"email2@email.com">>],
            [],
            #{},
            {undefined, 0},
            undefined
        }
    ],
    [],

    <<"default_space">>,
    <<"default_provider">>,

    [<<"token1">>, <<"token2">>],
    #{
        <<"sp1">> => <<"sp1Name">>,
        <<"sp2">> => <<"sp2Name">>
    },

    [
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST,
        oz_groups_list, oz_groups_list_users, oz_groups_list_groups, oz_groups_add_members, oz_groups_remove_members,
        oz_spaces_list, oz_spaces_list_users, oz_spaces_list_groups, oz_spaces_list_providers, oz_spaces_add_members, oz_spaces_remove_members,
        oz_providers_list, oz_providers_list_users, oz_providers_list_groups, oz_providers_list_spaces
    ],
    [],

    [<<"group1">>, <<"group2">>, <<"group3">>],
    [<<"space1">>, <<"space2">>, <<"space3">>],
    [<<"hservice1">>, <<"hservice2">>, <<"hservice3">>],
    [<<"handle1">>, <<"handle2">>, <<"handle3">>],

    #{},
    #{},
    #{},
    #{},
    #{},
    true
};
get_record(od_user, 10) -> {
    % Returns two records:
    %   ExpAfterUpgrade - expected value after upgrade from previous version
    %   NextIteration - different record that will be upgraded to the next version
    {od_user,
        <<"name">>,
        <<"username">>,
        [<<"email1@email.com">>, <<"email2@email.com">>],
        true,
        undefined,

        [
            #linked_account{
                idp = google,
                subject_id = <<"user_id1">>,
                full_name = <<"name1">>,
                username = <<"username1">>,
                emails = [<<"email1@email.com">>],
                entitlements = [],
                custom = #{},
                access_token = {undefined, 0},
                refresh_token = undefined
            },
            #linked_account{
                idp = github,
                subject_id = <<"user_id2">>,
                full_name = <<"name2">>,
                username = <<"username2">>,
                emails = [<<"email2@email.com">>],
                entitlements = [],
                custom = #{},
                access_token = {undefined, 0},
                refresh_token = undefined
            }
        ],
        [],

        [],

        <<"default_space">>,
        <<"default_provider">>,

        [<<"token1">>, <<"token2">>],
        #{
            <<"sp1">> => <<"sp1Name">>,
            <<"sp2">> => <<"sp2Name">>
        },

        [
            ?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_GROUPS_LIST, ?OZ_GROUPS_LIST_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_VIEW,
            ?OZ_PROVIDERS_LIST, ?OZ_PROVIDERS_LIST_RELATIONSHIPS, ?OZ_PROVIDERS_VIEW,
            ?OZ_SET_PRIVILEGES,
            ?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_SPACES_LIST, ?OZ_SPACES_LIST_RELATIONSHIPS, ?OZ_SPACES_REMOVE_RELATIONSHIPS, ?OZ_SPACES_VIEW,
            ?OZ_USERS_LIST, ?OZ_USERS_VIEW, ?OZ_VIEW_PRIVILEGES
        ],
        [],

        [<<"group1">>, <<"group2">>, <<"group3">>],
        [<<"space1">>, <<"space2">>, <<"space3">>],
        [<<"hservice1">>, <<"hservice2">>, <<"hservice3">>],
        [<<"handle1">>, <<"handle2">>, <<"handle3">>],
        [],
        [],

        #{},
        #{},
        #{},
        #{},
        #{},
        #{},
        #{},

        ?DUMMY_TIMESTAMP,

        true
    },
    {od_user,
        <<"name">>,
        <<"username">>,
        [<<"email1@email.com">>, <<"email2@email.com">>],
        true,
        undefined,

        [
            #linked_account{
                idp = google,
                subject_id = <<"user_id1">>,
                full_name = <<"name1">>,
                username = <<"username1">>,
                emails = [<<"email1@email.com">>],
                entitlements = [],
                custom = #{},
                access_token = {undefined, 0},
                refresh_token = undefined
            },
            #linked_account{
                idp = github,
                subject_id = <<"user_id2">>,
                full_name = <<"name2">>,
                username = <<"username2">>,
                emails = [<<"email2@email.com">>],
                entitlements = [],
                custom = #{},
                access_token = {undefined, 0},
                refresh_token = undefined
            }
        ],
        [<<"ent1">>, <<"ent2">>, <<"ent3">>],

        [],

        <<"default_space">>,
        <<"default_provider">>,

        [<<"token1">>, <<"token2">>],
        #{
            <<"sp1">> => <<"sp1Name">>,
            <<"sp2">> => <<"sp2Name">>
        },

        [
            ?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_GROUPS_LIST, ?OZ_GROUPS_LIST_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_VIEW,
            ?OZ_PROVIDERS_LIST, ?OZ_PROVIDERS_LIST_RELATIONSHIPS, ?OZ_PROVIDERS_VIEW,
            ?OZ_SET_PRIVILEGES,
            ?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_SPACES_LIST, ?OZ_SPACES_LIST_RELATIONSHIPS, ?OZ_SPACES_REMOVE_RELATIONSHIPS, ?OZ_SPACES_VIEW,
            ?OZ_USERS_LIST, ?OZ_USERS_VIEW, ?OZ_VIEW_PRIVILEGES
        ],
        [],

        [<<"group1">>, <<"group2">>, <<"group3">>],
        [<<"space1">>, <<"space2">>, <<"space3">>],
        [<<"hservice1">>, <<"hservice2">>, <<"hservice3">>],
        [<<"handle1">>, <<"handle2">>, <<"handle3">>],
        [],
        [],

        #{},
        #{},
        #{},
        #{},
        #{},
        #{},
        #{},

        ?DUMMY_TIMESTAMP,

        true
    }
};
get_record(od_user, 11) -> #od_user{
    full_name = <<"name">>,
    username = <<"username">>,
    emails = [<<"email1@email.com">>, <<"email2@email.com">>],
    basic_auth_enabled = true,
    password_hash = undefined,

    linked_accounts = [
        #linked_account{
            idp = google,
            subject_id = <<"user_id1">>,
            full_name = <<"name1">>,
            username = <<"username1">>,
            emails = [<<"email1@email.com">>],
            entitlements = [],
            custom = #{},
            access_token = {undefined, 0},
            refresh_token = undefined
        },
        #linked_account{
            idp = github,
            subject_id = <<"user_id2">>,
            full_name = <<"name2">>,
            username = <<"username2">>,
            emails = [<<"email2@email.com">>],
            entitlements = [],
            custom = #{},
            access_token = {undefined, 0},
            refresh_token = undefined
        }
    ],
    entitlements = [{<<"ent1">>, member}, {<<"ent2">>, member}, {<<"ent3">>, member}],

    active_sessions = [],

    default_space = <<"default_space">>,
    default_provider = <<"default_provider">>,

    client_tokens = [<<"token1">>, <<"token2">>],
    space_aliases = #{
        <<"sp1">> => <<"sp1Name">>,
        <<"sp2">> => <<"sp2Name">>
    },

    oz_privileges = [
        ?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_GROUPS_LIST, ?OZ_GROUPS_LIST_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_VIEW,
        ?OZ_PROVIDERS_LIST, ?OZ_PROVIDERS_LIST_RELATIONSHIPS, ?OZ_PROVIDERS_VIEW,
        ?OZ_SET_PRIVILEGES,
        ?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_SPACES_LIST, ?OZ_SPACES_LIST_RELATIONSHIPS, ?OZ_SPACES_REMOVE_RELATIONSHIPS, ?OZ_SPACES_VIEW,
        ?OZ_USERS_LIST, ?OZ_USERS_VIEW, ?OZ_VIEW_PRIVILEGES
    ],
    eff_oz_privileges = [],

    groups = [<<"group1">>, <<"group2">>, <<"group3">>],
    spaces = [<<"space1">>, <<"space2">>, <<"space3">>],
    handle_services = [<<"hservice1">>, <<"hservice2">>, <<"hservice3">>],
    handles = [<<"handle1">>, <<"handle2">>, <<"handle3">>],
    harvesters = [],
    clusters = [],

    eff_groups = #{},
    eff_spaces = #{},
    eff_providers = #{},
    eff_handle_services = #{},
    eff_handles = #{},
    eff_harvesters = #{},
    eff_clusters = #{},

    creation_time = ?DUMMY_TIMESTAMP,

    top_down_dirty = true
};


get_record(od_group, 1) -> {od_group,
    <<"ńąµę|"/utf8>>,
    role,
    [ % oz_privileges
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST,
        oz_groups_list, oz_groups_list_users, oz_groups_list_groups, oz_groups_add_members, oz_groups_remove_members,
        oz_spaces_list, oz_spaces_list_users, oz_spaces_list_groups, oz_spaces_list_providers, oz_spaces_add_members, oz_spaces_remove_members,
        oz_providers_list, oz_providers_list_users, oz_providers_list_groups, oz_providers_list_spaces
    ],
    [], % eff_oz_privileges
    [<<"parent1">>, <<"parent2">>],
    [
        {<<"child1">>, [?GROUP_VIEW, group_join_space, group_invite_group]},
        {<<"child2">>, [?GROUP_UPDATE, ?GROUP_DELETE, group_remove_group]}
    ],
    [], % eff_parents
    [], % eff_children
    [
        {<<"user1">>, [group_create_space, ?GROUP_SET_PRIVILEGES, group_join_group]},
        {<<"user2">>, [?GROUP_UPDATE, ?GROUP_VIEW, group_leave_group, group_invite_user]}
    ],
    [<<"space1">>, <<"space2">>, <<"space3">>],
    [<<"handle_service1">>],
    [<<"handle1">>, <<"handle2">>],
    [], % eff_users
    [], % eff_spaces
    [], % eff_shares
    [], % eff_providers
    [], % eff_handle_services
    [], % eff_handles
    false, % top_down_dirty
    false  % bottom_up_dirty
};
get_record(od_group, 2) -> {od_group,
    <<"ńąµę|"/utf8>>,
    role,
    [ % oz_privileges
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST,
        oz_groups_list, oz_groups_list_users, oz_groups_list_groups, oz_groups_add_members, oz_groups_remove_members,
        oz_spaces_list, oz_spaces_list_users, oz_spaces_list_groups, oz_spaces_list_providers, oz_spaces_add_members, oz_spaces_remove_members,
        oz_providers_list, oz_providers_list_users, oz_providers_list_groups, oz_providers_list_spaces
    ],
    [],

    [<<"parent1">>, <<"parent2">>],
    #{
        <<"child1">> => [?GROUP_VIEW, group_join_space, group_invite_group],
        <<"child2">> =>  [?GROUP_UPDATE, ?GROUP_DELETE, group_remove_group]
    },
    #{}, % eff_parents
    #{}, % eff_children

    #{
        <<"user1">> => [group_create_space, ?GROUP_SET_PRIVILEGES, group_join_group],
        <<"user2">> => [?GROUP_UPDATE, ?GROUP_VIEW, group_leave_group, group_invite_user]
    },
    [<<"space1">>, <<"space2">>, <<"space3">>],
    [<<"handle_service1">>],
    [<<"handle1">>, <<"handle2">>],

    #{}, % eff_users
    #{}, % eff_spaces
    #{}, % eff_providers
    #{}, % eff_handle_services
    #{}, % eff_handles

    true, % top_down_dirty
    true  % bottom_up_dirty
};
get_record(od_group, 3) -> {od_group,
    <<"ńąµę"/utf8>>,
    role,
    [ % oz_privileges
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST,
        oz_groups_list, oz_groups_list_users, oz_groups_list_groups, oz_groups_add_members, oz_groups_remove_members,
        oz_spaces_list, oz_spaces_list_users, oz_spaces_list_groups, oz_spaces_list_providers, oz_spaces_add_members, oz_spaces_remove_members,
        oz_providers_list, oz_providers_list_users, oz_providers_list_groups, oz_providers_list_spaces
    ],
    [],

    [<<"parent1">>, <<"parent2">>],
    #{
        <<"child1">> => [?GROUP_VIEW, group_join_space, group_invite_group],
        <<"child2">> =>  [?GROUP_UPDATE, ?GROUP_DELETE, group_remove_group]
    },
    #{},
    #{},

    #{
        <<"user1">> => [group_create_space, ?GROUP_SET_PRIVILEGES, group_join_group],
        <<"user2">> => [?GROUP_UPDATE, ?GROUP_VIEW, group_leave_group, group_invite_user]
    },
    [<<"space1">>, <<"space2">>, <<"space3">>],
    [<<"handle_service1">>],
    [<<"handle1">>, <<"handle2">>],

    #{},
    #{},
    #{},
    #{},
    #{},

    true,
    true
};
get_record(od_group, 4) ->
    get_record(od_group, 3);
get_record(od_group, 5) ->
    V4 = get_record(od_group, 4),
    % 3rd field -> type
    setelement(3, V4, role_holders);
get_record(od_group, 6) -> {
    % Returns two records:
    %   ExpAfterUpgrade - expected value after upgrade from previous version
    %   NextIteration - different record that will be upgraded to the next version
    #od_group{
        name = <<"ńąµę"/utf8>>,
        type = role_holders,
        protected = false,
        oz_privileges = [
            ?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_GROUPS_LIST, ?OZ_GROUPS_LIST_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_VIEW,
            ?OZ_PROVIDERS_LIST, ?OZ_PROVIDERS_LIST_RELATIONSHIPS, ?OZ_PROVIDERS_VIEW,
            ?OZ_SET_PRIVILEGES,
            ?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_SPACES_LIST, ?OZ_SPACES_LIST_RELATIONSHIPS, ?OZ_SPACES_REMOVE_RELATIONSHIPS, ?OZ_SPACES_VIEW,
            ?OZ_USERS_LIST, ?OZ_USERS_VIEW, ?OZ_VIEW_PRIVILEGES
        ],
        eff_oz_privileges = [],

        parents = [<<"parent1">>, <<"parent2">>],
        children = #{
            <<"child1">> => [?GROUP_ADD_CHILD, ?GROUP_ADD_SPACE, ?GROUP_VIEW, ?GROUP_VIEW_PRIVILEGES],
            <<"child2">> => [?GROUP_DELETE, ?GROUP_REMOVE_CHILD, ?GROUP_UPDATE]
        },
        eff_parents = #{},
        eff_children = #{},

        users = #{
            <<"user1">> => [?GROUP_ADD_PARENT, ?GROUP_ADD_SPACE, ?GROUP_SET_PRIVILEGES],
            <<"user2">> => [?GROUP_ADD_USER, ?GROUP_LEAVE_PARENT, ?GROUP_UPDATE, ?GROUP_VIEW, ?GROUP_VIEW_PRIVILEGES]
        },
        spaces = [<<"space1">>, <<"space2">>, <<"space3">>],
        handle_services = [<<"handle_service1">>],
        handles = [<<"handle1">>, <<"handle2">>],
        harvesters = [],
        clusters = [],

        eff_users = #{},
        eff_spaces = #{},
        eff_providers = #{},
        eff_handle_services = #{},
        eff_handles = #{},
        eff_harvesters = #{},
        eff_clusters = #{},

        creation_time = ?DUMMY_TIMESTAMP,
        creator = undefined,

        top_down_dirty = true,
        bottom_up_dirty = true
    },
    #od_group{
        name = <<"ńąµę"/utf8>>,
        type = role_holders,
        protected = false,
        oz_privileges = [
            ?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_GROUPS_LIST, ?OZ_GROUPS_LIST_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_VIEW,
            ?OZ_PROVIDERS_LIST, ?OZ_PROVIDERS_LIST_RELATIONSHIPS, ?OZ_PROVIDERS_VIEW,
            ?OZ_SET_PRIVILEGES,
            ?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_SPACES_LIST, ?OZ_SPACES_LIST_RELATIONSHIPS, ?OZ_SPACES_REMOVE_RELATIONSHIPS, ?OZ_SPACES_VIEW,
            ?OZ_USERS_LIST, ?OZ_USERS_VIEW, ?OZ_VIEW_PRIVILEGES
        ],
        eff_oz_privileges = [],

        parents = [<<"parent1">>, <<"parent2">>],
        children = #{
            <<"child1">> => [?GROUP_ADD_CHILD, ?GROUP_ADD_SPACE, ?GROUP_VIEW, ?GROUP_VIEW_PRIVILEGES],
            <<"child2">> => [?GROUP_DELETE, ?GROUP_REMOVE_CHILD, ?GROUP_UPDATE]
        },
        eff_parents = #{},
        eff_children = #{},

        users = #{
            <<"user1">> => [?GROUP_ADD_PARENT, ?GROUP_ADD_SPACE, ?GROUP_SET_PRIVILEGES],
            <<"user2">> => [?GROUP_ADD_USER, ?GROUP_LEAVE_PARENT, ?GROUP_UPDATE, ?GROUP_VIEW, ?GROUP_VIEW_PRIVILEGES]
        },
        spaces = [<<"space1">>, <<"space2">>, <<"space3">>],
        handle_services = [<<"handle_service1">>],
        handles = [<<"handle1">>, <<"handle2">>],
        harvesters = [],
        clusters = [],

        eff_users = #{},
        eff_spaces = #{},
        eff_providers = #{},
        eff_handle_services = #{},
        eff_handles = #{},
        eff_harvesters = #{},
        eff_clusters = #{},

        creation_time = ?DUMMY_TIMESTAMP,
        creator = {client, user, <<"userId123">>},

        top_down_dirty = true,
        bottom_up_dirty = true
    }
};
get_record(od_group, 7) -> #od_group{
    name = <<"ńąµę"/utf8>>,
    type = role_holders,
    protected = false,
    oz_privileges = [
        ?OZ_GROUPS_ADD_RELATIONSHIPS, ?OZ_GROUPS_LIST, ?OZ_GROUPS_LIST_RELATIONSHIPS, ?OZ_GROUPS_REMOVE_RELATIONSHIPS, ?OZ_GROUPS_VIEW,
        ?OZ_PROVIDERS_LIST, ?OZ_PROVIDERS_LIST_RELATIONSHIPS, ?OZ_PROVIDERS_VIEW,
        ?OZ_SET_PRIVILEGES,
        ?OZ_SPACES_ADD_RELATIONSHIPS, ?OZ_SPACES_LIST, ?OZ_SPACES_LIST_RELATIONSHIPS, ?OZ_SPACES_REMOVE_RELATIONSHIPS, ?OZ_SPACES_VIEW,
        ?OZ_USERS_LIST, ?OZ_USERS_VIEW, ?OZ_VIEW_PRIVILEGES
    ],
    eff_oz_privileges = [],

    parents = [<<"parent1">>, <<"parent2">>],
    children = #{
        <<"child1">> => [?GROUP_ADD_CHILD, ?GROUP_ADD_SPACE, ?GROUP_VIEW, ?GROUP_VIEW_PRIVILEGES],
        <<"child2">> => [?GROUP_DELETE, ?GROUP_REMOVE_CHILD, ?GROUP_UPDATE]
    },
    eff_parents = #{},
    eff_children = #{},

    users = #{
        <<"user1">> => [?GROUP_ADD_PARENT, ?GROUP_ADD_SPACE, ?GROUP_SET_PRIVILEGES],
        <<"user2">> => [?GROUP_ADD_USER, ?GROUP_LEAVE_PARENT, ?GROUP_UPDATE, ?GROUP_VIEW, ?GROUP_VIEW_PRIVILEGES]
    },
    spaces = [<<"space1">>, <<"space2">>, <<"space3">>],
    handle_services = [<<"handle_service1">>],
    handles = [<<"handle1">>, <<"handle2">>],
    harvesters = [],
    clusters = [],

    eff_users = #{},
    eff_spaces = #{},
    eff_providers = #{},
    eff_handle_services = #{},
    eff_handles = #{},
    eff_harvesters = #{},
    eff_clusters = #{},

    creation_time = ?DUMMY_TIMESTAMP,
    creator = ?SUB(user, <<"userId123">>),

    top_down_dirty = true,
    bottom_up_dirty = true
};


get_record(od_space, 1) -> {od_space,
    <<"name">>,
    [
        {<<"prov1">>, 1000},
        {<<"prov2">>, 250000},
        {<<"prov3">>, 19999999}
    ],
    [
        {<<"user1">>, [?SPACE_MANAGE_SHARES, ?SPACE_VIEW, ?SPACE_REMOVE_GROUP]},
        {<<"user2">>, [?SPACE_UPDATE, ?SPACE_SET_PRIVILEGES, space_invite_provider, space_invite_user]}
    ],
    [
        {<<"group1">>, [?SPACE_MANAGE_SHARES, ?SPACE_SET_PRIVILEGES, space_invite_provider]},
        {<<"group2">>, [?SPACE_REMOVE_PROVIDER, ?SPACE_REMOVE_GROUP, ?SPACE_UPDATE, space_invite_group]}
    ],
    [<<"share1">>, <<"share2">>, <<"share3">>, <<"share4">>],
    [],  % eff_users
    [],  % eff_groups
    false, % top_down_dirty
    false  % bottom_up_dirty
};
get_record(od_space, 2) -> {od_space,
    <<"name">>,
    #{
        <<"user1">> => [?SPACE_MANAGE_SHARES, ?SPACE_VIEW, ?SPACE_REMOVE_GROUP],
        <<"user2">> => [?SPACE_UPDATE, ?SPACE_SET_PRIVILEGES, space_invite_provider, space_invite_user]
    },
    #{
        <<"group1">> => [?SPACE_MANAGE_SHARES, ?SPACE_SET_PRIVILEGES, space_invite_provider],
        <<"group2">> => [?SPACE_REMOVE_PROVIDER, ?SPACE_REMOVE_GROUP, ?SPACE_UPDATE, space_invite_group]
    },
    #{
        <<"prov1">> => 1000,
        <<"prov2">> => 250000,
        <<"prov3">> => 19999999
    },
    [<<"share1">>, <<"share2">>, <<"share3">>, <<"share4">>],

    #{},  % eff_users
    #{},  % eff_groups
    #{},  % eff_providers

    true, % top_down_dirty
    true  % bottom_up_dirty
};
get_record(od_space, 3) -> {od_space,
    <<"name">>,
    #{
        <<"user1">> => [?SPACE_MANAGE_SHARES, ?SPACE_VIEW, ?SPACE_REMOVE_GROUP],
        <<"user2">> => [?SPACE_UPDATE, ?SPACE_SET_PRIVILEGES, space_invite_provider, space_invite_user]
    },
    #{
        <<"group1">> => [?SPACE_MANAGE_SHARES, ?SPACE_SET_PRIVILEGES, space_invite_provider],
        <<"group2">> => [?SPACE_REMOVE_PROVIDER, ?SPACE_REMOVE_GROUP, ?SPACE_UPDATE, space_invite_group]
    },
    #{
        <<"prov1">> => 1000,
        <<"prov2">> => 250000,
        <<"prov3">> => 19999999
    },
    [<<"share1">>, <<"share2">>, <<"share3">>, <<"share4">>],

    #{},
    #{},
    #{},

    true,
    true
};
get_record(od_space, 4) -> #od_space{
    name = <<"name">>,
    users = #{
        <<"user1">> => privileges:from_list([
            ?SPACE_MANAGE_SHARES, ?SPACE_VIEW, ?SPACE_VIEW_PRIVILEGES, ?SPACE_REMOVE_GROUP,
            ?SPACE_READ_DATA, space_manage_indexes, space_query_indexes, ?SPACE_VIEW_STATISTICS
        ]),
        <<"user2">> => privileges:from_list([
            ?SPACE_UPDATE, ?SPACE_SET_PRIVILEGES, ?SPACE_ADD_PROVIDER,
            ?SPACE_READ_DATA, space_manage_indexes, space_query_indexes, ?SPACE_VIEW_STATISTICS,
            ?SPACE_ADD_USER
        ])
    },
    groups = #{
        <<"group1">> => privileges:from_list([
            ?SPACE_MANAGE_SHARES, ?SPACE_SET_PRIVILEGES, ?SPACE_ADD_PROVIDER,
            ?SPACE_READ_DATA, space_manage_indexes, space_query_indexes, ?SPACE_VIEW_STATISTICS
        ]),
        <<"group2">> => privileges:from_list([
            ?SPACE_REMOVE_PROVIDER, ?SPACE_REMOVE_GROUP, ?SPACE_UPDATE, ?SPACE_ADD_GROUP,
            ?SPACE_READ_DATA, space_manage_indexes, space_query_indexes, ?SPACE_VIEW_STATISTICS
        ])
    },
    providers = #{
        <<"prov1">> => 1000,
        <<"prov2">> => 250000,
        <<"prov3">> => 19999999
    },
    shares = [<<"share1">>, <<"share2">>, <<"share3">>, <<"share4">>],
    harvesters = [],

    eff_users = #{},
    eff_groups = #{},
    eff_providers = #{},

    creation_time = ?DUMMY_TIMESTAMP,
    creator = undefined,

    top_down_dirty = true,
    bottom_up_dirty = true
};
get_record(od_space, 5) -> {
    % Returns two records:
    %   ExpAfterUpgrade - expected value after upgrade from previous version
    %   NextIteration - different record that will be upgraded to the next version
    #od_space{
        name = <<"name">>,
        users = #{
            <<"user1">> => privileges:from_list([
                ?SPACE_MANAGE_SHARES, ?SPACE_VIEW, ?SPACE_VIEW_CHANGES_STREAM, ?SPACE_VIEW_PRIVILEGES,
                ?SPACE_REMOVE_GROUP, ?SPACE_READ_DATA, ?SPACE_VIEW_STATISTICS,
                ?SPACE_MANAGE_VIEWS, ?SPACE_VIEW_VIEWS, ?SPACE_QUERY_VIEWS
            ]),
            <<"user2">> => privileges:from_list([
                ?SPACE_UPDATE, ?SPACE_SET_PRIVILEGES, ?SPACE_ADD_PROVIDER,
                ?SPACE_READ_DATA, ?SPACE_VIEW_STATISTICS, ?SPACE_ADD_USER,
                ?SPACE_MANAGE_VIEWS, ?SPACE_VIEW_VIEWS, ?SPACE_QUERY_VIEWS
            ])
        },
        groups = #{
            <<"group1">> => privileges:from_list([
                ?SPACE_MANAGE_SHARES, ?SPACE_SET_PRIVILEGES, ?SPACE_ADD_PROVIDER,
                ?SPACE_READ_DATA, ?SPACE_VIEW_STATISTICS,
                ?SPACE_MANAGE_VIEWS, ?SPACE_VIEW_VIEWS, ?SPACE_QUERY_VIEWS
            ]),
            <<"group2">> => privileges:from_list([
                ?SPACE_REMOVE_PROVIDER, ?SPACE_REMOVE_GROUP, ?SPACE_UPDATE, ?SPACE_ADD_GROUP,
                ?SPACE_READ_DATA, ?SPACE_VIEW_STATISTICS,
                ?SPACE_MANAGE_VIEWS, ?SPACE_VIEW_VIEWS, ?SPACE_QUERY_VIEWS
            ])
        },
        providers = #{
            <<"prov1">> => 1000,
            <<"prov2">> => 250000,
            <<"prov3">> => 19999999
        },
        shares = [<<"share1">>, <<"share2">>, <<"share3">>, <<"share4">>],
        harvesters = [],

        eff_users = #{},
        eff_groups = #{},
        eff_providers = #{},

        creation_time = ?DUMMY_TIMESTAMP,
        creator = undefined,

        top_down_dirty = true,
        bottom_up_dirty = true
    },
    #od_space{
        name = <<"name">>,
        users = #{
            <<"user1">> => privileges:from_list([
                ?SPACE_MANAGE_SHARES, ?SPACE_VIEW, ?SPACE_VIEW_CHANGES_STREAM, ?SPACE_VIEW_PRIVILEGES,
                ?SPACE_REMOVE_GROUP, ?SPACE_READ_DATA, ?SPACE_VIEW_STATISTICS,
                ?SPACE_MANAGE_VIEWS, ?SPACE_VIEW_VIEWS, ?SPACE_QUERY_VIEWS
            ]),
            <<"user2">> => privileges:from_list([
                ?SPACE_UPDATE, ?SPACE_SET_PRIVILEGES, ?SPACE_ADD_PROVIDER,
                ?SPACE_READ_DATA, ?SPACE_VIEW_STATISTICS, ?SPACE_ADD_USER,
                ?SPACE_MANAGE_VIEWS, ?SPACE_VIEW_VIEWS, ?SPACE_QUERY_VIEWS
            ])
        },
        groups = #{
            <<"group1">> => privileges:from_list([
                ?SPACE_MANAGE_SHARES, ?SPACE_SET_PRIVILEGES, ?SPACE_ADD_PROVIDER,
                ?SPACE_READ_DATA, ?SPACE_VIEW_STATISTICS,
                ?SPACE_MANAGE_VIEWS, ?SPACE_VIEW_VIEWS, ?SPACE_QUERY_VIEWS
            ]),
            <<"group2">> => privileges:from_list([
                ?SPACE_REMOVE_PROVIDER, ?SPACE_REMOVE_GROUP, ?SPACE_UPDATE, ?SPACE_ADD_GROUP,
                ?SPACE_READ_DATA, ?SPACE_VIEW_STATISTICS,
                ?SPACE_MANAGE_VIEWS, ?SPACE_VIEW_VIEWS, ?SPACE_QUERY_VIEWS
            ])
        },
        providers = #{
            <<"prov1">> => 1000,
            <<"prov2">> => 250000,
            <<"prov3">> => 19999999
        },
        shares = [<<"share1">>, <<"share2">>, <<"share3">>, <<"share4">>],
        harvesters = [],

        eff_users = #{},
        eff_groups = #{},
        eff_providers = #{},

        creation_time = ?DUMMY_TIMESTAMP,
        creator = {client, nobody, <<"">>},

        top_down_dirty = true,
        bottom_up_dirty = true
    }
};
get_record(od_space, 6) -> #od_space{
    name = <<"name">>,
    users = #{
        <<"user1">> => privileges:from_list([
            ?SPACE_MANAGE_SHARES, ?SPACE_VIEW, ?SPACE_VIEW_CHANGES_STREAM, ?SPACE_VIEW_PRIVILEGES,
            ?SPACE_REMOVE_GROUP, ?SPACE_READ_DATA, ?SPACE_VIEW_STATISTICS,
            ?SPACE_MANAGE_VIEWS, ?SPACE_VIEW_VIEWS, ?SPACE_QUERY_VIEWS
        ]),
        <<"user2">> => privileges:from_list([
            ?SPACE_UPDATE, ?SPACE_SET_PRIVILEGES, ?SPACE_ADD_PROVIDER,
            ?SPACE_READ_DATA, ?SPACE_VIEW_STATISTICS, ?SPACE_ADD_USER,
            ?SPACE_MANAGE_VIEWS, ?SPACE_VIEW_VIEWS, ?SPACE_QUERY_VIEWS
        ])
    },
    groups = #{
        <<"group1">> => privileges:from_list([
            ?SPACE_MANAGE_SHARES, ?SPACE_SET_PRIVILEGES, ?SPACE_ADD_PROVIDER,
            ?SPACE_READ_DATA, ?SPACE_VIEW_STATISTICS,
            ?SPACE_MANAGE_VIEWS, ?SPACE_VIEW_VIEWS, ?SPACE_QUERY_VIEWS
        ]),
        <<"group2">> => privileges:from_list([
            ?SPACE_REMOVE_PROVIDER, ?SPACE_REMOVE_GROUP, ?SPACE_UPDATE, ?SPACE_ADD_GROUP,
            ?SPACE_READ_DATA, ?SPACE_VIEW_STATISTICS,
            ?SPACE_MANAGE_VIEWS, ?SPACE_VIEW_VIEWS, ?SPACE_QUERY_VIEWS
        ])
    },
    providers = #{
        <<"prov1">> => 1000,
        <<"prov2">> => 250000,
        <<"prov3">> => 19999999
    },
    shares = [<<"share1">>, <<"share2">>, <<"share3">>, <<"share4">>],
    harvesters = [],

    eff_users = #{},
    eff_groups = #{},
    eff_providers = #{},

    creation_time = ?DUMMY_TIMESTAMP,
    creator = ?SUB(nobody),

    top_down_dirty = true,
    bottom_up_dirty = true
};


get_record(od_share, 1) -> {od_share,
    <<"name">>,
    <<"public_url">>,
    <<"parent_space_id">>,
    <<"handle_id">>,
    <<"root_file_id">>,
    [],  % eff_users
    [],  % eff_groups
    false  % bottom_up_dirty
};
get_record(od_share, 2) -> {od_share,
    <<"name">>,
    <<"public_url">>,
    <<"parent_space_id">>,
    <<"handle_id">>,
    <<"root_file_id">>
};
get_record(od_share, 3) -> {
    % Returns two records:
    %   ExpAfterUpgrade - expected value after upgrade from previous version
    %   NextIteration - different record that will be upgraded to the next version
    #od_share{
        name = <<"name">>,
        public_url = <<"public_url">>,
        space = <<"parent_space_id">>,
        handle = <<"handle_id">>,
        root_file = <<"root_file_id">>,

        creation_time = ?DUMMY_TIMESTAMP,
        creator = undefined
    },
    #od_share{
        name = <<"name">>,
        public_url = <<"public_url">>,
        space = <<"parent_space_id">>,
        handle = <<"handle_id">>,
        root_file = <<"root_file_id">>,

        creation_time = ?DUMMY_TIMESTAMP,
        creator = {client, root, <<"">>}
    }
};
get_record(od_share, 4) -> #od_share{
    name = <<"name">>,
    public_url = <<"public_url">>,
    space = <<"parent_space_id">>,
    handle = <<"handle_id">>,
    root_file = <<"root_file_id">>,

    creation_time = ?DUMMY_TIMESTAMP,
    creator = ?SUB(root)
};


get_record(od_provider, 1) -> {od_provider,
    <<"name">>,
    <<"redirection_point">>,
    [<<"url1.com">>, <<"url2.com">>, <<"url3.com">>],
    <<"cert_serial">>,
    -93.2341, % latitude
    17,       % longitude
    [<<"space1">>, <<"space2">>, <<"space3">>, <<"space4">>],
    [],  % eff_users
    [],  % eff_groups
    false  % bottom_up_dirty
};
get_record(od_provider, 2) -> {od_provider,
    <<"name">>, % name
    <<"redirection_point">>, % redirection_point
    [<<"url1.com">>, <<"url2.com">>, <<"url3.com">>], % urls
    <<"cert_serial">>,
    -93.2341,
    17,

    #{
        % During provider doc translation, extra information is added - support
        % sizes. However, it is not possible to gather this information because
        % during update there is no information about document id in context.
        <<"space1">> => 0,
        <<"space2">> => 0,
        <<"space3">> => 0,
        <<"space4">> => 0
    },

    #{}, % eff_users
    #{}, % eff_groups

    true % bottom_up_dirty
};
get_record(od_provider, 3) -> {od_provider,
    <<"name">>,
    undefined,
    undefined,
    false,
    <<"redirection_point">>,
    undefined,

    -93.2341,
    17,

    #{
        <<"space1">> => 0,
        <<"space2">> => 0,
        <<"space3">> => 0,
        <<"space4">> => 0
    },

    #{}, % eff_users
    #{}, % eff_groups

    true % bottom_up_dirty
};
get_record(od_provider, 4) -> {od_provider,
    <<"name">>,
    undefined,
    undefined,
    false,
    <<"redirection_point">>,
    undefined,

    -93.2341,
    17,

    #{
        <<"space1">> => 0,
        <<"space2">> => 0,
        <<"space3">> => 0,
        <<"space4">> => 0
    },

    #{},
    #{},

    true
};
get_record(od_provider, 5) -> #od_provider{
    name = <<"name">>,
    admin_email = undefined,
    root_macaroon = undefined,
    subdomain_delegation = false,
    domain = <<"redirection_point">>,
    subdomain = undefined,

    latitude = -93.2341,
    longitude = 17,

    spaces = #{
        <<"space1">> => 0,
        <<"space2">> => 0,
        <<"space3">> => 0,
        <<"space4">> => 0
    },

    eff_users = #{},
    eff_groups = #{},

    creation_time = ?DUMMY_TIMESTAMP,

    bottom_up_dirty = true
};


get_record(od_handle_service, 1) -> {od_handle_service,
    <<"name">>,
    <<"proxy_endpoint">>,
    [ % service_properties
        {<<"property1">>, <<"value1">>},
        {<<"property2">>, <<"value2">>},
        {<<"property3">>, <<"value3">>}
    ],
    [
        {<<"user1">>, [?HANDLE_SERVICE_LIST_HANDLES, ?HANDLE_SERVICE_VIEW, ?HANDLE_SERVICE_REGISTER_HANDLE]},
        {<<"user2">>, [?HANDLE_SERVICE_UPDATE, ?HANDLE_SERVICE_DELETE, ?HANDLE_SERVICE_VIEW]}
    ],
    [
        {<<"group1">>, [?HANDLE_SERVICE_DELETE, ?HANDLE_SERVICE_VIEW, ?HANDLE_SERVICE_VIEW]},
        {<<"group2">>, [?HANDLE_SERVICE_LIST_HANDLES, ?HANDLE_SERVICE_UPDATE, ?HANDLE_SERVICE_REGISTER_HANDLE]}
    ],
    [],  % eff_users
    [],  % eff_groups
    false  % bottom_up_dirty
};
get_record(od_handle_service, 2) -> {od_handle_service,
    <<"name">>,
    <<"proxy_endpoint">>,
    #{
        <<"property1">> => <<"value1">>,
        <<"property2">> => <<"value2">>,
        <<"property3">> => <<"value3">>
    },

    #{
        <<"user1">> => [?HANDLE_SERVICE_LIST_HANDLES, ?HANDLE_SERVICE_VIEW, ?HANDLE_SERVICE_REGISTER_HANDLE],
        <<"user2">> => [?HANDLE_SERVICE_UPDATE, ?HANDLE_SERVICE_DELETE, ?HANDLE_SERVICE_VIEW]
    },
    #{
        <<"group1">> => [?HANDLE_SERVICE_DELETE, ?HANDLE_SERVICE_VIEW, ?HANDLE_SERVICE_VIEW],
        <<"group2">> => [?HANDLE_SERVICE_LIST_HANDLES, ?HANDLE_SERVICE_UPDATE, ?HANDLE_SERVICE_REGISTER_HANDLE]
    },
    [],  % handles

    #{},  % eff_users
    #{},  % eff_groups

    true  % bottom_up_dirty
};
get_record(od_handle_service, 3) -> {od_handle_service,
    <<"name">>,
    <<"proxy_endpoint">>,
    #{
        <<"property1">> => <<"value1">>,
        <<"property2">> => <<"value2">>,
        <<"property3">> => <<"value3">>
    },

    #{
        <<"user1">> => [?HANDLE_SERVICE_LIST_HANDLES, ?HANDLE_SERVICE_VIEW, ?HANDLE_SERVICE_REGISTER_HANDLE],
        <<"user2">> => [?HANDLE_SERVICE_UPDATE, ?HANDLE_SERVICE_DELETE, ?HANDLE_SERVICE_VIEW]
    },
    #{
        <<"group1">> => [?HANDLE_SERVICE_DELETE, ?HANDLE_SERVICE_VIEW, ?HANDLE_SERVICE_VIEW],
        <<"group2">> => [?HANDLE_SERVICE_LIST_HANDLES, ?HANDLE_SERVICE_UPDATE, ?HANDLE_SERVICE_REGISTER_HANDLE]
    },
    [],


    #{},
    #{},

    true
};
get_record(od_handle_service, 4) -> {
    % Returns two records:
    %   ExpAfterUpgrade - expected value after upgrade from previous version
    %   NextIteration - different record that will be upgraded to the next version
    #od_handle_service{
        name = <<"name">>,
        proxy_endpoint = <<"proxy_endpoint">>,
        service_properties = #{
            <<"property1">> => <<"value1">>,
            <<"property2">> => <<"value2">>,
            <<"property3">> => <<"value3">>
        },

        users = #{
            <<"user1">> => [?HANDLE_SERVICE_LIST_HANDLES, ?HANDLE_SERVICE_VIEW, ?HANDLE_SERVICE_REGISTER_HANDLE],
            <<"user2">> => [?HANDLE_SERVICE_UPDATE, ?HANDLE_SERVICE_DELETE, ?HANDLE_SERVICE_VIEW]
        },
        groups = #{
            <<"group1">> => [?HANDLE_SERVICE_DELETE, ?HANDLE_SERVICE_VIEW, ?HANDLE_SERVICE_VIEW],
            <<"group2">> => [?HANDLE_SERVICE_LIST_HANDLES, ?HANDLE_SERVICE_UPDATE, ?HANDLE_SERVICE_REGISTER_HANDLE]
        },
        handles = [],


        eff_users = #{},
        eff_groups = #{},

        creation_time = ?DUMMY_TIMESTAMP,
        creator = undefined,

        bottom_up_dirty = true
    },
    #od_handle_service{
        name = <<"name">>,
        proxy_endpoint = <<"proxy_endpoint">>,
        service_properties = #{
            <<"property1">> => <<"value1">>,
            <<"property2">> => <<"value2">>,
            <<"property3">> => <<"value3">>
        },

        users = #{
            <<"user1">> => [?HANDLE_SERVICE_LIST_HANDLES, ?HANDLE_SERVICE_VIEW, ?HANDLE_SERVICE_REGISTER_HANDLE],
            <<"user2">> => [?HANDLE_SERVICE_UPDATE, ?HANDLE_SERVICE_DELETE, ?HANDLE_SERVICE_VIEW]
        },
        groups = #{
            <<"group1">> => [?HANDLE_SERVICE_DELETE, ?HANDLE_SERVICE_VIEW, ?HANDLE_SERVICE_VIEW],
            <<"group2">> => [?HANDLE_SERVICE_LIST_HANDLES, ?HANDLE_SERVICE_UPDATE, ?HANDLE_SERVICE_REGISTER_HANDLE]
        },
        handles = [],


        eff_users = #{},
        eff_groups = #{},

        creation_time = ?DUMMY_TIMESTAMP,
        creator = {client, provider, <<"123123">>},

        bottom_up_dirty = true
    }
};
get_record(od_handle_service, 5) -> #od_handle_service{
    name = <<"name">>,
    proxy_endpoint = <<"proxy_endpoint">>,
    service_properties = #{
        <<"property1">> => <<"value1">>,
        <<"property2">> => <<"value2">>,
        <<"property3">> => <<"value3">>
    },

    users = #{
        <<"user1">> => [?HANDLE_SERVICE_LIST_HANDLES, ?HANDLE_SERVICE_VIEW, ?HANDLE_SERVICE_REGISTER_HANDLE],
        <<"user2">> => [?HANDLE_SERVICE_UPDATE, ?HANDLE_SERVICE_DELETE, ?HANDLE_SERVICE_VIEW]
    },
    groups = #{
        <<"group1">> => [?HANDLE_SERVICE_DELETE, ?HANDLE_SERVICE_VIEW, ?HANDLE_SERVICE_VIEW],
        <<"group2">> => [?HANDLE_SERVICE_LIST_HANDLES, ?HANDLE_SERVICE_UPDATE, ?HANDLE_SERVICE_REGISTER_HANDLE]
    },
    handles = [],


    eff_users = #{},
    eff_groups = #{},

    creation_time = ?DUMMY_TIMESTAMP,
    creator = ?SUB(?ONEPROVIDER, <<"123123">>),

    bottom_up_dirty = true
};


get_record(od_handle, 1) -> {od_handle,
    <<"public_handle">>,
    <<"Share">>, % resource_type
    <<"resource_id">>,
    <<"<metadata_xml_string>">>,
    {{2016, 4, 4}, {14, 56, 33}}, % timestamp
    <<"handle_service_id">>,
    [
        {<<"user1">>, [?HANDLE_VIEW, ?HANDLE_UPDATE]},
        {<<"user2">>, [?HANDLE_VIEW, ?HANDLE_UPDATE, ?HANDLE_DELETE]}
    ],
    [
        {<<"group1">>, [?HANDLE_UPDATE]},
        {<<"group2">>, [?HANDLE_DELETE]}
    ],
    [],  % eff_users
    [],  % eff_groups
    false  % bottom_up_dirty
};
get_record(od_handle, 2) -> {od_handle,
    <<"public_handle">>,
    <<"Share">>, % resource_type
    <<"<metadata_xml_string>">>,
    {{2016, 4, 4}, {14, 56, 33}}, % timestamp

    <<"resource_id">>,
    <<"handle_service_id">>,

    #{
        <<"user1">> => [?HANDLE_VIEW, ?HANDLE_UPDATE],
        <<"user2">> => [?HANDLE_VIEW, ?HANDLE_UPDATE, ?HANDLE_DELETE]
    },
    #{
        <<"group1">> => [?HANDLE_UPDATE],
        <<"group2">> => [?HANDLE_DELETE]
    },

    #{},  % eff_users
    #{},  % eff_groups

    true  % bottom_up_dirty
};
get_record(od_handle, 3) -> {od_handle,
    <<"public_handle">>,
    <<"Share">>,
    <<"<metadata_xml_string>">>,
    {{2016, 4, 4}, {14, 56, 33}},

    <<"resource_id">>,
    <<"handle_service_id">>,

    #{
        <<"user1">> => [?HANDLE_VIEW, ?HANDLE_UPDATE],
        <<"user2">> => [?HANDLE_VIEW, ?HANDLE_UPDATE, ?HANDLE_DELETE]
    },
    #{
        <<"group1">> => [?HANDLE_UPDATE],
        <<"group2">> => [?HANDLE_DELETE]
    },

    #{},
    #{},

    true
};
get_record(od_handle, 4) -> {
    % Returns two records:
    %   ExpAfterUpgrade - expected value after upgrade from previous version
    %   NextIteration - different record that will be upgraded to the next version
    #od_handle{
        public_handle = <<"public_handle">>,
        resource_type = <<"Share">>,
        metadata = <<"<metadata_xml_string>">>,
        timestamp = {{2016, 4, 4}, {14, 56, 33}},

        resource_id = <<"resource_id">>,
        handle_service = <<"handle_service_id">>,

        users = #{
            <<"user1">> => [?HANDLE_VIEW, ?HANDLE_UPDATE],
            <<"user2">> => [?HANDLE_VIEW, ?HANDLE_UPDATE, ?HANDLE_DELETE]
        },
        groups = #{
            <<"group1">> => [?HANDLE_UPDATE],
            <<"group2">> => [?HANDLE_DELETE]
        },

        eff_users = #{},
        eff_groups = #{},

        creation_time = ?DUMMY_TIMESTAMP,
        creator = undefined,

        bottom_up_dirty = true
    },
    #od_handle{
        public_handle = <<"public_handle">>,
        resource_type = <<"Share">>,
        metadata = <<"<metadata_xml_string>">>,
        timestamp = {{2016, 4, 4}, {14, 56, 33}},

        resource_id = <<"resource_id">>,
        handle_service = <<"handle_service_id">>,

        users = #{
            <<"user1">> => [?HANDLE_VIEW, ?HANDLE_UPDATE],
            <<"user2">> => [?HANDLE_VIEW, ?HANDLE_UPDATE, ?HANDLE_DELETE]
        },
        groups = #{
            <<"group1">> => [?HANDLE_UPDATE],
            <<"group2">> => [?HANDLE_DELETE]
        },

        eff_users = #{},
        eff_groups = #{},

        creation_time = ?DUMMY_TIMESTAMP,
        creator = {client, provider, <<"">>},

        bottom_up_dirty = true
    }
};
get_record(od_handle, 5) -> #od_handle{
    public_handle = <<"public_handle">>,
    resource_type = <<"Share">>,
    metadata = <<"<metadata_xml_string>">>,
    timestamp = {{2016, 4, 4}, {14, 56, 33}},

    resource_id = <<"resource_id">>,
    handle_service = <<"handle_service_id">>,

    users = #{
        <<"user1">> => [?HANDLE_VIEW, ?HANDLE_UPDATE],
        <<"user2">> => [?HANDLE_VIEW, ?HANDLE_UPDATE, ?HANDLE_DELETE]
    },
    groups = #{
        <<"group1">> => [?HANDLE_UPDATE],
        <<"group2">> => [?HANDLE_DELETE]
    },

    eff_users = #{},
    eff_groups = #{},

    creation_time = ?DUMMY_TIMESTAMP,
    creator = ?SUB(nobody),

    bottom_up_dirty = true
};


get_record(od_harvester, 1) -> #od_harvester{
    name = <<"h-name">>,
    plugin = elasticsearch_plugin,
    endpoint = <<"https://es.example.com:9056">>,

    gui_plugin_config = #{
        <<"attr1">> => <<"val2">>,
        <<"attr2">> => 15
    },
    public = true,

    indices = #{
        <<"567">> => #harvester_index{
            name = <<"Simulations index">>,
            schema = <<"schema">>,
            gui_plugin_name = <<"simulations">>,
            stats = #{
                <<"space1">> => #{
                    <<"providerA">> => #index_stats{
                        current_seq = 5,
                        max_seq = 17,
                        last_update = ?DUMMY_TIMESTAMP + 18,
                        error = <<"temp-error">>,
                        archival = false
                    }
                },
                <<"space2">> => #{
                    <<"providerB">> => #index_stats{
                        current_seq = 1423,
                        max_seq = 1423,
                        last_update = ?DUMMY_TIMESTAMP + 892,
                        error = undefined,
                        archival = true
                    }
                }
            }
        }

    },

    users = #{
        <<"user1">> => [?HARVESTER_VIEW, ?HARVESTER_UPDATE],
        <<"user2">> => [?HARVESTER_VIEW, ?HARVESTER_UPDATE, ?HARVESTER_DELETE]
    },
    groups = #{
        <<"group1">> => [?HARVESTER_UPDATE],
        <<"group2">> => [?HARVESTER_DELETE]
    },
    spaces = [<<"s1">>, <<"s2">>],

    eff_users = #{},
    eff_groups = #{},
    eff_providers = #{},

    creation_time = ?DUMMY_TIMESTAMP,
    creator = {client, root, <<"">>},

    bottom_up_dirty = true,
    top_down_dirty = true
};
get_record(od_harvester, 2) -> #od_harvester{
    name = <<"h-name">>,
    plugin = elasticsearch_plugin,
    endpoint = <<"https://es.example.com:9056">>,

    gui_plugin_config = #{
        <<"attr1">> => <<"val2">>,
        <<"attr2">> => 15
    },
    public = true,

    indices = #{
        <<"567">> => #harvester_index{
            name = <<"Simulations index">>,
            schema = <<"schema">>,
            gui_plugin_name = <<"simulations">>,
            stats = #{
                <<"space1">> => #{
                    <<"providerA">> => #index_stats{
                        current_seq = 5,
                        max_seq = 17,
                        last_update = ?DUMMY_TIMESTAMP + 18,
                        error = <<"temp-error">>,
                        archival = false
                    }
                },
                <<"space2">> => #{
                    <<"providerB">> => #index_stats{
                        current_seq = 1423,
                        max_seq = 1423,
                        last_update = ?DUMMY_TIMESTAMP + 892,
                        error = undefined,
                        archival = true
                    }
                }
            }
        }

    },

    users = #{
        <<"user1">> => [?HARVESTER_VIEW, ?HARVESTER_UPDATE],
        <<"user2">> => [?HARVESTER_VIEW, ?HARVESTER_UPDATE, ?HARVESTER_DELETE]
    },
    groups = #{
        <<"group1">> => [?HARVESTER_UPDATE],
        <<"group2">> => [?HARVESTER_DELETE]
    },
    spaces = [<<"s1">>, <<"s2">>],

    eff_users = #{},
    eff_groups = #{},
    eff_providers = #{},

    creation_time = ?DUMMY_TIMESTAMP,
    creator = ?SUB(root),

    bottom_up_dirty = true,
    top_down_dirty = true
};


get_record(od_cluster, 1) -> #od_cluster{
    type = ?ONEPROVIDER,

    worker_version = {<<"19.02.0-beta1">>, <<"dc6e5ad5bc-98">>, <<"0bd06a1ac53dbc6db8f9fdd0e59ff0d">>},
    onepanel_version = {<<"19.02.0-beta1">>, <<"d92db7611a-115">>, <<"882856e9067e6c1d6b29416b66154a9">>},
    onepanel_proxy = true,

    creation_time = ?DUMMY_TIMESTAMP,
    creator = {client, user, <<"cluster-admin">>},


    users = #{
        <<"user1">> => [?CLUSTER_VIEW, ?CLUSTER_UPDATE],
        <<"user2">> => [?CLUSTER_VIEW, ?CLUSTER_UPDATE, ?CLUSTER_DELETE]
    },
    groups = #{
        <<"group1">> => [?CLUSTER_UPDATE],
        <<"group2">> => [?CLUSTER_DELETE]
    },

    eff_users = #{},
    eff_groups = #{},

    bottom_up_dirty = false
};
get_record(od_cluster, 2) -> #od_cluster{
    type = ?ONEPROVIDER,

    worker_version = {<<"19.02.0-beta1">>, <<"dc6e5ad5bc-98">>, <<"0bd06a1ac53dbc6db8f9fdd0e59ff0d">>},
    onepanel_version = {<<"19.02.0-beta1">>, <<"d92db7611a-115">>, <<"882856e9067e6c1d6b29416b66154a9">>},
    onepanel_proxy = true,

    creation_time = ?DUMMY_TIMESTAMP,
    creator = ?SUB(user, <<"cluster-admin">>),


    users = #{
        <<"user1">> => [?CLUSTER_VIEW, ?CLUSTER_UPDATE],
        <<"user2">> => [?CLUSTER_VIEW, ?CLUSTER_UPDATE, ?CLUSTER_DELETE]
    },
    groups = #{
        <<"group1">> => [?CLUSTER_UPDATE],
        <<"group2">> => [?CLUSTER_DELETE]
    },

    eff_users = #{},
    eff_groups = #{},

    bottom_up_dirty = false
};


get_record(dns_state, 1) -> {dns_state,
    #{<<"sub">> => <<"p1">>},
    #{<<"p1">> => <<"sub">>},
    #{<<"p1">> => [{1, 2, 3, 4}, {192, 168, 192, 1}]},
    #{<<"p1">> => [
        {<<"_acme-challenge">>, <<"token">>},
        {<<"second">>, <<"value">>}
    ]}
};
get_record(dns_state, 2) -> {dns_state,
    #{<<"sub">> => <<"p1">>},
    #{<<"p1">> => <<"sub">>},
    #{<<"p1">> => [{1, 2, 3, 4}, {192, 168, 192, 1}]},
    #{<<"p1">> => [
        {<<"_acme-challenge">>, <<"token">>, undefined},
        {<<"second">>, <<"value">>, undefined}
    ]}
};


get_record(token, 1) -> {token,
    <<"secret">>,
    resource,
    <<"resource_id">>,
    {client, user, <<"">>}
};
get_record(token, 2) -> {token,
    <<"secret">>,
    resource,
    <<"resource_id">>,
    {client, user, <<"">>},
    false
};
get_record(token, 3) -> #token{
    secret = <<"secret">>,
    resource = resource,
    resource_id = <<"resource_id">>,
    issuer = ?SUB(nobody),
    locked = false
};


get_record(macaroon_auth, 1) -> {macaroon_auth,
    <<"secret">>,
    authorization,
    {client, user, <<"client_id">>}
};
get_record(macaroon_auth, 2) -> #macaroon_auth{
    secret = <<"secret">>,
    type = authorization,
    issuer = ?SUB(user, <<"client_id">>)
}.
