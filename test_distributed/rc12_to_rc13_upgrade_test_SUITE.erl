%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This test verifies database upgrade process between versions
%%% rc12 and rc13.
%%% @end
%%%-------------------------------------------------------------------
-module(rc12_to_rc13_upgrade_test_SUITE).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([
    all/0, init_per_suite/1
]).
-export([
    user_upgrade_test/1,
    group_upgrade_test/1,
    space_upgrade_test/1,
    share_upgrade_test/1,
    provider_upgrade_test/1,
    handle_service_upgrade_test/1,
    handle_upgrade_test/1
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
    handle_upgrade_test
]).


user_upgrade_test(Config) ->
    UserRecordVer1 = user_record_1(),

    {Version2, UserRecordVer2} = oz_test_utils:call_oz(
        Config, od_user, record_upgrade, [1, UserRecordVer1]
    ),
    ?assertEqual(2, Version2),
    ?assertEqual(UserRecordVer2, user_record_2()),

    {Version3, UserRecordVer3} = oz_test_utils:call_oz(
        Config, od_user, record_upgrade, [2, UserRecordVer2]
    ),
    ?assertEqual(3, Version3),
    ?assertEqual(UserRecordVer3, user_record_3()).


group_upgrade_test(Config) ->
    OldGroupRecord = old_group_record(),
    {NewVersion, NewRecord} = oz_test_utils:call_oz(
        Config, od_group, record_upgrade, [1, OldGroupRecord]
    ),
    ?assertEqual(2, NewVersion),
    ?assertEqual(NewRecord, new_group_record()).


space_upgrade_test(Config) ->
    OldSpaceRecord = old_space_record(),
    {NewVersion, NewRecord} = oz_test_utils:call_oz(
        Config, od_space, record_upgrade, [1, OldSpaceRecord]
    ),
    ?assertEqual(2, NewVersion),
    ?assertEqual(NewRecord, new_space_record()).


share_upgrade_test(Config) ->
    OldShareRecord = old_share_record(),
    {NewVersion, NewRecord} = oz_test_utils:call_oz(
        Config, od_share, record_upgrade, [1, OldShareRecord]
    ),
    ?assertEqual(2, NewVersion),
    ?assertEqual(NewRecord, new_share_record()).


provider_upgrade_test(Config) ->
    OldProviderRecord = old_provider_record(),
    {NewVersion, NewRecord} = oz_test_utils:call_oz(
        Config, od_provider, record_upgrade, [1, OldProviderRecord]
    ),
    ?assertEqual(2, NewVersion),
    ?assertEqual(NewRecord, new_provider_record()).


handle_service_upgrade_test(Config) ->
    OldHandleServiceRecord = old_handle_service_record(),
    {NewVersion, NewRecord} = oz_test_utils:call_oz(
        Config, od_handle_service, record_upgrade, [1, OldHandleServiceRecord]
    ),
    ?assertEqual(2, NewVersion),
    ?assertEqual(NewRecord, new_handle_service_record()).


handle_upgrade_test(Config) ->
    OldHandleRecord = old_handle_record(),
    {NewVersion, NewRecord} = oz_test_utils:call_oz(
        Config, od_handle, record_upgrade, [1, OldHandleRecord]
    ),
    ?assertEqual(2, NewVersion),
    ?assertEqual(NewRecord, new_handle_record()).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    [{?LOAD_MODULES, [oz_test_utils]} | Config].


%%%===================================================================
%%% Record definitions
%%%===================================================================

user_record_1() -> {od_user,
    <<"name">>,
    <<"login">>,
    true,   % basic_auth_enabled
    <<"alias">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    [
        {oauth_account,
            google,  % provider_id
            <<"user_id1">>,
            <<"login1">>,
            <<"name1">>,
            [<<"email1@email.com">>]
        },
        {oauth_account,
            github,  % provider_id
            <<"user_id2">>,
            <<"login2">>,
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
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST, ?OZ_SPACES_ADD_MEMBERS
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
}.

user_record_2() -> {od_user,
    <<"name">>,
    <<"login">>,
    <<"alias">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    true,
    [
        {oauth_account,
            google,
            <<"user_id1">>,
            <<"login1">>,
            <<"name1">>,
            [<<"email1@email.com">>]
        },
        {oauth_account,
            github,
            <<"user_id2">>,
            <<"login2">>,
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
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES,
        ?OZ_USERS_LIST, ?OZ_SPACES_ADD_MEMBERS
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
}.

user_record_3() -> #od_user{
    name = <<"name">>,
    login = <<"login">>,
    alias = <<"alias">>,
    email_list = [<<"email1@email.com">>, <<"email2@email.com">>],
    basic_auth_enabled = true,
    linked_accounts = [
        #linked_account{
            provider_id = google,
            user_id = <<"user_id1">>,
            login = <<"login1">>,
            name = <<"name1">>,
            email_list = [<<"email1@email.com">>]
        },
        #linked_account{
            provider_id = github,
            user_id = <<"user_id2">>,
            login = <<"login2">>,
            name = <<"name2">>,
            email_list = [<<"email2@email.com">>]
        }
    ],
    default_space = <<"default_space">>,
    default_provider = <<"default_provider">>,
    chosen_provider = <<"chosen_provider">>,
    client_tokens = [<<"token1">>, <<"token2">>],
    space_aliases = #{
        <<"sp1">> => <<"sp1Name">>,
        <<"sp2">> => <<"sp2Name">>
    },
    oz_privileges = [
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES,
        ?OZ_USERS_LIST, ?OZ_SPACES_ADD_MEMBERS
    ],
    eff_oz_privileges = [],
    groups = [<<"group1">>, <<"group2">>, <<"group3">>],
    spaces = [<<"space1">>, <<"space2">>, <<"space3">>],
    handle_services = [<<"hservice1">>, <<"hservice2">>, <<"hservice3">>],
    handles = [<<"handle1">>, <<"handle2">>, <<"handle3">>],
    eff_groups = #{},
    eff_spaces = #{},
    eff_providers = #{},
    eff_handle_services = #{},
    eff_handles = #{},
    top_down_dirty = true
}.

old_group_record() -> {od_group,
    <<"name">>,
    role,
    [ % oz_privileges
        ?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST, ?OZ_SPACES_ADD_MEMBERS
    ],
    [], % eff_oz_privileges
    [<<"parent1">>, <<"parent2">>],
    [
        {<<"child1">>, [?GROUP_VIEW, ?GROUP_CREATE_SPACE, ?GROUP_INVITE_GROUP]},
        {<<"child2">>, [?GROUP_UPDATE, ?GROUP_DELETE, ?GROUP_JOIN_GROUP]}
    ],
    [], % eff_parents
    [], % eff_children
    [
        {<<"user1">>, [?GROUP_JOIN_GROUP, ?GROUP_CREATE_SPACE, ?GROUP_SET_PRIVILEGES]},
        {<<"user2">>, [?GROUP_UPDATE, ?GROUP_VIEW, ?GROUP_REMOVE_GROUP]}
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
}.

new_group_record() -> #od_group{
    name = <<"name">>,
    type = role,
    oz_privileges = [?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST, ?OZ_SPACES_ADD_MEMBERS],
    eff_oz_privileges = [],

    parents = [<<"parent1">>, <<"parent2">>],
    children = #{
        <<"child1">> => [?GROUP_VIEW, ?GROUP_CREATE_SPACE, ?GROUP_INVITE_GROUP],
        <<"child2">> => [?GROUP_UPDATE, ?GROUP_DELETE, ?GROUP_JOIN_GROUP]
    },
    eff_parents = #{},
    eff_children = #{},

    users = #{
        <<"user1">> => [?GROUP_JOIN_GROUP, ?GROUP_CREATE_SPACE, ?GROUP_SET_PRIVILEGES],
        <<"user2">> => [?GROUP_UPDATE, ?GROUP_VIEW, ?GROUP_REMOVE_GROUP]
    },
    spaces = [<<"space1">>, <<"space2">>, <<"space3">>],
    handle_services = [<<"handle_service1">>],
    handles = [<<"handle1">>, <<"handle2">>],

    eff_users = #{},
    eff_spaces = #{},
    eff_providers = #{},
    eff_handle_services = #{},
    eff_handles = #{},

    top_down_dirty = true,
    bottom_up_dirty = true
}.

old_space_record() -> {od_space,
    <<"name">>,
    [
        {<<"prov1">>, 1000},
        {<<"prov2">>, 250000},
        {<<"prov3">>, 19999999}
    ],
    [
        {<<"user1">>, [?SPACE_MANAGE_SHARES, ?SPACE_VIEW, ?SPACE_REMOVE_GROUP]},
        {<<"user2">>, [?SPACE_UPDATE, ?SPACE_SET_PRIVILEGES, ?SPACE_INVITE_PROVIDER]}
    ],
    [
        {<<"group1">>, [?SPACE_MANAGE_SHARES, ?SPACE_SET_PRIVILEGES, ?SPACE_INVITE_PROVIDER]},
        {<<"group2">>, [?SPACE_REMOVE_PROVIDER, ?SPACE_REMOVE_GROUP, ?SPACE_UPDATE]}
    ],
    [<<"share1">>, <<"share2">>, <<"share3">>, <<"share4">>],
    [],  % eff_users
    [],  % eff_groups
    false, % top_down_dirty
    false  % bottom_up_dirty
}.

new_space_record() -> #od_space{
    name = <<"name">>,
    users = #{
        <<"user1">> => [?SPACE_MANAGE_SHARES, ?SPACE_VIEW, ?SPACE_REMOVE_GROUP],
        <<"user2">> => [?SPACE_UPDATE, ?SPACE_SET_PRIVILEGES, ?SPACE_INVITE_PROVIDER]
    },
    groups = #{
        <<"group1">> => [?SPACE_MANAGE_SHARES, ?SPACE_SET_PRIVILEGES, ?SPACE_INVITE_PROVIDER],
        <<"group2">> => [?SPACE_REMOVE_PROVIDER, ?SPACE_REMOVE_GROUP, ?SPACE_UPDATE]
    },
    providers = #{
        <<"prov1">> => 1000,
        <<"prov2">> => 250000,
        <<"prov3">> => 19999999
    },
    shares = [<<"share1">>, <<"share2">>, <<"share3">>, <<"share4">>],

    eff_users = #{},
    eff_groups = #{},
    eff_providers = #{},

    top_down_dirty = true,
    bottom_up_dirty = true
}.

old_share_record() -> {od_share,
    <<"name">>,
    <<"public_url">>,
    <<"parent_space_id">>,
    <<"handle_id">>,
    <<"root_file_id">>,
    [],  % eff_users
    [],  % eff_groups
    false  % bottom_up_dirty
}.

new_share_record() -> #od_share{
    name = <<"name">>,
    public_url = <<"public_url">>,
    space = <<"parent_space_id">>,
    handle = <<"handle_id">>,
    root_file = <<"root_file_id">>
}.

old_provider_record() -> {od_provider,
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
}.

new_provider_record() -> #od_provider{
    name = <<"name">>,
    redirection_point = <<"redirection_point">>,
    urls = [<<"url1.com">>, <<"url2.com">>, <<"url3.com">>],
    serial = <<"cert_serial">>,
    latitude = -93.2341,
    longitude = 17,

    spaces = #{
        % During provider doc translation, extra information is added - support
        % sizes. However, it is not possible to gather this information because
        % during update there is no information about document id in context.
        <<"space1">> => 0,
        <<"space2">> => 0,
        <<"space3">> => 0,
        <<"space4">> => 0
    },

    eff_users = #{},
    eff_groups = #{},

    bottom_up_dirty = true
}.

old_handle_service_record() -> {od_handle_service,
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
}.

new_handle_service_record() -> #od_handle_service{
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

    bottom_up_dirty = true
}.


old_handle_record() -> {od_handle,
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
}.

new_handle_record() -> #od_handle{
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

    bottom_up_dirty = true
}.