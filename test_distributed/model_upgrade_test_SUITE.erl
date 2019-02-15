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
    dns_state_upgrade_test/1,
    token_upgrade_test/1
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
    dns_state_upgrade_test,
    token_upgrade_test
]).

%%%===================================================================
%%% Tests
%%%===================================================================

user_upgrade_test(Config) ->
    test_record_upgrade(Config, od_user, [1, 2, 3, 4, 5, 6, 7, 8, 9]).


group_upgrade_test(Config) ->
    test_record_upgrade(Config, od_group, [1, 2, 3, 4, 5]).


space_upgrade_test(Config) ->
    test_record_upgrade(Config, od_space, [1, 2, 3]).


share_upgrade_test(Config) ->
    test_record_upgrade(Config, od_share, [1, 2]).


provider_upgrade_test(Config) ->
    test_record_upgrade(Config, od_provider, [1, 2, 3, 4]).


handle_service_upgrade_test(Config) ->
    test_record_upgrade(Config, od_handle_service, [1, 2, 3]).


handle_upgrade_test(Config) ->
    test_record_upgrade(Config, od_handle, [1, 2, 3]).


dns_state_upgrade_test(Config) ->
    test_record_upgrade(Config, dns_state, [1, 2]).


token_upgrade_test(Config) ->
    test_record_upgrade(Config, token, [1, 2]).


test_record_upgrade(Config, Type, Versions) ->
    lists:foldl(fun(Version, RecordInOlderVersion) ->
        {NewVersion, NewRecord} = oz_test_utils:call_oz(
            Config, Type, upgrade_record, [Version - 1, RecordInOlderVersion]
        ),
        ?assertEqual({NewVersion, NewRecord}, {Version, get_record(Type, Version)}),
        NewRecord
    end, get_record(Type, hd(Versions)), tl(Versions)).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

end_per_suite(_Config) ->
    ok.

%%%===================================================================
%%% Record definitions
%%%===================================================================

get_record(od_user, 1) -> {od_user,
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
};
get_record(od_user, 2) -> {od_user,
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
};
get_record(od_user, 3) -> {od_user,
    <<"name">>,
    <<"login">>,
    <<"alias">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    true,
    [
        {linked_account,
            google,
            <<"user_id1">>,
            <<"login1">>,
            <<"name1">>,
            [<<"email1@email.com">>]
        },
        {linked_account,
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
};
get_record(od_user, 4) -> {od_user,
    <<"name">>,
    <<"login">>,
    <<"alias">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    true,
    [
        {linked_account,
            google,
            <<"user_id1">>,
            <<"login1">>,
            <<"name1">>,
            [<<"email1@email.com">>],
            []
        },
        {linked_account,
            github,
            <<"user_id2">>,
            <<"login2">>,
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
};
get_record(od_user, 5) -> {od_user,
    <<"name">>,
    <<"login">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    true,
    [
        {linked_account,
            google,
            <<"user_id1">>,
            <<"login1">>,
            <<"name1">>,
            [<<"email1@email.com">>],
            []
        },
        {linked_account,
            github,
            <<"user_id2">>,
            <<"login2">>,
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
};
get_record(od_user, 6) -> {od_user,
    <<"name">>,
    <<"login">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    true,
    [
        {linked_account,
            google,
            <<"user_id1">>,
            <<"login1">>,
            <<"name1">>,
            [<<"email1@email.com">>],
            []
        },
        {linked_account,
            github,
            <<"user_id2">>,
            <<"login2">>,
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
};
get_record(od_user, 7) -> {od_user,
    <<"name">>,
    <<"login">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    true,
    [
        {linked_account,
            google,
            <<"user_id1">>,
            <<"login1">>,
            <<"name1">>,
            [<<"email1@email.com">>],
            []
        },
        {linked_account,
            github,
            <<"user_id2">>,
            <<"login2">>,
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
};
get_record(od_user, 8) -> {od_user,
    <<"name">>,
    <<"login">>,
    [<<"email1@email.com">>, <<"email2@email.com">>],
    true,
    [
        {linked_account,
            google,
            <<"user_id1">>,
            <<"name1">>,
            <<"login1">>,
            [<<"email1@email.com">>],
            [],
            #{}
        },
        {linked_account,
            github,
            <<"user_id2">>,
            <<"name2">>,
            <<"login2">>,
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
};
get_record(od_user, 9) -> #od_user{
    name = <<"name">>,
    alias = <<"login">>,
    emails = [<<"email1@email.com">>, <<"email2@email.com">>],
    basic_auth_enabled = true,
    linked_accounts = [
        #linked_account{
            idp = google,
            subject_id = <<"user_id1">>,
            name = <<"name1">>,
            alias = <<"login1">>,
            emails = [<<"email1@email.com">>],
            entitlements = [],
            custom = #{},
            access_token = {undefined, 0},
            refresh_token = undefined
        },
        #linked_account{
            idp = github,
            subject_id = <<"user_id2">>,
            name = <<"name2">>,
            alias = <<"login2">>,
            emails = [<<"email2@email.com">>],
            entitlements = [],
            custom = #{},
            access_token = {undefined, 0},
            refresh_token = undefined
        }
    ],
    entitlements = [],

    default_space = <<"default_space">>,
    default_provider = <<"default_provider">>,

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
};


get_record(od_group, 1) -> {od_group,
    <<"ńąµę|"/utf8>>,
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
};
get_record(od_group, 2) -> {od_group,
    <<"ńąµę|"/utf8>>,
    role,
    [?OZ_VIEW_PRIVILEGES, ?OZ_SET_PRIVILEGES, ?OZ_USERS_LIST, ?OZ_SPACES_ADD_MEMBERS],
    [],

    [<<"parent1">>, <<"parent2">>],
    #{
        <<"child1">> => [?GROUP_VIEW, ?GROUP_CREATE_SPACE, ?GROUP_INVITE_GROUP],
        <<"child2">> => [?GROUP_UPDATE, ?GROUP_DELETE, ?GROUP_JOIN_GROUP]
    },
    #{}, % eff_parents
    #{}, % eff_children

    #{
        <<"user1">> => [?GROUP_JOIN_GROUP, ?GROUP_CREATE_SPACE, ?GROUP_SET_PRIVILEGES],
        <<"user2">> => [?GROUP_UPDATE, ?GROUP_VIEW, ?GROUP_REMOVE_GROUP]
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
get_record(od_group, 3) -> #od_group{
    name = <<"ńąµę"/utf8>>,
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
};
get_record(od_group, 4) ->
    get_record(od_group, 3);
get_record(od_group, 5) ->
    V4 = get_record(od_group, 4),
    V4#od_group{type = role_holders};


get_record(od_space, 1) -> {od_space,
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
};
get_record(od_space, 2) -> {od_space,
    <<"name">>,
    #{
        <<"user1">> => [?SPACE_MANAGE_SHARES, ?SPACE_VIEW, ?SPACE_REMOVE_GROUP],
        <<"user2">> => [?SPACE_UPDATE, ?SPACE_SET_PRIVILEGES, ?SPACE_INVITE_PROVIDER]
    },
    #{
        <<"group1">> => [?SPACE_MANAGE_SHARES, ?SPACE_SET_PRIVILEGES, ?SPACE_INVITE_PROVIDER],
        <<"group2">> => [?SPACE_REMOVE_PROVIDER, ?SPACE_REMOVE_GROUP, ?SPACE_UPDATE]
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
get_record(od_space, 3) -> #od_space{
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
get_record(od_share, 2) -> #od_share{
    name = <<"name">>,
    public_url = <<"public_url">>,
    space = <<"parent_space_id">>,
    handle = <<"handle_id">>,
    root_file = <<"root_file_id">>
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
get_record(od_provider, 4) -> #od_provider{
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
get_record(od_handle_service, 3) -> #od_handle_service{
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
get_record(od_handle, 3) -> #od_handle{
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
    {client, user, <<"client_id">>}
};
get_record(token, 2) -> {token,
    <<"secret">>,
    resource,
    <<"resource_id">>,
    {client, user, <<"client_id">>},
    false
}.
