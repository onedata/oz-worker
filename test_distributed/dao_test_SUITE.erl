%%%-------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This file contains tests for basic create-read-update-delete
%%% operations on: users, groups, spaces and providers
%%% @end
%%%-------------------------------------------------------------------
-module(dao_test_SUITE).
-author("Tomasz Lichon").

-include("registered_names.hrl").
-include("dao/dao_types.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("annotations/include/annotations.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([users_crud_test/1, groups_crud_test/1, spaces_crud_test/1, providers_crud_test/1, tokens_crud_test/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() ->
    [users_crud_test, groups_crud_test, spaces_crud_test, providers_crud_test, tokens_crud_test].

-define(REPEATS, 100).

-performance([
    {repeats, ?REPEATS},
    {config, [{name, users_crud}]}
]).
users_crud_test(Config) ->
    [Node] = ?config(gr_nodes, Config),

    % Data
    User = #user{name = <<"name">>, spaces = [<<"uuid1">>, <<"uuid2">>], groups = [<<"uuid3">>, <<"uuid4">>]},
    UpdatedUser = User#user{name = <<"name2">>},

    % Create
    {AnsC1, UserId} = rpc:call(Node, dao_lib, apply, [dao_users, save_user, [User], 1]),
    ?assertEqual(ok, AnsC1),
    AnsC2 = rpc:call(Node, dao_lib, apply, [dao_users, exist_user, [UserId], 1]),
    ?assertEqual({ok, true}, AnsC2),

    % Read
    AnsR1 = rpc:call(Node, dao_lib, apply, [dao_users, get_user, [UserId], 1]),
    ?assertMatch({ok, #db_document{uuid = UserId, record = User}}, AnsR1),

    % Update
    {AnsU1, UpdatedUserId} = rpc:call(Node, dao_lib, apply, [dao_users, save_user, [#db_document{record = UpdatedUser, uuid = UserId, force_update = true}], 1]),
    ?assertEqual(ok, AnsU1),
    ?assertEqual(UserId, UpdatedUserId),
    AnsU2 = rpc:call(Node, dao_lib, apply, [dao_users, get_user, [UserId], 1]),
    ?assertMatch({ok, #db_document{uuid = UserId, record = UpdatedUser}}, AnsU2),

    % Delete
    AnsD1 = rpc:call(Node, dao_lib, apply, [dao_users, remove_user, [UserId], 1]),
    ?assertEqual(ok, AnsD1),
    AnsD2 = rpc:call(Node, dao_lib, apply, [dao_users, exist_user, [UserId], 1]),
    ?assertEqual({ok, false}, AnsD2),
    AnsD3 = rpc:call(Node, dao_lib, apply, [dao_users, get_user, [UserId], 1]),
    ?assertEqual({error, {not_found, deleted}}, AnsD3).

-performance([
    {repeats, ?REPEATS},
    {config, [{name, groups_crud}]}
]).
groups_crud_test(Config) ->
    [Node] = ?config(gr_nodes, Config),

    % Data
    Group = #user_group{name = <<"name">>, spaces = [<<"uuid1">>, <<"uuid2">>], users = [{<<"uuid3">>, []}, {<<"uuid4">>, []}]},
    UpdatedGroup = Group#user_group{name = <<"name2">>},

    % Create
    {AnsC1, GroupId} = rpc:call(Node, dao_lib, apply, [dao_groups, save_group, [Group], 1]),
    ?assertEqual(ok, AnsC1),
    AnsC2 = rpc:call(Node, dao_lib, apply, [dao_groups, exist_group, [GroupId], 1]),
    ?assertEqual({ok, true}, AnsC2),

    % Read
    AnsR1 = rpc:call(Node, dao_lib, apply, [dao_groups, get_group, [GroupId], 1]),
    ?assertMatch({ok, #db_document{uuid = GroupId, record = Group}}, AnsR1),

    % Update
    {AnsU1, UpdatedGroupId} = rpc:call(Node, dao_lib, apply, [dao_groups, save_group, [#db_document{record = UpdatedGroup, uuid = GroupId, force_update = true}], 1]),
    ?assertEqual(ok, AnsU1),
    ?assertEqual(GroupId, UpdatedGroupId),
    AnsU2 = rpc:call(Node, dao_lib, apply, [dao_groups, get_group, [GroupId], 1]),
    ?assertMatch({ok, #db_document{uuid = GroupId, record = UpdatedGroup}}, AnsU2),

    % Delete
    AnsD1 = rpc:call(Node, dao_lib, apply, [dao_groups, remove_group, [GroupId], 1]),
    ?assertEqual(ok, AnsD1),
    AnsD2 = rpc:call(Node, dao_lib, apply, [dao_groups, exist_group, [GroupId], 1]),
    ?assertEqual({ok, false}, AnsD2),
    AnsD3 = rpc:call(Node, dao_lib, apply, [dao_groups, get_group, [GroupId], 1]),
    ?assertEqual({error, {not_found, deleted}}, AnsD3).

-performance([
    {repeats, ?REPEATS},
    {config, [{name, providers_crud}]}
]).
providers_crud_test(Config) ->
    [Node] = ?config(gr_nodes, Config),

    % Data
    Provider = #provider{redirection_point = <<"http://redirpoi.nt">>, urls = [<<"1.1.1.1">>], spaces = [<<"uuid1">>, <<"uuid2">>]},
    UpdatedProvider = Provider#provider{urls = [<<"2.2.2.2">>]},

    % Create
    {AnsC1, ProviderId} = rpc:call(Node, dao_lib, apply, [dao_providers, save_provider, [Provider], 1]),
    ?assertEqual(ok, AnsC1),
    AnsC2 = rpc:call(Node, dao_lib, apply, [dao_providers, exist_provider, [ProviderId], 1]),
    ?assertEqual({ok, true}, AnsC2),

    % Read
    AnsR1 = rpc:call(Node, dao_lib, apply, [dao_providers, get_provider, [ProviderId], 1]),
    ?assertMatch({ok, #db_document{uuid = ProviderId, record = Provider}}, AnsR1),

    % Update
    {AnsU1, UpdatedProviderId} = rpc:call(Node, dao_lib, apply, [dao_providers, save_provider, [#db_document{record = UpdatedProvider, uuid = ProviderId, force_update = true}], 1]),
    ?assertEqual(ok, AnsU1),
    ?assertEqual(ProviderId, UpdatedProviderId),
    AnsU2 = rpc:call(Node, dao_lib, apply, [dao_providers, get_provider, [ProviderId], 1]),
    ?assertMatch({ok, #db_document{uuid = ProviderId, record = UpdatedProvider}}, AnsU2),

    % Delete
    AnsD1 = rpc:call(Node, dao_lib, apply, [dao_providers, remove_provider, [ProviderId], 1]),
    ?assertEqual(ok, AnsD1),
    AnsD2 = rpc:call(Node, dao_lib, apply, [dao_providers, exist_provider, [ProviderId], 1]),
    ?assertEqual({ok, false}, AnsD2),
    AnsD3 = rpc:call(Node, dao_lib, apply, [dao_providers, get_provider, [ProviderId], 1]),
    ?assertEqual({error, {not_found, deleted}}, AnsD3).

-performance([
    {repeats, ?REPEATS},
    {config, [{name, spaces_crud}]}
]).
spaces_crud_test(Config) ->
    [Node] = ?config(gr_nodes, Config),

    % Data
    Space = #space{name = <<"name">>, users = [{<<"uuid1">>, [space_invite_user]}], groups = [{<<"uuid4">>, []}], providers = [<<"uuid5">>]},
    UpdatedSpace = Space#space{name = <<"name2">>},

    % Create
    {AnsC1, SpaceId} = rpc:call(Node, dao_lib, apply, [dao_spaces, save_space, [Space], 1]),
    ?assertEqual(ok, AnsC1),
    AnsC2 = rpc:call(Node, dao_lib, apply, [dao_spaces, exist_space, [SpaceId], 1]),
    ?assertEqual({ok, true}, AnsC2),

    % Read
    AnsR1 = rpc:call(Node, dao_lib, apply, [dao_spaces, get_space, [SpaceId], 1]),
    ?assertMatch({ok, #db_document{uuid = SpaceId, record = Space}}, AnsR1),

    % Update
    {AnsU1, UpdatedSpaceId} = rpc:call(Node, dao_lib, apply, [dao_spaces, save_space, [#db_document{record = UpdatedSpace, uuid = SpaceId, force_update = true}], 1]),
    ?assertEqual(ok, AnsU1),
    ?assertEqual(SpaceId, UpdatedSpaceId),
    AnsU2 = rpc:call(Node, dao_lib, apply, [dao_spaces, get_space, [SpaceId], 1]),
    ?assertMatch({ok, #db_document{uuid = SpaceId, record = UpdatedSpace}}, AnsU2),

    % Delete
    AnsD1 = rpc:call(Node, dao_lib, apply, [dao_spaces, remove_space, [SpaceId], 1]),
    ?assertEqual(ok, AnsD1),
    AnsD2 = rpc:call(Node, dao_lib, apply, [dao_spaces, exist_space, [SpaceId], 1]),
    ?assertEqual({ok, false}, AnsD2),
    AnsD3 = rpc:call(Node, dao_lib, apply, [dao_spaces, get_space, [SpaceId], 1]),
    ?assertEqual({error, {not_found, deleted}}, AnsD3).

-performance([
    {repeats, ?REPEATS},
    {config, [{name, tokens_crud}]}
]).
tokens_crud_test(Config) ->
    [Node] = ?config(gr_nodes, Config),

    % Data
    Token = #token{resource = space, secret = <<"secret">>, resource_id = <<"id">>},
    UpdatedToken = Token#token{resource = group},

    % Create
    {AnsC1, TokenId} = rpc:call(Node, dao_lib, apply, [dao_tokens, save_token, [Token], 1]),
    ?assertEqual(ok, AnsC1),
    AnsC2 = rpc:call(Node, dao_lib, apply, [dao_tokens, exist_token, [TokenId], 1]),
    ?assertEqual({ok, true}, AnsC2),

    % Read
    AnsR1 = rpc:call(Node, dao_lib, apply, [dao_tokens, get_token, [TokenId], 1]),
    ?assertMatch({ok, #db_document{uuid = TokenId, record = Token}}, AnsR1),

    % Update
    {AnsU1, UpdatedTokenId} = rpc:call(Node, dao_lib, apply, [dao_tokens, save_token, [#db_document{record = UpdatedToken, uuid = TokenId, force_update = true}], 1]),
    ?assertEqual(ok, AnsU1),
    ?assertEqual(TokenId, UpdatedTokenId),
    AnsU2 = rpc:call(Node, dao_lib, apply, [dao_tokens, get_token, [TokenId], 1]),
    ?assertMatch({ok, #db_document{uuid = TokenId, record = UpdatedToken}}, AnsU2),

    % Delete
    AnsD1 = rpc:call(Node, dao_lib, apply, [dao_tokens, remove_token, [TokenId], 1]),
    ?assertEqual(ok, AnsD1),
    AnsD2 = rpc:call(Node, dao_lib, apply, [dao_tokens, exist_token, [TokenId], 1]),
    ?assertEqual({ok, false}, AnsD2),
    AnsD3 = rpc:call(Node, dao_lib, apply, [dao_tokens, get_token, [TokenId], 1]),
    ?assertEqual({error, {not_found, deleted}}, AnsD3).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    timer:sleep(60000), % TODO add nagios to GR and delete sleep
    NewConfig.

end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).