%% ===================================================================
%% @author Tomasz Lichon
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains tests for basic create-read-update-delete
%% operations on: users, groups, spaces and providers
%% @end
%% ===================================================================
-author("Tomasz Lichon").

-module(dao_test_SUITE).
-author("Tomasz Lichon").

%% Includes
-include("registered_names.hrl").
-include("dao/dao_types.hrl").
-include("testing/test_utils.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("ctool/include/test/test_node_starter.hrl").
-include_lib("ctool/include/test/assertions.hrl").

%% API
-export([all/0,init_per_suite/1,end_per_suite/1]).
-export([users_crud_test/1,groups_crud_test/1,spaces_crud_test/1,providers_crud_test/1,tokens_crud_test/1]).

all() -> [users_crud_test,groups_crud_test,spaces_crud_test,providers_crud_test,tokens_crud_test].

users_crud_test(Config) ->
	[Node] = ?config(nodes,Config),

	% Data
	User = #user{name = "name",spaces = ["uuid1","uuid2"], groups = ["uuid3","uuid4"] },
	UpdatedUser = User#user{name="name2"},

	% Create
	{AnsC1,UserId} = rpc:call(Node,dao_lib,apply,[dao_users,save_user,[User],1]),
	?assertEqual(ok,AnsC1),
	AnsC2 = rpc:call(Node,dao_lib,apply,[dao_users,exist_user,[UserId],1]),
	?assertEqual({ok,true}, AnsC2),

	% Read
	AnsR1 = rpc:call(Node,dao_lib,apply,[dao_users,get_user,[UserId],1]),
	?assertMatch({ok,#veil_document{uuid=UserId,record=User}}, AnsR1),

	% Update
	{AnsU1,UpdatedUserId} = rpc:call(Node,dao_lib,apply,[dao_users,save_user,[#veil_document{record = UpdatedUser,uuid = UserId,force_update = true}],1]),
	?assertEqual(ok,AnsU1),
	?assertEqual(UserId,UpdatedUserId),
	AnsU2 = rpc:call(Node,dao_lib,apply,[dao_users,get_user,[UserId],1]),
	?assertMatch( {ok,#veil_document{uuid=UserId,record=UpdatedUser}}, AnsU2),

	% Delete
	AnsD1 = rpc:call(Node,dao_lib,apply,[dao_users,remove_user,[UserId],1]),
	?assertEqual(ok,AnsD1),
	AnsD2 = rpc:call(Node,dao_lib,apply,[dao_users,exist_user,[UserId],1]),
	?assertEqual({ok,false},AnsD2),
	AnsD3 = rpc:call(Node,dao_lib,apply,[dao_users,get_user,[UserId],1]),
	?assertEqual({error,{not_found,deleted}},AnsD3).

groups_crud_test(Config) ->
    [Node] = ?config(nodes,Config),

	% Data
	Group = #user_group{name = "name",spaces = ["uuid1","uuid2"], users = ["uuid3","uuid4"] },
	UpdatedGroup = Group#user_group{name="name2"},

	% Create
	{AnsC1,GroupId} = rpc:call(Node,dao_lib,apply,[dao_groups,save_group,[Group],1]),
	?assertEqual(ok,AnsC1),
	AnsC2 = rpc:call(Node,dao_lib,apply,[dao_groups,exist_group,[GroupId],1]),
	?assertEqual({ok,true}, AnsC2),

	% Read
	AnsR1 = rpc:call(Node,dao_lib,apply,[dao_groups,get_group,[GroupId],1]),
	?assertMatch({ok,#veil_document{uuid=GroupId,record=Group}}, AnsR1),

	% Update
	{AnsU1,UpdatedGroupId} = rpc:call(Node,dao_lib,apply,[dao_groups,save_group,[#veil_document{record = UpdatedGroup,uuid = GroupId,force_update = true}],1]),
	?assertEqual(ok,AnsU1),
	?assertEqual(GroupId,UpdatedGroupId),
	AnsU2 = rpc:call(Node,dao_lib,apply,[dao_groups,get_group,[GroupId],1]),
	?assertMatch( {ok,#veil_document{uuid=GroupId,record=UpdatedGroup}}, AnsU2),

	% Delete
	AnsD1 = rpc:call(Node,dao_lib,apply,[dao_groups,remove_group,[GroupId],1]),
	?assertEqual(ok,AnsD1),
	AnsD2 = rpc:call(Node,dao_lib,apply,[dao_groups,exist_group,[GroupId],1]),
	?assertEqual({ok,false},AnsD2),
	AnsD3 = rpc:call(Node,dao_lib,apply,[dao_groups,get_group,[GroupId],1]),
	?assertEqual({error,{not_found,deleted}},AnsD3).

providers_crud_test(Config) ->
    [Node] = ?config(nodes,Config),

	% Data
	Provider = #provider{url = <<"1.1.1.1">>,spaces = [<<"uuid1">>,<<"uuid2">>]},
	UpdatedProvider = Provider#provider{url = <<"2.2.2.2">>},

	% Create
	{AnsC1,ProviderId} = rpc:call(Node,dao_lib,apply,[dao_providers,save_provider,[Provider],1]),
	?assertEqual(ok,AnsC1),
	AnsC2 = rpc:call(Node,dao_lib,apply,[dao_providers,exist_provider,[ProviderId],1]),
	?assertEqual({ok,true}, AnsC2),

	% Read
	AnsR1 = rpc:call(Node,dao_lib,apply,[dao_providers,get_provider,[ProviderId],1]),
	?assertMatch({ok,#veil_document{uuid=ProviderId,record=Provider}}, AnsR1),

	% Update
	{AnsU1,UpdatedProviderId} = rpc:call(Node,dao_lib,apply,[dao_providers,save_provider,[#veil_document{record = UpdatedProvider,uuid = ProviderId,force_update = true}],1]),
	?assertEqual(ok,AnsU1),
	?assertEqual(ProviderId,UpdatedProviderId),
	AnsU2 = rpc:call(Node,dao_lib,apply,[dao_providers,get_provider,[ProviderId],1]),
	?assertMatch( {ok,#veil_document{uuid=ProviderId,record=UpdatedProvider}}, AnsU2),

	% Delete
	AnsD1 = rpc:call(Node,dao_lib,apply,[dao_providers,remove_provider,[ProviderId],1]),
	?assertEqual(ok,AnsD1),
	AnsD2 = rpc:call(Node,dao_lib,apply,[dao_providers,exist_provider,[ProviderId],1]),
	?assertEqual({ok,false},AnsD2),
	AnsD3 = rpc:call(Node,dao_lib,apply,[dao_providers,get_provider,[ProviderId],1]),
	?assertEqual({error,{not_found,deleted}},AnsD3).

spaces_crud_test(Config) ->
    [Node] = ?config(nodes,Config),

	% Data
	Space = #space{name = <<"name">>,users = [{<<"uuid1">>,[space_invite_user]}], groups = [{<<"uuid4">>,[]}], providers = [<<"uuid5">>] },
	UpdatedSpace = Space#space{name = <<"name2">>},

	% Create
	{AnsC1,SpaceId} = rpc:call(Node,dao_lib,apply,[dao_spaces,save_space,[Space],1]),
	?assertEqual(ok,AnsC1),
	AnsC2 = rpc:call(Node,dao_lib,apply,[dao_spaces,exist_space,[SpaceId],1]),
	?assertEqual({ok,true}, AnsC2),

	% Read
	AnsR1 = rpc:call(Node,dao_lib,apply,[dao_spaces,get_space,[SpaceId],1]),
	?assertMatch({ok,#veil_document{uuid=SpaceId,record=Space}}, AnsR1),

	% Update
	{AnsU1,UpdatedSpaceId} = rpc:call(Node,dao_lib,apply,[dao_spaces,save_space,[#veil_document{record = UpdatedSpace,uuid = SpaceId,force_update = true}],1]),
	?assertEqual(ok,AnsU1),
	?assertEqual(SpaceId,UpdatedSpaceId),
	AnsU2 = rpc:call(Node,dao_lib,apply,[dao_spaces,get_space,[SpaceId],1]),
	?assertMatch( {ok,#veil_document{uuid=SpaceId,record=UpdatedSpace}}, AnsU2),

	% Delete
	AnsD1 = rpc:call(Node,dao_lib,apply,[dao_spaces,remove_space,[SpaceId],1]),
	?assertEqual(ok,AnsD1),
	AnsD2 = rpc:call(Node,dao_lib,apply,[dao_spaces,exist_space,[SpaceId],1]),
	?assertEqual({ok,false},AnsD2),
	AnsD3 = rpc:call(Node,dao_lib,apply,[dao_spaces,get_space,[SpaceId],1]),
	?assertEqual({error,{not_found,deleted}},AnsD3).

tokens_crud_test(Config) ->
    [Node] = ?config(nodes,Config),

	% Data
	Token = #token{type = some_type1,expires = time_in_some_format },
	UpdatedToken = Token#token{type=some_type2},

	% Create
	{AnsC1,TokenId} = rpc:call(Node,dao_lib,apply,[dao_tokens,save_token,[Token],1]),
	?assertEqual(ok,AnsC1),
	AnsC2 = rpc:call(Node,dao_lib,apply,[dao_tokens,exist_token,[TokenId],1]),
	?assertEqual({ok,true}, AnsC2),

	% Read
	AnsR1 = rpc:call(Node,dao_lib,apply,[dao_tokens,get_token,[TokenId],1]),
	?assertMatch({ok,#veil_document{uuid=TokenId,record=Token}}, AnsR1),

	% Update
	{AnsU1,UpdatedTokenId} = rpc:call(Node,dao_lib,apply,[dao_tokens,save_token,[#veil_document{record = UpdatedToken,uuid = TokenId,force_update = true}],1]),
	?assertEqual(ok,AnsU1),
	?assertEqual(TokenId,UpdatedTokenId),
	AnsU2 = rpc:call(Node,dao_lib,apply,[dao_tokens,get_token,[TokenId],1]),
	?assertMatch( {ok,#veil_document{uuid=TokenId,record=UpdatedToken}}, AnsU2),

	% Delete
	AnsD1 = rpc:call(Node,dao_lib,apply,[dao_tokens,remove_token,[TokenId],1]),
	?assertEqual(ok,AnsD1),
	AnsD2 = rpc:call(Node,dao_lib,apply,[dao_tokens,exist_token,[TokenId],1]),
	?assertEqual({ok,false},AnsD2),
	AnsD3 = rpc:call(Node,dao_lib,apply,[dao_tokens,get_token,[TokenId],1]),
	?assertEqual({error,{not_found,deleted}},AnsD3).



%% ====================================================================
%% SetUp and TearDown functions
%% ====================================================================

init_per_suite(Config) ->
	?INIT_CODE_PATH,
	DbNodesEnv = {db_nodes,[?DB_NODE]},
	Nodes = test_node_starter:start_test_nodes(1),
    test_node_starter:start_app_on_nodes(?APP_Name,?GR_DEPS,Nodes,[[DbNodesEnv]]),
	Config ++ [{nodes,Nodes}].

end_per_suite(Config) ->
	Nodes = ?config(nodes,Config),
	test_node_starter:stop_app_on_nodes(?APP_Name,?GR_DEPS,Nodes),
	test_node_starter:stop_test_nodes(Nodes).