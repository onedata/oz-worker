%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Common functions for ct tests.
%% @end
%% ===================================================================

-module(gr_test_utils).

-include_lib("ctool/include/test/test_utils.hrl").
-include("dao/dao_users.hrl").

%% API
-export([make_dir/2, cleanup/0]).
-export([create_provider/4, create_space/3, create_user/2, create_group/3]).
-export([join_group/3, join_space/3, support_space/4]).

%% ====================================================================
%% API functions
%% ====================================================================


%% make_dir/2
%% ====================================================================
%% @doc Creates directory starting from given root.
%% @end
%% ====================================================================
-spec make_dir(Root :: string(), Dir :: string()) ->
    Path :: string().
%% ====================================================================
make_dir(Root, Dir) ->
    lists:foldl(fun(Leaf, Path) ->
        NewPath = filename:join(Path, Leaf),
        catch file:make_dir(NewPath),
        NewPath
    end, Root, filename:split(Dir)).


%% cleanup/0
%% ====================================================================
%% @doc Deletes databases and temporary files.
%% @end
%% ====================================================================
-spec cleanup() -> ok.
%% ====================================================================
cleanup() ->
    os:cmd("rm -rf " ++ ?TEMP_DIR),
    os:cmd("./delete_test_dbs.sh"),
    make_dir("/", "tmp/onedata"),
    make_dir(".", "resources"),
    file:write_file("resources/auth.config", <<"[].">>),
    ok.


%% create_provider/1
%% ====================================================================
%% @doc Creates provider in Global Registry.
%% @end
%% ====================================================================
-spec create_provider(Config :: term(), Name :: binary(), URLs :: [binary()], RedirectionPoint :: binary()) ->
    {ok, Id :: binary(), KeyFile :: string(), CertFile :: string()} | {error, Reason :: term()}.
%% ====================================================================
create_provider(Config, Name, URLs, RedirectionPoint) ->
    try
        {MegaSec, Sec, MiliSec} = erlang:now(),
        Prefix = lists:foldl(fun(Int, Acc) -> Acc ++ integer_to_list(Int) end, "provider", [MegaSec, Sec, MiliSec]),
        KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
        CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
        CertFile = filename:join(?TEMP_DIR, Prefix ++ "_cert.pem"),
        os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
        os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
        {ok, CSR} = file:read_file(CSRFile),
        [Node] = ?config(gr_nodes, Config),
        {ok, Id, Cert} = rpc:call(Node, provider_logic, create, [Name, URLs, RedirectionPoint, CSR]),
        file:write_file(CertFile, Cert),
        {ok, Id, KeyFile, CertFile}
    catch
        _:Reason ->
            {error, Reason}
    end.


%% create_space/3
%% ====================================================================
%% @doc Creates space in Global Registry.
%% @end
%% ====================================================================
-spec create_space(Config :: term(), Member :: {user | group, Id :: binary()}, Name :: binary()) ->
    {ok, Id :: binary()} | {error, Reason :: term()}.
%% ====================================================================
create_space(Config, Member, Name) ->
    try
        [Node] = ?config(gr_nodes, Config),
        rpc:call(Node, space_logic, create, [Member, Name])
    catch
        _:Reason ->
            {error, Reason}
    end.


%% create_user/2
%% ====================================================================
%% @doc Creates user in Global Registry.
%% @end
%% ====================================================================
-spec create_user(Config :: term(), User :: #user{}) ->
    {ok, Id :: binary()} | {error, Reason :: term()}.
%% ====================================================================
create_user(Config, User) ->
    try
        [Node] = ?config(gr_nodes, Config),
        rpc:call(Node, user_logic, create, [User])
    catch
        _:Reason ->
            {error, Reason}
    end.


%% create_group/3
%% ====================================================================
%% @doc Creates group in Global Registry.
%% @end
%% ====================================================================
-spec create_group(Config :: term(), UserId :: binary(), Name :: binary()) ->
    {ok, Id :: binary()} | {error, Reason :: term()}.
%% ====================================================================
create_group(Config, UserId, Name) ->
    try
        [Node] = ?config(gr_nodes, Config),
        rpc:call(Node, group_logic, create, [UserId, Name])
    catch
        _:Reason ->
            {error, Reason}
    end.


%% join_group/3
%% ====================================================================
%% @doc Adds user to group in Global Registry.
%% @end
%% ====================================================================
-spec join_group(Config :: term(), UserId :: binary(), GroupId :: binary()) ->
    ok | {error, Reason :: term()}.
%% ====================================================================
join_group(Config, UserId, GroupId) ->
    try
        [Node] = ?config(gr_nodes, Config),
        {ok, Token} = rpc:call(Node, token_logic, create, [group_invite_token, {group, GroupId}]),
        {ok, GroupId} = rpc:call(Node, group_logic, join, [UserId, Token]),
        ok
    catch
        _:Reason ->
            {error, Reason}
    end.


%% join_space/3
%% ====================================================================
%% @doc Creates group in Global Registry.
%% @end
%% ====================================================================
-spec join_space(Config :: term(), {user | group, Id :: binary()}, SpaceId :: binary()) ->
    ok | {error, Reason :: term()}.
%% ====================================================================
join_space(Config, {user, UserId}, SpaceId) ->
    try
        [Node] = ?config(gr_nodes, Config),
        {ok, Token} = rpc:call(Node, token_logic, create, [space_invite_user_token, {space, SpaceId}]),
        {ok, SpaceId} = rpc:call(Node, space_logic, join, [{user, UserId}, Token]),
        ok
    catch
        _:Reason ->
            {error, Reason}
    end;

join_space(Config, {group, GroupId}, SpaceId) ->
    try
        [Node] = ?config(gr_nodes, Config),
        {ok, Token} = rpc:call(Node, token_logic, create, [space_invite_group_token, {space, SpaceId}]),
        {ok, SpaceId} = rpc:call(Node, space_logic, join, [{group, GroupId}, Token]),
        ok
    catch
        _:Reason ->
            {error, Reason}
    end.


%% support_space/4
%% ====================================================================
%% @doc Supports space by provider.
%% @end
%% ====================================================================
-spec support_space(Config :: term(), ProviderId :: binary(), SpaceId :: binary(), Size :: non_neg_integer()) ->
    ok | {error, Reason :: term()}.
%% ====================================================================
support_space(Config, ProviderId, SpaceId, Size) ->
    try
        [Node] = ?config(gr_nodes, Config),
        {ok, Token} = rpc:call(Node, token_logic, create, [space_support_token, {space, SpaceId}]),
        {ok, SpaceId} = rpc:call(Node, space_logic, support, [ProviderId, Token, Size]),
        ok
    catch
        _:Reason ->
            {error, Reason}
    end.