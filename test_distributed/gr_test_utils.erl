%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Common functions for ct tests.
%%% @end
%%%-------------------------------------------------------------------

-module(gr_test_utils).

-include_lib("ctool/include/test/test_utils.hrl").
-include("datastore/gr_datastore_models_def.hrl").
-include("handlers/rest_handler.hrl").

%% API
-export([make_dir/2, cleanup/0]).
-export([create_provider/4, create_space/3, create_user/2, create_group/3]).
-export([join_group/3, join_space/3, support_space/4]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates directory starting from given root.
%% @end
%%--------------------------------------------------------------------
-spec make_dir(Root :: string(), Dir :: string()) ->
    Path :: string().
make_dir(Root, Dir) ->
    lists:foldl(fun(Leaf, Path) ->
        NewPath = filename:join(Path, Leaf),
            catch file:make_dir(NewPath),
        NewPath
    end, Root, filename:split(Dir)).

%%--------------------------------------------------------------------
%% @doc Deletes databases and temporary files.
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok.
cleanup() ->
    os:cmd("rm -rf " ++ ?TEMP_DIR),
    os:cmd("./delete_test_dbs.sh"),
    make_dir("/", "tmp/onedata"),
    make_dir(".", "resources"),
    file:write_file("resources/auth.config", <<"[].">>),
    ok.

%%--------------------------------------------------------------------
%% @doc Creates provider in Global Registry.
%% @end
%%--------------------------------------------------------------------
-spec create_provider(Node :: binary(), Name :: binary(), URLs :: [binary()], RedirectionPoint :: binary()) ->
    {ok, Id :: binary(), KeyFile :: string(), CertFile :: string()} | {error, Reason :: term()}.
create_provider(Node, Name, URLs, RedirectionPoint) ->
    try
        {MegaSec, Sec, MiliSec} = erlang:now(),
        Prefix = lists:foldl(fun(Int, Acc) ->
            Acc ++ integer_to_list(Int) end, "provider", [MegaSec, Sec, MiliSec]),
        KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
        CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
        CertFile = filename:join(?TEMP_DIR, Prefix ++ "_cert.pem"),
        os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
        os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
        {ok, CSR} = file:read_file(CSRFile),
        {ok, Id, Cert} = rpc:call(Node, provider_logic, create, [Name, URLs, RedirectionPoint, CSR]),
        file:write_file(CertFile, Cert),
        {ok, Id, KeyFile, CertFile}
    catch
        _:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Creates space in Global Registry.
%% @end
%%--------------------------------------------------------------------
-spec create_space(Node :: binary(), Member :: {user | group, Id :: binary()}, Name :: binary()) ->
    {ok, Id :: binary()} | {error, Reason :: term()}.
create_space(Node, Member, Name) ->
    try
        rpc:call(Node, space_logic, create, [Member, Name])
    catch
        _:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Creates user in Global Registry.
%% @end
%%--------------------------------------------------------------------
-spec create_user(Node :: binary(), User :: #onedata_user{}) ->
    {ok, Id :: binary()} | {error, Reason :: term()}.
create_user(Node, User) ->
    try
        rpc:call(Node, user_logic, create, [User])
    catch
        _:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Creates group in Global Registry.
%% @end
%%--------------------------------------------------------------------
-spec create_group(Node :: binary(), UserId :: binary(), Name :: binary()) ->
    {ok, Id :: binary()} | {error, Reason :: term()}.
create_group(Node, UserId, Name) ->
    try
        rpc:call(Node, group_logic, create, [UserId, Name])
    catch
        _:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Adds user to group in Global Registry.
%% @end
%%--------------------------------------------------------------------
-spec join_group(Node :: binary(), UserId :: binary(), GroupId :: binary()) ->
    ok | {error, Reason :: term()}.
join_group(Node, UserId, GroupId) ->
    try
        rpc:call(Node, erlang, apply, [fun() ->
            {ok, Token} = token_logic:create(#client{type = user, id = UserId}, group_invite_token, {group, GroupId}),
            {ok, Macaroon} = macaroon:deserialize(Token),
            {ok, GroupId} = group_logic:join(UserId, Macaroon)
        end, []]),
        ok
    catch
        _:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Creates group in Global Registry.
%% @end
%%--------------------------------------------------------------------
-spec join_space(Node :: binary(), {user | group, Id :: binary()}, SpaceId :: binary()) ->
    ok | {error, Reason :: term()}.
join_space(Node, {user, UserId}, SpaceId) ->
    try
        {ok, SpaceId} = rpc:call(Node, erlang, apply, [fun() ->
            {ok, Token} = rpc:call(Node, token_logic, create, [space_invite_user_token, {space, SpaceId}]),
            {ok, Macaroon} = macaroon:deserialize(Token),
            space_logic:join({user, UserId}, Macaroon)
        end, []]),
        ok
    catch
        _:Reason ->
            {error, Reason}
    end;

join_space(Node, {group, GroupId}, SpaceId) ->
    try
        {ok, SpaceId} = rpc:call(Node, erlang, apply, [fun() ->
            {ok, Token} = token_logic:create(#client{type = provider}, space_invite_group_token, {space, SpaceId}),
            {ok, Macaroon} = macaroon:deserialize(Token),
            space_logic:join({group, GroupId}, Macaroon)
        end, []]),
        ok
    catch
        _:Reason ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Supports space by provider.
%% @end
%%--------------------------------------------------------------------
-spec support_space(Node :: binary(), ProviderId :: binary(), SpaceId :: binary(), Size :: non_neg_integer()) ->
    ok | {error, Reason :: term()}.
support_space(Node, ProviderId, SpaceId, Size) ->
    try
        {ok, SpaceId} = rpc:call(Node, erlang, apply, [fun() ->
            {ok, Token} = token_logic:create(#client{type = provider}, space_support_token, {space, SpaceId}),
            {ok, Macaroon} = macaroon:deserialize(Token),
            space_logic:support(ProviderId, Macaroon, Size)
        end, []]),
        ok
    catch
        _:Reason ->
            {error, Reason}
    end.
