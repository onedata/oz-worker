%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Common functions for ct tests.
%%% @end
%%%-------------------------------------------------------------------

-module(location_service_test_utils).

-include_lib("ctool/include/test/test_utils.hrl").

-define(PORT, "7770").
-define(PATH, "location-service/start.js").

%% API
-export([adjust_env_desc/2, start/0, stop/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates copy of env description, which is adjusted to use
%% testmaster as location service clients bootstrap.
%% --
%% Replaces "testmaster" under "location_service_bootstrap_nodes"
%% to ip:port of testmaster's location service client.
%% @end
%%--------------------------------------------------------------------
-spec adjust_env_desc(TmpDir :: string(), EnvDescFile :: string()) -> string().
adjust_env_desc(TmpDir, EnvDescFile) ->
    TestMasterIP = get_ip(),
    TestMasterAddress = TestMasterIP ++ ":" ++ ?PORT,
    Filename = TmpDir ++ "/env_desc.json",

    {ok, Contents} = file:read_file(EnvDescFile),
    ContentsWithAddress = re:replace(Contents, "testmaster",
        TestMasterAddress, [global, {return, binary}]),

    file:write_file(Filename, ContentsWithAddress),
    Filename.

%%--------------------------------------------------------------------
%% @doc
%% Starts location service client on testmaster node.
%% This instance can be used as bootstrap for OZ location service clients.
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.
start() ->
    Command = "node ../../../" ++ ?PATH
        ++ " -h " ++ get_ip()
        ++ " -p " ++ ?PORT
        ++ " -vv",

    spawn(fun() ->
        process_flag(trap_exit, true),
        Port = erlang:open_port({spawn, Command}, [{line, 1000}, stderr_to_stdout]),
        ok = handle_loop_of_port_output(Port)
    end), ok.

%%--------------------------------------------------------------------
%% @doc
%% Stops location service client on testmaster node.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    os:cmd("pkill node").

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_ip() -> string().
get_ip() ->
    {ok, Addresses} = inet:getif(),
    [TestMasterIP] = lists:filtermap(fun
        ({{127, 0, 0, 1}, _, _}) -> false;
        ({IP, _, _}) -> {true, inet_parse:ntoa(IP)}
    end, Addresses),
    TestMasterIP.

-spec handle_loop_of_port_output(port()) -> ok | {error, term()}.
handle_loop_of_port_output(Port) ->
    receive
        {'EXIT', Port, normal} ->
            ct:print("Location Service client stopped normally"),
            ok;
        {'EXIT', Port, Reason} ->
            ct:print("Location Service client stopped abnormally due to ~p", [Reason]),
            {error, Reason};
        {Port, {data, {eol, Msg}}} ->
            ct:print("Location Service ~s", [Msg]),
            handle_loop_of_port_output(Port)
    end.
