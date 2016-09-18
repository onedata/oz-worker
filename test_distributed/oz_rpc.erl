%%%-------------------------------------------------------------------
%%% @author lopiola
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2016 20:53
%%%-------------------------------------------------------------------
-module(oz_rpc).
-author("lopiola").

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([call/4]).

call(Config, Module, Function, Args) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    Fun = fun() ->
        try
            erlang:apply(Module, Function, Args)
        catch Type:Reason ->
            {crash, Type, Reason, erlang:get_stacktrace()}
        end
    end,
    case rpc:call(Node, erlang, apply, [Fun, []]) of
        {crash, Type, Reason, Stacktrace} ->
            % Log a bad rpc - very useful when debugging tests.
            ct:print(
                "oz_proxy call crashed!~n"
                "Module: ~p~n"
                "Function: ~p~n"
                "Args: ~p~n"
                "Error: ~p:~p~n"
                "Stacktrace: ~p",
                [Module, Function, Args, Type, Reason, Stacktrace]
            ),
            {badrpc, Reason};
        Result ->
            Result
    end.