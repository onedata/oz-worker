%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(group_graph_tests).
-author("Michal Zmuda").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("datastore/oz_datastore_models_def.hrl").

%%%===================================================================
%%% Tests description
%%%===================================================================

example_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"finds effective privileges for an user", fun find_effective_user_privileges/0}
        ]
    }.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    ok.

teardown(_) ->
    ok.

%%%===================================================================
%%% Tests functions
%%%===================================================================

find_effective_user_privileges() ->
    G4 = #document{key = <<"G4">>, value = #user_group{
        users = [{<<"U">>, [p1, p9]}],
        groups = []
    }},
    G1 = #document{key = <<"G1">>, value = #user_group{
        users = [{<<"U">>, [p1, p5]}],
        groups = [{G4#document.key, [p1, p9]}]
    }},
    G2 = #document{key = <<"G2">>, value = #user_group{
        users = [{<<"U">>, [p1, p7]}],
        groups = []
    }},
    G3 = #document{key = <<"G3">>, value = #user_group{
        users = [],
        groups = [{G2#document.key, [p1, p2]}, {G4#document.key, [p1, p9]}]
    }},
    G5 = #document{key = <<"G5">>, value = #user_group{
        users = [],
        groups = [{G4#document.key, [p1, p8]}]
    }},
    G6 = #document{key = <<"G6">>, value = #user_group{
        users = [{<<"U">>, [p1, p2]}],
        groups = [{G1#document.key, [p1, p2, p5, p7]}, {G3#document.key, [p1, p2, p9]},
            {G5#document.key, [p1, p8]}]
    }},

    meck:new(user_group),
    meck:expect(user_group, get, fun
        (<<"G1">>) -> io:format("G1 "), {ok, G1};
        (<<"G2">>) -> io:format("G2 "), {ok, G2};
        (<<"G3">>) -> io:format("G3 "), {ok, G3};
        (<<"G4">>) -> io:format("G4 "), {ok, G4};
        (<<"G5">>) -> io:format("G5 "), {ok, G5};
        (<<"G6">>) -> io:format("G6 "), {ok, G6}
    end),

    EffectivePrivileges = group_graph:find_effective_user_privileges(<<"U">>, <<"G6">>),
    ?assertEqual([p1, p2, p5, p9], EffectivePrivileges).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-endif.