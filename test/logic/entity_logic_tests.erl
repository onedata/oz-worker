%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc 
%%% This module contains eunit tests of entity_logic module.
%%% @end
%%%-------------------------------------------------------------------
-module(entity_logic_tests).
-author("Michal Stanisz").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("entity_logic.hrl").

-define(TOO_LONG_NAME, <<"very_very_very_looong_name_with_at_least_50_characters">>).

%%%===================================================================
%%% Tests functions
%%%===================================================================

name_normalization_test() ->
    N = fun entity_logic:normalize_name/1,

    ?assertEqual(<<"aaa---------a"/utf8>>, N(<<"aaa*&:|}{][,a"/utf8>>)),
    ?assertEqual(<<"aaa---------a"/utf8>>, N(<<"][aaa*&:|}{][,a]["/utf8>>)),
    ?assertEqual(<<"Unnamed">>, N(<<"A">>)),
    ?assertEqual(<<"group_name">>, N(<<"|group_name">>)),
    ?assertEqual(<<"group_name">>, N(<<"group_name|">>)),
    ?assertEqual(string:slice(?TOO_LONG_NAME, 0, ?NAME_MAXIMUM_LENGTH), N(?TOO_LONG_NAME)),
    ?assertEqual(<<"Unnamed">>, N(<<"--------------------------------------------------">>)),
    ?assertEqual(<<"µńż_źć-21.3(1)"/utf8>>, N(<<"µńż_źć-21.3(1)"/utf8>>)).


name_validation_test() ->
    V = fun entity_logic:validate_name/1,

    ?assertEqual(false, V(<<"aaa*&:|}{][,a"/utf8>>)),
    ?assertEqual(false, V(<<"][aaa*&:|}{][,a]["/utf8>>)),
    ?assertEqual(false, V(<<"A">>)),
    ?assertEqual(false, V(<<"|group_name">>)),
    ?assertEqual(false, V(<<"group_name|">>)),
    ?assertEqual(false, V(<<"-group_name">>)),
    ?assertEqual(false, V(<<".group_name">>)),
    ?assertEqual(false, V(<<" group_name">>)),
    ?assertEqual(false, V(<<"group_name-">>)),
    ?assertEqual(false, V(<<"group_name.">>)),
    ?assertEqual(false, V(<<"group_name ">>)),
    ?assertEqual(false, V(?TOO_LONG_NAME)),
    ?assertEqual(true, V(<<"AB">>)),
    ?assertEqual(true, V(<<"_group_name">>)),
    ?assertEqual(true, V(<<"group_name_">>)),
    ?assertEqual(true, V(<<"group_name">>)),
    ?assertEqual(true, V(<<"_group-name_">>)),
    ?assertEqual(true, V(<<"(group_name)">>)),
    ?assertEqual(true, V(<<"(group) (name)">>)),
    ?assertEqual(true, V(<<"group.- _name">>)),
    ?assertEqual(true, V(<<"My Group Name">>)),
    ?assertEqual(true, V(<<"µńż_źć-21.3(1)"/utf8>>)).


-endif.