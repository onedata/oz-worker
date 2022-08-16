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
-include_lib("ctool/include/test/test_utils.hrl").
-include("entity_logic.hrl").


%%%===================================================================
%%% Tests functions
%%%===================================================================

name_normalization_test() ->
    N = fun(Name) -> entity_logic_sanitizer:normalize_name(Name, ?UNKNOWN_ENTITY_NAME) end,

    ?assertEqual(<<"aaa---------a"/utf8>>, N(<<"aaa*&:|}{][,a"/utf8>>)),
    ?assertEqual(<<"aaa---------a"/utf8>>, N(<<"][aaa*&:|}{][,a]["/utf8>>)),
    ?assertEqual(?UNKNOWN_ENTITY_NAME, N(<<"A">>)),
    ?assertEqual(<<"group_name">>, N(<<"|group_name">>)),
    ?assertEqual(<<"group_name">>, N(<<"*&:|group_name">>)),
    ?assertEqual(<<"group_name">>, N(<<"group_name|">>)),
    ?assertEqual(<<"group_name">>, N(<<"group_name|*&:">>)),
    ?assertEqual(<<"group----name">>, N(<<"*&:|group*&:|name|*&:">>)),
    ?assertEqual(string:slice(?TOO_LONG_NAME, 0, ?NAME_MAXIMUM_LENGTH), N(?TOO_LONG_NAME)),
    ?assertEqual(?UNKNOWN_ENTITY_NAME, N(<<"--------------------------------------------------">>)),
    ?assertEqual(<<"µńż_źć-21.3(1)"/utf8>>, N(<<"µńż_źć-21.3(1)"/utf8>>)).


-endif.