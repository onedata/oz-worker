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


full_name_normalization_test() ->
    N = fun entity_logic_sanitizer:normalize_full_name/1,

    ?assertEqual(<<"aaa--------a"/utf8>>, N(<<"aaa*&:|}{][a"/utf8>>)),
    ?assertEqual(<<"aaa----',----,a"/utf8>>, N(<<"][aaa*&:|',}{][,a]["/utf8>>)),
    ?assertEqual(<<"Unnamed User">>, N(<<"A">>)),
    ?assertEqual(<<"user-name">>, N(<<"|user-name">>)),
    ?assertEqual(<<"user-name">>, N(<<"user-name|">>)),
    ?assertEqual(
        binary:replace(string:slice(?TOO_LONG_NAME, 0, ?NAME_MAXIMUM_LENGTH), <<"_">>, <<"-">>, [global]),
        N(?TOO_LONG_NAME)
    ),
    ?assertEqual(<<"Unnamed User">>, N(<<"--------------------------------------------------">>)),
    ?assertEqual(<<"µ,ń'żźć -21.31"/utf8>>, N(<<"µ,ń'żźć -21.31"/utf8>>)).


username_normalization_test() ->
    N = fun entity_logic_sanitizer:normalize_username/1,

    ?assertEqual(<<"aaa.---------a"/utf8>>, N(<<"aaa.*&:|}{][,a"/utf8>>)),
    ?assertEqual(<<"aaa.-----------a"/utf8>>, N(<<"][aaa.*&:|',}{][,a]["/utf8>>)),
    ?assertEqual(undefined, N(<<"A">>)),
    ?assertEqual(<<"user-name">>, N(<<"|user-name">>)),
    ?assertEqual(<<"user-name">>, N(<<"user-name|">>)),
    ?assertEqual(string:slice(?TOO_LONG_NAME, 0, ?USERNAME_MAXIMUM_LENGTH), N(?TOO_LONG_NAME)),
    ?assertEqual(undefined, N(<<"--------------------------------------------------">>)),
    ?assertEqual(<<"21-31">>, N(<<"µ,ń'żźć -21^31"/utf8>>)),
    ?assertEqual(<<"21.31">>, N(<<"µ,ń'żźć -21.31"/utf8>>)),
    ?assertEqual(undefined, N(<<"µ,ń'żźć -2ąęŻ.łó"/utf8>>)).


full_name_validation_test() ->
    V = fun entity_logic_sanitizer:validate_full_name/1,

    ?assertEqual(false, V(<<"aaa*&:|}{][,a"/utf8>>)),
    ?assertEqual(false, V(<<"][aaa*&:|}{][,a]["/utf8>>)),
    ?assertEqual(false, V(<<"A">>)),
    ?assertEqual(false, V(<<"|user-name">>)),
    ?assertEqual(false, V(<<"user-name|">>)),
    ?assertEqual(false, V(<<"_user-name">>)),
    ?assertEqual(false, V(<<"-user-name">>)),
    ?assertEqual(false, V(<<".user-name">>)),
    ?assertEqual(false, V(<<" user-name">>)),
    ?assertEqual(false, V(<<"user-name_">>)),
    ?assertEqual(false, V(<<"user-name-">>)),
    ?assertEqual(false, V(<<"user-name ">>)),
    ?assertEqual(false, V(<<"(user-name)">>)),
    ?assertEqual(false, V(<<"(user) (name)">>)),
    ?assertEqual(false, V(?TOO_LONG_NAME)),
    ?assertEqual(true, V(<<"AB">>)),
    ?assertEqual(true, V(<<"user-name.">>)),
    ?assertEqual(true, V(<<"user-name">>)),
    ?assertEqual(true, V(<<"user.- ,'name">>)),
    ?assertEqual(true, V(<<"Thomas Mc'Donald, Jr.">>)),
    ?assertEqual(true, V(<<"µńż',źć-21.31"/utf8>>)).


username_validation_test() ->
    V = fun entity_logic_sanitizer:validate_username/1,

    ?assertEqual(false, V(<<"aaa*&:|}{][,a"/utf8>>)),
    ?assertEqual(false, V(<<"][aaa*&:|}{][,a]["/utf8>>)),
    ?assertEqual(false, V(<<"A">>)),
    ?assertEqual(false, V(<<"|user-name">>)),
    ?assertEqual(false, V(<<"user-name|">>)),
    ?assertEqual(false, V(<<"_user-name">>)),
    ?assertEqual(false, V(<<"-user-name">>)),
    ?assertEqual(false, V(<<".user-name">>)),
    ?assertEqual(false, V(<<" user-name">>)),
    ?assertEqual(false, V(<<"user-name_">>)),
    ?assertEqual(false, V(<<"user-name-">>)),
    ?assertEqual(false, V(<<"user-name.">>)),
    ?assertEqual(false, V(<<"user-name ">>)),
    ?assertEqual(false, V(<<"(user-name)">>)),
    ?assertEqual(false, V(<<"(user) (name)">>)),
    ?assertEqual(false, V(<<"user.- ,'name">>)),
    ?assertEqual(false, V(<<"µńż_źć-21.31"/utf8>>)),
    ?assertEqual(false, V(?TOO_LONG_NAME)),
    ?assertEqual(true, V(<<"AB">>)),
    ?assertEqual(true, V(<<"A-B">>)),
    ?assertEqual(true, V(<<"A_B">>)),
    ?assertEqual(true, V(<<"user-name">>)),
    ?assertEqual(true, V(<<"user_name">>)),
    ?assertEqual(true, V(<<"My-Username">>)),
    ?assertEqual(true, V(<<"My-4l14s">>)).


-endif.