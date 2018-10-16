%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc 
%%% This module contains eunit tests of user_logic module.
%%% @end
%%%-------------------------------------------------------------------
-module(user_logic_tests).
-author("Lukasz Opiola").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("entity_logic.hrl").

-define(TOO_LONG_NAME, <<"very-very-very-looong-name-with-at-least-50-characters">>).

%%%===================================================================
%%% Tests functions
%%%===================================================================

name_normalization_test() ->
    N = fun user_logic:normalize_name/1,

    ?assertEqual(<<"aaa--------a"/utf8>>, N(<<"aaa*&:|}{][a"/utf8>>)),
    ?assertEqual(<<"aaa----',----,a"/utf8>>, N(<<"][aaa*&:|',}{][,a]["/utf8>>)),
    ?assertEqual(<<"Unnamed User">>, N(<<"A">>)),
    ?assertEqual(<<"user-name">>, N(<<"|user-name">>)),
    ?assertEqual(<<"user-name">>, N(<<"user-name|">>)),
    ?assertEqual(string:slice(?TOO_LONG_NAME, 0, ?NAME_MAXIMUM_LENGTH), N(?TOO_LONG_NAME)),
    ?assertEqual(<<"Unnamed User">>, N(<<"--------------------------------------------------">>)),
    ?assertEqual(<<"µ,ń'żźć -21.31"/utf8>>, N(<<"µ,ń'żźć -21.31"/utf8>>)).


alias_normalization_test() ->
    N = fun user_logic:normalize_alias/1,

    ?assertEqual(<<"aaa.---------a"/utf8>>, N(<<"aaa.*&:|}{][,a"/utf8>>)),
    ?assertEqual(<<"aaa.-----------a"/utf8>>, N(<<"][aaa.*&:|',}{][,a]["/utf8>>)),
    ?assertEqual(undefined, N(<<"A">>)),
    ?assertEqual(<<"user-alias">>, N(<<"|user-alias">>)),
    ?assertEqual(<<"user-alias">>, N(<<"user-alias|">>)),
    ?assertEqual(string:slice(?TOO_LONG_NAME, 0, ?ALIAS_MAXIMUM_LENGTH), N(?TOO_LONG_NAME)),
    ?assertEqual(undefined, N(<<"--------------------------------------------------">>)),
    ?assertEqual(<<"21-31">>, N(<<"µ,ń'żźć -21^31"/utf8>>)),
    ?assertEqual(<<"21.31">>, N(<<"µ,ń'żźć -21.31"/utf8>>)),
    ?assertEqual(undefined, N(<<"µ,ń'żźć -2ąęŻ.łó"/utf8>>)).


name_validation_test() ->
    V = fun user_logic:validate_name/1,

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


alias_validation_test() ->
    V = fun user_logic:validate_alias/1,

    ?assertEqual(false, V(<<"aaa*&:|}{][,a"/utf8>>)),
    ?assertEqual(false, V(<<"][aaa*&:|}{][,a]["/utf8>>)),
    ?assertEqual(false, V(<<"A">>)),
    ?assertEqual(false, V(<<"|user-alias">>)),
    ?assertEqual(false, V(<<"user-alias|">>)),
    ?assertEqual(false, V(<<"_user-alias">>)),
    ?assertEqual(false, V(<<"-user-alias">>)),
    ?assertEqual(false, V(<<".user-alias">>)),
    ?assertEqual(false, V(<<" user-alias">>)),
    ?assertEqual(false, V(<<"user-alias_">>)),
    ?assertEqual(false, V(<<"user-alias-">>)),
    ?assertEqual(false, V(<<"user-alias.">>)),
    ?assertEqual(false, V(<<"user-alias ">>)),
    ?assertEqual(false, V(<<"(user-alias)">>)),
    ?assertEqual(false, V(<<"(user) (alias)">>)),
    ?assertEqual(false, V(<<"user.- ,'alias">>)),
    ?assertEqual(false, V(<<"µńż_źć-21.31"/utf8>>)),
    ?assertEqual(false, V(?TOO_LONG_NAME)),
    ?assertEqual(true, V(<<"AB">>)),
    ?assertEqual(true, V(<<"A-B">>)),
    ?assertEqual(true, V(<<"A_B">>)),
    ?assertEqual(true, V(<<"user-alias">>)),
    ?assertEqual(true, V(<<"user_alias">>)),
    ?assertEqual(true, V(<<"My-Alias">>)),
    ?assertEqual(true, V(<<"My-4l14s">>)).


-endif.