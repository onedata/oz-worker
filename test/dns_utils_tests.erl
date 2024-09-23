%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc 
%%% Eunit tests of dns_utils module.
%%% @end
%%%-------------------------------------------------------------------
-module(dns_utils_tests).
-author("Bartosz Walkowicz").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("ctool/include/test/test_utils.hrl").


%%%===================================================================
%%% Tests functions
%%%===================================================================


build_domain_test_() ->
    F = fun dns_utils:build_domain/2,

    [
        ?_assertEqual(<<"example.org">>, F(<<>>, <<"example.org">>)),
        ?_assertEqual(<<"lowercase.org">>, F(<<>>, <<"LoWeRcAsE.org">>)),
        ?_assertEqual(<<"krakow.zone.org">>, F(<<"krakow">>, <<"zone.org">>)),
        ?_assertEqual(<<"s3.krakow.zone.org">>, F(<<"s3.krakow">>, <<"zone.org">>))
    ].


build_fqdn_from_subdomain_test_() ->
    F = fun dns_utils:build_fqdn_from_subdomain/1,

    [
        ?_assertEqual(<<"krakow.onezone.org">>, F(<<"krakow">>)),
        ?_assertEqual(<<"s3.krakow.onezone.org">>, F(<<"s3.krakow">>))
    ].


is_equal_or_subdomain_test_() ->
    F = fun dns_utils:is_equal_or_subdomain/2,

    [
        ?_assertEqual(true, F(<<"example.org">>, <<"example.org">>)),
        ?_assertEqual(true, F(<<"s3.example.org">>, <<"example.org">>)),
        ?_assertEqual(false, F(<<"org">>, <<"example.org">>)),
        ?_assertEqual(false, F(<<"example.com">>, <<"example.org">>)),
        ?_assertEqual(false, F(<<"elpmaxe">>, <<"example">>))
    ].


-endif.