%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C): 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc 
%%% This module contains eunit tests of group_logic module.
%%% @end
%%%-------------------------------------------------------------------
-module(group_logic_tests).
-author("Michal Stanisz").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("entity_logic.hrl").

-define(TOO_LONG_NAME, <<"very_very_very_long_name_with_at_lest_50_characters">>).

%%%===================================================================
%%% Tests functions
%%%===================================================================

name_normalization_test() ->
    IncorrectName = <<"aaa*&:|}{][,a"/utf8>>,
    ?assertEqual(<<"aaa---------a"/utf8>>, group_logic:normalize_name(IncorrectName)), 
    ShortName = <<"A">>,
    ?assertEqual(<<"(A)">>, group_logic:normalize_name(ShortName)),
    LeadingDashName = <<"|group_name">>,
    ?assertEqual(<<"(-group_name)">>, group_logic:normalize_name(LeadingDashName)),
    TrailingDashName = <<"group_name|">>,
    ?assertEqual(<<"(group_name-)">>, group_logic:normalize_name(TrailingDashName)),
    ?assertEqual(string:slice(?TOO_LONG_NAME, 0, ?MAXIMUM_NAME_LENGTH), 
        group_logic:normalize_name(?TOO_LONG_NAME)),
    LongNameNotPassingValidation = <<"--------------------------------------------------">>,
    ?assertEqual(<<"(------------------------------------------------)">>, 
        group_logic:normalize_name(LongNameNotPassingValidation)),
    CorrectName = <<"µńż_źć-21.3(1)"/utf8>>,
    ?assertEqual(CorrectName, group_logic:normalize_name(CorrectName)).

-endif.