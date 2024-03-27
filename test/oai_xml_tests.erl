%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc 
%%% Eunit tests of XML handling in the OAI PMH machinery.
%%% @end
%%%-------------------------------------------------------------------
-module(oai_xml_tests).
-author("Lukasz Opiola").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("ctool/include/test/test_utils.hrl").


%%%===================================================================
%%% Tests functions
%%%===================================================================


encode_decode_utf8_test() ->
    Prolog = <<"<?xml version=\"1.0\" encoding=\"utf-8\" ?>">>,
    RawXmlWithUtf8Chars = <<
        "<tool name=\"óœę↓©’ŋśð←æŋ\" id=\"µńćźżąśð日本を- 旅す. d'ŋ-ジ(ャ\" version=\"1.0.0\">"
        "<description>l;'\\zxcvbnm,./əł</description>"
        "<item action=\"/library/index\" method=\"get\" target=\"_parent\">"
        "<param name=\"default_action\" type=\"hidden\" value=\"import_to_histories\"/>"
%%        TODO VFS-11365 xmerl strips comments: https://github.com/erlang/otp/issues/5697
%%                       consider using a different lib...
%%        "<!-- This is a comment d'ŋ-ジ(ャパル -->"
        "</item>"
        "</tool>"/utf8
    >>,
    % normalization of the metadata XML should ensure that it is wrapped in top-level
    % "metadata" tags
    ?assertEqual(
        <<Prolog/binary, "<metadata>", RawXmlWithUtf8Chars/binary, "</metadata>">>,
        parse_and_normalize_and_encode(RawXmlWithUtf8Chars)
    ),
    ?assertEqual(
        <<Prolog/binary, "<metadata>", RawXmlWithUtf8Chars/binary, "</metadata>">>,
        parse_and_normalize_and_encode(<<Prolog/binary, RawXmlWithUtf8Chars/binary>>)
    ),

    RawXmlWithUtf8CharsWrappedInMetadataTags = <<
        "<metadata>"
        "<tool name=\"óœę↓©’ŋśð←æŋ\" id=\"µńćźżąśð日本を- 旅す. d'ŋ-ジ(ャ\" version=\"1.0.0\">"
        "<description>l;'\\zxcvbnm,./əł</description>"
%%        TODO VFS-11365 xmerl strips comments: https://github.com/erlang/otp/issues/5697
%%                       consider using a different lib...
%%        "<!-- This is a comment d'ŋ-ジ(ャパル -->"
        "<item action=\"/library/index\" method=\"get\" target=\"_parent\">"
        "<param name=\"default_action\" type=\"hidden\" value=\"import_to_histories\"/>"
        "</item>"
        "</tool>"
        "</metadata>"/utf8
    >>,
    ?assertEqual(
        <<Prolog/binary, RawXmlWithUtf8CharsWrappedInMetadataTags/binary>>,
        parse_and_normalize_and_encode(RawXmlWithUtf8CharsWrappedInMetadataTags)
    ),
    ?assertEqual(
        <<Prolog/binary, RawXmlWithUtf8CharsWrappedInMetadataTags/binary>>,
        parse_and_normalize_and_encode(<<Prolog/binary, RawXmlWithUtf8CharsWrappedInMetadataTags/binary>>)
    ).


%% @private
parse_and_normalize_and_encode(RawXml) ->
    {ok, ParsedXml} = oai_metadata:parse_and_normalize_xml(RawXml),
    oai_utils:encode_xml(ParsedXml).


-endif.