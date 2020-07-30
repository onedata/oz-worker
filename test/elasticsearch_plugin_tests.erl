%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc 
%%% This module contains eunit tests of elasticsearch harvester plugin.
%%% @end
%%%-------------------------------------------------------------------
-module(elasticsearch_plugin_tests).
-author("Michal Stanisz").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


%%%===================================================================
%%% Tests functions
%%%===================================================================

elasticsearch_test_() ->
    {
        setup, fun setup/0, []
    }.

remove_field_test() ->
    ?assertEqual(#{}, elasticsearch_plugin:remove_field([a], #{a => b})),
    ?assertEqual([#{}], elasticsearch_plugin:remove_field([a], [#{a => b}])),
    
    ?assertEqual(#{a => #{}}, elasticsearch_plugin:remove_field([a,b], #{a => #{b => c}})),
    ?assertEqual(#{a => [#{}]}, elasticsearch_plugin:remove_field([a,b], #{a => [#{b => c}]})),
    ?assertEqual(#{a => [[[#{}]]]}, elasticsearch_plugin:remove_field([a,b], #{a => [[[#{b => c}]]]})),
    ?assertEqual(#{a => [[[#{}]],#{}]}, elasticsearch_plugin:remove_field([a,b], #{a => [[[#{b => c}]], #{b => 8}]})),
    
    ?assertEqual(#{a => #{d => e},b => c}, elasticsearch_plugin:remove_field([a,b], #{a => #{b => c, d => e}, b => c})),
    ?assertEqual(#{a => [#{},#{}]}, elasticsearch_plugin:remove_field([a,b], #{a => [#{b => c}, #{b => c}]})),
    ok.


convert_xattr_test() ->
    ?assertEqual(
        #{<<"a">> => #{<<"b">> => #{<<"c">> => #{<<"d">> => #{<<"e">> => #{<<"__value">> => 8}}}}}}, 
        elasticsearch_plugin:convert_xattrs(#{<<"a.b.c.d.e">> => 8})
    ),
    ?assertEqual(
        #{<<"a">> => #{<<"__value">> => <<"abcba">>, <<"b">> => #{<<"__value">> => 8}}}, 
        elasticsearch_plugin:convert_xattrs(#{<<"a.b">> => 8, <<"a">> => <<"abcba">>})
    ),
    ?assertEqual(
        #{<<"__rejected">> => [<<"a">>], <<"a">> => #{<<"__value">> => #{<<"__value">> => <<"abcba">>}}},
        elasticsearch_plugin:convert_xattrs(#{<<"a">> => 8, <<"a.__value">> => <<"abcba">>})
    ),
    ?assertEqual(
        #{<<"a">> => #{<<"__value">> => <<"abcba">>, <<"b">> => #{<<"__value">> => 8}}},
        elasticsearch_plugin:convert_xattrs(#{<<"a...b">> => 8, <<"a">> => <<"abcba">>})
    ),
    ?assertEqual(
        #{<<"a">> => #{
            <<"__value">> => #{<<"__value">> => 8}, 
            <<"b">> => #{<<"c">> => #{<<"d">> => #{<<"e">> => #{<<"__value">> => 8}}}}}
        }, 
        elasticsearch_plugin:convert_xattrs(#{<<"a.b.c.d.e">> => 8, <<"a.__value">> => 8})
    ),
    ok.


prepare_data_test() ->
    ?assertEqual(
        #{
            <<"__onedata">> => #{
                <<"fileName">> => <<"fileName">>,
                <<"json_metadata_exists">> => true, 
                <<"spaceId">> => <<"spaceId">>,
                <<"xattr_metadata_exists">> => false},
            <<"a">> => <<"B">>
        },
        elasticsearch_plugin:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{<<"json">> => <<"{\"a\":\"B\"}">>},
            <<"spaceId">> => <<"spaceId">>
        }, {[], <<>>})
    ),
    
    ?assertEqual(
        #{
            <<"__onedata">> => #{
                <<"fileName">> => <<"fileName">>,
                <<"json_metadata_exists">> => false,
                <<"spaceId">> => <<"spaceId">>,
                <<"xattr_metadata_exists">> => true,
                <<"xattrs">> => #{<<"x">> => #{<<"__value">> => <<"y">>}}
            }
        },
        elasticsearch_plugin:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{<<"xattrs">> => #{<<"x">> => <<"y">>}},
            <<"spaceId">> => <<"spaceId">>
        }, {[], <<>>})
    ),
    
    ?assertEqual(
        #{
            <<"__onedata">> => #{
                <<"fileName">> => <<"fileName">>,
                <<"json_metadata_exists">> => true,
                <<"spaceId">> => <<"spaceId">>,
                <<"xattr_metadata_exists">> => true,
                <<"xattrs">> => #{<<"x">> => #{<<"__value">> => <<"y">>}}
            },
            <<"a">> => <<"B">>
        },
        elasticsearch_plugin:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{<<"json">> => <<"{\"a\":\"B\"}">>, <<"xattrs">> => #{<<"x">> => <<"y">>}},
            <<"spaceId">> => <<"spaceId">>
        }, {[], <<>>})
    ),
    
    ?assertEqual(
        #{
            <<"__onedata">> => #{
                <<"__rejected">> => [<<"a">>],
                <<"__rejection_reason">> => <<"Rejection reason">>,
                <<"fileName">> => <<"fileName">>,
                <<"json_metadata_exists">> => true,
                <<"spaceId">> => <<"spaceId">>,
                <<"xattr_metadata_exists">> => true,
                <<"xattrs">> => #{<<"x">> => #{<<"__value">> => <<"y">>}}
            },
            <<"c">> => <<"d">>
        },
        elasticsearch_plugin:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{<<"json">> => <<"{\"a\":\"B\", \"c\":\"d\"}">>, <<"xattrs">> => #{<<"x">> => <<"y">>}},
            <<"spaceId">> => <<"spaceId">>
        }, {[<<"a">>], <<"Rejection reason">>})
    ),
    
    ?assertEqual(
        #{
            <<"__onedata">> => #{
                <<"__rejected">> => [<<"a">>,<<"x">>,<<"__onedata.xattrs.z">>],
                <<"__rejection_reason">> => <<"Rejection reason">>,
                <<"fileName">> => <<"fileName">>,
                <<"json_metadata_exists">> => true,
                <<"spaceId">> => <<"spaceId">>,
                <<"xattr_metadata_exists">> => true,
                <<"xattrs">> => #{<<"x">> => #{<<"__value">> => <<"y">>}}
            },
            <<"c">> => <<"d">>
        },
        elasticsearch_plugin:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{<<"json">> => 
                <<"{\"a\":\"B\", \"c\":\"d\"}">>, 
                <<"xattrs">> => #{<<"x">> => <<"y">>, <<"z">> => <<"b">>}
            },
            <<"spaceId">> => <<"spaceId">>
        }, {[<<"a">>, <<"x">>, <<"__onedata.xattrs.z">>], <<"Rejection reason">>})
    ),
    
    ?assertEqual(
        #{
            <<"__onedata">> => #{
                <<"__rejection_reason">> => <<"Rejection reason">>,
                <<"fileName">> => <<"fileName">>,
                <<"json_metadata_exists">> => true,
                <<"spaceId">> => <<"spaceId">>,
                <<"xattr_metadata_exists">> => true,
                <<"xattrs">> => #{<<"__rejected">> => <<"{\"z\":\"b\",\"x\":\"y\"}">>}
            },
            <<"__onedata.__rejected">> => <<"{\"c\":\"d\",\"a\":\"B\"}">>
        },
        elasticsearch_plugin:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{<<"json">> =>
            <<"{\"a\":\"B\", \"c\":\"d\"}">>,
                <<"xattrs">> => #{<<"x">> => <<"y">>, <<"z">> => <<"b">>}
            },
            <<"spaceId">> => <<"spaceId">>
        }, {all, <<"Rejection reason">>})
    ),
    ok.


retrieve_rejected_field_test() ->
    ?assertEqual(<<"a.b.c.d">>, elasticsearch_plugin:retrieve_rejected_field(<<"Existing mapping for [a.b.c.d] must ...">>)),
    ?assertEqual(<<"a.b.c.d">>, elasticsearch_plugin:retrieve_rejected_field(<<"... failed to parse field [a.b.c.d] of type ...">>)),
    ?assertEqual(<<"a.b.c.d">>, elasticsearch_plugin:retrieve_rejected_field(<<".. object mapping for [a.b.c.d] tried to parse ...">>)),
    ?assertEqual(<<"a.b.c.d">>, elasticsearch_plugin:retrieve_rejected_field(<<"... mapper [a.b.c.d] of different type ...">>)),
    ?assertEqual(all, elasticsearch_plugin:retrieve_rejected_field(<<"some not recognized error">>)),
    ok.


parse_batch_result_test() ->
    MakeSeq = fun(MaxSeq) -> lists:map(fun(Seq) -> #{<<"seq">> => Seq} end, lists:seq(1, MaxSeq)) end,
    ErrorResponse = fun(Type, Reason) -> #{<<"error">> => #{<<"type">> => Type, <<"reason">> => Reason}} end,
    ?assertEqual(ok, elasticsearch_plugin:parse_batch_result(
        #{<<"items">> => [#{<<"index">> => #{<<"no_error">> => <<"in_result">>}}]}, 
        MakeSeq(1))
    ),
    ?assertEqual(ok, elasticsearch_plugin:parse_batch_result(
        #{<<"items">> => [#{<<"delete">> => #{<<"no_error">> => <<"in_result">>}}]},
        MakeSeq(1))
    ),
    ?assertEqual(ok, elasticsearch_plugin:parse_batch_result(
        #{<<"items">> => [
            #{<<"index">> => #{<<"no_error">> => <<"in_result">>}},
            #{<<"delete">> => #{<<"no_error">> => <<"in_result">>}}
        ]},
        MakeSeq(2))
    ),
    ?assertEqual({error, undefined, 1, <<"error: reason">>}, elasticsearch_plugin:parse_batch_result(
        #{<<"items">> => [#{<<"delete">> => ErrorResponse(<<"error">>, <<"reason">>)}]},
        MakeSeq(1))
    ),
    ?assertEqual({error, 2, 3, <<"error: reason">>}, elasticsearch_plugin:parse_batch_result(
        #{<<"items">> => [
            #{<<"index">> => #{<<"no_error">> => <<"in_result">>}},
            #{<<"delete">> => #{<<"no_error">> => <<"in_result">>}},
            #{<<"index">> => ErrorResponse(<<"error">>, <<"reason">>)},
            #{<<"index">> => #{<<"no_error">> => <<"in_result">>}},
            #{<<"delete">> => #{<<"no_error">> => <<"in_result">>}}
        ]},
        MakeSeq(5))
    ),
    Reason = <<"... mapper [a.b.c.d] of different type ...">>,
    ?assertEqual({rejected, <<"a.b.c.d">>, Reason}, elasticsearch_plugin:parse_batch_result(
        #{<<"items">> => [
            #{<<"index">> => ErrorResponse(<<"mapper_parsing_exception">>, Reason)}
        ]},
        MakeSeq(1))
    ),
    ?assertEqual({rejected, all, <<"unrecognized error">>}, elasticsearch_plugin:parse_batch_result(
        #{<<"items">> => [
            #{<<"index">> => ErrorResponse(<<"mapper_parsing_exception">>, <<"unrecognized error">>)}
        ]},
        MakeSeq(1))
    ),
    ?assertEqual({rejected, <<"a.b.c.d">>, Reason}, elasticsearch_plugin:parse_batch_result(
        #{<<"items">> => [
            #{<<"index">> => #{<<"no_error">> => <<"in_result">>}},
            #{<<"delete">> => #{<<"no_error">> => <<"in_result">>}},
            #{<<"index">> => ErrorResponse(<<"mapper_parsing_exception">>, Reason)},
            #{<<"index">> => #{<<"no_error">> => <<"in_result">>}},
            #{<<"delete">> => #{<<"no_error">> => <<"in_result">>}}
        ]},
        MakeSeq(5))
    ),
    ?assertEqual({rejected, <<"a.b.c.d">>, Reason}, elasticsearch_plugin:parse_batch_result(
        #{<<"items">> => [
            #{<<"index">> => ErrorResponse(<<"mapper_parsing_exception">>, Reason)},
            #{<<"index">> => ErrorResponse(<<"error">>, <<"reason">>)}
        ]},
        MakeSeq(2))
    ),
    ?assertEqual({error, undefined, 1, <<"error: reason">>}, elasticsearch_plugin:parse_batch_result(
        #{<<"items">> => [
            #{<<"index">> => ErrorResponse(<<"error">>, <<"reason">>)},
            #{<<"index">> => ErrorResponse(<<"mapper_parsing_exception">>, Reason)}
            ]},
        MakeSeq(2))
    ),
    ok.


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

setup() ->
    {ok, Module} = compile:file("rel/files/plugins/elasticsearch_plugin", [{d, 'TEST'}]),
    code:purge(Module),
    {module, Module} = code:load_file(Module).

    
-endif.
