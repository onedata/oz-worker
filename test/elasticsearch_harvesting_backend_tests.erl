%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc 
%%% This module contains eunit tests of elasticsearch harvesting backend.
%%% @end
%%%-------------------------------------------------------------------
-module(elasticsearch_harvesting_backend_tests).
-author("Michal Stanisz").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("datastore/oz_datastore_models.hrl").


%%%===================================================================
%%% Tests functions
%%%===================================================================

remove_field_test() ->
    ?assertEqual(#{}, elasticsearch_harvesting_backend:remove_field([a], #{a => b})),
    ?assertEqual([#{}], elasticsearch_harvesting_backend:remove_field([a], [#{a => b}])),
    
    ?assertEqual(#{a => #{}}, elasticsearch_harvesting_backend:remove_field([a,b], #{a => #{b => c}})),
    ?assertEqual(#{a => [#{}]}, elasticsearch_harvesting_backend:remove_field([a,b], #{a => [#{b => c}]})),
    ?assertEqual(#{a => [[[#{}]]]}, elasticsearch_harvesting_backend:remove_field([a,b], #{a => [[[#{b => c}]]]})),
    ?assertEqual(#{a => [[[#{}]],#{}]}, elasticsearch_harvesting_backend:remove_field([a,b], #{a => [[[#{b => c}]], #{b => 8}]})),
    
    ?assertEqual(#{a => #{d => e},b => c}, elasticsearch_harvesting_backend:remove_field([a,b], #{a => #{b => c, d => e}, b => c})),
    ?assertEqual(#{a => [#{},#{}]}, elasticsearch_harvesting_backend:remove_field([a,b], #{a => [#{b => c}, #{b => c}]})),
    ok.


convert_xattr_test() ->
    ?assertEqual(
        #{<<"a">> => #{<<"b">> => #{<<"c">> => #{<<"d">> => #{<<"e">> => #{<<"__value">> => 8}}}}}}, 
        elasticsearch_harvesting_backend:convert_xattrs(#{<<"a.b.c.d.e">> => 8})
    ),
    ?assertEqual(
        #{<<"a">> => #{<<"__value">> => <<"abcba">>, <<"b">> => #{<<"__value">> => 8}}}, 
        elasticsearch_harvesting_backend:convert_xattrs(#{<<"a.b">> => 8, <<"a">> => <<"abcba">>})
    ),
    ?assertEqual(
        #{<<"__rejected">> => [<<"a">>], <<"a">> => #{<<"__value">> => #{<<"__value">> => <<"abcba">>}}},
        elasticsearch_harvesting_backend:convert_xattrs(#{<<"a">> => 8, <<"a.__value">> => <<"abcba">>})
    ),
    ?assertEqual(
        #{<<"a">> => #{<<"__value">> => <<"abcba">>, <<"b">> => #{<<"__value">> => 8}}},
        elasticsearch_harvesting_backend:convert_xattrs(#{<<"a...b">> => 8, <<"a">> => <<"abcba">>})
    ),
    ?assertEqual(
        #{<<"a">> => #{
            <<"__value">> => #{<<"__value">> => 8}, 
            <<"b">> => #{<<"c">> => #{<<"d">> => #{<<"e">> => #{<<"__value">> => 8}}}}}
        }, 
        elasticsearch_harvesting_backend:convert_xattrs(#{<<"a.b.c.d.e">> => 8, <<"a.__value">> => 8})
    ),
    ok.


prepare_data_test() ->
    IndexInfo = #harvester_index{
        include_metadata = [<<"json">>, <<"xattrs">>, <<"rdf">>],
        include_file_details = [<<"fileName">>, <<"spaceId">>, <<"metadataExistenceFlags">>],
        include_rejection_reason = true
    },
    ?assertEqual(
        #{
            <<"__onedata">> => #{
                <<"fileName">> => <<"fileName">>,
                <<"json_metadata_exists">> => true, 
                <<"spaceId">> => <<"spaceId">>,
                <<"xattrs_metadata_exists">> => false,
                <<"rdf_metadata_exists">> => false
            },
            <<"a">> => <<"B">>
        },
        elasticsearch_harvesting_backend:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{<<"json">> => <<"{\"a\":\"B\"}">>},
            <<"spaceId">> => <<"spaceId">>
        }, IndexInfo, {[], <<>>})
    ),
    
    ?assertEqual(
        #{
            <<"__onedata">> => #{
                <<"fileName">> => <<"fileName">>,
                <<"json_metadata_exists">> => false,
                <<"spaceId">> => <<"spaceId">>,
                <<"rdf_metadata_exists">> => false,
                <<"xattrs_metadata_exists">> => true,
                <<"xattrs">> => #{<<"x">> => #{<<"__value">> => <<"y">>}}
            }
        },
        elasticsearch_harvesting_backend:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{<<"xattrs">> => #{<<"x">> => <<"y">>}},
            <<"spaceId">> => <<"spaceId">>
        }, IndexInfo, {[], <<>>})
    ),
    
    ?assertEqual(
        #{
            <<"__onedata">> => #{
                <<"fileName">> => <<"fileName">>,
                <<"json_metadata_exists">> => true,
                <<"spaceId">> => <<"spaceId">>,
                <<"rdf_metadata_exists">> => true,
                <<"rdf">> => <<"some rdf">>,
                <<"xattrs_metadata_exists">> => true,
                <<"xattrs">> => #{<<"x">> => #{<<"__value">> => <<"y">>}}
            },
            <<"a">> => <<"B">>
        },
        elasticsearch_harvesting_backend:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{
                <<"json">> => <<"{\"a\":\"B\"}">>, 
                <<"xattrs">> => #{<<"x">> => <<"y">>},
                <<"rdf">> => <<"\"some rdf\"">>
            },
            <<"spaceId">> => <<"spaceId">>
        }, IndexInfo, {[], <<>>})
    ),
    
    ?assertEqual(
        #{
            <<"__onedata">> => #{
                <<"__rejected">> => [<<"a">>],
                <<"__rejection_reason">> => <<"Rejection reason">>,
                <<"fileName">> => <<"fileName">>,
                <<"json_metadata_exists">> => true,
                <<"spaceId">> => <<"spaceId">>,
                <<"rdf_metadata_exists">> => false,
                <<"xattrs_metadata_exists">> => true,
                <<"xattrs">> => #{<<"x">> => #{<<"__value">> => <<"y">>}}
            },
            <<"c">> => <<"d">>
        },
        elasticsearch_harvesting_backend:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{<<"json">> => <<"{\"a\":\"B\", \"c\":\"d\"}">>, <<"xattrs">> => #{<<"x">> => <<"y">>}},
            <<"spaceId">> => <<"spaceId">>
        }, IndexInfo, {[<<"a">>], <<"Rejection reason">>})
    ),
    
    ?assertEqual(
        #{
            <<"__onedata">> => #{
                <<"__rejected">> => [<<"a">>,<<"x">>,<<"__onedata.xattrs.z">>],
                <<"__rejection_reason">> => <<"Rejection reason">>,
                <<"fileName">> => <<"fileName">>,
                <<"json_metadata_exists">> => true,
                <<"spaceId">> => <<"spaceId">>,
                <<"rdf_metadata_exists">> => false,
                <<"xattrs_metadata_exists">> => true,
                <<"xattrs">> => #{<<"x">> => #{<<"__value">> => <<"y">>}}
            },
            <<"c">> => <<"d">>
        },
        elasticsearch_harvesting_backend:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{
                <<"json">> => <<"{\"a\":\"B\", \"c\":\"d\"}">>, 
                <<"xattrs">> => #{<<"x">> => <<"y">>, <<"z">> => <<"b">>}
            },
            <<"spaceId">> => <<"spaceId">>
        }, IndexInfo, {[<<"a">>, <<"x">>, <<"__onedata.xattrs.z">>], <<"Rejection reason">>})
    ),
    
    ?assertEqual(
        #{
            <<"__onedata">> => #{
                <<"__rejection_reason">> => <<"Rejection reason">>,
                <<"fileName">> => <<"fileName">>,
                <<"json_metadata_exists">> => true,
                <<"spaceId">> => <<"spaceId">>,
                <<"rdf_metadata_exists">> => false,
                <<"xattrs_metadata_exists">> => true,
                <<"xattrs">> => #{<<"__rejected">> => <<"{\"z\":\"b\",\"x\":\"y\"}">>}
            },
            <<"__onedata.__rejected">> => <<"{\"c\":\"d\",\"a\":\"B\"}">>
        },
        elasticsearch_harvesting_backend:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{
                <<"json">> => <<"{\"a\":\"B\", \"c\":\"d\"}">>,
                <<"xattrs">> => #{<<"x">> => <<"y">>, <<"z">> => <<"b">>}
            },
            <<"spaceId">> => <<"spaceId">>
        }, IndexInfo, {all, <<"Rejection reason">>})
    ),
    
    ?assertEqual(
        #{
            <<"a">> => <<"B">>
        },
        elasticsearch_harvesting_backend:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{
                <<"json">> => <<"{\"a\":\"B\"}">>,
                <<"xattrs">> => #{<<"x">> => <<"y">>},
                <<"rdf">> => <<"\"some rdf\"">>
            },
            <<"spaceId">> => <<"spaceId">>
        }, #harvester_index{include_metadata = [<<"json">>]}, {[], <<>>})
    ),
    
    ?assertEqual(
        #{
            <<"__onedata">> => #{
                <<"xattrs">> => #{<<"x">> => #{<<"__value">> => <<"y">>}}
            }
        },
        elasticsearch_harvesting_backend:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{
                <<"json">> => <<"{\"a\":\"B\"}">>,
                <<"xattrs">> => #{<<"x">> => <<"y">>},
                <<"rdf">> => <<"\"some rdf\"">>
            },
            <<"spaceId">> => <<"spaceId">>
        }, #harvester_index{include_metadata = [<<"xattrs">>]}, {[], <<>>})
    ),
    
    ?assertEqual(
        #{
            <<"__onedata">> => #{
                <<"rdf">> => <<"some rdf">>
            }
        },
        elasticsearch_harvesting_backend:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{
                <<"json">> => <<"{\"a\":\"B\"}">>,
                <<"xattrs">> => #{<<"x">> => <<"y">>},
                <<"rdf">> => <<"\"some rdf\"">>
            },
            <<"spaceId">> => <<"spaceId">>
        }, #harvester_index{include_metadata = [<<"rdf">>]}, {[], <<>>})
    ),
    
    ?assertEqual(
        #{
            <<"__onedata.__rejected">> => <<"{\"c\":\"d\",\"a\":\"B\"}">>
        },
        elasticsearch_harvesting_backend:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{
                <<"json">> => <<"{\"a\":\"B\", \"c\":\"d\"}">>,
                <<"xattrs">> => #{<<"x">> => <<"y">>, <<"z">> => <<"b">>}
            },
            <<"spaceId">> => <<"spaceId">>}, 
            #harvester_index{include_metadata = [<<"json">>], include_rejection_reason = false}, 
            {all, <<"Rejection reason">>}
        )
    ),
    
    ?assertEqual(
        #{
            <<"a">> => <<"B">>,
            <<"c">> => <<"d">>,
            <<"__onedata">> => #{<<"__rejected">> => [<<"x">>]}
        },
        elasticsearch_harvesting_backend:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{
                <<"json">> => <<"{\"a\":\"B\", \"c\":\"d\"}">>,
                <<"xattrs">> => #{<<"x">> => <<"y">>, <<"z">> => <<"b">>}
            },
            <<"spaceId">> => <<"spaceId">>},
            #harvester_index{include_metadata = [<<"json">>], include_rejection_reason = false},
            {[<<"x">>], <<"Rejection reason">>}
        )
    ),
    
    
    ?assertEqual(
        #{},
        elasticsearch_harvesting_backend:prepare_data(#{
            <<"fileId">> => <<"fileId">>,
            <<"fileName">> => <<"fileName">>,
            <<"payload">> => #{
                <<"json">> => <<"{}">>,
                <<"xattrs">> => #{<<"x">> => <<"y">>, <<"z">> => <<"b">>}
            },
            <<"spaceId">> => <<"spaceId">>},
            #harvester_index{include_metadata = [<<"json">>], include_rejection_reason = false},
            {all, <<"Rejection reason">>}
        )
    ),
    ok.


retrieve_rejected_field_test() ->
    ?assertEqual(<<"a.b.c.d">>, elasticsearch_harvesting_backend:retrieve_rejected_field(<<"Existing mapping for [a.b.c.d] must ...">>)),
    ?assertEqual(<<"a.b.c.d">>, elasticsearch_harvesting_backend:retrieve_rejected_field(<<"... failed to parse field [a.b.c.d] of type ...">>)),
    ?assertEqual(<<"a.b.c.d">>, elasticsearch_harvesting_backend:retrieve_rejected_field(<<".. object mapping for [a.b.c.d] tried to parse ...">>)),
    ?assertEqual(<<"a.b.c.d">>, elasticsearch_harvesting_backend:retrieve_rejected_field(<<"... mapper [a.b.c.d] of different type ...">>)),
    ?assertEqual(all, elasticsearch_harvesting_backend:retrieve_rejected_field(<<"some not recognized error">>)),
    ok.


parse_batch_result_test() ->
    ErrorResponse = fun(Type, Reason) -> #{<<"error">> => #{<<"type">> => Type, <<"reason">> => Reason}} end,
    parse_batch_result_test_base(
        ok, 
        ok, 
        #{<<"items">> => [#{<<"index">> => #{<<"no_error">> => <<"in_result">>}}]}
    ),
    parse_batch_result_test_base(
        ok,
        ok, 
        #{<<"items">> => [#{<<"delete">> => #{<<"no_error">> => <<"in_result">>}}]}
    ),
    parse_batch_result_test_base(
        ok,
        ok, 
        #{<<"items">> => [
            #{<<"index">> => #{<<"no_error">> => <<"in_result">>}},
            #{<<"delete">> => #{<<"no_error">> => <<"in_result">>}}
        ]}
    ),
    parse_batch_result_test_base(
        {error, undefined, 1, <<"error: reason">>},
        {error, undefined, 1, <<"error: reason">>},
        #{<<"items">> => [#{<<"delete">> => ErrorResponse(<<"error">>, <<"reason">>)}]}
    ),
    parse_batch_result_test_base(
        {error, 2, 3, <<"error: reason">>},
        {error, 2, 3, <<"error: reason">>},
        #{<<"items">> => [
            #{<<"index">> => #{<<"no_error">> => <<"in_result">>}},
            #{<<"delete">> => #{<<"no_error">> => <<"in_result">>}},
            #{<<"index">> => ErrorResponse(<<"error">>, <<"reason">>)},
            #{<<"index">> => #{<<"no_error">> => <<"in_result">>}},
            #{<<"delete">> => #{<<"no_error">> => <<"in_result">>}}
        ]}
    ),
    Reason = <<"... mapper [a.b.c.d] of different type ...">>,
    parse_batch_result_test_base(
        ok,
        {rejected, <<"a.b.c.d">>, Reason},
        #{<<"items">> => [
            #{<<"index">> => ErrorResponse(<<"mapper_parsing_exception">>, Reason)}
        ]}
    ),
    parse_batch_result_test_base(
        ok,
        {rejected, all, <<"unrecognized error">>}, 
        #{<<"items">> => [
            #{<<"index">> => ErrorResponse(<<"mapper_parsing_exception">>, <<"unrecognized error">>)}
        ]}
    ),
    parse_batch_result_test_base(
        ok,
        {rejected, <<"a.b.c.d">>, Reason}, 
        #{<<"items">> => [
            #{<<"index">> => #{<<"no_error">> => <<"in_result">>}},
            #{<<"delete">> => #{<<"no_error">> => <<"in_result">>}},
            #{<<"index">> => ErrorResponse(<<"mapper_parsing_exception">>, Reason)},
            #{<<"index">> => #{<<"no_error">> => <<"in_result">>}},
            #{<<"delete">> => #{<<"no_error">> => <<"in_result">>}}
        ]}
    ),
    parse_batch_result_test_base(
        {error, 1, 2, <<"error: reason">>},
        {rejected, <<"a.b.c.d">>, Reason}, 
        #{<<"items">> => [
            #{<<"index">> => ErrorResponse(<<"mapper_parsing_exception">>, Reason)},
            #{<<"index">> => ErrorResponse(<<"error">>, <<"reason">>)}
        ]}
    ),
    parse_batch_result_test_base(
        {error, undefined, 1, <<"error: reason">>},
        {error, undefined, 1, <<"error: reason">>}, 
        #{<<"items">> => [
            #{<<"index">> => ErrorResponse(<<"error">>, <<"reason">>)},
            #{<<"index">> => ErrorResponse(<<"mapper_parsing_exception">>, Reason)}
        ]}
    ),
    ok.


prepare_internal_fields_schema_test() ->
    TextEsType = elasticsearch_harvesting_backend:get_es_schema_type(text),
    BooleanEsType = elasticsearch_harvesting_backend:get_es_schema_type(boolean),
    ?assertEqual(
        #{<<"rdf">> => TextEsType}, 
        elasticsearch_harvesting_backend:prepare_internal_fields_schema(
            #harvester_index{
                include_metadata = [<<"rdf">>, <<"json">>, <<"xattrs">>],
                retry_on_rejection = false,
                include_rejection_reason = false
            }, 
            #{}
    )),
    ?assertEqual(
        #{<<"__rejected">> => TextEsType},
        elasticsearch_harvesting_backend:prepare_internal_fields_schema(
            #harvester_index{
                include_metadata = [],
                retry_on_rejection = true,
                include_rejection_reason = false
            },
            #{}
    )),
    ?assertEqual(
        #{<<"__rejection_reason">> => TextEsType},
        elasticsearch_harvesting_backend:prepare_internal_fields_schema(
            #harvester_index{
                include_metadata = [],
                retry_on_rejection = false,
                include_rejection_reason = true
            },
            #{}
    )),
    ?assertEqual(
        #{<<"fileName">> => TextEsType, <<"spaceId">> => TextEsType},
        elasticsearch_harvesting_backend:prepare_internal_fields_schema(
            #harvester_index{
                include_metadata = [],
                include_file_details = [<<"fileName">>, <<"spaceId">>],
                retry_on_rejection = false,
                include_rejection_reason = false
            },
            #{}
    )),
    ?assertEqual(
        #{
            <<"json_metadata_exists">> => BooleanEsType,
            <<"xattrs_metadata_exists">> => BooleanEsType
        },
        elasticsearch_harvesting_backend:prepare_internal_fields_schema(
            #harvester_index{
                include_metadata = [<<"json">>, <<"xattrs">>],
                include_file_details = [<<"metadataExistenceFlags">>],
                retry_on_rejection = false,
                include_rejection_reason = false
            },
            #{}
    )),
    ?assertEqual(
        #{
            <<"rdf_metadata_exists">> => BooleanEsType,
            <<"rdf">> => TextEsType
        },
        elasticsearch_harvesting_backend:prepare_internal_fields_schema(
            #harvester_index{
                include_metadata = [<<"rdf">>],
                include_file_details = [<<"metadataExistenceFlags">>],
                retry_on_rejection = false,
                include_rejection_reason = false
            },
            #{}
    )),
    ?assertEqual(
        #{
            <<"json_metadata_exists">> => BooleanEsType,
            <<"rdf_metadata_exists">> => BooleanEsType,
            <<"xattrs_metadata_exists">> => BooleanEsType,
            <<"fileName">> => TextEsType, 
            <<"spaceId">> => TextEsType,
            <<"__rejected">> => TextEsType,
            <<"__rejection_reason">> => TextEsType,
            <<"rdf">> => TextEsType
        },
        elasticsearch_harvesting_backend:prepare_internal_fields_schema(
            #harvester_index{
                include_metadata = [<<"rdf">>, <<"json">>, <<"xattrs">>],
                include_file_details = [<<"fileName">>, <<"spaceId">>, <<"metadataExistenceFlags">>],
                retry_on_rejection = true,
                include_rejection_reason = true
            },
            #{}
        )),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_batch_result_test_base(ExpectedWithIgnore, ExpectedWithoutIgnore, BatchResult) ->
    ?assertEqual(ExpectedWithIgnore, elasticsearch_harvesting_backend:parse_batch_result(
        BatchResult, true)
    ),
    ?assertEqual(ExpectedWithoutIgnore, elasticsearch_harvesting_backend:parse_batch_result(
        BatchResult, false)
    ).

-endif.
