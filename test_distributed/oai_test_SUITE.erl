%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% This module contains CT tests of OAI-PMH repository interface
%%% supported by oz-worker.
%%% @end
%%%-------------------------------------------------------------------
-module(oai_test_SUITE).
-author("Jakub Kudzia").

-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include_lib("ctool/include/privileges.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("registered_names.hrl").
-include("oai_test_SUITE.hrl").
-include("http/handlers/oai.hrl").

%% API
-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2, end_per_suite/1]).

%% Test functions
-export([
    identify_get_test/1, identify_post_test/1,
    no_verb_get_test/1, no_verb_post_test/1,
    invalid_verb_get_test/1, invalid_verb_post_test/1,
    empty_verb_get_test/1, empty_verb_post_test/1,
    illegal_arg_get_test/1, illegal_arg_post_test/1,
    get_record_get_test/1, get_record_post_test/1,
    missing_arg_get_test/1, missing_arg_post_test/1,
    id_not_existing_get_test/1, id_not_existing_post_test/1,
    cannot_disseminate_format_get_test/1, cannot_disseminate_format_post_test/1,
    no_set_hierarchy_get_test/1, no_set_hierarchy_post_test/1,
    list_metadata_formats_get_test/1, list_metadata_formats_post_test/1,
    list_metadata_formats_no_format_error_get_test/1,
    list_metadata_formats_no_format_error_post_test/1,
    list_identifiers_get_test/1, list_identifiers_post_test/1,
    list_identifiers_empty_repository_error_get_test/1,
    list_identifiers_empty_repository_error_post_test/1,
    selective_list_identifiers1_get_test/1, selective_list_identifiers1_post_test/1,
    selective_list_identifiers2_get_test/1, selective_list_identifiers2_post_test/1,
    selective_list_identifiers3_get_test/1, selective_list_identifiers3_post_test/1,
    selective_list_identifiers4_get_test/1, selective_list_identifiers4_post_test/1,
    list_identifiers_no_records_match_error1_get_test/1,
    list_identifiers_no_records_match_error1_post_test/1,
    list_identifiers_no_records_match_error2_get_test/1,
    list_identifiers_no_records_match_error2_post_test/1,
    list_identifiers_granularity_mismatch_error_get_test/1,
    list_identifiers_granularity_mismatch_error_post_test/1,
    list_identifiers_wrong_date_format_error1_get_test/1,
    list_identifiers_wrong_date_format_error1_post_test/1,
    list_identifiers_wrong_date_format_error2_get_test/1,
    list_identifiers_wrong_date_format_error2_post_test/1,
    list_identifiers_wrong_date_format_error3_get_test/1,
    list_identifiers_wrong_date_format_error3_post_test/1,
    list_records_get_test/1, list_records_post_test/1,
    selective_list_records1_get_test/1, selective_list_records1_post_test/1,
    selective_list_records2_get_test/1, selective_list_records2_post_test/1,
    selective_list_records3_get_test/1, selective_list_records3_post_test/1,
    selective_list_records4_get_test/1, selective_list_records4_post_test/1,
    list_records_no_set_error_get_test/1, list_records_no_set_error_post_test/1,
    identify_change_earliest_datestamp_get_test/1,
    identify_change_earliest_datestamp_post_test/1,
    list_identifiers_modify_timestamp_get_test/1,
    list_identifiers_modify_timestamp_post_test/1,
    list_identifiers_modify_timestamp1_get_test/1,
    list_identifiers_modify_timestamp1_post_test/1,
    list_identifiers_modify_timestamp2_get_test/1,
    list_identifiers_modify_timestamp2_post_test/1,
    list_records_modify_timestamp_get_test/1,
    list_records_modify_timestamp_post_test/1,
    list_records_modify_timestamp1_get_test/1,
    list_records_modify_timestamp1_post_test/1,
    list_records_modify_timestamp2_get_test/1,
    list_records_modify_timestamp2_post_test/1]).

%% useful macros
-define(CONTENT_TYPE_HEADER,
    #{<<"content-type">> => <<"application/x-www-form-urlencoded">>}).
-define(RESPONSE_CONTENT_TYPE_HEADER,
    #{<<"content-type">> => <<"text/xml">>}).

%% Example test data
-define(SHARE_ID, <<"identifier1">>).
-define(SHARE_IDS(Num),
    lists:map(fun(N) ->
        list_to_binary("identifier" ++ integer_to_list(N))
    end, lists:seq(0, Num - 1))
).
-define(SPACE_NAME1, <<"space1">>).
-define(SPACE_NAMES(Num),
    lists:map(fun(N) ->
        list_to_binary("space" ++ integer_to_list(N))
    end, lists:seq(0, Num - 1))).
-define(DC_METADATA_PREFIX, <<"oai_dc">>).
-define(DC_NAMESPACE, <<"http://www.openarchives.org/OAI/2.0/oai_dc/">>).
-define(DC_SCHEMA, <<"http://www.openarchives.org/OAI/2.0/oai_dc.xsd">>).

-define(PROXY_ENDPOINT, <<"172.17.0.9:8080/api/v1">>).

-define(DOI_NAME, <<"LifeWatch DataCite">>).
-define(DOI_SERVICE_PROPERTIES, #{
    <<"type">> => <<"DOI">>,
    <<"host">> => <<"https://mds.test.datacite.org">>,
    <<"doiEndpoint">> => <<"/doi">>,
    <<"metadataEndpoint">> => <<"/metadata">>,
    <<"mediaEndpoint">> => <<"/media">>,
    <<"prefix">> => <<"10.5072">>,
    <<"username">> => <<"alice">>,
    <<"password">> => <<"*******">>,
    <<"identifierTemplate">> => <<"{{space.name}}-{{space.guid}}">>,
    <<"allowTemplateOverride">> => false
}).

-define(HANDLE_RESOURCE_TYPE, <<"Share">>).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    identify_get_test,
    identify_post_test,
    identify_change_earliest_datestamp_get_test,
    identify_change_earliest_datestamp_post_test,
    get_record_get_test,
    get_record_post_test,
    list_metadata_formats_get_test,
    list_metadata_formats_post_test,
    list_identifiers_get_test,
    list_identifiers_post_test,
    selective_list_identifiers1_get_test,
    selective_list_identifiers1_post_test,
    selective_list_identifiers2_get_test,
    selective_list_identifiers2_post_test,
    selective_list_identifiers3_get_test,
    selective_list_identifiers3_post_test,
    selective_list_identifiers4_get_test,
    selective_list_identifiers4_post_test,
    list_identifiers_modify_timestamp_get_test,
    list_identifiers_modify_timestamp_post_test,
    list_identifiers_modify_timestamp1_get_test,
    list_identifiers_modify_timestamp1_post_test,
    list_identifiers_modify_timestamp2_get_test,
    list_identifiers_modify_timestamp2_post_test,
    list_records_get_test,
    list_records_post_test,
    selective_list_records1_get_test,
    selective_list_records1_post_test,
    selective_list_records2_get_test,
    selective_list_records2_post_test,
    selective_list_records3_get_test,
    selective_list_records3_post_test,
    selective_list_records4_get_test,
    selective_list_records4_post_test,
    list_records_modify_timestamp_get_test,
    list_records_modify_timestamp_post_test,
    list_records_modify_timestamp1_get_test,
    list_records_modify_timestamp1_post_test,
    list_records_modify_timestamp2_get_test,
    list_records_modify_timestamp2_post_test,
    no_verb_get_test,
    no_verb_post_test,
    empty_verb_get_test,
    empty_verb_post_test,
    invalid_verb_get_test,
    invalid_verb_post_test,
    illegal_arg_get_test,
    illegal_arg_post_test,
    missing_arg_get_test,
    missing_arg_post_test,
    id_not_existing_get_test,
    id_not_existing_post_test,
    list_metadata_formats_no_format_error_get_test,
    list_metadata_formats_no_format_error_post_test,
    cannot_disseminate_format_get_test,
    cannot_disseminate_format_post_test,
    no_set_hierarchy_get_test,
    no_set_hierarchy_post_test,
    list_identifiers_empty_repository_error_get_test,
    list_identifiers_empty_repository_error_post_test,
    list_identifiers_no_records_match_error1_get_test,
    list_identifiers_no_records_match_error1_post_test,
    list_identifiers_no_records_match_error2_get_test,
    list_identifiers_no_records_match_error2_post_test,
    list_identifiers_granularity_mismatch_error_post_test,
    list_identifiers_granularity_mismatch_error_get_test,
    list_identifiers_wrong_date_format_error1_get_test,
    list_identifiers_wrong_date_format_error1_post_test,
    list_identifiers_wrong_date_format_error2_get_test,
    list_identifiers_wrong_date_format_error2_post_test,
    list_identifiers_wrong_date_format_error3_get_test,
    list_identifiers_wrong_date_format_error3_post_test,
    list_records_no_set_error_get_test,
    list_records_no_set_error_post_test
]).

%%%===================================================================
%%% Test functions
%%%===================================================================

identify_get_test(Config) ->
    identify_test_base(Config, get).

identify_post_test(Config) ->
    identify_test_base(Config, post).

identify_change_earliest_datestamp_get_test(Config) ->
    identify_change_earliest_datestamp_test_base(Config, get).

identify_change_earliest_datestamp_post_test(Config) ->
    identify_change_earliest_datestamp_test_base(Config, post).

get_record_get_test(Config) ->
    get_record_test_base(Config, get).

get_record_post_test(Config) ->
    get_record_test_base(Config, post).

list_metadata_formats_get_test(Config) ->
    list_metadata_formats_test_base(Config, get).

list_metadata_formats_post_test(Config) ->
    list_metadata_formats_test_base(Config, post).

list_identifiers_get_test(Config) ->
    list_identifiers_test_base(Config, get, 10, undefined, undefined).

list_identifiers_post_test(Config) ->
    list_identifiers_test_base(Config, post, 10, undefined, undefined).

selective_list_identifiers1_get_test(Config) ->
    list_identifiers_test_base(Config, get, 10, 0, 5).

selective_list_identifiers1_post_test(Config) ->
    list_identifiers_test_base(Config, post, 10, 0, 5).

selective_list_identifiers2_get_test(Config) ->
    list_identifiers_test_base(Config, get, 10, 0, 0).

selective_list_identifiers2_post_test(Config) ->
    list_identifiers_test_base(Config, post, 10, 0, 0).

selective_list_identifiers3_get_test(Config) ->
    list_identifiers_test_base(Config, get, 10, undefined, 7).

selective_list_identifiers3_post_test(Config) ->
    list_identifiers_test_base(Config, get, 10, undefined, 7).

selective_list_identifiers4_get_test(Config) ->
    list_identifiers_test_base(Config, get, 10, 3, undefined).

selective_list_identifiers4_post_test(Config) ->
    list_identifiers_test_base(Config, post, 10, 3, undefined).

list_identifiers_modify_timestamp_get_test(Config) ->
    list_identifiers_modify_timestamp_test_base(Config, get, 10, undefined, undefined, 2).

list_identifiers_modify_timestamp_post_test(Config) ->
    list_identifiers_modify_timestamp_test_base(Config, get, 10, undefined, undefined, 2).

list_identifiers_modify_timestamp1_get_test(Config) ->
    list_identifiers_modify_timestamp_test_base(Config, get, 10, 1, undefined, 6).

list_identifiers_modify_timestamp1_post_test(Config) ->
    list_identifiers_modify_timestamp_test_base(Config, get, 10, 1, undefined, 6).

list_identifiers_modify_timestamp2_get_test(Config) ->
    list_identifiers_modify_timestamp_test_base(Config, get, 10, undefined, 9, 9).

list_identifiers_modify_timestamp2_post_test(Config) ->
    list_identifiers_modify_timestamp_test_base(Config, get, 10, undefined, 9, 9).

list_records_get_test(Config) ->
    list_records_test_base(Config, get, 10, undefined, undefined).

list_records_post_test(Config) ->
    list_records_test_base(Config, post, 10, undefined, undefined).

selective_list_records1_get_test(Config) ->
    list_records_test_base(Config, get, 10, 1, 7).

selective_list_records1_post_test(Config) ->
    list_records_test_base(Config, post, 10, 1, 7).

selective_list_records2_get_test(Config) ->
    list_records_test_base(Config, get, 10, 9, 9).

selective_list_records2_post_test(Config) ->
    list_records_test_base(Config, post, 10, 9, 9).

selective_list_records3_get_test(Config) ->
    list_records_test_base(Config, get, 10, undefined, 2).

selective_list_records3_post_test(Config) ->
    list_records_test_base(Config, get, 10, undefined, 2).

selective_list_records4_get_test(Config) ->
    list_records_test_base(Config, get, 10, 9, undefined).

selective_list_records4_post_test(Config) ->
    list_records_test_base(Config, post, 10, 9, undefined).

list_records_modify_timestamp_get_test(Config) ->
    list_records_modify_timestamp_test_base(Config, get, 10, undefined, undefined, 10).

list_records_modify_timestamp_post_test(Config) ->
    list_records_modify_timestamp_test_base(Config, get, 10, undefined, undefined, 10).

list_records_modify_timestamp1_get_test(Config) ->
    list_records_modify_timestamp_test_base(Config, get, 10, 1, undefined, 7).

list_records_modify_timestamp1_post_test(Config) ->
    list_records_modify_timestamp_test_base(Config, get, 10, 1, undefined, 7).

list_records_modify_timestamp2_get_test(Config) ->
    list_records_modify_timestamp_test_base(Config, get, 10, undefined, 9, 8).

list_records_modify_timestamp2_post_test(Config) ->
    list_records_modify_timestamp_test_base(Config, get, 10, undefined, 9, 8).

%%% Tests of error handling

no_verb_get_test(Config) ->
    no_verb_test_base(Config, get).

no_verb_post_test(Config) ->
    no_verb_test_base(Config, post).

empty_verb_get_test(Config) ->
    empty_verb_test_base(Config, get).

empty_verb_post_test(Config) ->
    empty_verb_test_base(Config, post).

invalid_verb_get_test(Config) ->
    invalid_verb_test_base(Config, get).

invalid_verb_post_test(Config) ->
    invalid_verb_test_base(Config, post).

illegal_arg_get_test(Config) ->
    illegal_arg_test_base(Config, get).

illegal_arg_post_test(Config) ->
    illegal_arg_test_base(Config, post).

missing_arg_get_test(Config) ->
    missing_arg_test_base(Config, get).

missing_arg_post_test(Config) ->
    missing_arg_test_base(Config, post).

id_not_existing_get_test(Config) ->
    id_not_existing_test_base(Config, get).

id_not_existing_post_test(Config) ->
    id_not_existing_test_base(Config, post).

cannot_disseminate_format_get_test(Config) ->
    cannot_disseminate_format_test_base(Config, get).

cannot_disseminate_format_post_test(Config) ->
    cannot_disseminate_format_test_base(Config, post).

no_set_hierarchy_get_test(Config) ->
    no_set_hierarchy_test_base(Config, get).

no_set_hierarchy_post_test(Config) ->
    no_set_hierarchy_test_base(Config, post).

list_metadata_formats_no_format_error_get_test(Config) ->
    list_metadata_formats_no_format_error_test_base(Config, get).

list_metadata_formats_no_format_error_post_test(Config) ->
    list_metadata_formats_no_format_error_test_base(Config, post).

list_identifiers_empty_repository_error_get_test(Config) ->
    list_identifiers_empty_repository_error_test_base(Config, get).

list_identifiers_empty_repository_error_post_test(Config) ->
    list_identifiers_empty_repository_error_test_base(Config, post).

list_identifiers_no_records_match_error1_get_test(Config) ->
    list_identifiers_no_records_match_error_test_base(Config, get, 10, 11, 15).

list_identifiers_no_records_match_error1_post_test(Config) ->
    list_identifiers_no_records_match_error_test_base(Config, post, 10, 11, 15).

list_identifiers_no_records_match_error2_get_test(Config) ->
    list_identifiers_no_records_match_error_test_base(Config, get, 10, -11, -5).

list_identifiers_no_records_match_error2_post_test(Config) ->
    list_identifiers_no_records_match_error_test_base(Config, post, 10, -11, -5).

list_identifiers_granularity_mismatch_error_get_test(Config) ->
    list_identifiers_granularity_mismatch_error_test_base(Config, get).

list_identifiers_granularity_mismatch_error_post_test(Config) ->
    list_identifiers_granularity_mismatch_error_test_base(Config, post).

list_identifiers_wrong_date_format_error1_get_test(Config) ->
    list_identifiers_wrong_date_format_error_test_base(Config, get, <<"1111-01">>, undefined).

list_identifiers_wrong_date_format_error1_post_test(Config) ->
    list_identifiers_wrong_date_format_error_test_base(Config, post, <<"1111-01">>, undefined).

list_identifiers_wrong_date_format_error2_get_test(Config) ->
    list_identifiers_wrong_date_format_error_test_base(Config, get, undefined, <<"1111-01-25T00:01:25">>).

list_identifiers_wrong_date_format_error2_post_test(Config) ->
    list_identifiers_wrong_date_format_error_test_base(Config, post, undefined, <<"1111-01-25T00:01:25">>).

list_identifiers_wrong_date_format_error3_get_test(Config) ->
    list_identifiers_wrong_date_format_error_test_base(Config, get, undefined, <<"1111-13-25T65:01:25">>).

list_identifiers_wrong_date_format_error3_post_test(Config) ->
    list_identifiers_wrong_date_format_error_test_base(Config, post, undefined, <<"1111-13-25T65:01:25">>).

list_records_no_set_error_get_test(Config) ->
    list_records_no_set_error_test_base(Config, get).

list_records_no_set_error_post_test(Config) ->
    list_records_no_set_error_test_base(Config, post).

%%%===================================================================
%%% Test base functions
%%%===================================================================

identify_test_base(Config, Method) ->

    [Node | _] = ?config(oz_worker_nodes, Config),
    Path = ?config(oai_pmh_path, Config),
    ExpectedBaseURL = string:concat(get_domain(Node), binary_to_list(Path)),

    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Space1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    {ok, ?SHARE_ID} = oz_test_utils:create_share(
        Config, ?USER(User), ?SHARE_ID, ?SHARE_ID, <<"root">>, Space1
    ),
    HSId = create_handle_service(Config, User),
    Timestamp = erlang:universaltime(),
    create_handle_with_mocked_timestamp(Config, User, HSId, ?SHARE_ID,
        ?DC_METADATA_XML, Timestamp),

    ExpResponseContent = [
        #xmlElement{name = repositoryName, content = [#xmlText{value = "unnamed"}]},
        #xmlElement{name = baseURL, content = [#xmlText{value = ExpectedBaseURL}]},
        #xmlElement{name = protocolVersion, content = [#xmlText{value = "2.0"}]},
        #xmlElement{name = earliestDatestamp, content = [#xmlText{value = convert(Timestamp)}]},
        #xmlElement{name = deletedRecord, content = [#xmlText{value = "no"}]},
        #xmlElement{name = granularity, content = [#xmlText{value = "YYYY-MM-DDThh:mm:ssZ"}]},
        #xmlElement{name = adminEmail, content = [#xmlText{value = "info@onedata.org"}]}
    ],
    ?assert(check_identify(200, [], Method, ExpResponseContent, Config)).

identify_change_earliest_datestamp_test_base(Config, Method) ->

    [Node | _] = ?config(oz_worker_nodes, Config),
    Path = ?config(oai_pmh_path, Config),
    ExpectedBaseURL = string:concat(get_domain(Node), binary_to_list(Path)),

    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    SpaceIds = create_spaces(Config, ?SPACE_NAMES(2), ?USER(User)),
    [ShareId1, ShareId2] = create_shares(Config, SpaceIds),
    HSId = create_handle_service(Config, User),
    Timestamp1 = erlang:universaltime(),
    Timestamp2 = increase_timestamp(Timestamp1, 1),
    Timestamp3 = increase_timestamp(Timestamp2, 1),
    Identifier1 = create_handle_with_mocked_timestamp(Config, User, HSId, ShareId1,
        ?DC_METADATA_XML, Timestamp1),
    _Identifier2 = create_handle_with_mocked_timestamp(Config, User, HSId, ShareId2,
        ?DC_METADATA_XML, Timestamp2),

    ExpResponseContent = [
        #xmlElement{name = repositoryName, content = [#xmlText{value = "unnamed"}]},
        #xmlElement{name = baseURL, content = [#xmlText{value = ExpectedBaseURL}]},
        #xmlElement{name = protocolVersion, content = [#xmlText{value = "2.0"}]},
        #xmlElement{name = earliestDatestamp, content = [#xmlText{value = convert(Timestamp1)}]},
        #xmlElement{name = deletedRecord, content = [#xmlText{value = "no"}]},
        #xmlElement{name = granularity, content = [#xmlText{value = "YYYY-MM-DDThh:mm:ssZ"}]},
        #xmlElement{name = adminEmail, content = [#xmlText{value = "info@onedata.org"}]}
    ],
    ?assert(check_identify(200, [], Method, ExpResponseContent, Config)),

    modify_handle_with_mocked_timestamp(Config, Identifier1, ?DC_METADATA_XML, Timestamp3),

    ExpResponseContent2 = [
        #xmlElement{name = repositoryName, content = [#xmlText{value = "unnamed"}]},
        #xmlElement{name = baseURL, content = [#xmlText{value = ExpectedBaseURL}]},
        #xmlElement{name = protocolVersion, content = [#xmlText{value = "2.0"}]},
        #xmlElement{name = earliestDatestamp, content = [#xmlText{value = convert(Timestamp2)}]},
        #xmlElement{name = deletedRecord, content = [#xmlText{value = "no"}]},
        #xmlElement{name = granularity, content = [#xmlText{value = "YYYY-MM-DDThh:mm:ssZ"}]},
        #xmlElement{name = adminEmail, content = [#xmlText{value = "info@onedata.org"}]}
    ],
    ?assert(check_identify(200, [], Method, ExpResponseContent2, Config)).


get_record_test_base(Config, Method) ->

    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Space1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    {ok, ?SHARE_ID} = oz_test_utils:create_share(
        Config, ?USER(User), ?SHARE_ID, ?SHARE_ID, <<"root">>, Space1
    ),
    HSId = create_handle_service(Config, User),
    Timestamp = erlang:universaltime(),
    Identifier = create_handle_with_mocked_timestamp(Config, User, HSId, ?SHARE_ID,
        ?DC_METADATA_XML, Timestamp),

    {#xmlElement{content = DCMetadata}, _} = xmerl_scan:string(binary_to_list(?DC_METADATA_XML)),

    Args = [
        {<<"identifier">>, oai_identifier(Identifier)},
        {<<"metadataPrefix">>, ?DC_METADATA_PREFIX}
    ],

    ExpResponseContent = [
        #xmlElement{name = record, content = [
            #xmlElement{
                name = header,
                content = [
                    #xmlElement{
                        name = identifier,
                        content = [#xmlText{
                            value = binary_to_list(oai_identifier(Identifier))
                        }]
                    },
                    #xmlElement{
                        name = datestamp,
                        content = [#xmlText{
                            value = convert(Timestamp)
                        }]
                    }
                ]
            },
            #xmlElement{
                name = metadata,
                content = [
                    #xmlElement{
                        name = 'oai_dc:dc',
                        content = DCMetadata
                    }
                ]
            }
        ]}
    ],
    ?assert(check_get_record(200, Args, Method, ExpResponseContent, Config)).

list_metadata_formats_test_base(Config, Method) ->
    ExpResponseContent = [
        #xmlElement{
            name = metadataFormat,
            content = [
                #xmlElement{
                    name = metadataPrefix,
                    content = [#xmlText{value = binary_to_list(?DC_METADATA_PREFIX)}]
                },
                #xmlElement{
                    name = schema,
                    content = [#xmlText{value = binary_to_list(?DC_SCHEMA)}]
                },
                #xmlElement{
                    name = metadataNamespace,
                    content = [#xmlText{value = binary_to_list(?DC_NAMESPACE)}]
                }
            ]
        }
    ],

    ?assert(check_list_metadata_formats(200, [], Method, ExpResponseContent, Config)).

list_identifiers_test_base(Config, Method, IdentifiersNum, FromOffset, UntilOffset) ->

    BeginTime = erlang:universaltime(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each

    Identifiers =
        setup_test_for_harvesting(Config, IdentifiersNum, BeginTime, TimeOffsets,
            ?DC_METADATA_XML),

    From = convert(increase_timestamp(BeginTime, FromOffset)),
    Until = convert(increase_timestamp(BeginTime, UntilOffset)),
    Args = prepare_harvesting_args(?DC_METADATA_PREFIX, From, Until),

    IdsAndTimestamps =
        ids_and_timestamps_to_be_harvested(Identifiers, TimeOffsets, FromOffset, UntilOffset),

    ExpResponseContent = lists:map(fun({Id, TimeOffset}) ->
        #xmlElement{
            name = header,
            content = [
                #xmlElement{
                    name = identifier,
                    content = [#xmlText{
                        value = binary_to_list(oai_identifier(Id))
                    }]
                },
                #xmlElement{
                    name = datestamp,
                    content = [#xmlText{
                        value = convert(increase_timestamp(BeginTime, TimeOffset))
                    }]
                }
            ]
        }
    end, IdsAndTimestamps),

    ?assert(check_list_identifiers(200, Args, Method, ExpResponseContent, Config)).

list_identifiers_modify_timestamp_test_base(Config, Method, IdentifiersNum,
    FromOffset, UntilOffset, IdentifiersToBeModified) ->

    %% IdentifiersToBeModified is number of identifiers that will be modified
    %% so that their timestamps will be set to Until + 1 (if Until is undefined

    BeginTime = erlang:universaltime(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each

    Identifiers =
        setup_test_for_harvesting(Config, IdentifiersNum, BeginTime, TimeOffsets,
            ?DC_METADATA_XML),

    From = convert(increase_timestamp(BeginTime, FromOffset)),
    Until = convert(increase_timestamp(BeginTime, UntilOffset)),
    Args = prepare_harvesting_args(?DC_METADATA_PREFIX, From, Until),

    IdsAndTimestamps =
        ids_and_timestamps_to_be_harvested(Identifiers, TimeOffsets, FromOffset, UntilOffset),

    ExpResponseContent = lists:map(fun({Id, TimeOffset}) ->
        #xmlElement{
            name = header,
            content = [
                #xmlElement{
                    name = identifier,
                    content = [#xmlText{
                        value = binary_to_list(oai_identifier(Id))
                    }]
                },
                #xmlElement{
                    name = datestamp,
                    content = [#xmlText{
                        value = convert(increase_timestamp(BeginTime, TimeOffset))
                    }]
                }
            ]
        }
    end, IdsAndTimestamps),
    ?assert(check_list_identifiers(200, Args, Method, ExpResponseContent, Config)),

    TimeOffsets2 = lists:map(fun({T, N}) ->
        case N =< IdentifiersToBeModified of
            true -> exclude_offset_from_range(T, FromOffset, UntilOffset);
            _ -> T
        end
    end, lists:zip(TimeOffsets, lists:seq(1, length(TimeOffsets)))),

    Identifiers = modify_handles_with_mocked_timestamp(Config, Identifiers,
        BeginTime, TimeOffsets2, ?DC_METADATA_XML),

    IdsAndTimestamps2 =
        ids_and_timestamps_to_be_harvested(Identifiers, TimeOffsets2, FromOffset, UntilOffset),

    ExpResponseContent2 = lists:map(fun({Id, TimeOffset}) ->
        #xmlElement{
            name = header,
            content = [
                #xmlElement{
                    name = identifier,
                    content = [#xmlText{
                        value = binary_to_list(oai_identifier(Id))
                    }]
                },
                #xmlElement{
                    name = datestamp,
                    content = [#xmlText{
                        value = convert(increase_timestamp(BeginTime, TimeOffset))
                    }]
                }
            ]
        }
    end, IdsAndTimestamps2),
    ?assert(check_list_identifiers(200, Args, Method, ExpResponseContent2, Config)).


list_records_test_base(Config, Method, IdentifiersNum, FromOffset, UntilOffset) ->

    BeginTime = erlang:universaltime(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each

    Identifiers =
        setup_test_for_harvesting(Config, IdentifiersNum, BeginTime, TimeOffsets,
            ?DC_METADATA_XML),

    From = convert(increase_timestamp(BeginTime, FromOffset)),
    Until = convert(increase_timestamp(BeginTime, UntilOffset)),
    Args = prepare_harvesting_args(?DC_METADATA_PREFIX, From, Until),

    {#xmlElement{content = DCMetadata}, _} = xmerl_scan:string(binary_to_list(?DC_METADATA_XML)),

    IdsAndTimestamps =
        ids_and_timestamps_to_be_harvested(Identifiers, TimeOffsets, FromOffset, UntilOffset),

    ExpResponseContent = lists:map(fun({Id, TimeOffset}) ->
        #xmlElement{
            name = record,
            content = [
                #xmlElement{
                    name = header,
                    content = [
                        #xmlElement{
                            name = identifier,
                            content = [#xmlText{
                                value = binary_to_list(oai_identifier(Id))
                            }]
                        },
                        #xmlElement{
                            name = datestamp,
                            content = [#xmlText{
                                value = convert(increase_timestamp(BeginTime, TimeOffset))
                            }]
                        }
                    ]
                },
                #xmlElement{
                    name = metadata,
                    content = [
                        #xmlElement{
                            name = 'oai_dc:dc',
                            content = DCMetadata
                        }
                    ]
                }
            ]
        }
    end, IdsAndTimestamps),

    ?assert(check_list_records(200, Args, Method, ExpResponseContent, Config)).

list_records_modify_timestamp_test_base(Config, Method, IdentifiersNum,
    FromOffset, UntilOffset, IdentifiersToBeModified) ->

    %% IdentifiersToBeModified is number of identifiers that will be modified
    %% so that their timestamps will be set to Until + 1 (if Until is undefined

    BeginTime = erlang:universaltime(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each

    Identifiers =
        setup_test_for_harvesting(Config, IdentifiersNum, BeginTime, TimeOffsets,
            ?DC_METADATA_XML),

    From = convert(increase_timestamp(BeginTime, FromOffset)),
    Until = convert(increase_timestamp(BeginTime, UntilOffset)),
    Args = prepare_harvesting_args(?DC_METADATA_PREFIX, From, Until),

    {#xmlElement{content = DCMetadata}, _} = xmerl_scan:string(binary_to_list(?DC_METADATA_XML)),

    IdsAndTimestamps =
        ids_and_timestamps_to_be_harvested(Identifiers, TimeOffsets, FromOffset, UntilOffset),

    ExpResponseContent = lists:map(fun({Id, TimeOffset}) ->
        #xmlElement{
            name = record,
            content = [
                #xmlElement{
                    name = header,
                    content = [
                        #xmlElement{
                            name = identifier,
                            content = [#xmlText{
                                value = binary_to_list(oai_identifier(Id))
                            }]
                        },
                        #xmlElement{
                            name = datestamp,
                            content = [#xmlText{
                                value = convert(increase_timestamp(BeginTime, TimeOffset))
                            }]
                        }
                    ]
                },
                #xmlElement{
                    name = metadata,
                    content = [
                        #xmlElement{
                            name = 'oai_dc:dc',
                            content = DCMetadata
                        }
                    ]
                }
            ]
        }
    end, IdsAndTimestamps),
    ?assert(check_list_records(200, Args, Method, ExpResponseContent, Config)),

    TimeOffsets2 = lists:map(fun({T, N}) ->
        case N =< IdentifiersToBeModified of
            true -> exclude_offset_from_range(T, FromOffset, UntilOffset);
            _ -> T
        end
    end, lists:zip(TimeOffsets, lists:seq(1, length(TimeOffsets)))),

    Identifiers = modify_handles_with_mocked_timestamp(Config, Identifiers,
        BeginTime, TimeOffsets2, ?DC_METADATA_XML),

    IdsAndTimestamps2 =
        ids_and_timestamps_to_be_harvested(Identifiers, TimeOffsets2, FromOffset, UntilOffset),

    ExpResponseContent2 = lists:map(fun({Id, TimeOffset}) ->
        #xmlElement{
            name = record,
            content = [
                #xmlElement{
                    name = header,
                    content = [
                        #xmlElement{
                            name = identifier,
                            content = [#xmlText{
                                value = binary_to_list(oai_identifier(Id))
                            }]
                        },
                        #xmlElement{
                            name = datestamp,
                            content = [#xmlText{
                                value = convert(increase_timestamp(BeginTime, TimeOffset))
                            }]
                        }
                    ]
                },
                #xmlElement{
                    name = metadata,
                    content = [
                        #xmlElement{
                            name = 'oai_dc:dc',
                            content = DCMetadata
                        }
                    ]
                }
            ]
        }
    end, IdsAndTimestamps2),
    ?assert(check_list_records(200, Args, Method, ExpResponseContent2, Config)).


no_verb_test_base(Config, Method) ->
    ?assert(check_no_verb_error(200, [], Method, [], Config)).

empty_verb_test_base(Config, Method) ->
    ?assert(check_empty_verb_error(200, [], Method, [], Config)).

invalid_verb_test_base(Config, Method) ->
    ?assert(check_invalid_verb_error(200, [], Method, [], Config)).

illegal_arg_test_base(Config, Method) ->
    ?assert(check_illegal_arg_error(200, [{"k", "v"}], Method, [], Config)).

missing_arg_test_base(Config, Method) ->
    Args = [{<<"identifier">>, ?SHARE_ID}],
    %% will perform GetRecord, metadataPrefix is missing
    ?assert(check_missing_arg_error(200, Args, Method, [], Config)).

id_not_existing_test_base(Config, Method) ->

    Args = [
        {<<"identifier">>, ?SHARE_ID},
        {<<"metadataPrefix">>, ?DC_METADATA_PREFIX}
    ],
    ?assert(check_id_not_existing_error(200, Args, Method, [], Config)).

cannot_disseminate_format_test_base(Config, Method) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Space1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    {ok, ?SHARE_ID} = oz_test_utils:create_share(
        Config, ?USER(User), ?SHARE_ID, ?SHARE_ID, <<"root">>, Space1
    ),
    HSId = create_handle_service(Config, User),
    Identifier = create_handle(Config, User, HSId, ?SHARE_ID, ?DC_METADATA_XML),

    Args = [
        {<<"identifier">>, oai_identifier(Identifier)},
        {<<"metadataPrefix">>, <<"not_supported_format">>}
    ],
    ?assert(check_cannot_disseminate_format_error(200, Args, Method, [], Config)).

no_set_hierarchy_test_base(Config, Method) ->
    ?assert(check_no_set_hierarchy_error(200, [], Method, [], Config)).

list_metadata_formats_no_format_error_test_base(Config, Method) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    {ok, Space1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    {ok, ?SHARE_ID} = oz_test_utils:create_share(
        Config, ?USER(User), ?SHARE_ID, ?SHARE_ID, <<"root">>, Space1
    ),
    HSId = create_handle_service(Config, User),
    Identifier = create_handle(Config, User, HSId, ?SHARE_ID, ?DC_METADATA_XML),
    % Modify handle metadata to undefined (this should not occur in normal
    % conditions because entity logic won't accept undefined metadata,
    % but check if returned OAI error in such case is correct).
    ?assertMatch({ok, _}, oz_test_utils:call_oz(Config, od_handle, update, [
        Identifier, fun(Handle = #od_handle{}) ->
            {ok, Handle#od_handle{metadata = undefined}}
        end]
    )),

    Args = [{<<"identifier">>, oai_identifier(Identifier)}],

    ?assert(check_list_metadata_formats_error(200, Args, Method, [], Config)).

list_identifiers_empty_repository_error_test_base(Config, Method) ->
    Args = [{<<"metadataPrefix">>, ?DC_METADATA_PREFIX}],
    ?assert(check_list_identifiers_no_records_match_error(200, Args, Method, [], Config)).

list_identifiers_no_records_match_error_test_base(Config, Method, IdentifiersNum, FromOffset, UntilOffset) ->

    BeginTime = erlang:universaltime(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each

    setup_test_for_harvesting(Config, IdentifiersNum, BeginTime,
        TimeOffsets, ?DC_METADATA_XML),

    From = convert(increase_timestamp(BeginTime, FromOffset)),
    Until = convert(increase_timestamp(BeginTime, UntilOffset)),
    Args = prepare_harvesting_args(?DC_METADATA_PREFIX, From, Until),

    ?assert(check_list_identifiers_no_records_match_error(200, Args, Method, [], Config)).

list_identifiers_granularity_mismatch_error_test_base(Config, Method) ->

    Args = [
        {<<"metadataPrefix">>, <<"oai_dc">>},
        {<<"from">>, <<"1111-12-01">>},
        {<<"until">>, <<"1111-12-01T00:00:00Z">>}
    ],
    ?assert(check_list_identifiers_bad_argument_error(200, Args, Method, [], Config)).

list_identifiers_wrong_date_format_error_test_base(Config, Method, From, Until) ->

    Args = prepare_harvesting_args(?DC_METADATA_PREFIX, From, Until),
    ?assert(check_list_identifiers_bad_argument_error(200, Args, Method, [], Config)).

list_records_no_set_error_test_base(Config, Method) ->
    Args = prepare_harvesting_args(?DC_METADATA_PREFIX, undefined, undefined, <<"some_set">>),
    ?assert(check_list_records_no_set_error(200, Args, Method, [], Config)).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) ->
        [Node1 | _] = ?config(oz_worker_nodes, NewConfig),
        [
            {oai_pmh_url, get_oai_pmh_URL(Node1)},
            {oai_pmh_path, get_oai_pmh_api_path(Node1)} | NewConfig
        ]
    end,
    [
        {?ENV_UP_POSTHOOK, Posthook},
        {?LOAD_MODULES, [oz_test_utils]} | Config
    ].

init_per_testcase(_, Config) ->
    mock_handle_proxy(Config),
    Config.

end_per_testcase(_, Config) ->
    oz_test_utils:delete_all_entities(Config),
    unmock_handle_proxy(Config),
    ok.

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().

%%%===================================================================
%%% Functions used to validate REST calls
%%%=================================================================

check_identify(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"Identify">>, Args, Method, ExpResponseContent,
        'Identify', Config).

check_get_record(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"GetRecord">>, Args, Method, ExpResponseContent,
        'GetRecord', Config).

check_no_verb_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, none, Args, Method, ExpResponseContent,
        {error, badVerb}, Config).

check_empty_verb_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"">>, Args, Method, ExpResponseContent,
        {error, badVerb}, Config).

check_invalid_verb_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"invalid_verb">>, Args, Method, ExpResponseContent,
        {error, badVerb}, Config).

check_illegal_arg_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"Identify">>, Args, Method, ExpResponseContent,
        {error, badArgument}, Config).

check_missing_arg_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"GetRecord">>, Args, Method, ExpResponseContent,
        {error, badArgument}, Config).

check_id_not_existing_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"GetRecord">>, Args, Method, ExpResponseContent,
        {error, idDoesNotExist}, Config).

check_cannot_disseminate_format_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"GetRecord">>, Args, Method, ExpResponseContent,
        {error, cannotDisseminateFormat}, Config).

check_no_set_hierarchy_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListSets">>, Args, Method, ExpResponseContent,
        {error, noSetHierarchy}, Config).

check_list_metadata_formats(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListMetadataFormats">>, Args, Method,
        ExpResponseContent, 'ListMetadataFormats', Config).

check_list_metadata_formats_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListMetadataFormats">>, Args, Method,
        ExpResponseContent, {error, noMetadataFormats}, Config).

check_list_identifiers(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListIdentifiers">>, Args, Method,
        ExpResponseContent, 'ListIdentifiers', Config).

check_list_identifiers_no_records_match_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListIdentifiers">>, Args, Method,
        ExpResponseContent, {error, noRecordsMatch}, Config).

check_list_identifiers_bad_argument_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListIdentifiers">>, Args, Method,
        ExpResponseContent, {error, badArgument}, Config).

check_list_records(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListRecords">>, Args, Method,
        ExpResponseContent, 'ListRecords', Config).

check_list_records_no_set_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListRecords">>, Args, Method,
        ExpResponseContent, {error, noSetHierarchy}, Config).

check_oai_request(Code, Verb, Args, Method, ExpResponseContent, ResponseType, Config) ->

    URL = ?config(oai_pmh_url, Config),
    Path = ?config(oai_pmh_path, Config),
    Args2 = case Verb of
        none -> Args;
        _ -> add_verb(Verb, Args)
    end,
    ResponseDate = erlang:universaltime(),
    ExpectedBody = expected_body(Config, ExpResponseContent, ResponseType, Args2, ResponseDate),
    QueryString = prepare_querystring(Args2),
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_new(Nodes, oai_handler, [passthrough]),
    test_utils:mock_expect(Nodes, oai_handler, generate_response_date_element,
        fun() ->
            {responseDate, list_to_binary(convert(ResponseDate))}
        end
    ),

    Request = case Method of
        get -> #{
            method => get,
            url => URL,
            path => str_utils:format_bin("~s?~s", [Path, QueryString])
        };
        post -> #{
            method => post,
            url => URL,
            path => Path,
            body => QueryString,
            headers => ?CONTENT_TYPE_HEADER
        }
    end,

    Check = rest_test_utils:check_rest_call(Config, #{
        request => Request,
        expect => #{
            code => Code,
            body => ExpectedBody,
            headers => {contains, ?RESPONSE_CONTENT_TYPE_HEADER}
        }
    }),
    ok = test_utils:mock_validate_and_unload(Nodes, oai_handler),
    Check.


%%%===================================================================
%%% Internal functions
%%%===================================================================

get_oai_pmh_URL(Node) ->
    OZ_IP_1 = test_utils:get_docker_ip(Node),
    str_utils:format_bin("http://~s", [OZ_IP_1]).

get_oai_pmh_api_path(Node) ->
    {ok, OAIPrefix} = rpc:call(Node, oz_worker, get_env, [oai_pmh_api_prefix]),
    list_to_binary(OAIPrefix).

prepare_querystring(Proplist) ->
    QS = lists:foldl(fun({K, V}, Acc) ->
        KBin = str_utils:to_binary(K),
        VBin = str_utils:to_binary(V),
        <<Acc/binary, KBin/binary, "=", VBin/binary, "&">>
    end, <<"">>, Proplist),

    case QS of
        <<"">> -> <<"">>;
        _ -> binary_part(QS, 0, size(QS) - 1)
    end.

add_verb(Verb, Args) ->
    add_to_args(<<"verb">>, Verb, Args).

add_to_args(_Key, undefined, Args) ->
    Args;
add_to_args(Key, Value, Args) ->
    [{str_utils:to_binary(Key), str_utils:to_binary(Value)} | Args].

get_domain(Hostname) ->
    [_Node | Domain] = string:tokens(atom_to_list(Hostname), "."),
    string:join(Domain, ".").

expected_body(Config, ExpectedResponse, ResponseType, Args, ResponseDate) ->
    Path = ?config(oai_pmh_path, Config),
    URL = ?config(oai_pmh_url, Config),
    RequestURL = binary_to_list(<<URL/binary, Path/binary>>),

    ExpectedResponseElement = case ResponseType of
        {error, Code} -> expected_response_error(Code);
        Verb -> expected_response_verb(Verb, ExpectedResponse)
    end,

    ExpectedRequestElement = case ResponseType of
        %% when error is badVerb or badArgument request element
        %% should only contain request URL
        {error, badVerb} -> expected_request_element(RequestURL);
        {error, badArgument} -> expected_request_element(RequestURL);
        _ -> expected_request_element(RequestURL, Args)
    end,

    #xmlElement{
        name = 'OAI-PMH',
        attributes = [
            ?OAI_XML_NAMESPACE,
            ?OAI_XML_SCHEMA_NAMESPACE,
            ?OAI_XSI_SCHEMA_LOCATION],
        content = [
            #xmlElement{
                name = responseDate,
                content = [#xmlText{value = convert(ResponseDate)}]
            },
            ExpectedRequestElement,
            ExpectedResponseElement]
    }.

expected_response_error(Code) ->
    #xmlElement{
        name = error,
        attributes = [#xmlAttribute{name = code, value = str_utils:to_list(Code)}]
    }.

expected_response_verb(Verb, {Content, Attributes}) ->
    #xmlElement{name = ensure_atom(Verb), attributes = Attributes, content = Content};
expected_response_verb(Verb, Content) ->
    #xmlElement{name = ensure_atom(Verb), content = Content}.

expected_request_element(RequestURL) ->
    expected_request_element(RequestURL, []).

expected_request_element(RequestURL, Args) ->
    Attributes = lists:map(fun({K, V}) ->
        #xmlAttribute{name = binary_to_atom(K, latin1), value = str_utils:to_list(V)}
    end, Args),
    #xmlElement{
        name = request,
        attributes = Attributes,
        content = [#xmlText{value = RequestURL}]
    }.

ensure_atom(Arg) when is_atom(Arg) -> Arg;
ensure_atom(Arg) when is_binary(Arg) -> binary_to_atom(Arg, latin1);
ensure_atom(Arg) when is_list(Arg) -> list_to_atom(Arg).

create_spaces(Config, SpacesNames, Client) ->
    Result = lists:map(fun(SpaceName) ->
        {ok, SpaceId} = oz_test_utils:create_space(Config, Client, SpaceName),
        SpaceId
    end, SpacesNames),
    Result.

create_shares(Config, SpaceIds) ->
    ShareIds = ?SHARE_IDS(length(SpaceIds)),
    lists:map(fun({ShareId, SpaceId}) ->
        {ok, ShareId} = oz_test_utils:create_share(
            Config, ?ROOT, ShareId, ShareId, <<"root_file_id">>, SpaceId
        ),
        ShareId
    end, lists:zip(ShareIds, SpaceIds)).

modify_handles_with_mocked_timestamp(Config, Identifiers, BeginTime, TimeOffsets,
    Metadata) ->

    lists:map(fun({Id, TimeOffset}) ->
        MockedTimestamp = increase_timestamp(BeginTime, TimeOffset),
        ok = modify_handle_with_mocked_timestamp(Config, Id, Metadata, MockedTimestamp),
        Id
    end, lists:zip(Identifiers, TimeOffsets)).

modify_handle_with_mocked_timestamp(Config, HId, Metadata, Timestamp) ->

    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, od_handle, [passthrough]),
    ok = test_utils:mock_expect(Nodes, od_handle, actual_timestamp, fun() ->
        Timestamp end),
    ok = modify_handle(Config, HId, Metadata),
    ok = test_utils:mock_validate_and_unload(Nodes, od_handle).

setup_test_for_harvesting(Config, RecordsNum, BeginTime, TimeOffsets, Metadata) ->
    {ok, User} = oz_test_utils:create_user(Config, #od_user{}),
    SpaceIds = create_spaces(Config, ?SPACE_NAMES(RecordsNum), ?USER(User)),
    ShareIds = create_shares(Config, SpaceIds),
    HSId = create_handle_service(Config, User),
    create_handles_with_mocked_timestamps(Config, User, HSId, ShareIds, BeginTime,
        TimeOffsets, Metadata).

%% for each SpaceId in SpaceIds creates share,
%% adds metadata to this share and mock timestamp
create_handles_with_mocked_timestamps(Config, User, HSId, ResourceIds, BeginTime,
    TimeOffsets, Metadata) ->

    lists:map(fun({ResourceId, TimeOffset}) ->
        MockedTimestamp = increase_timestamp(BeginTime, TimeOffset),
        create_handle_with_mocked_timestamp(Config, User, HSId, ResourceId, Metadata, MockedTimestamp)
    end, lists:zip(ResourceIds, TimeOffsets)).

create_handle_service(Config, User) ->
    ok = oz_test_utils:user_set_oz_privileges(Config, User, grant, [
        ?OZ_HANDLE_SERVICES_CREATE
    ]),
    {ok, HSId} = oz_test_utils:create_handle_service(Config, ?USER(User),
        ?DOI_NAME, ?PROXY_ENDPOINT, ?DOI_SERVICE_PROPERTIES
    ),
    HSId.

create_handle_with_mocked_timestamp(Config, User, HandleServiceId, ResourceId, Metadata, Timestamp) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, od_handle, [passthrough]),
    ok = test_utils:mock_expect(Nodes, od_handle, actual_timestamp, fun() ->
        Timestamp end),
    HId = create_handle(Config, User, HandleServiceId, ResourceId, Metadata),
    ok = test_utils:mock_validate_and_unload(Nodes, od_handle),
    HId.

create_handle(Config, User, HandleServiceId, ResourceId, Metadata) ->
    {ok, HId} = oz_test_utils:create_handle(Config, ?USER(User),
        HandleServiceId, ?HANDLE_RESOURCE_TYPE, ResourceId, Metadata
    ),
    HId.

modify_handle(Config, HandleId, Metadata) ->
    ok = oz_test_utils:update_handle(Config, HandleId, #{
        <<"metadata">> => Metadata
    }).

mock_handle_proxy(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),

    ok = test_utils:mock_new(Nodes, handle_proxy_client),
    ok = test_utils:mock_expect(Nodes, handle_proxy_client, put,
        fun(?PROXY_ENDPOINT, <<"/handle", _/binary>>, _, _) ->
            {ok, 201, [{<<"location">>, <<"/test_location">>}], <<"">>}
        end),
    ok = test_utils:mock_expect(Nodes, handle_proxy_client, patch,
        fun(?PROXY_ENDPOINT, <<"/handle", _/binary>>, _, _) ->
            {ok, 204, [], <<"">>}
        end),
    ok = test_utils:mock_expect(Nodes, handle_proxy_client, delete,
        fun(?PROXY_ENDPOINT, <<"/handle", _/binary>>, _, _) ->
            {ok, 200, [], <<"">>}
        end).

unmock_handle_proxy(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, handle_proxy_client).

prepare_harvesting_args(MetadataPrefix, From, Until) ->
    prepare_harvesting_args(MetadataPrefix, From, Until, undefined).

prepare_harvesting_args(MetadataPrefix, From, Until, Set) ->
    Args = add_to_args(<<"metadataPrefix">>, MetadataPrefix, []),
    Args2 = add_to_args(<<"from">>, From, Args),
    Args3 = add_to_args(<<"until">>, Until, Args2),
    add_to_args(<<"set">>, Set, Args3).

ids_and_timestamps_to_be_harvested(Identifiers, TimeOffsets, FromOffset, UntilOffset) ->
    lists:filter(fun({_Id, TimeOffset}) ->
        offset_in_range(FromOffset, UntilOffset, TimeOffset)
    end, lists:zip(Identifiers, TimeOffsets)).

increase_timestamp(_, undefined) -> undefined;
increase_timestamp(undefined, _) -> undefined;
increase_timestamp(Datetime, ExtraSeconds) ->
    Seconds = calendar:datetime_to_gregorian_seconds(Datetime),
    calendar:gregorian_seconds_to_datetime(Seconds + ExtraSeconds).

convert(undefined) -> undefined;
convert(DateTime) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    str_utils:format(
        "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
        [Year, Month, Day, Hour, Minute, Second]).

offset_in_range(undefined, undefined, _) -> true;
offset_in_range(undefined, Until, Offset) ->
    Offset =< Until;
offset_in_range(From, undefined, Offset) ->
    From =< Offset;
offset_in_range(From, Until, Offset) ->
    (From =< Offset) and (Offset =< Until).


oai_identifier(HandleId) ->
    <<"oai:onedata.org:", HandleId/binary>>.

exclude_offset_from_range(Offset, undefined, undefined) -> Offset;
exclude_offset_from_range(_Offset, From, undefined) -> From - rand:uniform(100);
exclude_offset_from_range(_Offset, undefined, Until) ->
    Until + rand:uniform(100);
exclude_offset_from_range(Offset, From, Until) ->
    Offset + random_out_of_range(From, Until, 100).

random_out_of_range(Lower, Upper, Max) ->
    Number = rand:uniform(Max) * (Upper + Lower - 1),
    case rand:uniform(2) rem 2 of
        0 -> -1 * Number;
        _ -> Number
    end.
