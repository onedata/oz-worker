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
-include_lib("ctool/include/http/headers.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("registered_names.hrl").
-include("http/handlers/oai.hrl").


%% API
-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2, end_per_suite/1]).

%% Test functions
-export([
    identify_get_test/1,
    identify_post_test/1,
    identify_change_earliest_datestamp_get_test/1,
    identify_change_earliest_datestamp_post_test/1,

    get_record_get_test/1,
    get_record_post_test/1,
    get_dc_record_with_bad_metadata_get_test/1,
    get_dc_record_with_bad_metadata_post_test/1,
    get_deleted_record_get_test/1,
    get_deleted_record_post_test/1,

    list_metadata_formats_get_test/1,
    list_metadata_formats_post_test/1,

    list_identifiers_get_test/1,
    list_identifiers_post_test/1,
    list_identifiers_resumption_token_get_test/1,
    list_identifiers_resumption_token_post_test/1,
    selective_list_identifiers1_get_test/1,
    selective_list_identifiers1_post_test/1,
    selective_list_identifiers2_get_test/1,
    selective_list_identifiers2_post_test/1,
    selective_list_identifiers3_get_test/1,
    selective_list_identifiers3_post_test/1,
    selective_list_identifiers4_get_test/1,
    selective_list_identifiers4_post_test/1,
    list_identifiers_modify_timestamp_get_test/1,
    list_identifiers_modify_timestamp_post_test/1,
    list_identifiers_modify_timestamp1_get_test/1,
    list_identifiers_modify_timestamp1_post_test/1,
    list_identifiers_modify_timestamp2_get_test/1,
    list_identifiers_modify_timestamp2_post_test/1,

    list_records_get_test/1,
    list_records_post_test/1,
    list_records_resumption_token_get_test/1,
    list_records_resumption_token_post_test/1,
    selective_list_records1_get_test/1,
    selective_list_records1_post_test/1,
    selective_list_records2_get_test/1,
    selective_list_records2_post_test/1,
    selective_list_records3_get_test/1,
    selective_list_records3_post_test/1,
    selective_list_records4_get_test/1,
    selective_list_records4_post_test/1,
    list_records_modify_timestamp_get_test/1,
    list_records_modify_timestamp_post_test/1,
    list_records_modify_timestamp1_get_test/1,
    list_records_modify_timestamp1_post_test/1,
    list_records_modify_timestamp2_get_test/1,
    list_records_modify_timestamp2_post_test/1,

    list_sets_get_test/1,
    list_sets_post_test/1,
    list_sets_empty_repository_get_test/1,
    list_sets_empty_repository_post_test/1,

    no_verb_get_test/1,
    no_verb_post_test/1,
    empty_verb_get_test/1,
    empty_verb_post_test/1,
    invalid_verb_get_test/1,
    invalid_verb_post_test/1,
    illegal_arg_get_test/1,
    illegal_arg_post_test/1,
    missing_arg_get_test/1,
    missing_arg_post_test/1,
    id_not_existing_get_test/1,
    id_not_existing_post_test/1,
    cannot_disseminate_format_get_test/1,
    cannot_disseminate_format_post_test/1,
    exclusive_resumption_token_required_get_test/1,
    exclusive_resumption_token_required_post_test/1,
    list_metadata_formats_error_get_test/1,
    list_metadata_formats_error_post_test/1,
    list_identifiers_empty_repository_error_get_test/1,
    list_identifiers_empty_repository_error_post_test/1,
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

    list_records_no_records_match_error1_get_test/1,
    list_records_no_records_match_error1_post_test/1,
    list_records_no_records_match_error2_get_test/1,
    list_records_no_records_match_error2_post_test/1,

    timestamp_evolution_and_harvesting_get_test/1,
    timestamp_evolution_and_harvesting_post_test/1,

    filtering_of_broken_records_get_test/1,
    filtering_of_broken_records_post_test/1
]).

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
    get_dc_record_with_bad_metadata_get_test,
    get_dc_record_with_bad_metadata_post_test,
    get_deleted_record_get_test,
    get_deleted_record_post_test,

    list_metadata_formats_get_test,
    list_metadata_formats_post_test,
    list_identifiers_get_test,
    list_identifiers_post_test,

    list_identifiers_resumption_token_get_test,
    list_identifiers_resumption_token_post_test,

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
    list_records_resumption_token_get_test,
    list_records_resumption_token_post_test,

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

    list_sets_get_test,
    list_sets_post_test,
    list_sets_empty_repository_get_test,
    list_sets_empty_repository_post_test,

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
    cannot_disseminate_format_get_test,
    cannot_disseminate_format_post_test,
    exclusive_resumption_token_required_get_test,
    exclusive_resumption_token_required_post_test,
    list_metadata_formats_error_get_test,
    list_metadata_formats_error_post_test,
    list_identifiers_empty_repository_error_get_test,
    list_identifiers_empty_repository_error_post_test,
    list_identifiers_no_records_match_error1_get_test,
    list_identifiers_no_records_match_error1_post_test,
    list_identifiers_no_records_match_error2_get_test,
    list_identifiers_no_records_match_error2_post_test,
    list_identifiers_granularity_mismatch_error_get_test,
    list_identifiers_granularity_mismatch_error_post_test,
    list_identifiers_wrong_date_format_error1_get_test,
    list_identifiers_wrong_date_format_error1_post_test,
    list_identifiers_wrong_date_format_error2_get_test,
    list_identifiers_wrong_date_format_error2_post_test,
    list_identifiers_wrong_date_format_error3_get_test,
    list_identifiers_wrong_date_format_error3_post_test,

    list_records_no_records_match_error1_get_test,
    list_records_no_records_match_error1_post_test,
    list_records_no_records_match_error2_get_test,
    list_records_no_records_match_error2_post_test,

    timestamp_evolution_and_harvesting_get_test,
    timestamp_evolution_and_harvesting_post_test,

    filtering_of_broken_records_get_test,
    filtering_of_broken_records_post_test
]).

%% useful macros
-define(CONTENT_TYPE_HEADER,
    #{?HDR_CONTENT_TYPE => <<"application/x-www-form-urlencoded">>}).
-define(RESPONSE_CONTENT_TYPE_HEADER,
    #{?HDR_CONTENT_TYPE => <<"text/xml">>}).

%% Example test data
-define(RAND_METADATA_PREFIX(), ?RAND_ELEMENT(ozt_handles:supported_metadata_prefixes())).
-define(NOW(), ozt_mocks:get_frozen_time_seconds()).

-define(TESTED_LIST_BATCH_SIZE, 10).

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

get_dc_record_with_bad_metadata_get_test(Config) ->
    get_dc_record_with_bad_metadata_test_base(Config, get).

get_dc_record_with_bad_metadata_post_test(Config) ->
    get_dc_record_with_bad_metadata_test_base(Config, post).

get_deleted_record_get_test(Config) ->
    get_deleted_record_test_base(Config, get).

get_deleted_record_post_test(Config) ->
    get_deleted_record_test_base(Config, post).

list_metadata_formats_get_test(Config) ->
    list_metadata_formats_test_base(Config, get).

list_metadata_formats_post_test(Config) ->
    list_metadata_formats_test_base(Config, post).

list_identifiers_get_test(Config) ->
    list_identifiers_test_base(Config, get, 10, undefined, undefined).

list_identifiers_post_test(Config) ->
    list_identifiers_test_base(Config, post, 10, undefined, undefined).

list_identifiers_resumption_token_get_test(Config) ->
    list_resumption_token_test_base(Config, get, <<"ListIdentifiers">>, (?TESTED_LIST_BATCH_SIZE * 5) div 2).

list_identifiers_resumption_token_post_test(Config) ->
    list_resumption_token_test_base(Config, post, <<"ListIdentifiers">>, (?TESTED_LIST_BATCH_SIZE * 5) div 2).

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

list_records_resumption_token_get_test(Config) ->
    list_resumption_token_test_base(Config, get, <<"ListRecords">>, (?TESTED_LIST_BATCH_SIZE * 5) div 2).

list_records_resumption_token_post_test(Config) ->
    list_resumption_token_test_base(Config, post, <<"ListRecords">>, (?TESTED_LIST_BATCH_SIZE * 5) div 2).

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

list_sets_get_test(Config) ->
    list_sets_test_base(Config, get).

list_sets_post_test(Config) ->
    list_sets_test_base(Config, post).

list_sets_empty_repository_get_test(Config) ->
    list_sets_empty_repository_test_base(Config, get).

list_sets_empty_repository_post_test(Config) ->
    list_sets_empty_repository_test_base(Config, post).

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

exclusive_resumption_token_required_get_test(Config) ->
    exclusive_resumption_token_required_error(Config, get).

exclusive_resumption_token_required_post_test(Config) ->
    exclusive_resumption_token_required_error(Config, post).

list_metadata_formats_error_get_test(Config) ->
    list_metadata_formats_error_test_base(Config, get).

list_metadata_formats_error_post_test(Config) ->
    list_metadata_formats_error_test_base(Config, post).

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
    list_identifiers_wrong_date_format_error_test_base(Config, get, <<"1111-01">>, undefined, <<"from">>).

list_identifiers_wrong_date_format_error1_post_test(Config) ->
    list_identifiers_wrong_date_format_error_test_base(Config, post, <<"1111-01">>, undefined, <<"from">>).

list_identifiers_wrong_date_format_error2_get_test(Config) ->
    list_identifiers_wrong_date_format_error_test_base(Config, get, undefined, <<"1111-01-25T00:01:25">>, <<"until">>).

list_identifiers_wrong_date_format_error2_post_test(Config) ->
    list_identifiers_wrong_date_format_error_test_base(Config, post, undefined, <<"1111-01-25T00:01:25">>, <<"until">>).

list_identifiers_wrong_date_format_error3_get_test(Config) ->
    list_identifiers_wrong_date_format_error_test_base(Config, get, undefined, <<"1111-13-25T65:01:25">>, <<"until">>).

list_identifiers_wrong_date_format_error3_post_test(Config) ->
    list_identifiers_wrong_date_format_error_test_base(Config, post, undefined, <<"1111-13-25T65:01:25">>, <<"until">>).

list_records_no_records_match_error1_get_test(Config) ->
    list_records_no_records_match_error_test_base(Config, get, 10, 11, 15).

list_records_no_records_match_error1_post_test(Config) ->
    list_records_no_records_match_error_test_base(Config, post, 10, 11, 15).

list_records_no_records_match_error2_get_test(Config) ->
    list_records_no_records_match_error_test_base(Config, get, 10, -11, -5).

list_records_no_records_match_error2_post_test(Config) ->
    list_records_no_records_match_error_test_base(Config, post, 10, -11, -5).

timestamp_evolution_and_harvesting_get_test(Config) ->
    timestamp_evolution_and_harvesting_test_base(Config, get).

timestamp_evolution_and_harvesting_post_test(Config) ->
    timestamp_evolution_and_harvesting_test_base(Config, post).

filtering_of_broken_records_get_test(Config) ->
    filtering_of_broken_records_test_base(Config, get).

filtering_of_broken_records_post_test(Config) ->
    filtering_of_broken_records_test_base(Config, post).

%%%===================================================================
%%% Test base functions
%%%===================================================================

identify_test_base(Config, Method) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    Path = get_oai_pmh_api_path(),
    ExpectedBaseURL = string:concat(get_domain(Node), binary_to_list(Path)),

    #handle_listing_entry{timestamp = Timestamp} = create_handle(),
    ExpResponseContent = [
        #xmlElement{name = repositoryName, content = [#xmlText{value = "undefined"}]},
        #xmlElement{name = baseURL, content = [#xmlText{value = ExpectedBaseURL}]},
        #xmlElement{name = protocolVersion, content = [#xmlText{value = "2.0"}]}
    ] ++ [
        #xmlElement{name = adminEmail, content = [#xmlText{value = Email}]} || Email <- expected_admin_emails()
    ] ++ [
        #xmlElement{name = earliestDatestamp, content = [#xmlText{value = to_datestamp(Timestamp)}]},
        #xmlElement{name = deletedRecord, content = [#xmlText{value = "persistent"}]},
        #xmlElement{name = granularity, content = [#xmlText{value = "YYYY-MM-DDThh:mm:ssZ"}]}
    ],
    ?assert(check_identify(200, [], Method, ExpResponseContent, Config)).


identify_change_earliest_datestamp_test_base(Config, Method) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    Path = get_oai_pmh_api_path(),
    ExpectedBaseURL = string:concat(get_domain(Node), binary_to_list(Path)),

    Timestamp1 = ?NOW(),
    % for an empty repository, the time an hour before now should be returned
    EmptyRepositoryTimestamp = decrease_timestamp(Timestamp1, 3600),
    Timestamp2 = increase_timestamp(Timestamp1, 1),
    Timestamp3 = increase_timestamp(Timestamp2, 1),

    % identify earliest datestamp with empty repository
    ExpResponseContentEmpty = [
        #xmlElement{name = repositoryName, content = [#xmlText{value = "undefined"}]},
        #xmlElement{name = baseURL, content = [#xmlText{value = ExpectedBaseURL}]},
        #xmlElement{name = protocolVersion, content = [#xmlText{value = "2.0"}]}
    ] ++ [
        #xmlElement{name = adminEmail, content = [#xmlText{value = Email}]} || Email <- expected_admin_emails()
    ] ++ [
        #xmlElement{name = earliestDatestamp, content = [#xmlText{value = to_datestamp(EmptyRepositoryTimestamp)}]},
        #xmlElement{name = deletedRecord, content = [#xmlText{value = "persistent"}]},
        #xmlElement{name = granularity, content = [#xmlText{value = "YYYY-MM-DDThh:mm:ssZ"}]}
    ],
    ?assert(check_identify(200, [], Method, ExpResponseContentEmpty, Config)),

    HandleEntry = create_handle_with_mocked_timestamp(Timestamp1),
    create_handle_with_mocked_timestamp(Timestamp2),

    ExpResponseContent1 = [
        #xmlElement{name = repositoryName, content = [#xmlText{value = "undefined"}]},
        #xmlElement{name = baseURL, content = [#xmlText{value = ExpectedBaseURL}]},
        #xmlElement{name = protocolVersion, content = [#xmlText{value = "2.0"}]}
    ] ++ [
        #xmlElement{name = adminEmail, content = [#xmlText{value = Email}]} || Email <- expected_admin_emails()
    ] ++ [
        #xmlElement{name = earliestDatestamp, content = [#xmlText{value = to_datestamp(Timestamp1)}]},
        #xmlElement{name = deletedRecord, content = [#xmlText{value = "persistent"}]},
        #xmlElement{name = granularity, content = [#xmlText{value = "YYYY-MM-DDThh:mm:ssZ"}]}
    ],
    ?assert(check_identify(200, [], Method, ExpResponseContent1, Config)),

    modify_handle_with_mocked_timestamp(HandleEntry, Timestamp3),

    ExpResponseContent2 = [
        #xmlElement{name = repositoryName, content = [#xmlText{value = "undefined"}]},
        #xmlElement{name = baseURL, content = [#xmlText{value = ExpectedBaseURL}]},
        #xmlElement{name = protocolVersion, content = [#xmlText{value = "2.0"}]}
    ] ++ [
        #xmlElement{name = adminEmail, content = [#xmlText{value = Email}]} || Email <- expected_admin_emails()
    ] ++ [
        #xmlElement{name = earliestDatestamp, content = [#xmlText{value = to_datestamp(Timestamp2)}]},
        #xmlElement{name = deletedRecord, content = [#xmlText{value = "persistent"}]},
        #xmlElement{name = granularity, content = [#xmlText{value = "YYYY-MM-DDThh:mm:ssZ"}]}
    ],
    ?assert(check_identify(200, [], Method, ExpResponseContent2, Config)).


get_record_test_base(Config, Method) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    HandleEntry = create_handle(MetadataPrefix),
    Args = [
        {<<"identifier">>, oai_identifier(HandleEntry)},
        {<<"metadataPrefix">>, MetadataPrefix}
    ],
    ?assert(check_get_record(200, Args, Method, [expected_oai_record_xml(HandleEntry)], Config)),

    ModifiedHandleEntry = modify_handle_with_mocked_timestamp(HandleEntry, ?NOW() + 123456),
    ?assert(check_get_record(200, Args, Method, [expected_oai_record_xml(ModifiedHandleEntry)], Config)).


% Simulate a DC record that preexisted since the previous version of Onezone
% (currently, it is not possible to create handles with invalid metadata).
% Then, simulate a Onezone upgrade and try to read such record.
get_dc_record_with_bad_metadata_test_base(Config, Method) ->
    User = ozt_users:create(),
    SpaceId = ozt_users:create_space_for(User),
    HServiceId = ozt_users:create_handle_service_for(User),

    BadMetadataExamples = [
        <<"">>,
        <<"null">>,
        <<"<bad-xml></yes-very-bad>">>,
        <<"<not-a-metadata-root-element></not-a-metadata-root-element>">>
    ],

    lists:foreach(fun(Metadata) ->
        ShareId = ozt_users:create_share_for(User, SpaceId),
        HandleDoc = ozt_handles:gen_legacy_handle_doc(HServiceId, ShareId, Metadata),
        {ok, #document{
            key = HandleId,
            value = #od_handle{
                public_handle = PublicHandle,
                timestamp = Timestamp
            }
        }} = ozt:rpc(od_handle, create, [HandleDoc]),
        {ok, _} = ozt:rpc(od_handle_service, update, [HServiceId, fun(HS = #od_handle_service{handles = Handles}) ->
            {ok, HS#od_handle_service{handles = [HandleId | Handles]}}
        end]),

        ?assertMatch(ok, ozt:rpc(od_handle, migrate_legacy_handles, [])),

        Args = [
            {<<"identifier">>, oai_identifier(HandleId)},
            {<<"metadataPrefix">>, ?OAI_DC_METADATA_PREFIX}  % all legacy handles have the DC format
        ],

        HandleEntry = #handle_listing_entry{
            timestamp = Timestamp, handle_id = HandleId, service_id = HServiceId, status = present
        },

        % Badly formatted metadata should result in only dc:identifiers
        % (public handle and public share url) being present in the OAI output
        ExpectedDCMetadata = ozt:rpc(oai_metadata, adapt_for_oai_pmh, [
            ?OAI_DC_METADATA_PREFIX,
            #xmlElement{name = metadata, content = [
                #xmlText{value = "\n    "},
                #xmlElement{
                    name = 'dc:identifier',
                    content = [#xmlText{value = binary_to_list(PublicHandle)}]
                },
                #xmlText{value = "\n    "},
                #xmlElement{
                    name = 'dc:identifier',
                    content = [#xmlText{value = binary_to_list(ozt:rpc(share_logic, build_public_url, [ShareId]))}]
                }
            ]}
        ]),
        ExpResponseContent = [expected_oai_record_xml(HandleEntry, ExpectedDCMetadata)],
        ?assert(check_get_record(200, Args, Method, ExpResponseContent, Config))
    end, BadMetadataExamples).


get_deleted_record_test_base(Config, Method) ->
    Timestamp = ?NOW(),
    Timestamp2 = increase_timestamp(Timestamp, 1),
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    HandleEntry = create_handle_with_mocked_timestamp(MetadataPrefix, Timestamp),

    Args = [
        {<<"identifier">>, oai_identifier(HandleEntry)},
        {<<"metadataPrefix">>, MetadataPrefix}
    ],
    ExpResponseContent = [expected_oai_record_xml(HandleEntry)],
    ?assert(check_get_record(200, Args, Method, ExpResponseContent, Config)),

    delete_handle_with_mocked_timestamp(HandleEntry, Timestamp2),
    ExpResponseContent2 = [expected_oai_record_xml(HandleEntry#handle_listing_entry{
        timestamp = Timestamp2, status = deleted
    })],
    ?assert(check_get_record(200, Args, Method, ExpResponseContent2, Config)).


list_metadata_formats_test_base(Config, Method) ->
    BuildExpResponseContent = fun(SupportedPrefixes) ->
        lists:map(fun(MetadataPrefix) ->
            {_, Namespace} = ozt:rpc(oai_metadata, main_namespace, [MetadataPrefix]),
            #xmlElement{
                name = metadataFormat,
                content = [
                    #xmlElement{
                        name = metadataPrefix,
                        content = [#xmlText{value = binary_to_list(MetadataPrefix)}]
                    },
                    #xmlElement{
                        name = schema,
                        content = [#xmlText{value = binary_to_list(ozt:rpc(oai_metadata, schema_URL, [MetadataPrefix]))}]
                    },
                    #xmlElement{
                        name = metadataNamespace,
                        content = [#xmlText{value = binary_to_list(Namespace)}]
                    }
                ]
            }
        end, SupportedPrefixes)
    end,
    % when no identifier is provided, returns all formats supported by the repository
    ?assert(check_list_metadata_formats(
        200, [], Method, BuildExpResponseContent(ozt_handles:supported_metadata_prefixes()), Config
    )),

    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    HandleEntry = create_handle(MetadataPrefix),
    Args = [{<<"identifier">>, oai_identifier(HandleEntry)}],
    ?assert(check_list_metadata_formats(
        200, Args, Method, BuildExpResponseContent([MetadataPrefix]), Config
    )).


list_identifiers_test_base(Config, Method, IdentifiersNum, FromOffset, UntilOffset) ->
    BeginTime = ?NOW(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    HandleEntries = setup_test_for_harvesting(MetadataPrefix, BeginTime, TimeOffsets),
    list_with_time_offsets_test_base(Config, Method, <<"ListIdentifiers">>, HandleEntries,
        BeginTime, FromOffset, UntilOffset, MetadataPrefix).


list_resumption_token_test_base(Config, Method, Verb, IdentifiersNum) ->
    BeginTime = ?NOW(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    HandleEntries = setup_test_for_harvesting(MetadataPrefix, BeginTime, TimeOffsets),
    Args = prepare_harvesting_args(MetadataPrefix, undefined, undefined),
    ?assert(check_list_entries_continuously_with_resumption_token(Config, Method, Verb, HandleEntries, Args)).


list_identifiers_modify_timestamp_test_base(Config, Method, IdentifiersNum,
    FromOffset, UntilOffset, IdentifiersToBeModified) ->

    %% IdentifiersToBeModified is number of identifiers that will be modified
    %% so that their timestamps will be set to Until + 1 (if Until is undefined
    BeginTime = ?NOW(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),

    HandleEntries = setup_test_for_harvesting(MetadataPrefix, BeginTime, TimeOffsets, no_deleted_handles),
    list_with_time_offsets_test_base(Config, Method, <<"ListIdentifiers">>, HandleEntries,
        BeginTime, FromOffset, UntilOffset, MetadataPrefix),

    TimeOffsets2 = lists:map(fun({T, N}) ->
        case N =< IdentifiersToBeModified of
            true -> exclude_offset_from_range(T, FromOffset, UntilOffset);
            _ -> T
        end
    end, lists:zip(TimeOffsets, lists:seq(1, length(TimeOffsets)))),
    ModifiedHandleEntries = modify_handles_with_mocked_timestamp(HandleEntries, BeginTime, TimeOffsets2),
    list_with_time_offsets_test_base(Config, Method, <<"ListIdentifiers">>, ModifiedHandleEntries,
        BeginTime, FromOffset, UntilOffset, MetadataPrefix).


list_records_test_base(Config, Method, IdentifiersNum, FromOffset, UntilOffset) ->
    BeginTime = ?NOW(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    HandleEntries = setup_test_for_harvesting(MetadataPrefix, BeginTime, TimeOffsets),
    list_with_time_offsets_test_base(Config, Method, <<"ListRecords">>, HandleEntries,
        BeginTime, FromOffset, UntilOffset, MetadataPrefix).


list_records_modify_timestamp_test_base(Config, Method, IdentifiersNum,
    FromOffset, UntilOffset, IdentifiersToBeModified) ->

    %% IdentifiersToBeModified is number of identifiers that will be modified
    %% so that their timestamps will be set to Until + 1 (if Until is undefined

    BeginTime = ?NOW(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    HandleEntries = setup_test_for_harvesting(MetadataPrefix, BeginTime, TimeOffsets, no_deleted_handles),
    list_with_time_offsets_test_base(Config, Method, <<"ListRecords">>, HandleEntries,
        BeginTime, FromOffset, UntilOffset, MetadataPrefix),

    TimeOffsets2 = lists:map(fun({T, N}) ->
        case N =< IdentifiersToBeModified of
            true -> exclude_offset_from_range(T, FromOffset, UntilOffset);
            _ -> T
        end
    end, lists:zip(TimeOffsets, lists:seq(1, length(TimeOffsets)))),
    ModifiedHandleEntries = modify_handles_with_mocked_timestamp(HandleEntries, BeginTime, TimeOffsets2),
    list_with_time_offsets_test_base(Config, Method, <<"ListRecords">>, ModifiedHandleEntries,
        BeginTime, FromOffset, UntilOffset, MetadataPrefix).


list_with_time_offsets_test_base(
    Config, Method, Verb, HandleEntries, BeginTime, FromOffset, UntilOffset, MetadataPrefix
) ->
    From = increase_timestamp(BeginTime, FromOffset),
    Until = increase_timestamp(BeginTime, UntilOffset),
    Args = prepare_harvesting_args(MetadataPrefix, to_datestamp(From), to_datestamp(Until)),

    FilteredHandleEntries = filter_handles_from_until(HandleEntries, From, Until),
    ?assert(check_list_entries(Verb, Args, Method, FilteredHandleEntries, Config)),

    % check filtering by sets
    HandleServices = ozt_handle_services:list(),
    lists:foreach(fun(HandleServiceId) ->
        FilteredHService = lists:filter(fun(HandleEntry) ->
            HandleServiceId =:= HandleEntry#handle_listing_entry.service_id
        end, FilteredHandleEntries),

        ArgsWithSet = [{<<"set">>, HandleServiceId} | Args],
        ?assert(check_list_entries(Verb, ArgsWithSet, Method, FilteredHService, Config))
    end, HandleServices).


list_sets_test_base(Config, Method) ->
    ExpectedHandleServiceIdsAndNames = lists:sort(lists_utils:generate(fun() ->
        Name = ?RAND_STR(),
        {ozt_handle_services:create(Name), Name}
    end, ?RAND_INT(0, 10))),

    ExpResponseContent = lists:map(fun({HandleServiceId, HandleServiceName}) ->
        #xmlElement{name = set, content = [
            #xmlElement{
                name = setSpec,
                content = [#xmlText{
                    value = binary_to_list(HandleServiceId)
                }]
            },
            #xmlElement{
                name = setName,
                content = [#xmlText{
                    value = binary_to_list(HandleServiceName)
                }]
            }
        ]}
    end, ExpectedHandleServiceIdsAndNames),

    ?assert(check_list_sets(200, Method, ExpResponseContent, Config)).


list_sets_empty_repository_test_base(Config, Method) ->
    ?assert(check_list_sets(200, Method, [], Config)).


no_verb_test_base(Config, Method) ->
    ?assert(check_no_verb_error(200, [], Method, [], Config)).


empty_verb_test_base(Config, Method) ->
    ?assert(check_empty_verb_error(200, [], Method, [], Config)).


invalid_verb_test_base(Config, Method) ->
    ?assert(check_invalid_verb_error(200, [], Method, [], Config)).


illegal_arg_test_base(Config, Method) ->
    ?assert(check_illegal_arg_error(200, [{"k", "v"}], Method, [], Config)).


missing_arg_test_base(Config, Method) ->
    Args = [{<<"identifier">>, <<"some-identifier">>}],
    %% will perform GetRecord, metadataPrefix is missing
    ?assert(check_missing_arg_error(200, Args, Method, [], Config)).


id_not_existing_test_base(Config, Method) ->
    Args = [
        {<<"identifier">>, <<"some-identifier">>},
        {<<"metadataPrefix">>, ?RAND_METADATA_PREFIX()}
    ],
    ?assert(check_id_not_existing_error(200, Args, Method, [], Config)).


cannot_disseminate_format_test_base(Config, Method) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    HandleEntry = create_handle(MetadataPrefix),

    Args = [
        {<<"identifier">>, oai_identifier(HandleEntry)},
        {<<"metadataPrefix">>, <<"not_supported_format">>}
    ],
    ?assert(check_cannot_disseminate_format_error(200, Args, Method, [], Config)),

    AnotherMetadataPrefix = ?RAND_ELEMENT(lists:delete(MetadataPrefix, ozt_handles:supported_metadata_prefixes())),
    Args2 = [
        {<<"identifier">>, oai_identifier(HandleEntry)},
        {<<"metadataPrefix">>, AnotherMetadataPrefix}
    ],
    ?assert(check_cannot_disseminate_format_error(200, Args2, Method, [], Config)).


exclusive_resumption_token_required_error(Config, Method) ->
    Args = [
        {<<"metadataPrefix">>, ?RAND_METADATA_PREFIX()},
        {<<"resumptionToken">>, <<"example_token">>}
    ],
    ?assert(check_exclusive_resumption_token_required_error(200, Args, Method, [], Config)).


list_metadata_formats_error_test_base(Config, Method) ->
    ?assert(check_list_metadata_formats_error(
        200,
        [{<<"identifier">>, <<"bad-identifier">>}],
        Method,
        {illegalId, <<"bad-identifier">>},
        Config
    )),
    ?assert(check_list_metadata_formats_error(
        200,
        [{<<"identifier">>, oai_identifier(<<"bad-handle-id">>)}],
        Method,
        {idDoesNotExist, oai_identifier(<<"bad-handle-id">>)},
        Config
    )).


list_identifiers_empty_repository_error_test_base(Config, Method) ->
    Args = [{<<"metadataPrefix">>, ?RAND_METADATA_PREFIX()}],
    ?assert(check_list_entries_no_records_match_error(200, <<"ListIdentifiers">>, Args, Method, Config)).


list_identifiers_no_records_match_error_test_base(Config, Method, IdentifiersNum, FromOffset, UntilOffset) ->
    BeginTime = ?NOW(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    setup_test_for_harvesting(MetadataPrefix, BeginTime, TimeOffsets),

    From = to_datestamp(increase_timestamp(BeginTime, FromOffset)),
    Until = to_datestamp(increase_timestamp(BeginTime, UntilOffset)),
    Args = prepare_harvesting_args(?RAND_METADATA_PREFIX(), From, Until),

    ?assert(check_list_entries_no_records_match_error(200, <<"ListIdentifiers">>, Args, Method, Config)).


list_identifiers_granularity_mismatch_error_test_base(Config, Method) ->
    Args = [
        {<<"metadataPrefix">>, ?RAND_METADATA_PREFIX()},
        {<<"from">>, <<"1111-12-01">>},
        {<<"until">>, <<"1111-12-01T00:00:00Z">>}
    ],
    ?assert(check_list_identifiers_bad_argument_granularity_mismatch_error(200, Args, Method, [], Config)).


list_identifiers_wrong_date_format_error_test_base(Config, Method, From, Until, DateFormat) ->
    Args = prepare_harvesting_args(?RAND_METADATA_PREFIX(), From, Until),
    ?assert(check_list_identifiers_bad_argument_invalid_date_format_error(200, Args, Method, [], DateFormat, Config)).


list_records_no_records_match_error_test_base(Config, Method, IdentifiersNum, FromOffset, UntilOffset) ->
    BeginTime = ?NOW(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    setup_test_for_harvesting(MetadataPrefix, BeginTime, TimeOffsets),

    From = to_datestamp(increase_timestamp(BeginTime, FromOffset)),
    Until = to_datestamp(increase_timestamp(BeginTime, UntilOffset)),
    Args = prepare_harvesting_args(?RAND_METADATA_PREFIX(), From, Until),

    ?assert(check_list_entries_no_records_match_error(200, <<"ListRecords">>, Args, Method, Config)).


timestamp_evolution_and_harvesting_test_base(Config, Method) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),

    CheckListing = fun(From, Until, ExpHandleEntries) ->
        Args = prepare_harvesting_args(MetadataPrefix, to_datestamp(From), to_datestamp(Until)),
        Verb = ?RAND_ELEMENT([<<"ListIdentifiers">>, <<"ListRecords">>]),
        check_list_entries(Verb, Args, Method, ExpHandleEntries, Config)
    end,

    [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday] = lists_utils:generate(fun(DayNumber) ->
        ?NOW() + (DayNumber * 86400)
    end, 7),

    % the timestamp of a record should be the time of its last change (creation, modification, deletion)
    AlphaFromThursday0 = create_handle_with_mocked_timestamp(MetadataPrefix, Monday),
    AlphaFromThursday = modify_handle_with_mocked_timestamp(AlphaFromThursday0, Thursday),

    BetaFromFriday0 = create_handle_with_mocked_timestamp(MetadataPrefix, Monday),
    BetaFromFriday1 = modify_handle_with_mocked_timestamp(BetaFromFriday0, Tuesday),
    BetaFromFriday = delete_handle_with_mocked_timestamp(BetaFromFriday1, Friday),

    GammaFromSaturday0 = create_handle_with_mocked_timestamp(MetadataPrefix, Tuesday),
    GammaFromSaturday1 = modify_handle_with_mocked_timestamp(GammaFromSaturday0, Wednesday),
    GammaFromSaturday2 = modify_handle_with_mocked_timestamp(GammaFromSaturday1, Thursday),
    GammaFromSaturday = delete_handle_with_mocked_timestamp(GammaFromSaturday2, Saturday),

    DeltaFromWednesday = create_handle_with_mocked_timestamp(MetadataPrefix, Wednesday),

    OmegaFromSunday0 = create_handle_with_mocked_timestamp(MetadataPrefix, Monday),
    OmegaFromSunday = delete_handle_with_mocked_timestamp(OmegaFromSunday0, Sunday),

    ?assert(CheckListing(Monday, Sunday, [DeltaFromWednesday, AlphaFromThursday, BetaFromFriday, GammaFromSaturday, OmegaFromSunday])),
    ?assert(CheckListing(Monday, Wednesday, [DeltaFromWednesday])),
    ?assert(CheckListing(Monday, Monday, [])),
    ?assert(CheckListing(Monday, Tuesday, [])),
    ?assert(CheckListing(Saturday, Saturday, [GammaFromSaturday])),
    ?assert(CheckListing(Thursday, Saturday, [AlphaFromThursday, BetaFromFriday, GammaFromSaturday])),
    ?assert(CheckListing(Friday, Sunday, [BetaFromFriday, GammaFromSaturday, OmegaFromSunday])),
    ?assert(CheckListing(Wednesday, Sunday, [DeltaFromWednesday, AlphaFromThursday, BetaFromFriday, GammaFromSaturday, OmegaFromSunday])),
    ?assert(CheckListing(Sunday, Sunday, [OmegaFromSunday])).


% normally this should not happen but if for any reason a handle registered as present
% is missing from the DB, we expect it to be filtered out from the ListRecords results
filtering_of_broken_records_test_base(Config, Method) ->
    MetadataPrefix = ?RAND_METADATA_PREFIX(),

    % Beta and Omega handles are not created, but only entries are added to the registry

    Alpha = create_handle_with_mocked_timestamp(MetadataPrefix, ?NOW() + 1),

    Beta = #handle_listing_entry{
        timestamp = ?NOW() + 2,
        handle_id = ?RAND_STR(),
        service_id = ?RAND_ELEMENT(ozt_handle_services:list())
    },
    ozt:rpc(handle_registry, report_created, [
        MetadataPrefix,
        Beta#handle_listing_entry.service_id,
        Beta#handle_listing_entry.handle_id,
        Beta#handle_listing_entry.timestamp
    ]),

    Gamma = create_handle_with_mocked_timestamp(MetadataPrefix, ?NOW() + 3),

    Delta = create_handle_with_mocked_timestamp(MetadataPrefix, ?NOW() + 4),

    Omega = #handle_listing_entry{
        timestamp = ?NOW() + 5,
        handle_id = ?RAND_STR(),
        service_id = ?RAND_ELEMENT(ozt_handle_services:list())
    },
    ozt:rpc(handle_registry, report_created, [
        MetadataPrefix,
        Omega#handle_listing_entry.service_id,
        Omega#handle_listing_entry.handle_id,
        Omega#handle_listing_entry.timestamp
    ]),

    Args = prepare_harvesting_args(MetadataPrefix, undefined, undefined),
    ?assert(check_list_entries(<<"ListRecords">>, Args, Method, [
        Alpha,
        Beta#handle_listing_entry{status = deleted},
        Gamma,
        Delta,
        Omega#handle_listing_entry{status = deleted}
    ], Config)).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    application:ensure_all_started(hackney),
    ozt:init_per_suite(Config, fun() ->
        ozt:set_env(oai_pmh_list_identifiers_batch_size, ?TESTED_LIST_BATCH_SIZE),
        ozt:set_env(oai_pmh_list_records_batch_size, ?TESTED_LIST_BATCH_SIZE)
    end).

init_per_testcase(_, Config) ->
    ozt_mocks:freeze_time(),
    ozt_mocks:mock_handle_proxy(),
    ozt:delete_all_entities(),
    Config.

end_per_testcase(filtering_of_broken_records_get_test, _Config) ->
    end_per_testcase(filtering_of_broken_records_post_test, _Config);
end_per_testcase(filtering_of_broken_records_post_test, _Config) ->
    % this test simulates some DB inconsistencies so a specialized cleanup is needed
    try
        ozt:delete_all_entities()
    catch _:_ ->
        lists:foreach(fun(MetadataPrefix) ->
            lists:foreach(fun(#handle_listing_entry{timestamp = Timestamp, handle_id = HandleId, service_id = HServiceId}) ->
                ozt:rpc(handle_registry, report_deleted, [MetadataPrefix, HServiceId, HandleId, Timestamp, Timestamp])
            end, ozt:rpc(handle_registry, list_completely, [#{metadata_prefix => MetadataPrefix}]))
        end, ozt_handles:supported_metadata_prefixes())
    end,
    end_per_testcase(default, _Config);
end_per_testcase(_, _Config) ->
    ozt_mocks:unmock_handle_proxy(),
    ozt_mocks:unfreeze_time(),
    ok.

end_per_suite(_Config) ->
    application:stop(hackney),
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


check_list_sets(Code, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListSets">>, [], Method, ExpResponseContent,
        'ListSets', Config).


check_no_verb_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, none, Args, Method, ExpResponseContent,
        {error, {missing_key, <<"verb">>}}, Config).


check_empty_verb_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"">>, Args, Method, ExpResponseContent,
        {error, {not_legal_verb, <<"">>}}, Config).


check_invalid_verb_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"invalid_verb">>, Args, Method, ExpResponseContent,
        {error, {not_legal_verb, <<"invalid_verb">>}}, Config).


check_illegal_arg_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"Identify">>, Args, Method, ExpResponseContent,
        {error, {illegal_argument, ["k"]}}, Config).


check_missing_arg_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"GetRecord">>, Args, Method, ExpResponseContent,
        {error, {missing_key, [<<"metadataPrefix">>]}}, Config).


check_id_not_existing_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"GetRecord">>, Args, Method, ExpResponseContent,
        {error, {illegalId, proplists:get_value(<<"identifier">>, Args)}}, Config).


check_cannot_disseminate_format_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"GetRecord">>, Args, Method, ExpResponseContent,
        {error, {cannotDisseminateFormat, proplists:get_value(<<"metadataPrefix">>, Args)}}, Config).


check_exclusive_resumption_token_required_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListRecords">>, Args, Method, ExpResponseContent,
        {error, exclusive_argument}, Config
    ) andalso check_oai_request(Code, <<"ListIdentifiers">>, Args, Method,
        ExpResponseContent, {error, exclusive_argument}, Config
    ).


check_list_metadata_formats(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListMetadataFormats">>, Args, Method,
        ExpResponseContent, 'ListMetadataFormats', Config).


check_list_metadata_formats_error(Code, Args, Method, ExpErrorSpec, Config) ->
    check_oai_request(Code, <<"ListMetadataFormats">>, Args, Method, [], {error, ExpErrorSpec}, Config).


check_list_identifiers_bad_argument_granularity_mismatch_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListIdentifiers">>, Args, Method, ExpResponseContent, {error, {granularity_mismatch,
        proplists:get_value(<<"from">>, Args), proplists:get_value(<<"until">>, Args)
    }}, Config).


check_list_identifiers_bad_argument_invalid_date_format_error(Code, Args, Method, ExpResponseContent, DateFormat, Config) ->
    check_oai_request(Code, <<"ListIdentifiers">>, Args, Method, ExpResponseContent,
        {error, {invalid_date_format, proplists:get_value(DateFormat, Args)}}, Config
    ).


check_list_entries_no_records_match_error(Code, <<"ListIdentifiers">>, Args, Method, Config) ->
    check_oai_request(
        Code, <<"ListIdentifiers">>, Args, Method, [], {error, {noRecordsMatch,
            proplists:get_value(<<"from">>, Args), proplists:get_value(<<"until">>, Args),
            proplists:get_value(<<"set">>, Args), proplists:get_value(<<"metadataPrefix">>, Args)
        }}, Config
    );
check_list_entries_no_records_match_error(Code, <<"ListRecords">>, Args, Method, Config) ->
    check_oai_request(
        Code, <<"ListRecords">>, Args, Method, [], {error, {noRecordsMatch,
            proplists:get_value(<<"from">>, Args), proplists:get_value(<<"until">>, Args),
            proplists:get_value(<<"set">>, Args), proplists:get_value(<<"metadataPrefix">>, Args)
        }}, Config
    ).


check_list_entries(Verb, Args, Method, ExpHandleEntries, Config) ->
    BuildExpectedObject = fun(HandleEntry) ->
        case Verb of
            <<"ListIdentifiers">> -> expected_oai_header_xml(HandleEntry);
            <<"ListRecords">> -> expected_oai_record_xml(HandleEntry)
        end
    end,
    case ExpHandleEntries of
        [] ->
            check_list_entries_no_records_match_error(200, Verb, Args, Method, Config);
        _ ->
            check_list_entries(200, Verb, Args, Method, BuildExpectedObject, ExpHandleEntries, Config)
    end.


check_list_entries(Code, Verb, Args, Method, BuildExpectedObject, ExpectedHandleEntries, Config) ->
    ListingOpts = request_arguments_to_handle_listing_opts(Verb, Args),
    {_, ExpResumptionToken} = ozt:rpc(handle_registry, list_portion, [ListingOpts]),

    ExpectedBase = lists:map(fun(HandleEntry) -> BuildExpectedObject(HandleEntry) end, ExpectedHandleEntries),
    ExpResponseContent = ExpectedBase ++ expected_response_body_wrt_resumption_token(ExpResumptionToken, ListingOpts),
    check_oai_request(Code, Verb, Args, Method, ExpResponseContent, binary_to_atom(Verb), Config).


check_list_entries_continuously_with_resumption_token(_Config, _Method, _Verb, _RemainingExpEntries, []) ->
    true;
check_list_entries_continuously_with_resumption_token(Config, Method, Verb, RemainingExpEntries, Args) ->
    ExpListedEntries = lists:sublist(RemainingExpEntries, ?TESTED_LIST_BATCH_SIZE),
    ListingOpts = request_arguments_to_handle_listing_opts(Verb, Args),
    {_, ExpResumptionToken} = ozt:rpc(handle_registry, list_portion, [ListingOpts]),

    case check_list_entries(Verb, Args, Method, ExpListedEntries, Config) of
        true ->
            ArgsWithToken = add_to_args_if_defined(<<"resumptionToken">>, ExpResumptionToken, []),
            check_list_entries_continuously_with_resumption_token(
                Config, Method, Verb, lists:subtract(RemainingExpEntries, ExpListedEntries), ArgsWithToken
            );
        false -> false
    end.


check_oai_request(Code, Verb, Args, Method, ExpResponseContent, ResponseType, Config) ->
    URL = get_oai_pmh_URL(),
    Path = get_oai_pmh_api_path(),
    Args2 = case Verb of
        none -> Args;
        _ -> add_verb(Verb, Args)
    end,
    ResponseDate = time:seconds_to_datetime(?NOW()),
    ExpectedBody = expected_body(ExpResponseContent, ResponseType, Args2, ResponseDate),
    QueryString = prepare_querystring(Args2),

    Request = case Method of
        get -> #{
            method => get,
            url => URL,
            path => str_utils:format_bin("~ts?~ts", [Path, QueryString])
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
            body => oai_xml:encode(ExpectedBody),
            headers => {contains, ?RESPONSE_CONTENT_TYPE_HEADER}
        }
    }),

    Check.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_oai_pmh_URL() ->
    str_utils:format_bin("http://~ts", [ozt:get_domain()]).


get_oai_pmh_api_path() ->
    list_to_binary(ozt:get_env(oai_pmh_api_prefix)).


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
    add_to_args_if_defined(<<"verb">>, Verb, Args).


add_to_args_if_defined(_Key, undefined, Args) ->
    Args;
add_to_args_if_defined(Key, Value, Args) ->
    [{str_utils:to_binary(Key), str_utils:to_binary(Value)} | Args].


get_domain(Hostname) ->
    [_Node | Domain] = string:tokens(atom_to_list(Hostname), "."),
    string:join(Domain, ".").


expected_body(ExpectedResponse, ResponseType, Args, ResponseDate) ->
    Path = get_oai_pmh_api_path(),
    URL = get_oai_pmh_URL(),
    RequestURL = binary_to_list(<<URL/binary, Path/binary>>),

    ExpectedResponseElement = case ResponseType of
        {error, ErrorSpec} -> expected_response_error(ErrorSpec);
        Verb -> expected_response_verb(Verb, ExpectedResponse)
    end,
    ExpectedRequestElement = case ExpectedResponseElement of
        %% when error is badVerb or badArgument request element
        %% should only contain request URL
        #xmlElement{attributes = [#xmlAttribute{value = "badVerb"}]} -> expected_request_element(RequestURL);
        #xmlElement{attributes = [#xmlAttribute{value = "badArgument"}]} -> expected_request_element(RequestURL);
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
                content = [#xmlText{value = to_datestamp(ResponseDate)}]
            },
            ExpectedRequestElement,
            ExpectedResponseElement]
    }.


expected_response_error(ErrorSpec) ->
    #oai_error{code = Code, description = Description} = ozt:rpc(oai_errors, handle, [ErrorSpec]),
    #xmlElement{
        name = error,
        attributes = [#xmlAttribute{name = code, value = str_utils:to_list(Code)}],
        content = [#xmlText{value = binary_to_list(Description)}]
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


create_handle() ->
    create_handle(?RAND_METADATA_PREFIX()).

create_handle(MetadataPrefix) ->
    create_handle_with_mocked_timestamp(MetadataPrefix, ?NOW()).


create_handle_with_mocked_timestamp(MockedTimestamp) ->
    create_handle_with_mocked_timestamp(?RAND_METADATA_PREFIX(), MockedTimestamp).

create_handle_with_mocked_timestamp(MetadataPrefix, MockedTimestamp) ->
    Metadata = example_input_metadata(MetadataPrefix),
    create_handle_with_mocked_timestamp(MetadataPrefix, Metadata, MockedTimestamp).

create_handle_with_mocked_timestamp(MetadataPrefix, Metadata, MockedTimestamp) ->
    ozt_handle_services:list() == [] andalso lists_utils:generate(fun ozt_handle_services:create/0, 10),
    ozt_spaces:list() == [] andalso lists_utils:generate(fun ozt_spaces:create/0, 10),
    HServiceId = ?RAND_ELEMENT(ozt_handle_services:list()),
    SpaceId = ?RAND_ELEMENT(ozt_spaces:list()),
    ShareId = ozt_spaces:create_share(SpaceId),
    create_handle_with_mocked_timestamp(HServiceId, ShareId, MetadataPrefix, Metadata, MockedTimestamp).

create_handle_with_mocked_timestamp(HServiceId, ShareId, MetadataPrefix, Metadata, MockedTimestamp) ->
    ozt_mocks:set_frozen_time_seconds(MockedTimestamp),
    #handle_listing_entry{
        timestamp = MockedTimestamp,
        handle_id = ozt_handles:create(HServiceId, ShareId, MetadataPrefix, Metadata),
        service_id = HServiceId,
        status = present
    }.


modify_handles_with_mocked_timestamp(HandleEntries, BeginTime, TimeOffsets) ->
    lists:map(fun({HandleEntry, TimeOffset}) ->
        modify_handle_with_mocked_timestamp(HandleEntry, increase_timestamp(BeginTime, TimeOffset))
    end, lists:zip(HandleEntries, TimeOffsets)).


modify_handle_with_mocked_timestamp(HandleEntry, MockedTimestamp) ->
    HandleRecord = ozt_handles:get(HandleEntry#handle_listing_entry.handle_id),
    NewMetadata = example_input_metadata(HandleRecord#od_handle.metadata_prefix),
    modify_handle_with_mocked_timestamp(HandleEntry, NewMetadata, MockedTimestamp).

modify_handle_with_mocked_timestamp(HandleEntry, Metadata, MockedTimestamp) ->
    ozt_mocks:set_frozen_time_seconds(MockedTimestamp),
    ozt_handles:update(HandleEntry#handle_listing_entry.handle_id, #{<<"metadata">> => Metadata}),
    HandleEntry#handle_listing_entry{timestamp = MockedTimestamp}.


delete_handle_with_mocked_timestamp(HandleEntry, MockedTimestamp) ->
    ozt_mocks:set_frozen_time_seconds(MockedTimestamp),
    ozt:rpc(handle_logic, delete, [?ROOT, HandleEntry#handle_listing_entry.handle_id]),
    HandleEntry#handle_listing_entry{timestamp = MockedTimestamp, status = deleted}.


setup_test_for_harvesting(MetadataPrefix, BeginTime, TimeOffsets) ->
    setup_test_for_harvesting(MetadataPrefix, BeginTime, TimeOffsets, randomly_deleted_handles).

setup_test_for_harvesting(MetadataPrefix, BeginTime, TimeOffsets, DeletionStrategy) ->
    lists:map(fun(TimeOffset) ->
        Timestamp = increase_timestamp(BeginTime, TimeOffset),
        HandleEntry = create_handle_with_mocked_timestamp(MetadataPrefix, Timestamp),
        case {?RAND_BOOL(), DeletionStrategy} of
            {false, randomly_deleted_handles} ->
                delete_handle_with_mocked_timestamp(HandleEntry, Timestamp);
            _ ->
                HandleEntry
        end
    end, TimeOffsets).


% examples generated by ozt_handles are constant and based on the static examples in the plugins
% here we would like a bit more diversity in the metadata, so they are modified and the
% expected final metadata has to be calculated every time
example_input_metadata(MetadataPrefix) ->
    BasicExample = ozt_handles:example_input_metadata(MetadataPrefix),
    ParsedXml = ?check(oai_xml:parse(BasicExample)),
    EnrichedMetadata = case MetadataPrefix of
        ?OAI_DC_METADATA_PREFIX ->
            #xmlElement{name = metadata, content = Content} = ParsedXml,
            ParsedXml#xmlElement{content = oai_xml:prepend_element_with_indent(
                4, #xmlElement{
                    name = 'dc:identifier',
                    content = [#xmlText{value = binary_to_list(?RAND_UNICODE_STR())}]
                },
                Content
            )};
        ?EDM_METADATA_PREFIX ->
            #xmlElement{name = 'rdf:RDF', content = Content} = ParsedXml,
            ParsedXml#xmlElement{content = oai_xml:prepend_element_with_indent(
                4, #xmlElement{
                    name = 'edm:WebResource',
                    attributes = [#xmlAttribute{name = 'rdf:about', value = binary_to_list(?RAND_UNICODE_STR())}],
                    content = oai_xml:prepend_element_with_indent(
                        8, #xmlElement{
                            name = 'dc:description',
                            content = [#xmlText{value = binary_to_list(?RAND_UNICODE_STR())}]
                        },
                        []
                    )
                },
                Content
            )}
    end,
    ozt:rpc(oai_metadata, encode_xml, [MetadataPrefix, EnrichedMetadata]).




prepare_harvesting_args(MetadataPrefix, From, Until) ->
    prepare_harvesting_args(MetadataPrefix, From, Until, undefined).

prepare_harvesting_args(MetadataPrefix, From, Until, Set) ->
    Args = add_to_args_if_defined(<<"metadataPrefix">>, MetadataPrefix, []),
    Args2 = add_to_args_if_defined(<<"from">>, From, Args),
    Args3 = add_to_args_if_defined(<<"until">>, Until, Args2),
    add_to_args_if_defined(<<"set">>, Set, Args3).


request_arguments_to_handle_listing_opts(Verb, Args) ->
    VerbAtom = case Verb of
        <<"ListIdentifiers">> -> list_identifiers;
        <<"ListRecords">> -> list_records
    end,
    ozt:rpc(oai_utils, request_arguments_to_handle_listing_opts, [VerbAtom, Args]).


filter_handles_from_until(HandleEntries, From, Until) ->
    lists:filter(fun(H) -> offset_in_range(From, Until, H#handle_listing_entry.timestamp) end, HandleEntries).


increase_timestamp(_, undefined) -> undefined;
increase_timestamp(undefined, _) -> undefined;
increase_timestamp(Seconds, ExtraSeconds) ->
    Seconds + ExtraSeconds.


decrease_timestamp(Seconds, ExtraSeconds) ->
    Seconds - ExtraSeconds.


to_datestamp(undefined) -> undefined;
to_datestamp({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    str_utils:format(
        "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
        [Year, Month, Day, Hour, Minute, Second]);
to_datestamp(TimeSeconds) when is_integer(TimeSeconds) ->
    to_datestamp(time:seconds_to_datetime(TimeSeconds)).


offset_in_range(undefined, undefined, _) -> true;
offset_in_range(undefined, Until, Offset) ->
    Offset =< Until;
offset_in_range(From, undefined, Offset) ->
    From =< Offset;
offset_in_range(From, Until, Offset) ->
    (From =< Offset) and (Offset =< Until).


oai_identifier(#handle_listing_entry{handle_id = HandleId}) ->
    oai_identifier(HandleId);
oai_identifier(HandleId) ->
    <<"oai:", (ozt:get_domain())/binary, ":", HandleId/binary>>.


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


expected_admin_emails() ->
    ozt:get_env(admin_emails).


expected_oai_record_xml(#handle_listing_entry{status = deleted} = HandleEntry) ->
    #xmlElement{name = record, content = [
        expected_oai_header_xml(HandleEntry)
    ]};
expected_oai_record_xml(#handle_listing_entry{status = present} = HandleEntry) ->
    HandleRecord = ozt_handles:get(HandleEntry#handle_listing_entry.handle_id),
    ParsedXml = ?check(oai_xml:parse(HandleRecord#od_handle.metadata)),
    ExpectedMetadata = ozt:rpc(oai_metadata, adapt_for_oai_pmh, [HandleRecord#od_handle.metadata_prefix, ParsedXml]),
    expected_oai_record_xml(HandleEntry, ExpectedMetadata).

expected_oai_record_xml(HandleEntry, ExpectedMetadata) ->
    #xmlElement{name = record, content = [
        expected_oai_header_xml(HandleEntry),
        #xmlElement{
            name = metadata,
            content = [ExpectedMetadata]
        }
    ]}.


expected_oai_header_xml(#handle_listing_entry{
    timestamp = Timestamp, handle_id = HandleId, service_id = HServiceId, status = Status
}) ->
    #xmlElement{
        name = header,
        attributes = case Status of
            present -> [];
            deleted -> [#xmlAttribute{name = status, value = "deleted"}]
        end,
        content = [
            #xmlElement{
                name = identifier,
                content = [#xmlText{
                    value = binary_to_list(oai_identifier(HandleId))
                }]
            },
            #xmlElement{
                name = datestamp,
                content = [#xmlText{
                    value = to_datestamp(Timestamp)
                }]
            },
            #xmlElement{
                name = setSpec,
                content = [#xmlText{
                    value = binary_to_list(HServiceId)
                }]
            }
        ]
    }.


%%%-------------------------------------------------------------------
%%% @doc
%%% @private
%%% According to OAI-PMH spec, the token MUST be present in the response if an incomplete list is returned,
%%% and MUST be present and MUST be empty when the last batch that completes the list is returned.
%%% However, if the whole list is returned in one response, there should be no resumption token element at all.
%%% @end
%%%-------------------------------------------------------------------
-spec expected_response_body_wrt_resumption_token(handle_registry:resumption_token(), handle_registry:listing_opts()) ->
    [#xmlElement{}].
expected_response_body_wrt_resumption_token(undefined, ListingOpts) when not is_map_key(resumption_token, ListingOpts) ->
    [];
expected_response_body_wrt_resumption_token(ExpResumptionToken, _) ->
    [
        #xmlElement{
            name = resumptionToken,
            content = case ExpResumptionToken of
                undefined -> [];
                _ -> [#xmlText{value = binary_to_list(ExpResumptionToken)}]
            end
        }
    ].

