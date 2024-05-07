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

    list_metadata_formats_get_test/1,
    list_metadata_formats_post_test/1,

    list_identifiers_get_test/1,
    list_identifiers_post_test/1,
    list_identifiers_include_deleted_get_test/1,
    list_identifiers_include_deleted_post_test/1,
    list_identifiers_resumption_token_get_test/1,
    list_identifiers_resumption_token_post_test/1,
    list_deleted_identifier_get_test/1,
    list_deleted_identifier_post_test/1,
    list_all_identifiers_no_resumption_token_get_test/1,
    list_all_identifiers_no_resumption_token_post_test/1,
    selective_list_identifiers1_get_test/1,
    selective_list_identifiers1_post_test/1,
    selective_list_identifiers2_get_test/1,
    selective_list_identifiers2_post_test/1,
    selective_list_identifiers3_get_test/1,
    selective_list_identifiers3_post_test/1,
    selective_list_identifiers4_get_test/1,
    selective_list_identifiers4_post_test/1,
    selective_list_identifiers1_include_deleted_get_test/1,
    selective_list_identifiers1_include_deleted_post_test/1,
    selective_list_identifiers2_include_deleted_get_test/1,
    selective_list_identifiers2_include_deleted_post_test/1,
    selective_list_identifiers3_include_deleted_get_test/1,
    selective_list_identifiers3_include_deleted_post_test/1,
    selective_list_identifiers4_include_deleted_get_test/1,
    selective_list_identifiers4_include_deleted_post_test/1,
    list_identifiers_modify_timestamp_get_test/1,
    list_identifiers_modify_timestamp_post_test/1,
    list_identifiers_modify_timestamp1_get_test/1,
    list_identifiers_modify_timestamp1_post_test/1,
    list_identifiers_modify_timestamp2_get_test/1,
    list_identifiers_modify_timestamp2_post_test/1,

    list_records_get_test/1,
    list_records_post_test/1,
    list_records_include_deleted_get_test/1,
    list_records_include_deleted_post_test/1,
    list_records_resumption_token_get_test/1,
    list_records_resumption_token_post_test/1,
    list_deleted_record_get_test/1,
    list_deleted_record_post_test/1,
    list_all_records_no_resumption_token_get_test/1,
    list_all_records_no_resumption_token_post_test/1,
    selective_list_records1_get_test/1,
    selective_list_records1_post_test/1,
    selective_list_records2_get_test/1,
    selective_list_records2_post_test/1,
    selective_list_records3_get_test/1,
    selective_list_records3_post_test/1,
    selective_list_records4_get_test/1,
    selective_list_records4_post_test/1,
    selective_list_records1_include_deleted_get_test/1,
    selective_list_records1_include_deleted_post_test/1,
    selective_list_records2_include_deleted_get_test/1,
    selective_list_records2_include_deleted_post_test/1,
    selective_list_records3_include_deleted_get_test/1,
    selective_list_records3_include_deleted_post_test/1,
    selective_list_records4_include_deleted_get_test/1,
    selective_list_records4_include_deleted_post_test/1,
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
    list_metadata_formats_no_format_error_get_test/1,
    list_metadata_formats_no_format_error_post_test/1,
    cannot_disseminate_format_get_test/1,
    cannot_disseminate_format_post_test/1,
    exclusive_resumption_token_required_get_test/1,
    exclusive_resumption_token_required_post_test/1,
    list_identifiers_empty_repository_error_get_test/1,
    list_identifiers_empty_repository_error_post_test/1,
    list_identifiers_no_records_match_error1_get_test/1,
    list_identifiers_no_records_match_error1_post_test/1,
    list_identifiers_no_records_match_error2_get_test/1,
    list_identifiers_no_records_match_error2_post_test/1,
    list_identifiers_granularity_mismatch_error_post_test/1,
    list_identifiers_granularity_mismatch_error_get_test/1,
    list_identifiers_wrong_date_format_error1_get_test/1,
    list_identifiers_wrong_date_format_error1_post_test/1,
    list_identifiers_wrong_date_format_error2_get_test/1,
    list_identifiers_wrong_date_format_error2_post_test/1,
    list_identifiers_wrong_date_format_error3_get_test/1,
    list_identifiers_wrong_date_format_error3_post_test/1,

    list_records_no_records_match_error1_get_test/1,
    list_records_no_records_match_error1_post_test/1,
    list_records_no_records_match_error2_get_test/1,
    list_records_no_records_match_error2_post_test/1
]).

-define(TESTED_HANDLE_LIST_LIMIT, 10).

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

    list_metadata_formats_get_test,
    list_metadata_formats_post_test,
    list_identifiers_get_test,
    list_identifiers_post_test,
    list_identifiers_include_deleted_get_test,
    list_identifiers_include_deleted_post_test,

    list_identifiers_resumption_token_get_test,
    list_identifiers_resumption_token_post_test,
    list_all_identifiers_no_resumption_token_get_test,
    list_all_identifiers_no_resumption_token_post_test,

    list_deleted_identifier_get_test,
    list_deleted_identifier_post_test,

    selective_list_identifiers1_get_test,
    selective_list_identifiers1_post_test,
    selective_list_identifiers2_get_test,
    selective_list_identifiers2_post_test,
    selective_list_identifiers3_get_test,
    selective_list_identifiers3_post_test,
    selective_list_identifiers4_get_test,
    selective_list_identifiers4_post_test,
    selective_list_identifiers1_include_deleted_get_test,
    selective_list_identifiers1_include_deleted_post_test,
    selective_list_identifiers2_include_deleted_get_test,
    selective_list_identifiers2_include_deleted_post_test,
    selective_list_identifiers3_include_deleted_get_test,
    selective_list_identifiers3_include_deleted_post_test,
    selective_list_identifiers4_include_deleted_get_test,
    selective_list_identifiers4_include_deleted_post_test,
    list_identifiers_modify_timestamp_get_test,
    list_identifiers_modify_timestamp_post_test,
    list_identifiers_modify_timestamp1_get_test,
    list_identifiers_modify_timestamp1_post_test,
    list_identifiers_modify_timestamp2_get_test,
    list_identifiers_modify_timestamp2_post_test,

    list_records_get_test,
    list_records_post_test,
    list_records_include_deleted_get_test,
    list_records_include_deleted_post_test,
    list_records_resumption_token_get_test,
    list_records_resumption_token_post_test,

    list_deleted_record_get_test,
    list_deleted_record_post_test,

    list_all_records_no_resumption_token_get_test,
    list_all_records_no_resumption_token_post_test,
    selective_list_records1_get_test,
    selective_list_records1_post_test,
    selective_list_records2_get_test,
    selective_list_records2_post_test,
    selective_list_records3_get_test,
    selective_list_records3_post_test,
    selective_list_records4_get_test,
    selective_list_records4_post_test,
    selective_list_records1_include_deleted_get_test,
    selective_list_records1_include_deleted_post_test,
    selective_list_records2_include_deleted_get_test,
    selective_list_records2_include_deleted_post_test,
    selective_list_records3_include_deleted_get_test,
    selective_list_records3_include_deleted_post_test,
    selective_list_records4_include_deleted_get_test,
    selective_list_records4_include_deleted_post_test,
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
    list_metadata_formats_no_format_error_get_test,
    list_metadata_formats_no_format_error_post_test,
    cannot_disseminate_format_get_test,
    cannot_disseminate_format_post_test,
    exclusive_resumption_token_required_get_test,
    exclusive_resumption_token_required_post_test,
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

    list_records_no_records_match_error1_get_test,
    list_records_no_records_match_error1_post_test,
    list_records_no_records_match_error2_get_test,
    list_records_no_records_match_error2_post_test
]).

%% useful macros
-define(CONTENT_TYPE_HEADER,
    #{?HDR_CONTENT_TYPE => <<"application/x-www-form-urlencoded">>}).
-define(RESPONSE_CONTENT_TYPE_HEADER,
    #{?HDR_CONTENT_TYPE => <<"text/xml">>}).

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
-define(RAND_METADATA_PREFIX(), ?RAND_ELEMENT(ozt_handles:supported_metadata_prefixes())).

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

-define(NOW(), global_clock:timestamp_seconds()).
-define(CURRENT_DATETIME(), time:seconds_to_datetime(?NOW())).

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

list_metadata_formats_get_test(Config) ->
    list_metadata_formats_test_base(Config, get).

list_metadata_formats_post_test(Config) ->
    list_metadata_formats_test_base(Config, post).

list_identifiers_get_test(Config) ->
    list_identifiers_test_base(Config, get, 10, undefined, undefined).

list_identifiers_post_test(Config) ->
    list_identifiers_test_base(Config, post, 10, undefined, undefined).

list_identifiers_include_deleted_get_test(Config) ->
    list_identifiers_include_deleted_test_base(Config, get, 10, undefined, undefined).

list_identifiers_include_deleted_post_test(Config) ->
    list_identifiers_include_deleted_test_base(Config, post, 10, undefined, undefined).

list_identifiers_resumption_token_get_test(Config) ->
    list_resumption_token_test_base(Config, get, <<"ListIdentifiers">>, (?TESTED_HANDLE_LIST_LIMIT * 5) div 2).

list_identifiers_resumption_token_post_test(Config) ->
    list_resumption_token_test_base(Config, post, <<"ListIdentifiers">>, (?TESTED_HANDLE_LIST_LIMIT * 5) div 2).

list_deleted_identifier_get_test(Config) ->
    list_deleted_test_base(Config, get, <<"ListIdentifiers">>).

list_deleted_identifier_post_test(Config) ->
    list_deleted_test_base(Config, post, <<"ListIdentifiers">>).

list_all_identifiers_no_resumption_token_get_test(Config) ->
    list_no_resumption_token_test_base(Config, get, <<"ListIdentifiers">>, ?TESTED_HANDLE_LIST_LIMIT).

list_all_identifiers_no_resumption_token_post_test(Config) ->
    list_no_resumption_token_test_base(Config, post, <<"ListIdentifiers">>, ?TESTED_HANDLE_LIST_LIMIT).

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

selective_list_identifiers1_include_deleted_get_test(Config) ->
    list_identifiers_include_deleted_test_base(Config, get, 10, 0, 5).

selective_list_identifiers1_include_deleted_post_test(Config) ->
    list_identifiers_include_deleted_test_base(Config, post, 10, 0, 5).

selective_list_identifiers2_include_deleted_get_test(Config) ->
    list_identifiers_include_deleted_test_base(Config, get, 10, 0, 0).

selective_list_identifiers2_include_deleted_post_test(Config) ->
    list_identifiers_include_deleted_test_base(Config, post, 10, 0, 0).

selective_list_identifiers3_include_deleted_get_test(Config) ->
    list_identifiers_include_deleted_test_base(Config, get, 10, undefined, 7).

selective_list_identifiers3_include_deleted_post_test(Config) ->
    list_identifiers_include_deleted_test_base(Config, get, 10, undefined, 7).

selective_list_identifiers4_include_deleted_get_test(Config) ->
    list_identifiers_include_deleted_test_base(Config, get, 10, 3, undefined).

selective_list_identifiers4_include_deleted_post_test(Config) ->
    list_identifiers_include_deleted_test_base(Config, post, 10, 3, undefined).

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

list_records_include_deleted_get_test(Config) ->
    list_records_include_deleted_test_base(Config, get, 10, undefined, undefined).

list_records_include_deleted_post_test(Config) ->
    list_records_include_deleted_test_base(Config, post, 10, undefined, undefined).

list_records_resumption_token_get_test(Config) ->
    list_resumption_token_test_base(Config, get, <<"ListRecords">>, (?TESTED_HANDLE_LIST_LIMIT * 5) div 2).

list_records_resumption_token_post_test(Config) ->
    list_resumption_token_test_base(Config, post, <<"ListRecords">>, (?TESTED_HANDLE_LIST_LIMIT * 5) div 2).

list_all_records_no_resumption_token_get_test(Config) ->
    list_no_resumption_token_test_base(Config, get, <<"ListRecords">>, ?TESTED_HANDLE_LIST_LIMIT).

list_all_records_no_resumption_token_post_test(Config) ->
    list_no_resumption_token_test_base(Config, post, <<"ListRecords">>, ?TESTED_HANDLE_LIST_LIMIT).

list_deleted_record_get_test(Config) ->
    list_deleted_test_base(Config, get, <<"ListRecords">>).

list_deleted_record_post_test(Config) ->
    list_deleted_test_base(Config, post, <<"ListRecords">>).

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

selective_list_records1_include_deleted_get_test(Config) ->
    list_records_include_deleted_test_base(Config, get, 10, 1, 7).

selective_list_records1_include_deleted_post_test(Config) ->
    list_records_include_deleted_test_base(Config, post, 10, 1, 7).

selective_list_records2_include_deleted_get_test(Config) ->
    list_records_include_deleted_test_base(Config, get, 10, 9, 9).

selective_list_records2_include_deleted_post_test(Config) ->
    list_records_include_deleted_test_base(Config, post, 10, 9, 9).

selective_list_records3_include_deleted_get_test(Config) ->
    list_records_include_deleted_test_base(Config, get, 10, undefined, 2).

selective_list_records3_include_deleted_post_test(Config) ->
    list_records_include_deleted_test_base(Config, get, 10, undefined, 2).

selective_list_records4_include_deleted_get_test(Config) ->
    list_records_include_deleted_test_base(Config, get, 10, 9, undefined).

selective_list_records4_include_deleted_post_test(Config) ->
    list_records_include_deleted_test_base(Config, post, 10, 9, undefined).

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
    list_identifiers_wrong_from_date_format_error_test_base(Config, get, <<"1111-01">>, undefined).

list_identifiers_wrong_date_format_error1_post_test(Config) ->
    list_identifiers_wrong_from_date_format_error_test_base(Config, post, <<"1111-01">>, undefined).

list_identifiers_wrong_date_format_error2_get_test(Config) ->
    list_identifiers_wrong_until_date_format_error_test_base(Config, get, undefined, <<"1111-01-25T00:01:25">>).

list_identifiers_wrong_date_format_error2_post_test(Config) ->
    list_identifiers_wrong_until_date_format_error_test_base(Config, post, undefined, <<"1111-01-25T00:01:25">>).

list_identifiers_wrong_date_format_error3_get_test(Config) ->
    list_identifiers_wrong_until_date_format_error_test_base(Config, get, undefined, <<"1111-13-25T65:01:25">>).

list_identifiers_wrong_date_format_error3_post_test(Config) ->
    list_identifiers_wrong_until_date_format_error_test_base(Config, post, undefined, <<"1111-13-25T65:01:25">>).

list_records_no_records_match_error1_get_test(Config) ->
    list_records_no_records_match_error_test_base(Config, get, 10, 11, 15).

list_records_no_records_match_error1_post_test(Config) ->
    list_records_no_records_match_error_test_base(Config, post, 10, 11, 15).

list_records_no_records_match_error2_get_test(Config) ->
    list_records_no_records_match_error_test_base(Config, get, 10, -11, -5).

list_records_no_records_match_error2_post_test(Config) ->
    list_records_no_records_match_error_test_base(Config, post, 10, -11, -5).

%%%===================================================================
%%% Test base functions
%%%===================================================================

identify_test_base(Config, Method) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
    Path = get_oai_pmh_api_path(),
    ExpectedBaseURL = string:concat(get_domain(Node), binary_to_list(Path)),

    {ok, User} = oz_test_utils:create_user(Config),
    {ok, Space1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    ShareId = datastore_key:new(),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?USER(User), ShareId, ShareId, Space1
    ),
    {HSId, _} = create_handle_service(Config, User),
    Timestamp = ?CURRENT_DATETIME(),
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Metadata = ozt_handles:example_input_metadata(MetadataPrefix),
    create_handle_with_mocked_timestamp(Config, User, HSId, ShareId,
        Metadata, MetadataPrefix, Timestamp),

    ExpResponseContent = [
        #xmlElement{name = repositoryName, content = [#xmlText{value = "undefined"}]},
        #xmlElement{name = baseURL, content = [#xmlText{value = ExpectedBaseURL}]},
        #xmlElement{name = protocolVersion, content = [#xmlText{value = "2.0"}]}
    ] ++ [
        #xmlElement{name = adminEmail, content = [#xmlText{value = Email}]} || Email <- expected_admin_emails(Config)
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

    {ok, User} = oz_test_utils:create_user(Config),
    SpaceIds = create_spaces(Config, ?SPACE_NAMES(2), ?USER(User)),
    [ShareId1, ShareId2] = create_shares(Config, SpaceIds),
    {HSId, _} = create_handle_service(Config, User),
    Timestamp1 = ?CURRENT_DATETIME(),
    Timestamp2 = increase_timestamp(Timestamp1, 1),
    Timestamp3 = increase_timestamp(Timestamp2, 1),
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Metadata = ozt_handles:example_input_metadata(MetadataPrefix),
    Identifier1 = create_handle_with_mocked_timestamp(Config, User, HSId, ShareId1,
        Metadata, MetadataPrefix, Timestamp1),
    Metadata2 = ozt_handles:example_input_metadata(MetadataPrefix),
    _Identifier2 = create_handle_with_mocked_timestamp(Config, User, HSId, ShareId2,
        Metadata2, MetadataPrefix, Timestamp2),

    ExpResponseContent = [
        #xmlElement{name = repositoryName, content = [#xmlText{value = "undefined"}]},
        #xmlElement{name = baseURL, content = [#xmlText{value = ExpectedBaseURL}]},
        #xmlElement{name = protocolVersion, content = [#xmlText{value = "2.0"}]}
    ] ++ [
        #xmlElement{name = adminEmail, content = [#xmlText{value = Email}]} || Email <- expected_admin_emails(Config)
    ] ++ [
        #xmlElement{name = earliestDatestamp, content = [#xmlText{value = to_datestamp(Timestamp1)}]},
        #xmlElement{name = deletedRecord, content = [#xmlText{value = "persistent"}]},
        #xmlElement{name = granularity, content = [#xmlText{value = "YYYY-MM-DDThh:mm:ssZ"}]}
    ],

    ?assert(check_identify(200, [], Method, ExpResponseContent, Config)),

    modify_handle_with_mocked_timestamp(Config, Identifier1, Metadata, Timestamp3),

    ExpResponseContent2 = [
        #xmlElement{name = repositoryName, content = [#xmlText{value = "undefined"}]},
        #xmlElement{name = baseURL, content = [#xmlText{value = ExpectedBaseURL}]},
        #xmlElement{name = protocolVersion, content = [#xmlText{value = "2.0"}]}
    ] ++ [
        #xmlElement{name = adminEmail, content = [#xmlText{value = Email}]} || Email <- expected_admin_emails(Config)
    ] ++ [
        #xmlElement{name = earliestDatestamp, content = [#xmlText{value = to_datestamp(Timestamp2)}]},
        #xmlElement{name = deletedRecord, content = [#xmlText{value = "persistent"}]},
        #xmlElement{name = granularity, content = [#xmlText{value = "YYYY-MM-DDThh:mm:ssZ"}]}
    ],
    ?assert(check_identify(200, [], Method, ExpResponseContent2, Config)).

get_record_test_base(Config, Method) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, Space1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    ShareId = datastore_key:new(),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?USER(User), ShareId, ShareId, Space1
    ),
    {HSId, _} = create_handle_service(Config, User),
    Timestamp = ?CURRENT_DATETIME(),
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Metadata = ozt_handles:example_input_metadata(MetadataPrefix),
    HandleId = create_handle_with_mocked_timestamp(Config, User, HSId, ShareId,
        Metadata, MetadataPrefix, Timestamp),
    ExpectedMetadata = expected_final_metadata_element(MetadataPrefix, HandleId),

    Args = [
        {<<"identifier">>, oai_identifier(Config, HandleId)},
        {<<"metadataPrefix">>, MetadataPrefix}
    ],
    ExpResponseContent = [
        expected_oai_record_xml(Config, HandleId, Timestamp, ExpectedMetadata)
    ],
    ?assert(check_get_record(200, Args, Method, ExpResponseContent, Config)).


get_dc_record_with_bad_metadata_test_base(Config, Method) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, Space1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    {HSId, _} = create_handle_service(Config, User),
    Timestamp = ?NOW(),
    DataTime = ?CURRENT_DATETIME(),

    BadMetadataExamples = [
        <<"">>,
        <<"null">>,
        <<"<bad-xml></yes-very-bad>">>
    ],

    lists:foreach(fun(Metadata) ->
        % simulate a handle being added in the old system version, when metadata was not sanitized
        ShareId = datastore_key:new(),
        {ok, ShareId} = oz_test_utils:create_share(Config, ?USER(User), ShareId, ShareId, Space1),
        MetadataPrefix = ?OAI_DC_METADATA_PREFIX,
        {ok, PublicHandle} = ozt:rpc(handle_proxy, register_handle,
            [HSId, ?HANDLE_RESOURCE_TYPE, ShareId, Metadata]
        ),
        {ok, #document{key = Identifier}} = ozt:rpc(od_handle, create, [#document{value = #od_handle{
            handle_service = HSId,
            resource_type = ?HANDLE_RESOURCE_TYPE,
            resource_id = ShareId,
            public_handle = PublicHandle,
            metadata = Metadata,
            metadata_prefix = MetadataPrefix,
            timestamp = Timestamp
        }}]),
        ozt:rpc(handles, report_created, [MetadataPrefix, HSId, Identifier, Timestamp]),
        Args = [
            {<<"identifier">>, oai_identifier(Config, Identifier)},
            {<<"metadataPrefix">>, MetadataPrefix}
        ],

        % Badly formatted metadata should result in only
        % dc:identifiers being present in the OAI output
        ExpectedDCMetadata = ozt:rpc(oai_metadata, adapt_for_oai_pmh, [
                MetadataPrefix,
                #xmlElement{name = metadata, content = expected_dc_identifiers(Config, Identifier)}
        ]),
        ExpResponseContent = [
            expected_oai_record_xml(Config, Identifier, DataTime, ExpectedDCMetadata)
        ],

        ?assert(check_get_record(200, Args, Method, ExpResponseContent, Config))
    end, BadMetadataExamples).


list_metadata_formats_test_base(Config, Method) ->
    ExpResponseContent = lists:map(fun(MetadataPrefix) ->
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
    end, ozt_handles:supported_metadata_prefixes()),
    ?assert(check_list_metadata_formats(200, [], Method, ExpResponseContent, Config)).


list_identifiers_test_base(Config, Method, IdentifiersNum, FromOffset, UntilOffset) ->
    BeginTime = ?CURRENT_DATETIME(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Metadata = ozt_handles:example_input_metadata(MetadataPrefix),
    Identifiers = setup_test_for_harvesting(
        Config, IdentifiersNum, BeginTime, TimeOffsets, Metadata, MetadataPrefix
    ),
    list_with_time_offsets_test_base(Config, Method, <<"ListIdentifiers">>, Identifiers,
        TimeOffsets, BeginTime, FromOffset, UntilOffset, MetadataPrefix).


list_identifiers_include_deleted_test_base(Config, Method, IdentifiersNum, FromOffset, UntilOffset) ->
    BeginTime = ?CURRENT_DATETIME(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Metadata = ozt_handles:example_input_metadata(MetadataPrefix),
    IdentifiersExistsFlag = setup_test_for_harvesting_with_deletion(
        Config, IdentifiersNum, BeginTime, TimeOffsets, Metadata, MetadataPrefix
    ),
    list_with_time_offsets_include_deleted_test_base(Config, Method, <<"ListIdentifiers">>, IdentifiersExistsFlag,
        BeginTime, FromOffset, UntilOffset, MetadataPrefix).


list_resumption_token_test_base(Config, Method, Verb, IdentifiersNum) ->
    BeginTime = ?CURRENT_DATETIME(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Metadata = ozt_handles:example_input_metadata(MetadataPrefix),
    Identifiers = setup_test_for_harvesting(
        Config, IdentifiersNum, BeginTime, TimeOffsets, Metadata, MetadataPrefix
    ),
    Args = prepare_harvesting_args(MetadataPrefix, undefined, undefined),
    BuildExpectedObject = fun(HandleId, TimeOffset) ->
        Timestamp = increase_timestamp(BeginTime, TimeOffset),
        case Verb of
            <<"ListIdentifiers">> ->
                expected_oai_header_xml(Config, HandleId, Timestamp);
            <<"ListRecords">> ->
                ExpectedMetadata = expected_final_metadata_element(MetadataPrefix, HandleId),
                expected_oai_record_xml(Config, HandleId, Timestamp, ExpectedMetadata)
        end
    end,
    ExpIdentifiersAndTimeOffsets = lists:zip(Identifiers, TimeOffsets),
    ?assert(check_list_entries_continuously_with_resumption_token(Config, Method, Verb, ExpIdentifiersAndTimeOffsets,
        Args, BuildExpectedObject)).


list_deleted_test_base(Config, Method, Verb) ->
    User = ozt_users:create(),
    Space1 = ozt_users:create_space_for(User, ?SPACE_NAME1),
    ShareId = datastore_key:new(),
    {ok, ShareId} = oz_test_utils:create_share(Config, ?USER(User), ShareId, ShareId, Space1),
    HSId = ozt_users:create_handle_service_for(User),
    DateTime = ?CURRENT_DATETIME(),
    DateTime2 = increase_timestamp(DateTime, 1),
    Identifier = ozt_users:create_handle_for(User, HSId, ShareId),
    #od_handle{metadata_prefix = MetadataPrefix} = ozt_handles:get(Identifier),
    Args = prepare_harvesting_args(MetadataPrefix, undefined, undefined, undefined, true),

    ExpResponseContent = case Verb of
        <<"ListIdentifiers">> ->
            [expected_oai_header_xml(Config, Identifier, DateTime)];
        <<"ListRecords">> ->
            ExpectedMetadata = expected_final_metadata_element(MetadataPrefix, Identifier),
            [expected_oai_record_xml(Config, Identifier, DateTime, ExpectedMetadata)]

    end,
    ?assert(check_oai_request(200, Verb, Args, Method, ExpResponseContent, binary_to_atom(Verb), Config)),

    delete_handle_with_mocked_timestamp(Config, Identifier, DateTime2),
    ExpResponseContent2 = case Verb of
        <<"ListIdentifiers">> ->
            [expected_oai_header_xml(Config, Identifier, DateTime2, HSId, deleted)];
        <<"ListRecords">> ->
            [expected_oai_record_xml(Config, Identifier, DateTime2, HSId, deleted)]
    end,

    ?assert(check_oai_request(200, Verb, Args, Method, ExpResponseContent2, binary_to_atom(Verb), Config)).


list_no_resumption_token_test_base(Config, Method, Verb, IdentifiersNum) ->
    BeginTime = ?CURRENT_DATETIME(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Metadata = ozt_handles:example_input_metadata(MetadataPrefix),
    Identifiers = setup_test_for_harvesting(
        Config, IdentifiersNum, BeginTime, TimeOffsets, Metadata, MetadataPrefix
    ),

    Args = prepare_harvesting_args(MetadataPrefix, undefined, undefined),

    BuildExpectedObject = fun(HandleId, TimeOffset) ->
        Timestamp = increase_timestamp(BeginTime, TimeOffset),
        case Verb of
            <<"ListIdentifiers">> ->
                expected_oai_header_xml(Config, HandleId, Timestamp);
            <<"ListRecords">> ->
                ExpectedMetadata = expected_final_metadata_element(MetadataPrefix, HandleId),
                expected_oai_record_xml(Config, HandleId, Timestamp, ExpectedMetadata)
        end
    end,

    ?assert(check_list_entries(200, Verb, Args, Method, BuildExpectedObject, lists:zip(Identifiers, TimeOffsets), Config)).


list_identifiers_modify_timestamp_test_base(Config, Method, IdentifiersNum,
    FromOffset, UntilOffset, IdentifiersToBeModified) ->

    %% IdentifiersToBeModified is number of identifiers that will be modified
    %% so that their timestamps will be set to Until + 1 (if Until is undefined
    BeginTime = ?CURRENT_DATETIME(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Metadata = ozt_handles:example_input_metadata(MetadataPrefix),

    Identifiers = setup_test_for_harvesting(
        Config, IdentifiersNum, BeginTime, TimeOffsets, Metadata, MetadataPrefix
    ),
    list_with_time_offsets_test_base(Config, Method, <<"ListIdentifiers">>, Identifiers,
        TimeOffsets, BeginTime, FromOffset, UntilOffset, MetadataPrefix),

    TimeOffsets2 = lists:map(fun({T, N}) ->
        case N =< IdentifiersToBeModified of
            true -> exclude_offset_from_range(T, FromOffset, UntilOffset);
            _ -> T
        end
    end, lists:zip(TimeOffsets, lists:seq(1, length(TimeOffsets)))),
    modify_handles_with_mocked_timestamp(Config, Identifiers, BeginTime, TimeOffsets2, Metadata),
    list_with_time_offsets_test_base(Config, Method, <<"ListIdentifiers">>, Identifiers,
        TimeOffsets2, BeginTime, FromOffset, UntilOffset, MetadataPrefix).


list_records_test_base(Config, Method, IdentifiersNum, FromOffset, UntilOffset) ->
    BeginTime = ?CURRENT_DATETIME(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Metadata = ozt_handles:example_input_metadata(MetadataPrefix),
    Identifiers = setup_test_for_harvesting(
        Config, IdentifiersNum, BeginTime, TimeOffsets, Metadata, MetadataPrefix
    ),
    list_with_time_offsets_test_base(Config, Method, <<"ListRecords">>, Identifiers,
        TimeOffsets, BeginTime, FromOffset, UntilOffset, MetadataPrefix).


list_records_include_deleted_test_base(Config, Method, IdentifiersNum, FromOffset, UntilOffset) ->
    BeginTime = ?CURRENT_DATETIME(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Metadata = ozt_handles:example_input_metadata(MetadataPrefix),
    IdentifiersExistsFlag = setup_test_for_harvesting_with_deletion(
        Config, IdentifiersNum, BeginTime, TimeOffsets, Metadata, MetadataPrefix
    ),
    list_with_time_offsets_include_deleted_test_base(Config, Method, <<"ListRecords">>, IdentifiersExistsFlag,
        BeginTime, FromOffset, UntilOffset, MetadataPrefix).


list_records_modify_timestamp_test_base(Config, Method, IdentifiersNum,
    FromOffset, UntilOffset, IdentifiersToBeModified) ->

    %% IdentifiersToBeModified is number of identifiers that will be modified
    %% so that their timestamps will be set to Until + 1 (if Until is undefined

    BeginTime = ?CURRENT_DATETIME(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Metadata = ozt_handles:example_input_metadata(MetadataPrefix),
    Identifiers = setup_test_for_harvesting(
        Config, IdentifiersNum, BeginTime, TimeOffsets, Metadata, MetadataPrefix
    ),
    list_with_time_offsets_test_base(Config, Method, <<"ListRecords">>, Identifiers,
        TimeOffsets, BeginTime, FromOffset, UntilOffset, MetadataPrefix),

    TimeOffsets2 = lists:map(fun({T, N}) ->
        case N =< IdentifiersToBeModified of
            true -> exclude_offset_from_range(T, FromOffset, UntilOffset);
            _ -> T
        end
    end, lists:zip(TimeOffsets, lists:seq(1, length(TimeOffsets)))),
    modify_handles_with_mocked_timestamp(Config, Identifiers, BeginTime, TimeOffsets2, Metadata),
    list_with_time_offsets_test_base(Config, Method, <<"ListRecords">>, Identifiers,
        TimeOffsets2, BeginTime, FromOffset, UntilOffset, MetadataPrefix).


list_with_time_offsets_test_base(
    Config, Method, Verb, Identifiers, TimeOffsets, BeginTime, FromOffset, UntilOffset, MetadataPrefix
) ->
    BuildExpectedObject = fun(HandleId, TimeOffset) ->
        Timestamp = increase_timestamp(BeginTime, TimeOffset),
        case Verb of
            <<"ListIdentifiers">> ->
                expected_oai_header_xml(Config, HandleId, Timestamp);
            <<"ListRecords">> ->
                ExpectedMetadata = expected_final_metadata_element(MetadataPrefix, HandleId),
                expected_oai_record_xml(Config, HandleId, Timestamp, ExpectedMetadata)
        end
    end,

    From = to_datestamp(increase_timestamp(BeginTime, FromOffset)),
    Until = to_datestamp(increase_timestamp(BeginTime, UntilOffset)),
    Args = prepare_harvesting_args(MetadataPrefix, From, Until),

    IdsAndTimestamps = ids_and_timestamps_to_be_harvested(Identifiers, TimeOffsets, FromOffset, UntilOffset),
    ?assert(check_list_entries(200, Verb, Args, Method, BuildExpectedObject, IdsAndTimestamps, Config)),

    % check filtering by sets
    {ok, HandleServices} = oz_test_utils:list_handle_services(Config),
    lists:foreach(fun(HandleServiceId) ->
        IdsAndTimestampsInTheHService = lists:filter(fun({HandleId, _TimeOffset}) ->
            HandleServiceId =:= lookup_handle_service_of_handle(Config, HandleId)
        end, IdsAndTimestamps),

        ArgsWithSet = [{<<"set">>, HandleServiceId} | Args],
        case IdsAndTimestampsInTheHService of
            [] ->
                ?assert(check_list_entries_no_records_match_error(200, Verb, ArgsWithSet, Method, Config));
            _ ->
                ?assert(check_list_entries(
                    200, Verb, ArgsWithSet, Method, BuildExpectedObject, IdsAndTimestampsInTheHService, Config
                ))

        end
    end, HandleServices).


list_with_time_offsets_include_deleted_test_base(
    Config, Method, Verb, IdentifiersExistsFlag, BeginTime, FromOffset, UntilOffset, MetadataPrefix) ->
    BuildExpectedObject = fun(HandleId, TimeOffset, HSId, ExistsFlag) ->
        Timestamp = increase_timestamp(BeginTime, TimeOffset),
        case Verb of
            <<"ListIdentifiers">> ->
                case ExistsFlag of
                    true -> expected_oai_header_xml(Config, HandleId, Timestamp);
                    false -> expected_oai_header_xml(Config, HandleId, Timestamp, HSId, deleted)
                end;
            <<"ListRecords">> ->
                case ExistsFlag of
                    true ->
                        ExpectedMetadata = expected_final_metadata_element(MetadataPrefix, HandleId),
                        expected_oai_record_xml(Config, HandleId, Timestamp, ExpectedMetadata);
                    false ->
                        expected_oai_record_xml(Config, HandleId, Timestamp, HSId, deleted)
                end
        end
    end,

    From = to_datestamp(increase_timestamp(BeginTime, FromOffset)),
    Until = to_datestamp(increase_timestamp(BeginTime, UntilOffset)),
    Args = prepare_harvesting_args(MetadataPrefix, From, Until, undefined, true),

    IdsAndTimestamps = lists:keysort(2, ids_exs_flag_and_timestamps_to_be_harvested(
        IdentifiersExistsFlag, FromOffset, UntilOffset)
    ),

    %%  if FromOffset =:= UntilOffset =:= creation of first entry and this entry will be deleted than
    %%  result of listing will be empty list
    case IdsAndTimestamps of
        [] ->
            ?assert(check_list_entries_no_records_match_error(200, Verb, Args, Method, Config));
        _ ->
            ?assert(check_list_entries_including_deleted(200, Verb, Args, Method, BuildExpectedObject, IdsAndTimestamps, Config))
    end,

    % check filtering by sets
    {ok, HandleServices} = oz_test_utils:list_handle_services(Config),
    lists:foreach(fun(HandleServiceId) ->
        IdsAndTimestampsInTheHService = lists:filter(fun({_HandleId, _TimeOffset, HSId, _ExistsFlag}) ->
            HandleServiceId =:= HSId
        end, IdsAndTimestamps),

        ArgsWithSet = [{<<"set">>, HandleServiceId} | Args],
        case IdsAndTimestampsInTheHService of
            [] ->
                ?assert(check_list_entries_no_records_match_error(200, Verb, ArgsWithSet, Method, Config));
            _ ->
                ?assert(check_list_entries_including_deleted(
                    200, Verb, ArgsWithSet, Method, BuildExpectedObject, IdsAndTimestampsInTheHService, Config
                ))

        end
    end, HandleServices).


list_sets_test_base(Config, Method) ->
    {ok, User} = oz_test_utils:create_user(Config),

    ExpectedHandleServiceIdsAndNames = lists:sort(lists:map(fun(_) ->
        create_handle_service(Config, User)
    end, lists:seq(1, rand:uniform(10)))),

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
    Args = [{<<"identifier">>, ?SHARE_ID}],
    %% will perform GetRecord, metadataPrefix is missing
    ?assert(check_missing_arg_error(200, Args, Method, [], Config)).

id_not_existing_test_base(Config, Method) ->
    Args = [
        {<<"identifier">>, ?SHARE_ID},
        {<<"metadataPrefix">>, ?RAND_METADATA_PREFIX()}
    ],
    ?assert(check_id_not_existing_error(200, Args, Method, [], Config)).

cannot_disseminate_format_test_base(Config, Method) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, Space1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    ShareId = datastore_key:new(),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?USER(User), ShareId, ShareId, Space1
    ),
    {HSId, _} = create_handle_service(Config, User),
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Metadata = ozt_handles:example_input_metadata(MetadataPrefix),
    Identifier = create_handle(Config, User, HSId, ShareId, MetadataPrefix, Metadata),

    Args = [
        {<<"identifier">>, oai_identifier(Config, Identifier)},
        {<<"metadataPrefix">>, <<"not_supported_format">>}
    ],
    ?assert(check_cannot_disseminate_format_error(200, Args, Method, [], Config)).

exclusive_resumption_token_required_error(Config, Method) ->
    Args = [
        {<<"metadataPrefix">>, ?RAND_METADATA_PREFIX()},
        {<<"resumptionToken">>, <<"example_token">>}
    ],
    ?assert(check_exclusive_resumption_token_required_error(200, Args, Method, [], Config)).

list_metadata_formats_no_format_error_test_base(Config, Method) ->
    {ok, User} = oz_test_utils:create_user(Config),
    {ok, Space1} = oz_test_utils:create_space(Config, ?USER(User), ?SPACE_NAME1),
    ShareId = datastore_key:new(),
    {ok, ShareId} = oz_test_utils:create_share(
        Config, ?USER(User), ShareId, ShareId, Space1
    ),
    {HSId, _} = create_handle_service(Config, User),
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Metadata = ozt_handles:example_input_metadata(MetadataPrefix),
    Identifier = create_handle(Config, User, HSId, ShareId, MetadataPrefix, Metadata),
    % Modify handle metadata to undefined (this should not occur in normal
    % conditions because entity logic won't accept undefined metadata,
    % but check if returned OAI error in such case is correct).
    ?assertMatch({ok, _}, oz_test_utils:call_oz(Config, od_handle, update, [
        Identifier, fun(Handle = #od_handle{}) ->
            {ok, Handle#od_handle{metadata = undefined}}
        end]
    )),

    Args = [{<<"identifier">>, oai_identifier(Config, Identifier)}],

    ?assert(check_list_metadata_formats_error(200, Args, Method, [], Config)).

list_identifiers_empty_repository_error_test_base(Config, Method) ->
    Args = [{<<"metadataPrefix">>, ?RAND_METADATA_PREFIX()}],
    ?assert(check_list_entries_no_records_match_error(200, <<"ListIdentifiers">>, Args, Method, Config)).

list_identifiers_no_records_match_error_test_base(Config, Method, IdentifiersNum, FromOffset, UntilOffset) ->

    BeginTime = ?CURRENT_DATETIME(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Metadata = ozt_handles:example_input_metadata(MetadataPrefix),
    setup_test_for_harvesting(Config, IdentifiersNum, BeginTime,
        TimeOffsets, Metadata, MetadataPrefix),

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

list_identifiers_wrong_from_date_format_error_test_base(Config, Method, From, Until) ->

    Args = prepare_harvesting_args(?RAND_METADATA_PREFIX(), From, Until),
    ?assert(check_list_identifiers_bad_argument_invalid_from_date_format_error(200, Args, Method, [], Config)).

list_identifiers_wrong_until_date_format_error_test_base(Config, Method, From, Until) ->

    Args = prepare_harvesting_args(?RAND_METADATA_PREFIX(), From, Until),
    ?assert(check_list_identifiers_bad_argument_invalid_until_date_format_error(200, Args, Method, [], Config)).

list_records_no_records_match_error_test_base(Config, Method, IdentifiersNum, FromOffset, UntilOffset) ->

    BeginTime = ?CURRENT_DATETIME(),
    TimeOffsets = lists:seq(0, IdentifiersNum - 1), % timestamps will differ with 1 second each
    MetadataPrefix = ?RAND_METADATA_PREFIX(),
    Metadata = ozt_handles:example_input_metadata(MetadataPrefix),
    setup_test_for_harvesting(Config, IdentifiersNum, BeginTime,
        TimeOffsets, Metadata, MetadataPrefix),

    From = to_datestamp(increase_timestamp(BeginTime, FromOffset)),
    Until = to_datestamp(increase_timestamp(BeginTime, UntilOffset)),
    Args = prepare_harvesting_args(?RAND_METADATA_PREFIX(), From, Until),

    ?assert(check_list_entries_no_records_match_error(200, <<"ListRecords">>, Args, Method, Config)).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    application:ensure_all_started(hackney),
    ozt:init_per_suite(Config, fun() ->
        ozt:set_env(default_handle_list_limit, ?TESTED_HANDLE_LIST_LIMIT)
    end).

init_per_testcase(_, Config) ->
    mock_handle_proxy(Config),
    Config.

end_per_testcase(_, Config) ->
    oz_test_utils:delete_all_entities(Config),
    ok = ozt:rpc(handles, purge_all_deleted_entry, []),
    unmock_handle_proxy(Config),
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
        {error, {badVerb, [missing_key, <<"verb">>]}}, Config).

check_empty_verb_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"">>, Args, Method, ExpResponseContent,
        {error, {badVerb, [not_legal_verb, <<"">>]}}, Config).

check_invalid_verb_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"invalid_verb">>, Args, Method, ExpResponseContent,
        {error, {badVerb, [not_legal_verb, <<"invalid_verb">>]}}, Config).

check_illegal_arg_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"Identify">>, Args, Method, ExpResponseContent,
        {error, {badArgument, [illegal_argument, ["k"]]}}, Config).

check_missing_arg_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"GetRecord">>, Args, Method, ExpResponseContent,
        {error, {badArgument, [missing_key, [<<"metadataPrefix">>]]}}, Config).

check_id_not_existing_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"GetRecord">>, Args, Method, ExpResponseContent,
        {error, {idDoesNotExist, [illegalId, <<"identifier">>]}}, Config).

check_cannot_disseminate_format_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"GetRecord">>, Args, Method, ExpResponseContent,
        {error, [cannotDisseminateFormat, <<"metadataPrefix">>]}, Config).

check_exclusive_resumption_token_required_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListRecords">>, Args, Method, ExpResponseContent,
        {error, {badArgument, exclusive_argument}}, Config
    ) andalso check_oai_request(Code, <<"ListIdentifiers">>, Args, Method,
        ExpResponseContent, {error, {badArgument, exclusive_argument}}, Config
    ).

check_list_metadata_formats(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListMetadataFormats">>, Args, Method,
        ExpResponseContent, 'ListMetadataFormats', Config).

check_list_metadata_formats_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListMetadataFormats">>, Args, Method,
        ExpResponseContent, {error, [noMetadataFormats, <<"identifier">>]}, Config).

check_list_identifiers_bad_argument_granularity_mismatch_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListIdentifiers">>, Args, Method, ExpResponseContent,
        {error, {badArgument, [granularity_mismatch, <<"from">>, <<"until">>]}}, Config
    ).

check_list_identifiers_bad_argument_invalid_from_date_format_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListIdentifiers">>, Args, Method, ExpResponseContent,
        {error, {badArgument, [invalid_date_format, <<"from">>]}}, Config
    ).

check_list_identifiers_bad_argument_invalid_until_date_format_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"ListIdentifiers">>, Args, Method, ExpResponseContent,
        {error, {badArgument, [invalid_date_format, <<"until">>]}}, Config
    ).

check_list_entries(Code, Verb, Args, Method, BuildExpectedObject, ExpListedEntries, Config) ->
    ListingOpts = oai_utils:request_arguments_to_handle_listing_opts(Args),
    {_, ExpResumptionToken} = ozt:rpc(handles, list, [ListingOpts]),

    ExpectedBase = lists:map(fun({HandleId, TimeOffset}) ->
        BuildExpectedObject(HandleId, TimeOffset)
    end, ExpListedEntries),
    ExpResponseContent = ExpectedBase ++ expected_response_body_wrt_resumption_token(ExpResumptionToken, ListingOpts),
    check_oai_request(Code, Verb, Args, Method, ExpResponseContent, binary_to_atom(Verb), Config).


check_list_entries_including_deleted(Code, Verb, Args, Method, BuildExpectedObject, ExpListedEntries, Config) ->
    ListingOpts = oai_utils:request_arguments_to_handle_listing_opts(Args),
    {_, ExpResumptionToken} = ozt:rpc(handles, list, [ListingOpts]),

    ExpectedBase = lists:map(fun({HandleId, TimeOffset, HSId, ExistsFlag}) ->
        BuildExpectedObject(HandleId, TimeOffset, HSId, ExistsFlag)
    end, ExpListedEntries),

    ExpResponseContent = ExpectedBase ++ expected_response_body_wrt_resumption_token(ExpResumptionToken, ListingOpts),

    check_oai_request(Code, Verb, Args, Method, ExpResponseContent, binary_to_atom(Verb), Config).

check_list_entries_no_records_match_error(Code, <<"ListIdentifiers">>, Args, Method, Config) ->
    check_oai_request(
        Code, <<"ListIdentifiers">>, Args, Method, [],
        {error, [noRecordsMatch, <<"from">>, <<"until">>, <<"set">>, <<"metadataPrefix">>]}, Config
    );
check_list_entries_no_records_match_error(Code, <<"ListRecords">>, Args, Method, Config) ->
    check_oai_request(
        Code, <<"ListRecords">>, Args, Method, [],
        {error, [noRecordsMatch, <<"from">>, <<"until">>, <<"set">>, <<"metadataPrefix">>]}, Config
    ).

check_list_entries_continuously_with_resumption_token(_Config, _Method, _Verb, _RemainingExpEntries, [], _BuildExpectedObject) ->
    true;
check_list_entries_continuously_with_resumption_token(Config, Method, Verb, RemainingExpEntries, Args, BuildExpectedObject) ->
    ExpListedEntries = lists:sublist(RemainingExpEntries, ?TESTED_HANDLE_LIST_LIMIT),
    ListingOpts = oai_utils:request_arguments_to_handle_listing_opts(Args),
    {_, ExpResumptionToken} = ozt:rpc(handles, list, [ListingOpts]),

    case check_list_entries(200, Verb, Args, Method, BuildExpectedObject, ExpListedEntries, Config) of
        true ->
            ArgsWithToken = add_to_args_if_defined(<<"resumptionToken">>, ExpResumptionToken, []),
            check_list_entries_continuously_with_resumption_token(
                Config, Method, Verb, lists:subtract(RemainingExpEntries, ExpListedEntries), ArgsWithToken, BuildExpectedObject
            );
        Other -> Other
    end.


check_oai_request(Code, Verb, Args, Method, ExpResponseContent, ResponseType, Config) ->
    URL = get_oai_pmh_URL(),
    Path = get_oai_pmh_api_path(),
    Args2 = case Verb of
        none -> Args;
        _ -> add_verb(Verb, Args)
    end,
    ResponseDate = ?CURRENT_DATETIME(),
    ExpectedBody = expected_body(ExpResponseContent, ResponseType, Args2, ResponseDate),

    QueryString = prepare_querystring(Args2),
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_new(Nodes, oai_handler, [passthrough]),
    test_utils:mock_expect(Nodes, oai_handler, generate_response_date_element,
        fun() ->
            {responseDate, list_to_binary(to_datestamp(ResponseDate))}
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
            body => oai_utils:encode_xml(ExpectedBody),
            headers => {contains, ?RESPONSE_CONTENT_TYPE_HEADER}
        }
    }),

    ok = test_utils:mock_validate_and_unload(Nodes, oai_handler),
    Check.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_oai_pmh_URL() ->
    str_utils:format_bin("http://~s", [ozt:get_domain()]).

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
        {error, ErrorSpec} -> expected_response_error(ErrorSpec, Args);
        Verb -> expected_response_verb(Verb, ExpectedResponse)
    end,

    ExpectedRequestElement = case ResponseType of
        %% when error is badVerb or badArgument request element
        %% should only contain request URL
        {error, {badVerb, _}} -> expected_request_element(RequestURL);
        {error, badVerb} -> expected_request_element(RequestURL);
        {error, {badArgument, _}} -> expected_request_element(RequestURL);
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
                content = [#xmlText{value = to_datestamp(ResponseDate)}]
            },
            ExpectedRequestElement,
            ExpectedResponseElement]
    }.

expected_response_error(ErrorSpecElem, Args) ->
    {Code, ErrorSpec} = expected_code_and_error_spec(ErrorSpecElem, Args),
    #oai_error{description = Description} = oai_errors:handle(ErrorSpec),
    #xmlElement{
        name = error,
        attributes = [#xmlAttribute{name = code, value = str_utils:to_list(Code)}],
        content = [#xmlText{value = binary_to_list(Description)}]
    }.

expected_code_and_error_spec(ErrorSpec, _) when is_atom(ErrorSpec) ->
    {ErrorSpec, ErrorSpec};
expected_code_and_error_spec(ErrorSpec, Args) when is_tuple(ErrorSpec) ->
    {Code, ErrorSpecElem} = ErrorSpec,
    {Code, expected_error_spec_base(ErrorSpecElem, Args)};
expected_code_and_error_spec(ErrorSpec, Args) when is_list(ErrorSpec) ->
    FullSpec = expected_error_spec_base(ErrorSpec, Args),
    {element(1, FullSpec), FullSpec}.

expected_error_spec_base(ErrorSpec, _Args) when is_atom(ErrorSpec) ->
    ErrorSpec;
expected_error_spec_base(ErrorSpec, Args) when is_list(ErrorSpec) ->
    ArgsKeys = [<<"identifier">>, <<"metadataPrefix">>, <<"from">>, <<"until">>, <<"set">>],
    ErrorSpecArgs = lists:map(fun(Element) ->
        case lists:member(Element, ArgsKeys) of
            true ->
                case lists:keyfind(Element, 1, Args) of
                    {Element, Value} -> Value;
                    false -> undefined
                end;
            false ->
                Element
        end
    end, tl(ErrorSpec)),
    list_to_tuple([hd(ErrorSpec) | ErrorSpecArgs]).

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
    lists:map(fun(SpaceName) ->
        {ok, SpaceId} = oz_test_utils:create_space(Config, Client, SpaceName),
        SpaceId
    end, SpacesNames).

create_shares(Config, SpaceIds) ->
    ShareIds = ?SHARE_IDS(length(SpaceIds)),
    lists:map(fun({ShareId, SpaceId}) ->
        {ok, ShareId} = oz_test_utils:create_share(
            Config, ?ROOT, ShareId, ShareId, SpaceId
        ),
        ShareId
    end, lists:zip(ShareIds, SpaceIds)).

create_handle_services(Config, User, Count) ->
    lists:map(fun(_) ->
        {HSId, _} = create_handle_service(Config, User),
        HSId
    end, lists:seq(1, Count)).

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
    ok = test_utils:mock_expect(Nodes, od_handle, current_timestamp, fun() ->
        time:datetime_to_seconds(Timestamp)
    end),
    ok = modify_handle(Config, HId, Metadata),
    ok = test_utils:mock_validate_and_unload(Nodes, od_handle).

delete_handle_with_mocked_timestamp(Config, HId, Timestamp) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, od_handle, [passthrough]),
    ok = test_utils:mock_expect(Nodes, od_handle, current_timestamp, fun() ->
        time:datetime_to_seconds(Timestamp)
    end),
    ozt:rpc(handle_logic, delete, [?ROOT, HId]),
    ok = test_utils:mock_validate_and_unload(Nodes, od_handle).

setup_test_for_harvesting(Config, RecordsNum, BeginTime, TimeOffsets, Metadata, MetadataPrefix) ->
    {ok, User} = oz_test_utils:create_user(Config),
    SpaceIds = create_spaces(Config, ?SPACE_NAMES(RecordsNum), ?USER(User)),
    ShareIds = create_shares(Config, SpaceIds),
    HandleServices = create_handle_services(Config, User, rand:uniform(RecordsNum)),
    create_handles_with_mocked_timestamps(
        Config, User, HandleServices, ShareIds, BeginTime, TimeOffsets, Metadata, MetadataPrefix
    ).

setup_test_for_harvesting_with_deletion(Config, RecordsNum, BeginTime, TimeOffsets, Metadata, MetadataPrefix) ->
    Identifiers = setup_test_for_harvesting(Config, RecordsNum, BeginTime, TimeOffsets, Metadata, MetadataPrefix),
    EnumeratedIdentifiers = lists:enumerate(lists:zip(Identifiers, TimeOffsets)),
    NumberToDelete = ?RAND_INT(1, RecordsNum - 1),
    ElementsToDelete = [?RAND_INT(1, RecordsNum) || _ <- lists:seq(1, NumberToDelete)],
    IdentifiersExistsFlag = lists:map(fun({Number, {Identifier, TimeOffset}}) ->
        HSId = lookup_handle_service_of_handle(Config, Identifier),
        case lists:member(Number, ElementsToDelete) of
            true ->
                NewTimestamp = increase_timestamp(BeginTime, TimeOffset + RecordsNum),
                delete_handle_with_mocked_timestamp(Config, Identifier, NewTimestamp),
                {Identifier, TimeOffset + RecordsNum, HSId, false};
            false ->
                {Identifier, TimeOffset, HSId, true}
        end
    end, EnumeratedIdentifiers),
    IdentifiersExistsFlag.

%% for each SpaceId in SpaceIds creates share,
%% adds metadata to this share and mock timestamp,
%% randomizing one of available handle services
create_handles_with_mocked_timestamps(Config, User, HandleServices, ResourceIds,
    BeginTime, TimeOffsets, Metadata, MetadataPrefix) ->
    lists:map(fun({ResourceId, TimeOffset}) ->
        MockedTimestamp = increase_timestamp(BeginTime, TimeOffset),
        HandleServiceId = lists_utils:random_element(HandleServices),
        create_handle_with_mocked_timestamp(Config, User, HandleServiceId, ResourceId, Metadata,
            MetadataPrefix, MockedTimestamp)
    end, lists:zip(ResourceIds, TimeOffsets)).

create_handle_service(Config, User) ->
    ok = oz_test_utils:user_set_oz_privileges(Config, User, [
        ?OZ_HANDLE_SERVICES_CREATE
    ], []),
    Name = str_utils:rand_hex(16),
    {ok, HSId} = oz_test_utils:create_handle_service(Config, ?USER(User),
        Name, ?PROXY_ENDPOINT, ?DOI_SERVICE_PROPERTIES
    ),
    {HSId, Name}.

lookup_handle_service_of_handle(Config, HandleId) ->
    {ok, #od_handle{handle_service = HandleServiceId}} = oz_test_utils:get_handle(Config, HandleId),
    HandleServiceId.

create_handle_with_mocked_timestamp(Config, User, HandleServiceId, ResourceId,
    Metadata, MetadataPrefix, Timestamp) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, od_handle, [passthrough]),
    ok = test_utils:mock_expect(Nodes, od_handle, current_timestamp, fun() ->
        time:datetime_to_seconds(Timestamp)
    end),
    HId = create_handle(Config, User, HandleServiceId, ResourceId, MetadataPrefix, Metadata),
    ok = test_utils:mock_validate_and_unload(Nodes, od_handle),
    HId.

create_handle(Config, User, HandleServiceId, ResourceId, MetadataPrefix, Metadata) ->
    {ok, HId} = oz_test_utils:create_handle(Config, ?USER(User),
        HandleServiceId, ?HANDLE_RESOURCE_TYPE, ResourceId, MetadataPrefix, Metadata),
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
            {ok, 201, [{?HDR_LOCATION, <<"/test_location">>}], <<"">>}
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
    prepare_harvesting_args(MetadataPrefix, From, Until, Set, false).

prepare_harvesting_args(MetadataPrefix, From, Until, Set, ExistsFlag) ->
    Args = add_to_args_if_defined(<<"metadataPrefix">>, MetadataPrefix, []),
    Args2 = add_to_args_if_defined(<<"from">>, From, Args),
    Args3 = add_to_args_if_defined(<<"until">>, Until, Args2),
    Args4 = add_to_args_if_defined(<<"set">>, Set, Args3),
    add_to_args_if_defined(<<"includeDeleted">>, ExistsFlag, Args4).

ids_and_timestamps_to_be_harvested(Identifiers, TimeOffsets, FromOffset, UntilOffset) ->
    lists:filter(fun({_Id, TimeOffset}) ->
        offset_in_range(FromOffset, UntilOffset, TimeOffset)
    end, lists:zip(Identifiers, TimeOffsets)).

ids_exs_flag_and_timestamps_to_be_harvested(IdentifiersExistsFlag, FromOffset, UntilOffset) ->
    lists:filter(fun({_Id, TimeOffset, _HSId, _ExistsFlag}) ->
        offset_in_range(FromOffset, UntilOffset, TimeOffset)
    end, IdentifiersExistsFlag).

increase_timestamp(_, undefined) -> undefined;
increase_timestamp(undefined, _) -> undefined;
increase_timestamp(Datetime, ExtraSeconds) ->
    Seconds = calendar:datetime_to_gregorian_seconds(Datetime),
    calendar:gregorian_seconds_to_datetime(Seconds + ExtraSeconds).

to_datestamp(undefined) -> undefined;
to_datestamp(DateTime) ->
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


oai_identifier(Config, HandleId) ->
    OnezoneDomain = oz_test_utils:oz_domain(Config),
    <<"oai:", OnezoneDomain/binary, ":", HandleId/binary>>.

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

% Resulting metadata should include additional identifiers - public handle and public share url
expected_dc_identifiers(Config, HandleId) ->
    {ok, #od_handle{
        resource_id = ShareId,
        public_handle = PublicHandle
    }} = oz_test_utils:get_handle(Config, HandleId),
    Name = 'dc:identifier',
    ShareUrl = oz_test_utils:get_share_public_url(Config, ShareId),
    [
        #xmlElement{
            name = Name,
            content = [#xmlText{value = binary_to_list(ShareUrl)}]
        },
        #xmlElement{
            name = Name,
            content = [#xmlText{value = binary_to_list(PublicHandle)}]
        }
    ].

expected_admin_emails(Config) ->
    oz_test_utils:get_env(Config, admin_emails).

expected_oai_record_xml(Config, HandleId, Timestamp, HSId, deleted) ->
    #xmlElement{name = record, content = [
        expected_oai_header_xml(Config, HandleId, Timestamp, HSId, deleted)
    ]}.

expected_oai_record_xml(Config, HandleId, Timestamp, ExpectedMetadata) ->
    #xmlElement{name = record, content = [
        expected_oai_header_xml(Config, HandleId, Timestamp),
        #xmlElement{
            name = metadata,
            content = [ExpectedMetadata]
        }
    ]}.

expected_oai_header_xml(Config, HandleId, Timestamp) ->
    expected_oai_header_xml(Config, HandleId, Timestamp,
        lookup_handle_service_of_handle(Config, HandleId)
    ).
expected_oai_header_xml(Config, HandleId, Timestamp, HSId) ->
    #xmlElement{
        name = header,
        content = [
            #xmlElement{
                name = identifier,
                content = [#xmlText{
                    value = binary_to_list(oai_identifier(Config, HandleId))
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
                    value = binary_to_list(HSId)
                }]
            }
        ]
    }.

expected_oai_header_xml(Config, HandleId, Timestamp, HSId, deleted) ->
    XMLBase = expected_oai_header_xml(Config, HandleId, Timestamp, HSId),
    XMLBase#xmlElement{
        attributes = [#xmlAttribute{name = status, value = "deleted"}]
    }.

%% @private
expected_final_metadata_element(MetadataPrefix, HandleId) ->
    ExpFinalMetadata = ozt_handles:expected_final_metadata(HandleId),
    ParsedXml = ?check(oai_metadata:parse_xml(ExpFinalMetadata)),
    ozt:rpc(oai_metadata, adapt_for_oai_pmh, [MetadataPrefix, ParsedXml]).


%%%-------------------------------------------------------------------
%%% @doc
%%% @private
%%% According to OAI-PMH spec, the token MUST be present in the response if an incomplete list is returned,
%%% and MUST be present and MUST be empty when the last batch that completes the list is returned.
%%% However, if the whole list is returned in one response, there should be no resumption token element at all.
%%% @end
%%%-------------------------------------------------------------------
-spec expected_response_body_wrt_resumption_token(handles:resumption_token(), handles:listing_opts()) ->
    [#xmlElement{}].
expected_response_body_wrt_resumption_token(undefined, ListingOpts) when not is_map_key(resumption_token, ListingOpts) ->
    [];
expected_response_body_wrt_resumption_token(ExpResumptionToken, _) ->
    [
        #xmlElement{name = resumptionToken,
            content = case ExpResumptionToken of
                undefined -> [];
                _ -> [#xmlText{value = binary_to_list(ExpResumptionToken)}]
            end
        }
    ].
