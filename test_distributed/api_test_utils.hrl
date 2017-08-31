%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Definitions of macros and records used in API (logic + REST) tests.
%%% @end
%%%-------------------------------------------------------------------
-author("Lukasz Opiola").

-ifndef(API_TEST_UTILS_HRL).
-define(API_TEST_UTILS_HRL, 1).

-type client() :: nobody | root | {user, UserId :: binary()} |
{provider, ProviderId :: binary(), KeyFile :: string(), CertFile :: string()}.

-type data_error() :: bad | empty | bad_token | bad_token_type |
id_not_found | id_occupied | relation_exists | relation_does_not_exist.

-type logic_expectation() :: ok_binary| {ok_binary, binary()} |
{ok_list, [term()]} | {ok_term, fun((Result :: term()) -> boolean())} |
{ok_env, fun((Result :: term()) -> boolean())} | {error_reason, term()}.

-record(client_spec, {
    correct = [] :: [client()],
    unauthorized = [] :: [client()],
    forbidden = [] :: [client()]
}).

-record(data_spec, {
    required = [] :: [Key :: binary()],
    optional = [] :: [Key :: binary()],
    at_least_one = [] :: [Key :: binary()],
    correct_values = #{} :: #{Key :: binary() => Value :: [binary()]},
    bad_values = [] :: [{Key :: binary(), Value :: term(), data_error()}]
}).

-record(rest_spec, {
    method = get :: get | patch | post | put | delete,
    path = <<"/">> :: binary() | [binary()],
    headers = undefined :: undefined | #{Key :: binary() => Value :: binary()},
    expected_code = undefined :: undefined | integer(),
    expected_headers = undefined :: undefined | #{} | {contains, #{}},
    expected_body = undefined :: undefined | #{} | {contains, #{}}
}).

-record(logic_spec, {
    operation = get :: create | get | update | delete,
    module = undefined :: module(),
    function = undefined :: atom(),
    % In args, special atoms 'client' and 'data' can be used. In this case,
    % client and data will be automatically injected in these placeholders.
    args = [] :: [term()],
    expected_result = undefined :: undefined | logic_expectation()
}).

-record(gs_spec, {
    operation = get :: create | get | update | delete,
    gri,
    subscribe = false :: boolean(),
    auth_hint = undefined,
    expected_result
}).

-record(api_test_spec, {
    client_spec = undefined :: undefined | #client_spec{},
    rest_spec = undefined :: undefined | #rest_spec{},
    logic_spec = undefined :: undefined | #logic_spec{},
    gs_spec = undefined :: undefined | #gs_spec{},
    data_spec = undefined :: undefined | #data_spec{}
}).

% Convenience macros for expressing logic result expectations
-define(OK, ok).
-define(TRUE, true).
-define(OK_BINARY, ok_binary).
-define(OK_BINARY(__ExactValue), {ok_binary, __ExactValue}).
-define(OK_MAP(__ExactValue), {ok_map, __ExactValue}).
-define(OK_MAP_CONTAINS(__Contains), {ok_map_contains, __Contains}).
-define(OK_LIST(__ExpectedList), {ok_list, __ExpectedList}).
-define(OK_LIST_CONTAINS(__ExpectedList), {ok_list_contains, __ExpectedList}).
-define(OK_LIST_DOESNT_CONTAIN(__ExpectedList),
    {ok_list_doesnt_contain, __ExpectedList}).
-define(OK_TERM(__VerifyFun), {ok_term, __VerifyFun}).
-define(OK_ENV(__PrepareFun), {ok_env, __PrepareFun}).
-define(ERROR_REASON(__ExpectedError), {error_reason, __ExpectedError}).

% Convenience macros to use when dealing with handles/handle services
-define(PROXY_ENDPOINT, <<"172.17.0.9:8080/api/v1">>).

-define(DOI_SERVICE,
    #{
        <<"name">> => <<"LifeWatch DataCite">>,
        <<"proxyEndpoint">> => ?PROXY_ENDPOINT,
        <<"serviceProperties">> => #{
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
        }
    }
).

-define(PID_SERVICE,
    #{
        <<"name">> => <<"iMarine EPIC">>,
        <<"proxyEndpoint">> => ?PROXY_ENDPOINT,
        <<"serviceProperties">> => #{
            <<"type">> => <<"PID">>,
            <<"endpoint">> => <<"https://epic.grnet.gr/api/v2/handles">>,
            <<"prefix">> => <<"11789">>,
            <<"suffixGeneration">> => <<"auto">>,
            <<"suffixPrefix">> => <<"{{space.name}}">>,
            <<"suffixSuffix">> => <<"{{user.name}}">>,
            <<"username">> => <<"alice">>,
            <<"password">> => <<"*******">>,
            <<"identifierTemplate">> => <<"{{space.name}}-{{space.guid}}">>,
            <<"allowTemplateOverride">> => false
        }
    }
).

-define(DC_METADATA, <<"<?xml version=\"1.0\"?>",
    "<metadata xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xmlns:dc=\"http:\/\/purl.org\/dc\/elements\/1.1\/\">"
    "<dc:title>Test dataset<\/dc:title>",
    "<dc:creator>John Johnson<\/dc:creator>",
    "<dc:creator>Jane Doe<\/dc:creator>",
    "<dc:subject>Test of datacite<\/dc:subject>",
    "<dc:description>Lorem ipsum lorem ipusm<\/dc:description>",
    "<dc:publisher>Onedata<\/dc:publisher>",
    "<dc:publisher>EGI<\/dc:publisher>",
    "<dc:date>2016<\/dc:date>",
    "<dc:format>application\/pdf<\/dc:format>",
    "<dc:identifier>onedata:LKJHASKFJHASLKDJHKJHuah132easd<\/dc:identifier>",
    "<dc:language>eng<\/dc:language>",
    "<dc:rights>CC-0<\/dc:rights>",
    "<\/metadata>">>).

-define(HANDLE(HandleServiceId, ResourceId),
    #{
        <<"handleServiceId">> => HandleServiceId,
        <<"resourceType">> => <<"Share">>,
        <<"resourceId">> => ResourceId,
        <<"metadata">> => ?DC_METADATA
    }
).

-endif.
