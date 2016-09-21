%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(oai_test_SUITE).
-author("Jakub Kudzia").

-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/performance.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").
-include("oai_test_SUITE.hrl").

%% API
-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2, end_per_suite/1]).

%% Test functions
-export([
    identify_get_test/1, identify_post_test/1,
    no_verb_get_test/1, no_verb_post_test/1,
    invalid_verb_get_test/1, invalid_verb_post_test/1,
    empty_verb_get_test/1, empty_verb_post_test/1,
    illegal_arg_get_test/1, illegal_arg_post_test/1
    , get_record_get_test/1]).

%% useful macros
-define(CONTENT_TYPE_HEADER, [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}]).
-define(RESPONSE_CONTENT_TYPE_HEADER, [{<<"content-type">>, <<"text/xml">>}]).

%% Example test data
-define(ID1, <<"identifier1">>).
-define(SPACE_NAME1, <<"space1">>).
-define(DC_METADATA_PREFIX, <<"oai_dc">>).


%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    identify_get_test,
    identify_post_test,
    get_record_get_test,
    no_verb_get_test,
    no_verb_post_test,
    empty_verb_get_test,
    empty_verb_post_test,
    invalid_verb_get_test,
    invalid_verb_post_test,
    illegal_arg_get_test,
    illegal_arg_post_test
]).

%%%===================================================================
%%% Test functions
%%%===================================================================

identify_get_test(Config) ->
    identify_test_base(Config, get).

identify_post_test(Config) ->
    identify_test_base(Config, post).

get_record_get_test(Config) ->
%%    tracer:start(node()),
%%    tracer:trace_calls(rest_test_utils, compare_xml),
    get_record_test_base(Config, get).



%%%% Tests of error handling

no_verb_get_test(Config) ->
    no_verb_test_base(Config, get).
%%
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

%%%===================================================================
%%% Test base functions
%%%===================================================================

identify_test_base(Config, Method) ->

    [Node | _] = ?config(oz_worker_nodes, Config),
    Path = ?config(oai_pmh_path, Config),
    ExpectedBaseURL = string:concat(get_domain(Node), binary_to_list(Path)),

    ExpResponseContent = [
        #xmlElement{name = repositoryName, content = [#xmlText{value = "unnamed"}]},
        #xmlElement{name = baseURL, content = [#xmlText{value = ExpectedBaseURL}]},
        #xmlElement{name = protocolVersion, content = [#xmlText{value = "2.0"}]},
        #xmlElement{name = earliestDatestamp}, %todo how to check it
        #xmlElement{name = deletedRecord, content = [#xmlText{value = "no"}]},
        #xmlElement{name = granularity, content = [#xmlText{value = "YYYY-MM-DDThh:mm:ss:Z"}]},
        #xmlElement{name = adminEmail, content = [#xmlText{value = "info@onedata.org"}]}
    ],
    ?assert(check_identify(200, [], Method, ExpResponseContent, Config)).

get_record_test_base(Config, Method) ->

    [Node | _] = ?config(oz_worker_nodes, Config),

    {ok, UserWithSpaces1} = oz_test_utils:create_user(Config, #onedata_user{}),
    {ok, Space1} = oz_test_utils:create_space(Config, {user, UserWithSpaces1}, ?SPACE_NAME1),
    {ok, ?ID1} = oz_test_utils:create_share(Config, ?ID1, ?ID1, <<"root">>, Space1),
    ct:pal("SPACE ID: ~p~n", [Space1]),
    ok = oz_test_utils:modify_share_metadata(Config, ?ID1, ?DC_METADATA_XML, ?DC_METADATA_PREFIX),
    {#xmlElement{content = DCMetadata}, _} = xmerl_scan:string(binary_to_list(?DC_METADATA_XML)),

    Args = [
        {<<"identifier">>, ?ID1},
        {<<"metadataPrefix">>, ?DC_METADATA_PREFIX}
    ],

    ExpResponseContent = [
        #xmlElement{name=record, content=[
            #xmlElement{
                name=header,
                content=[
                    #xmlElement{
                        name=identifier,
                        content=[#xmlText{
                            value=binary_to_list(?ID1)
                        }]
                    }
                ]
            },
            #xmlElement{
                name=metadata,
                content=[
                    #xmlElement{
                        name = 'oai_dc:dc',
                        content = DCMetadata
                    }
                ]
            }
        ]}
    ],
    ?assert(check_get_record(200, Args, Method, ExpResponseContent, Config)).

no_verb_test_base(Config, Method) ->
    ?assert(check_no_verb_error(200, [], Method, [], Config)).

empty_verb_test_base(Config, Method) ->
    ?assert(check_empty_verb_error(200, [], Method, [], Config)).

invalid_verb_test_base(Config, Method) ->
    ?assert(check_invalid_verb_error(200, [], Method, [], Config)).

illegal_arg_test_base(Config, Method) ->
    ?assert(check_illegal_arg_error(200, [{"k", "v"}], Method, [], Config)).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    application:start(etls),
    hackney:start(),
    NewConfig = ?TEST_INIT(
        Config, ?TEST_FILE(Config, "env_desc.json"), [oz_test_utils]
    ),
    [Node1 | _] = ?config(oz_worker_nodes, NewConfig),
    [
        {oai_pmh_url, get_oai_pmh_URL(Node1)},
        {oai_pmh_path, get_oai_pmh_api_path(Node1)} | NewConfig
    ].

init_per_testcase(_, Config) ->
    rest_test_utils:set_config(Config),
    Config.

end_per_testcase(_, _Config) ->
    ok.

end_per_suite(Config) ->
    ok.
%%    hackney:stop(),
%%    application:stop(etls),
%%    test_node_starter:clean_environment(Config).

%%%===================================================================
%%% Functions used to validate REST calls
%%%=================================================================

check_identify(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"Identify">>, Args, Method, ExpResponseContent, 'Identify',Config).

check_get_record(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"GetRecord">>, Args, Method, ExpResponseContent, 'GetRecord',Config).

check_no_verb_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, none, Args, Method, ExpResponseContent, {error, badVerb}, Config).

check_empty_verb_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"">>, Args, Method, ExpResponseContent, {error, badVerb}, Config).

check_invalid_verb_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"invalid_verb">>, Args, Method, ExpResponseContent, {error, badVerb}, Config).

check_illegal_arg_error(Code, Args, Method, ExpResponseContent, Config) ->
    check_oai_request(Code, <<"Identify">>, Args, Method, ExpResponseContent, {error, badArgument}, Config).

check_oai_request(Code, Verb, Args, Method, ExpResponseContent, ResponseType, Config) ->

    URL = ?config(oai_pmh_url, Config),
    Path = ?config(oai_pmh_path, Config),
    Args2 = case Verb of
        none -> Args;
        _ -> add_verb(Verb, Args)
    end,
    ExpectedBody = expected_body(Config, ExpResponseContent, ResponseType, Args2),
    ct:pal("EXPECTED: ~p", [ExpectedBody]),

    QueryString = prepare_querystring(Args2),

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

    rest_test_utils:check_rest_call(#{
        request => Request,
        expect => #{
            code => Code,
            body => ExpectedBody,
            headers => {contains, ?RESPONSE_CONTENT_TYPE_HEADER}
        }
    }).

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_oai_pmh_URL(Node) ->
    OZ_IP_1 = test_utils:get_docker_ip(Node),
    str_utils:format_bin("http://~s", [OZ_IP_1]).

get_oai_pmh_api_path(Node) ->
    {ok, OAIPrefix} = rpc:call(Node, application, get_env, [?APP_Name, oai_pmh_api_prefix]),
    list_to_binary(OAIPrefix).

prepare_querystring(Proplist) ->
    lists:foldl(fun({K, V}, Acc) ->
        KBin = str_utils:to_binary(K),
        VBin = str_utils:to_binary(V),
        <<Acc/binary, KBin/binary, "=", VBin/binary, "&">>
    end, <<"">>, Proplist).

add_verb(Verb, Args) ->
    [{<<"verb">>, str_utils:to_binary(Verb)} | Args].

get_domain(Hostname) ->
    [_Node | Domain] = string:tokens(atom_to_list(Hostname), "."),
    string:join(Domain, ".").


expected_body(Config, ExpectedResponse, ResponseType, Args) ->
    Path = ?config(oai_pmh_path, Config),
    URL = ?config(oai_pmh_url, Config),
    RequestURL = binary_to_list(<<URL/binary, Path/binary>>),
    %% todo add namespace attributes

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
        content = [
            ExpectedRequestElement,
            #xmlElement{name = responseDate},
            ExpectedResponseElement
        ]
    }.

expected_response_error(Code) ->
  #xmlElement{
        name = error,
        attributes = [#xmlAttribute{name = code, value = str_utils:to_list(Code)}]
    }.

expected_response_verb(Verb, Content) ->
    #xmlElement{name = ensure_atom(Verb), content=Content}.

expected_request_element(RequestURL) ->
    expected_request_element(RequestURL, []).

expected_request_element(RequestURL, Args) ->
    Attributes = lists:map(fun({K, V}) ->
        #xmlAttribute{name=binary_to_atom(K, latin1), value=binary_to_list(V)}
    end, Args),
    #xmlElement{
        name = request,
        attributes = Attributes,
        content = [#xmlText{value = RequestURL}]
    }.

ensure_atom(Arg) when is_atom(Arg) -> Arg;
ensure_atom(Arg) when is_binary(Arg) -> binary_to_atom(Arg, latin1);
ensure_atom(Arg) when is_list(Arg) -> list_to_atom(Arg).