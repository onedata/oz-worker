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
-include("registered_names.hrl").
-include_lib("xmerl/include/xmerl.hrl").


%% API
-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2, end_per_suite/1]).

%% Test functions
-export([
    identify_get_test/1, identify_post_test/1,
    no_verb_get_test/1, no_verb_post_test/1,
    invalid_verb_get_test/1, invalid_verb_post_test/1,
    empty_verb_get_test/1, empty_verb_post_test/1,
    illegal_arg_get_test/1, illegal_arg_post_test/1]).

%% useful macros
-define(CONTENT_TYPE_HEADER, [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}]).
-define(RESPONSE_CONTENT_TYPE_HEADER, [{<<"content-type">>, <<"text/xml">>}]).



%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    identify_get_test,
    identify_post_test,
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

%%%===================================================================
%%% Test base functions
%%%===================================================================

identify_test_base(Config, Method) ->

    [Node | _] = ?config(oz_worker_nodes, Config),
    Path = ?config(oai_pmh_path, Config),
    URL = ?config(oai_pmh_url, Config),
    ExpectedBaseURL = string:concat(get_domain(Node), binary_to_list(Path)),
    RequestURL = binary_to_list(<<URL/binary, Path/binary>>),

    ExpectedBody = #xmlElement{
        name = 'OAI-PMH',
        content = [
            #xmlElement{
                name = request,
                attributes = [#xmlAttribute{name = verb, value = "Identify"}],
                content = [#xmlText{value = RequestURL}]},
            #xmlElement{name = responseDate},
            #xmlElement{
                name = 'Identify',
                content = [
                    #xmlElement{name = repositoryName, content = [#xmlText{value = "unnamed"}]},
                    #xmlElement{name = baseURL, content = [#xmlText{value = ExpectedBaseURL}]},
                    #xmlElement{name = protocolVersion, content = [#xmlText{value = "2.0"}]},
                    #xmlElement{name = earliestDatestamp}, %todo how to check it
                    #xmlElement{name = deletedRecord, content = [#xmlText{value = "no"}]},
                    #xmlElement{name = granularity, content = [#xmlText{value = "YYYY-MM-DDThh:mm:ss:Z"}]},
                    #xmlElement{name = adminEmail, content = [#xmlText{value = "info@onedata.org"}]}
                ]
            }
        ]
    },
    ?assert(check_identify(200, [], Method, ExpectedBody, Config)).

no_verb_test_base(Config, Method) ->
    ExpectedBody = expected_body_bad_verb(Config),
    ?assert(check_no_verb_error(200, [], Method, ExpectedBody, Config)).

empty_verb_test_base(Config, Method) ->
    ExpectedBody = expected_body_bad_verb(Config),
    ?assert(check_empty_verb_error(200, [], Method, ExpectedBody, Config)).

invalid_verb_test_base(Config, Method) ->
    ExpectedBody = expected_body_bad_verb(Config),
    ?assert(check_invalid_verb_error(200, [], Method, ExpectedBody, Config)).

illegal_arg_test_base(Config, Method) ->
    ExpectedBody = expected_body_bad_argument(Config),
    ?assert(check_illegal_arg_error(200, [{"k", "v"}], Method, ExpectedBody, Config)).


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
    [{oai_pmh_url, get_oai_pmh_URL(Node1)},
        {oai_pmh_path, get_oai_pmh_api_path(Node1)} | NewConfig].

init_per_testcase(_, Config) ->
    rest_test_utils:set_config(Config),
    Config.

end_per_testcase(_, _Config) ->
    ok.
%%    {KeyFile, CSRFile, CertFile} = ?config(cert_files, Config),
%%    file:delete(KeyFile),
%%    file:delete(CSRFile),
%%    file:delete(CertFile).

end_per_suite(Config) ->
    hackney:stop(),
    application:stop(etls),
    test_node_starter:clean_environment(Config).

%%%===================================================================
%%% Functions used to validate REST calls
%%%=================================================================

check_identify(Code, Args, Method, ExpectedBody, Config) ->
    check_oai_request(Code, <<"Identify">>, Args, Method, ExpectedBody, Config).

check_no_verb_error(Code, Args, Method, ExpectedBody, Config) ->
    check_oai_request(Code, none, Args, Method, ExpectedBody, Config).

check_empty_verb_error(Code, Args, Method, ExpectedBody, Config) ->
    check_oai_request(Code, <<"">>, Args, Method, ExpectedBody, Config).

check_invalid_verb_error(Code, Args, Method, ExpectedBody, Config) ->
    check_oai_request(Code, <<"invalid_verb">>, Args, Method, ExpectedBody, Config).

check_illegal_arg_error(Code, Args, Method, ExpectedBody, Config) ->
    check_oai_request(Code, <<"Identify">>, Args, Method, ExpectedBody, Config).

check_oai_request(Code, Verb, Args, Method, ExpectedBody, Config) ->
    
    URL = ?config(oai_pmh_url, Config),
    Path = ?config(oai_pmh_path, Config),
    Args2 = case Verb of
        none -> Args;
        _ -> add_verb(Verb, Args)
    end,
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

expected_body_bad_verb(Config) ->
    expected_body_error(Config, badVerb).

expected_body_bad_argument(Config) ->
    expected_body_error(Config, badArgument).

expected_body_error(Config, Code) ->
    Path = ?config(oai_pmh_path, Config),
    URL = ?config(oai_pmh_url, Config),
    RequestURL = binary_to_list(<<URL/binary, Path/binary>>),
    #xmlElement{
        name = 'OAI-PMH',
        content = [
            #xmlElement{name = request, content = [#xmlText{value = RequestURL}]},
            #xmlElement{name = responseDate},
            #xmlElement{
                name = error,
                attributes = [
                    #xmlAttribute{
                        name = code,
                        value = str_utils:to_list(Code)
                    }
                ]
            }
        ]
    }.


%% TODO
%% TODO * empty verb test
