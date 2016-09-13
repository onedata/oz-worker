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

%% API
-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2, end_per_suite/1, oai_test/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([oai_test]).

oai_test(Config) ->




    ok.

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    application:start(etls),
    hackney:start(),
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    [Node1 | _] = ?config(oz_worker_nodes, NewConfig),
    OZ_IP_1 = test_utils:get_docker_ip(Node1),
    OAIAPIPrefix = get_oai_pmh_api_prefix(Node1),
    OAIAddress = str_utils:format("http://~s~s", [OZ_IP_1, OAIAPIPrefix]),
    [{oaiAddress, OAIAddress} | NewConfig].

init_per_testcase(_, Config) ->
    [Node | _] = ?config(oz_worker_nodes, Config),
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
%%% Internal functions
%%%===================================================================

get_oai_pmh_api_prefix(Node) ->
    {ok, OAIPrefix} = rpc:call(Node, application, get_env, [?APP_Name, oai_pmh_api_prefix]),
    OAIPrefix.

%%generate_cert_files() ->
%%    Prefix = "provider" ++ integer_to_list(erlang:system_time(micro_seconds)),
%%    KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
%%    CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
%%    CertFile = filename:join(?TEMP_DIR, Prefix ++ "_cert.pem"),
%%    os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
%%    os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
%%    {KeyFile, CSRFile, CertFile}.




oai_request(Verb, Args, Config, get) ->
    URL = ?config(aoi_pmh_api_prefix, Config),
    http_client:get(URl)


