%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(identity_test_SUITE).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([
    oz_certs_published_after_refresh/1
]).

%% appends function name to id (atom) and yields binary
-define(ID(Id), list_to_binary(
    atom_to_list(Id) ++ "_" ++
        atom_to_list(element(2, element(2, process_info(self(), current_function))))
)).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    oz_certs_published_after_refresh
]).

oz_certs_published_after_refresh(Config) ->
    %% given
    [OZ1, OZ2, OZ3] = Nodes = ?config(oz_worker_nodes, Config),
    Cert1 = get_cert(OZ1),
    Cert2 = get_cert(OZ2),
    Cert3 = get_cert(OZ3),
    rpc:multicall(Nodes, worker_proxy, call, [identity_publisher_worker, refresh_published_pubkey]),

    %% when
    Results = [verify(OZ, Cert) || OZ <- [OZ1, OZ2, OZ3], Cert <- [Cert1, Cert2, Cert3]],

    %% then
    [?assertMatch(ok, Res) || Res <- Results].


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")).

init_per_testcase(_, _Config) ->
    _Config.

end_per_testcase(_, _Config) ->
    ok.

end_per_suite(Config) ->
    ok.
%%    test_node_starter:clean_environment(Config).


%%%===================================================================
%%% Internal functions
%%%===================================================================

publish(Node, Cert) ->
    rpc:call(Node, identity, publish, [Cert]).

verify(Node, Cert) ->
    rpc:call(Node, identity, verify, [Cert]).

get_cert(Node) ->
    {ok, IdentityCertFile} = rpc:call(Node, application, get_env, [?APP_Name, identity_cert_file]),
    rpc:call(Node, identity, read_cert, [IdentityCertFile]).

get_id(Node) ->
    identity:get_id(get_cert(Node)).

new_self_signed_cert(ID) ->
    TmpDir = utils:mkdtemp(),
    KeyFile = TmpDir ++ "/key.pem",
    CertFile = TmpDir ++ "/cert.pem",
    PassFile = TmpDir ++ "/pass",
    CSRFile = TmpDir ++ "/csr",
    DomainForCN = binary_to_list(ID),

    os:cmd(["openssl genrsa", " -des3 ", " -passout ", " pass:x ", " -out ", PassFile, " 2048 "]),
    os:cmd(["openssl rsa", " -passin ", " pass:x ", " -in ", PassFile, " -out ", KeyFile]),
    os:cmd(["openssl req", " -new ", " -key ", KeyFile, " -out ", CSRFile, " -subj ", "\"/CN=" ++ DomainForCN ++ "\""]),
    os:cmd(["openssl x509", " -req ", " -days ", " 365 ", " -in ", CSRFile, " -signkey ", KeyFile, " -out ", CertFile]),

    Cert = identity:read_cert(CertFile),
    utils:rmtempdir(TmpDir),
    Cert.