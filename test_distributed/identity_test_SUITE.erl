%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains identity service tests.
%%% @end
%%%-------------------------------------------------------------------
-module(identity_test_SUITE).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0]).
-export([init_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    oz_certs_published_after_refresh/1,
    oz_certs_obtainable_via_rest/1,
    certs_are_published_on_provider_registration_via_rest/1,
    provider_certs_are_updated_via_rest/1]).

%% appends function name to id (atom) and yields binary
-define(ID(Id), list_to_binary(
    atom_to_list(Id) ++ "_" ++
        atom_to_list(element(2, element(2, process_info(self(), current_function))))
)).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([
    oz_certs_published_after_refresh,
    oz_certs_obtainable_via_rest,
    certs_are_published_on_provider_registration_via_rest,
    provider_certs_are_updated_via_rest
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

oz_certs_obtainable_via_rest(Config) ->
    %% given
    [OZ1, OZ2, OZ3] = ?config(oz_worker_nodes, Config),
    Key1 = get_public_key(OZ1),
    Key2 = get_public_key(OZ2),
    Key3 = get_public_key(OZ3),

    %% when
    Result1 = get_public_key_rest(OZ1, get_id(OZ1)),
    Result2 = get_public_key_rest(OZ2, get_id(OZ2)),
    Result3 = get_public_key_rest(OZ3, get_id(OZ3)),

    %% then
    ?assertMatch(Key1, Result1),
    ?assertMatch(Key2, Result2),
    ?assertMatch(Key3, Result3).


certs_are_published_on_provider_registration_via_rest(Config) ->
    %% given
    [OZ1, OZ2, _] = ?config(oz_worker_nodes, Config),
    Cert1 = new_self_signed_cert(<<"certs_are_published_on_provider_registration_via_rest 1">>),
    Cert2 = new_self_signed_cert(<<"certs_are_published_on_provider_registration_via_rest 2">>),

    %% when
    register_provider_rest(OZ1, identity_utils:get_id(Cert1), identity_utils:get_public_key(Cert1)),
    register_provider_rest(OZ1, identity_utils:get_id(Cert2), identity_utils:get_public_key(Cert2)),

    %% then
    ?assertMatch(ok, verify(OZ1, Cert1)),
    ?assertMatch(ok, verify(OZ2, Cert1)),
    ?assertMatch(ok, verify(OZ1, Cert2)),
    ?assertMatch(ok, verify(OZ2, Cert2)).

%% todo: this is a-happy-path only as auth in rest_handler is mocked anyway
%% todo: remove rest_handler mock and test auth thoroughly
provider_certs_are_updated_via_rest(Config) ->
    %% given
    [OZ1, OZ2, _] = ?config(oz_worker_nodes, Config),
    Cert1 = new_self_signed_cert(<<"provider_certs_are_updateable_via_rest 1">>),
    Cert2 = new_self_signed_cert(<<"provider_certs_are_updateable_via_rest 2">>),
    Cert1bis = new_self_signed_cert(<<"provider_certs_are_updateable_via_rest 1">>),
    Cert2bis = new_self_signed_cert(<<"provider_certs_are_updateable_via_rest 2">>),
    register_provider_rest(OZ1, identity_utils:get_id(Cert1), identity_utils:get_public_key(Cert1)),
    register_provider_rest(OZ1, identity_utils:get_id(Cert2), identity_utils:get_public_key(Cert2)),

    %% when
    update_public_key_rest(OZ1, identity_utils:get_id(Cert1bis), identity_utils:get_public_key(Cert1bis)),
    update_public_key_rest(OZ1, identity_utils:get_id(Cert2bis), identity_utils:get_public_key(Cert2bis)),

    %% then
    ?assertMatch(ok, verify(OZ1, Cert1bis)),
    ?assertMatch(ok, verify(OZ2, Cert1bis)),
    ?assertMatch(ok, verify(OZ1, Cert2bis)),
    ?assertMatch(ok, verify(OZ2, Cert2bis)).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    [{?LOAD_MODULES, [oz_test_utils]} | Config].

init_per_testcase(_, Config) ->
    ssl:start(),
    hackney:start(),

    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_new(Nodes, rest_handler),
    test_utils:mock_expect(Nodes, rest_handler, is_authorized,
        fun(Req, State) ->
            {Bindings, _} = cowboy_req:bindings(Req),
            ResId = proplists:get_value(id, Bindings),
            {true, Req, setelement(2, State, ?PROVIDER(ResId))}
        end),

    Config.

end_per_testcase(_, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, [rest_handler]),

    hackney:stop(),
    ssl:stop(),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

verify(Node, Cert) ->
    rpc:call(Node, identity, verify, [Cert]).

get_cert(Node) ->
    {ok, IdentityCertFile} = rpc:call(Node, application, get_env, [?APP_NAME, identity_cert_file]),
    rpc:call(Node, identity_utils, read_cert, [IdentityCertFile]).

get_id(Node) ->
    identity_utils:get_id(get_cert(Node)).

get_public_key(Node) ->
    identity_utils:get_public_key(get_cert(Node)).

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

    Cert = identity_utils:read_cert(CertFile),
    utils:rmtempdir(TmpDir),
    Cert.

get_public_key_rest(OzNode, ID) ->
    RestAddress = get_rest_address(OzNode),
    EncodedID = binary_to_list(http_utils:url_encode(ID)),
    Endpoint = RestAddress ++ "/publickey/" ++ EncodedID,
    Opts = [{ssl_options, [{cacerts, ca_certs(OzNode)}]}],
    Response = http_client:request(get, Endpoint, #{}, [], Opts),

    ?assertMatch({ok, 200, _ResponseHeaders, _}, Response),
    {_, _, _, ResponseBody} = Response,
    Data = json_utils:decode(ResponseBody),
    EncodedPublicKey = proplists:get_value(<<"publicKey">>, Data),
    identity_utils:decode(EncodedPublicKey).

update_public_key_rest(OzNode, ID, PublicKey) ->
    RestAddress = get_rest_address(OzNode),
    EncodedID = binary_to_list(http_utils:url_encode(ID)),
    Endpoint = RestAddress ++ "/publickey/" ++ EncodedID,
    Encoded = identity_utils:encode(PublicKey),
    Body = json_utils:encode([{<<"publicKey">>, Encoded}]),
    Headers = #{<<"content-type">> => <<"application/json">>},
    Opts = [{ssl_options, [{cacerts, ca_certs(OzNode)}]}],
    Response = http_client:request(patch, Endpoint, Headers, Body, Opts),
    ?assertMatch({ok, 204, _, _}, Response).

register_provider_rest(OzNode, ID, PublicKey) ->
    RestAddress = get_rest_address(OzNode),
    EncodedID = binary_to_list(http_utils:url_encode(ID)),
    Endpoint = RestAddress ++ "/provider_data/" ++ EncodedID,
    Encoded = identity_utils:encode(PublicKey),
    Body = json_utils:encode([
        {<<"publicKey">>, Encoded},
        {<<"domain">>, <<"127.0.0.1">>}
    ]),
    Headers = #{<<"content-type">> => <<"application/json">>},
    Opts = [{ssl_options, [{cacerts, ca_certs(OzNode)}]}],
    Response = http_client:request(post, Endpoint, Headers, Body, Opts),
    ?assertMatch({ok, 204, _, _}, Response).

get_rest_address(OzNode) ->
    {ok, Domain} = test_utils:get_env(OzNode, ?APP_NAME, http_domain),
    RestPort = get_rest_port(OzNode),
    RestAPIPrefix = get_rest_api_prefix(OzNode),
    RestAddress = str_utils:format("https://~s:~B~s", [Domain, RestPort, RestAPIPrefix]),
    RestAddress.

get_rest_port(Node) ->
    {ok, RestPort} = rpc:call(Node, application, get_env, [?APP_NAME, gui_port]),
    RestPort.

get_rest_api_prefix(Node) ->
    {ok, RestAPIPrefix} = rpc:call(Node, application, get_env, [?APP_NAME, rest_api_prefix]),
    RestAPIPrefix.

ca_certs(OzNode) ->
    {ok, CaCertsDir} = rpc:call(OzNode, application, get_env, [?APP_NAME, cacerts_dir]),
    rpc:call(OzNode, cert_utils, load_ders_in_dir, [CaCertsDir]).