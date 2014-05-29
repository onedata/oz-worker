%% ===================================================================
%% @author Konrad Zemek
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc The main module implementing the logic behind Global Registry Provider
%% CA. This module's methods should be used to manipulate Providers'
%% certificates.
%% @end
%% ===================================================================
-module(grpca).
-author("Konrad Zemek").

-include("registered_names.hrl").
-include_lib("public_key/include/public_key.hrl").


-define(REQUEST_TIMEOUT, 10000).
-define(CACERT_FILE, "cacert.pem").
-define(CAKEY_FILE, filename:join("private", "cakey.pem")).

-record(dn, {commonName = "Global Registry REST",
    organizationalUnitName = "REST",
    organizationName = "OneData",
    localityName = "Krakow",
    countryName = "PL",
    emailAddress = "rest@onedata.com"}).


%% API
-export([start/4, stop/0, sign_provider_req/2, loop/1, verify_provider/1,
    cacert_path/1]).


-spec cacert_path(CaDir :: string()) -> string().
cacert_path(CaDir) ->
    {ok, CaDir} = application:get_env(?APP_Name, grpca_dir),
    filename:join(CaDir, ?CACERT_FILE).


-spec start(CaDir :: string(), CertPath :: string(), KeyPath :: string(),
    Domain :: string()) -> ok.
start(CADir, CertPath, KeyPath, Domain) ->
    case filelib:is_regular(CertPath) of
        true -> ok;
        false ->
            generate_gr_cert(CADir, CertPath, KeyPath, Domain)
    end,
    register(ca_loop, spawn(?MODULE, loop, [CADir])),
    ok.


-spec stop() -> ok.
stop() ->
    ca_loop ! stop,
    ok.


-spec sign_provider_req(ProviderId :: binary(), CSRPem :: binary()) ->
    {ok, CertPem :: binary()}.
sign_provider_req(ProviderId, CSRPem) ->
    delegate({sign_provider_req, ProviderId, CSRPem}).


-spec verify_provider(PeerCertDer :: public_key:der_encoded()) ->
    {ok, ProviderId :: binary()}.
verify_provider(PeerCertDer) -> %% @todo: CRLs
    delegate({verify_provider, PeerCertDer}).


-spec generate_gr_cert(CaDir :: string(), CertPath :: string(),
    KeyPath :: string(), Domain :: string()) -> ok.
generate_gr_cert(CADir, CertPath, KeyPath, Domain) ->
    TmpDir = mochitemp:mkdtemp(),
    CSRFile = random_filename(TmpDir),
    ReqConfigFile = req_config_file(TmpDir, #dn{commonName = Domain}),
    CaConfigFile = ca_config_file(TmpDir, CADir),

    io:format("~s~n", [os:cmd(["openssl req",
        " -config ", ReqConfigFile,
        " -new ",
        " -keyout ", KeyPath,
        " -out ", CSRFile])]),

    io:format("~s~n", [os:cmd(["openssl ca",
        " -config ", CaConfigFile,
        " -batch",
        " -notext",
        " -extensions user_cert",
        " -in ", CSRFile,
        " -out ", CertPath])]),

    mochitemp:rmtempdir(TmpDir),
    ok.


-spec sign_provider_req_imp(ProviderId :: binary(), CSRPem :: binary(),
                            CaDir :: string()) -> {ok, Pem :: binary()}.
sign_provider_req_imp(ProviderId, CSRPem, CaDir) ->
    TmpDir = mochitemp:mkdtemp(),
    CSRFile = random_filename(TmpDir),
    CertFile = random_filename(TmpDir),
    CaConfigFile = ca_config_file(TmpDir, CaDir),

    ok = file:write_file(CSRFile, CSRPem),
    os:cmd(["openssl ca",
        " -config ", CaConfigFile,
        " -batch",
        " -notext",
        " -extensions user_cert",
        " -subj \"/CN=", binary:bin_to_list(ProviderId), "/O=OneData/OL=Providers\"",
        " -in ", CSRFile,
        " -out ", CertFile]),

    Pem = file:read_file(CertFile),
    mochitemp:rmtempdir(TmpDir),

    {ok, Pem}.


-spec verify_provider_imp(PeerCertDer :: public_key:der_encoded(),
                          CaDir :: string()) -> {ok, ProviderId :: binary()}.
verify_provider_imp(PeerCertDer, CaDir) -> %% @todo: CRLs
    CaCertFile = cacert_path(CaDir),
    {ok, CaCertPem} = file:read_file(CaCertFile),
    [{'Certificate', CaCertDer, not_encrypted}] = public_key:pem_decode(CaCertPem),
    #'OTPCertificate'{} = Cert = public_key:pkix_decode_cert(CaCertDer, otp),
    case public_key:pkix_path_validation(Cert, [PeerCertDer], [{max_path_length, 0}]) of
        {ok, _} ->
            PeerCert = public_key:pkix_decode_cert(PeerCertDer, plain),
            {ok, get_provider_id(PeerCert)};
        Error -> Error
    end.


-spec req_config_file(TmpDir :: string(), DN :: #dn{}) -> string().
req_config_file(TmpDir, #dn{} = DN) ->
    Config = random_filename(TmpDir),
    ok = file:write_file(Config, req_cnf(DN)),
    Config.


-spec ca_config_file(TmpDir :: string(), CaDir :: string()) -> string().
ca_config_file(TmpDir, CaDir) ->
    Config = random_filename(TmpDir),
    ok = file:write_file(Config, ca_cnf(CaDir)),
    Config.


-spec delegate(Request :: atom()) -> Response :: any().
delegate(Request) ->
    ca_loop ! {self(), Request},
    receive
        {ok, Response} -> Response;
        Whatever -> error({unexpected_message, Whatever})
    after ?REQUEST_TIMEOUT ->
        error(ca_loop_not_responding)
    end.


-spec loop(CaDir :: string()) -> ok.
loop(CaDir) ->
    receive %% @todo: deduplication?
        {Requester, {sign_provider_req, ProviderId, CSRPem}} ->
            Reply = (catch sign_provider_req_imp(ProviderId, CSRPem, CaDir)),
            Requester ! Reply,
            loop(CaDir);

        {Requester, {verify_provider, ProviderId}} ->
            Reply = (catch verify_provider_imp(ProviderId, CaDir)),
            Requester ! Reply,
            loop(CaDir);

        stop -> ok
    after 60000 ->
        ?MODULE:loop(CaDir)
    end.


-spec get_provider_id(Cert :: #'Certificate'{}) -> ProviderId :: binary().
get_provider_id(#'Certificate'{} = Cert) ->
    #'Certificate'{tbsCertificate =
        #'TBSCertificate'{subject = {rdnSequence, Attrs}}} = Cert,

    [ProviderId] = lists:filtermap(fun([Attribute]) ->
        case Attribute#'AttributeTypeAndValue'.type of
            ?'id-at-commonName' ->
                Value = Attribute#'AttributeTypeAndValue'.value,
                {_, Id} = public_key:der_decode('X520CommonName', Value),
                {true, binary:list_to_bin(Id)};
            _ -> false
        end
    end, Attrs),

    ProviderId.


-spec random_filename(TmpDir :: string()) -> string().
random_filename(TmpDir) ->
    FileName = mochihex:to_hex(crypto:hash(sha, term_to_binary({make_ref(), now()}))),
    filename:join(TmpDir, FileName).


%%
%% Contents of configuration files
%%

req_cnf(DN) ->
    ["# Purpose: Configuration for requests (end users and CAs)."
    "\n"
    "ROOTDIR                = $ENV::ROOTDIR\n"
    "\n"

    "[req]\n"
    "input_password         = secret\n"
    "output_password        = secret\n"
    "default_bits           = 4096\n"
    "RANDFILE               = $ROOTDIR/RAND\n"
    "encrypt_key            = no\n"
    "default_md             = sha1\n"
    "#string_mask           = pkix\n"
    "x509_extensions        = ca_ext\n"
    "prompt                 = no\n"
    "distinguished_name     = name\n"
    "\n"

    "[name]\n"
    "commonName             = ", DN#dn.commonName, "\n"
    "organizationalUnitName = ", DN#dn.organizationalUnitName, "\n"
    "organizationName       = ", DN#dn.organizationName, "\n"
    "localityName           = ", DN#dn.localityName, "\n"
    "countryName            = ", DN#dn.countryName, "\n"
    "emailAddress           = ", DN#dn.emailAddress, "\n"
    "\n"

    "[ca_ext]\n"
    "basicConstraints       = critical, CA:true\n"
    "keyUsage               = cRLSign, keyCertSign\n"
    "subjectKeyIdentifier   = hash\n"
    "subjectAltName         = email:copy\n"].


ca_cnf(CaDir) ->
    ["# Purpose: Configuration for CAs.\n"
    "\n"
    "ROOTDIR                = ", CaDir, "\n"
    "default_ca             = ca\n"
    "\n"

    "[ca]\n"
    "dir                    = $ROOTDIR\n"
    "certs                  = $dir/certs\n"
    "crl_dir                = $dir/crl\n"
    "database               = $dir/index.txt\n"
    "new_certs_dir          = $dir/newcerts\n"
    "certificate            = $dir/", ?CACERT_FILE, "\n"
    "serial                 = $dir/serial\n"
    "crl                    = $dir/crl.pem\n"
    "private_key            = $dir/", ?CAKEY_FILE, "\n"
    "RANDFILE               = $dir/private/RAND\n"
    "\n"
    "x509_extensions        = user_cert\n"
    "default_days           = 3600\n"
    "default_md             = sha1\n"
    "preserve               = no\n"
    "policy                 = policy_provider\n"
    "\n"

    "[policy_provider]\n"
    "commonName             = optional\n"
    "organizationalUnitName = optional\n"
    "organizationName       = optional\n"
    "countryName            = optional\n"
    "localityName           = optional\n"
    "emailAddress           = optional\n"
    "\n"

    "[user_cert]\n"
    "basicConstraints       = CA:false\n"
    "keyUsage               = nonRepudiation, digitalSignature, keyEncipherment\n"
    "subjectKeyIdentifier   = hash\n"
    "authorityKeyIdentifier = keyid,issuer:always\n"
    "subjectAltName         = email:copy\n"
    "issuerAltName          = issuer:copy\n"
    "\n"].
