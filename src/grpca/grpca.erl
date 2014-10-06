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
-include_lib("ctool/include/logging.hrl").

-define(REQUEST_TIMEOUT, timer:seconds(10)).
-define(CRL_REGENERATION_PERIOD, timer:hours(1)).
-define(CACERT_FILE, "cacert.pem").
-define(CAKEY_FILE, filename:join("private", "cakey.pem")).

-record(dn, {commonName,
    organizationalUnitName = "REST",
    organizationName = "onedata",
    localityName = "Krakow",
    countryName = "PL",
    emailAddress = "rest@onedata.com"}).


%% ====================================================================
%% API
%% ====================================================================
-export([start/4, stop/0, sign_provider_req/2, loop/1, verify_provider/1,
    cacert_path/1, revoke/1, gen_crl/0]).


%% ====================================================================
%% API functions
%% ====================================================================


%% cacert_path/1
%% ====================================================================
%% @doc Returns a path to a CA Certificate based on a given CA directory.
%% ====================================================================
-spec cacert_path(CaDir :: string()) -> string().
%% ====================================================================
cacert_path(CaDir) ->
    {ok, CaDir} = application:get_env(?APP_Name, grpca_dir),
    filename:join(CaDir, ?CACERT_FILE).


%% start/4
%% ====================================================================
%% @doc Starts a GRPCA process which handles all CA duties.
%% ====================================================================
-spec start(CaDir :: string(), CertPath :: string(), KeyPath :: string(),
    Domain :: string()) -> ok.
%% ====================================================================
start(CADir, CertPath, KeyPath, Domain) ->
    case filelib:is_regular(CertPath) of
        true -> ok;
        false ->
            generate_gr_cert(CADir, CertPath, KeyPath, Domain)
    end,
    register(ca_loop, spawn(?MODULE, loop, [CADir])),
    ca_loop ! schedule_crl_gen,
    ok.


%% stop/0
%% ====================================================================
%% @doc Stops the GRPCA process.
%% ====================================================================
-spec stop() -> ok.
%% ====================================================================
stop() ->
    ca_loop ! stop,
    ok.


%% sign_provider_req/2
%% ====================================================================
%% @doc Signs CSR from a provider, returning a new certificate.
%% The CSR's DN will be overriden by the GRPCA; most importantly the
%% Common Name will be set to the Provider's ID.
%% @end
%% ====================================================================
-spec sign_provider_req(ProviderId :: binary(), CSRPem :: binary()) ->
    {ok, CertPem :: binary(), Serial :: binary()}.
%% ====================================================================
sign_provider_req(ProviderId, CSRPem) ->
    case delegate(fun sign_provider_req_imp/3, [ProviderId, CSRPem]) of
        {ok, CertPem, Serial} when is_binary(CertPem), is_integer(Serial) ->
            {ok, CertPem, integer_to_binary(Serial, 16)}
    end.


%% verify_provider/1
%% ====================================================================
%% @doc Verifies provider's certificate, returning Provider's ID if the
%% certificate is valid.
%% @end
%% ====================================================================
-spec verify_provider(PeerCertDer :: public_key:der_encoded()) ->
    {ok, ProviderId :: binary()} | {error, {bad_cert, Reason :: any()}}.
%% ====================================================================
verify_provider(PeerCertDer) ->
    case delegate(fun verify_provider_imp/2, [PeerCertDer]) of
        {ok, ProviderId} = Result when is_binary(ProviderId) -> Result;
        {error, {bad_cert, _Reason}} = Result -> Result
    end.


%% gen_crl/0
%% ====================================================================
%% @doc Generates a new CRL file.
%% ====================================================================
-spec gen_crl() -> ok.
%% ====================================================================
gen_crl() ->
    case delegate(fun gen_crl_imp/1, []) of
        ok -> ok
    end.


%% revoke/1
%% ====================================================================
%% @doc Revokes a certificate identified by a given serial.
%% ====================================================================
-spec revoke(Serial :: binary()) -> ok.
%% ====================================================================
revoke(Serial) ->
    case delegate(fun revoke_imp/2, [Serial]) of
        ok -> ok
    end.


%% generate_gr_cert/4
%% ====================================================================
%% @doc Generates a certificate for Global Registry's REST interface.
%% ====================================================================
-spec generate_gr_cert(CaDir :: string(), CertPath :: string(),
    KeyPath :: string(), Domain :: string()) -> ok.
%% ====================================================================
generate_gr_cert(CADir, CertPath, KeyPath, Domain) ->
    TmpDir = mochitemp:mkdtemp(),
    CSRFile = random_filename(TmpDir),
    ReqConfigFile = req_config_file(TmpDir, #dn{commonName = Domain}),
    CaConfigFile = ca_config_file(TmpDir, CADir),

    ?info("Creating a CSR for the Global Registry REST interface..."),

    RequestOutput = os:cmd(["openssl req",
        " -config ", ReqConfigFile,
        " -new ",
        " -keyout ", KeyPath,
        " -out ", CSRFile]),

    ?info("~s", [RequestOutput]),
    ?info("Signing the Global Resistry REST interface CSR..."),

    SigningOutput = os:cmd(["openssl ca",
        " -config ", CaConfigFile,
        " -batch",
        " -notext",
        " -extensions user_cert",
        " -in ", CSRFile,
        " -out ", CertPath]),

    ?info("~s", [SigningOutput]),

    mochitemp:rmtempdir(TmpDir),
    ok.


%% ====================================================================
%% Internal functions
%% ====================================================================


%% revoke_imp/2
%% ====================================================================
%% @doc The underlying implementation of {@link grpca:revoke/1}.
%% ====================================================================
-spec revoke_imp(Serial :: binary(), CaDir :: string()) -> ok.
%% ====================================================================
revoke_imp(Serial, CaDir) ->
    TmpDir = mochitemp:mkdtemp(),
    CaConfigFile = ca_config_file(TmpDir, CaDir),
    LSerial = utils:ensure_list(Serial),
    ?info("Revoking a certificate with serial number ~p", [Serial]),
    RevokeOutput = os:cmd(["openssl ca",
        " -config ", CaConfigFile,
        " -batch",
        " -revoke ", filename:join([CaDir, "newcerts", LSerial]), ".pem"]),
    ?info("~s", [RevokeOutput]),
    mochitemp:rmtempdir(TmpDir),
    gen_crl(),
    ok.


%% sign_provider_req_imp/3
%% ====================================================================
%% @doc The underlying implementation of {@link grpca:sign_provider_req/2}.
%% ====================================================================
-spec sign_provider_req_imp(ProviderId :: binary(), CSRPem :: binary(),
    CaDir :: string()) -> {ok, Pem :: binary(), Serial :: integer()}.
%% ====================================================================
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
        " -subj \"/CN=", utils:ensure_list(ProviderId), "/O=onedata/OU=Providers\"",
        " -in ", CSRFile,
        " -out ", CertFile]),

    {ok, Pem} = file:read_file(CertFile),
    [{'Certificate', CertDer, not_encrypted}] = public_key:pem_decode(Pem),
    Cert = public_key:pkix_decode_cert(CertDer, otp),
    #'OTPCertificate'{tbsCertificate = #'OTPTBSCertificate'{serialNumber = Serial}} = Cert,
    mochitemp:rmtempdir(TmpDir),

    {ok, Pem, Serial}.


%% verify_provider_imp/2
%% ====================================================================
%% @doc The underlying implementation of {@link grpca:verify_provider_imp/1}.
%% ====================================================================
-spec verify_provider_imp(PeerCertDer :: public_key:der_encoded(),
    CaDir :: string()) -> {ok, ProviderId :: binary()} | {error, {bad_cert, Reason :: any()}}.
%% ====================================================================
verify_provider_imp(PeerCertDer, CaDir) ->
    CaCertFile = cacert_path(CaDir),
    {ok, CaCertPem} = file:read_file(CaCertFile),
    [{'Certificate', CaCertDer, not_encrypted}] = public_key:pem_decode(CaCertPem),
    #'OTPCertificate'{} = Cert = public_key:pkix_decode_cert(CaCertDer, otp),
    case public_key:pkix_path_validation(Cert, [PeerCertDer], [{max_path_length, 0}]) of
        {ok, _} ->
            PeerCert = public_key:pkix_decode_cert(PeerCertDer, otp),
            case check_revoked(CaCertDer, Cert, PeerCert) of
                valid -> {ok, get_provider_id(PeerCert)};
                BadCert -> {error, BadCert}
            end;

        Error -> Error
    end.


%% gen_crl_imp/1
%% ====================================================================
%% @doc The underlying implementation of {@link grpca:gen_crl/0}.
%% ====================================================================
-spec gen_crl_imp(CaDir :: string()) -> ok.
%% ====================================================================
gen_crl_imp(CaDir) ->
    TmpDir = mochitemp:mkdtemp(),
    CaConfigFile = ca_config_file(TmpDir, CaDir),
    ?info("Generating an updated CRL"),
    CreateCrlOutput = os:cmd(["openssl ca",
        " -config ", CaConfigFile,
        " -gencrl",
        " -out ", filename:join(CaDir, "crl.pem")]),
    ?info("~s", [CreateCrlOutput]),
    mochitemp:rmtempdir(TmpDir),
    ok.


%% req_config_file/2
%% ====================================================================
%% @doc Creates a temporary config file for creating Global Registry REST
%% certificate's CSR.
%% ====================================================================
-spec req_config_file(TmpDir :: string(), DN :: #dn{}) -> string().
%% ====================================================================
req_config_file(TmpDir, #dn{} = DN) ->
    Config = random_filename(TmpDir),
    ok = file:write_file(Config, req_cnf(DN)),
    Config.


%% ca_config_file/2
%% ====================================================================
%% @doc Creates a temporary config file for signing CSR requests.
%% ====================================================================
-spec ca_config_file(TmpDir :: string(), CaDir :: string()) -> string().
%% ====================================================================
ca_config_file(TmpDir, CaDir) ->
    Config = random_filename(TmpDir),
    ok = file:write_file(Config, ca_cnf(CaDir)),
    Config.


%% delegate/2
%% ====================================================================
%% @doc Delegates a request from the API to the GRPCA process.
%% ====================================================================
-spec delegate(Request :: function(), Args :: list()) -> Response :: any().
%% ====================================================================
delegate(Request, Args) ->
    ca_loop ! {self(), Request, Args},
    receive
        {ok, Response} -> Response;
        Whatever -> error({unexpected_message, Whatever})
    after ?REQUEST_TIMEOUT ->
        error(ca_loop_not_responding)
    end.


%% loop/1
%% ====================================================================
%% @doc The GRPCA process loop. @see start/4, @see stop/0 .
%% ====================================================================
-spec loop(CaDir :: string()) -> ok.
%% ====================================================================
loop(CaDir) ->
    receive
        {Requester, Fun, Args} ->
            Reply = (catch apply(Fun, Args ++ [CaDir])),
            Requester ! {ok, Reply},
            ?MODULE:loop(CaDir);

        schedule_crl_gen ->
            gen_crl_imp(CaDir),
            erlang:send_after(?CRL_REGENERATION_PERIOD, self(), schedule_crl_gen),
            ?MODULE:loop(CaDir);

        stop -> ok
    after timer:minutes(1) ->
        ?MODULE:loop(CaDir)
    end.


%% check_revoked/3
%% ====================================================================
%% @doc Checks whether the certificate has been revoked by the GRPCA.
%% ====================================================================
-spec check_revoked(CaCertDer :: public_key:der_encoded(),
                    CaCert :: #'OTPCertificate'{},
                    PeerCert :: #'OTPCertificate'{}) ->
    valid | {bad_cert, Reason :: any()}.
%% ====================================================================
check_revoked(CaCertDer, CaCert, PeerCert) ->
    {ok, CaDir} = application:get_env(?APP_Name, grpca_dir),

    {ok, CRLPem} = file:read_file(filename:join(CaDir, "crl.pem")),
    [{'CertificateList', CRLDer, not_encrypted}] = public_key:pem_decode(CRLPem),
    #'CertificateList'{} = CRL = public_key:der_decode('CertificateList', CRLDer),
    FakeDP = #'DistributionPoint'{cRLIssuer = asn1_NOVALUE},

    public_key:pkix_crls_validate(PeerCert, [{FakeDP, {CRLDer, CRL}}],
        [{issuer_fun, {fun(_, _, _, _) -> {ok, CaCert, [CaCertDer]} end, []}}]).


%% get_provider_id/1
%% ====================================================================
%% @doc Extracts Provider's ID out of the certificate's Common Name.
%% ====================================================================
-spec get_provider_id(Cert :: #'OTPCertificate'{}) -> ProviderId :: binary().
%% ====================================================================
get_provider_id(#'OTPCertificate'{} = Cert) ->
    #'OTPCertificate'{tbsCertificate =
        #'OTPTBSCertificate'{subject = {rdnSequence, Attrs}}} = Cert,

    [ProviderId] = lists:filtermap(fun([Attribute]) ->
        case Attribute#'AttributeTypeAndValue'.type of
            ?'id-at-commonName' ->
                {_, Id} = Attribute#'AttributeTypeAndValue'.value,
                {true, utils:ensure_binary(Id)};
            _ -> false
        end
    end, Attrs),

    ProviderId.


%% random_filename/1
%% ====================================================================
%% @doc Generates a random file name and returns a path residing under a given
%% directory.
%% @end
%% ====================================================================
-spec random_filename(TmpDir :: string()) -> string().
%% ====================================================================
random_filename(TmpDir) ->
    FileName = mochihex:to_hex(crypto:hash(sha, term_to_binary({make_ref(), now()}))),
    filename:join(TmpDir, FileName).


%% ====================================================================
%% Contents of configuration files.
%% ====================================================================


%% req_cnf/1
%% ====================================================================
%% @doc Returns a configuration for creating a CSR with given DN by the GRPCA.
%% ====================================================================
-spec req_cnf(DN :: #dn{}) -> Config :: iolist().
%% ====================================================================
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


%% ca_cnf/1
%% ====================================================================
%% @doc Returns a configuration for the GRPCA.
%% ====================================================================
-spec ca_cnf(CaDir :: string()) -> Config :: iolist().
%% ====================================================================
ca_cnf(CaDir) ->
    {ok, CertDomain} = application:get_env(?APP_Name, rest_cert_domain),
    {ok, RestPort} = application:get_env(?APP_Name, rest_port),
    Port = integer_to_binary(RestPort),
    ["# Purpose: Configuration for CAs.\n"
    "\n"
    "ROOTDIR                = ", CaDir, "\n"
    "default_ca             = ca\n"
    "\n"

    "[ca]\n"
    "dir                    = $ROOTDIR\n"
    "certs                  = $dir/certs\n"
    "new_certs_dir          = $dir/newcerts\n"
    "database               = $dir/index.txt\n"
    "serial                 = $dir/serial\n"
    "crl_dir                = $dir/crl\n"
    "crlnumber              = $dir/crlnumber\n"
    "crl                    = $dir/crl.pem\n"
    "RANDFILE               = $dir/private/RAND\n"
    "certificate            = $dir/", ?CACERT_FILE, "\n"
    "private_key            = $dir/", ?CAKEY_FILE, "\n"
    "\n"
    "x509_extensions        = user_cert\n"
    "crl_extensions         = crl_ext\n"
    "default_days           = 3600\n"
    "default_crl_days       = 1\n"
    "default_md             = sha1\n"
    "preserve               = no\n"
    "policy                 = policy_provider\n"
    "crlDistributionPoints  = URI:https://", CertDomain, ":", Port, "/crl.pem\n"
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
    "crlDistributionPoints  = URI:https://", CertDomain, ":", Port, "/crl.pem\n"
    "basicConstraints       = CA:false\n"
    "keyUsage               = nonRepudiation, digitalSignature, keyEncipherment\n"
    "subjectKeyIdentifier   = hash\n"
    "authorityKeyIdentifier = keyid,issuer:always\n"
    "subjectAltName         = email:copy\n"
    "issuerAltName          = issuer:copy\n"
    "\n"

    "[crl_ext]\n"
    "authorityKeyIdentifier = keyid:always, issuer:always"].
