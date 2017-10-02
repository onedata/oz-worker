%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The main module implementing the logic behind onezone CA.
%%% This module's methods should be used to manipulate Providers' certificates.
%%% @end
%%%-------------------------------------------------------------------
-module(ozpca).
-author("Konrad Zemek").
-author("Krzysztof Trzepla").

-behaviour(gen_server).

-include("registered_names.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([start_link/0, sign_provider_req/2, verify_provider/1, revoke/1,
    gen_crl/0, cacert_path/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(INTERNAL_REQUEST_TIMEOUT, timer:seconds(10)).
-define(CRL_REGENERATION_PERIOD, timer:hours(1)).
-define(CACERT_FILE, "cacert.pem").
-define(CAKEY_FILE, filename:join("private", "cakey.pem")).

-record(dn, {
    commonName,
    organizationalUnitName = "onezone",
    organizationName = "onedata",
    localityName = "Krakow",
    countryName = "PL",
    emailAddress = "support@onedata.com"}
).

-record(state, {
    ca_dir :: string()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Signs CSR from a provider, returning a new certificate.
%% The CSR's DN will be overridden by the zone CA; most importantly the
%% Common Name will be set to the Provider's ID.
%% @end
%%--------------------------------------------------------------------
-spec sign_provider_req(ProviderId :: binary(), CSRPem :: binary()) ->
    {ok, {CertPem :: binary(), Serial :: binary()}} | {error, bad_csr}.
sign_provider_req(ProviderId, CSRPem) ->
    case delegate(fun sign_provider_req_imp/3, [ProviderId, CSRPem]) of
        {ok, CertPem, Serial} when is_binary(CertPem), is_integer(Serial) ->
            {ok, {CertPem, integer_to_binary(Serial, 16)}};
        {error, bad_csr} ->
            {error, bad_csr}
    end.

%%--------------------------------------------------------------------
%% @doc Verifies provider's certificate, returning Provider's ID if the
%% certificate is valid.
%% @end
%%--------------------------------------------------------------------
-spec verify_provider(PeerCertDer :: public_key:der_encoded()) ->
    {ok, ProviderId :: binary()} | {error, {bad_cert, Reason :: any()}}.
verify_provider(PeerCertDer) ->
    case delegate(fun verify_provider_imp/2, [PeerCertDer]) of
        {ok, ProviderId} = Result when is_binary(ProviderId) -> Result;
        {error, {bad_cert, _Reason}} = Result -> Result
    end.

%%--------------------------------------------------------------------
%% @doc Generates a new CRL file.
%% @end
%%--------------------------------------------------------------------
-spec gen_crl() -> ok.
gen_crl() ->
    case delegate(fun gen_crl_imp/1, []) of
        ok -> ok
    end.

%%--------------------------------------------------------------------
%% @doc Revokes a certificate identified by a given serial.
%% @end
%%--------------------------------------------------------------------
-spec revoke(Serial :: binary()) -> ok.
revoke(Serial) ->
    case delegate(fun revoke_imp/2, [Serial]) of
        ok -> ok
    end.

%%--------------------------------------------------------------------
%% @doc Returns a path to a CA Certificate based on a given CA directory.
%% @end
%%--------------------------------------------------------------------
-spec cacert_path(CaDir :: string()) -> string().
cacert_path(CaDir) ->
    {ok, CaDir} = application:get_env(?APP_NAME, ozpca_dir),
    filename:join(CaDir, ?CACERT_FILE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Initializes the server.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init(_Args) ->
    {ok, KeyFile} = application:get_env(?APP_NAME, oz_key_file),
    {ok, CertFile} = application:get_env(?APP_NAME, oz_cert_file),
    {ok, CaDir} = application:get_env(?APP_NAME, ozpca_dir),
    {ok, Domain} = application:get_env(?APP_NAME, http_domain),
    case filelib:is_regular(CertFile) of
        true -> ok;
        false -> generate_oz_cert(KeyFile, CertFile, CaDir, Domain)
    end,
    gen_crl_imp(CaDir),
    erlang:send_after(?CRL_REGENERATION_PERIOD, self(), gen_crl),
    {ok, #state{ca_dir = CaDir}}.


%%--------------------------------------------------------------------
%% @private @doc Handles call messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_call(Request, _From, State) ->
    ?log_bad_request(Request),
    {reply, {error, {invalid_request, Request}}, State}.


%%--------------------------------------------------------------------
%% @private @doc Handles cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_cast({execute, Ref, Pid, Fun, Args}, #state{ca_dir = CaDir} = State) ->
    Response = (catch apply(Fun, [CaDir | Args])),
    Pid ! {Ref, Response},
    {noreply, State};

handle_cast(Request, State) ->
    ?log_bad_request(Request),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private @doc Handles all non call/cast messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_info(gen_crl, #state{ca_dir = CaDir} = State) ->
    gen_crl_imp(CaDir),
    erlang:send_after(?CRL_REGENERATION_PERIOD, self(), gen_crl),
    {noreply, State};

handle_info(Info, State) ->
    ?log_bad_request(Info),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term().
terminate(Reason, State) ->
    ?log_terminate(Reason, State).


%%--------------------------------------------------------------------
%% @private @doc Converts process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc
%% Delegates a request from the API to the zone CA process.
%% @end
%%--------------------------------------------------------------------
-spec delegate(Fun :: function(), Args :: list()) -> Response :: any().
delegate(Fun, Args) ->
    Ref = erlang:make_ref(),
    gen_server:cast(?MODULE, {execute, Ref, self(), Fun, Args}),
    receive
        {Ref, Response} -> Response
    after ?INTERNAL_REQUEST_TIMEOUT ->
        error(ca_loop_not_responding)
    end.

%%--------------------------------------------------------------------
%% @private @doc
%% Generates a certificate for onezone's interfaces.
%% @end
%%--------------------------------------------------------------------
-spec generate_oz_cert(KeyFile :: string(), CertFile :: string(),
    CaDir :: string(), Domain :: string()) -> ok.
generate_oz_cert(KeyFile, CertFile, CaDir, Domain) ->
    TmpDir = utils:mkdtemp(),
    CSRFile = random_filename(TmpDir),
    ReqConfigFile = req_config_file(TmpDir, #dn{commonName = Domain}),
    CaConfigFile = ca_config_file(TmpDir, CaDir),

    ?info("Creating a CSR for the onezone interfaces..."),

    RequestOutput = os:cmd(["openssl req",
        " -config ", ReqConfigFile,
        " -new ",
        " -keyout ", KeyFile,
        " -out ", CSRFile]),

    ?info("~s", [RequestOutput]),
    ?info("Signing the onezone interfaces CSR..."),

    SigningOutput = os:cmd(["openssl ca",
        " -config ", CaConfigFile,
        " -batch",
        " -notext",
        " -extensions user_cert",
        " -in ", CSRFile,
        " -out ", CertFile]),

    ?info("~s", [SigningOutput]),

    utils:rmtempdir(TmpDir),
    ok.

%%--------------------------------------------------------------------
%% @private @doc
%% The underlying implementation of {@link ozpca:sign_provider_req/2}.
%% @end
%%--------------------------------------------------------------------
-spec sign_provider_req_imp(CaDir :: string(), ProviderId :: binary(),
    CSRPem :: binary()) -> {ok, Pem :: binary(), Serial :: integer()}.
sign_provider_req_imp(CaDir, ProviderId, CSRPem) ->
    TmpDir = utils:mkdtemp(),
    CSRFile = random_filename(TmpDir),
    CertFile = random_filename(TmpDir),
    CaConfigFile = ca_config_file(TmpDir, CaDir),

    ok = file:write_file(CSRFile, CSRPem),
    os:cmd(["openssl ca",
        " -config ", CaConfigFile,
        " -batch",
        " -notext",
        " -extensions user_cert",
        " -subj \"/CN=", str_utils:to_list(ProviderId), "/O=onedata/OU=Providers\"",
        " -in ", CSRFile,
        " -out ", CertFile]),

    {ok, Pem} = file:read_file(CertFile),
    case public_key:pem_decode(Pem) of
        [] ->
            % Cert was not written, which implies openssl error (bad CSR)
            {error, bad_csr};
        [{'Certificate', CertDer, not_encrypted}] ->
            Cert = public_key:pkix_decode_cert(CertDer, otp),
            #'OTPCertificate'{
                tbsCertificate = #'OTPTBSCertificate'{
                    serialNumber = Serial
                }} = Cert,
            utils:rmtempdir(TmpDir),
            {ok, Pem, Serial}
    end.

%%--------------------------------------------------------------------
%% @private @doc
%% The underlying implementation of {@link ozpca:verify_provider_imp/1}.
%% @end
%%--------------------------------------------------------------------
-spec verify_provider_imp(CaDir :: string(), PeerCertDer :: public_key:der_encoded()) ->
    {ok, ProviderId :: binary()} | {error, {bad_cert, Reason :: any()}}.
verify_provider_imp(CaDir, PeerCertDer) ->
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

%%--------------------------------------------------------------------
%% @private @doc
%% The underlying implementation of {@link ozpca:gen_crl/0}.
%% @end
%%--------------------------------------------------------------------
-spec gen_crl_imp(CaDir :: string()) -> ok.
gen_crl_imp(CaDir) ->
    TmpDir = utils:mkdtemp(),
    CaConfigFile = ca_config_file(TmpDir, CaDir),
    ?info("Generating an updated CRL"),
    CreateCrlOutput = os:cmd(["openssl ca",
        " -config ", CaConfigFile,
        " -gencrl",
        " -out ", filename:join(CaDir, "crl.pem")]),
    ?info("~s", [CreateCrlOutput]),
    utils:rmtempdir(TmpDir),
    ok.

%%--------------------------------------------------------------------
%% @private @doc
%% The underlying implementation of {@link ozpca:revoke/1}.
%% @end
%%--------------------------------------------------------------------
-spec revoke_imp(CaDir :: string(), Serial :: binary()) -> ok.
revoke_imp(CaDir, Serial) ->
    TmpDir = utils:mkdtemp(),
    CaConfigFile = ca_config_file(TmpDir, CaDir),
    LSerial = str_utils:to_list(Serial),
    ?info("Revoking a certificate with serial number ~p", [Serial]),
    RevokeOutput = os:cmd(["openssl ca",
        " -config ", CaConfigFile,
        " -batch",
        " -revoke ", filename:join([CaDir, "newcerts", LSerial]), ".pem"]),
    ?info("~s", [RevokeOutput]),
    utils:rmtempdir(TmpDir),
    gen_crl_imp(CaDir),
    ok.

%%--------------------------------------------------------------------
%% @private @doc
%% Creates a temporary config file for creating onezone certificate's CSR.
%% @end
%%--------------------------------------------------------------------
-spec req_config_file(TmpDir :: string(), DN :: #dn{}) -> string().
req_config_file(TmpDir, #dn{} = DN) ->
    Config = random_filename(TmpDir),
    ok = file:write_file(Config, req_cnf(DN)),
    Config.

%%--------------------------------------------------------------------
%% @private @doc
%% Creates a temporary config file for signing CSR requests.
%% @end
%%--------------------------------------------------------------------
-spec ca_config_file(TmpDir :: string(), CaDir :: string()) -> string().
ca_config_file(TmpDir, CaDir) ->
    Config = random_filename(TmpDir),
    ok = file:write_file(Config, ca_cnf(CaDir)),
    Config.

%%--------------------------------------------------------------------
%% @private @doc
%% Checks whether the certificate has been revoked by the zone CA.
%% @end
%%--------------------------------------------------------------------
-spec check_revoked(CaCertDer :: public_key:der_encoded(),
    CaCert :: #'OTPCertificate'{},
    PeerCert :: #'OTPCertificate'{}) ->
    valid | {bad_cert, Reason :: any()}.
check_revoked(CaCertDer, CaCert, PeerCert) ->
    {ok, CaDir} = application:get_env(?APP_NAME, ozpca_dir),

    {ok, CRLPem} = file:read_file(filename:join(CaDir, "crl.pem")),
    [{'CertificateList', CRLDer, not_encrypted}] = public_key:pem_decode(CRLPem),
    #'CertificateList'{} = CRL = public_key:der_decode('CertificateList', CRLDer),
    FakeDP = #'DistributionPoint'{cRLIssuer = asn1_NOVALUE},

    public_key:pkix_crls_validate(PeerCert, [{FakeDP, {CRLDer, CRL}}],
        [{issuer_fun, {fun(_, _, _, _) -> {ok, CaCert, [CaCertDer]} end, []}}]).

%%--------------------------------------------------------------------
%% @private @doc
%% Extracts Provider's ID out of the certificate's Common Name.
%% @end
%%--------------------------------------------------------------------
-spec get_provider_id(Cert :: #'OTPCertificate'{}) -> ProviderId :: binary().
get_provider_id(#'OTPCertificate'{} = Cert) ->
    #'OTPCertificate'{tbsCertificate =
    #'OTPTBSCertificate'{subject = {rdnSequence, Attrs}}} = Cert,

    [ProviderId] = lists:filtermap(fun([Attribute]) ->
        case Attribute#'AttributeTypeAndValue'.type of
            ?'id-at-commonName' ->
                {_, Id} = Attribute#'AttributeTypeAndValue'.value,
                {true, str_utils:to_binary(Id)};
            _ -> false
        end
    end, Attrs),

    ProviderId.

%%--------------------------------------------------------------------
%% @private @doc
%% Generates a random file name and returns a path residing under a given
%% directory.
%% @end
%%--------------------------------------------------------------------
-spec random_filename(TmpDir :: string()) -> string().
random_filename(TmpDir) ->
    FileName = hex_utils:to_hex(crypto:hash(sha, term_to_binary({make_ref(), erlang:timestamp()}))),
    filename:join(TmpDir, FileName).

%%%===================================================================
%%% Contents of configuration files.
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Returns a configuration for creating a CSR with given DN by the zone CA.
%%--------------------------------------------------------------------
-spec req_cnf(DN :: #dn{}) -> Config :: iolist().
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

%%--------------------------------------------------------------------
%% @private
%% @doc Returns a configuration for the zone CA.
%%--------------------------------------------------------------------
-spec ca_cnf(CaDir :: string()) -> Config :: iolist().
ca_cnf(CaDir) ->
    {ok, CertDomain} = application:get_env(?APP_NAME, http_domain),
    {ok, RestPort} = application:get_env(?APP_NAME, rest_port),
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
