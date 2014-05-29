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


%% API
-export([sign_csr/2, verify/1]).


-spec sign_csr(CSRBin :: binary(), ProviderId :: binary()) -> {ok, CertBin :: binary()}.
sign_csr(CSRBin, ProviderId) ->
    {ok, CaCertFile} = application:get_env(?APP_Name, grpca_cert_file),
    {ok, CaKeyFile} = application:get_env(?APP_Name, grpca_key_file),

    RandomName = get_random_string(10),
    ok = file:write_file("/tmp/" ++ RandomName ++ ".csr", CSRBin),

    CMD = "openssl ca -config ./grpca/openssl.cnf -batch -in /tmp/" ++ RandomName ++
        ".csr -out /tmp/" ++ RandomName ++ ".crt -cert " ++ CaCertFile ++
        " -keyfile " ++ CaKeyFile ++ " -days 180 -outdir /tmp/ -notext " ++
        "-policy policy_anything -subj \"/CN="++binary:bin_to_list(ProviderId)++
        "/O=OneData Providers\"",

    io:format("~s~n", [os:cmd(CMD)]),
    {ok, CertBin} = file:read_file("/tmp/" ++ RandomName ++ ".crt"),
    {ok, CertBin}.


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


-spec cacert() -> Cert :: #'OTPCertificate'{}.
cacert() ->
    {ok, CaCertPath} = application:get_env(?APP_Name, grpca_cert_file),
    cert(CaCertPath).


-spec verify(PeerCertDer :: public_key:der_encoded()) -> {ok, ProviderId :: binary()} | {error, Reason :: term()}.
verify(PeerCertDer) -> %% @todo: CRLs
    case public_key:pkix_path_validation(cacert(), [PeerCertDer], [{max_path_length, 0}]) of
        {ok, _} ->
            PeerCert = public_key:pkix_decode_cert(PeerCertDer, plain),
            {ok, get_provider_id(PeerCert)};
        Error -> Error
    end.


-spec get_random_string(Length :: integer()) -> string().
get_random_string(Length) ->
    AllowedChars = "qwertyuiopasdfghjklzxcvbnm1234567890",
    lists:foldl(fun(_, Acc) ->
        [lists:nth(random:uniform(length(AllowedChars)), AllowedChars) | Acc]
    end, [], lists:seq(1, Length)).


%% validity(DaysBefore, DaysAfter) ->
%%     DefFrom = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(date()) + DaysBefore),
%%     DefTo = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(date()) + DaysAfter),
%%     Format = fun({Y,M,D}) -> lists:flatten(io_lib:format("~w~2..0w~2..0w000000Z",[Y,M,D])) end,
%%     #'Validity'{notBefore={generalTime, Format(DefFrom)},
%%         notAfter ={generalTime, Format(DefTo)}}.


-spec cert(CertPath :: string()) -> Cert :: #'OTPCertificate'{}.
cert(CertPath) ->
    {ok, CertBin} = file:read_file(CertPath),
    [{'Certificate', CertDer, not_encrypted}] = public_key:pem_decode(CertBin),
    #'OTPCertificate'{} = Cert = public_key:pkix_decode_cert(CertDer, otp),
    Cert.


-spec key(KeyPath :: string()) -> Key :: #'PrivateKeyInfo'{}.
key(KeyPath) ->
    %% @todo: we may want to password-protect the private key
    {ok, KeyBin} = file:read_file(KeyPath),
    [{'PriveteKeyInfo', _} = KeyEntry] = public_key:pem_decode(KeyBin),
    #'PrivateKeyInfo'{} = Key = public_key:pem_entry_decode(KeyEntry),
    Key.
