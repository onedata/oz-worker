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
-module(rest_utils).
-author("Michal Zmuda").


-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").

-export([get_rest_port/1, get_node_ip/1, generate_cert_files/0, register_provider/4, do_request/3, check_status/1, get_response_status/1, do_request/4, do_request/5, update_req_params/3, register_user/4]).


register_user(UserName, ProviderId, Node, ProviderReqParams) ->
    UserId = create_user(UserName, Node),
    NewHeaders = authorize_user(UserId, ProviderId, ProviderReqParams, Node),
    UserReqParams = update_req_params(ProviderReqParams, NewHeaders, headers),
    {UserId, UserReqParams}.

create_user(UserName, Node) ->
    {ok, UserId} = rpc:call(Node, user_logic, create, [#onedata_user{name = UserName}]),
    UserId.

%% this function authorizes users
%% is sends request to endpoint /user/authorize
%% then it parses macaroons from response
%% and returns headers updated with these macaroons
%% headers are needed to confirm that user is authorized
authorize_user(UserId, ProviderId, ReqParams, Node) ->
    SerializedMacaroon = rpc:call(Node, auth_logic, gen_token, [UserId, ProviderId]),
    {RestAddress, Headers, _Options} = ReqParams,
    Identifier = get_macaroon_id(SerializedMacaroon),
    Body = json_utils:encode([{<<"identifier">>, Identifier}]),
    Resp = do_request(RestAddress ++ "/user/authorize", Headers, post, Body),
    SerializedDischarges = get_response_body(Resp),
    prepare_macaroons_headers(SerializedMacaroon, SerializedDischarges).

get_rest_port(Node) ->
    {ok, RestPort} = rpc:call(Node, application, get_env, [?APP_Name, rest_port]),
    RestPort.

%% returns ip (as a string) of given node
get_node_ip(Node) ->
    CMD = "docker inspect --format '{{ .NetworkSettings.IPAddress }}'" ++ " " ++ utils:get_host(Node),
    re:replace(os:cmd(CMD), "\\s+", "", [global, {return, list}]).


register_provider(URLS, RedirectionPoint, ClientName, ReqParams) ->
    {RestAddress, Headers, _Options} = ReqParams,
    {KeyFile, CSRFile, CertFile} = generate_cert_files(),

    {ok, CSR} = file:read_file(CSRFile),
    Body = json_utils:encode([
        {<<"urls">>, URLS},
        {<<"csr">>, CSR},
        {<<"redirectionPoint">>, RedirectionPoint},
        {<<"clientName">>, ClientName}
    ]),
    % Add insecure option - we do not want the GR server cert to be checked.
    hackney_pool:start_pool(noauth, [{timeout, 150000}, {max_connections, 100}]),
    Response = do_request(RestAddress ++ "/provider", Headers, post, Body),
    hackney_pool:stop_pool(noauth),
    %% save cert
    [Cert, ProviderId] = get_body_val([certificate, providerId], Response),
    file:write_file(CertFile, Cert),
    %% set request options for provider
    Options = [{ssl_options, [{keyfile, KeyFile}, {certfile, CertFile}]}],
    %% set request parametres for provider
    ProviderReqParams = update_req_params(ReqParams, Options, options),
    {ProviderId, ProviderReqParams}.


generate_cert_files() ->
    {MegaSec, Sec, MiliSec} = erlang:now(),
    Prefix = lists:foldl(fun(Int, Acc) ->
        Acc ++ integer_to_list(Int) end, "provider", [MegaSec, Sec, MiliSec]),
    KeyFile = filename:join(?TEMP_DIR, Prefix ++ "_key.pem"),
    CSRFile = filename:join(?TEMP_DIR, Prefix ++ "_csr.pem"),
    CertFile = filename:join(?TEMP_DIR, Prefix ++ "_cert.pem"),
    os:cmd("openssl genrsa -out " ++ KeyFile ++ " 2048"),
    os:cmd("openssl req -new -batch -key " ++ KeyFile ++ " -out " ++ CSRFile),
    {KeyFile, CSRFile, CertFile}.

get_response_status(Response) ->
    {ok, Status, _ResponseHeaders, _ResponseBody} = Response,
    Status.

get_response_headers(Response) ->
    {ok, _Status, ResponseHeaders, _ResponseBody} = Response,
    ResponseHeaders.

get_response_body(Response) ->
    {ok, _Status, _ResponseHeaders, ResponseBody} = Response,
    ResponseBody.

%% returns list of values from Response's body,
%% returned list is ordered accordingly to keys in Keylist
%% Keylist is list of atoms
get_body_val(KeyList, Response) ->
    case check_status(Response) of
        {bad_response_code, Code} -> {request_error, Code};
        _ -> JSONOutput = json_utils:decode(get_response_body(Response)),
            [proplists:get_value(atom_to_binary(Key, latin1), JSONOutput) || Key <- KeyList]
    end.

get_header_val(Parameter, Response) ->
    case check_status(Response) of
        {bad_response_code, Code} -> {request_error, Code};
        _ ->
            case lists:keysearch(<<"location">>, 1, get_response_headers(Response)) of
                {value, {_HeaderType, HeaderValue}} ->
                    parse_http_param(Parameter, HeaderValue);
                false -> parameter_not_in_header
            end
    end.

parse_http_param(Parameter, HeaderValue) ->
    [_, ParamVal] = binary:split(HeaderValue, <<"/", Parameter/binary, "/">>, [global]),
    ParamVal.

check_status(Response) ->
    Status = get_response_status(Response),
    case (Status >= 200) and (Status < 300) of
        true -> ok;
        _ -> {bad_response_code, Status}
    end.

get_macaroon_id(Token) ->
    {ok, Macaroon} = macaroon:deserialize(Token),
    [{_, Identifier}] = macaroon:third_party_caveats(Macaroon),
    Identifier.

prepare_macaroons_headers(SerializedMacaroon, SerializedDischarges) ->
    {ok, Macaroon} = macaroon:deserialize(SerializedMacaroon),
    BoundMacaroons = lists:map(
        fun(SrlzdDischMacaroon) ->
            {ok, DM} = macaroon:deserialize(SrlzdDischMacaroon),
            BDM = macaroon:prepare_for_request(Macaroon, DM),
            {ok, SBDM} = macaroon:serialize(BDM),
            SBDM
        end, [str_utils:to_binary(SerializedDischarges)]),
    % Bound discharge macaroons are sent in one header,
    % separated by spaces.
    BoundMacaroonsValue = str_utils:join_binary(BoundMacaroons, <<" ">>),
    [
        {<<"macaroon">>, SerializedMacaroon},
        {<<"discharge-macaroons">>, BoundMacaroonsValue}
    ].

%% returns list of values from responsebody
do_request(Endpoint, Headers, Method) ->
    do_request(Endpoint, Headers, Method, <<>>, []).
do_request(Endpoint, Headers, Method, Body) ->
    do_request(Endpoint, Headers, Method, Body, []).
do_request(Endpoint, Headers, Method, Body, Options) ->
    % Add insecure option - we do not want the GR server cert to be checked.
    http_client:request(Method, Endpoint, Headers, Body, [insecure | Options]).

update_req_params(ReqParams, NewParam, headers) ->
    {RestAddress, Headers, Options} = ReqParams,
    {RestAddress, Headers ++ NewParam, Options};
update_req_params(ReqParams, NewParam, options) ->
    {RestAddress, Headers, Options} = ReqParams,
    {RestAddress, Headers, Options ++ NewParam};
update_req_params(ReqParams, NewParam, address) ->
    {_, Headers, Options} = ReqParams,
    {NewParam, Headers, Options}.