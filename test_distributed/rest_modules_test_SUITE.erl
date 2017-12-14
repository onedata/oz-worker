%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Integration tests of rest_modules in onezone.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_modules_test_SUITE).
-author("Jakub Kudzia").

-include("registered_names.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/test/performance.hrl").

-define(CONTENT_TYPE_HEADER, [{<<"content-type">>, <<"application/json">>}]).

%%  set example test data
-define(LATITUDE, 23.10).
-define(LONGITUDE, 44.44).
-define(DOMAIN1, <<"127.0.0.1">>).
-define(CLIENT_NAME1, <<"provider1">>).
-define(CLIENT_NAME2, <<"provider2">>).
-define(USER_NAME1, <<"user1">>).
-define(USER_NAME2, <<"user2">>).
-define(USER_NAME3, <<"user3">>).
% Default alias (= no alias set) is an empty string
-define(DEFAULT_USER_ALIAS, <<"">>).
-define(USER_ALIAS1, <<"alias1">>).
-define(USER_ALIAS2, <<"alias2">>).
-define(SPACE_NAME1, <<"space1">>).
-define(SPACE_NAME2, <<"space2">>).
-define(GROUP_NAME1, <<"group1">>).
-define(GROUP_NAME2, <<"group2">>).
-define(GROUP_TYPE1, unit).
-define(GROUP_TYPE2, team).
-define(GROUP_TYPE1_BIN, <<"unit">>).
-define(GROUP_TYPE2_BIN, <<"team">>).
-define(SPACE_SIZE1, <<"1024024024">>).
-define(SPACE_SIZE2, <<"4096096096">>).

-define(BAD_REQUEST, 400).
-define(UNAUTHORIZED, 401).
-define(FORBIDDEN, 403).
-define(NOT_FOUND, 404).


-define(GROUP_PRIVILEGES,
    [
        ?GROUP_VIEW, ?GROUP_UPDATE, ?GROUP_INVITE_USER,
        ?GROUP_REMOVE_USER, ?GROUP_JOIN_SPACE, ?GROUP_CREATE_SPACE,
        ?GROUP_SET_PRIVILEGES, ?GROUP_DELETE, ?GROUP_LEAVE_SPACE,
        ?GROUP_JOIN_GROUP,
        ?GROUP_INVITE_GROUP, ?GROUP_REMOVE_GROUP
    ]
).
-define(SPACE_PRIVILEGES,
    [
        ?SPACE_VIEW, ?SPACE_UPDATE,
        % ?SPACE_WRITE_DATA, ?SPACE_MANAGE_SHARES, - they are checked in rest_shares_test_SUITE
        ?SPACE_INVITE_USER, ?SPACE_REMOVE_USER,
        ?SPACE_INVITE_GROUP, ?SPACE_REMOVE_GROUP,
        ?SPACE_SET_PRIVILEGES, ?SPACE_DELETE,
        ?SPACE_INVITE_PROVIDER, ?SPACE_REMOVE_PROVIDER
    ]
).

-define(PROXY_ENDPOINT, <<"172.17.0.9:8080/api/v1">>).
-define(DOI_SERVICE,
    #{
        <<"name">> => <<"LifeWatch DataCite">>,
        <<"proxyEndpoint">> => ?PROXY_ENDPOINT,
        <<"serviceProperties">> => #{
            <<"type">> => <<"DOI">>,
            <<"host">> => <<"https://mds.test.datacite.org">>,
            <<"doiEndpoint">> => <<"/doi">>,
            <<"metadataEndpoint">> => <<"/metadata">>,
            <<"mediaEndpoint">> => <<"/media">>,
            <<"prefix">> => <<"10.5072">>,
            <<"username">> => <<"alice">>,
            <<"password">> => <<"*******">>,
            <<"identifierTemplate">> => <<"{{space.name}}-{{space.guid}}">>,
            <<"allowTemplateOverride">> => false
        }
    }
).

-define(PID_SERVICE,
    #{
        <<"name">> => <<"iMarine EPIC">>,
        <<"proxyEndpoint">> => ?PROXY_ENDPOINT,
        <<"serviceProperties">> => #{
            <<"type">> => <<"PID">>,
            <<"endpoint">> => <<"https://epic.grnet.gr/api/v2/handles">>,
            <<"prefix">> => <<"11789">>,
            <<"suffixGeneration">> => <<"auto">>,
            <<"suffixPrefix">> => <<"{{space.name}}">>,
            <<"suffixSuffix">> => <<"{{user.name}}">>,
            <<"username">> => <<"alice">>,
            <<"password">> => <<"*******">>,
            <<"identifierTemplate">> => <<"{{space.name}}-{{space.guid}}">>,
            <<"allowTemplateOverride">> => false
        }
    }
).

-define(DC_METADATA, <<"<?xml version=\"1.0\"?>",
    "<metadata xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xmlns:dc=\"http:\/\/purl.org\/dc\/elements\/1.1\/\">"
    "<dc:title>Test dataset<\/dc:title>",
    "<dc:creator>John Johnson<\/dc:creator>",
    "<dc:creator>Jane Doe<\/dc:creator>",
    "<dc:subject>Test of datacite<\/dc:subject>",
    "<dc:description>Lorem ipsum lorem ipusm<\/dc:description>",
    "<dc:publisher>Onedata<\/dc:publisher>",
    "<dc:publisher>EGI<\/dc:publisher>",
    "<dc:date>2016<\/dc:date>",
    "<dc:format>application\/pdf<\/dc:format>",
    "<dc:identifier>onedata:LKJHASKFJHASLKDJHKJHuah132easd<\/dc:identifier>",
    "<dc:language>eng<\/dc:language>",
    "<dc:rights>CC-0<\/dc:rights>",
    "<\/metadata>">>).

-define(DC_METADATA_2, <<"<?xml version=\"1.0\"?>",
    "<metadata xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xmlns:dc=\"http:\/\/purl.org\/dc\/elements\/1.1\/\">"
    "<dc:title>Test dataset<\/dc:title>",
    "<dc:creator>Jane Johnson<\/dc:creator>",
    "<dc:creator>John Doe<\/dc:creator>",
    "<dc:subject>Test of datacite<\/dc:subject>",
    "<dc:description>Dolor sit amet<\/dc:description>",
    "<dc:publisher>Onedata<\/dc:publisher>",
    "<dc:publisher>EGI<\/dc:publisher>",
    "<dc:date>2017<\/dc:date>",
    "<dc:format>application\/pdf<\/dc:format>",
    "<dc:identifier>onedata:LKJHASKFJHASLKDJHKJHuah132easd<\/dc:identifier>",
    "<dc:language>eng<\/dc:language>",
    "<dc:rights>CC-0<\/dc:rights>",
    "<\/metadata>">>).

-define(SHARE_ID_1, <<"shareId1">>).
-define(SHARE_ID_2, <<"shareId2">>).
-define(SHARE_1_PUBLIC_URL, <<"https://onedata.org/shares/shareId1">>).
-define(SHARE_2_PUBLIC_URL, <<"https://onedata.org/shares/shareId2">>).

-define(HANDLE(ServiceId, ResourceId),
    #{
        <<"handleServiceId">> => ServiceId,
        <<"resourceType">> => <<"Share">>,
        <<"resourceId">> => ResourceId,
        <<"metadata">> => ?DC_METADATA
    }
).

-define(MAP_GROUP_TEST_AUTH, test_auth).
-define(MAP_GROUP_TEST_AUTH_BIN, atom_to_binary(?MAP_GROUP_TEST_AUTH, latin1)).
-define(MAP_GROUP_TEST_AUTH_MODULE, test_auth_module).
-define(MAPPED_MEMBERSHIP_SPEC, <<"mapped_group1/user:member">>).
-define(MAPPED_GROUP_SPEC, <<"mapped_group1">>).

%% API
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

% provider_rest_module_test_group
-export([
    get_unsupported_space_info_test/1,
    map_group_fail_test/1,
    map_group_test/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() ->
    ?ALL([
        get_unsupported_space_info_test,
        map_group_fail_test,
        map_group_test,
        create_pid_handle_test
    ]).

%%%===================================================================
%%% Test functions
%%%===================================================================


get_unsupported_space_info_test(Config) ->
    ProviderReqParams = ?config(providerReqParams, Config),
    UserReqParams = ?config(userReqParams, Config),
    OtherRestAddress = ?config(otherRestAddress, Config),
    ParamsWithOtherAddress = update_req_params(ProviderReqParams, OtherRestAddress, address),

    SID = create_space_for_user(Config, ?SPACE_NAME1, UserReqParams),
    ?assertMatch({request_error, ?NOT_FOUND}, get_space_info_by_provider(SID, ProviderReqParams)),
    ?assertMatch({request_error, ?NOT_FOUND}, get_space_info_by_provider(SID, ParamsWithOtherAddress)).


map_group_fail_test(Config) ->
    ProviderReqParams = ?config(providerReqParams, Config),
    ?assertMatch({request_error, 400}, map_group(<<"github">>, <<"abc">>, ProviderReqParams)).


map_group_test(Config) ->
    ProviderReqParams = ?config(providerReqParams, Config),
    MappedGroupHash = idp_group_mapping:group_spec_to_db_id(?MAPPED_GROUP_SPEC),
    ?assertMatch(MappedGroupHash,
        map_group(?MAP_GROUP_TEST_AUTH_BIN, <<"group1">>, ProviderReqParams)).


%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) ->
        [Node1, Node2] = ?config(oz_worker_nodes, NewConfig),
        OZ_IP_1 = test_utils:get_docker_ip(Node1),
        OZ_IP_2 = test_utils:get_docker_ip(Node2),
        RestPort = get_rest_port(Node1),
        RestAPIPrefix = get_rest_api_prefix(Node1),
        RestAddress = str_utils:format("https://~s:~B~s", [OZ_IP_1, RestPort, RestAPIPrefix]),
        OtherRestAddress = str_utils:format("https://~s:~B~s", [OZ_IP_2, RestPort, RestAPIPrefix]),
        [{otherRestAddress, OtherRestAddress} | [{restAddress, RestAddress} | NewConfig]]
    end,
    [{env_up_posthook, Posthook}, {?LOAD_MODULES, [oz_test_utils]} | Config].


init_per_testcase(non_register, Config) ->
    RestAddress = RestAddress = ?config(restAddress, Config),
    mock_handle_proxy(Config),
    [{cert_files, generate_cert_files()} | Config];
init_per_testcase(register_only_provider, Config) ->
    %% this init function is for tests
    %% that need registered provider
    NewConfig = init_per_testcase(non_register, Config),
    RestAddress = ?config(restAddress, NewConfig),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},
    {ProviderId, ProviderReqParams} =
        register_provider(?DOMAIN1, ?CLIENT_NAME1, NewConfig, ReqParams),
    [
        {providerId, ProviderId},
        {providerReqParams, ProviderReqParams}
        | NewConfig
    ];
init_per_testcase(map_group_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, auth_config),
    ok = test_utils:mock_expect(Nodes, auth_config, has_group_mapping_enabled, fun(_) ->
        true
    end),
    ok = test_utils:mock_expect(Nodes, auth_config, get_auth_providers, fun() ->
        [?MAP_GROUP_TEST_AUTH | meck:passthrough([])]
    end),
    ok = test_utils:mock_expect(Nodes, auth_config, get_provider_module, fun(_) ->
        ?MAP_GROUP_TEST_AUTH_MODULE
    end),
    ok = test_utils:mock_new(Nodes, ?MAP_GROUP_TEST_AUTH_MODULE, [passthrough, non_strict]),
    ok = test_utils:mock_expect(Nodes, ?MAP_GROUP_TEST_AUTH_MODULE, normalized_membership_spec, fun(_) ->
        ?MAPPED_MEMBERSHIP_SPEC
    end),
    init_per_testcase(default, Config);
init_per_testcase(_Default, Config) ->
    %% this default init function is for tests
    %% than need registered provider and user
    NewConfig = init_per_testcase(register_only_provider, Config),
    ProviderId = ?config(providerId, NewConfig),
    ProviderReqParams = ?config(providerReqParams, NewConfig),
    {UserId, UserReqParams} =
        register_user(?USER_NAME1, ProviderId, NewConfig, ProviderReqParams),
    [
        {userId, UserId},
        {userReqParams, UserReqParams}
        | NewConfig
    ].
end_per_testcase(map_group_test, Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, auth_config),
    test_utils:mock_unload(Nodes, ?MAP_GROUP_TEST_AUTH_MODULE),
    end_per_testcase(default, Config);
end_per_testcase(_, Config) ->
    {KeyFile, CSRFile, CertFile} = ?config(cert_files, Config),
    oz_test_utils:delete_all_entities(Config),
    unmock_handle_proxy(Config),
    file:delete(KeyFile),
    file:delete(CSRFile),
    file:delete(CertFile).

end_per_suite(_Config) ->
    hackney:stop(),
    ssl:stop().

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_rest_port(Node) ->
    {ok, RestPort} = rpc:call(Node, application, get_env, [?APP_NAME, rest_port]),
    RestPort.

get_rest_api_prefix(Node) ->
    {ok, RestAPIPrefix} = rpc:call(Node, application, get_env, [?APP_NAME, rest_api_prefix]),
    RestAPIPrefix.

generate_cert_files() ->
    Prefix = "provider" ++ integer_to_list(erlang:system_time(micro_seconds)),
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
        {bad_response_code, Code} ->
            {request_error, Code};
        _ ->
            JSONOutput = json_utils:decode(get_response_body(Response)),
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

%% returns list of values from response body
do_request(Endpoint, Headers, Method, Body) ->
    do_request(Endpoint, Headers, Method, Body, []).
do_request(Endpoint, Headers, Method, Body, Options) ->
    % Add insecure option - we do not want the OZ server cert to be checked.
    SslOpts = proplists:get_value(ssl_options, Options, []),
    CompleteOpts = [
        {ssl_options, [{secure, false} | SslOpts]},
        proplists:delete(ssl_options, Options)
    ],
    case http_client:request(Method, Endpoint, maps:from_list(Headers), Body, CompleteOpts) of
        {ok, RespCode, RespHeaders, RespBody} ->
            {ok, RespCode, maps:to_list(RespHeaders), RespBody};
        Other ->
            Other
    end.

get_macaroon_id(Token) ->
    {ok, Macaroon} = token_utils:deserialize(Token),
    [{_, Identifier}] = macaroon:third_party_caveats(Macaroon),
    Identifier.

prepare_macaroons_headers(SerializedMacaroon, SerializedDischarges) ->
    {ok, Macaroon} = token_utils:deserialize(SerializedMacaroon),
    BoundMacaroons = lists:map(
        fun(SrlzdDischMacaroon) ->
            {ok, DM} = token_utils:deserialize(SrlzdDischMacaroon),
            BDM = macaroon:prepare_for_request(Macaroon, DM),
            {ok, SBDM} = token_utils:serialize62(BDM),
            SBDM
        end, [str_utils:to_binary(SerializedDischarges)]),
    % Bound discharge macaroons are sent in one header,
    % separated by spaces.
    BoundMacaroonsValue = str_utils:join_binary(BoundMacaroons, <<" ">>),
    [
        {<<"macaroon">>, SerializedMacaroon},
        {<<"discharge-macaroons">>, BoundMacaroonsValue}
    ].

update_req_params({RestAddress, Headers, Options}, NewParam, headers) ->
    {RestAddress, Headers ++ NewParam, Options};
update_req_params({RestAddress, Headers, Options}, NewParam, options) ->
    {RestAddress, Headers, Options ++ NewParam};
update_req_params({_, Headers, Options}, NewParam, address) ->
    {NewParam, Headers, Options}.

%% Provider functions =====================================================

register_provider(Domain, ClientName, Config, ReqParams) ->
    register_provider(undefined, undefined, Domain, ClientName, Config, ReqParams).

register_provider(Latitude, Longitude, Domain, ClientName, Config, ReqParams = {RestAddress, Headers, _Options}) ->
    {KeyFile, CSRFile, CertFile} = ?config(cert_files, Config),
    {ok, CSR} = file:read_file(CSRFile),
    Params = [
        {<<"csr">>, CSR},
        {<<"domain">>, Domain},
        {<<"subdomainDelegation">>, false},
        {<<"clientName">>, ClientName}
    ],
    Body = json_utils:encode(case {Latitude, Longitude} of
        {undefined, _} -> Params;
        {_, undefined} -> Params;
        _ ->
            Params ++ [{<<"latitude">>, Latitude}, {<<"longitude">>, Longitude}]
    end),

    hackney_pool:start_pool(noauth, [{timeout, 150000}, {max_connections, 100}]),
    Response = do_request(RestAddress ++ "/provider", Headers, post, Body),
    hackney_pool:stop_pool(noauth),
    %% save cert
    [Cert, ProviderId] = get_body_val([certificate, providerId], Response),
    file:write_file(CertFile, Cert),
    %% set request options for provider
    Options = [{ssl_options, [{keyfile, KeyFile}, {certfile, CertFile}]}],
    %% set request parameters for provider
    ProviderReqParams = update_req_params(ReqParams, Options, options),
    {ProviderId, ProviderReqParams}.

get_space_info_by_provider(SID, {RestAddress, Headers, Options}) ->
    EncodedSID = binary_to_list(http_utils:url_encode(SID)),
    Response = do_request(RestAddress ++ "/provider/spaces/" ++ EncodedSID, Headers, get, [], Options),
    get_body_val([spaceId, name, providersSupports], Response).

map_group(Idp, GroupId, {RestAddress, Headers, Options}) ->
    Body = json_utils:encode([
        {<<"idp">>, Idp},
        {<<"groupId">>, GroupId}
    ]),
    Response = do_request(RestAddress ++ "/provider/test/map_idp_group", Headers, post, Body, Options),
    case get_body_val([groupId], Response) of
        Error = {request_error, _} ->
            Error;
        [MappedGroupId] ->
            MappedGroupId
    end.

%% User functions =========================================================

create_user(UserName, Node) ->
    {ok, UserId} = rpc:call(Node, user_logic, create, [#od_user{name = UserName}]),
    UserId.

%% this function authorizes users
%% is sends request to endpoint /user/authorize
%% then it parses macaroons from response
%% and returns headers updated with these macaroons
%% headers are needed to confirm that user is authorized
authorize_user(UserId, ProviderId, {RestAddress, Headers, _Options}, Node) ->
    SerializedMacaroon = rpc:call(Node, auth_logic, gen_token, [UserId, ProviderId]),
    Identifier = get_macaroon_id(SerializedMacaroon),
    Body = json_utils:encode([{<<"identifier">>, Identifier}]),
    Resp = do_request(RestAddress ++ "/user/authorize", Headers, post, Body),
    SerializedDischarges = get_response_body(Resp),
    prepare_macaroons_headers(SerializedMacaroon, SerializedDischarges).

register_user(UserName, ProviderId, Config, ProviderReqParams) ->
    [Node1, _] = ?config(oz_worker_nodes, Config),
    UserId = create_user(UserName, Node1),
    NewHeaders = authorize_user(UserId, ProviderId, ProviderReqParams, Node1),
    UserReqParams = update_req_params(ProviderReqParams, NewHeaders, headers),
    {UserId, UserReqParams}.

create_space_for_user(_Config, SpaceName, {RestAddress, Headers, Options}) ->
    Body = json_utils:encode([
        {<<"name">>, SpaceName}
    ]),
    Response = do_request(RestAddress ++ "/user/spaces", Headers, post, Body, Options),
    % Make sure that user's privileges are synchronized
    get_header_val(<<"spaces">>, Response).


%% Other functions =========================================================

mock_handle_proxy(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    ok = test_utils:mock_new(Nodes, od_share, [passthrough]),
    ok = test_utils:mock_expect(Nodes, od_share, get,
        fun
            (?SHARE_ID_1) ->
                {ok, #document{value = Share} = Doc} = meck:passthrough([?SHARE_ID_1]),
                {ok, Doc#document{value = Share#od_share{public_url = ?SHARE_1_PUBLIC_URL}}};
            (?SHARE_ID_2) ->
                {ok, #document{value = Share} = Doc} = meck:passthrough([?SHARE_ID_2]),
                {ok, Doc#document{value = Share#od_share{public_url = ?SHARE_2_PUBLIC_URL}}};
            (_) ->
                meck:passthrough()
        end),

    ok = test_utils:mock_new(Nodes, handle_proxy_client, [passthrough]),
    ok = test_utils:mock_expect(Nodes, handle_proxy_client, put,
        fun(?PROXY_ENDPOINT, <<"/handle", _/binary>>, _, _) ->
            {ok, 201, #{<<"location">> => <<"/test_location">>}, <<"">>}
        end),
    ok = test_utils:mock_expect(Nodes, handle_proxy_client, patch,
        fun(?PROXY_ENDPOINT, <<"/handle", _/binary>>, _, _) ->
            {ok, 204, #{}, <<"">>}
        end),
    ok = test_utils:mock_expect(Nodes, handle_proxy_client, delete,
        fun(?PROXY_ENDPOINT, <<"/handle", _/binary>>, _, _) ->
            {ok, 200, #{}, <<"">>}
        end).

unmock_handle_proxy(Config) ->
    Nodes = ?config(oz_worker_nodes, Config),
    test_utils:mock_unload(Nodes, od_share),
    test_utils:mock_unload(Nodes, handle_proxy_client).
