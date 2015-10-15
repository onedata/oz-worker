%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Integration tests of rest_modules
%%% @end
%%%-------------------------------------------------------------------
-module(rest_modules_test_SUITE).
-author("Jakub Kudzia").

-include("registered_names.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("annotations/include/annotations.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include("dao/dao_users.hrl").

%% TODO
%% -delete after finishing

-define(PRINT(ARG),
    begin
        ct:pal("DEBUG: ~p~n",[ARG])
    end).

-define(CONTENT_TYPE_HEADER,[{"content-type","application/json"}]).

%%  set example test data
-define(URLS1, [<<"127.0.0.1">>]).
-define(URLS2, [<<"127.0.0.2">>]).
-define(REDIRECTION_POINT1, <<"https://127.0.0.1:443">>).
-define(REDIRECTION_POINT2, <<"https://127.0.0.2:443">>).
-define(CLIENT_NAME1, <<"provider1">>).
-define(CLIENT_NAME2, <<"provider2">>).
-define(USER_NAME1, <<"user1">>).
-define(USER_NAME2, <<"user2">>).
-define(SPACE_NAME1, <<"space1">>).
-define(SPACE_NAME2, <<"space2">>).
-define(GROUP_NAME1, <<"group1">>).
-define(GROUP_NAME2, <<"group2">>).
-define(SPACE_SIZE1, <<"1024">>).
-define(SPACE_SIZE2, <<"4096">>).

%% API
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([provider_crud_test/1, user_space_test/1, user_crud_test/1, group_crud_test/1,
    provider_space_test/1, provider_check_test/1, many_users_space_test/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-performance({test_cases, []}).

all() ->
    [
        {group, provider_rest_module_test_group}
        ,
        {group, user_rest_module_test_group}
     ,
        {group, group_rest_module_test_group}
    ].

groups() ->
    [
        {
            provider_rest_module_test_group,
            [],
            [
                provider_crud_test,
                provider_space_test,
                provider_check_test
            ]
        }
        ,
        {
            user_rest_module_test_group,
            [],
            [
                user_crud_test,
                user_space_test
            ]
        }
        ,
        {
            group_rest_module_test_group,
            [],
            [
                group_crud_test
            ]

        }
    ].

%%%===================================================================
%%% Test functions
%%%===================================================================


%% provider_rest_module_test_group
provider_crud_test(Config) ->
    RestAddress = ?config(restAddress, Config),
    {KeyFile, CSRFile, CertFile} = ?config(cert_files, Config),
    {ok, CSR} =file:read_file(CSRFile),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},
    
    RegisterResponse =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, CSR, ReqParams),

    [Cert, ProviderId] = get_response_val([certificate, providerId], RegisterResponse),
    file:write_file(CertFile, Cert),

    %% set request options
    Options = [{ssl_options, [{keyfile, KeyFile}, {certfile, CertFile }]}],

    ProviderReqParams = update_req_params(ReqParams, Options, options),

    ReadResponse1 = read_provider(ProviderReqParams),
    ?assertMatch([?CLIENT_NAME1, ?URLS1, ?REDIRECTION_POINT1, ProviderId],
                 get_response_val([clientName, urls, redirectionPoint, providerId], ReadResponse1)),

    update_provider(?URLS2, ?REDIRECTION_POINT2, ?CLIENT_NAME2, ProviderReqParams),

    ReadResponse2 = read_provider(ProviderReqParams),
    ?assertMatch([?CLIENT_NAME2, ?URLS2, ?REDIRECTION_POINT2, ProviderId],
                 get_response_val([clientName, urls, redirectionPoint, providerId], ReadResponse2)),

    ReadResponse3 = get_provider_data_by_pid(ProviderId, ProviderReqParams),
                 get_response_val([clientName, urls, redirectionPoint, providerId], ReadResponse3),

    DeleteResponse = delete_provider(ProviderReqParams),
    ?assertMatch("202", get_response_status(DeleteResponse)),

    ReadResponse4 = read_provider(ProviderReqParams),
    ?assertMatch("401",get_response_status(ReadResponse4)).

provider_space_test(Config) ->
    ?PRINT("PROVIDER SPACE TEST"),
    RestAddress = ?config(restAddress, Config),
    {KeyFile, CSRFile, CertFile} = ?config(cert_files, Config),
    {ok, CSR} =file:read_file(CSRFile),
    [Node] = ?config(gr_nodes, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},

    RegisterResponse =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, CSR, ReqParams),

    [Cert, ProviderId] = get_response_val([certificate, providerId], RegisterResponse),
    file:write_file(CertFile, Cert),

    %% set request options
    Options = [{ssl_options, [{keyfile, KeyFile}, {certfile, CertFile }]}],
    ProviderReqParams = update_req_params(ReqParams, Options, options),

    %% create user
    UserId = create_user(?USER_NAME1, Node),

    %% authenticate user
    NewHeaders = auth_user(UserId, ProviderId, ReqParams, Node),
    UserReqParams = update_req_params(ProviderReqParams, NewHeaders, headers),

    %% get space creation token1
    SCRTokenResponse1 = get_space_creation_token(UserReqParams),
    %% ?PRINT(SCRTokenResponse1),

    [SCRToken1] = get_response_val([token], SCRTokenResponse1),
    %% ?PRINT(SCRToken1),

    SCRResponse1 =
        create_space_for_provider(SCRToken1, ?SPACE_NAME1, ?SPACE_SIZE1, ProviderReqParams),
    ?PRINT(SCRResponse1),

    SID1 = get_header_val("spaces", get_response_headers(SCRResponse1)),

    %% get space creation token2
    SCRTokenResponse2 = get_space_creation_token(UserReqParams),
%%     ?PRINT(SCRTokenResponse2),

    [SCRToken2] = get_response_val([token], SCRTokenResponse2),

    SCRResponse2 =
        create_space_for_provider(SCRToken2, ?SPACE_NAME2, ?SPACE_SIZE2, ProviderReqParams),
    ?PRINT(SCRResponse2),

    SID2 = get_header_val("spaces", get_response_headers(SCRResponse2)),

    ?PRINT(SID1),
    ?PRINT(SID2),

    SpacesResponse1 = get_provider_spaces(ProviderReqParams),
    ?PRINT(SpacesResponse1),

    [SupportedSpaces] = get_response_val([spaces],SpacesResponse1),
    ?assertMatch(true, is_included([SID1, SID2], SupportedSpaces)),

    DeleteResponse = delete_provider_space(SID1, ProviderReqParams),

    ?assertMatch(ok, check_status(get_response_status(DeleteResponse))),

    SpacesResponse3 = get_provider_spaces(ProviderReqParams),
    ?PRINT(SpacesResponse3),

    ?assertMatch([[SID2]], get_response_val([spaces],SpacesResponse3)),

    ReadResponse3 = get_provider_space(SID2, ProviderReqParams),
    ?PRINT(ReadResponse3),
    ?assertMatch([[SID2]], get_response_body(ReadResponse3)).


provider_check_test(Config) ->
    ?PRINT("PROVIDER CHECK TEST"),
    RestAddress = ?config(restAddress, Config),
    ReqParams = {
        RestAddress,
        [
            {"content-type","application/x-www-form-urlencoded"}
        ],
        []
    },
    ResponseIP = check_provider_ip(ReqParams),
    ?PRINT(ResponseIP),
    ResponsePort = check_provider_ports(ReqParams),
    ?PRINT(ResponsePort).

%%%===================================================================

%% user_rest_module_test_group

user_crud_test(Config) ->
    RestAddress = ?config(restAddress, Config),
    {KeyFile, CSRFile, CertFile} = ?config(cert_files, Config),
    {ok, CSR} =file:read_file(CSRFile),
    [Node] = ?config(gr_nodes, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},
    
    RegisterResponse =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, CSR, ReqParams),

    [Cert, ProviderId] = get_response_val([certificate, providerId], RegisterResponse),
    file:write_file(CertFile, Cert),

    %% set request options
    Options = [{ssl_options, [{keyfile, KeyFile}, {certfile, CertFile }]}],
    
    ProviderReqParams = update_req_params(ReqParams, Options, options),

     %% create user
    UserId = create_user(?USER_NAME1, Node),

    %% authenticate user
    NewHeaders = auth_user(UserId, ProviderId, ReqParams, Node),
    UserReqParams = update_req_params(ProviderReqParams, NewHeaders, headers),

    %% check if user data is correct
    ReadResponse = read_user(UserReqParams),
    ?assertMatch([UserId, ?USER_NAME1], get_response_val([userId, name], ReadResponse)),

    update_user(?USER_NAME2, UserReqParams),

    %% check if user data was updated
    ReadResponse2 = read_user(UserReqParams),
    ?assertMatch([UserId, ?USER_NAME2], get_response_val([userId, name], ReadResponse2)),
    
    DeleteResponse = delete_user(UserReqParams),
    ?assertMatch(ok, check_status(get_response_status(DeleteResponse))),

    ReadResponse3 = read_user(UserReqParams),
    ?assertMatch(bad, check_status(get_response_status(ReadResponse3))).

user_space_test(Config) ->
    RestAddress = ?config(restAddress, Config),
    {KeyFile, CSRFile, CertFile} = ?config(cert_files, Config),
    {ok, CSR} =file:read_file(CSRFile),
    [Node] = ?config(gr_nodes, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},

    RegisterResponse =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, CSR, ReqParams),

    [Cert, ProviderId] = get_response_val([certificate, providerId], RegisterResponse),
    file:write_file(CertFile, Cert),

    %% update options
    Options = [{ssl_options, [{keyfile, KeyFile}, {certfile, CertFile }]}],
    ProviderReqParams = update_req_params(ReqParams, Options, options),
    
    %% create users
    UserId = create_user(?USER_NAME1, Node),

    %% authenticate users
    NewHeaders = auth_user(UserId, ProviderId, ReqParams, Node),
    UserReqParams = update_req_params(ProviderReqParams, NewHeaders, headers),

    SCRResponse = create_space_for_user(?SPACE_NAME1, UserReqParams),
    SCRResponse2 = create_space_for_user(?SPACE_NAME2, UserReqParams),

    SID1 = get_header_val("spaces", get_response_headers(SCRResponse)),
    SID2 = get_header_val("spaces", get_response_headers(SCRResponse2)),

    Response = get_user_spaces(UserReqParams),
    ?assertMatch([[SID1, SID2], <<"undefined">>], get_response_val([spaces, default], Response)),

    set_default_space_for_user(SID2, UserReqParams),

    Response2 = get_user_spaces(UserReqParams),
    ?assertMatch([[SID1, SID2], SID2], get_response_val([spaces, default], Response2)),

    DefSpaceResponse = get_user_default_space(UserReqParams),
    ?assertMatch([SID2], get_response_val([spaceId], DefSpaceResponse)),

    Response3 = get_user_space(SID1, UserReqParams),
    ?assertMatch([SID1, ?SPACE_NAME1], get_response_val([spaceId, name], Response3)),

    DeleteResponse = delete_user_space(SID1, UserReqParams),
    ?assertMatch(ok, check_status(get_response_status(DeleteResponse))),

    Response4 = get_user_space(SID1, UserReqParams),
    ?assertMatch(bad, check_status(get_response_status(Response4))).

many_users_space_test(Config) ->
    RestAddress = ?config(restAddress, Config),
    {KeyFile, CSRFile, CertFile} = ?config(cert_files, Config),
    {ok, CSR} =file:read_file(CSRFile),
    [Node] = ?config(gr_nodes, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},

    RegisterResponse =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, CSR, ReqParams),

    [Cert, ProviderId] = get_response_val([certificate, providerId], RegisterResponse),
    file:write_file(CertFile, Cert),

    %% update options
    Options = [{ssl_options, [{keyfile, KeyFile}, {certfile, CertFile }]}],
    ProviderReqParams = update_req_params(ReqParams, Options, options),

    %% create users
    UserId = create_user(?USER_NAME1, Node),
    UserId2 = create_user(?USER_NAME2, Node),

    %% authenticate users
    NewHeaders1 = auth_user(UserId, ProviderId, ReqParams, Node),
    NewHeaders2 = auth_user(UserId2, ProviderId, ReqParams, Node),
    UserReqParams1 = update_req_params(ProviderReqParams, NewHeaders1, headers),
    UserReqParams2 = update_req_params(ProviderReqParams, NewHeaders2, headers),

    SCRResponse = create_space_for_user(?SPACE_NAME1, UserReqParams1),
    SID1 = get_header_val("spaces", get_response_headers(SCRResponse)),

    .
%%%===================================================================

%% group_rest_module_test_group

group_crud_test(Config) ->
    RestAddress = ?config(restAddress, Config),
    {KeyFile, CSRFile, CertFile} = ?config(cert_files, Config),
    {ok, CSR} =file:read_file(CSRFile),
    [Node] = ?config(gr_nodes, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},

    RegisterResponse =
        register_provider(?URLS1,?REDIRECTION_POINT1, ?CLIENT_NAME1, CSR, ReqParams),

    [Cert, ProviderId] = get_response_val([certificate, providerId], RegisterResponse),
    file:write_file(CertFile, Cert),

    %%  set request options
    Options = [{ssl_options, [{keyfile, KeyFile}, {certfile, CertFile }]}],
    ProviderReqParams = update_req_params(ReqParams, Options, options),
    
     %% create users
    UserId = create_user(?USER_NAME1, Node),
    
    NewHeaders = auth_user(UserId, ProviderId, ReqParams, Node),
    UserReqParams = update_req_params(ProviderReqParams, NewHeaders, headers),
    
    %% create group
    CreateResponse = create_group(?GROUP_NAME1, UserReqParams),
    GID = get_header_val("groups", get_response_headers(CreateResponse)),

    ReadResponse = read_group(GID, UserReqParams),
    ?assertMatch([GID, ?GROUP_NAME1], get_response_val([groupId, name], ReadResponse)),

    UpdateResponse = update_group(GID, ?GROUP_NAME2, UserReqParams),
    ?assertMatch(ok, check_status(get_response_status(UpdateResponse))),

    ReadResponse2 = read_group(GID, UserReqParams),
    ?assertMatch([GID, ?GROUP_NAME2], get_response_val([groupId, name], ReadResponse2)),

    DeleteResponse = delete_provider_group(GID, UserReqParams),
    ?assertMatch(ok, check_status(get_response_status(DeleteResponse))),

    ReadResponse3 = read_group(GID, UserReqParams),
    ?assertMatch(bad, check_status(get_response_status(ReadResponse3))).


%%%===================================================================

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    [Node] = ?config(gr_nodes, NewConfig),
    GR_IP = get_node_ip(Node),
    RestPort = get_rest_port(Node),
    RestAddress = "https://" ++ GR_IP ++ ":" ++ integer_to_list(RestPort),
    timer:sleep(10000), % TODO add nagios to GR and delete sleep
    [{restAddress, RestAddress} | NewConfig ].

init_per_testcase(_, Config) ->
    ibrowse:start(),
    ssl:start(),
    [{cert_files,generate_cert_files()}| Config].

end_per_testcase(_, Config) ->
    ssl:stop(),
    ibrowse:stop(),
    {KeyFile, CSRFile, CertFile} = ?config(cert_files, Config),
    file:delete(KeyFile),
    file:delete(CSRFile),
    file:delete(CertFile).

end_per_suite(Config) ->
%%     timer:sleep(30000),
    test_node_starter:clean_environment(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_included(_, []) -> false;
is_included([], _MainList) -> true;
is_included([H|T], MainList) ->
    case lists:member(H, MainList) of
        true -> is_included(T, MainList);
        _ -> false
    end.

get_rest_port(Node)->
    {ok, RestPort} = rpc:call(Node, application, get_env, [?APP_Name, rest_port]),
    RestPort.

%% returns ip (as a string) of given node
get_node_ip(Node) ->
    CMD = "docker inspect --format '{{ .NetworkSettings.IPAddress }}'" ++ " " ++ utils:get_host(Node),
    re:replace(os:cmd(CMD), "\\s+", "", [global,{return,list}]).

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
get_response_val(KeyList, Response) ->
    {JSONOutput} = jiffy:decode(get_response_body(Response)),
    [ proplists:get_value(atom_to_binary(Key,latin1), JSONOutput) || Key <-KeyList ].

get_header_val(Parameter, Headers) ->
    case lists:keysearch("location", 1, Headers) of
        {value, {_HeaderType, HeaderValue}} -> read_http_param(Parameter, HeaderValue);
        false -> failure
    end.

read_http_param(Parameter, HeaderValue)->
    [_, ParamVal] = re:split(HeaderValue, "/" ++ Parameter ++ "/"),
    ParamVal.

check_status(StatusStr) ->
    Status = list_to_integer(StatusStr),
    case (Status >= 200) and (Status < 300) of
        true -> ok;
        _ -> bad
%%     TODO
%%     replace failure atom with raising exception
    end.

%% returns list of values from responsebody
do_request(Endpoint, Headers, Method) ->
    do_request(Endpoint, Headers, Method, [], []).
do_request(Endpoint, Headers, Method, Body) ->
    do_request(Endpoint, Headers, Method, Body, []).
do_request(Endpoint, Headers, Method, Body, Options) ->
    case ibrowse:send_req(Endpoint, Headers, Method, Body, Options) of
%%         {error, Reason} -> error(Reason);
%%         {ibrowse_req_id, ReqID} -> error("Ibrowse req_id: " ++ ReqID);
        Response -> Response
    end.

get_macaroon_id(Token) ->
    {ok, Macaroon} = macaroon:deserialize(Token),
    {ok, [{_, Identifier}]} = macaroon:third_party_caveats(Macaroon),
    Identifier.

%% TODO chyba mozna usunac
update_headers(Headers, SerializedMacaroon, SerializedDischarges) ->
        [
            {"discharge-macaroons", SerializedDischarges},
            {"macaroon", SerializedMacaroon}
            | Headers
        ].

prepare_macaroons_headers(SerializedMacaroon, SerializedDischarges)->
    {ok, Macaroon} = macaroon:deserialize(SerializedMacaroon),
    BoundMacaroons = lists:map(
        fun(SrlzdDischMacaroon) ->
            {ok, DM} = macaroon:deserialize(SrlzdDischMacaroon),
            {ok, BDM} = macaroon:prepare_for_request(Macaroon, DM),
            {ok, SBDM} = macaroon:serialize(BDM),
            binary_to_list(SBDM)
        end, [list_to_binary(SerializedDischarges)]),
    [
        {"macaroon", binary_to_list(SerializedMacaroon)},
        {"discharge-macaroons", BoundMacaroons}
    ].

update_req_params(ReqParams, NewParam, headers) ->
    {RestAddress, Headers, Options} = ReqParams,
    {RestAddress, Headers ++ NewParam, Options};
update_req_params(ReqParams, NewParam, options) ->
    {RestAddress, Headers, Options} = ReqParams,
    {RestAddress, Headers, Options++ NewParam}.

%% Provider functions
register_provider(URLS, RedirectionPoint, ClientName, CSR, ReqParams) ->
    Body = jiffy:encode({[
        {urls,URLS},
        {csr, CSR},
        {redirectionPoint, RedirectionPoint},
        {clientName, ClientName}
    ]}),
    {RestAddress, Headers, _Options} = ReqParams,
    do_request(RestAddress ++ "/provider", Headers, post, Body).

read_provider(ReqParams) ->
 {RestAddress, Headers, Options} = ReqParams,
 do_request(RestAddress ++ "/provider", Headers, get, [], Options).

get_provider_data_by_pid(PID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/provider/" ++ binary_to_list(PID), Headers, get, [], Options).

update_provider(URLS, RedirectionPoint, ClientName, ReqParams) ->
    Body = jiffy:encode({[
        {urls,URLS},
        {redirectionPoint, RedirectionPoint},
        {clientName, ClientName}
    ]}),
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/provider", Headers, patch, Body, Options).

delete_provider(ReqParams) ->
    {RestAddress, _Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/provider", [], delete,[], Options).

create_space_for_provider(Token, SpaceName, Size, ReqParams) ->
  {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {name, SpaceName},
        {token, Token},
        {size, Size}
    ]}),
    do_request(RestAddress ++ "/provider/spaces", Headers, post, Body, Options).

get_provider_spaces(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/provider/spaces", Headers, get, [], Options).

get_provider_space(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/provider/spaces/" ++ binary_to_list(SID), Headers, get, [], Options).

delete_provider_space(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/provider/spaces/" ++ binary_to_list(SID), Headers, delete, [], Options).

get_space_support_token(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    ?PRINT(SID),
    ?PRINT(ReqParams),
    do_request(
        RestAddress ++ "/spaces/" ++ binary_to_list(SID) ++"/providers/token", Headers, get, [], Options
    ).

support_provider_space(Token, Size, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {token, Token}, 
        {size, Size}
    ]}),
    do_request(RestAddress ++ "/provider/spaces/support", Headers, post, Body, Options).


check_provider_ip(ReqParams)->
{RestAddress, _, _} = ReqParams,
do_request(RestAddress ++ "/provider/test/check_my_ip", [], get).

check_provider_ports(ReqParams)->
{RestAddress, _, _} = ReqParams,
do_request(RestAddress ++ "/provider/test/check_my_ports", [], post).


%% User functions

create_user(UserName, Node) ->
    {ok, UserId} = rpc:call(Node, user_logic, create, [#user{name = UserName}]),
    UserId.

%% create_user(UserName, ReqParams) ->
%%     {RestAddress, Headers, Options} = ReqParams,
%%     Body = jiffy:encode({[
%%         {name, UserName}
%%     ]}),
%%     do_request(RestAddress ++ "/user", Headers, post, Body, Options).

read_user(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/user", Headers, get,[], Options).

update_user(NewUserName, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {name, NewUserName}
    ]}),
    do_request(RestAddress ++ "/user", Headers, patch, Body, Options).

delete_user(ReqParams) ->
     {RestAddress, Headers, Options} = ReqParams,
     do_request(RestAddress ++ "/user", Headers, delete, [], Options).

get_user_spaces(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/user/spaces", Headers, get, [], Options).

get_user_space(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/user/spaces/" ++ binary_to_list(SID), Headers, get, [], Options).

get_user_default_space(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/user/spaces/default", Headers, get, [], Options).

create_space_for_user(SpaceName, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {name, SpaceName}
    ]}),
    do_request(RestAddress ++ "/user/spaces", Headers, post, Body, Options).

set_default_space_for_user(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {spaceId, SID}
    ]}),
    do_request(RestAddress ++ "/user/spaces/default", Headers, put, Body, Options).

get_space_creation_token(ReqParams)->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/user/spaces/token", Headers, get, [], Options).

get_space_invitation_token(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/spaces/" ++ binary_to_list(SID)++ "users/token", Headers, get, [], Options).

delete_user_space(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/user/spaces/" ++ binary_to_list(SID), Headers, delete, [], Options).

%% returns updated headers (with macaroons)
auth_user(UserId, ProviderId, ReqParams, Node) ->
    SerializedMacaroon = rpc:call(Node, auth_logic, gen_token, [UserId, ProviderId]),
    {RestAddress, Headers, _Options} = ReqParams,
    Identifier = get_macaroon_id(SerializedMacaroon),
    Body = jiffy:encode({[{identifier, Identifier}]}),
    Resp = do_request(RestAddress ++ "/user/authorize", Headers, post, Body),
    {ok, _, _, SerializedDischarges} = Resp,
    prepare_macaroons_headers(SerializedMacaroon, SerializedDischarges).

%% Group functions

create_group(GroupName, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {name, GroupName}
    ]}),
    do_request(RestAddress ++ "/groups", Headers, post, Body, Options).

read_group(GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/groups/" ++ binary_to_list(GID) , Headers, get,[], Options).

update_group(GID, NewGroupName, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
     Body = jiffy:encode({[
        {name, NewGroupName}
    ]}),
    do_request(RestAddress ++ "/groups/" ++ binary_to_list(GID) , Headers, patch, Body, Options).

delete_provider_group(GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/groups/" ++ binary_to_list(GID) , Headers, delete,[], Options).


%% Spaces functions
