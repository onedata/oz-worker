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

%% TODO delete after finishing
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
-define(GROUP_PRIVILEGES,
    [
        group_view_data, group_change_data, group_invite_user,
        group_remove_user, group_join_space, group_create_space,
        group_set_privileges, group_remove, group_leave_space,
        group_create_space_token
    ]
).

%% API
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([provider_crud_test/1, user_space_test/1, user_crud_test/1, group_crud_test/1,
    provider_space_test/1, provider_check_test/1, many_users_space_test/1, register_provider/5,
    create_group_for_user/2, user_group_test/1, many_users_group_test/1, group_users_test/1,
    group_privileges_test/1, group_spaces_test/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-performance({test_cases, []}).

all() ->
    [
        {group, provider_rest_module_test_group},
        {group, user_rest_module_test_group},
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
                user_space_test,
                many_users_space_test,
                user_group_test,
                many_users_group_test
            ]
        }
        ,
        {
            group_rest_module_test_group,
            [],
            [
                group_crud_test,
                group_users_test,
                group_privileges_test,
                group_spaces_test
            ]

        }
    ].

%%%===================================================================
%%% Test functions
%%%===================================================================


%% provider_rest_module_test_group====================================

provider_crud_test(Config) ->
    RestAddress = ?config(restAddress, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},
    
    {ProviderId, ProviderReqParams} =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, Config, ReqParams),

    ?assertMatch(
        [?CLIENT_NAME1, ?URLS1, ?REDIRECTION_POINT1, ProviderId],
        read_provider(ProviderReqParams)
    ),

    update_provider(?URLS2, ?REDIRECTION_POINT2, ?CLIENT_NAME2, ProviderReqParams),

    ?assertMatch(
        [?CLIENT_NAME2, ?URLS2, ?REDIRECTION_POINT2, ProviderId],
        read_provider(ProviderReqParams)
    ),

    ?assertMatch(
        [?CLIENT_NAME2, ?URLS2, ?REDIRECTION_POINT2, ProviderId],
        get_provider_data_by_pid(ProviderId, ProviderReqParams)
    ),

    delete_provider(ProviderReqParams),

    ?assertMatch(request_error,read_provider(ProviderReqParams)).

provider_space_test(Config) ->
    ?PRINT("PROVIDER SPACE TEST"),
    RestAddress = ?config(restAddress, Config),
    [Node] = ?config(gr_nodes, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},

    {ProviderId, ProviderReqParams} =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, Config, ReqParams),

    %% create user
    UserId = create_user(?USER_NAME1, Node),

    %% authenticate user
    NewHeaders = auth_user(UserId, ProviderId, ReqParams, Node),
    UserReqParams = update_req_params(ProviderReqParams, NewHeaders, headers),

    %% get space creation token1
    SCRToken1 = get_space_creation_token_for_user(UserReqParams),

    SID1 = create_space_for_provider(SCRToken1, ?SPACE_NAME1, ?SPACE_SIZE1, ProviderReqParams),

    %% get space creation token2
    SCRToken2 = get_space_creation_token_for_user(UserReqParams),

    SID2 = create_space_for_provider(SCRToken2, ?SPACE_NAME2, ?SPACE_SIZE2, ProviderReqParams),

    SupportedSpaces = get_provider_spaces(ProviderReqParams),
%%     ?PRINT(SupportedSpaces),

%%     TODO below assert fails
    ?assertMatch(true, is_included([SID1, SID2], SupportedSpaces)),

%% TODO sprawdzanie czy funkcja, ktora nic nie zwraca dzfiala ok, moze powinna zwracac ok

    DeleteResponse = delete_provider_space(SID1, ProviderReqParams),

    ?assertMatch(ok, check_status(DeleteResponse)),

    SupportedSpaces2 = get_provider_spaces(ProviderReqParams),
    ?PRINT(SupportedSpaces2),

%%     TODO below assert fails
    ?assertMatch([[SID2]], SupportedSpaces2),

    [
        SID_test, SpaceName_test, {[{ProviderId_test, SpaceSize}]}
    ] = get_provider_space(SID2, ProviderReqParams),

    ?assertMatch(
        [
            SID_test, SpaceName_test, ProviderId_test, SpaceSize
        ],
        [
            SID2, ?SPACE_NAME2, ProviderId, binary_to_integer(?SPACE_SIZE2)
        ]
    ).

provider_check_test(Config) ->
    ?PRINT("PROVIDER CHECK TEST"),
    RestAddress = ?config(restAddress, Config),
    ReqParams = {RestAddress, [], []},

    ?assertMatch(ok, check_status(check_provider_ip(ReqParams))),
    Response = check_provider_ports(ReqParams),
    ?PRINT(Response),
    ?assertMatch(ok, check_status(Response)).

%%%===================================================================

%% user_rest_module_test_group========================================

user_crud_test(Config) ->
    RestAddress = ?config(restAddress, Config),
    [Node] = ?config(gr_nodes, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},
    
    {ProviderId, ProviderReqParams} =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, Config, ReqParams),

     %% create user
    UserId = create_user(?USER_NAME1, Node),

    %% authenticate user
    NewHeaders = auth_user(UserId, ProviderId, ReqParams, Node),
    UserReqParams = update_req_params(ProviderReqParams, NewHeaders, headers),

    %% check if user data is correct
    ?assertMatch(
        [UserId, ?USER_NAME1],
        read_user(UserReqParams)
    ),

    update_user(?USER_NAME2, UserReqParams),

    %% check if user data was updated
    ?assertMatch(
        [UserId, ?USER_NAME2],
        read_user(UserReqParams)
    ),

     UserId2 = create_user(?USER_NAME2, Node),

    %% authenticate user
    NewHeaders2 = auth_user(UserId2, ProviderId, ReqParams, Node),
    UserReqParams2 = update_req_params(ProviderReqParams, NewHeaders2, headers),

    MergeToken = get_user_merge_token(UserReqParams2),

%%     TODO whoses UID and username stays ?
    ?PRINT(UserId),
    ?PRINT(UserId2),

    %%  merge user2 with user1
    Response = merge_users(MergeToken, UserReqParams),

    ?assertMatch(ok, check_status(Response)),

    ?PRINT(read_user(UserReqParams)),

    DeleteResponse = delete_user(UserReqParams),
    ?assertMatch(ok, check_status(DeleteResponse)),

    %% try to read deleted user
    ?assertMatch(request_error, read_user(UserReqParams)).

user_space_test(Config) ->
    RestAddress = ?config(restAddress, Config),
    [Node] = ?config(gr_nodes, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},

    {ProviderId, ProviderReqParams} =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, Config, ReqParams),
    
    %% create user
    UserId = create_user(?USER_NAME1, Node),

    %% authenticate user
    NewHeaders = auth_user(UserId, ProviderId, ReqParams, Node),
    UserReqParams = update_req_params(ProviderReqParams, NewHeaders, headers),

    SID1 = create_space_for_user(?SPACE_NAME1, UserReqParams),
    SID2 = create_space_for_user(?SPACE_NAME2, UserReqParams),

    ?assertMatch(
        [[SID1, SID2], <<"undefined">>],
        get_user_spaces(UserReqParams)
    ),

    set_default_space_for_user(SID2, UserReqParams),

    ?assertMatch(
        [[SID1, SID2], SID2],
        get_user_spaces(UserReqParams)
    ),

    ?assertMatch(SID2, get_user_default_space(UserReqParams)),

    ?assertMatch([SID1, ?SPACE_NAME1], get_user_space(SID1, UserReqParams)),

    DeleteResponse = delete_user_space(SID1, UserReqParams),
    ?assertMatch(ok, check_status(DeleteResponse)),

    %% try to read deleted user's space
    ?assertMatch(request_error, get_user_space(SID1, UserReqParams)).

many_users_space_test(Config) ->
    RestAddress = ?config(restAddress, Config),
    [Node] = ?config(gr_nodes, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},

    {ProviderId, ProviderReqParams} =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, Config, ReqParams),

    %% create users
    UserId = create_user(?USER_NAME1, Node),
    UserId2 = create_user(?USER_NAME2, Node),

    %% authenticate users
    NewHeaders1 = auth_user(UserId, ProviderId, ReqParams, Node),
    NewHeaders2 = auth_user(UserId2, ProviderId, ReqParams, Node),
    UserReqParams1 = update_req_params(ProviderReqParams, NewHeaders1, headers),
    UserReqParams2 = update_req_params(ProviderReqParams, NewHeaders2, headers),

    SID1 = create_space_for_user(?SPACE_NAME1, UserReqParams1),

    SupportToken = get_space_support_token(SID1, UserReqParams1),

    SupportResponse = provider_support_space(SupportToken, ?SPACE_SIZE1, ProviderReqParams),

    ?assertMatch(ok, check_status(SupportResponse)),

    InvitationToken = get_space_invitation_token(user, SID1, UserReqParams1),

    SID2 = join_user_to_space(InvitationToken, UserReqParams2),

    %% check if SID returned for user2 is the same as SID1
    ?assertMatch(SID1, SID2),

    ?PRINT(get_user_spaces(UserReqParams2)),

    %% check if space is in list of user2 space
    ?assertMatch(
        [[SID1], <<"undefined">>],
        get_user_spaces(UserReqParams2)
    ).

user_group_test(Config) ->
    RestAddress = ?config(restAddress, Config),
    [Node] = ?config(gr_nodes, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},
    {ProviderId, ProviderReqParams} =
    register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, Config, ReqParams),

    %% create users
    UserId = create_user(?USER_NAME1, Node),

    %% authenticate user
    NewHeaders = auth_user(UserId, ProviderId, ReqParams, Node),
    UserReqParams = update_req_params(ProviderReqParams, NewHeaders, headers),

    GID1 = create_group_for_user(?GROUP_NAME1, UserReqParams),
%%     ?PRINT(GID1),

    GID2 = create_group_for_user(?GROUP_NAME2, UserReqParams),
%%     ?PRINT(GID2),
    
%%     ?PRINT(get_user_groups(UserReqParams)),

    ?assertMatch(
        true,
        is_included([GID1, GID2], get_user_groups(UserReqParams))
    ),

    Response = user_leave_group(GID2,UserReqParams),

    ?assertMatch(ok, check_status(Response)),

    ?assertMatch(
        false,
        is_included([GID2], get_user_groups(UserReqParams))
    ),

    ?assertMatch(
        [GID1, ?GROUP_NAME1],
        get_user_group(GID1, UserReqParams)
    ).

many_users_group_test(Config) ->
    RestAddress = ?config(restAddress, Config),
    [Node] = ?config(gr_nodes, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},

    {ProviderId, ProviderReqParams} =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, Config, ReqParams),

    %% create users
    UserId1 = create_user(?USER_NAME1, Node),
    UserId2 = create_user(?USER_NAME2, Node),

    %% authenticate users
    NewHeaders1 = auth_user(UserId1, ProviderId, ReqParams, Node),
    NewHeaders2 = auth_user(UserId2, ProviderId, ReqParams, Node),
    UserReqParams1 = update_req_params(ProviderReqParams, NewHeaders1, headers),
    UserReqParams2 = update_req_params(ProviderReqParams, NewHeaders2, headers),

    GID1 = create_group_for_user(?GROUP_NAME1, UserReqParams1),

    InvitationToken = get_group_invitation_token(GID1, UserReqParams1),

    GID2 = join_user_to_group(InvitationToken, UserReqParams2),

    %% check if GID returned for user2 is the same as GID1
    ?assertMatch(GID1, GID2),

    ?assertMatch(
        [GID1, ?GROUP_NAME1],
        get_user_group(GID2, UserReqParams2)
    ),

    ?PRINT(get_group_users(GID1, UserReqParams1)),
    ?PRINT(UserId1),
    ?PRINT(UserId2),

    ?assertMatch(
        true,
        is_included([UserId1, UserId2], get_group_users(GID1, UserReqParams1))
    ).

%% group_rest_module_test_group =======================================

group_crud_test(Config) ->
    RestAddress = ?config(restAddress, Config),
    [Node] = ?config(gr_nodes, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},

    {ProviderId, ProviderReqParams} =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, Config, ReqParams),

     %% create users
    UserId = create_user(?USER_NAME1, Node),

    %% authenticate user
    NewHeaders = auth_user(UserId, ProviderId, ReqParams, Node),
    UserReqParams = update_req_params(ProviderReqParams, NewHeaders, headers),
    
    %% create group
    GID = create_group(?GROUP_NAME1, UserReqParams),

    ?assertMatch(
        [GID, ?GROUP_NAME1],
        read_group(GID, UserReqParams)
    ),

    UpdateResponse = update_group(GID, ?GROUP_NAME2, UserReqParams),
    ?assertMatch(ok, check_status(UpdateResponse)),

    ?assertMatch(
        [GID, ?GROUP_NAME2],
        read_group(GID, UserReqParams)
    ),

    DeleteResponse = delete_provider_group(GID, UserReqParams),
    ?assertMatch(ok, check_status(DeleteResponse)),

    ?assertMatch(request_error, read_group(GID, UserReqParams)).

group_users_test(Config) ->
    RestAddress = ?config(restAddress, Config),
    [Node] = ?config(gr_nodes, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},

    {ProviderId, ProviderReqParams} =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, Config, ReqParams),

     %% create users
    UserId1 = create_user(?USER_NAME1, Node),
    UserId2 = create_user(?USER_NAME2, Node),

    %% authenticate users
    NewHeaders = auth_user(UserId1, ProviderId, ReqParams, Node),
    UserReqParams1 = update_req_params(ProviderReqParams, NewHeaders, headers),
    NewHeaders2 = auth_user(UserId2, ProviderId, ReqParams, Node),
    UserReqParams2 = update_req_params(ProviderReqParams, NewHeaders2, headers),

    GID1 = create_group(?GROUP_NAME1, UserReqParams1),
    
    ?assertMatch(
        [UserId1, ?USER_NAME1],
        get_group_user(GID1, UserId1, UserReqParams1)
    ),

    ?assertMatch(
        [UserId1],
        get_group_users(GID1, UserReqParams1)
    ),

    DeleteResponse = delete_group_user(GID1, UserId1, UserReqParams1),

    ?assertMatch(ok, check_status(DeleteResponse)).

group_privileges_test(Config) ->
    RestAddress = ?config(restAddress, Config),
    [Node] = ?config(gr_nodes, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},

    {ProviderId, ProviderReqParams} =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, Config, ReqParams),

     %% create users
    UserId1 = create_user(?USER_NAME1, Node),
    UserId2 = create_user(?USER_NAME2, Node),

    %% authenticate users
    NewHeaders = auth_user(UserId1, ProviderId, ReqParams, Node),
    UserReqParams1 = update_req_params(ProviderReqParams, NewHeaders, headers),
    NewHeaders2 = auth_user(UserId2, ProviderId, ReqParams, Node),
    UserReqParams2 = update_req_params(ProviderReqParams, NewHeaders2, headers),

    GID = create_group(?GROUP_NAME1, UserReqParams1),

    InvitationToken = get_group_invitation_token(GID, UserReqParams1),

    %% add user to group
    join_user_to_group(InvitationToken, UserReqParams2),

    %% TODO check user creator privileges
    get_group_user_privileges(GID, UserId1, UserReqParams1),

    %% TODO check user creator privileges
    get_group_user_privileges(GID, UserId2, UserReqParams2),

    Privileges =
        [
            group_view_data, group_change_data, group_invite_user,group_set_privileges
%%  ,           group_remove_user, group_join_space,
%%             group_create_space, group_remove, group_leave_space,
%%              group_create_space_token
        ],
    ?assertMatch(
        ok,
        group_privileges_check(Privileges, {UserId1, UserReqParams1}, {UserId2, UserReqParams2}, GID)
    ).

group_spaces_test(Config) ->
    RestAddress = ?config(restAddress, Config),
    [Node] = ?config(gr_nodes, Config),
    ReqParams = {RestAddress, ?CONTENT_TYPE_HEADER, []},

    {ProviderId, ProviderReqParams} =
        register_provider(?URLS1, ?REDIRECTION_POINT1, ?CLIENT_NAME1, Config, ReqParams),

     %% create user
    UserId1 = create_user(?USER_NAME1, Node),

    %% authenticate user
    NewHeaders = auth_user(UserId1, ProviderId, ReqParams, Node),
    UserReqParams1 = update_req_params(ProviderReqParams, NewHeaders, headers),

    GID = create_group(?GROUP_NAME1, UserReqParams1),
    SID1 = create_space_for_group(?SPACE_NAME1, GID, UserReqParams1),
    
    ?assertMatch(
        [SID1, ?SPACE_NAME1],
        get_group_space(GID, SID1, UserReqParams1)
    ),
    
    SID2 = create_space_for_user(?SPACE_NAME2, UserReqParams1),

    InvitationToken = get_space_invitation_token(group, SID2, UserReqParams1),

    SID_check = join_group_to_space(InvitationToken, GID, UserReqParams1),

    ?assertMatch(SID2, SID_check),

    ?assertMatch(
        true,
        is_included([SID1, SID2], get_group_spaces(GID, UserReqParams1))
    ),

    DeleteResponse = group_leave_space(GID, SID2, UserReqParams1),

    ?assertMatch(
        ok,
        check_status(DeleteResponse)
    ),

    ?assertMatch(
        true,
        is_included([SID1], get_group_spaces(GID, UserReqParams1))
    ).


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
get_body_val(KeyList, Response) ->
    case check_status(Response) of
        bad -> request_error;
        _ -> {JSONOutput} = jiffy:decode(get_response_body(Response)),
            [ proplists:get_value(atom_to_binary(Key,latin1), JSONOutput) || Key <-KeyList ]
    end.

get_header_val(Parameter, Response) ->
    case check_status(Response) of
        bad -> request_error;
        _ -> case lists:keysearch("location", 1, get_response_headers(Response)) of
                {value, {_HeaderType, HeaderValue}} -> read_http_param(Parameter, HeaderValue);
                false -> parameter_not_in_heading
            end
    end.

read_http_param(Parameter, HeaderValue)->
    [_, ParamVal] = re:split(HeaderValue, "/" ++ Parameter ++ "/"),
    ParamVal.

check_status(Response) ->
    Status = list_to_integer(get_response_status(Response)),
    case (Status >= 200) and (Status < 300) of
        true -> ok;
        _ -> bad
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

new_privileges(OldPrivileges, NewPrivilige) ->
    [NewPrivilige | OldPrivileges].

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
register_provider(URLS, RedirectionPoint, ClientName, Config, ReqParams) ->
    {RestAddress, Headers, _Options} = ReqParams,
    {KeyFile, CSRFile, CertFile} = ?config(cert_files, Config),
    {ok, CSR} =file:read_file(CSRFile),
    Body = jiffy:encode({[
        {urls,URLS},
        {csr, CSR},
        {redirectionPoint, RedirectionPoint},
        {clientName, ClientName}
    ]}),
    Response = do_request(RestAddress ++ "/provider", Headers, post, Body),
    %% save cert
    [Cert, ProviderId] = get_body_val([certificate, providerId], Response),
    file:write_file(CertFile, Cert),

    %% set request options for provider
    Options = [{ssl_options, [{keyfile, KeyFile}, {certfile, CertFile }]}],
    %% set request parametres for provider
    ProviderReqParams = update_req_params(ReqParams, Options, options),
    {ProviderId, ProviderReqParams}.

read_provider(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(RestAddress ++ "/provider", Headers, get, [], Options),
    get_body_val([clientName, urls, redirectionPoint, providerId], Response).

get_provider_data_by_pid(PID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(RestAddress ++ "/provider/" ++ binary_to_list(PID), Headers, get, [], Options),
    get_body_val([clientName, urls, redirectionPoint, providerId], Response).

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
    Response = do_request(RestAddress ++ "/provider/spaces", Headers, post, Body, Options),
    get_header_val("spaces", Response).

get_provider_spaces(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(RestAddress ++ "/provider/spaces", Headers, get, [], Options),
    [Spaces] = get_body_val([spaces],Response),
    Spaces.

get_provider_space(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(RestAddress ++ "/provider/spaces/" ++ binary_to_list(SID), Headers, get, [], Options),
    get_body_val([spaceId, name, size],Response).

delete_provider_space(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/provider/spaces/" ++ binary_to_list(SID), Headers, delete, [], Options).

check_provider_ip(ReqParams)->
    {RestAddress, _, _} = ReqParams,
    do_request(RestAddress ++ "/provider/test/check_my_ip", [], get).

check_provider_ports(ReqParams)->
    {RestAddress, _, _} = ReqParams,
    do_request(RestAddress ++ "/provider/test/check_my_ports", [], post).

provider_support_space(Token, Size, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {token, Token}, 
        {size, Size}
    ]}),
    do_request(RestAddress ++ "/provider/spaces/support", Headers, post, Body, Options).


%% User functions

create_user(UserName, Node) ->
    {ok, UserId} = rpc:call(Node, user_logic, create, [#user{name = UserName}]),
    UserId.

read_user(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(RestAddress ++ "/user", Headers, get,[], Options),
    get_body_val([userId, name], Response).

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
    Response = do_request(RestAddress ++ "/user/spaces", Headers, get, [], Options),
    get_body_val([spaces, default], Response).

get_user_space(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(RestAddress ++ "/user/spaces/" ++ binary_to_list(SID), Headers, get, [], Options),
    get_body_val([spaceId, name], Response).

get_user_default_space(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(RestAddress ++ "/user/spaces/default", Headers, get, [], Options),
    [DefSpace] = get_body_val([spaceId], Response),
    DefSpace.

create_space_for_user(SpaceName, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {name, SpaceName}
    ]}),
    Response = do_request(RestAddress ++ "/user/spaces", Headers, post, Body, Options),
    get_header_val("spaces", Response).

set_default_space_for_user(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {spaceId, SID}
    ]}),
    do_request(RestAddress ++ "/user/spaces/default", Headers, put, Body, Options).

get_user_merge_token(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(RestAddress ++ "/user/merge/token", Headers, get, [], Options),
    [Token] = get_body_val([token], Response),
    Token.

merge_users(Token, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
     Body = jiffy:encode({[
        {token, Token}
    ]}),
    do_request(RestAddress ++ "/user/merge", Headers, post, Body, Options).

delete_user_space(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/user/spaces/" ++ binary_to_list(SID), Headers, delete, [], Options).

%% returns macaroons headers
auth_user(UserId, ProviderId, ReqParams, Node) ->
    SerializedMacaroon = rpc:call(Node, auth_logic, gen_token, [UserId, ProviderId]),
    {RestAddress, Headers, _Options} = ReqParams,
    Identifier = get_macaroon_id(SerializedMacaroon),
    Body = jiffy:encode({[{identifier, Identifier}]}),
    Resp = do_request(RestAddress ++ "/user/authorize", Headers, post, Body),
    {ok, _, _, SerializedDischarges} = Resp,
    prepare_macaroons_headers(SerializedMacaroon, SerializedDischarges).

join_user_to_space(Token, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {token, Token}
    ]}),
    Response = do_request(RestAddress ++ "/user/spaces/join", Headers, post, Body, Options),
    get_header_val("user/spaces",Response).
    
create_group_for_user(GroupName, ReqParams)->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {name, GroupName}
    ]}),
    Response = do_request(RestAddress ++ "/user/groups", Headers, post, Body, Options),
    get_header_val("groups", Response).

get_user_groups(ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(RestAddress ++ "/user/groups", Headers, get, [], Options),
    [Groups] = get_body_val([groups], Response),
    Groups.

get_user_group(GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(RestAddress ++ "/user/groups/" ++ binary_to_list(GID) , Headers, get,[], Options),
    get_body_val([groupId, name], Response).

user_leave_group(GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/user/groups/" ++ binary_to_list(GID), Headers, delete, [], Options).

join_user_to_group(Token, ReqParams) ->
     {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {token, Token}
    ]}),
    Response = do_request(RestAddress ++ "/user/groups/join", Headers, post, Body, Options),
    get_header_val("user/groups",Response).

%% Group functions

create_group(GroupName, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {name, GroupName}
    ]}),
    Response = do_request(RestAddress ++ "/groups", Headers, post, Body, Options),
    get_header_val("groups", Response).

read_group(GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(RestAddress ++ "/groups/" ++ binary_to_list(GID) , Headers, get,[], Options),
    get_body_val([groupId, name], Response).

update_group(GID, NewGroupName, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
     Body = jiffy:encode({[
        {name, NewGroupName}
    ]}),
    do_request(RestAddress ++ "/groups/" ++ binary_to_list(GID) , Headers, patch, Body, Options).

delete_provider_group(GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(RestAddress ++ "/groups/" ++ binary_to_list(GID) , Headers, delete,[], Options).

get_group_invitation_token(GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/groups/" ++ binary_to_list(GID)++ "/users/token", Headers, get, [], Options
        ),
%%     ?PRINT(Response),
    [Token] = get_body_val([token], Response),
    Token.

get_group_users(GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/groups/" ++ binary_to_list(GID)++ "/users", Headers, get, [], Options
        ),
    %% TODO remove below line after tests
    ?PRINT(Response),
    [Users] = get_body_val([users], Response),
    Users.

get_group_user(GID, UID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/groups/" ++ binary_to_list(GID) ++ "/users/" ++ binary_to_list(UID),
            Headers, get, [], Options
        ),
    %% TODO remove below line after tests
    ?PRINT(Response),
    get_body_val([userId, name], Response).

delete_group_user(GID, UID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(
        RestAddress ++ "/groups/" ++ binary_to_list(GID) ++ "/users/" ++ binary_to_list(UID),
        Headers, delete, [], Options
    ).


get_group_user_privileges(GID, UID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/groups/" ++ binary_to_list(GID) ++ "/users/" ++
                binary_to_list(UID) ++ "/privileges",
            Headers, get, [], Options
        ),
    %% TODO remove below line after tests
    ?PRINT(Response),
    Response.

set_group_user_privileges(GID, UID, Privileges, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/groups/" ++ binary_to_list(GID) ++ "/users/" ++
                binary_to_list(UID) ++ "/privileges",
            Headers, post, [], Options
        ),
    %% TODO remove below line after tests
    ?PRINT(Response),
    Response.

get_group_spaces(GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/groups/" ++ binary_to_list(GID)++ "/spaces", Headers, get, [], Options
        ),
    %% TODO remove below line after tests
    ?PRINT(Response),
    [Spaces] = get_body_val([spaces], Response),
    Spaces.

get_group_space(GID, SID, ReqParams) ->
   {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/groups/" ++ binary_to_list(GID) ++ "/spaces/" ++ binary_to_list(SID),
            Headers, get, [], Options),
    get_body_val([spaceId, name], Response).

create_space_for_group(Name, GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {name, Name}
    ]}),
    Response = do_request(
        RestAddress ++ "/groups/" ++ binary_to_list(GID)++ "/spaces", Headers, post, Body, Options
    ),
    get_header_val("spaces", Response).
    

group_leave_space(GID, SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    do_request(
        RestAddress ++ "/groups/" ++ binary_to_list(GID)++ "/spaces/" ++ binary_to_list(SID),
        Headers, delete, [], Options
    ).

join_group_to_space(Token, GID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Body = jiffy:encode({[
        {token, Token}
    ]}),
    Response =
        do_request(
            RestAddress ++ "/groups/" ++ binary_to_list(GID)++ "/spaces/join",
            Headers, post, Body, Options
        ),
    get_header_val("groups/" ++ binary_to_list(GID) ++"/spaces/", Response).

%% Spaces functions

get_space_creation_token_for_user(ReqParams)->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(RestAddress ++ "/user/spaces/token", Headers, get, [], Options),
    [Token] = get_body_val([token], Response),
    Token.

get_space_creation_token_for_group(GID, ReqParams)->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(
        RestAddress ++ "/groups/"++ binary_to_list(GID) ++ "/spaces/token", Headers, get, [], Options
    ),
    [Token] = get_body_val([token], Response),
    Token.

get_space_invitation_token(UserType, ID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response =
        do_request(
            RestAddress ++ "/spaces/" ++ binary_to_list(ID)++ "/" ++ atom_to_list(UserType)++"/token",
            Headers, get, [], Options
        ),
    [Token] = get_body_val([token], Response),
    Token.

get_space_support_token(SID, ReqParams) ->
    {RestAddress, Headers, Options} = ReqParams,
    Response = do_request(
        RestAddress ++ "/spaces/" ++ binary_to_list(SID) ++"/providers/token", Headers, get, [], Options
    ),
    [Token] = get_body_val([token], Response),
    Token.


%% Group privileges tests

group_privileges_check([], _, _, _) -> ok;
group_privileges_check([FirstPrivilige | Privileges], UserData1, UserData2, GID) ->
    group_privileges_check(FirstPrivilige,  UserData1, UserData2, GID),
    group_privileges_check(Privileges, UserData1, UserData2, GID);
group_privileges_check(group_view_data, UserData1, UserData2, GID) ->
    {UserId1, UserReqParams1} = UserData1,
    {UserId2, UserReqParams2} = UserData2,

    %% user who belongs to group should have group_view_data privilege by default

    ?assertMatch(
        [GID, ?GROUP_NAME1],
        read_group(GID, UserReqParams2)
    );
group_privileges_check(group_change_data, UserData1, UserData2, GID) ->
    {UserId1, UserReqParams1} = UserData1,
    {UserId2, UserReqParams2} = UserData2,

     %% test if user2 lacks group_change_data privileges
    ?assertMatch(
        bad,
        check_status(update_group(GID, ?GROUP_NAME2, UserReqParams2))
    ),

    set_group_user_privileges(GID, UserId2, [group_change_data], UserReqParams1),

    ?assertMatch(
        ok,
        check_status(update_group(GID, ?GROUP_NAME2, UserReqParams2))
    );
group_privileges_check(group_invite_user, UserData1, UserData2, GID) ->
    {UserId1, UserReqParams1} = UserData1,
    {UserId2, UserReqParams2} = UserData2,

     %% test if user2 lacks group_change_data privileges
    ?assertMatch(
        bad,
        check_status(get_group_invitation_token(GID, UserReqParams2))
    ),

    set_group_user_privileges(GID, UserId2, [group_invite_user], UserReqParams1),

    ?assertMatch(
        ok,
        check_status(get_group_invitation_token(GID, UserReqParams2))
    );
group_privileges_check(group_set_privileges, UserData1, UserData2, GID) ->
    {UserId1, UserReqParams1} = UserData1,
    {UserId2, UserReqParams2} = UserData2,

     %% test if user2 lacks group_change_data privileges
    ?assertMatch(
        bad,
        check_status(set_group_user_privileges(GID, UserId1, ?GROUP_PRIVILEGES, UserReqParams2))
    ),

    set_group_user_privileges(GID, UserId2, [group_set_privileges], UserReqParams1),

    ?assertMatch(
        ok,
        check_status(set_group_user_privileges(GID, UserId1, ?GROUP_PRIVILEGES, UserReqParams2))
    ).



