%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Utility functions used in API (logic + REST) tests.
%%% @end
%%%-------------------------------------------------------------------
-module(api_test_utils).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("api_test_utils.hrl").
-include("graph_sync/oz_graph_sync.hrl").
-include("http/rest.hrl").
-include("entity_logic.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("gui/include/gui_session.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("cluster_worker/include/graph_sync/graph_sync.hrl").

-define(NO_DATA, undefined).
-define(GS_RESP(Result), #gs_resp_graph{data = Result}).

% Arbitrary test env that will be available in teardown / verify functions
-type env() :: map().
-type env_setup_fun() :: fun(() -> env()).
-type env_teardown_fun() :: fun((env()) -> any()).
-type verify_fun() :: fun((ShouldSucceed :: boolean(), env(), entity_logic:data()) -> any()).
-export_type([env/0, env_setup_fun/0, env_teardown_fun/0, verify_fun/0]).

%% API
-export([
    default_env_setup/0, default_env_teardown/1,
    default_verify_fun/3,
    run_tests/2, run_tests/5
]).


default_env_setup() -> #{}.
default_env_teardown(_Env) -> ok.
default_verify_fun(_ShouldSucceed, _Env, _Data) -> ok.


% Runs possible combinations of tests on a given endpoint (logic + REST + GS)
run_tests(Config, ApiTestSpec) ->
    oz_test_utils:ensure_entity_graph_is_up_to_date(Config),
    run_tests(
        Config, ApiTestSpec,
        fun default_env_setup/0,
        fun default_env_teardown/1,
        fun default_verify_fun/3
    ).


run_tests(Config, ApiTestSpec, undefined, EnvTearDownFun, VerifyFun) ->
    run_tests(
        Config, ApiTestSpec, fun default_env_setup/0, EnvTearDownFun, VerifyFun
    );
run_tests(Config, ApiTestSpec, EnvSetUpFun, undefined, VerifyFun) ->
    run_tests(
        Config, ApiTestSpec, EnvSetUpFun, fun default_env_teardown/1, VerifyFun
    );
run_tests(Config, ApiTestSpec, EnvSetUpFun, EnvTearDownFun, undefined) ->
    run_tests(
        Config, ApiTestSpec, EnvSetUpFun, EnvTearDownFun, fun default_verify_fun/3
    );
run_tests(Config, ApiTestSpec, EnvSetUpFun, EnvTearDownFun, VerifyFun) ->
    #api_test_spec{
        client_spec = ClientSpec,
        rest_spec = RestSpec,
        logic_spec = LogicSpec,
        gs_spec = GsSpec,
        data_spec = DataSpec
    } = ApiTestSpec,
    try
        % Run tests for REST and logic (if specified). If the code does not
        % crash, it means that all tests passed (return true).
        % If throw:fail is thrown from any test procedure, return false.
        run_rest_tests(
            Config, RestSpec, ClientSpec, DataSpec,
            EnvSetUpFun, EnvTearDownFun, VerifyFun
        ),
        run_logic_tests(
            Config, LogicSpec, ClientSpec, DataSpec,
            EnvSetUpFun, EnvTearDownFun, VerifyFun
        ),
        run_gs_tests(
            Config, GsSpec, ClientSpec, DataSpec,
            EnvSetUpFun, EnvTearDownFun, VerifyFun
        ),
        true
    catch
        % Tests failed, return false
        % (failure details are printed before the throw)
        throw:fail ->
            ct:pal(io_lib_pretty:print(
                ApiTestSpec, fun get_api_test_spec_rec_def/2)
            ),
            false;
        % Unexpected error
        Type:Message ->
            ct:pal("~p:run_tests failed with unexpected result - ~p:~p~n"
            "Stacktrace: ~s", [
                ?MODULE, Type, Message,
                lager:pr_stacktrace(erlang:get_stacktrace())
            ]),
            ct:pal(io_lib_pretty:print(
                ApiTestSpec, fun get_api_test_spec_rec_def/2)
            ),
            false
    end.


%%%===================================================================
%%% Prepare args and run rest tests
%%%===================================================================

run_rest_tests(_, undefined, _, _, _, _, _) ->
    ok;
run_rest_tests(
    Config, RestSpec, ClientSpec, DataSpec,
    EnvSetUpFun, EnvTearDownFun, VerifyFun
) ->
    % Remove 'root' auth, which is not applicable in rest
    NewClientSpec = #client_spec{
        correct = ClientSpec#client_spec.correct -- [root],
        unauthorized = ClientSpec#client_spec.unauthorized -- [root],
        forbidden = ClientSpec#client_spec.forbidden -- [root]
    },

    run_test_combinations(
        Config, RestSpec, NewClientSpec, DataSpec,
        EnvSetUpFun, EnvTearDownFun, VerifyFun, fun run_rest_test/7
    ).


run_rest_test(Config, RestSpec, Client, Data, Description, Env, undefined) ->
    NewRestSpec = prepare_rest_spec(Config, RestSpec, Data, Env),
    ResolvedClient = resolve_auth(Client, Env),

    Result = rest_test_utils:check_rest_call(Config, #{
        request => #{
            method => NewRestSpec#rest_spec.method,
            path => NewRestSpec#rest_spec.path,
            headers => NewRestSpec#rest_spec.headers,
            body => Data,
            auth => ResolvedClient
        },
        expect => #{
            code => NewRestSpec#rest_spec.expected_code,
            body => NewRestSpec#rest_spec.expected_body,
            headers => NewRestSpec#rest_spec.expected_headers
        }
    }),

    log_rest_test_result(NewRestSpec, ResolvedClient, Data, Description, Result);

run_rest_test(Config, RestSpec, Client, Data, Description, Env, ExpError) ->
    {ExpCode, ExpHeaders, ExpBody} = error_to_rest_expectations(
        Config, ExpError
    ),
    run_rest_test(
        Config, RestSpec#rest_spec{
            expected_code = ExpCode,
            expected_headers = ExpHeaders,
            expected_body = ExpBody
        },
        Client, Data, Description, Env, undefined
    ).


% Convert placeholders in various spec fields into real data
prepare_rest_spec(_Config, RestSpec, Body, Env) ->
    #rest_spec{
        expected_code = ExpCode,
        expected_body = ExpBody,
        expected_headers = ExpHeaders
    } = RestSpec,

    RestSpec#rest_spec{
        path = prepare_rest_path(RestSpec#rest_spec.path, Env),
        expected_code = prepare_exp_result(ExpCode, Env, Body),
        expected_body = prepare_exp_result(ExpBody, Env, Body),
        expected_headers = prepare_exp_result(ExpHeaders, Env, Body)
    }.


% Converts placeholders in rest path into real data
prepare_rest_path(Path, _) when is_binary(Path) ->
    Path;
prepare_rest_path(Path, Env) ->
    [maps:get(Arg, Env, Arg) || Arg <- Path].


% Converts a predefined error to REST reply expectations
error_to_rest_expectations(Config, ErrorType) ->
    #rest_resp{
        code = ExpCode,
        headers = Headers,
        body = Body
    } = oz_test_utils:call_oz(
        Config, rest_translator, response, [#el_req{}, ErrorType]
    ),
    ExpHeaders = case Headers of
        #{} -> undefined;
        _ -> Headers
    end,
    ExpBody = case Body of
        {binary, <<"">>} -> undefined;
        _ ->
            %% Encoding to json transforms all atoms in response to binary strings
            EncodedBody = json_utils:encode(Body),
            json_utils:decode(EncodedBody)
    end,
    {ExpCode, ExpHeaders, ExpBody}.


% Do not log anything on success as to not garbage report
log_rest_test_result(_, _, _, _, true = _Result) ->
    ok;

% Do not log anything on unexpected error
% as it is already logged by rest_test_utils
% and throw fail to indicate failure
log_rest_test_result(_, _, _, _, false = _Result) ->
    throw(fail);

% On failure log comprehensive info
% and throw fail to indicate failure
log_rest_test_result(RestSpec, Client, Data, Description,
    {UnmetExp,
        {got, Got},
        {expected, Expected},
        {response, {Code, Headers, Body}}
    } = _Result
) ->
    ct:pal("API REST test failed: ~p~n"
    "Method: ~p~n"
    "Path: ~s~n"
    "Body: ~p~n"
    "Client: ~s~n"
    "Unmet expectation: ~p~n"
    "Expected: ~p~n"
    "Got:      ~p~n"
    "--------~n"
    "Full response: ~n"
    "   Code: ~p~n"
    "   Headers: ~p~n"
    "   Body: ~p", [
        Description,
        RestSpec#rest_spec.method,
        RestSpec#rest_spec.path,
        Data,
        aai:auth_to_printable(prepare_logic_auth(Client)),
        UnmetExp, Expected, Got, Code, Headers, Body
    ]),
    throw(fail).


%%%===================================================================
%%% Prepare args and run logic tests
%%%===================================================================

run_logic_tests(_, undefined, _, _, _, _, _) ->
    ok;
run_logic_tests(
    Config, LogicSpec, ClientSpec, DataSpec,
    EnvSetUpFun, EnvTearDownFun, VerifyFun
) ->
    run_test_combinations(
        Config, LogicSpec, ClientSpec, DataSpec,
        EnvSetUpFun, EnvTearDownFun, VerifyFun, fun run_logic_test/7
    ).


run_logic_test(Config, LogicSpec, Client, Data, Description, Env, undefined) ->
    LogicClient = prepare_logic_auth(resolve_auth(Client, Env)),
    NewLogicSpec = prepare_logic_spec(LogicSpec, LogicClient, Data, Env),
    Result = check_logic_call(Config, NewLogicSpec),
    log_logic_test_result(NewLogicSpec, LogicClient, Description, Result);

run_logic_test(Config, LogicSpec, Client, Data, DescFmt, Env, ExpError) ->
    NewLogicSpec = LogicSpec#logic_spec{
        expected_result = ?ERROR_REASON(ExpError)
    },
    run_logic_test(
        Config, NewLogicSpec, Client, Data, DescFmt, Env, undefined
    ).


% Converts auth used in tests into logic auth
prepare_logic_auth(nobody) ->
    ?NOBODY;
prepare_logic_auth(root) ->
    ?ROOT;
prepare_logic_auth({user, UserId}) ->
    ?USER(UserId);
prepare_logic_auth({user, UserId, _Token}) ->
    ?USER(UserId);
prepare_logic_auth({provider, ProviderId}) ->
    ?PROVIDER(ProviderId);
prepare_logic_auth({provider, ProviderId, _Token}) ->
    ?PROVIDER(ProviderId);
prepare_logic_auth({op_panel, ProviderId}) ->
    #auth{subject = ?SUB(?ONEPROVIDER, ?OP_PANEL, ProviderId)};
prepare_logic_auth({op_panel, ProviderId, _Token}) ->
    #auth{subject = ?SUB(?ONEPROVIDER, ?OP_PANEL, ProviderId)}.


% Convert placeholders in various spec fields into real data
prepare_logic_spec(LogicSpec, Client, Data, Env) ->
    #logic_spec{
        args = Args,
        expected_result = ExpResult
    } = LogicSpec,

    LogicSpec#logic_spec{
        args = prepare_logic_args(Args, Client, Data, Env),
        expected_result = prepare_exp_result(ExpResult, Env, Data)
    }.


% Converts placeholders in logic args into real data
prepare_logic_args(Args, Client, Data, Env) ->
    lists:map(
        fun
            (auth) -> Client;
            (data) -> Data;
            (Fun) when is_function(Fun, 0) -> Fun();
            (Arg) -> maps:get(Arg, Env, Arg)
        end, Args
    ).


% Make logic call and verify result
check_logic_call(Config, LogicSpec) ->
    #logic_spec{
        module = Module,
        function = Function,
        args = Args,
        expected_result = ExpResult
    } = LogicSpec,

    Result = oz_test_utils:call_oz(Config, Module, Function, Args),

    try
        verify_logic_result(Result, ExpResult)
    of
        false ->
            {result, Result};
        _ ->
            true
    catch
        Type:Message ->
            ct:pal(
                "Logic result verification function crashed - ~p:~p~n"
                "Stacktrace: ~s", [
                    Type, Message, lager:pr_stacktrace(erlang:get_stacktrace())
                ]),
            false
    end.


% Verifies if logic result is as expected
verify_logic_result(ok, ?OK_RES) ->
    true;
verify_logic_result({ok, Bin}, ?OK_BINARY) when is_binary(Bin) ->
    true;
verify_logic_result({ok, Value}, ?OK_BINARY(Value)) when is_binary(Value) ->
    true;
verify_logic_result({ok, Got}, ?OK_MAP(Expected)) when is_map(Got) ->
    Got =:= Expected;
verify_logic_result({ok, Got}, ?OK_MAP_CONTAINS(Expected)) when is_map(Got) ->
    rest_test_utils:contains_map(Got, Expected);
verify_logic_result({ok, GotList}, ?OK_LIST(ExpList)) ->
    lists:sort(ExpList) =:= lists:sort(GotList);
verify_logic_result({ok, GotList}, ?OK_LIST_CONTAINS(ExpList)) ->
        ExpList -- GotList =:= [];
verify_logic_result({ok, GotList}, ?OK_LIST_DOESNT_CONTAIN(ExpList)) ->
        GotList -- ExpList =:= GotList;
verify_logic_result({error, Error}, ?ERROR_REASON({error, Error})) ->
    true;
verify_logic_result({ok, Result}, ?OK_TERM(VerifyFun)) ->
    VerifyFun(Result);
verify_logic_result(_, _) ->
    false.


% Do not log anything on success as to not garbage report
log_logic_test_result(_, _, _, true = _Result) ->
    ok;

% Do not log anything on unexpected error
% as it is already logged by verify_logic_result
% and throw fail to indicate failure
log_logic_test_result(_, _, _, false = _Result) ->
    throw(fail);

% Displays an error when a logic test fails
log_logic_test_result(LogicSpec, Client, Description, {result, Result}) ->
    ct:pal("API logic test failed: ~p~n"
    "Module: ~p~n"
    "Function: ~s~n"
    "Args: ~p~n"
    "Client: ~s~n"
    "Expected: ~p~n"
    "Got:      ~p", [
        Description,
        LogicSpec#logic_spec.module,
        LogicSpec#logic_spec.function,
        LogicSpec#logic_spec.args,
        aai:auth_to_printable(Client),
        LogicSpec#logic_spec.expected_result,
        Result
    ]),
    throw(fail).


%%%===================================================================
%%% Prepare args and run gs tests
%%%===================================================================

run_gs_tests(_, undefined, _, _, _, _, _) ->
    ok;
run_gs_tests(
    Config, GsSpec, ClientSpec, DataSpec,
    EnvSetUpFun, EnvTearDownFun, VerifyFun
) ->
    % Remove 'root' auth, which is not applicable in graph sync
    NewClientSpec = #client_spec{
        correct = ClientSpec#client_spec.correct -- [root],
        unauthorized = ClientSpec#client_spec.unauthorized -- [root],
        forbidden = ClientSpec#client_spec.forbidden -- [root]
    },

    run_test_combinations(
        Config, GsSpec, NewClientSpec, DataSpec,
        EnvSetUpFun, EnvTearDownFun, VerifyFun, fun run_gs_test/7
    ).


% TODO rm clause after it will be possible to test gs nobody auth
run_gs_test(_Config, _GsSpec, nobody, _Data, _DescFmt, _Env, undefined) ->
    ok;
run_gs_test(Config, GsSpec, Client, Data, Description, Env, undefined) ->
    ResolvedClient = resolve_auth(Client, Env),
    GsClient = prepare_gs_client(Config, ResolvedClient),
    NewGsSpec = prepare_gs_spec(Config, GsSpec, ResolvedClient, Data, Env),
    Result = check_gs_call(NewGsSpec, GsClient, Data),
    log_gs_test_result(NewGsSpec, ResolvedClient, Data, Description, Result);

run_gs_test(Config, GsSpec, Client, Data, DescFmt, Env, ExpError) ->
    NewGsSpec = GsSpec#gs_spec{
        expected_result = ?ERROR_REASON(error_to_gs_expectations(
            Config, ExpError
        ))
    },
    run_gs_test(
        Config, NewGsSpec, Client, Data, DescFmt, Env, undefined
    ).


% Converts a predefined error to gs reply expectations
% To change atoms in error, it is required to
% fully jsonify error and dejsonify it
error_to_gs_expectations(Config, ErrorType) ->
    ErrorJson = json_utils:encode(oz_test_utils:call_oz(
        Config, errors, to_json, [ErrorType]
    )),
    oz_test_utils:call_oz(
        Config, errors, from_json, [json_utils:decode(ErrorJson)]
    ).


prepare_gs_client(Config, {user, UserId, _Token}) ->
    prepare_gs_client(Config, {user, UserId});
prepare_gs_client(Config, {user, UserId}) ->
    {ok, {_SessionId, CookieValue}} = oz_test_utils:log_in(Config, UserId),
    {ok, GuiToken} = oz_test_utils:request_gui_token(Config, CookieValue),
    prepare_gs_client(
        Config,
        ?SUB(user, UserId),
        {token, GuiToken},
        [{cacerts, oz_test_utils:gui_ca_certs(Config)}]
    );
prepare_gs_client(_Config, nobody) ->
    ok;
prepare_gs_client(Config, {provider, ProviderId}) ->
    Token = oz_test_utils:acquire_temporary_token(Config, ?SUB(?ONEPROVIDER, ProviderId)),
    prepare_gs_client(Config, {provider, ProviderId, Token});
prepare_gs_client(Config, {provider, ProviderId, Token}) ->
    prepare_gs_client(
        Config,
        ?SUB(?ONEPROVIDER, ProviderId),
        {token, Token},
        [{cacerts, oz_test_utils:gui_ca_certs(Config)}]
    );
prepare_gs_client(_Config, {op_panel, _ProviderId}) ->
    error(op_panel_graph_sync_not_supported);
prepare_gs_client(_Config, {op_panel, _ProviderId, _Token}) ->
    error(op_panel_graph_sync_not_supported).


prepare_gs_client(Config, ExpIdentity, Authorization, Opts) ->
    {ok, GsClient, #gs_resp_handshake{
        identity = ExpIdentity
    }} = gs_client:start_link(
        % @todo VFS-4520 Currently only provider endpoint is tested
        oz_test_utils:graph_sync_url(Config, provider),
        Authorization,
        oz_test_utils:get_gs_supported_proto_versions(Config),
        fun(_) -> ok end,
        Opts
    ),
    GsClient.


% Convert placeholders in various spec fields into real data
prepare_gs_spec(_Config, GsSpec, Client, Data, Env) ->
    GsSpec#gs_spec{
        gri = prepare_gri(GsSpec#gs_spec.gri, Env),
        auth_hint = prepare_auth_hint(GsSpec#gs_spec.auth_hint, Client, Env),
        expected_result = prepare_exp_result(
            GsSpec#gs_spec.expected_result, Env, Data
        )
    }.


% Convert placeholders in gri into real data
prepare_gri(#gri{aspect = {A, Id}} = Gri, Env) when is_atom(Id) ->
    prepare_gri(Gri#gri{aspect = {A, maps:get(Id, Env, Id)}}, Env);
prepare_gri(#gri{id = Id} = Gri, Env) when is_atom(Id) ->
    Gri#gri{id = maps:get(Id, Env, Id)};
prepare_gri(Gri, _Env) ->
    Gri.


% Convert placeholders in auth hint into real data
prepare_auth_hint(undefined, _, _Env) ->
    undefined;
prepare_auth_hint(?THROUGH_USER(auth), {user, UserId}, _Env) ->
    ?THROUGH_USER(UserId);
prepare_auth_hint(?THROUGH_USER(UserId), _, Env) when is_atom(UserId) ->
    ?THROUGH_USER(maps:get(UserId, Env, UserId));
prepare_auth_hint(?THROUGH_GROUP(GroupId), _, Env) when is_atom(GroupId) ->
    ?THROUGH_GROUP(maps:get(GroupId, Env, GroupId));
prepare_auth_hint(?THROUGH_SPACE(SpaceId), _, Env) when is_atom(SpaceId) ->
    ?THROUGH_SPACE(maps:get(SpaceId, Env, SpaceId));
prepare_auth_hint(?THROUGH_PROVIDER(ProvId), _, Env) when is_atom(ProvId) ->
    ?THROUGH_PROVIDER(maps:get(ProvId, Env, ProvId));
prepare_auth_hint(?THROUGH_HANDLE_SERVICE(HSId), _, Env) when is_atom(HSId) ->
    ?THROUGH_HANDLE_SERVICE(maps:get(HSId, Env, HSId));
prepare_auth_hint(?THROUGH_HANDLE(HandleId), _, Env) when is_atom(HandleId) ->
    ?THROUGH_HANDLE(maps:get(HandleId, Env, HandleId));
prepare_auth_hint(?AS_USER(auth), {user, UserId}, _Env) ->
    ?AS_USER(UserId);
prepare_auth_hint(?AS_USER(UserId), _, Env) when is_atom(UserId) ->
    ?AS_USER(maps:get(UserId, Env, UserId));
prepare_auth_hint(?AS_GROUP(GroupId), _, Env) when is_atom(GroupId) ->
    ?AS_GROUP(maps:get(GroupId, Env, GroupId));
prepare_auth_hint(Auth_Hint, _Client, _Env) ->
    Auth_Hint.


check_gs_call(GsSpec, GsClient, Data) ->
    #gs_spec{
        operation = Operation,
        gri = Gri,
        subscribe = Subscribe,
        auth_hint = AuthHint,
        expected_result = ExpResult
    } = GsSpec,

    Result = gs_client:graph_request(
        GsClient, Gri, Operation, Data, Subscribe, AuthHint
    ),

    try
        % make sure that every response with resource data format has a revision,
        % but do not check the revision against expected value (it is hard to
        % predict in tests)
        ResultWithoutRevision = case Result of
            {ok, #gs_resp_graph{data_format = resource, data = Map}} ->
                {Revision, NewMap} = maps:take(<<"revision">>, Map),
                ?assert(Revision >= 1),
                {ok, #gs_resp_graph{data_format = resource, data = NewMap}};
            Other ->
                Other
        end,
        verify_gs_result(ResultWithoutRevision, ExpResult)
    of
        false ->
            {result, Result};
        _ ->
            true
    catch
        Type:Message ->
            ct:pal(
                "Gs result verification function crashed - ~p:~p~n"
                "Stacktrace: ~s", [
                    Type, Message, lager:pr_stacktrace(erlang:get_stacktrace())
                ]),
            false
    end.


% Verifies if gs result is as expected
verify_gs_result({ok, ?GS_RESP(undefined)}, ?OK_RES) ->
    true;
verify_gs_result({error, Error}, ?ERROR_REASON({error, Error})) ->
    true;
verify_gs_result({ok, ?GS_RESP(Map)}, ?OK_MAP(ExpMap)) when is_map(Map) ->
    case maps:take(<<"gri">>, ExpMap) of
        {GriVerifyFun, ExpMap2} when is_function(GriVerifyFun, 1) ->
            {Gri, Map2} = maps:take(<<"gri">>, Map),
            GriVerifyFun(Gri),
            Map2 =:= ExpMap2;
        _ ->
            Map =:= ExpMap
    end;
verify_gs_result({ok, ?GS_RESP(Map)}, ?OK_MAP_CONTAINS(ExpMap)) when is_map(Map) ->
    case maps:take(<<"gri">>, ExpMap) of
        {GriVerifyFun, ExpMap2} when is_function(GriVerifyFun, 1) ->
            {Gri, Map2} = maps:take(<<"gri">>, Map),
            GriVerifyFun(Gri),
            rest_test_utils:contains_map(Map2, ExpMap2);
        _ ->
            rest_test_utils:contains_map(Map, ExpMap)
    end;
verify_gs_result({ok, ?GS_RESP(Result)}, ?OK_TERM(VerifyFun)) ->
    VerifyFun(Result);
verify_gs_result(_, _) ->
    false.


% Do not log anything on success as to not garbage report
log_gs_test_result(_, _, _, _, true = _Result) ->
    ok;

% Do not log anything on unexpected error
% as it is already logged by verify_logic_result
% and throw fail to indicate failure
log_gs_test_result(_, _, _, _, false = _Result) ->
    throw(fail);

% Displays an error when a gs test fails
log_gs_test_result(GsSpec, Client, Data, Description, {result, Result}) ->
    ct:pal("API GS test failed: ~s~n"
    "Client: ~p~n"
    "Operation: ~p~n"
    "Gri: ~p~n"
    "Data: ~p~n"
    "Subscribe: ~p~n"
    "Auth hint: ~p~n"
    "Expected: ~p~n"
    "Got:      ~p", [
        Description,
        aai:auth_to_printable(prepare_logic_auth(Client)),
        GsSpec#gs_spec.operation,
        GsSpec#gs_spec.gri,
        Data,
        GsSpec#gs_spec.subscribe,
        GsSpec#gs_spec.auth_hint,
        GsSpec#gs_spec.expected_result,
        Result
    ]),
    throw(fail).


%%%===================================================================
%%% Prepare and run test combinations
%%%===================================================================

% Using clients, data and helper functions run various combinations of them
% (e.g. unauthorized/forbidden/correct clients with correct/malformed data)
% with accordance to given spec
run_test_combinations(
    Config, Spec, ClientSpec, DataSpec, EnvSetUpFun,
    EnvTearDownFun, VerifyFun, RunTestFun
) ->
    CorrectClients = ClientSpec#client_spec.correct,
    UnauthorizedClients = ClientSpec#client_spec.unauthorized,
    ForbiddenClients = ClientSpec#client_spec.forbidden,

    % Get list of various combinations of data regardless of operation.
    % In case of operations that do not require nor expect any data
    % (DataSpec == undefined) such as get or delete [?NO_DATA] is prepared
    RequiredDataSets = required_data_sets(DataSpec),
    CorrectDataSets = correct_data_sets(DataSpec),
    BadDataSets = bad_data_sets(DataSpec),

    % assert that unauthorized and forbidden clients with required data sets,
    % along with correct clients with bad/malformed data sets,
    % fails with an appropriate error
    Environment = EnvSetUpFun(),
    lists:foreach(
        fun({Clients, DataSets, DescFmt, Error}) ->
            lists:foreach(
                fun(Client) ->
                    PreparedClient = prepare_auth(Client, Environment, Config),
                    lists:foreach(
                        fun
                        % get and delete operations cannot
                        % be run with malformed data
                            (?NO_DATA) when Error == undefined ->
                                ok;
                            (DataSet) ->
                                {Data, Description, ExpError} = prepare_error(
                                    DataSet, DescFmt, Error
                                ),
                                PreparedData = prepare_data(Data, Environment),
                                RunTestFun(
                                    Config, Spec, PreparedClient, PreparedData,
                                    Description, Environment, ExpError
                                ),
                                VerifyFun(false, Environment, PreparedData)
                        end, DataSets
                    )
                end, Clients
            )
        end, [
            {UnauthorizedClients, RequiredDataSets,
                "unauthorized client should fail", ?ERROR_UNAUTHORIZED},
            {ForbiddenClients, RequiredDataSets,
                "forbidden client should fail", ?ERROR_FORBIDDEN},
            {CorrectClients, BadDataSets,
                "bad data should fail: ~s => ~p", undefined}
        ]
    ),
    EnvTearDownFun(Environment),

    % Assert correct clients with correct data sets succeeds
    Description = "correct data should succeed",
    lists:foreach(
        fun(Client) ->
            lists:foreach(
                fun(DataSet) ->

                    Env = EnvSetUpFun(),
                    PreparedData = prepare_data(DataSet, Env),
                    RunTestFun(
                        Config, Spec, prepare_auth(Client, Env, Config),
                        PreparedData, Description, Env, undefined
                    ),
                    VerifyFun(true, Env, PreparedData),
                    EnvTearDownFun(Env)

                end, CorrectDataSets
            )
        end, CorrectClients
    ).


% Prepare data, description and error code for test cases
% where data error is expected
prepare_error({Data, BadKey, ExpError}, DescFmt, _) ->
    Description = str_utils:format(DescFmt, [BadKey, maps:get(BadKey, Data)]),
    {Data, Description, ExpError};
prepare_error(Data, Description, ExpError) ->
    {Data, Description, ExpError}.


% Converts placeholders in auth into real data
prepare_auth({user, User}, Env, _Config) when is_atom(User) ->
    {user, maps:get(User, Env, User)};
prepare_auth({user, User, Token}, Env, _Config) when is_atom(User) ->
    {user, maps:get(User, Env, User), Token};

prepare_auth({provider, Provider}, Env, _Config) when is_atom(Provider) ->
    {provider, maps:get(Provider, Env, Provider)};
prepare_auth({provider, Provider, Token}, Env, _Config) when is_atom(Provider) orelse is_atom(Token) ->
    {provider, maps:get(Provider, Env, Provider), maps:get(Token, Env, Token)};

prepare_auth({op_panel, Provider}, Env, _Config) when is_atom(Provider) ->
    {op_panel, maps:get(Provider, Env, Provider)};
prepare_auth({op_panel, Provider, Token}, Env, _Config) when is_atom(Provider) orelse is_atom(Token) ->
    {op_panel, maps:get(Provider, Env, Provider), maps:get(Token, Env, Token)};

prepare_auth({admin, Privs}, _Env, Config) ->
    {ok, Admin} = oz_test_utils:create_user(Config),
    oz_test_utils:user_set_oz_privileges(Config, Admin, Privs, []),
    {user, Admin};

prepare_auth(Client, Env, _Config) when is_atom(Client) ->
    maps:get(Client, Env, Client);
prepare_auth(Client, _Env, _Config) ->
    Client.


% Convert placeholders in data into real data
prepare_data(undefined, _Env) ->
    undefined;
prepare_data(Fun, _Env) when is_function(Fun, 0) ->
    Fun();
prepare_data(Fun, Env) when is_function(Fun, 1) ->
    Fun(Env);
prepare_data(List, Env) when is_list(List) ->
    [prepare_data(Val, Env) || Val <- List];
prepare_data(Map, Env) when is_map(Map) ->
    maps:map(fun(_, Val) -> prepare_data(Val, Env) end, Map);
prepare_data(Val, Env) ->
    maps:get(Val, Env, Val).


% Generates all combinations of "required" and "at_least_one" data
required_data_sets(undefined) ->
    [?NO_DATA];
required_data_sets(DataSpec) ->
    #data_spec{
        required = Required,
        at_least_one = AtLeastOne
    } = DataSpec,

    AtLeastOneWithValues = lists:flatten(lists:map(
        fun(Key) ->
            [#{Key => Val} || Val <- get_correct_value(Key, DataSpec)]
        end, AtLeastOne)
    ),
    RequiredWithValues = lists:map(
        fun(Key) ->
            [#{Key => Val} || Val <- get_correct_value(Key, DataSpec)]
        end, Required
    ),
    RequiredCombinations = lists:foldl(
        fun(ValuesForKey, Acc) ->
            [maps:merge(A, B) || A <- ValuesForKey, B <- Acc]
        end, [#{}], RequiredWithValues
    ),
    RequiredWithOne = lists:flatten(lists:map(
        fun(ReqMap) ->
            [maps:merge(AtLeastOneMap, ReqMap) || AtLeastOneMap <- AtLeastOneWithValues]
        end, RequiredCombinations
    )),
    AllAtLeastOneMap = lists:foldl(fun maps:merge/2, #{}, AtLeastOneWithValues),
    RequiredWithAll = lists:map(
        fun(ReqMap) ->
            maps:merge(AllAtLeastOneMap, ReqMap)
        end, RequiredCombinations
    ),
    case AtLeastOne of
        [] -> RequiredCombinations;
        [_] -> RequiredWithOne;
        _ -> RequiredWithAll ++ RequiredWithOne
    end.


% Data sets wih required params and one or all optional params
% (e.g. returns 5 data sets for 4 optional params).
optional_data_sets(undefined, _) ->
    [?NO_DATA];
optional_data_sets(DataSpec, RequiredWithAll) ->
    #data_spec{
        optional = Optional
    } = DataSpec,

    OptionalWithValues = lists:flatten(lists:map(
        fun(Key) ->
            [#{Key => Val} || Val <- get_correct_value(Key, DataSpec)]
        end, Optional)
    ),
    RequiredWithOneOptional = lists:map(
        fun(OneOptionalMap) ->
            maps:merge(OneOptionalMap, RequiredWithAll)
        end, OptionalWithValues
    ),
    AllOptionalsMap = lists:foldl(fun maps:merge/2, #{}, OptionalWithValues),
    RequiredWithAllOptional = maps:merge(RequiredWithAll, AllOptionalsMap),

    case Optional of
        [] -> [];
        [_] -> RequiredWithOneOptional;
        _ -> [RequiredWithAllOptional | RequiredWithOneOptional]
    end.


% Returns all data sets that are correct
correct_data_sets(undefined) ->
    [?NO_DATA];
correct_data_sets(DataSpec) ->
    RequiredDataSets = required_data_sets(DataSpec),
    AllRequired = case RequiredDataSets of
        [] -> #{};
        _ -> hd(RequiredDataSets)
    end,
    OptionalDataSets = optional_data_sets(DataSpec, AllRequired),
    RequiredDataSets ++ OptionalDataSets.


% Generates all combinations of bad data sets by adding wrong values to
% correct data sets.
bad_data_sets(undefined) ->
    [?NO_DATA];
bad_data_sets(DataSpec) ->
    #data_spec{
        required = Required,
        at_least_one = AtLeastOne,
        optional = Optional,
        bad_values = BadValues
    } = DataSpec,
    AllCorrect = maps:from_list(lists:map(fun(Key) ->
        {Key, hd(get_correct_value(Key, DataSpec))}
    end, Required ++ AtLeastOne ++ Optional)),
    lists:map(
        fun({Key, Value, ErrorType}) ->
            Data = AllCorrect#{Key => Value},
            {Data, Key, ErrorType}
        end, BadValues).


% Converts correct value spec into a value
get_correct_value(Key, #data_spec{correct_values = CorrectValues}) ->
    case maps:get(Key, CorrectValues) of
        Fun when is_function(Fun, 0) ->
            Fun();
        Value ->
            Value
    end.


%%%===================================================================
%%% General use private functions
%%%===================================================================

% Returns information about api test spec record, such as fields,
% required to for example pretty print it
get_api_test_spec_rec_def(api_test_spec, N) ->
    case record_info(size, api_test_spec) - 1 of
        N ->
            record_info(fields, api_test_spec);
        _ ->
            no
    end;
get_api_test_spec_rec_def(client_spec, N) ->
    case record_info(size, client_spec) - 1 of
        N ->
            record_info(fields, client_spec);
        _ ->
            no
    end;
get_api_test_spec_rec_def(data_spec, N) ->
    case record_info(size, data_spec) - 1 of
        N ->
            record_info(fields, data_spec);
        _ ->
            no
    end;
get_api_test_spec_rec_def(rest_spec, N) ->
    case record_info(size, rest_spec) - 1 of
        N ->
            record_info(fields, rest_spec);
        _ ->
            no
    end;
get_api_test_spec_rec_def(logic_spec, N) ->
    case record_info(size, logic_spec) - 1 of
        N ->
            record_info(fields, logic_spec);
        _ ->
            no
    end;
get_api_test_spec_rec_def(gs_spec, N) ->
    case record_info(size, gs_spec) - 1 of
        N ->
            record_info(fields, gs_spec);
        _ ->
            no
    end;
get_api_test_spec_rec_def(gri, N) ->
    case record_info(size, gri) - 1 of
        N ->
            record_info(fields, gri);
        _ ->
            no
    end;
get_api_test_spec_rec_def(_, _) ->
    no.


% In case of OK_ENV(PrepareFun) expectation evaluate PrepareFun
prepare_exp_result(?OK_ENV(PrepareFun), Env, Data) ->
    PrepareFun(Env, Data);
prepare_exp_result(Expectation, _Env, _Data) ->
    Expectation.


resolve_auth(nobody, _Env) ->
    nobody;
resolve_auth(root, _Env) ->
    root;
resolve_auth({user, UserId}, _Env) ->
    {user, UserId};
resolve_auth({user, UserId, Token}, _Env) ->
    {user, UserId, Token};
resolve_auth({provider, ProviderId}, _Env) ->
    {provider, ProviderId};
resolve_auth({provider, ProviderId, Token}, _Env) ->
    {provider, ProviderId, Token};
resolve_auth({op_panel, ProviderId}, _Env) ->
    {op_panel, ProviderId};
resolve_auth({op_panel, ProviderId, Token}, _Env) ->
    {op_panel, ProviderId, Token};
resolve_auth(Arg, Env) ->
    maps:get(Arg, Env).
