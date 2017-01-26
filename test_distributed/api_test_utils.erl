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
-include("rest.hrl").
-include("entity_logic.hrl").
-include("errors.hrl").
-include_lib("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([run_tests/2]).

% Runs all possible combinations of tests on a given endpoint (logic + REST)
run_tests(Config, ApiTestSpec) ->
    #api_test_spec{
        client_spec = ClientSpec,
        rest_spec = RestSpec,
        logic_spec = LogicSpec,
        data_spec = DataSpec
    } = ApiTestSpec,
    try
        % Run tests for REST and logic (if specified). If the code does not
        % crash, it means that all tests passed (return true). If throw:fail is
        % thrown from any test procedure, return false.
        run_rest_tests(Config, RestSpec, ClientSpec, DataSpec),
        run_logic_tests(Config, LogicSpec, ClientSpec, DataSpec),
        true
    catch
        % Tests failed, return false (failure details are printed before the throw)
        throw:fail ->
            false;
        % Unexpected error
        Type:Message ->
            ct:print("~p:run_tests failed with unexpected result - ~p:~p~n"
            "Stacktrace: ~s", [
                ?MODULE, Type, Message, lager:pr_stacktrace(erlang:get_stacktrace())
            ]),
            false
    end.

% If spec is undefined do not do anything -> success.
run_rest_tests(_, undefined, _, _) ->
    ok;
% All clients have been checked successfully -> success.
run_rest_tests(_, _, #client_spec{correct = [], unauthorized = [], forbidden = []}, _) ->
    ok;
% ROOT client is not checked in REST calls (it's a virtual, internal client in OZ).
run_rest_tests(Config, RestSpec, #client_spec{correct = [root | Tail]} = ClientSpec, DataSpec) ->
    run_rest_tests(Config, RestSpec, ClientSpec#client_spec{correct = Tail}, DataSpec);
run_rest_tests(Config, RestSpec, #client_spec{unauthorized = [root | Tail]} = ClientSpec, DataSpec) ->
    run_rest_tests(Config, RestSpec, ClientSpec#client_spec{unauthorized = Tail}, DataSpec);
run_rest_tests(Config, RestSpec, #client_spec{forbidden = [root | Tail]} = ClientSpec, DataSpec) ->
    run_rest_tests(Config, RestSpec, ClientSpec#client_spec{forbidden = Tail}, DataSpec);
% Check bad clients
run_rest_tests(Config, RestSpec, #client_spec{correct = []} = ClientSpec, DataSpec) ->
    #rest_spec{method = Method, path = Path} = RestSpec,
    {TestDesc, Client, ExpCode, NewClientSpec} = case ClientSpec of
        #client_spec{unauthorized = [Head | Tail]} ->
            {"unauthorized client should fail", Head, 401, ClientSpec#client_spec{
                unauthorized = Tail
            }};
        #client_spec{forbidden = [Head | Tail]} ->
            {"forbidden client should fail", Head, 403, ClientSpec#client_spec{
                forbidden = Tail
            }}
    end,
    DataSets = case Method of
        patch -> required_data_sets(DataSpec);
        post -> required_data_sets(DataSpec);
        put -> required_data_sets(DataSpec);
        get -> [undefined];
        delete -> [undefined]
    end,
    lists:foreach(
        fun(Data) ->
            verify_rest_result(Config, TestDesc, #{
                request => #{
                    method => Method,
                    path => Path,
                    body => Data,
                    auth => Client
                },
                expect => #{
                    code => ExpCode
                }
            })
        end, DataSets),
    run_rest_tests(Config, RestSpec, NewClientSpec, DataSpec);
% Check correct client concerning endpoints without data sent (get, delete).
run_rest_tests(Config, #rest_spec{method = Method} = RestSpec, ClientSpec, DataSpec)
    when Method =:= get; Method =:= delete ->
    #client_spec{correct = [Client | Tail]} = ClientSpec,
    #rest_spec{
        method = Method,
        path = Path,
        expected_code = ExpCode,
        expected_headers = ExpHeaders,
        expected_body = ExpBody
    } = RestSpec,
    verify_rest_result(Config, "correct client should succeed", #{
        request => #{
            method => Method,
            path => Path,
            auth => Client
        },
        expect => #{
            code => ExpCode,
            headers => ExpHeaders,
            body => ExpBody
        }
    }),
    run_rest_tests(Config, RestSpec, ClientSpec#client_spec{correct = Tail}, DataSpec);
% Check correct client concerning endpoints with data sent (patch, post, put).
run_rest_tests(Config, #rest_spec{method = Method} = RestSpec, ClientSpec, DataSpec)
    when Method =:= patch; Method =:= post; Method =:= put ->
    #client_spec{correct = [Client | Tail]} = ClientSpec,
    #rest_spec{
        method = Method,
        path = Path,
        expected_code = ExpCode,
        expected_headers = ExpHeaders,
        expected_body = ExpBody
    } = RestSpec,
    CorrectDataSets = correct_data_sets(DataSpec),
    lists:foreach(
        fun(Data) ->
            verify_rest_result(Config, "correct data should succeed", #{
                request => #{
                    method => Method,
                    path => Path,
                    body => Data,
                    auth => Client
                },
                expect => #{
                    code => ExpCode,
                    headers => ExpHeaders,
                    body => ExpBody
                }
            })
        end, CorrectDataSets),
    BadDataSets = bad_data_sets(DataSpec),
    lists:foreach(
        fun({Data, BadKey, ErrorType}) ->
            {ExpCode2, ExpHeaders2, ExpBody2} = error_to_rest_expectations(Config, ErrorType),
            verify_rest_result(Config,
                {"bad data should fail: ~s => ~p", [BadKey, maps:get(BadKey, Data)]}, #{
                    request => #{
                        method => Method,
                        path => Path,
                        body => Data,
                        auth => Client
                    },
                    expect => #{
                        code => ExpCode2,
                        headers => ExpHeaders2,
                        body => ExpBody2
                    }
                })
        end, BadDataSets),
    run_rest_tests(Config, RestSpec, ClientSpec#client_spec{correct = Tail}, DataSpec).


% Verifies if result returned from REST is correct
verify_rest_result(Config, TestDesc, ArgsMap) ->
    Result = rest_test_utils:check_rest_call(Config, ArgsMap),
    case Result of
        true ->
            ok;
        {UnmetExp, {got, Got}, {expected, Expected}, {response, Response}} ->
            #{request := #{
                method := Method,
                path := Path,
                auth := Client
            }} = ArgsMap,
            log_failed_rest_test(
                TestDesc, Method, Path, Client, UnmetExp, Got, Expected, Response
            ),
            throw(fail)
    end.


% Converts a predefined error to REST reply expectations
error_to_rest_expectations(Config, ErrorType) ->
    #rest_resp{
        code = ExpCode,
        headers = Headers,
        body = Body
    } = oz_test_utils:call_oz(Config, error_rest_translator, response, [ErrorType]),
    ExpHeaders = case Headers of
        #{} -> undefined;
        _ -> Headers
    end,
    ExpBody = case Body of
        <<"">> -> undefined;
        _ -> Body
    end,
    {ExpCode, ExpHeaders, ExpBody}.


% If spec is undefined do not do anything -> success.
run_logic_tests(_, undefined, _, _) ->
    ok;
% All clients have been checked successfully -> success.
run_logic_tests(_, _, #client_spec{correct = [], unauthorized = [], forbidden = []}, _) ->
    ok;
% Check bad clients
run_logic_tests(Config, LogicSpec, #client_spec{correct = []} = ClientSpec, DataSpec) ->
    #logic_spec{
        operation = Operation,
        module = Module,
        function = Function,
        args = Args
    } = LogicSpec,
    {TestDesc, Client, ExpResult, NewClientSpec} = case ClientSpec of
        #client_spec{unauthorized = [Head | Tail]} ->
            {"unauthorized client should fail", Head, ?ERROR_UNAUTHORIZED,
                ClientSpec#client_spec{unauthorized = Tail}
            };
        #client_spec{forbidden = [Head | Tail]} ->
            {"forbidden client should fail", Head, ?ERROR_FORBIDDEN,
                ClientSpec#client_spec{forbidden = Tail}
            }
    end,
    DataSets = case Operation of
        create -> required_data_sets(DataSpec);
        update -> required_data_sets(DataSpec);
        get -> [undefined];
        delete -> [undefined]
    end,
    lists:foreach(
        fun(Data) ->
            PreparedArgs = prepare_logic_args(Args, Client, Data),
            Result = oz_test_utils:call_oz(Config, Module, Function, PreparedArgs),
            ExpectedError = ?ERROR_REASON(ExpResult),
            case verify_logic_result(Result, ExpectedError) of
                true ->
                    ok;
                false ->
                    log_failed_logic_test(
                        TestDesc, Module, Function, Args, Client, ExpectedError, Result
                    ),
                    throw(fail)
            end
        end, DataSets),
    run_logic_tests(Config, LogicSpec, NewClientSpec, DataSpec);
% Check correct clients
run_logic_tests(Config, #logic_spec{operation = Operation} = LogicSpec, ClientSpec, DataSpec)
    when Operation =:= create; Operation =:= update ->
    #client_spec{correct = [Client | Tail]} = ClientSpec,
    #logic_spec{
        module = Module,
        function = Function,
        args = Args,
        expected_result = ExpectedResult
    } = LogicSpec,
    CorrectDataSets = correct_data_sets(DataSpec),
    lists:foreach(
        fun(Data) ->
            PreparedArgs = prepare_logic_args(Args, Client, Data),
            Result = oz_test_utils:call_oz(Config, Module, Function, PreparedArgs),
            case verify_logic_result(Result, ExpectedResult) of
                true ->
                    ok;
                false ->
                    log_failed_logic_test("correct data should succeed",
                        Module, Function, Args, Client, ExpectedResult, Result
                    ),
                    throw(fail)
            end
        end, CorrectDataSets),
    BadDataSets = bad_data_sets(DataSpec),
    lists:foreach(
        fun({Data, BadKey, ErrorType}) ->
            PreparedArgs = prepare_logic_args(Args, Client, Data),
            Result = oz_test_utils:call_oz(Config, Module, Function, PreparedArgs),
            ExpectedError = ?ERROR_REASON(ErrorType),
            case verify_logic_result(Result, ExpectedError) of
                true ->
                    ok;
                false ->
                    log_failed_logic_test(
                        {"bad data should fail: ~s => ~p", [BadKey, maps:get(BadKey, Data)]},
                        Module, Function, Args, Client, ExpectedError, Result
                    ),
                    throw(fail)
            end
        end, BadDataSets),
    run_logic_tests(Config, LogicSpec, ClientSpec#client_spec{correct = Tail}, DataSpec);
run_logic_tests(Config, #logic_spec{operation = Operation} = LogicSpec, ClientSpec, DataSpec)
    when Operation =:= get; Operation =:= delete ->
    #client_spec{correct = [Client | Tail]} = ClientSpec,
    #logic_spec{
        module = Module,
        function = Function,
        args = Args,
        expected_result = ExpectedResult
    } = LogicSpec,
    PreparedArgs = prepare_logic_args(Args, Client, []),
    Result = oz_test_utils:call_oz(Config, Module, Function, PreparedArgs),
    case verify_logic_result(Result, ExpectedResult) of
        true ->
            ok;
        false ->
            log_failed_logic_test("correct client should succeed",
                Module, Function, Args, Client, ExpectedResult, Result
            ),
            throw(fail)
    end,
    run_logic_tests(Config, LogicSpec, ClientSpec#client_spec{correct = Tail}, DataSpec).


% Converts placeholders in logic args into real data
prepare_logic_args(Args, Client, Data) ->
    lists:map(
        fun(Arg) ->
            case Arg of
                client -> client_to_logic_client(Client);
                data -> Data;
                _ -> Arg
            end
        end, Args).


% Converts client used in tests into logic client
client_to_logic_client(nobody) -> ?NOBODY;
client_to_logic_client(root) -> ?ROOT;
client_to_logic_client({user, UserId}) -> ?USER(UserId);
client_to_logic_client({provider, ProviderId, _, _}) -> ?PROVIDER(ProviderId).


% Verifies if logic result is as expected
verify_logic_result(ok, ?OK) ->
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
    try
        VerifyFun(Result)
    catch
        Type:Message ->
            ct:print("Logic result verification function crashed - ~p:~p~n"
            "Stacktrace: ~s", [
                Type, Message, lager:pr_stacktrace(erlang:get_stacktrace())
            ]),
            false
    end;
verify_logic_result(_, _) ->
    false.


% Displays an error when a logic test fails
log_failed_logic_test(TestDesc, Module, Function, Args, Client, Expected, Got) ->
    TestDescString = case TestDesc of
        {Format, Args} ->
            str_utils:format(Format, Args);
        Other ->
            Other
    end,
    ct:print("API logic test failed: ~p~n"
    "Module: ~p~n"
    "Function: ~s~n"
    "Args: ~p~n"
    "Client: ~s~n"
    "Expected: ~p~n"
    "Got: ~p", [
        TestDescString, Module, Function, Args,
        n_entity_logic:client_to_string(client_to_logic_client(Client)),
        Expected, Got
    ]).


% Displays an error when a REST test fails
log_failed_rest_test(TestDesc, Method, Path, Client, UnmetExp, Got, Expected, Response) ->
    {Code, Headers, Body} = Response,
    TestDescString = case TestDesc of
        {Format, Args} ->
            str_utils:format(Format, Args);
        Other ->
            Other
    end,
    ct:print("API REST test failed: ~p~n"
    "Method: ~p~n"
    "Path: ~s~n"
    "Client: ~s~n"
    "Unmet expectation: ~p~n"
    "Got: ~p~n"
    "Expected: ~p~n"
    "--------~n"
    "Full response: ~n"
    "   Code: ~p~n"
    "   Headers: ~p~n"
    "   Body: ~p", [
        TestDescString, Method, Path,
        n_entity_logic:client_to_string(client_to_logic_client(Client)),
        UnmetExp, Got, Expected, Code, Headers, Body
    ]).


% Generates all combinations of "required" and "at_least_one" data
required_data_sets(DataSpec) ->
    #data_spec{
        required = Required,
        at_least_one = AtLeastOne
    } = DataSpec,
    RequiredWithValues = lists:map(fun(Key) ->
        {Key, get_correct_value(Key, DataSpec)}
    end, Required),
    AtLeastOneWithValues = lists:map(fun(Key) ->
        {Key, get_correct_value(Key, DataSpec)}
    end, AtLeastOne),
    RequiredWithOne = lists:map(
        fun({Key, Value}) ->
            maps:from_list([{Key, Value} | RequiredWithValues])
        end, AtLeastOneWithValues),
    RequiredWithAll = maps:from_list(
        AtLeastOneWithValues ++ RequiredWithValues
    ),
    [RequiredWithAll | RequiredWithOne].


% Data sets wih required params and one or all optional params
% (e.g. returns 5 data sets for 4 optional params).
optional_data_sets(DataSpec) ->
    #data_spec{
        optional = Optional
    } = DataSpec,
    WithValues = lists:map(fun(Key) ->
        {Key, get_correct_value(Key, DataSpec)}
    end, Optional),
    [RequiredWithAll | _] = required_data_sets(DataSpec),
    RequiredWithOneOptional = lists:map(
        fun({Key, Value}) ->
            RequiredWithAll#{Key => Value}
        end, WithValues),
    RequiredWithAllOptional = maps:merge(
        RequiredWithAll, maps:from_list(WithValues)
    ),
    [RequiredWithAllOptional | RequiredWithOneOptional].

% Returns all data sets that are correct
correct_data_sets(DataSpec) ->
    RequiredDataSets = required_data_sets(DataSpec),
    OptionalDataSets = optional_data_sets(DataSpec),
    RequiredDataSets ++ OptionalDataSets.


% Generates all combinations of bad data sets by adding wrong values to
% correct data sets.
bad_data_sets(DataSpec) ->
    #data_spec{
        required = Required,
        at_least_one = AtLeastOne,
        optional = Optional,
        bad_values = BadValues
    } = DataSpec,
    AllCorrect = maps:from_list(lists:map(fun(Key) ->
        {Key, get_correct_value(Key, DataSpec)}
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
