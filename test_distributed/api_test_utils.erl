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
-include_lib("entity_logic.hrl").
-include_lib("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([run_tests/2]).

run_tests(Config, ApiTestSpec) ->
    #api_test_spec{
        client_spec = ClientSpec,
        rest_spec = RestSpec,
        logic_spec = LogicSpec,
        data_spec = DataSpec
    } = ApiTestSpec,
    % Save config in process dictionary for cleaner code
    try
        % Run tests for REST and logic (if specified). If the code does not
        % crash, it means that all tests passed (return true). If throw:fail is
        % thrown from any test procedure, return false.
        run_rest_tests(Config, RestSpec, ClientSpec, DataSpec),
        run_logic_tests(Config, LogicSpec, ClientSpec, DataSpec),
        true
    catch
        % Tests failed, return false
        throw:fail ->
            false;
        % Unexpected error
        Type:Message ->
            ct:print("~p:run_tests failed with unexpected result - ~p:~p~n"
            "Stacktrace: ~p", [
                ?MODULE, Type, Message, erlang:get_stacktrace()
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
    Data = case Method of
        patch -> required_data_set(DataSpec);
        post -> required_data_set(DataSpec);
        put -> required_data_set(DataSpec);
        get -> undefined;
        delete -> undefined
    end,
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
    }),
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
            ErrorBin = <<"todo">>, % todo wywolac error transaltor
            verify_rest_result(Config, "bad data should fail", #{
                request => #{
                    method => Method,
                    path => Path,
                    body => Data,
                    auth => Client
                },
                expect => #{
                    code => 400,
                    body => ErrorBin
                }
            })
        end, BadDataSets),
    run_rest_tests(Config, RestSpec, ClientSpec#client_spec{correct = Tail}, DataSpec).


% If spec is undefined do not do anything -> success.
run_logic_tests(_, undefined, _, _) ->
    ok;
% All clients have been checked successfully -> success.
run_logic_tests(_, _, #client_spec{correct = [], unauthorized = [], forbidden = []}, _) ->
    ok;
% Check bad clients
run_logic_tests(Config, LogicSpec, #client_spec{correct = []} = ClientSpec, DataSpec) ->


    ok.

todo_run_logic_tests(Config, Operation, LogicSpec, #client_spec{correct = [_ | _]} = ClientSpec, DataSpec) ->
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
            Result = oz_test_utils:call_oz(Config, Module, Function, PreparedArgs)
        end, CorrectDataSets),
    todo_run_logic_tests(Config, Operation, LogicSpec, ClientSpec#client_spec{correct = Tail}, DataSpec);

todo_run_logic_tests(_, _, _, _, _) ->
    o.


prepare_logic_args(Args, Client, Data) ->
    lists:map(
        fun(Arg) ->
            case Arg of
                client -> client_to_logic_client(Client);
                data -> Data;
                _ -> Arg
            end
        end, Args).


client_to_logic_client(nobody) -> ?NOBODY;
client_to_logic_client(root) -> ?ROOT;
client_to_logic_client({user, UserId}) -> ?USER(UserId);
client_to_logic_client({provider, ProviderId, _, _}) -> ?PROVIDER(ProviderId).



verify_rest_result(Config, TestDesc, ArgsMap) ->
    Result = rest_test_utils:check_rest_call(Config, ArgsMap),
    case Result of
        true ->
            ok;
        {UnmetExp, {got, Got}, {expected, Expected}} ->
            #{request := #{
                method := Method,
                path := Path,
                auth := Client
            }} = ArgsMap,
            log_failed_rest_test(
                TestDesc, Method, Path, Client, UnmetExp, Got, Expected
            ),
            throw(fail)
    end.


log_failed_rest_test(TestDesc, Method, Path, Client, UnmetExp, Got, Expected) ->
    ct:print("API REST test failed: ~s~n"
    "Method: ~p~n"
    "Path: ~s~n"
    "Client: ~p~n"
    "Unmet expectation: ~p~n"
    "Got: ~p~n"
    "Expected: ~p", [
        TestDesc, Method, Path, client_to_readable(Client),
        UnmetExp, Got, Expected
    ]).








client_to_readable(nobody) ->
    "NOBODY";
client_to_readable({user, UserId}) ->
    str_utils:format_bin("USER#~s", [UserId]);
client_to_readable({provider, ProviderId, _, _}) ->
    str_utils:format_bin("PROVIDER#~s", [ProviderId]).


required_data_set(DataSpec) ->
    #data_spec{
        correct_values = CorrectValues,
        required = Required
    } = DataSpec,
    maps:from_list(lists:map(fun(Key) ->
        {Key, maps:get(Key, CorrectValues)}
    end, Required)).


correct_data_sets(DataSpec) ->
    #data_spec{
        correct_values = CorrectValues,
        required = Required,
        optional = Optional,
        at_least_one = AtLeastOne
    } = DataSpec,
    AllRequired = required_data_set(DataSpec),
    % TODO
    [AllRequired].

bad_data_sets(DataSpec) ->
    #data_spec{
        correct_values = CorrectValues,
        required = Required,
        bad_values = BadValues
    } = DataSpec,
    AllRequired = required_data_set(DataSpec),
    lists:map(
        fun(Key, Value, ErrorType) ->
            Data = AllRequired#{Key => Value},
            {Data, Key, ErrorType}
        end, BadValues).
