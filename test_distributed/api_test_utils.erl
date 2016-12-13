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
-include_lib("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

-include("api_test_utils.hrl").

%% API
-export([run_tests/2]).

run_tests(Config, ApiTestSpec) ->
    #api_test_spec{
        operation = Operation,
        client_spec = ClientSpec,
        rest_spec = RestSpec,
        logic_spec = LogicSpec,
        data_spec = DataSpec
    } = ApiTestSpec,
    % Save config in process dictionary for cleaner code
    put(config, Config),
    try
        % Run tests for REST and logic (if specified). If the code does not
        % crash, it means that all tests passed (return true). If throw:fail is
        % thrown from any test procedure, return false.
        case Operation of
            create ->
                create_tests(ClientSpec, RestSpec, DataSpec),
                create_tests(ClientSpec, LogicSpec, DataSpec);
            get ->
                get_tests(ClientSpec, RestSpec),
                get_tests(ClientSpec, LogicSpec);
            update ->
                update_tests(ClientSpec, RestSpec, DataSpec),
                update_tests(ClientSpec, LogicSpec, DataSpec);
            delete ->
                delete_tests(ClientSpec, RestSpec),
                delete_tests(ClientSpec, LogicSpec)
        end,
        true
    catch
        throw:fail ->
            false;
        Type:Message ->
            ct:print("~p:run_tests failed with unexpected result - ~p:~p~nStacktrace: ~p", [
                ?MODULE, Type, Message, erlang:get_stacktrace()
            ]),
            false
    end.

% If REST spec is undefined do not do anything.
create_tests(_, undefined, _) ->
    ok;
% ROOT client is not checked in REST calls (it's a virtual, internal client in OZ).
create_tests(#client_spec{correct = [root | Tail]} = ClientSpec, #rest_spec{} = RestSpec, DataSpec) ->
    create_tests(ClientSpec#client_spec{correct = Tail}, RestSpec, DataSpec);
create_tests(#client_spec{forbidden = [root | Tail]} = ClientSpec, #rest_spec{} = RestSpec, DataSpec) ->
    create_tests(ClientSpec#client_spec{forbidden = Tail}, RestSpec, DataSpec);
create_tests(#client_spec{unauthorized = [root | Tail]} = ClientSpec, #rest_spec{} = RestSpec, DataSpec) ->
    create_tests(ClientSpec#client_spec{unauthorized = Tail}, RestSpec, DataSpec);
create_tests(#client_spec{correct = [_ | _]} = ClientSpec, #rest_spec{} = RestSpec, DataSpec) ->
    #client_spec{correct = [First | Tail]} = ClientSpec,
    #rest_spec{
        method = Method,
        path = Path,
        expected_code = ExpCode,
        expected_headers = ExpHeaders,
        expected_body = ExpBody
    } = RestSpec,

    Result = rest_test_utils:check_rest_call(#{
        request => #{
            method => delete,
            path => Path,
            auth => First
        },
        expect => #{
            code => ExpCode,
            headers => ExpHeaders,
            body => ExpBody
        }
    }),

    ok;

create_tests(#client_spec{}, _, _) ->
    % All clients have been checked successfully, continue.
    ok.