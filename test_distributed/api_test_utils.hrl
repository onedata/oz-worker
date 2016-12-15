%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% Definitions of macros and records used in API (logic + REST) tests.
%%% @end
%%%-------------------------------------------------------------------
-author("Lukasz Opiola").

-ifndef(API_TEST_UTILS_HRL).
-define(API_TEST_UTILS_HRL, 1).

-type client() :: nobody | root | {user, UserId :: binary()} |
{provider, ProviderId :: binary(), KeyFile :: string(), CertFile :: string()}.

-type data_error() :: bad | empty | bad_token | bad_token_type |
id_not_found | id_occupied | relation_exists | relation_does_not_exist.

-record(client_spec, {
    correct = [] :: [client()],
    unauthorized = [] :: [client()],
    forbidden = [] :: [client()]
}).

-record(data_spec, {
    required = [] :: [Key :: binary()],
    optional = [] :: [Key :: binary()],
    at_least_one = [] :: [Key :: binary()],
    correct_values = #{} :: #{Key :: binary() => Value :: binary()},
    bad_values = [] :: [{Key :: binary(), Value :: term(), data_error()}]
}).

-record(rest_spec, {
    method = get :: get | patch | post | put | delete,
    path = <<"/">> :: binary() | [binary()],
    headers = #{} :: #{Key :: binary() => Value :: binary()},
    expected_code = undefined :: undefined | integer(),
    expected_headers = undefined :: undefined | #{} | {contains, #{}},
    expected_body = undefined :: undefined | #{} | {contains, #{}}
}).

-record(logic_spec, {
    operation = get :: create | get | update | delete,
    module = undefined :: module(),
    function = undefined :: atom(),
    % In args, special atoms 'client' and 'data' can be used. In this case,
    % client and data will be automatically injected in these placeholders.
    args = [] :: [term()],
    expected_result = undefined :: undefined | fun((Result :: term()) -> boolean())
}).

-record(api_test_spec, {
    client_spec = undefined :: undefined | #client_spec{},
    rest_spec = undefined :: undefined | #rest_spec{},
    logic_spec = undefined :: undefined | #logic_spec{},
    data_spec = undefined :: undefined | #data_spec{}
}).

% Convenience macros for expected_result functions of logic_spec
-define(OK_BINARY, fun(Result) ->
    case Result of
        {ok, Bin} when is_binary(Bin) -> true;
        _ -> false
    end
end).

-define(OK_BINARY(__ExactValue), fun(__Result) ->
    case __Result of
        {ok, __ExactValue} -> true;
        _ -> false
    end
end).

-define(OK_TERM(__VerifyFun), fun(__Result) ->
    case __Result of
        {ok, Entity} -> __VerifyFun(Entity);
        _ -> false
    end
end).

-define(OK_LIST(__ExpectedList), fun(__Result) ->
    lists:sort(__ExpectedList) =:= lists:sort(__Result)
end).

-define(ERROR_REASON(__ExpectedError), fun(__Result) ->
    __ExpectedError =:= __Result
end).

-endif.