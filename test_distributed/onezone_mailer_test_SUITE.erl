%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Tests of utilities for sending emails using SMTP relay.
%%% @end
%%%-------------------------------------------------------------------
-module(onezone_mailer_test_SUITE).
-author("Lukasz Opiola").

-include("ozt.hrl").
-include("utils/smtp_client.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/errors.hrl").


-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([
    unconfigured_mailer_test/1,
    error_handling_test/1,
    successful_sending_test/1
]).

all() ->
    ?ALL([
        unconfigured_mailer_test,
        error_handling_test,
        successful_sending_test
    ]).


% an SMTP server is started on each node, so from the point of view of oz-worker,
% this is the hostname of the server
-define(STUB_SMTP_SERVER_HOST, "localhost").
-define(STUB_SMTP_SERVER_PORT, 2525).

-define(RECIPIENTS, [<<"a@example.com">>, <<"b@example.com">>, <<"c@example.com">>]).
-define(SUBJECT, <<"Email subject ", (?RAND_STR(20))/binary>>).
-define(BODY, ?RAND_STR(500)).

%%%===================================================================
%%% Test functions
%%%===================================================================


unconfigured_mailer_test(_Config) ->
    % by default, the mailer config is not set; attempts to send an email should result in errors
    ?assertMatch(?ERROR_INTERNAL_SERVER_ERROR(_), call_onezone_mailer_send()).


error_handling_test(_Config) ->
    ozt:set_env(onezone_mailer_relay, ?STUB_SMTP_SERVER_HOST),
    ozt:set_env(onezone_mailer_username, "test@example.com"),
    ozt:set_env(onezone_mailer_password, "password"),
    % bad relay
    ozt:set_env(onezone_mailer_smtp_client_opts, [{onezone_mailer_relay, "inexistent-relay-host"}]),
    ?assertMatch(?ERROR_INTERNAL_SERVER_ERROR(_), call_onezone_mailer_send()),
    ?assertMatch({error, {retries_exceeded, {network_failure, _, {error, econnrefused}}}}, call_smtp_client_send()),
    % bad port
    ozt:set_env(onezone_mailer_relay, ?STUB_SMTP_SERVER_HOST),
    ozt:set_env(onezone_mailer_smtp_client_opts, [{port, "not-a-port"}]),
    ?assertMatch(?ERROR_INTERNAL_SERVER_ERROR(_), call_onezone_mailer_send()),
    ?assertMatch({error, {bad_options, invalid_port}}, call_smtp_client_send()),
    % unreachable port
    ozt:set_env(onezone_mailer_smtp_client_opts, [{port, ?STUB_SMTP_SERVER_PORT + 1}]),
    ?assertMatch(?ERROR_INTERNAL_SERVER_ERROR(_), call_onezone_mailer_send()),
    ?assertMatch({error, {retries_exceeded, {network_failure, _, {error, econnrefused}}}}, call_smtp_client_send()).


successful_sending_test(_Config) ->
    ozt:set_env(onezone_mailer_relay, ?STUB_SMTP_SERVER_HOST),
    ozt:set_env(onezone_mailer_username, "test@example.com"),
    ozt:set_env(onezone_mailer_password, "password"),
    ozt:set_env(onezone_mailer_smtp_client_opts, [{port, ?STUB_SMTP_SERVER_PORT}]),
    ?assertMatch(ok, call_onezone_mailer_send()),
    ?assertMatch({ok, _}, call_smtp_client_send()).


%%%===================================================================
%%% Internal functions
%%%===================================================================

% Two interfaces are tested; onezone_mailer that logs any problems and
% returns standardized errors, as well as smtp_client that can return
% low-level errors, not relevant to high-level modules that need to send emails.

%% @private
-spec call_onezone_mailer_send() ->
    {ok, binary()} | {error, {Type :: atom(), Details :: term()}}.
call_onezone_mailer_send() ->
    ozt:rpc(onezone_mailer, send, [?RECIPIENTS, ?SUBJECT, ?BODY]).


%% @private
-spec call_smtp_client_send() ->
    {ok, binary()} | {error, {Type :: atom(), Details :: term()}}.
call_smtp_client_send() ->
    ozt:rpc(smtp_client, send_email, [#email_spec{
        relay = str_utils:to_list(ozt:get_env(onezone_mailer_relay)),
        username = str_utils:to_list(ozt:get_env(onezone_mailer_username)),
        password = str_utils:to_list(ozt:get_env(onezone_mailer_password)),
        sender_address = str_utils:to_binary(ozt:get_env(onezone_mailer_username)),
        sender_name = <<"SENDER_NAME">>,
        recipient_addresses = ?RECIPIENTS,
        subject = ?SUBJECT,
        body = ?BODY,
        options = ozt:get_env(onezone_mailer_smtp_client_opts, [])
    }]).

%%%===================================================================
%%% Setup/teardown functions
%%%===================================================================

init_per_suite(Config) ->
    ozt:init_per_suite(Config, fun() ->
        % starts a simple SMTP server on each node so that integration
        % of the Onezone's mailer client can be tested
        ozt:rpc(ozt:get_nodes(), gen_smtp_server, start, [smtp_server_example, [{port, ?STUB_SMTP_SERVER_PORT}]])
    end).

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.
