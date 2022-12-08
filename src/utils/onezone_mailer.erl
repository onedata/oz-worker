%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Interface for sending emails. Uses a simple SMTP relay to send emails
%%% using an external mailbox. Credentials are configured in env variables.
%%% @end
%%%-------------------------------------------------------------------
-module(onezone_mailer).
-author("Lukasz Opiola").

-include_lib("ctool/include/smtp_client.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/errors.hrl").


%% API
-export([send/3]).

-define(ENV(E), oz_worker:get_env(E)).
-define(ENV(E, D), oz_worker:get_env(E, D)).
-define(AS_LIST(T), str_utils:to_list(T)).
-define(AS_BINARY(T), str_utils:to_binary(T)).

-define(RELAY, ?AS_LIST(?ENV(onezone_mailer_relay))).
-define(USERNAME, ?AS_LIST(?ENV(onezone_mailer_username))).
-define(PASSWORD, ?AS_LIST(?ENV(onezone_mailer_password))).
-define(SENDER_ADDRESS, ?AS_BINARY(?ENV(onezone_mailer_sender_address, ?USERNAME))).
-define(SENDER_NAME, ?AS_BINARY(?ENV(onezone_mailer_sender_name))).
-define(OPTIONS, ?ENV(onezone_mailer_smtp_client_opts, [])).


%%%===================================================================
%%% API
%%%===================================================================


%% @doc NOTE: the Subject is automatically prefixed with Onezone domain for unified appearance
-spec send([smtp_client:email_address()], binary(), binary()) -> ok | ?ERROR_INTERNAL_SERVER_ERROR.
send(RecipientAddresses, Subject, Body) ->
    EmailSpec = #email_spec{
        relay = ?RELAY,
        username = ?USERNAME,
        password = ?PASSWORD,
        sender_address = ?SENDER_ADDRESS,
        sender_name = ?SENDER_NAME,
        recipient_addresses = RecipientAddresses,
        subject = str_utils:format_bin("[~s] ~s", [oz_worker:get_domain(), Subject]),
        body = Body,
        options = ?OPTIONS
    },
    case smtp_client:send_email(EmailSpec) of
        {ok, ServerReply} ->
            ?info(
                "Email successfully sent~n"
                " To:        ~s~n"
                " Subject:   ~s~n"
                " Body:      ~B bytes~n"
                " Srv reply: ~s",
                [
                    str_utils:join_binary(RecipientAddresses, <<", ">>),
                    Subject,
                    byte_size(Body),
                    ServerReply
                ]
            );
        {error, {Type, Details}} ->
            ?error(
                "Email send error~n"
                " To:       ~s~n"
                " Subject:  ~s~n"
                " Body:     ~B bytes~n"
                " Err type: ~w~n"
                " Details:  ~p",
                [
                    str_utils:join_binary(RecipientAddresses, <<", ">>),
                    Subject,
                    byte_size(Body),
                    Type,
                    Details
                ]
            ),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.
