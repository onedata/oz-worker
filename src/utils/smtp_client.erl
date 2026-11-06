%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module allows sending emails using an SMTP relay server.
%%% Merely a wrapper for gen_smtp_client library, but with a more
%%% convenient API.
%%% @end
%%%-------------------------------------------------------------------
-module(smtp_client).

-include("utils/smtp_client.hrl").


%% API
-export([send_email/1]).


-type email_address() :: binary().  % e.g. <<"example@gmail.com">>
-type email_spec() :: #email_spec{}.
-export_type([email_address/0, email_spec/0]).


%%%===================================================================
%%% API
%%%===================================================================

-spec send_email(email_spec()) -> {ok, binary()} | {error, {Type :: atom(), Details :: term()}}.
send_email(#email_spec{
    sender_address = SenderAddress,
    sender_name = SenderName,
    recipient_addresses = RecipientAddresses,
    subject = Subject,
    body = Body
} = EmailSpec) ->
    ToWithBrackets = str_utils:join_binary([<<"<", A/binary, ">">> || A <- RecipientAddresses], <<", ">>),
    Data = <<
        "Subject: ", Subject/binary, "\r\n",
        "From: ", SenderName/binary, " <", SenderAddress/binary, ">\r\n",
        "To: ", ToWithBrackets/binary, "\r\n\r\n",
        Body/binary
    >>,
    case gen_smtp_client:send_blocking({SenderAddress, RecipientAddresses, Data}, build_opts(EmailSpec)) of
        ServerReply when is_binary(ServerReply) ->
            {ok, ServerReply};
        {error, ValidateOptionsError} ->
            {error, {bad_options, ValidateOptionsError}};
        {error, Type, Details} ->
            {error, {Type, Details}}
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================

%% @private
-spec build_opts(email_spec()) -> gen_smtp_client:options().
build_opts(#email_spec{
    relay = Relay,
    username = Username,
    password = Password,
    options = Options
}) ->
    [
        {relay, Relay},
        {username, Username},
        {password, Password} |
        Options
    ].
