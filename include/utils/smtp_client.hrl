%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2022 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Definitions related to smtp_client.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(SMTP_CLIENT_HRL).
-define(SMTP_CLIENT_HRL, 1).


% contains information required to build and send an email message
-record(email_spec, {
    relay :: string(),  % e.g. "smtp.gmail.com"
    username :: string(),  % e.g. "example@gmail.com"
    % password for the above mailbox username;
    % for gmail, an "App Password" is needed here
    password :: string(),
    % address and name that will appear in the "From" section of the email;
    % typically same as the username but does not have to be, as long as the user
    % is authorized to send emails from different addresses
    sender_address :: smtp_client:email_address(),
    sender_name :: binary(),
    recipient_addresses :: [smtp_client:email_address()],
    subject :: binary(),
    body :: binary(),
    % extended with relay, username and password and then passed to the gen_smtp_client lib
    options = [] :: gen_smtp_client:options()
}).


-endif.
