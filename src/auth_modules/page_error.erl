%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code
%% @end
%% ===================================================================

-module(page_error).
-compile(export_all).
-include("auth_common.hrl").
-include("gui_common.hrl").

%% Template points to the template file, which will be filled with content
main() -> #dtl{file = "bare", app = veil_cluster_node, bindings = [{title, title()}, {body, body()}]}.

%% Page title
title() -> <<"Error">>.

%% This will be placed in the template instead of {{body}} tag
body() ->
    {Reason, Description} = get_reason_and_description(),
    #panel{style = <<"position: relative;">>, body = [
        #panel{class = <<"alert alert-danger login-page">>, body = [
            #h3{body = <<"Error">>},
            #p{class = <<"login-info">>, style = <<"font-weight: bold;">>, body = Reason},
            #p{class = <<"login-info">>, body = Description},
            #button{postback = to_login, class = <<"btn btn-warning btn-block">>, body = <<"Login page">>}
        ]}
    ] ++ gui_utils:logotype_footer(120)}.

event(init) -> ok;

event(to_login) -> gui_utils:redirect_to_login(false).


% This function causes a HTTP rediurect to error page, which displays an error message.
redirect_with_error(ErrorID) ->
    wf:redirect(<<"/error?id=", (gui_utils:to_binary(ErrorID))/binary>>).


get_reason_and_description() ->
    IDBinary = gui_utils:to_binary(wf:q(<<"id">>)),
    id_to_reason_and_message(binary_to_atom(IDBinary, latin1)).


id_to_reason_and_message(?error_internal_server_error) ->
    {<<"500 - Internal server error">>, <<"Server encountered an unexpected error. Please contact the site administrator if the problem persists.">>};

id_to_reason_and_message(?error_auth_invalid_request) ->
    {<<"Invalid request state">>, <<"This OpenID or OAuth request could not be validated.">>};

id_to_reason_and_message(?error_auth_new_email_occupied) ->
    {<<"Cannot create new account">>, <<"One or more e-mail addresses returned by the OpenID / OAuth provider are occupied.">>};

id_to_reason_and_message(?error_auth_connect_email_occupied) ->
    {<<"Cannot connect chosen account to the profile">>, <<"One or more e-mail addresses returned by the OpenID / OAuth provider are occupied.">>};

id_to_reason_and_message(?error_auth_account_already_connected) ->
    {<<"Cannot connect chosen account to the profile">>, <<"This account has already been connected with other profile.">>};

id_to_reason_and_message(_) ->
    {<<"Unknown">>, <<"No description">>}.