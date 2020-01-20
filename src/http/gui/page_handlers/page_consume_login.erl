%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called
%%% when OIDC or SAML assertion consume page is visited.
%%% @end
%%%-------------------------------------------------------------------
-module(page_consume_login).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("http/gui_paths.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include("auth/auth_errors.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").

-export([handle/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link dynamic_page_behaviour} callback handle/2.
%% @end
%%--------------------------------------------------------------------
-spec handle(gui:method(), cowboy_req:req()) -> cowboy_req:req().
handle(Method, Req) ->
    ValidateResult = idp_auth:validate_login(Method, Req),
    case idp_auth_test_mode:process_is_test_mode_enabled() of
        true ->
            cowboy_req:reply(?HTTP_200_OK, #{
                ?HDR_CONTENT_TYPE => <<"text/html">>
            }, render_test_login_results(ValidateResult), Req);
        false ->
            {NewReq, RedirectURL} = case ValidateResult of
                {ok, UserId, RedirectAfterLogin} ->
                    Req2 = gui_session:log_in(UserId, Req),
                    {Req2, RedirectAfterLogin};
                {auth_error, Error, State, RedirectAfterLogin} ->
                    Req2 = cowboy_req:set_resp_cookie(
                        <<"authentication_error_reason">>, format_error_reason(Error), Req,
                        #{path => <<"/">>}
                    ),
                    Req3 = cowboy_req:set_resp_cookie(
                        <<"authentication_error_state">>, State, Req2,
                        #{path => <<"/">>}
                    ),
                    {Req3, RedirectAfterLogin}
            end,
            % This page is visited with a POST request, so use a 303 redirect in
            % response so that web browser switches to GET.
            cowboy_req:reply(?HTTP_303_SEE_OTHER, #{
                <<"location">> => RedirectURL,
                % Connection close is required, otherwise chrome/safari can get stuck
                % stalled waiting for data.
                ?HDR_CONNECTION => <<"close">>
            }, NewReq)
    end.


%% @private
-spec format_error_reason({error, term()}) -> binary().
format_error_reason(?ERROR_BAD_AUTH_CONFIG) ->
    <<"bad_auth_config">>;
format_error_reason(?ERROR_INVALID_STATE) ->
    <<"invalid_state:", (integer_to_binary(state_token:ttl()))/binary>>;
format_error_reason(?ERROR_INVALID_AUTH_REQUEST) ->
    <<"invalid_auth_request">>;
format_error_reason(?ERROR_IDP_UNREACHABLE(_)) ->
    <<"idp_unreachable">>;
format_error_reason(?ERROR_BAD_IDP_RESPONSE(_, _, _, _)) ->
    <<"bad_idp_response">>;
format_error_reason(?ERROR_CANNOT_RESOLVE_REQUIRED_ATTRIBUTE(Attr)) ->
    <<"cannot_resolve_required_attribute:", (atom_to_binary(Attr, utf8))/binary>>;
format_error_reason(?ERROR_BAD_ATTRIBUTE_TYPE(Attr, _)) ->
    <<"cannot_resolve_required_attribute:", (atom_to_binary(Attr, utf8))/binary>>;
format_error_reason(?ERROR_ATTRIBUTE_MAPPING_ERROR(Attr, _, _, _, _)) ->
    <<"cannot_resolve_required_attribute:", (atom_to_binary(Attr, utf8))/binary>>;
format_error_reason(?ERROR_ACCOUNT_ALREADY_LINKED_TO_CURRENT_USER(_)) ->
    <<"account_already_linked_to_current_user">>;
format_error_reason(?ERROR_ACCOUNT_ALREADY_LINKED_TO_ANOTHER_USER(_, _)) ->
    <<"account_already_linked_to_another_user">>;
format_error_reason(?ERROR_INTERNAL_SERVER_ERROR) ->
    <<"internal_server_error">>.


%% @private
-spec render_test_login_results({ok, od_user:id(), RedirectPage :: binary()} |
{auth_error, {error, term()}, state_token:state_token(), RedirectPage :: binary()}) -> binary().
render_test_login_results(ValidateResult) ->
    StatusHeaders = case ValidateResult of
        {ok, _, _} ->
            UserData = idp_auth_test_mode:get_user_data(),
            str_utils:unicode_list_to_binary(str_utils:format(
                "<h3 style='color: #1c8e23;'>Login successful, gathered user data:</h3>~n"
                "<pre><code>~ts</code></pre>",
                [json_utils:encode(UserData, [pretty])]
            ));
        {auth_error, Error, _, _} ->
            str_utils:unicode_list_to_binary(str_utils:format(
                "<h3 style='color: #912011;'>Login failed due to:</h3>~n"
                "<pre><code>~tp</code></pre>",
                [Error]
            ))
    end,
    test_login_page_html(StatusHeaders, str_utils:unicode_list_to_binary(idp_auth_test_mode:get_logs())).


%% @private
-spec test_login_page_html(StatusHeaders :: binary(), Log :: binary()) -> binary().
test_login_page_html(StatusHeaders, Log) -> <<"
<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>
<head>
<body>
<div>
<p><a href=\"/#/test/login\">Back to test login page</a></p>
<i>Below results are a simulation of user login and what data would be gathered
in the process. Note that no users or groups are actually created in the system
in the test mode. Use only for diagnostics.</i>

<hr />

", StatusHeaders/binary, "

<hr />

<h3>Detailed log:</h3>
<pre style='max-width: 100%; word-wrap: break-word; white-space: pre-wrap;'><code>
", Log/binary, "
</code></pre>

</div>
</body>
</html>
">>.
