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
-include("registered_names.hrl").
-include("auth/auth_errors.hrl").
-include("entity_logic.hrl").
-include_lib("ctool/include/api_errors.hrl").
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
-spec handle(new_gui:method(), cowboy_req:req()) -> cowboy_req:req().
handle(Method, Req) ->
    {NewReq, RedirectURL} = case auth_logic:validate_login(Method, Req) of
        {ok, UserId, RedirectUrl} ->
            Req2 = new_gui_session:log_in(?USER(UserId), Req),
            {Req2, RedirectUrl};
        {auth_error, Error, State} ->
            Req2 = cowboy_req:set_resp_cookie(
                <<"authentication_error_reason">>, format_error_reason(Error), Req,
                #{path => <<"/">>}
            ),
            Req3 = cowboy_req:set_resp_cookie(
                <<"authentication_error_state">>, State, Req2,
                #{path => <<"/">>}
            ),
            {Req3, <<?LOGIN_PAGE_PATH>>}
    end,
    % This page is visited with a POST request, so use a 303 redirect in
    % response so that web browser switches to GET.
    cowboy_req:reply(303, #{
        <<"location">> => RedirectURL,
        % Connection close is required, otherwise chrome/safari can get stuck
        % stalled waiting for data.
        <<"connection">> => <<"close">>
    }, NewReq).


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
