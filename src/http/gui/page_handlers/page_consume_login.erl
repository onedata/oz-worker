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
    ValidationResult = case Method of
        <<"POST">> ->
            % SAML flow
            auth_utils:validate_saml_login(Req);
        <<"GET">> ->
            % OIDC flow
            auth_utils:validate_oidc_login(Req)
    end,
    {NewReq, RedirectURL} = case ValidationResult of
        {ok, {UserId, RedirectUrl}} ->
            ?info("User ~s logged in", [UserId]),
            Req2 = oz_gui_session:log_in(UserId, Req),
            {Req2, RedirectUrl};
        {error, ErrorId} ->
            Req2 = new_gui:set_cookie(
                <<"authentication_error">>,
                atom_to_binary(ErrorId, utf8),
                #{path => <<"/">>},
                Req
            ),
            {Req2, <<?LOGIN_PAGE_PATH>>}
    end,
    % This page is visited with a POST request, so use a 303 redirect in
    % response so that web browser switches to GET.
    cowboy_req:reply(303, #{
        <<"location">> => RedirectURL,
        % Connection close is required, otherwise chrome/safari can get stuck
        % stalled waiting for data.
        <<"connection">> => <<"close">>
    }, NewReq).