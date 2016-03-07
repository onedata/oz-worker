%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements page_backend_behaviour and is called
%%% when login page is visited - it contains login logic (redirects to GR).
%%% THIS IS A PROTOTYPE AND AN EXAMPLE OF IMPLEMENTATION.
%%% @end
%%%-------------------------------------------------------------------
-module(validate_dev_login_backend).
-author("Lukasz Opiola").
-behaviour(page_backend_behaviour).

-compile([export_all]).

-include_lib("ctool/include/logging.hrl").

%% API
-export([page_init/0]).


page_init() ->
    ParamsProps = g_ctx:get_url_params(),
    UserId = proplists:get_value(<<"user">>, ParamsProps),
    case {g_session:is_logged_in(), g_session:get_user_id()} of
        {true, UserId} ->
            ok;
        {true, _} ->
            g_session:log_out(),
            g_session:log_in(UserId);
        _ ->
            g_session:log_in(UserId)
    end,
    ?info("[DEV MODE] User ~p logged in", [UserId]),
    case user_logic:get_default_provider(UserId) of
        {ok, undefined} ->
            {redirect_relative, <<"/">>};
        {ok, ProvId} ->
            ?debug("Automatically redirecting user `~s` "
            "to default provider `~s`", [UserId, ProvId]),
            {ok, ProvURL} = auth_logic:get_redirection_uri(UserId, ProvId),
            {redirect_absolute, ProvURL}
    end.
