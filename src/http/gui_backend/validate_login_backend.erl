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
-module(validate_login_backend).
-author("Lukasz Opiola").
-behaviour(page_backend_behaviour).

-compile([export_all]).

-include_lib("ctool/include/logging.hrl").

%% API
-export([page_init/0]).


page_init() ->
    case auth_utils:validate_login() of
        {redirect, URL} ->
            ?info("User ~p logged in", [g_session:get_user_id()]),
            {redirect_relative, URL};
        new_user ->
            ?info("User ~p logged in for the first time", [g_session:get_user_id()]),
            {redirect_relative, <<"/#/onezone">>};
        {error, ErrorID} ->
            ?info("Error: ~p", [ErrorID]),
            {redirect_relative, <<"/">>}
    end.
