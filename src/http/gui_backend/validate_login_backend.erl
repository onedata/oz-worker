%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements page_backend_behaviour and is called
%%% when validate_login page is visited. It is used to verify the data
%%% returned by auth providers and log the user in.
%%% @end
%%%-------------------------------------------------------------------
-module(validate_login_backend).
-author("Lukasz Opiola").
-behaviour(page_backend_behaviour).

-include("gui/common.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([page_init/0]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link page_backend_behaviour} callback page_init/0.
%% @end
%%--------------------------------------------------------------------
page_init() ->
    case auth_utils:validate_login() of
        {redirect, URL} ->
            UserId = g_session:get_user_id(),
            ?info("User ~p logged in", [UserId]),
            case user_logic:get_default_provider(UserId) of
                {ok, undefined} ->
                    {redirect_relative, URL};
                {ok, ProvId} ->
                    ?debug("Automatically redirecting user `~s` "
                    "to default provider `~s`", [UserId, ProvId]),
                    ProvURL = auth_logic:get_redirection_uri(UserId, ProvId),
                    {redirect_relative, ProvURL}
            end;
        new_user ->
            UserId = g_session:get_user_id(),
            ?info("User ~p logged in for the first time", [UserId]),
            {redirect_relative, <<"/#/onezone">>};
        {error, ErrorId} ->
            ?info("Error: ~p", [ErrorId]),
            {redirect_relative, <<"/">>}
    end.
