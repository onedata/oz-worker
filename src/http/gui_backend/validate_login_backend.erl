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
-include("datastore/oz_datastore_models_def.hrl").

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
-spec page_init() -> gui_html_handler:page_init_result().
page_init() ->
    case auth_utils:validate_login() of
        {redirect, URL} ->
            UserId = gui_session:get_user_id(),
            ?info("User ~p logged in", [UserId]),
            case n_user_logic:get_default_provider_if_online(UserId) of
                {true, DefaultProv} ->
                    ?debug("Automatically redirecting user `~s` "
                    "to default provider `~s`", [UserId, DefaultProv]),
                    {ok, ProvURL} = auth_logic:get_redirection_uri(
                        UserId, DefaultProv
                    ),
                    {redirect_absolute, ProvURL};
                false ->
                    {redirect_relative, URL}
            end;
        new_user ->
            UserId = gui_session:get_user_id(),
            ?info("User ~p logged in for the first time", [UserId]),
            {redirect_relative, <<?PAGE_AFTER_LOGIN>>};
        {error, ErrorId} ->
            ?error("Error during login: ~p", [ErrorId]),
            {redirect_relative, <<"/">>}
    end.
