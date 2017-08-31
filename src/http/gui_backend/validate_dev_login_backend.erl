%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements page_backend_behaviour and is called
%%% when validate_dev_login page is visited.
%%% It is used only in developer mode to log in bypassing auth providers.
%%% @end
%%%-------------------------------------------------------------------
-module(validate_dev_login_backend).
-author("Lukasz Opiola").
-behaviour(page_backend_behaviour).

-include_lib("ctool/include/logging.hrl").
-include("datastore/oz_datastore_models.hrl").

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
    ParamsProps = gui_ctx:get_url_params(),
    UserId = proplists:get_value(<<"user">>, ParamsProps),
    case gui_session:is_logged_in() of
        true ->
            case gui_session:get_user_id() of
                UserId ->
                    ok;
                _ ->
                    gui_session:log_out(),
                    gui_session:log_in(UserId)
            end;
        _ ->
            gui_session:log_in(UserId)
    end,
    ?info("[DEV MODE] User ~p logged in", [UserId]),
    case user_logic:get_default_provider_if_online(UserId) of
        {true, DefaultProv} ->
            ?debug("Automatically redirecting user `~s` "
            "to default provider `~s`", [UserId, DefaultProv]),
            {ok, ProvURL} = auth_logic:get_redirection_uri(UserId, DefaultProv),
            {redirect_absolute, ProvURL};
        false ->
            {redirect_relative, <<"/">>}
    end.
