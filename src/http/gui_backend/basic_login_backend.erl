%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements page_backend_behaviour and is called
%%% when /do_login page is visited. It is used to verify the data
%%% returned by auth providers and log the user in.
%%% @end
%%%-------------------------------------------------------------------
-module(basic_login_backend).
-author("Lukasz Opiola").
-behaviour(page_backend_behaviour).

-include("gui/common.hrl").
-include("datastore/oz_datastore_models_def.hrl").
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
-spec page_init() -> gui_html_handler:page_init_result().
page_init() ->
    Req = gui_ctx:get_cowboy_req(),
    try
        {<<"Basic ", UserAndPassword/binary>>, _} =
            cowboy_req:header(<<"authorization">>, Req),
        [User, Passwd] = binary:split(base64:decode(UserAndPassword), <<":">>),
        case n_user_logic:authenticate_by_basic_credentials(User, Passwd) of
            {ok, UserDoc, FirstLogin} ->
                #document{
                    key = UserId,
                    value = #od_user{
                        default_provider = DefaultProvider
                    }} = UserDoc,
                gui_session:log_in(UserId),
                gui_session:put_value(firstLogin, FirstLogin),
                % If user has a default provider, redirect him straight there
                URL = case DefaultProvider of
                    undefined ->
                        <<"/">>;
                    ProviderId ->
                        ?debug("Automatically redirecting user `~s` "
                        "to default provider `~s`", [UserId, ProviderId]),
                        {ok, ProviderURL} = auth_logic:get_redirection_uri(
                            UserId, ProviderId
                        ),
                        ProviderURL
                end,
                JSONHeader = #{<<"content-type">> => <<"application/json">>},
                Body = json_utils:encode_map(#{<<"url">> => URL}),
                {reply, 200, JSONHeader, Body};
            {error, Binary} when is_binary(Binary) ->
                {reply, 401, [], Binary};
            _ ->
                {reply, 401}
        end
    catch T:M ->
        ?error_stacktrace("Login by credentials failed - ~p:~p", [T, M]),
        {reply, 401}
    end.
