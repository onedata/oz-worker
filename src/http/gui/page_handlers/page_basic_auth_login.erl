%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called
%%% when basic login page is visited.
%%% @end
%%%-------------------------------------------------------------------
-module(page_basic_auth_login).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([handle/2]).

% Session cookie id
-define(SESSION_COOKIE_KEY, <<"session_id">>).
% Value of cookie when there is no session
-define(NO_SESSION_COOKIE, <<"no_session">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link dynamic_page_behaviour} callback handle/2.
%% @end
%%--------------------------------------------------------------------
-spec handle(new_gui:method(), cowboy_req:req()) -> cowboy_req:req().
handle(<<"POST">>, Req) ->
    try
        <<"Basic ", UserAndPassword/binary>> =
            cowboy_req:header(<<"authorization">>, Req),
        [User, Passwd] = binary:split(base64:decode(UserAndPassword), <<":">>),
        case user_logic:authenticate_by_basic_credentials(User, Passwd) of
            {ok, #document{key = UserId}} ->
                ?info("User ~s logged in", [UserId]),
                Req2 = oz_gui_session:log_in(UserId, Req),
                JSONHeader = #{<<"content-type">> => <<"application/json">>},
                Body = json_utils:encode(#{<<"url">> => <<"/">>}),
                cowboy_req:reply(200, JSONHeader, Body, Req2);
            {error, Binary} when is_binary(Binary) ->
                cowboy_req:reply(401, #{}, Binary, Req);
            _ ->
                cowboy_req:reply(401, Req)
        end
    catch T:M ->
        ?error_stacktrace("Login by credentials failed - ~p:~p", [T, M]),
        cowboy_req:reply(401, Req)
    end.


