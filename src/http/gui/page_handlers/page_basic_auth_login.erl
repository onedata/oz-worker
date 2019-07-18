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

-include_lib("ctool/include/http/codes.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/api_errors.hrl").

%% API
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
handle(<<"POST">>, Req) ->
    try
        case auth_logic:authorize_by_basic_auth(Req) of
            {true, ?USER(UserId)} ->
                {ok, FullName} = user_logic:get_full_name(?ROOT, UserId),
                ?info("User '~ts' has logged in (~s)", [FullName, UserId]),
                Req2 = gui_session:log_in(UserId, Req),
                JSONHeader = #{<<"content-type">> => <<"application/json">>},
                Body = json_utils:encode(#{<<"url">> => <<"/">>}),
                cowboy_req:reply(?HTTP_200_OK, JSONHeader, Body, Req2);
            ?ERROR_BAD_BASIC_CREDENTIALS ->
                cowboy_req:reply(?HTTP_401_UNAUTHORIZED, #{}, <<"Invalid username or password">>, Req);
            ?ERROR_BASIC_AUTH_DISABLED ->
                cowboy_req:reply(?HTTP_401_UNAUTHORIZED, #{}, <<"Basic auth is disabled for this user">>, Req);
            ?ERROR_BASIC_AUTH_NOT_SUPPORTED ->
                cowboy_req:reply(?HTTP_400_BAD_REQUEST, #{}, <<"Basic auth is not supported by this Onezone">>, Req)
        end
    catch T:M ->
        ?error_stacktrace("Login by credentials failed - ~p:~p", [T, M]),
        cowboy_req:reply(?HTTP_401_UNAUTHORIZED, Req)
    end.
