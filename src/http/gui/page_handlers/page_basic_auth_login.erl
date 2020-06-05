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
-include_lib("ctool/include/http/headers.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/errors.hrl").

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
    Result = try
        case basic_auth:authenticate(Req) of
            {true, ?USER(UserId)} ->
                {ok, FullName} = od_user:get_full_name(UserId),
                ?info("User '~ts' has logged in (~s)", [FullName, UserId]),
                {ok, gui_session:log_in(UserId, Req)};
            false ->
                ?ERROR_UNAUTHORIZED;
            {error, _} = AuthenticationError ->
                AuthenticationError
        end
    catch Type:Reason ->
        ?error_stacktrace("Login by basic credentials failed - ~w:~p", [Type, Reason]),
        ?ERROR_INTERNAL_SERVER_ERROR
    end,
    case Result of
        {ok, NewReq} ->
            Body = json_utils:encode(#{<<"url">> => <<"/">>}),
            cowboy_req:reply(?HTTP_200_OK, #{?HDR_CONTENT_TYPE => <<"application/json">>}, Body, NewReq);
        {error, _} = Error ->
            Code = errors:to_http_code(Error),
            Body = json_utils:encode(#{<<"error">> => errors:to_json(Error)}),
            cowboy_req:reply(Code, #{?HDR_CONTENT_TYPE => <<"application/json">>}, Body, Req)
    end.
