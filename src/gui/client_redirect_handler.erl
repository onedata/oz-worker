%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles requests with URL in form alias.gr.domain and
%%% performs a HTTP redirect to proper provider.
%%% @end
%%%-------------------------------------------------------------------
-module(client_redirect_handler).

-include_lib("ctool/include/logging.hrl").
-include("dao/dao_types.hrl").

-export([init/3, handle/2, terminate/3]).

%%--------------------------------------------------------------------
%% @doc Cowboy handler callback, no state is required
%% @end
%%--------------------------------------------------------------------
-spec init(any(), term(), any()) -> {ok, term(), []}.
init(_Type, Req, _Opts) ->
    {ok, Req, []}.

%%--------------------------------------------------------------------
%% @doc Handles a request returning a HTTP Redirect (307 - Moved temporarily).
%% @end
%%--------------------------------------------------------------------
-spec handle(term(), term()) -> {ok, term(), term()}.
handle(Req, State) ->
    try
        % Get query string from URL
        {QS, _} = cowboy_req:qs(Req),
        % Get the alias from URL
        {Alias, _} = cowboy_req:binding(alias, Req),

        % Find the user and his default provider
        GetUserResult = case Alias of
                            <<?NO_ALIAS_UUID_PREFIX, UUID/binary>> ->
                                user_logic:get_user_doc(UUID);
                            _ ->
                                case user_logic:get_user_doc({alias, Alias}) of
                                    {ok, Ans} ->
                                        {ok, Ans};
                                    _ ->
                                        user_logic:get_user_doc(Alias)
                                end
                        end,
        {ok, #db_document{record = #user{default_provider = DefaultProvider}}} = GetUserResult,
        {ok, DataProplist} = provider_logic:get_data(DefaultProvider),
        RedPoint = binary_to_list(proplists:get_value(redirectionPoint, DataProplist)),
        {ok, {_Scheme, _UserInfo, HostStr, _Port, _Path, _Query}} = http_uri:parse(str_utils:to_list(RedPoint)),
        {Path, _} = cowboy_req:path(Req),
        {ok, Req2} = cowboy_req:reply(307,
            [
                {<<"location">>, <<"https://", (list_to_binary(HostStr))/binary, Path/binary, "?", QS/binary>>},
                {<<"content-type">>, <<"text/html">>}
            ], <<"">>, Req),
        {ok, Req2, State}
    catch T:M ->
        ?debug_stacktrace("Error while redirecting client - ~p:~p", [T, M]),
        {ok, Req3} = cowboy_req:reply(404, [], <<"">>, Req),
        {ok, Req3, State}
    end.

%%--------------------------------------------------------------------
%% @doc Cowboy handler callback, no cleanup needed
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), term(), term()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
