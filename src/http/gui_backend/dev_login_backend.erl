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
-module(dev_login_backend).
-author("Lukasz Opiola").
-behaviour(page_backend_behaviour).

-compile([export_all]).

-include_lib("ctool/include/logging.hrl").

%% API
-export([page_init/0]).


page_init() ->
    % @todo while we don't have list all, assume that all users have names
    % like u0, u1, u2, ... user0, user1, user2 ... up to u10, user10
%%    {ok, UserIds} = lists:foldl(
%%        fun(Index, Acc) ->
%%            Id1 = <<"u", (integer_to_binary(Index))/binary>>,
%%            U1 = case onedata_user:get(Id1) of
%%                {ok, _} -> [Id1];
%%                _ -> []
%%            end,
%%            Id2 = <<"user", (integer_to_binary(Index))/binary>>,
%%            U2 = case onedata_user:get(Id2) of
%%                {ok, _} -> [Id2];
%%                _ -> []
%%            end,
%%            Acc ++ U1 ++ U2
%%        end, [], lists:seq(0, 10)),
    {ok, UserIds} = onedata_user:get_all_ids(),
    ?dump(UserIds),
    Buttons = lists:map(
        fun(UserId) ->
            Path = str_utils:format_bin(<<"/validate_dev_login?user=~s">>, [UserId]),
            str_utils:format_bin(<<"<p><a href='~s'>~s</a></p>">>, [Path, UserId])
        end, lists:usort(UserIds)),
    ButtonsBin = str_utils:join_binary(Buttons, <<"">>),
    Body = str_utils:format_bin(<<"
<!DOCTYPE html>
<html>
<body>
<div style='text-align: center;'>
    <h1>Developer mode login:</h1>
    ~s
</div>
</body>
</html>
    ">>, [ButtonsBin]),
    {serve_body, Body, [{<<"content-type">>, <<"text/html">>}]}.
