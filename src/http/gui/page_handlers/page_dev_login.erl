%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called
%%% when developer login page is visited.
%%% @end
%%%-------------------------------------------------------------------
-module(page_dev_login).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("registered_names.hrl").
-include("entity_logic.hrl").
-include_lib("ctool/include/logging.hrl").

-export([handle/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link dynamic_page_behaviour} callback handle/2.
%% @end
%%--------------------------------------------------------------------
-spec handle(new_gui:method(), cowboy_req:req()) -> cowboy_req:req().
handle(<<"GET">>, Req) ->
    case oz_worker:get_env(dev_mode, false) of
        false ->
            cowboy_req:reply(404, Req);
        true ->
            {ok, UserIds} = user_logic:list(?ROOT),
            Buttons = lists:map(
                fun(UserId) ->
                    Path = str_utils:format_bin("/validate_dev_login?user=~s",
                        [UserId]),
                    str_utils:format_bin("<p><a href='~s'>~s</a></p>",
                        [Path, UserId])
                end, lists:usort(UserIds)),
            ButtonsBin = str_utils:join_binary(Buttons, <<"">>),
            Body = str_utils:format_bin("
<!DOCTYPE html>
<html>
<body>
<div style='text-align: center;'>
    <h1>Developer mode login:</h1>
    ~s
</div>
</body>
</html>
    ", [ButtonsBin]),
            cowboy_req:reply(200, #{}, Body, Req)
    end.
