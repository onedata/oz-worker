%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements page_backend_behaviour and is called
%%% when dev_login page is visited.
%%% It is used only in developer mode to log in bypassing auth providers.
%%% @end
%%%-------------------------------------------------------------------
-module(dev_login_backend).
-author("Lukasz Opiola").
-behaviour(page_backend_behaviour).

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
    {ok, UserIds} = onedata_user:get_all_ids(),
    ?dump(UserIds),
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
    {serve_body, Body, [{<<"content-type">>, <<"text/html">>}]}.
