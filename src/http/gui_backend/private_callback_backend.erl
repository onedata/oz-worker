%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements callback_backend_behaviour.
%%% It is used to handle callbacks from the client - such requests that do not
%%% correspond to underlying models. For example -
%%% 'give me the name of current user'.
%%% @end
%%%-------------------------------------------------------------------
-module(private_callback_backend).
-author("Lukasz Opiola").

-compile([export_all]).

-include_lib("ctool/include/logging.hrl").

%% API
-export([callback/2]).

callback(<<"sessionDetails">>, _) ->
    {ok, [
        {<<"sessionDetails">>, [
            {<<"userName">>, <<"zbyszek">>},
            {<<"firstLogin">>, true}
        ]}
    ]}.
