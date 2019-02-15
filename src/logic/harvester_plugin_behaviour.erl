%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% FIXME
%%% @end
%%%-------------------------------------------------------------------
-module(harvester_plugin_behaviour).


%%--------------------------------------------------------------------
%% @doc
%% FIXME
%% @end
%%--------------------------------------------------------------------
-callback submit_entry(binary(), binary(), binary(), maps:map()) -> ok | {error, term()}.

-callback delete_entry(binary(), binary(), binary()) -> ok | {error, term()}.

-callback query(binary(), binary(), binary()) -> {ok, value, binary()} | {error, term()}.
