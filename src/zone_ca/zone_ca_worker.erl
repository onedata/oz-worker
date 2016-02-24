%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% As CA actions need to be performed on all GR nodes,
%%% such actions are available through the worker interface.
%%% (in order to benefit from multicalls).
%%% todo: implement distributed CA properly (connected with VFS-1499)
%%% @end
%%%-------------------------------------------------------------------
-module(zone_ca_worker).
-author("Michal Zmuda").

-include_lib("ctool/include/logging.hrl").

-export([init/1, cleanup/0, handle/1]).

%%--------------------------------------------------------------------
%% @doc
%% Initialize module
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: worker_host:plugin_state()} | {error, Reason :: term()}.

init(_) ->
    {ok, #{}}.

%%--------------------------------------------------------------------
%% @doc
%% Do your work.
%% @end
%%--------------------------------------------------------------------
-spec handle(Request :: term()) ->
    nagios_handler:healthcheck_response() | ok | pong | {ok, Answer :: term()} |
    {error, Reason :: term()}.

handle(ping) ->
    pong;

handle(healthcheck) ->
    ok;

handle({revoke, Serial}) ->
    zone_ca:revoke(Serial);

handle({sign_provider_req, BinProviderId, CSRBin}) ->
    zone_ca:sign_provider_req(BinProviderId, CSRBin).


%%--------------------------------------------------------------------
%% @doc
%% The module will not be used anymore. Clean up!
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok | {error, Reason :: term()}.
cleanup() ->
    ok.