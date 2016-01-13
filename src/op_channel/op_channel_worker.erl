%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% It is the worker which handles communication with providers.
%%% @end
%%%-------------------------------------------------------------------
-module(op_channel_worker).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("op_channel/op_channel.hrl").
-include("datastore/datastore_types.hrl").
-include_lib("ctool/include/logging.hrl").

-define(STATE_KEY, <<"op_logic_state_key">>).

-export([init/1, cleanup/0, handle/1]).

%%--------------------------------------------------------------------
%% @doc
%% Initialize module
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: worker_host:plugin_state()} | {error, Reason :: term()}.

init(_) ->
    op_logic_state:create(#document{key = ?STATE_KEY, value = #op_logic_state{providers = #{}, connections = #{}}}),
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

handle({add_connection, Provider, Connection}) ->
    Providers = state_get(providers),
    Connections = state_get(connections),

    ?info("Provider ~p connected successfully.", [Provider]),
    link(Connection),% todo: this does not work as it is worker

    FinalConnections = case maps:find(Provider, Connections) of
                           {ok, ProviderConnections} -> [Connection | ProviderConnections];
                           _ -> [Connection]
                       end,

    state_put(providers, maps:put(Connection, Provider, Providers)),
    state_put(connections, maps:put(Provider, FinalConnections, Connections)),
    ok;

handle({push, Providers, Msg}) ->
    Connections = state_get(connections),
    utils:pforeach(fun(Provider) ->
        case maps:find(Provider, Connections) of
            {ok, ProviderConnections} ->
                Connection = lists:nth(crypto:rand_uniform(1, length(ProviderConnections) + 1), ProviderConnections),
                Connection ! {push, Msg};
            Other ->
                ?warning("Cannot find provider ~p connections: ~p", [Provider, Other])
        end
                   end, Providers),
    ok;

handle({'EXIT', Connection, Reason}) -> % todo: this does not work as it is worker
    Providers = state_get(providers),
    Connections = state_get(connections),
    case maps:find(Connection, Providers) of
        {ok, Provider} ->
            ?warning("Connection to provider ~p lost due to: ~p", [Provider, Reason]),
            case maps:find(Provider, Connections) of
                {ok, [Connection]} ->
                    state_put(providers, maps:remove(Connection, Providers)),
                    state_put(connections, maps:remove(Provider, Connections)),
                    ok;
                {ok, ProviderConnections} ->
                    state_put(providers, maps:remove(Connection, Providers)),
                    state_put(connections, maps:put(Provider, ProviderConnections -- [Connection], Connections)),
                    ok;
                Other ->
                    ?error("Cannot find provider ~p connections: ~p", [Provider, Other]),
                    {error, provider_conections_not_found_at_exit}
            end;
        Other ->
            ?error("Cannot find provider for connection ~p: ~p", [Connection, Other]),
            {error, provider_not_found_at_exit}
    end.

%%--------------------------------------------------------------------
%% @doc
%% The module will not be used anymore. Clean up!
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok | {error, Reason :: term()}.
cleanup() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Puts given Value in datastore worker's state
%% @end
%%--------------------------------------------------------------------
-spec state_put(Key :: term(), Value :: term()) -> ok.
state_put(providers, Value) ->
    {ok, #document{value = #op_logic_state{} = State} = Doc} = op_logic_state:get(?STATE_KEY),
    {ok, _} = op_logic_state:save(Doc#document{value = State#op_logic_state{providers = Value}}),
    ok;

state_put(connections, Value) ->
    {ok, #document{value = #op_logic_state{} = State} = Doc} = op_logic_state:get(?STATE_KEY),
    {ok, _} = op_logic_state:save(Doc#document{value = State#op_logic_state{connections = Value}}),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Puts Value from datastore worker's state
%% @end
%%--------------------------------------------------------------------
-spec state_get(Key :: term()) -> Value :: term().
state_get(providers) ->
    {ok, #document{value = #op_logic_state{providers = Result}}} = op_logic_state:get(?STATE_KEY),
    Result;
state_get(connections) ->
    {ok, #document{value = #op_logic_state{connections = Result}}} = op_logic_state:get(?STATE_KEY),
    Result.
