%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% As CA actions need to be performed on dedicated OZ node,
%%% requests are delegated to single, selected node to be processed.
%%% If dedicated node fails, CA ceases to function (as all the certificates
%%% and revocation list are stored only by the dedicated node).
%%% todo: implement distributed CA properly (connected with VFS-1499)
%%% @end
%%%-------------------------------------------------------------------
-module(ozpca_worker).
-author("Michal Zmuda").

-include_lib("ctool/include/logging.hrl").
-include("datastore/oz_datastore_models_def.hrl").

-define(KEY, <<"current_state">>).

%% worker_plugin_behaviour callbacks
-export([init/1, handle/1, cleanup/0]).

%% API
-export([supervisor_spec/0, supervisor_child_spec/0]).

%%%===================================================================
%%% worker_plugin_behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link worker_plugin_behaviour} callback init/1.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: worker_host:plugin_state()} | {error, Reason :: term()}.

init(_) ->
    {ok, #{dedicated_node => {error, no_nodes}}}.

%%--------------------------------------------------------------------
%% @doc
%% {@link worker_plugin_behaviour} callback handle/1.
%% @end
%%--------------------------------------------------------------------
-spec handle(Request :: term()) ->
    nagios_handler:healthcheck_response() | ok | pong | {ok, Answer :: term()} |
    {error, Reason :: term()}.

handle(ping) ->
    pong;

handle(healthcheck) ->
    case call_dedicated_node(fun() -> pong end, ping) of
        pong -> ok;
        {error, _Reason} -> {error, _Reason}
    end;

handle({verify_provider, PeerCert} = Req) ->
    call_dedicated_node(fun() -> ozpca:verify_provider(PeerCert) end, Req);

handle({revoke, Serial} = Req) ->
    call_dedicated_node(fun() -> ozpca:revoke(Serial) end, Req);

handle({sign_provider_req, BinProviderId, CSRBin} = Req) ->
    call_dedicated_node(fun() ->
        ozpca:sign_provider_req(BinProviderId, CSRBin)
    end, Req).


%%--------------------------------------------------------------------
%% @doc
%% {@link worker_plugin_behaviour} callback cleanup/0
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok | {error, Reason :: term()}.
cleanup() ->
    ok.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns a supervisor spec for a ozpca worker supervisor.
%% @end
%%--------------------------------------------------------------------
-spec supervisor_spec() -> supervisor:sup_flags().
supervisor_spec() ->
    #{
        strategy => one_for_one,
        intensity => 1000,
        period => 3600
    }.

%%--------------------------------------------------------------------
%% @doc
%% Returns a worker child_spec for a ozpca gen_server.
%% @end
%%--------------------------------------------------------------------
-spec supervisor_child_spec() -> supervisor:child_spec().
supervisor_child_spec() ->
    #{
        id => ozpca,
        start => {ozpca, start_link, []},
        restart => permanent,
        shutdown => timer:seconds(10),
        type => worker,
        modules => [ozpca]
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Pass a request to a dedicated node or execute the callback if already at
%% the dedicated node.
%% @end
%%--------------------------------------------------------------------

-spec call_dedicated_node(ExecIfImDedicated :: fun(()-> term()), Req :: term())
        -> Result :: term() | {error, Reason :: term()}.
call_dedicated_node(Fun, Req) ->
    Self = node(),
    case get_dedicated_node() of
        {ok, Self} ->
            Fun();
        {ok, DedicatedNode} ->
            worker_proxy:call({?MODULE, DedicatedNode}, Req);
        {error, _Reason} ->
            ?error_stacktrace("Cannot process CA request ~p due to error ~p", [Req, _Reason]),
            {error, _Reason}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a dedicated node or tries to select one if none is selected yet.
%% @end
%%--------------------------------------------------------------------
-spec get_dedicated_node() -> {ok, node()} | {error, Reason :: term()}.
get_dedicated_node() ->
    case ozpca_state:get(?KEY) of
        {ok, #document{value = #ozpca_state{dedicated_node = {ok, _Node}}}} ->
            {ok, _Node};
        _ ->
            SelectResult = select_dedicated_node(),
            ozpca_state:create_or_update(#document{
                key = ?KEY,
                value = #ozpca_state{dedicated_node = SelectResult}
            }, fun
                (State = #ozpca_state{dedicated_node = {error, _}}) ->
                    {ok, State#ozpca_state{dedicated_node = SelectResult}};
                (State = #ozpca_state{dedicated_node = {ok, _Node}}) ->
                    {ok, State}
            end),
            SelectResult
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a node dedicated to handle requests.
%% @end
%%--------------------------------------------------------------------
-spec select_dedicated_node() -> {ok, node()} | {error, Reason :: term()}.
select_dedicated_node() ->
    case request_dispatcher:get_worker_nodes(?MODULE) of
        {ok, [Node | _]} -> {ok, Node};
        _ -> {error, no_nodes}
    end.