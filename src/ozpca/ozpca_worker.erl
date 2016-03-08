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

-export([init/1, cleanup/0, handle/1]).

%%--------------------------------------------------------------------
%% @doc
%% Initialize module
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: worker_host:plugin_state()} | {error, Reason :: term()}.

init(_) ->
    {ok, #{dedicated_node => {error, no_nodes}}}.

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
%% The module will not be used anymore. Clean up!
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok | {error, Reason :: term()}.
cleanup() ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @private
%% Pass request to dedicated node
%% or execute the callback if already at dedicated node.
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
%% @doc
%% @private
%% Get selected dedicated node or try to select one if none is selected yet.
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
%% @doc
%% @private
%% Selects one node eligible to be dedicated.
%% @end
%%--------------------------------------------------------------------
-spec select_dedicated_node() -> {ok, node()} | {error, Reason :: term()}.
select_dedicated_node() ->
    case request_dispatcher:get_worker_nodes(?MODULE) of
        {ok, [Node | _]} -> {ok, Node};
        _ -> {error, no_nodes}
    end.