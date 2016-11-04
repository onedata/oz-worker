%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements logic that guards the consistency of effective graph.
%%% Effective graph concerns all entities in the system that are related to each
%%% other and decide of user's privileges in the system. These models are:
%%%     - od_user
%%%     - od_group
%%%     - od_space
%%%     - od_share
%%%     - od_provider
%%%     - od_handle_service
%%%     - od_handle
%%% The graph can be in two states:
%%%   - up-to-date: all entities have their effective relations up-to-date,
%%%   - dirty: some documents require recalculation.
%%% All operations concerning effective graph state are guarded by a lock.
%%% Recalculations are scheduled after a relation has been modified - during
%%% documents update, they are marked as dirty, and added to the queue in
%%% effective graph state to be processed. After that, the modifying process
%%% calls eff_graph_worker to schedule a refresh.
%%% Operations that concern group relations cannot be performed while
%%% effective graph is dirty. They will wait on critical section lock until
%%% the graph has been fully recalculated. This is because groups can be
%%% nested, but cycles must be disallowed.
%%% Other operations can be performed in parallel, as effective graph logic
%%% operates only on effective relations, so no conflicts are possible.
%%% The relations introduced by other operations do not have possible negative
%%% effects, they will just mark some records as dirty and the effective graph
%%% will become eventually consistent.
%%% Graph refreshes can be scheduled multiple times, only one process at a time
%%% can recalculate the graph, and if the graph is already up-to-date, the
%%% refresh does nothing.
%%% @end
%%%-------------------------------------------------------------------
-module(eff_graph).
-author("Lukasz Opiola").

-include_lib("ctool/include/logging.hrl").
-include("datastore/oz_datastore_models_def.hrl").

-define(STATE_KEY, <<"eff_graph_state">>).
-define(EFF_GRAPH_LOCK, eff_graph_lock).

%% API
-export([init_state/0, get_state/0, refresh/0, modify_group_relations/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes effective graph state, which is a globally shared singleton.
%% Should be run during node initialization. The critical section makes sure
%% that only one state record creation is attempted.
%% @end
%%--------------------------------------------------------------------
-spec init_state() -> ok.
init_state() ->
    critical_section:run(?EFF_GRAPH_LOCK, fun() ->
        case eff_graph_state:get(?STATE_KEY) of
            {ok, #document{value = #eff_graph_state{}}} ->
                ok;
            {error, {not_found, eff_graph_state}} ->
                eff_graph_state:create(
                    #document{key = ?STATE_KEY, value = #eff_graph_state{}}
                )
        end
    end).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves effective graph state, which is a globally shared singleton.
%% @end
%%--------------------------------------------------------------------
-spec get_state() -> #eff_graph_state{} | {error, term()}.
get_state() ->
    case eff_graph_state:get(?STATE_KEY) of
        {ok, #document{value = State}} ->
            State;
        Error ->
            Error
    end.


update_state(UpdateFun) ->
    {ok, _} = eff_graph_state:update(?STATE_KEY, UpdateFun),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Performs refreshing of effective graph. If the graph is already up to date
%% (i.e. not dirty according to its state), it does nothing. If the graph is
%% dirty, it is processed until all documents that were marked dirty are
%% recalculated.
%% @end
%%--------------------------------------------------------------------
-spec refresh() -> ok.
refresh() ->
    critical_section:run(?EFF_GRAPH_LOCK, fun() ->
        #eff_graph_state{dirty = Dirty} = get_state(),
        case Dirty of
            true ->
                ?emergency("Refresh needed, performing..."),
                timer:sleep(5000),
                update_state(fun(EffGraphState) ->
                    {ok, EffGraphState#eff_graph_state{dirty = false}}
                end);
            false ->
                ?emergency("Refresh skipped."),
                ok
        end
    end),
    ok.


modify_group_relations(UpdateFunction) ->
    Success = critical_section:run(?EFF_GRAPH_LOCK, fun() ->
        #eff_graph_state{dirty = Dirty} = get_state(),
        case Dirty of
            true ->
                ?emergency("Cannot modify relations, waiting"),
                false;
            false ->
                ?emergency("Modifying relations..."),
                % TODO try catch
                UpdateFunction(),
                timer:sleep(1000),
                update_state(fun(EffGraphState) ->
                    {ok, EffGraphState#eff_graph_state{dirty = true}}
                end),
                true
        end
    end),
    % Schedule a refresh no matter if the operation succeeded
    worker_proxy:cast(eff_graph_worker, refresh),
    case Success of
        false ->
            % Try again in some time to acquire the lock and make modifications
            timer:sleep(100),
            modify_group_relations(UpdateFunction);
        true ->
            ok
    end.
