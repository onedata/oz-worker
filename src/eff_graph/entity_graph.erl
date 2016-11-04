%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements logic that guards the consistency of entity graph.
%%% Entity graph concerns all entities in the system that are related to each
%%% other and decide of user's privileges in the system. These models are:
%%%     - od_user
%%%     - od_group
%%%     - od_space
%%%     - od_share
%%%     - od_provider
%%%     - od_handle_service
%%%     - od_handle
%%% There are two main jobs of the entity graph:
%%%     - encapsulate all logic concerning relations between entities
%%%     - ensure that effective relations are always up to date
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
-module(entity_graph).
-author("Lukasz Opiola").

-include_lib("ctool/include/logging.hrl").
-include("datastore/oz_datastore_models_def.hrl").

-define(STATE_KEY, <<"entity_graph_state">>).
-define(EFF_GRAPH_LOCK, eff_graph_lock).

%% API
-export([init_state/0, get_state/0]).
-export([schedule_refresh/0, refresh/0]).
-export([add_relation/5]).
-export([remove_relation/4]).

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
        case entity_graph_state:get(?STATE_KEY) of
            {ok, #document{value = #entity_graph_state{}}} ->
                ok;
            {error, {not_found, entity_graph_state}} ->
                entity_graph_state:create(
                    #document{key = ?STATE_KEY, value = #entity_graph_state{}}
                )
        end
    end).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves effective graph state, which is a globally shared singleton.
%% @end
%%--------------------------------------------------------------------
-spec get_state() -> #entity_graph_state{} | {error, term()}.
get_state() ->
    case entity_graph_state:get(?STATE_KEY) of
        {ok, #document{value = State}} ->
            State;
        Error ->
            Error
    end.


update_state(UpdateFun) ->
    {ok, _} = entity_graph_state:update(?STATE_KEY, UpdateFun),
    ok.


schedule_refresh() ->
    worker_proxy:cast(eff_graph_worker, refresh).


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
        #entity_graph_state{dirty = Dirty} = get_state(),
        case Dirty of
            true ->
                ?emergency("Refresh needed, performing..."),
                timer:sleep(5000),
                update_state(fun(EffGraphState) ->
                    {ok, EffGraphState#entity_graph_state{dirty = false}}
                end),
                ?emergency("Refresh done");
            false ->
                ?emergency("Refresh skipped."),
                ok
        end
    end),
    ok.


modify_group_relations(UpdateFunction) ->
    Success = critical_section:run(?EFF_GRAPH_LOCK, fun() ->
        #entity_graph_state{dirty = Dirty} = get_state(),
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
                    {ok, EffGraphState#entity_graph_state{dirty = true}}
                end),
                true
        end
    end),
    % Schedule a refresh no matter if the operation succeeded
    schedule_refresh(),
    case rand:uniform(2) of
        1 ->
            spawn(fun() -> entity_graph:modify_group_relations(fun() -> ok end),
                io:format("Done: ~p~n", [self()]) end);
        _ ->
            ok
    end,
    case Success of
        false ->
            % Try again in some time to acquire the lock and make modifications
            timer:sleep(100),
            modify_group_relations(UpdateFunction);
        true ->
            ok
    end.


add_relation(od_group, ChildGroupId, od_group, ParentGroupId, Privileges) ->
    entity_graph:modify_group_relations(fun() ->
        {ok, _} = od_group:update(ParentGroupId, fun(Group) ->
            #od_group{children = Groups} = Group,
            {ok, Group#od_group{
                children = [{ChildGroupId, Privileges} | Groups],
                bottom_up_dirty = true
            }}
        end),
        {ok, _} = od_group:update(ChildGroupId, fun(Group) ->
            #od_group{parents = Groups} = Group,
            {ok, Group#od_group{
                parents = [ParentGroupId | Groups],
                top_down_dirty = true
            }}
        end)
    end).
