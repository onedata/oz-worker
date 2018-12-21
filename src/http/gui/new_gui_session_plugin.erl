%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements new_gui_session_plugin behaviour and integrates GUI
%%% session handling with internal Onezone session.
%%% @end
%%%-------------------------------------------------------------------
-module(new_gui_session_plugin).
-behavior(new_gui_session_plugin_behaviour).

-author("Lukasz Opiola").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("gui/include/gui_session.hrl").
-include_lib("ctool/include/logging.hrl").

-define(TO_DETAILS(__Term), case __Term of
    {ok, #document{value = __Session}} -> {ok, session_to_details(__Session)};
    {error, _} = __Error -> __Error
end).

-export([create/2, get/1, update/2, delete/1, timestamp/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link new_gui_session_plugin_behaviour} callback create/2.
%% @end
%%--------------------------------------------------------------------
-spec create(new_gui_session:id(), new_gui_session:details()) -> ok | {error, term()}.
create(Id, Details) ->
    session:create(Id, details_to_session(Details)).


%%--------------------------------------------------------------------
%% @doc
%% {@link new_gui_session_plugin_behaviour} callback get/1.
%% @end
%%--------------------------------------------------------------------
-spec get(new_gui_session:id()) -> {ok, new_gui_session:details()} | {error, term()}.
get(Id) ->
    ?TO_DETAILS(session:get(Id)).


%%--------------------------------------------------------------------
%% @doc
%% {@link new_gui_session_plugin_behaviour} callback update/2.
%% @end
%%--------------------------------------------------------------------
-spec update(new_gui_session:id(), fun((new_gui_session:details()) -> new_gui_session:details())) ->
    ok | {error, term()}.
update(Id, Diff) ->
    UpdateFun = fun(Session) ->
        {ok, details_to_session(Diff(session_to_details(Session)))}
    end,
    ?TO_DETAILS(session:update(Id, UpdateFun)).


%%--------------------------------------------------------------------
%% @doc
%% {@link new_gui_session_plugin_behaviour} callback delete/1.
%% @end
%%--------------------------------------------------------------------
-spec delete(new_gui_session:id()) -> ok | {error, term()}.
delete(Id) ->
    session:delete(Id).


%%--------------------------------------------------------------------
%% @doc
%% {@link new_gui_session_plugin_behaviour} callback timestamp/0.
%% @end
%%--------------------------------------------------------------------
-spec timestamp() -> non_neg_integer().
timestamp() ->
    time_utils:cluster_time_seconds().


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec session_to_details(session:record()) -> new_gui_session:details().
session_to_details(#session{user_id = UserId, last_refresh = Refreshed, nonce = Nonce, previous_nonce = PrevNonce}) ->
    #gui_session{client = UserId, last_refresh = Refreshed, nonce = Nonce, previous_nonce = PrevNonce}.


-spec details_to_session(new_gui_session:details()) -> session:record().
details_to_session(#gui_session{client = UserId, last_refresh = Refreshed, nonce = Nonce, previous_nonce = PrevNonce}) ->
    #session{user_id = UserId, last_refresh = Refreshed, nonce = Nonce, previous_nonce = PrevNonce}.