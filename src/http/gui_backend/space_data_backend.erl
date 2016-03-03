%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements data_backend_behaviour and is used to synchronize
%%% the file model used in Ember application.
%%% THIS IS A PROTOTYPE AND AN EXAMPLE OF IMPLEMENTATION.
%%% @end
%%%-------------------------------------------------------------------
-module(space_data_backend).
-author("Lukasz Opiola").

-compile([export_all]).

-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([init/0]).
-export([find/2, find_all/1, find_query/2]).
-export([create_record/2, update_record/3, delete_record/2]).

%% Convenience macro to log a debug level log dumping given variable.
-define(log_debug(_Arg),
    ?alert("~s", [str_utils:format("SPACE_DATA_BACKEND: ~s: ~p", [??_Arg, _Arg])])
).


init() ->
    ?log_debug({websocket_init, g_session:get_session_id()}),
%%    {ok, _Pid} = data_backend:async_process(fun() -> async_loop() end),
    ok.


find(<<"space">>, [_SpaceId]) ->
    {error, not_iplemented}.

find_query(<<"space">>, _Data) ->
    {error, not_iplemented}.

%% Called when ember asks for all files
find_all(<<"space">>) ->
    UserId = g_session:get_user_id(),
    {ok, GetSpaces} = user_logic:get_spaces(UserId),
    Spaces = proplists:get_value(spaces, GetSpaces),
    Default = proplists:get_value(default, GetSpaces),
    Res = lists:map(
        fun(SpaceId) ->
            ?dump(SpaceId),
            {ok, SpaceData} = space_logic:get_data(SpaceId, provider),
            Name = proplists:get_value(name, SpaceData),
            {ok, [{providers, Providers}]} =
                space_logic:get_providers(SpaceId, provider),
            [
                {<<"id">>, SpaceId},
                {<<"name">>, Name},
                {<<"isDefault">>, SpaceId =:= Default},
                {<<"providers">>, Providers}
            ]
        end, Spaces),
    {ok, Res}.


%% Called when ember asks to create a record
create_record(<<"space">>, Data) ->
    ?dump(Data),
    Name = proplists:get_value(<<"name">>, Data),
    {ok, SpaceId} = space_logic:create({user, g_session:get_user_id()}, Name),
    NewSpaceData = [
        {<<"id">>, SpaceId},
        {<<"name">>, Name},
        {<<"isDefault">>, false},
        {<<"providers">>, []}
    ],
    {ok, NewSpaceData}.

%% Called when ember asks to update a record
update_record(<<"space">>, SpaceId, Data) ->
    ?dump({SpaceId, Data}),
    UserId = g_session:get_user_id(),
    IsDefault = proplists:get_value(<<"isDefault">>, Data),
    case IsDefault of
        true ->
            user_logic:set_default_space(UserId, SpaceId);
        false ->
            ok
    end,
    ok.

%% Called when ember asks to delete a record
delete_record(<<"space">>, _Id) ->
    {error, not_iplemented}.
