%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This modules is responsible for publishing location data in location service.
%%% @end
%%%-------------------------------------------------------------------
-module(record_location_hooks).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-define(METHODS, [create, create_or_update, save]).
-define(MODELS, [od_space, od_group, od_user, od_provider]).

-export([get_hooks/0, handle_before/3, handle_after/4]).

%%--------------------------------------------------------------------
%% @doc
%% Returns hooks to be added.
%% These hooks ensure location data is published in location service.
%% @end
%%--------------------------------------------------------------------
-spec get_hooks() -> [{model_behaviour:model_type(), model_behaviour:model_action()}].
get_hooks() ->
    case application:get_env(?APP_NAME, location_service_enabled) of
        {ok, true} ->
            [{X, Y} || X <- ?MODELS, Y <- ?METHODS];
        {ok, false} ->
            ?debug("location service hooks disabled"),
            []
    end.

%%--------------------------------------------------------------------
%% @doc
%% Ensures location data is published on record creation.
%% Situations when this callback should be called are defined in get_hooks/0.
%% @end
%%--------------------------------------------------------------------
-spec handle_before(ModelName :: model_behaviour:model_type(),
    Method :: model_behaviour:model_action(),
    Context :: term()) -> ok | datastore:generic_error().
handle_before(_Model, create, [Doc]) ->
    claim_model(Doc);
handle_before(_Model, save, [Doc]) ->
    claim_model(Doc);
handle_before(_Model, create_or_update, [Doc, _Diff]) ->
    claim_model(Doc);
handle_before(_Model, _Method, _Context) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Ensures data in location service about what zones
%% contain data about user is up to date.
%% Situations when this callback should be called are defined in get_hooks/0.
%% @end
%%--------------------------------------------------------------------
-spec handle_after(ModelName :: model_behaviour:model_type(), Method :: model_behaviour:model_action(),
    Context :: term(), ReturnValue :: term()) -> ok | datastore:generic_error().
handle_after(_Model, _Method, _Context, _ReturnValue) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @private
%% Sets location data in dht.
%% @end
%%--------------------------------------------------------------------
-spec claim_model(#document{}) -> ok | {error, term()}.
claim_model(#document{key = ID, value = Value}) ->
    case locations:claim(element(1, Value), ID) of
        ok -> ok;
        Error ->
            ?error("Unable to claim ~p ~p due to ~p", [element(1, Value), ID, Error]),
            %% todo: {error, location_service_refused_id}
            ok
    end.
