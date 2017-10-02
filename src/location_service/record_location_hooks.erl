%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2017 ACK CYFRONET AGH
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
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

-export([get_prehooks/0]).

%%--------------------------------------------------------------------
%% @doc
%% Returns hooks to be added.
%% These hooks ensure location data is published in location service.
%% @end
%%--------------------------------------------------------------------
-spec get_prehooks() -> [datastore_hooks:prehook()].
get_prehooks() ->
    case application:get_env(?APP_NAME, location_service_enabled) of
        {ok, true} -> [fun claim_model/2];
        {ok, false} -> []
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures location data is published on record creation.
%% @end
%%--------------------------------------------------------------------
-spec claim_model(Function :: atom(), Args :: list()) -> ok.
claim_model(create, [_Ctx, _Key, Doc]) ->
    claim_model(Doc);
claim_model(save, [_Ctx, _Key, Doc]) ->
    claim_model(Doc);
claim_model(update, [_Ctx, _Key, _Diff, Default]) ->
    claim_model(Default);
claim_model(_Function, _Args) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sets location data in dht.
%% @end
%%--------------------------------------------------------------------
-spec claim_model(#document{}) -> ok | {error, term()}.
claim_model(#document{key = ID, value = Value}) ->
    case locations:claim(element(1, Value), ID) of
        ok ->
            ok;
        Error ->
            ?error("Unable to claim ~p ~p due to ~p",
                [element(1, Value), ID, Error]),
            ok
    end.
