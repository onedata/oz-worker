%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(subscribers).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([providers/3]).

providers(_Seq, Doc, space) ->
    #document{value = Value} = Doc,
    #space{providers = SpaceProviders} = Value,
    SpaceProviders;

providers(_Seq, _Doc, user_group) ->
    [];

providers(_Seq, _Doc, onedata_user) ->
    [];

providers(_Seq, _Doc, _Type) ->
    [].

