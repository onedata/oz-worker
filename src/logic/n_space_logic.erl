%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc

%%% @end
%%%-------------------------------------------------------------------
-module(n_space_logic).
-author("Lukasz Opiola").
-behaviour(data_logic_behaviour).

-include_lib("ctool/include/logging.hrl").

-export([create/3, get/2]).

create(Issuer, Resource, Data) ->
    n_entity_logic:create(Issuer, n_space_logic_plugin, Resource, Data).

get(Issuer, Resource) ->
    n_entity_logic:get(Issuer, n_space_logic_plugin, Resource).

update(Issuer, SpaceId, Resource, Data) ->
    ok.

delete(Issuer, SpaceId, Resource) ->
    ok.
