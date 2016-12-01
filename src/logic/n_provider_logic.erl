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
-module(n_provider_logic).
-author("Lukasz Opiola").
-behaviour(data_logic_behaviour).

-include_lib("ctool/include/logging.hrl").

-define(PLUGIN, n_provider_logic_plugin).

-export([create/1]).

-export([
    support_space/4, support_space/3
]).

-export([get/2]).

-export([update/3]).

-export([delete/2]).

create(Name) when is_binary(Name) ->
    create(#{<<"name">> => Name});
create(Data) ->
    n_entity_logic:create(nobody, ?PLUGIN, undefined, entity, Data).


support_space(Issuer, ProviderId, Token, SupportSize) ->
    support_space(Issuer, ProviderId, #{<<"token">> => Token, <<"size">> => SupportSize}).
support_space(Issuer, ProviderId, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, ProviderId, spaces, Data).


get(Issuer, ProviderId) ->
    n_entity_logic:get(Issuer, ?PLUGIN, entity, ProviderId).


update(Issuer, ProviderId, Data) ->
    n_entity_logic:update(Issuer, ?PLUGIN, ProviderId, entity, Data).

delete(Issuer, ProviderId) ->
    n_entity_logic:delete(Issuer, ?PLUGIN, ProviderId, entity).
