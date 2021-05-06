%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all Automation Lambda logic functionality.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_lambda_logic).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/logging.hrl").

-export([
    create/2
]).
-export([
    get/2,
    get_atm_inventories/2,
    list/1
]).
-export([
    update/3
]).
-export([
    % @TODO VFS-7596 Temporary solution -> implement lambda removal with reference count checking
    delete/2
]).
-export([
    add_to_inventory/3
]).

-export([
    exists/1
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec create(aai:auth(), od_atm_lambda:name() | entity_logic:data()) ->
    {ok, od_atm_lambda:id()} | errors:error().
create(Auth, Name) when is_binary(Name) ->
    create(Auth, #{<<"name">> => Name});
create(Auth, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_atm_lambda, id = undefined, aspect = instance},
        data = Data
    })).


%% Only for admins - regular users must use the atm_inventory_logic:get_atm_lambda/3
-spec get(aai:auth(), od_atm_lambda:id()) ->
    {ok, od_atm_lambda:record()} | errors:error().
get(Auth, AtmLambdaId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_lambda, id = AtmLambdaId, aspect = instance}
    }).


%% Only for admins - regular users must use the atm_inventory_logic:get_atm_lambda_atm_inventories/3
-spec get_atm_inventories(aai:auth(), od_atm_lambda:id()) ->
    {ok, [od_atm_inventory:id()]} | errors:error().
get_atm_inventories(Auth, AtmLambdaId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_lambda, id = AtmLambdaId, aspect = atm_inventories}
    }).


%% Only for admins
-spec list(aai:auth()) ->
    {ok, [od_atm_lambda:id()]} | errors:error().
list(Auth) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_lambda, id = undefined, aspect = list}
    }).


-spec update(aai:auth(), od_atm_lambda:id(), entity_logic:data()) ->
    ok | errors:error().
update(Auth, AtmLambdaId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_atm_lambda, id = AtmLambdaId, aspect = instance},
        data = Data
    }).


-spec delete(Auth :: aai:auth(), od_atm_lambda:id()) ->
    ok | errors:error().
delete(Auth, AtmLambdaId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_atm_lambda, id = AtmLambdaId, aspect = instance}
    }).


-spec add_to_inventory(aai:auth(), od_atm_lambda:id(), od_atm_inventory:id()) ->
    ok | errors:error().
add_to_inventory(Auth, AtmLambdaId, AtmInventoryId) ->
    entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_atm_lambda, id = AtmLambdaId, aspect = {atm_inventory, AtmInventoryId}}
    }).


-spec exists(od_atm_lambda:id()) -> boolean().
exists(AtmLambdaId) ->
    {ok, Exists} = od_atm_lambda:exists(AtmLambdaId),
    Exists.

