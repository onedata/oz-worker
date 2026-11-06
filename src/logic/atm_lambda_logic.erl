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
    create/2,
    parse_revision/2,
    dump/3
]).
-export([
    get/2,
    get_atm_inventories/2,
    get_atm_workflow_schemas/2,
    list/1
]).
-export([
    update/3,
    add_revision/4,
    update_revision_lifecycle_state/4,
    dump_revision/3
]).
-export([
    link_to_inventory/3,
    unlink_from_inventory/3
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


-spec parse_revision(aai:auth(), od_atm_lambda:name() | entity_logic:data()) ->
    {ok, atm_lambda_revision:record()} | errors:error().
parse_revision(Auth, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_atm_lambda, id = undefined, aspect = parse_revision, scope = public},
        data = Data
    })).


-spec dump(
    aai:auth(),
    od_atm_lambda:id(),
    entity_logic:data() | atm_lambda_revision:revision_number()
) ->
    {ok, json_utils:json_map()} | errors:error().
dump(Auth, AtmLambdaId, Revision) when is_integer(Revision) ->
    dump(Auth, AtmLambdaId, #{<<"includeRevision">> => Revision});
dump(Auth, AtmLambdaId, Data) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_atm_lambda, id = AtmLambdaId, aspect = dump},
        data = Data
    })).


-spec get(aai:auth(), od_atm_lambda:id()) ->
    {ok, od_atm_lambda:record()} | errors:error().
get(Auth, AtmLambdaId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_lambda, id = AtmLambdaId, aspect = instance}
    }).


-spec get_atm_inventories(aai:auth(), od_atm_lambda:id()) ->
    {ok, [od_atm_inventory:id()]} | errors:error().
get_atm_inventories(Auth, AtmLambdaId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_lambda, id = AtmLambdaId, aspect = atm_inventories}
    }).


-spec get_atm_workflow_schemas(aai:auth(), od_atm_lambda:id()) ->
    {ok, [od_atm_inventory:id()]} | errors:error().
get_atm_workflow_schemas(Auth, AtmLambdaId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_atm_lambda, id = AtmLambdaId, aspect = atm_workflow_schemas}
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


-spec add_revision(
    aai:auth(),
    od_atm_lambda:id(),
    atm_lambda_revision:revision_number() | binary(),
    entity_logic:data()
) ->
    ok | errors:error().
add_revision(Auth, AtmLambdaId, TargetRevisionNumber, Data) when is_integer(TargetRevisionNumber) ->
    add_revision(Auth, AtmLambdaId, integer_to_binary(TargetRevisionNumber), Data);
add_revision(Auth, AtmLambdaId, TargetRevisionNumberBin, Data) when is_binary(TargetRevisionNumberBin) ->
    entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{
            type = od_atm_lambda,
            id = AtmLambdaId,
            aspect = {revision, TargetRevisionNumberBin}
        },
        data = Data
    }).


-spec update_revision_lifecycle_state(
    aai:auth(),
    od_atm_lambda:id(),
    atm_lambda_revision:revision_number() | binary(),
    entity_logic:data()
) ->
    ok | errors:error().
update_revision_lifecycle_state(Auth, AtmLambdaId, RevisionNumber, Data) when is_integer(RevisionNumber) ->
    update_revision_lifecycle_state(Auth, AtmLambdaId, integer_to_binary(RevisionNumber), Data);
update_revision_lifecycle_state(Auth, AtmLambdaId, RevisionNumberBin, Data) when is_binary(RevisionNumberBin) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{
            type = od_atm_lambda,
            id = AtmLambdaId,
            aspect = {revision, RevisionNumberBin}
        },
        data = Data
    }).


-spec dump_revision(
    aai:auth(),
    od_atm_lambda:id(),
    atm_lambda_revision:revision_number() | binary()
) ->
    {ok, json_utils:json_map()} | errors:error().
dump_revision(Auth, AtmLambdaId, RevisionNumber) when is_integer(RevisionNumber) ->
    dump_revision(Auth, AtmLambdaId, integer_to_binary(RevisionNumber));
dump_revision(Auth, AtmLambdaId, RevisionNumberBin) when is_binary(RevisionNumberBin) ->
    ?CREATE_RETURN_DATA(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{
            type = od_atm_lambda,
            id = AtmLambdaId,
            aspect = {dump_revision, RevisionNumberBin}
        }
    })).


-spec link_to_inventory(aai:auth(), od_atm_lambda:id(), od_atm_inventory:id()) ->
    ok | errors:error().
link_to_inventory(Auth, AtmLambdaId, AtmInventoryId) ->
    entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_atm_lambda, id = AtmLambdaId, aspect = {atm_inventory, AtmInventoryId}}
    }).


-spec unlink_from_inventory(aai:auth(), od_atm_lambda:id(), od_atm_inventory:id()) ->
    ok | errors:error().
unlink_from_inventory(Auth, AtmLambdaId, AtmInventoryId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_atm_lambda, id = AtmLambdaId, aspect = {atm_inventory, AtmInventoryId}}
    }).


-spec exists(od_atm_lambda:id()) -> boolean().
exists(AtmLambdaId) ->
    {ok, Exists} = od_atm_lambda:exists(AtmLambdaId),
    Exists.

