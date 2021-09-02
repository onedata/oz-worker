%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% atm_inventory entities into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(atm_inventory_rest_translator).
-behaviour(rest_translator_behaviour).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models.hrl").
-include("http/rest.hrl").

-export([create_response/4, get_response/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback create_response/4.
%% @end
%%--------------------------------------------------------------------
-spec create_response(entity_logic:gri(), entity_logic:auth_hint(),
    entity_logic:data_format(), Result :: term() | {entity_logic:gri(), term()} |
    {entity_logic:gri(), entity_logic:auth_hint(), term()}) -> rest_handler:rest_resp().
create_response(#gri{id = undefined, aspect = instance}, AuthHint, resource, {#gri{id = AtmInventoryId}, _, Rev}) ->
    create_response(#gri{id = undefined, aspect = instance}, AuthHint, resource, {#gri{id = AtmInventoryId}, Rev});
create_response(#gri{id = undefined, aspect = instance}, AuthHint, resource, {#gri{id = AtmInventoryId}, _}) ->
    LocationTokens = case AuthHint of
        ?AS_USER(_UserId) ->
            [<<"user">>, <<"atm_inventories">>, AtmInventoryId];
        ?AS_GROUP(GroupId) ->
            [<<"groups">>, GroupId, <<"atm_inventories">>, AtmInventoryId];
        _ ->
            [<<"atm_inventories">>, AtmInventoryId]
    end,
    rest_translator:created_reply_with_location(LocationTokens);

create_response(#gri{aspect = join} = Gri, AuthHint, resource, Result) ->
    create_response(Gri#gri{aspect = instance}, AuthHint, resource, Result);

create_response(#gri{id = AtmInventoryId, aspect = {user, UserId}}, _, resource, _) ->
    rest_translator:created_reply_with_location(
        [<<"atm_inventories">>, AtmInventoryId, <<"users">>, UserId]
    );

create_response(#gri{id = AtmInventoryId, aspect = {group, GroupId}}, _, resource, _) ->
    rest_translator:created_reply_with_location(
        [<<"atm_inventories">>, AtmInventoryId, <<"groups">>, GroupId]
    );

create_response(#gri{id = AtmInventoryId, aspect = group}, _, resource, {#gri{id = GroupId}, _}) ->
    rest_translator:created_reply_with_location(
        [<<"atm_inventories">>, AtmInventoryId, <<"groups">>, GroupId]
    ).

%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback get_response/2.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Resource :: term()) -> rest_handler:rest_resp().
get_response(#gri{id = undefined, aspect = list}, AtmInventories) ->
    rest_translator:ok_body_reply(#{<<"atm_inventories">> => AtmInventories});

get_response(#gri{id = undefined, aspect = privileges}, Privileges) ->
    rest_translator:ok_body_reply(Privileges);

get_response(#gri{id = AtmInventoryId, aspect = instance, scope = protected}, AtmInventoryData) ->
    #{
        <<"name">> := Name,
        <<"creator">> := Creator,
        <<"creationTime">> := CreationTime
    } = AtmInventoryData,
    rest_translator:ok_body_reply(#{
        <<"atmInventoryId">> => AtmInventoryId,
        <<"name">> => Name,
        <<"creator">> => aai:subject_to_json(utils:ensure_defined(Creator, undefined, ?SUB(nobody))),
        <<"creationTime">> => CreationTime
    });

get_response(#gri{aspect = users}, Users) ->
    rest_translator:ok_body_reply(#{<<"users">> => Users});

get_response(#gri{aspect = eff_users}, Users) ->
    rest_translator:ok_body_reply(#{<<"users">> => Users});

get_response(#gri{aspect = {user_privileges, _UserId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = {eff_user_privileges, _UserId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = {eff_user_membership, _UserId}}, Intermediaries) ->
    rest_translator:ok_encoded_intermediaries_reply(Intermediaries);

get_response(#gri{aspect = groups}, Groups) ->
    rest_translator:ok_body_reply(#{<<"groups">> => Groups});

get_response(#gri{aspect = eff_groups}, Groups) ->
    rest_translator:ok_body_reply(#{<<"groups">> => Groups});

get_response(#gri{aspect = {group_privileges, _GroupId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = {eff_group_privileges, _GroupId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = {eff_group_membership, _GroupId}}, Intermediaries) ->
    rest_translator:ok_encoded_intermediaries_reply(Intermediaries);

get_response(#gri{aspect = atm_lambdas}, AtmLambdas) ->
    rest_translator:ok_body_reply(#{<<"atm_lambdas">> => AtmLambdas});

get_response(#gri{aspect = atm_workflow_schemas}, AtmWorkflowSchemas) ->
    rest_translator:ok_body_reply(#{<<"atm_workflow_schemas">> => AtmWorkflowSchemas}).
