%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% harvester entities into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(harvester_rest_translator).
-behaviour(rest_translator_behaviour).
-author("Michal Stanisz").

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
    {entity_logic:gri(), entity_logic:auth_hint(), term()}) -> #rest_resp{}.
create_response(#gri{id = undefined, aspect = instance}, AuthHint, resource, {#gri{id = HarvesterId}, _}) ->
    LocationTokens = case AuthHint of
        ?AS_USER(_UserId) ->
            [<<"user">>, <<"harvesters">>, HarvesterId];
        ?AS_GROUP(GroupId) ->
            [<<"groups">>, GroupId, <<"harvesters">>, HarvesterId];
        ?AS_SPACE(SpaceId) ->
            [<<"spaces">>, SpaceId, <<"harvesters">>, HarvesterId];
        _ ->
            [<<"harvesters">>, HarvesterId]
    end,
    rest_translator:created_reply(LocationTokens);

create_response(#gri{aspect = join} = Gri, AuthHint, resource, Result) ->
    create_response(Gri#gri{aspect = instance}, AuthHint, resource, Result);

create_response(#gri{aspect = invite_user_token}, _, value, Macaroon) ->
    {ok, Token} = macaroons:serialize(Macaroon),
    rest_translator:ok_body_reply(#{<<"token">> => Token});

create_response(#gri{aspect = invite_group_token}, _, value, Macaroon) ->
    {ok, Token} = macaroons:serialize(Macaroon),
    rest_translator:ok_body_reply(#{<<"token">> => Token});

create_response(#gri{aspect = invite_space_token}, _, value, Macaroon) ->
    {ok, Token} = macaroons:serialize(Macaroon),
    rest_translator:ok_body_reply(#{<<"token">> => Token});

create_response(#gri{id = HarvesterId, aspect = {user, UserId}}, _, resource, _) ->
    rest_translator:created_reply(
        [<<"harvesters">>, HarvesterId, <<"users">>, UserId]
    );

create_response(#gri{id = HarvesterId, aspect = {group, GroupId}}, _, resource, _) ->
    rest_translator:created_reply(
        [<<"harvesters">>, HarvesterId, <<"groups">>, GroupId]
    );

create_response(#gri{id = HarvesterId, aspect = group}, _, resource, {#gri{id = GroupId}, _}) ->
    rest_translator:created_reply(
        [<<"harvesters">>, HarvesterId, <<"groups">>, GroupId]
    );

create_response(#gri{id = HarvesterId, aspect = {space, SpaceId}}, _, resource, _) ->
    rest_translator:created_reply(
        [<<"harvesters">>, HarvesterId, <<"spaces">>, SpaceId]
    );

create_response(#gri{id = HarvesterId, aspect = index}, _, resource, {#gri{aspect = {index, IndexId}}, _}) ->
    rest_translator:created_reply(
        [<<"harvesters">>, HarvesterId, <<"indices">>, IndexId]
    );

create_response(#gri{aspect = {query, _}}, _, value, Response) ->
    rest_translator:ok_body_reply(Response).

%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback get_response/2.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Resource :: term()) -> #rest_resp{}.
get_response(#gri{id = undefined, aspect = list}, Harvesters) ->
    rest_translator:ok_body_reply(#{<<"harvesters">> => Harvesters});

get_response(#gri{id = undefined, aspect = privileges}, Privileges) ->
    rest_translator:ok_body_reply(Privileges);

get_response(#gri{id = HarvesterId, aspect = instance, scope = protected}, HarvesterData) ->
    #{
        <<"name">> := Name,
        <<"public">> := Public,
        <<"plugin">> := Plugin,
        <<"endpoint">> := Endpoint
    } = HarvesterData,
    rest_translator:ok_body_reply(#{
        <<"harvesterId">> => HarvesterId,
        <<"name">> => Name,
        <<"public">> => Public,
        <<"plugin">> => Plugin,
        <<"endpoint">> => Endpoint
    });

get_response(#gri{aspect = indices}, Indices) ->
    rest_translator:ok_body_reply(#{<<"indices">> => Indices});

get_response(#gri{aspect = {index, IndexId}}, IndexData) ->
    #{
        <<"name">> := Name,
        <<"schema">> := Schema,
        <<"guiPluginName">> := GuiPluginName
    } = IndexData,
    rest_translator:ok_body_reply(#{
        <<"indexId">> => IndexId,
        <<"name">> => Name,
        <<"schema">> => gs_protocol:undefined_to_null(Schema),
        <<"guiPluginName">> => gs_protocol:undefined_to_null(GuiPluginName)
    });

get_response(#gri{aspect = {index_stats, _}}, IndexStats) ->
    rest_translator:ok_body_reply(IndexStats);

get_response(#gri{aspect = gui_plugin_config}, Config) ->
    rest_translator:ok_body_reply(#{<<"guiPluginConfig">> => Config});

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

get_response(#gri{aspect = spaces}, Spaces) ->
    rest_translator:ok_body_reply(#{<<"spaces">> => Spaces}).


