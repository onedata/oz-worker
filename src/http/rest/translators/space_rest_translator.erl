%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% space entities into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(space_rest_translator).
-behaviour(rest_translator_behaviour).
-author("Lukasz Opiola").

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
create_response(#gri{id = undefined, aspect = instance}, AuthHint, resource, {#gri{id = SpaceId}, _}) ->
    LocationTokens = case AuthHint of
        ?AS_USER(_UserId) ->
            [<<"user">>, <<"spaces">>, SpaceId];
        ?AS_GROUP(GroupId) ->
            [<<"groups">>, GroupId, <<"spaces">>, SpaceId];
        _ ->
            [<<"spaces">>, SpaceId]
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

create_response(#gri{aspect = invite_provider_token}, _, value, Macaroon) ->
    {ok, Token} = macaroons:serialize(Macaroon),
    rest_translator:ok_body_reply(#{<<"token">> => Token});

create_response(#gri{id = SpaceId, aspect = {user, UserId}}, _, resource, _) ->
    rest_translator:created_reply(
        [<<"spaces">>, SpaceId, <<"users">>, UserId]
    );

create_response(#gri{id = SpaceId, aspect = {group, GroupId}}, _, resource, _) ->
    rest_translator:created_reply(
        [<<"spaces">>, SpaceId, <<"groups">>, GroupId]
    );

create_response(#gri{id = SpaceId, aspect = group}, _, resource, {#gri{id = GroupId}, _}) ->
    rest_translator:created_reply(
        [<<"spaces">>, SpaceId, <<"groups">>, GroupId]
    );

create_response(#gri{id = SpaceId, aspect = harvester}, _, resource, {#gri{id = HarvesterId}, _}) ->
    rest_translator:created_reply(
        [<<"spaces">>, SpaceId, <<"harvesters">>, HarvesterId]
    ).


%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback get_response/2.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Resource :: term()) -> #rest_resp{}.
get_response(#gri{id = undefined, aspect = list}, Spaces) ->
    rest_translator:ok_body_reply(#{<<"spaces">> => Spaces});

get_response(#gri{id = SpaceId, aspect = instance, scope = protected}, SpaceData) ->
    #{<<"name">> := Name, <<"providers">> := Providers} = SpaceData,
    rest_translator:ok_body_reply(#{
        <<"spaceId">> => SpaceId, <<"name">> => Name, <<"providers">> => Providers
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

get_response(#gri{aspect = shares}, Shares) ->
    rest_translator:ok_body_reply(#{<<"shares">> => Shares});

get_response(#gri{aspect = providers}, Providers) ->
    rest_translator:ok_body_reply(#{<<"providers">> => Providers});

get_response(#gri{aspect = harvesters}, Harvesters) ->
    rest_translator:ok_body_reply(#{<<"harvesters">> => Harvesters}).

