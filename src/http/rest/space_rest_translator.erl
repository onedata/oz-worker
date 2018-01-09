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

-include("rest.hrl").

-export([create_response/3, get_response/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Translates given entity logic CREATE result into REST response
%% expressed by #rest_resp{} record. GRI holds the #gri{} od the request,
%% new GRI holds the #gri{} of new aspect that was created.
%% @end
%%--------------------------------------------------------------------
-spec create_response(entity_logic:gri(), entity_logic:auth_hint(),
    Result :: {data, term()} | {fetched, entity_logic:gri(), term()} |
    {not_fetched, entity_logic:gri()} |
    {not_fetched, entity_logic:gri(), entity_logic:auth_hint()}) -> #rest_resp{}.
create_response(#gri{id = undefined, aspect = instance}, AuthHint, {not_fetched, #gri{id = SpaceId}}) ->
    LocationTokens = case AuthHint of
        ?AS_USER(_UserId) ->
            [<<"user">>, <<"spaces">>, SpaceId];
        ?AS_GROUP(GroupId) ->
            [<<"groups">>, GroupId, <<"spaces">>, SpaceId];
        _ ->
            [<<"spaces">>, SpaceId]
    end,
    rest_translator:created_reply(LocationTokens);

create_response(#gri{aspect = join} = Gri, AuthHint, Result) ->
    create_response(Gri#gri{aspect = instance}, AuthHint, Result);

create_response(#gri{aspect = invite_user_token}, _, {data, Macaroon}) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    rest_translator:ok_body_reply(#{<<"token">> => Token});

create_response(#gri{aspect = invite_group_token}, _, {data, Macaroon}) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    rest_translator:ok_body_reply(#{<<"token">> => Token});

create_response(#gri{aspect = invite_provider_token}, _, {data, Macaroon}) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    rest_translator:ok_body_reply(#{<<"token">> => Token});

create_response(#gri{id = SpaceId, aspect = {user, UserId}}, _, {not_fetched, #gri{id = UserId}, _}) ->
    rest_translator:created_reply(
        [<<"spaces">>, SpaceId, <<"users">>, UserId]
    );

create_response(#gri{id = SpaceId, aspect = {group, GrId}}, _, {not_fetched, #gri{id = GrId}, _}) ->
    rest_translator:created_reply(
        [<<"spaces">>, SpaceId, <<"groups">>, GrId]
    ).


%%--------------------------------------------------------------------
%% @doc
%% Translates given entity logic GET result into REST response
%% expressed by #rest_resp{} record.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), entity_logic:get_result()) ->
    #rest_resp{}.
get_response(#gri{id = undefined, aspect = list}, Spaces) ->
    rest_translator:ok_body_reply(#{<<"spaces">> => Spaces});

get_response(#gri{id = SpaceId, aspect = instance, scope = _}, SpaceData) ->
    % scope can be protected or shared
    rest_translator:ok_body_reply(SpaceData#{<<"spaceId">> => SpaceId});

get_response(#gri{aspect = users}, Users) ->
    rest_translator:ok_body_reply(#{<<"users">> => Users});

get_response(#gri{aspect = eff_users}, Users) ->
    rest_translator:ok_body_reply(#{<<"users">> => Users});

get_response(#gri{aspect = {user_privileges, _UserId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = {eff_user_privileges, _UserId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = groups}, Groups) ->
    rest_translator:ok_body_reply(#{<<"groups">> => Groups});

get_response(#gri{aspect = eff_groups}, Groups) ->
    rest_translator:ok_body_reply(#{<<"groups">> => Groups});

get_response(#gri{aspect = {group_privileges, _GroupId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = {eff_group_privileges, _GroupId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = shares}, Shares) ->
    rest_translator:ok_body_reply(#{<<"shares">> => Shares});

get_response(#gri{aspect = providers}, Providers) ->
    rest_translator:ok_body_reply(#{<<"providers">> => Providers}).

