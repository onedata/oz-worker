%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% handle entities into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(handle_rest_translator).
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
create_response(#gri{id = undefined, aspect = instance}, AuthHint, {not_fetched, #gri{id = HandleId}}) ->
    LocationTokens = case AuthHint of
        ?AS_USER(_UserId) ->
            [<<"user">>, <<"handles">>, HandleId];
        ?AS_GROUP(GroupId) ->
            [<<"groups">>, GroupId, <<"handles">>, HandleId];
        _ ->
            [<<"handles">>, HandleId]
    end,
    rest_translator:created_reply(LocationTokens);

create_response(Gri=#gri{id = undefined, aspect = instance}, AuthHint, {fetched, #gri{id = HandleId}, _}) ->
    create_response(Gri, AuthHint, {not_fetched, #gri{id = HandleId}});

create_response(#gri{id = HandleId, aspect = {user, UserId}}, _, {not_fetched, #gri{id = UserId}, _}) ->
    rest_translator:created_reply(
        [<<"handles">>, HandleId, <<"users">>, UserId]
    );

create_response(#gri{id = HandleId, aspect = {group, GroupId}}, _, {not_fetched, #gri{id = GroupId}, _}) ->
    rest_translator:created_reply(
        [<<"handles">>, HandleId, <<"groups">>, GroupId]
    ).

%%--------------------------------------------------------------------
%% @doc
%% Translates given entity logic GET result into REST response
%% expressed by #rest_resp{} record.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), entity_logic:get_result()) ->
    #rest_resp{}.
get_response(#gri{id = undefined, aspect = list}, Handles) ->
    rest_translator:ok_body_reply(#{<<"handles">> => Handles});

get_response(#gri{id = HandleId, aspect = instance, scope = protected}, HandleData) ->
    Timestamp = maps:get(<<"timestamp">>, HandleData),
    % Replace "publicHandle" with "handle" key
    PublicHandle = maps:get(<<"publicHandle">>, HandleData),
    NewData = maps:remove(<<"publicHandle">>, HandleData),
    rest_translator:ok_body_reply(NewData#{
        <<"handleId">> => HandleId,
        <<"handle">> => PublicHandle,
        <<"timestamp">> => time_utils:datetime_to_datestamp(Timestamp)
    });

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
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges}).