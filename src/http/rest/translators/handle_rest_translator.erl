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
create_response(#gri{id = undefined, aspect = instance}, AuthHint, resource, {#gri{id = HandleId}, _}) ->
    LocationTokens = case AuthHint of
        ?AS_USER(_UserId) ->
            [<<"user">>, <<"handles">>, HandleId];
        ?AS_GROUP(GroupId) ->
            [<<"groups">>, GroupId, <<"handles">>, HandleId];
        _ ->
            [<<"handles">>, HandleId]
    end,
    rest_translator:created_reply(LocationTokens);

create_response(#gri{id = HandleId, aspect = {user, UserId}}, _, resource, _) ->
    rest_translator:created_reply(
        [<<"handles">>, HandleId, <<"users">>, UserId]
    );

create_response(#gri{id = HandleId, aspect = {group, GroupId}}, _, resource, _) ->
    rest_translator:created_reply(
        [<<"handles">>, HandleId, <<"groups">>, GroupId]
    ).

%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback get_response/2.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Resource :: term()) -> #rest_resp{}.
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