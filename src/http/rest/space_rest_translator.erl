%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
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
-include("errors.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

-export([response/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Translates given entity logic result into REST response
%% expressed by #rest_resp{} record.
%% @end
%%--------------------------------------------------------------------
-spec response(Operation :: entity_logic:operation(),
    EntityId :: entity_logic:entity_id(), Resource :: entity_logic:resource(),
    Result :: entity_logic:result()) -> #rest_resp{}.
% TODO VFS-2918
response(create, _SpaceId, {deprecated_create_share, ShareId}, {ok, ShareId}) ->
    n_rest_handler:ok_no_content_reply();
% TODO VFS-2918
response(create, _SpaceId, {deprecated_user_privileges, _UserId}, ok) ->
    n_rest_handler:ok_no_content_reply();
% TODO VFS-2918
response(create, _SpaceId, {deprecated_group_privileges, _GroupId}, ok) ->
    n_rest_handler:ok_no_content_reply();

response(create, undefined, entity, {ok, SpaceId}) ->
    n_rest_handler:created_reply([<<"spaces">>, SpaceId]);

response(create, _SpaceId, invite_user_token, {ok, Macaroon}) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    n_rest_handler:ok_body_reply(#{<<"token">> => Token});

response(create, _SpaceId, invite_group_token, {ok, Macaroon}) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    n_rest_handler:ok_body_reply(#{<<"token">> => Token});

response(create, _SpaceId, invite_provider_token, {ok, Macaroon}) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    n_rest_handler:ok_body_reply(#{<<"token">> => Token});

response(create, SpaceId, {user, UserId}, {ok, UserId}) ->
    n_rest_handler:created_reply(
        [<<"spaces">>, SpaceId, <<"users">>, UserId]
    );

response(create, SpaceId, {group, GroupId}, {ok, GroupId}) ->
    n_rest_handler:created_reply(
        [<<"spaces">>, SpaceId, <<"groups">>, GroupId]
    );

% TODO VFS-2918
response(get, _SpaceId, deprecated_invite_user_token, {ok, Macaroon}) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    n_rest_handler:ok_body_reply(#{<<"token">> => Token});
% TODO VFS-2918
response(get, _SpaceId, deprecated_invite_group_token, {ok, Macaroon}) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    n_rest_handler:ok_body_reply(#{<<"token">> => Token});
% TODO VFS-2918
response(get, _SpaceId, deprecated_invite_provider_token, {ok, Macaroon}) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    n_rest_handler:ok_body_reply(#{<<"token">> => Token});

response(get, SpaceId, data, {ok, SpaceData}) ->
    n_rest_handler:ok_body_reply(SpaceData#{<<"spaceId">> => SpaceId});

response(get, undefined, list, {ok, Spaces}) ->
    n_rest_handler:ok_body_reply(#{<<"spaces">> => Spaces});

response(get, _SpaceId, users, {ok, Users}) ->
    n_rest_handler:ok_body_reply(#{<<"users">> => Users});

response(get, _SpaceId, eff_users, {ok, Users}) ->
    n_rest_handler:ok_body_reply(#{<<"users">> => Users});

response(get, _SpaceId, {user, UserId}, {ok, User}) ->
    user_rest_translator:response(get, UserId, data, {ok, User});

response(get, _SpaceId, {eff_user, UserId}, {ok, User}) ->
    user_rest_translator:response(get, UserId, data, {ok, User});

response(get, _SpaceId, {user_privileges, _UserId}, {ok, Privileges}) ->
    n_rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _SpaceId, {eff_user_privileges, _UserId}, {ok, Privileges}) ->
    n_rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _SpaceId, groups, {ok, Groups}) ->
    n_rest_handler:ok_body_reply(#{<<"groups">> => Groups});

response(get, _SpaceId, eff_groups, {ok, Groups}) ->
    n_rest_handler:ok_body_reply(#{<<"groups">> => Groups});

response(get, _SpaceId, {group, GroupId}, {ok, Group}) ->
    group_rest_translator:response(get, GroupId, data, {ok, Group});

response(get, _SpaceId, {eff_group, GroupId}, {ok, Group}) ->
    group_rest_translator:response(get, GroupId, data, {ok, Group});

response(get, _SpaceId, {group_privileges, _GroupId}, {ok, Privileges}) ->
    n_rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _SpaceId, {eff_group_privileges, _GroupId}, {ok, Privileges}) ->
    n_rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _SpaceId, shares, {ok, Shares}) ->
    n_rest_handler:ok_body_reply(#{<<"shares">> => Shares});

response(get, _SpaceId, {share, ShareId}, {ok, Share}) ->
    share_rest_translator:response(get, ShareId, data, {ok, Share});

response(get, _SpaceId, providers, {ok, Providers}) ->
    n_rest_handler:ok_body_reply(#{<<"providers">> => Providers});

response(get, _SpaceId, {provider, ProviderId}, {ok, Provider}) ->
    provider_rest_translator:response(get, ProviderId, data, {ok, Provider});


response(update, _SpaceId, _, ok) ->
    n_rest_handler:updated_reply();


response(delete, _SpaceId, _, ok) ->
    n_rest_handler:deleted_reply().

