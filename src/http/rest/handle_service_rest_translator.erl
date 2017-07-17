%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% handle service entities into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(handle_service_rest_translator).
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
response(create, _HServiceId, {deprecated_user_privileges, _UserId}, ok) ->
    rest_handler:ok_no_content_reply();
% TODO VFS-2918
response(create, _HServiceId, {deprecated_group_privileges, _GroupId}, ok) ->
    rest_handler:ok_no_content_reply();

response(create, undefined, entity, {ok, HServiceId}) ->
    rest_handler:created_reply([<<"handle_services">>, HServiceId]);

response(create, HServiceId, {user, UserId}, {ok, UserId}) ->
    rest_handler:created_reply(
        [<<"handle_services">>, HServiceId, <<"users">>, UserId]
    );

response(create, HServiceId, {group, GroupId}, {ok, GroupId}) ->
    rest_handler:created_reply(
        [<<"handle_services">>, HServiceId, <<"groups">>, GroupId]
    );

response(get, HServiceId, data, {ok, HServiceData}) ->
    rest_handler:ok_body_reply(HServiceData#{<<"handleServiceId">> => HServiceId});

response(get, undefined, list, {ok, HServices}) ->
    rest_handler:ok_body_reply(#{<<"handle_services">> => HServices});

response(get, _HServiceId, users, {ok, Users}) ->
    rest_handler:ok_body_reply(#{<<"users">> => Users});

response(get, _HServiceId, eff_users, {ok, Users}) ->
    rest_handler:ok_body_reply(#{<<"users">> => Users});

response(get, _HServiceId, {user, UserId}, {ok, User}) ->
    user_rest_translator:response(get, UserId, data, {ok, User});

response(get, _HServiceId, {eff_user, UserId}, {ok, User}) ->
    user_rest_translator:response(get, UserId, data, {ok, User});

response(get, _HServiceId, {user_privileges, _UserId}, {ok, Privileges}) ->
    rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _HServiceId, {eff_user_privileges, _UserId}, {ok, Privileges}) ->
    rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _HServiceId, groups, {ok, Groups}) ->
    rest_handler:ok_body_reply(#{<<"groups">> => Groups});

response(get, _HServiceId, eff_groups, {ok, Groups}) ->
    rest_handler:ok_body_reply(#{<<"groups">> => Groups});

response(get, _HServiceId, {group, GroupId}, {ok, Group}) ->
    group_rest_translator:response(get, GroupId, data, {ok, Group});

response(get, _HServiceId, {eff_group, GroupId}, {ok, Group}) ->
    group_rest_translator:response(get, GroupId, data, {ok, Group});

response(get, _HServiceId, {group_privileges, _GroupId}, {ok, Privileges}) ->
    rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _HServiceId, {eff_group_privileges, _GroupId}, {ok, Privileges}) ->
    rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _HServiceId, handles, {ok, Handles}) ->
    rest_handler:ok_body_reply(#{<<"handles">> => Handles});

response(get, _HServiceId, {handle, HandleId}, {ok, Handle}) ->
    handle_rest_translator:response(get, HandleId, data, {ok, Handle});


response(update, _HServiceId, _, ok) ->
    rest_handler:updated_reply();


response(delete, _HServiceId, _, ok) ->
    rest_handler:deleted_reply().