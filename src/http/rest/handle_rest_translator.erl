%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% handle entities into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(handle_rest_translator).
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
-spec response(Operation :: n_entity_logic:operation(),
    EntityId :: n_entity_logic:entity_id(), Resource :: n_entity_logic:resource(),
    Result :: n_entity_logic:result()) -> #rest_resp{}.
% TODO VFS-2918
response(create, _HandleId, {deprecated_user_privileges, _UserId}, ok) ->
    n_rest_handler:ok_no_content_reply();
% TODO VFS-2918
response(create, _HandleId, {deprecated_child_privileges, _GroupId}, ok) ->
    n_rest_handler:ok_no_content_reply();

response(create, undefined, entity, {ok, HandleId}) ->
    n_rest_handler:created_reply([<<"handles">>, HandleId]);

response(create, HandleId, users, {ok, UserId}) ->
    n_rest_handler:created_reply(
        [<<"handles">>, HandleId, <<"users">>, UserId]
    );

response(create, HandleId, groups, {ok, GroupId}) ->
    n_rest_handler:created_reply(
        [<<"handles">>, HandleId, <<"groups">>, GroupId]
    );

response(get, HandleId, data, {ok, HServiceData}) ->
    n_rest_handler:ok_body_reply(HServiceData#{<<"handleId">> => HandleId});

response(get, undefined, list, {ok, HServices}) ->
    n_rest_handler:ok_body_reply(#{<<"handles">> => HServices});

response(get, _HandleId, users, {ok, Users}) ->
    n_rest_handler:ok_body_reply(#{<<"users">> => Users});

response(get, _HandleId, eff_users, {ok, Users}) ->
    n_rest_handler:ok_body_reply(#{<<"users">> => Users});

response(get, _HandleId, {user, UserId}, {ok, User}) ->
    user_rest_translator:response(get, UserId, data, {ok, User});

response(get, _HandleId, {eff_user, UserId}, {ok, User}) ->
    user_rest_translator:response(get, UserId, data, {ok, User});

response(get, _HandleId, {user_privileges, _UserId}, {ok, Privileges}) ->
    n_rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _HandleId, {eff_user_privileges, _UserId}, {ok, Privileges}) ->
    n_rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _HandleId, groups, {ok, Groups}) ->
    n_rest_handler:ok_body_reply(#{<<"groups">> => Groups});

response(get, _HandleId, eff_groups, {ok, Groups}) ->
    n_rest_handler:ok_body_reply(#{<<"groups">> => Groups});

response(get, _HandleId, {group, GroupId}, {ok, Group}) ->
    group_rest_translator:response(get, GroupId, data, {ok, Group});

response(get, _HandleId, {eff_group, GroupId}, {ok, Group}) ->
    group_rest_translator:response(get, GroupId, data, {ok, Group});

response(get, _HandleId, {group_privileges, _GroupId}, {ok, Privileges}) ->
    n_rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _HandleId, {eff_group_privileges, _GroupId}, {ok, Privileges}) ->
    n_rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});


response(update, _HandleId, _, ok) ->
    n_rest_handler:updated_reply();


response(delete, _HandleId, _, ok) ->
    n_rest_handler:deleted_reply().