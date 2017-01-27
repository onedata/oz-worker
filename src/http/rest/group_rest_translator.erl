%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% group entities into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(group_rest_translator).
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
response(create, _GroupId, {deprecated_user_privileges, _UserId}, ok) ->
    n_rest_handler:ok_no_content_reply();
% TODO VFS-2918
response(create, _GroupId, {deprecated_child_privileges, _ChildGroupId}, ok) ->
    n_rest_handler:ok_no_content_reply();

response(create, undefined, entity, {ok, GroupId}) ->
    n_rest_handler:created_reply([<<"groups">>, GroupId]);

response(create, _GroupId, invite_user_token, {ok, Macaroon}) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    n_rest_handler:ok_body_reply(#{<<"token">> => Token});

response(create, _GroupId, invite_group_token, {ok, Macaroon}) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    n_rest_handler:ok_body_reply(#{<<"token">> => Token});

response(create, GroupId, create_space, {ok, SpaceId}) ->
    n_rest_handler:created_reply(
        [<<"groups">>, GroupId, <<"spaces">>, SpaceId]
    );

response(create, GroupId, create_handle_service, {ok, HServiceId}) ->
    n_rest_handler:created_reply(
        [<<"groups">>, GroupId, <<"handle_services">>, HServiceId]
    );

response(create, GroupId, create_handle, {ok, HandleId}) ->
    n_rest_handler:created_reply(
        [<<"groups">>, GroupId, <<"handles">>, HandleId]
    );

response(create, GroupId, join_group, {ok, ParentGroupId}) ->
    n_rest_handler:created_reply(
        % TODO VFS-2918
%%        [<<"groups">>, GroupId, <<"parents">>, ParentGroupId]
        [<<"groups">>, GroupId, <<"nested">>, ParentGroupId]
    );

response(create, GroupId, join_space, {ok, SpaceId}) ->
    n_rest_handler:created_reply(
        [<<"groups">>, GroupId, <<"spaces">>, SpaceId]
    );

response(create, GroupId, {user, UserId}, {ok, UserId}) ->
    n_rest_handler:created_reply(
        [<<"groups">>, GroupId, <<"users">>, UserId]
    );

response(create, GroupId, {child, ChildGroupId}, {ok, ChildGroupId}) ->
    n_rest_handler:created_reply(
        [<<"groups">>, GroupId, <<"children">>, ChildGroupId]
    );


% TODO VFS-2918
response(get, _GroupId, deprecated_invite_user_token, {ok, Macaroon}) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    n_rest_handler:ok_body_reply(#{<<"token">> => Token});
% TODO VFS-2918
response(get, _GroupId, deprecated_invite_group_token, {ok, Macaroon}) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    n_rest_handler:ok_body_reply(#{<<"token">> => Token});


response(get, GroupId, data, {ok, GroupData}) ->
    n_rest_handler:ok_body_reply(GroupData#{<<"groupId">> => GroupId});


response(get, undefined, list, {ok, Groups}) ->
    n_rest_handler:ok_body_reply(#{<<"groups">> => Groups});

response(get, _GroupId, oz_privileges, {ok, Privileges}) ->
    n_rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _GroupId, eff_oz_privileges, {ok, Privileges}) ->
    n_rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _GroupId, users, {ok, Users}) ->
    n_rest_handler:ok_body_reply(#{<<"users">> => Users});

response(get, _GroupId, eff_users, {ok, Users}) ->
    n_rest_handler:ok_body_reply(#{<<"users">> => Users});

response(get, _GroupId, {user, UserId}, {ok, User}) ->
    user_rest_translator:response(get, UserId, data, {ok, User});

response(get, _GroupId, {eff_user, UserId}, {ok, User}) ->
    user_rest_translator:response(get, UserId, data, {ok, User});

response(get, _GroupId, {user_privileges, _UserId}, {ok, Privileges}) ->
    n_rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _GroupId, {eff_user_privileges, _UserId}, {ok, Privileges}) ->
    n_rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _GroupId, parents, {ok, Parents}) ->
    % TODO VFS-2918
%%    n_rest_handler:ok_body_reply(#{<<"parents">> => Parents});
    n_rest_handler:ok_body_reply(#{<<"parent_groups">> => Parents});

response(get, _GroupId, eff_parents, {ok, Parents}) ->
    n_rest_handler:ok_body_reply(#{<<"parents">> => Parents});

response(get, _GroupId, {parent, ParentId}, {ok, Parent}) ->
    response(get, ParentId, data, {ok, Parent});

response(get, _GroupId, {eff_parent, ParentId}, {ok, Parent}) ->
    response(get, ParentId, data, {ok, Parent});

response(get, _GroupId, children, {ok, Parents}) ->
    % TODO VFS-2918
%%    n_rest_handler:ok_body_reply(#{<<"children">> => Parents});
    n_rest_handler:ok_body_reply(#{<<"nested_groups">> => Parents});

response(get, _GroupId, eff_children, {ok, Parents}) ->
    n_rest_handler:ok_body_reply(#{<<"children">> => Parents});

response(get, _GroupId, {child, ChildId}, {ok, Child}) ->
    response(get, ChildId, data, {ok, Child});

response(get, _GroupId, {eff_child, ChildId}, {ok, Child}) ->
    response(get, ChildId, data, {ok, Child});

response(get, _GroupId, {child_privileges, _ChildId}, {ok, Privileges}) ->
    n_rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _GroupId, {eff_child_privileges, _ChildId}, {ok, Privileges}) ->
    n_rest_handler:ok_body_reply(#{<<"privileges">> => Privileges});

response(get, _GroupId, spaces, {ok, Spaces}) ->
    n_rest_handler:ok_body_reply(#{<<"spaces">> => Spaces});

response(get, _GroupId, eff_spaces, {ok, Spaces}) ->
    n_rest_handler:ok_body_reply(#{<<"spaces">> => Spaces});

response(get, _GroupId, {space, SpaceId}, {ok, Space}) ->
    space_rest_translator:response(get, SpaceId, data, {ok, Space});

response(get, _GroupId, {eff_space, SpaceId}, {ok, Space}) ->
    space_rest_translator:response(get, SpaceId, data, {ok, Space});

response(get, _GroupId, eff_providers, {ok, Providers}) ->
    n_rest_handler:ok_body_reply(#{<<"providers">> => Providers});

response(get, _GroupId, {eff_provider, ProviderId}, {ok, Provider}) ->
    provider_rest_translator:response(get, ProviderId, data, {ok, Provider});

response(get, _GroupId, handle_services, {ok, HServices}) ->
    n_rest_handler:ok_body_reply(#{<<"handle_services">> => HServices});

response(get, _GroupId, eff_handle_services, {ok, HServices}) ->
    n_rest_handler:ok_body_reply(#{<<"handle_services">> => HServices});

response(get, _GroupId, {handle_service, HServiceId}, {ok, HService}) ->
    handle_service_rest_translator:response(get, HServiceId, data, {ok, HService});

response(get, _GroupId, {eff_handle_service, HServiceId}, {ok, HService}) ->
    handle_service_rest_translator:response(get, HServiceId, data, {ok, HService});

response(get, _GroupId, handles, {ok, Handles}) ->
    n_rest_handler:ok_body_reply(#{<<"handles">> => Handles});

response(get, _GroupId, eff_handles, {ok, Handles}) ->
    n_rest_handler:ok_body_reply(#{<<"handles">> => Handles});

response(get, _GroupId, {handle, HandleId}, {ok, Handle}) ->
    handle_rest_translator:response(get, HandleId, data, {ok, Handle});

response(get, _GroupId, {eff_handle, HandleId}, {ok, Handle}) ->
    handle_rest_translator:response(get, HandleId, data, {ok, Handle});


response(update, _GroupId, _, ok) ->
    n_rest_handler:updated_reply();


response(delete, _GroupId, _, ok) ->
    n_rest_handler:deleted_reply().
