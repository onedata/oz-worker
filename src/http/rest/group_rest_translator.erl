%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% group entities into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(group_rest_translator).
-behaviour(rest_translator_behaviour).
-author("Lukasz Opiola").

-include("rest.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

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
create_response(#gri{id = undefined, aspect = instance}, AuthHint, {not_fetched, #gri{id = GroupId}}) ->
    LocationTokens = case AuthHint of
        ?AS_USER(_UserId) ->
            % TODO VFS-2918
%%        [<<"user">>, <<"groups">>, GroupId]
            [<<"groups">>, GroupId];
        ?AS_GROUP(GroupId) ->
            [<<"groups">>, GroupId, <<"parents">>, GroupId];
        _ ->
            [<<"groups">>, GroupId]
    end,
    rest_translator:created_reply(LocationTokens);
% TODO VFS-2918 Responses should be the same
%%    create_response(#gri{aspect = join}, AuthHint, #gri{id = GroupId}, Data);

create_response(#gri{aspect = join}, AuthHint, {not_fetched, #gri{id = ParentGroupId}}) ->
    LocationTokens = case AuthHint of
        ?AS_USER(_UserId) ->
            [<<"user">>, <<"groups">>, ParentGroupId];
        ?AS_GROUP(GroupId) ->
            % TODO VFS-2918
%%        [<<"groups">>, GroupId, <<"parents">>, ParentGroupId]
            [<<"groups">>, GroupId, <<"nested">>, ParentGroupId];
        _ ->
            [<<"groups">>, ParentGroupId]
    end,
    rest_translator:created_reply(LocationTokens);

create_response(#gri{aspect = invite_user_token}, _, {data, Macaroon}) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    rest_translator:ok_body_reply(#{<<"token">> => Token});

create_response(#gri{aspect = invite_group_token}, _, {data, Macaroon}) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    rest_translator:ok_body_reply(#{<<"token">> => Token});

create_response(#gri{id = GroupId, aspect = {user, UserId}}, _, {not_fetched, #gri{id = UserId}, _}) ->
    rest_translator:created_reply(
        [<<"groups">>, GroupId, <<"users">>, UserId]
    );

create_response(#gri{id = GroupId, aspect = {child, ChGrId}}, _, {not_fetched, #gri{id = ChGrId}, _}) ->
    rest_translator:created_reply(
        [<<"groups">>, GroupId, <<"children">>, ChGrId]
    ).


%%--------------------------------------------------------------------
%% @doc
%% Translates given entity logic GET result into REST response
%% expressed by #rest_resp{} record.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), entity_logic:get_result()) ->
    #rest_resp{}.
% TODO VFS-2918
get_response(#gri{aspect = deprecated_invite_user_token}, Macaroon) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    rest_translator:ok_body_reply(#{<<"token">> => Token});
% TODO VFS-2918
get_response(#gri{aspect = deprecated_invite_group_token}, Macaroon) ->
    {ok, Token} = token_utils:serialize62(Macaroon),
    rest_translator:ok_body_reply(#{<<"token">> => Token});

get_response(#gri{id = undefined, aspect = list}, Groups) ->
    rest_translator:ok_body_reply(#{<<"groups">> => Groups});

get_response(#gri{id = GroupId, aspect = instance, scope = _}, GroupData) ->
    % scope can be protected or shared
    rest_translator:ok_body_reply(GroupData#{<<"groupId">> => GroupId});

get_response(#gri{aspect = oz_privileges}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = eff_oz_privileges}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = users}, Users) ->
    rest_translator:ok_body_reply(#{<<"users">> => Users});

get_response(#gri{aspect = eff_users}, Users) ->
    rest_translator:ok_body_reply(#{<<"users">> => Users});

get_response(#gri{aspect = {user_privileges, _UserId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = {eff_user_privileges, _UserId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = parents}, Parents) ->
% TODO VFS-2918
%%    rest_translator:ok_body_reply(#{<<"parents">> => Parents});
    rest_translator:ok_body_reply(#{<<"parent_groups">> => Parents});

get_response(#gri{aspect = eff_parents}, Parents) ->
    rest_translator:ok_body_reply(#{<<"parents">> => Parents});


get_response(#gri{aspect = children}, Children) ->
% TODO VFS-2918
%%    rest_translator:ok_body_reply(#{<<"children">> => Children});
    rest_translator:ok_body_reply(#{<<"nested_groups">> => Children});

get_response(#gri{aspect = eff_children}, Children) ->
    rest_translator:ok_body_reply(#{<<"children">> => Children});

get_response(#gri{aspect = {child_privileges, _ChildId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = {eff_child_privileges, _ChildId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = spaces}, Spaces) ->
    rest_translator:ok_body_reply(#{<<"spaces">> => Spaces});

get_response(#gri{aspect = eff_spaces}, Spaces) ->
    rest_translator:ok_body_reply(#{<<"spaces">> => Spaces});

get_response(#gri{aspect = eff_providers}, Providers) ->
    rest_translator:ok_body_reply(#{<<"providers">> => Providers});


get_response(#gri{aspect = handle_services}, HServices) ->
    rest_translator:ok_body_reply(#{<<"handle_services">> => HServices});

get_response(#gri{aspect = eff_handle_services}, HServices) ->
    rest_translator:ok_body_reply(#{<<"handle_services">> => HServices});


get_response(#gri{aspect = handles}, Handles) ->
    rest_translator:ok_body_reply(#{<<"handles">> => Handles});

get_response(#gri{aspect = eff_handles}, Handles) ->
    rest_translator:ok_body_reply(#{<<"handles">> => Handles}).

