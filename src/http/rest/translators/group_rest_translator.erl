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

-include("http/rest.hrl").
-include("datastore/oz_datastore_models.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

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
create_response(#gri{id = undefined, aspect = instance}, AuthHint, resource, {#gri{id = GroupId}, _}) ->
    LocationTokens = case AuthHint of
        ?AS_USER(_UserId) ->
            [<<"user">>, <<"groups">>, GroupId];
        ?AS_GROUP(ChildGroupId) ->
            [<<"groups">>, ChildGroupId, <<"parents">>, GroupId];
        _ ->
            [<<"groups">>, GroupId]
    end,
    rest_translator:created_reply_with_location(LocationTokens);

create_response(#gri{aspect = join} = Gri, AuthHint, resource, Result) ->
    create_response(Gri#gri{aspect = instance}, AuthHint, resource, Result);

create_response(#gri{id = ParentGroupId, aspect = child}, _, resource, {#gri{id = GroupId}, _}) ->
    rest_translator:created_reply_with_location([<<"groups">>, ParentGroupId, <<"children">>, GroupId]);

create_response(#gri{aspect = invite_user_token}, _, value, Token) ->
    {ok, Serialized} = tokens:serialize(Token),
    rest_translator:ok_body_reply(#{<<"token">> => Serialized});

create_response(#gri{aspect = invite_group_token}, _, value, Token) ->
    {ok, Serialized} = tokens:serialize(Token),
    rest_translator:ok_body_reply(#{<<"token">> => Serialized});

create_response(#gri{id = GroupId, aspect = {user, UserId}}, _, resource, _) ->
    rest_translator:created_reply_with_location(
        [<<"groups">>, GroupId, <<"users">>, UserId]
    );

create_response(#gri{id = GroupId, aspect = {child, ChGrId}}, _, resource, _) ->
    rest_translator:created_reply_with_location(
        [<<"groups">>, GroupId, <<"children">>, ChGrId]
    ).


%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback get_response/2.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Resource :: term()) -> #rest_resp{}.
get_response(#gri{id = undefined, aspect = list}, Groups) ->
    rest_translator:ok_body_reply(#{<<"groups">> => Groups});

get_response(#gri{id = undefined, aspect = privileges}, Privileges) ->
    rest_translator:ok_body_reply(Privileges);

get_response(#gri{id = GroupId, aspect = instance, scope = _}, GroupData) ->
    % scope can be protected or shared
    #{<<"name">> := Name, <<"type">> := Type} = GroupData,
    rest_translator:ok_body_reply(#{
        <<"groupId">> => GroupId, <<"name">> => Name, <<"type">> => Type
    });

get_response(#gri{aspect = oz_privileges}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = eff_oz_privileges}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = parents}, Parents) ->
    rest_translator:ok_body_reply(#{<<"groups">> => Parents});

get_response(#gri{aspect = eff_parents}, Parents) ->
    rest_translator:ok_body_reply(#{<<"groups">> => Parents});

get_response(#gri{aspect = children}, Children) ->
    rest_translator:ok_body_reply(#{<<"groups">> => Children});

get_response(#gri{aspect = eff_children}, Children) ->
    rest_translator:ok_body_reply(#{<<"groups">> => Children});

get_response(#gri{aspect = {child_privileges, _ChildId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = {eff_child_privileges, _ChildId}}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = {eff_child_membership, _ChildId}}, Intermediaries) ->
    rest_translator:ok_encoded_intermediaries_reply(Intermediaries);

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
    rest_translator:ok_body_reply(#{<<"handles">> => Handles});

get_response(#gri{aspect = harvesters}, Harvesters) ->
    rest_translator:ok_body_reply(#{<<"harvesters">> => Harvesters});

get_response(#gri{aspect = eff_harvesters}, Harvesters) ->
    rest_translator:ok_body_reply(#{<<"harvesters">> => Harvesters});


get_response(#gri{aspect = clusters}, Clusters) ->
    rest_translator:ok_body_reply(#{<<"clusters">> => Clusters});

get_response(#gri{aspect = eff_clusters}, Clusters) ->
    rest_translator:ok_body_reply(#{<<"clusters">> => Clusters}).
