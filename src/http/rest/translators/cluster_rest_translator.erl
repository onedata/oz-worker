%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% cluster entities into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_rest_translator).
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
create_response(#gri{id = undefined, aspect = join}, AuthHint, resource, {#gri{id = ClusterId}, _}) ->
    LocationTokens = case AuthHint of
        ?AS_USER(_UserId) ->
            [<<"user">>, <<"clusters">>, ClusterId];
        ?AS_GROUP(GroupId) ->
            [<<"groups">>, GroupId, <<"clusters">>, ClusterId];
        _ ->
            [<<"clusters">>, ClusterId]
    end,
    rest_translator:created_reply(LocationTokens);

create_response(#gri{aspect = invite_user_token}, _, value, Macaroon) ->
    {ok, Token} = onedata_macaroons:serialize(Macaroon),
    rest_translator:ok_body_reply(#{<<"token">> => Token});

create_response(#gri{aspect = invite_group_token}, _, value, Macaroon) ->
    {ok, Token} = onedata_macaroons:serialize(Macaroon),
    rest_translator:ok_body_reply(#{<<"token">> => Token});

create_response(#gri{id = ClusterId, aspect = {user, UserId}}, _, resource, _) ->
    rest_translator:created_reply(
        [<<"clusters">>, ClusterId, <<"users">>, UserId]
    );

create_response(#gri{id = ClusterId, aspect = {group, GroupId}}, _, resource, _) ->
    rest_translator:created_reply(
        [<<"clusters">>, ClusterId, <<"groups">>, GroupId]
    );

create_response(#gri{id = ClusterId, aspect = group}, _, resource, {#gri{id = GroupId}, _}) ->
    rest_translator:created_reply(
        [<<"clusters">>, ClusterId, <<"groups">>, GroupId]
    ).


%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback get_response/2.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Resource :: term()) -> #rest_resp{}.
get_response(#gri{id = undefined, aspect = list}, Clusters) ->
    rest_translator:ok_body_reply(#{<<"clusters">> => Clusters});

get_response(#gri{id = ClusterId, aspect = instance, scope = protected}, ClusterData) ->
    #{
        <<"type">> := Type,
        <<"serviceId">> := ServiceId,
        <<"workerVersion">> := WorkerVersion,
        <<"onepanelVersion">> := OnepanelVersion,
        <<"onepanelProxy">> := OnepanelProxy
    } = ClusterData,
    rest_translator:ok_body_reply(#{
        <<"clusterId">> => ClusterId,
        <<"type">> => Type,
        <<"serviceId">> => ServiceId,
        <<"workerVersion">> => WorkerVersion,
        <<"onepanelVersion">> => OnepanelVersion,
        <<"onepanelProxy">> => OnepanelProxy
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
    rest_translator:ok_encoded_intermediaries_reply(Intermediaries).

