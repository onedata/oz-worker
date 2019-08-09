%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% provider entities into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(provider_rest_translator).
-behaviour(rest_translator_behaviour).
-author("Lukasz Opiola").

-include("http/rest.hrl").
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
create_response(#gri{id = undefined, aspect = instance}, _, resource, {#gri{id = ProvId}, {{_, Token}, _Rev}}) ->
    {ok, Serialized} = tokens:serialize(Token),

    rest_translator:ok_body_reply(#{
        <<"providerId">> => ProvId,
        <<"providerRootToken">> => Serialized,
        % @todo VFS-5554 Deprecated, for backward compatibility
        <<"macaroon">> => Serialized
    });

create_response(#gri{id = undefined, aspect = instance_dev}, _, resource, {#gri{id = ProvId}, {{_, Token}, _Rev}}) ->
    {ok, Serialized} = tokens:serialize(Token),
    rest_translator:ok_body_reply(#{
        <<"providerId">> => ProvId,
        <<"providerRootToken">> => Serialized,
        % @todo VFS-5554 Deprecated, for backward compatibility
        <<"macaroon">> => Serialized
    });

create_response(#gri{aspect = provider_registration_token}, _, value, Token) ->
    {ok, Serialized} = macaroons:serialize(Token),
    rest_translator:ok_body_reply(#{<<"token">> => Serialized});

create_response(#gri{aspect = support}, _, resource, {#gri{id = SpaceId}, _}) ->
    rest_translator:created_reply([<<"provider">>, <<"spaces">>, SpaceId]);

create_response(#gri{aspect = check_my_ports}, _, value, Value) ->
    rest_translator:ok_body_reply(Value);

create_response(#gri{aspect = map_idp_group}, _, value, GroupId) ->
    rest_translator:ok_body_reply(#{
        <<"groupId">> => GroupId
    }).


%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback get_response/2.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Resource :: term()) -> #rest_resp{}.
get_response(#gri{id = undefined, aspect = list}, Providers) ->
    rest_translator:ok_body_reply(#{<<"providers">> => Providers});

get_response(#gri{id = ProviderId, aspect = instance, scope = protected}, ProviderData) ->
    #{
        <<"name">> := Name, <<"domain">> := Domain,
        <<"latitude">> := Latitude, <<"longitude">> := Longitude,
        <<"online">> := Online
    } = ProviderData,
    rest_translator:ok_body_reply(#{
        <<"providerId">> => ProviderId,
        <<"name">> => Name, <<"domain">> => Domain,
        <<"latitude">> => Latitude, <<"longitude">> => Longitude,
        <<"online">> => Online
    });

get_response(#gri{aspect = domain_config}, #{<<"ipList">> := IPList} = DomainConfig) ->
    IPBinaries = [list_to_binary(inet:ntoa(IP)) || IP <- IPList],
    rest_translator:ok_body_reply(DomainConfig#{<<"ipList">> := IPBinaries});

get_response(#gri{aspect = {user_spaces, _}}, SpaceIds) ->
    rest_translator:ok_body_reply(#{<<"spaces">> => SpaceIds});

get_response(#gri{aspect = {group_spaces, _}}, SpaceIds) ->
    rest_translator:ok_body_reply(#{<<"spaces">> => SpaceIds});

get_response(#gri{aspect = eff_users}, UserIds) ->
    rest_translator:ok_body_reply(#{<<"users">> => UserIds});

get_response(#gri{aspect = {eff_user_membership, _UserId}}, Intermediaries) ->
    rest_translator:ok_encoded_intermediaries_reply(Intermediaries);

get_response(#gri{aspect = eff_groups}, GroupIds) ->
    rest_translator:ok_body_reply(#{<<"groups">> => GroupIds});

get_response(#gri{aspect = {eff_group_membership, _GroupId}}, Intermediaries) ->
    rest_translator:ok_encoded_intermediaries_reply(Intermediaries);

get_response(#gri{aspect = spaces}, SpaceIds) ->
    rest_translator:ok_body_reply(#{<<"spaces">> => SpaceIds});

get_response(#gri{aspect = {check_my_ip, _}}, IP) ->
    rest_translator:ok_body_reply(IP);

get_response(#gri{aspect = current_time}, Timestamp) ->
    rest_translator:ok_body_reply(#{<<"timeMillis">> => Timestamp}).