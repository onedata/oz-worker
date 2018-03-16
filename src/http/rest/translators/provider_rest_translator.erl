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

-include("rest.hrl").
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
create_response(#gri{id = undefined, aspect = instance}, _, {fetched, #gri{id = ProvId}, {_, Macaroon}}) ->
    {ok, MacaroonBin} = onedata_macaroons:serialize(Macaroon),
    rest_translator:ok_body_reply(#{
        <<"providerId">> => ProvId,
        <<"macaroon">> => MacaroonBin
    });

create_response(#gri{id = undefined, aspect = instance_dev}, _, {fetched, #gri{id = ProvId}, {_, Macaroon}}) ->
    {ok, MacaroonBin} = onedata_macaroons:serialize(Macaroon),
    rest_translator:ok_body_reply(#{
        <<"providerId">> => ProvId,
        <<"macaroon">> => MacaroonBin
    });

create_response(#gri{aspect = provider_registration_token}, _, {data, Macaroon}) ->
    {ok, Token} = onedata_macaroons:serialize(Macaroon),
    rest_translator:ok_body_reply(#{<<"token">> => Token});

create_response(#gri{aspect = support}, _, {not_fetched, #gri{id = SpaceId}}) ->
    rest_translator:created_reply([<<"provider">>, <<"spaces">>, SpaceId]);

create_response(#gri{aspect = check_my_ports}, _, {data, Body}) ->
    rest_translator:ok_body_reply(Body);

create_response(#gri{aspect = map_idp_group}, _, {data, GroupId}) ->
    rest_translator:ok_body_reply(#{
        <<"groupId">> => GroupId
    }).


%%--------------------------------------------------------------------
%% @doc
%% Translates given entity logic GET result into REST response
%% expressed by #rest_resp{} record.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), entity_logic:get_result()) ->
    #rest_resp{}.
get_response(#gri{id = undefined, aspect = list}, Providers) ->
    rest_translator:ok_body_reply(#{<<"providers">> => Providers});

get_response(#gri{id = ProviderId, aspect = instance, scope = protected}, ProviderData) ->
    rest_translator:ok_body_reply(ProviderData#{<<"providerId">> => ProviderId});

get_response(#gri{aspect = eff_users}, UserIds) ->
    rest_translator:ok_body_reply(#{<<"users">> => UserIds});

get_response(#gri{aspect = eff_groups}, GroupIds) ->
    rest_translator:ok_body_reply(#{<<"groups">> => GroupIds});

get_response(#gri{aspect = spaces}, SpaceIds) ->
    rest_translator:ok_body_reply(#{<<"spaces">> => SpaceIds});

get_response(#gri{aspect = {check_my_ip, _}}, IP) ->
    rest_translator:ok_body_reply(IP);

get_response(#gri{aspect = current_time}, Timestamp) ->
    rest_translator:ok_body_reply(#{<<"timeMillis">> => Timestamp}).