%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module handles translation of entity logic results concerning
%%% user entities into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(user_rest_translator).
-behaviour(rest_translator_behaviour).
-author("Lukasz Opiola").

-include("rest.hrl").
-include_lib("ctool/include/api_errors.hrl").


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
create_response(#gri{aspect = authorize}, _, {data, DischargeMacaroon}) ->
    rest_translator:ok_body_reply({binary, DischargeMacaroon});

create_response(#gri{aspect = client_tokens}, _, {fetched, _, Token}) ->
    rest_translator:ok_body_reply(#{<<"token">> => Token}).


%%--------------------------------------------------------------------
%% @doc
%% Translates given entity logic GET result into REST response
%% expressed by #rest_resp{} record.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), entity_logic:get_result()) ->
    #rest_resp{}.
get_response(#gri{id = undefined, aspect = list}, Users) ->
    rest_translator:ok_body_reply(#{<<"users">> => Users});

get_response(#gri{id = UserId, aspect = instance, scope = _}, UserData) ->
    % scope can be protected or shared
    Alias = gs_protocol:undefined_to_null(maps:get(<<"alias">>, UserData)),
    rest_translator:ok_body_reply(UserData#{
        <<"userId">> => UserId,
        <<"alias">> => Alias
    });

get_response(#gri{aspect = oz_privileges}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = eff_oz_privileges}, Privileges) ->
    rest_translator:ok_body_reply(#{<<"privileges">> => Privileges});

get_response(#gri{aspect = default_space}, DefaultSpace) ->
    rest_translator:ok_body_reply(#{<<"spaceId">> => DefaultSpace});

get_response(#gri{aspect = {space_alias, _}}, SpaceAlias) ->
    rest_translator:ok_body_reply(#{<<"alias">> => SpaceAlias});

get_response(#gri{aspect = default_provider}, DefaultProvider) ->
    rest_translator:ok_body_reply(#{<<"providerId">> => DefaultProvider});

get_response(#gri{aspect = client_tokens}, Tokens) ->
    rest_translator:ok_body_reply(#{<<"tokens">> => Tokens});

get_response(#gri{aspect = groups}, Groups) ->
    rest_translator:ok_body_reply(#{<<"groups">> => Groups});

get_response(#gri{aspect = eff_groups}, Groups) ->
    rest_translator:ok_body_reply(#{<<"groups">> => Groups});


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
