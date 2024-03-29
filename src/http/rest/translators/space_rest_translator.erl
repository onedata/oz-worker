%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
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

-include("http/rest.hrl").
-include("datastore/oz_datastore_models.hrl").

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
    {entity_logic:gri(), entity_logic:auth_hint(), term()}) -> rest_handler:rest_resp().
create_response(#gri{id = undefined, aspect = instance}, AuthHint, resource, {#gri{id = SpaceId}, _}) ->
    LocationTokens = case AuthHint of
        ?AS_USER(_UserId) ->
            [<<"user">>, <<"spaces">>, SpaceId];
        ?AS_GROUP(GroupId) ->
            [<<"groups">>, GroupId, <<"spaces">>, SpaceId];
        ?AS_HARVESTER(HarvesterId) ->
            [<<"harvesters">>, HarvesterId, <<"spaces">>, SpaceId];
        _ ->
            [<<"spaces">>, SpaceId]
    end,
    rest_translator:created_reply_with_location(LocationTokens);

create_response(#gri{aspect = join} = Gri, AuthHint, resource, Result) ->
    create_response(Gri#gri{aspect = instance}, AuthHint, resource, Result);

create_response(#gri{aspect = membership_request}, _, value, RequestId) ->
    rest_translator:ok_body_reply(#{<<"requestId">> => RequestId});

create_response(#gri{aspect = invite_user_token}, _, value, Token) ->
    {ok, Serialized} = tokens:serialize(Token),
    rest_translator:ok_body_reply(#{<<"token">> => Serialized});

create_response(#gri{aspect = invite_group_token}, _, value, Token) ->
    {ok, Serialized} = tokens:serialize(Token),
    rest_translator:ok_body_reply(#{<<"token">> => Serialized});

create_response(#gri{aspect = space_support_token}, _, value, Token) ->
    {ok, Serialized} = tokens:serialize(Token),
    rest_translator:ok_body_reply(#{<<"token">> => Serialized});

create_response(#gri{id = _SpaceId, aspect = {owner, _}}, _, resource, _) ->
    rest_translator:ok_no_content_reply();

create_response(#gri{id = SpaceId, aspect = {user, UserId}}, _, resource, _) ->
    rest_translator:created_reply_with_location(
        [<<"spaces">>, SpaceId, <<"users">>, UserId]
    );

create_response(#gri{id = SpaceId, aspect = {group, GroupId}}, _, resource, _) ->
    rest_translator:created_reply_with_location(
        [<<"spaces">>, SpaceId, <<"groups">>, GroupId]
    );

create_response(#gri{id = SpaceId, aspect = group}, _, resource, {#gri{id = GroupId}, _}) ->
    rest_translator:created_reply_with_location(
        [<<"spaces">>, SpaceId, <<"groups">>, GroupId]
    );

create_response(#gri{id = SpaceId, aspect = harvester}, _, resource, {#gri{id = HarvesterId}, _}) ->
    rest_translator:created_reply_with_location(
        [<<"spaces">>, SpaceId, <<"harvesters">>, HarvesterId]
    );

create_response(#gri{aspect = Aspect}, _, value, {Entries, IsLast, NextPageToken}) when
    Aspect =:= list_marketplace;
    Aspect =:= list_marketplace_with_data
->
    rest_translator:ok_body_reply(#{
        <<"spaces">> => Entries,
        <<"isLast">> => IsLast,
        <<"nextPageToken">> => utils:undefined_to_null(NextPageToken)
    }).


%%--------------------------------------------------------------------
%% @doc
%% {@link rest_translator_behaviour} callback get_response/2.
%% @end
%%--------------------------------------------------------------------
-spec get_response(entity_logic:gri(), Resource :: term()) -> rest_handler:rest_resp().
get_response(#gri{id = undefined, aspect = list}, Spaces) ->
    rest_translator:ok_body_reply(#{<<"spaces">> => Spaces});

get_response(#gri{id = undefined, aspect = privileges}, Privileges) ->
    rest_translator:ok_body_reply(Privileges);

get_response(#gri{id = SpaceId, aspect = instance, scope = protected}, SpaceData) ->
    #{
        <<"name">> := Name,
        <<"description">> := Description,
        <<"organizationName">> := OrganizationName,
        <<"tags">> := Tags,
        <<"advertisedInMarketplace">> := AdvertisedInMarketplace,
        <<"marketplaceContactEmail">> := MarketplaceContactEmail,
        <<"providers">> := Providers,
        <<"supportParametersRegistry">> := SupportParametersRegistry,
        <<"creator">> := Creator,
        <<"creationTime">> := CreationTime
    } = SpaceData,
    rest_translator:ok_body_reply(#{
        <<"spaceId">> => SpaceId,
        <<"name">> => Name,
        <<"description">> => Description,
        <<"organizationName">> => OrganizationName,
        <<"tags">> => Tags,
        <<"advertisedInMarketplace">> => AdvertisedInMarketplace,
        <<"marketplaceContactEmail">> => MarketplaceContactEmail,
        <<"providers">> => Providers,
        <<"supportParametersRegistry">> => jsonable_record:to_json(SupportParametersRegistry, support_parameters_registry),
        <<"creator">> => aai:subject_to_json(utils:ensure_defined(Creator, undefined, ?SUB(nobody))),
        <<"creationTime">> => CreationTime
    });

get_response(#gri{aspect = {membership_requester_info, _}}, RequesterInfo) ->
    rest_translator:ok_body_reply(RequesterInfo);

get_response(#gri{id = SpaceId, aspect = marketplace_data, scope = protected}, MarketplaceData) ->
    rest_translator:ok_body_reply(MarketplaceData#{<<"spaceId">> => SpaceId});

get_response(#gri{aspect = owners}, Users) ->
    rest_translator:ok_body_reply(#{<<"users">> => Users});

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
    rest_translator:ok_encoded_intermediaries_reply(Intermediaries);

get_response(#gri{aspect = shares}, Shares) ->
    rest_translator:ok_body_reply(#{<<"shares">> => Shares});

get_response(#gri{aspect = eff_providers}, Providers) ->
    rest_translator:ok_body_reply(#{<<"providers">> => Providers});

get_response(#gri{aspect = harvesters}, Harvesters) ->
    rest_translator:ok_body_reply(#{<<"harvesters">> => Harvesters}).
