%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc

%%% @end
%%%-------------------------------------------------------------------
-module(n_share_logic).
-author("Lukasz Opiola").
-behaviour(data_logic_behaviour).


-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-define(PLUGIN, n_share_logic_plugin).

-export([create/5, create/2]).

-export([
    add_user/3,
    add_group/3
]).

-export([
    get/2,
    list/1
]).

-export([update/3]).

-export([delete/2]).

-export([exists/1]).

-export([share_id_to_public_url/1, share_id_to_redirect_url/1]).



create(Issuer, ShareId, Name, RootFileId, SpaceId) ->
    create(Issuer, #{
        <<"shareId">> => ShareId,
        <<"name">> => Name,
        <<"spaceId">> => SpaceId,
        <<"rootFileId">> => RootFileId
    }).
create(Issuer, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, undefined, entity, Data).


add_user(Issuer, ShareId, UserId) when is_binary(UserId) ->
    add_user(Issuer, ShareId, #{<<"userId">> => UserId});
add_user(Issuer, ShareId, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, ShareId, users, Data).


add_group(Issuer, ShareId, GroupId) when is_binary(GroupId) ->
    add_group(Issuer, ShareId, #{<<"groupId">> => GroupId});
add_group(Issuer, ShareId, Data) ->
    n_entity_logic:create(Issuer, ?PLUGIN, ShareId, groups, Data).


get(Issuer, ShareId) ->
    n_entity_logic:get(Issuer, ?PLUGIN, entity, ShareId).


list(Issuer) ->
    n_entity_logic:get(Issuer, ?PLUGIN, undefined, list).




update(Issuer, ShareId, Data) ->
    n_entity_logic:update(Issuer, ?PLUGIN, ShareId, entity, Data).

delete(Issuer, ShareId) ->
    n_entity_logic:delete(Issuer, ?PLUGIN, ShareId, entity).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a share exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(ShareId :: od_share:id()) -> boolean().
exists(ShareId) ->
    od_share:exists(ShareId).


%%--------------------------------------------------------------------
%% @doc Returns public access URL for given share that points to onezone.
%% OneZone will then redirect such clients to one of providers that support the
%% parent space of the share.
%%--------------------------------------------------------------------
-spec share_id_to_public_url(ShareId :: binary()) -> binary().
share_id_to_public_url(ShareId) ->
    {ok, OZHostname} = application:get_env(oz_worker, http_domain),
    str_utils:format_bin("https://~s/share/~s", [OZHostname, ShareId]).


%%--------------------------------------------------------------------
%% @doc Returns public access URL for given share that points to one of
%% providers that support the parent space of the share.
%%--------------------------------------------------------------------
-spec share_id_to_redirect_url(ShareId :: binary()) -> binary().
share_id_to_redirect_url(ShareId) ->
    {ok, #document{
        value = #od_share{
            space = ParentSpaceId
        }}} = od_share:get(ShareId),
    {ok, [{providers, Providers}]} =
        space_logic:get_providers(ParentSpaceId, provider),
    % Prefer online providers
    {Online, Offline} = lists:partition(
        fun(ProviderId) ->
            subscriptions:any_connection_active(ProviderId)
        end, Providers),
    % But if there are none, choose one of inactive
    Choice = case length(Online) of
        0 -> Offline;
        _ -> Online
    end,
    ChosenProvider = lists:nth(rand:uniform(length(Choice)), Choice),
    {ok, ProviderURL} = provider_logic:get_url(ChosenProvider),
    str_utils:format_bin("~s/#/public/shares/~s", [ProviderURL, ShareId]).
