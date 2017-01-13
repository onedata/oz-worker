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

-export([
    create/5, create/2
]).
-export([
    get/2,
    get_data/2,
    list/1
]).
-export([
    update/3
]).
-export([
    delete/2
]).
-export([
    exists/1
]).
-export([
    share_id_to_public_url/1,
    share_id_to_redirect_url/1
]).



create(Client, ShareId, Name, RootFileId, SpaceId) ->
    create(Client, #{
        <<"shareId">> => ShareId,
        <<"name">> => Name,
        <<"spaceId">> => SpaceId,
        <<"rootFileId">> => RootFileId
    }).
create(Client, Data) ->
    n_entity_logic:create(Client, ?PLUGIN, undefined, entity, Data).


get(Client, ShareId) ->
    n_entity_logic:get(Client, ?PLUGIN, entity, ShareId).


get_data(Client, ShareId) ->
    n_entity_logic:get(Client, ?PLUGIN, data, ShareId).


list(Client) ->
    n_entity_logic:get(Client, ?PLUGIN, undefined, list).


update(Client, ShareId, Data) ->
    n_entity_logic:update(Client, ?PLUGIN, ShareId, entity, Data).

delete(Client, ShareId) ->
    n_entity_logic:delete(Client, ?PLUGIN, ShareId, entity).


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
        value = #od_share{space = ParentSpaceId}
    }} = od_share:get(ShareId),
    {ok, #document{
        value = #od_space{providers = Providers}
    }} = od_space:get(ParentSpaceId),
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
    {ok, ProviderURL} = n_provider_logic:get_url(ChosenProvider),
    str_utils:format_bin("~s/#/public/shares/~s", [ProviderURL, ShareId]).
