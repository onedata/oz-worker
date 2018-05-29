%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all share logic functionalities.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(share_logic).
-author("Lukasz Opiola").

-include("http/gui_paths.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

-define(PLUGIN, share_logic_plugin).

-export([
    create/5, create/2
]).
-export([
    get/2,
    get_public_data/2,
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

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new share document in database based on Share Id, Name,
%% RootFileId and parent SpaceId.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: entity_logic:client(), ShareId :: od_share:id(),
    Name :: binary(), RootFileId :: binary(), SpaceId :: od_space:id()) ->
    {ok, od_share:id()} | {error, term()}.
create(Client, ShareId, Name, RootFileId, SpaceId) ->
    create(Client, #{
        <<"shareId">> => ShareId,
        <<"name">> => Name,
        <<"rootFileId">> => RootFileId,
        <<"spaceId">> => SpaceId
    }).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new share document in database. Share Id, Name,
%% RootFileId and parent SpaceId are provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec create(Client :: entity_logic:client(), Data :: #{}) ->
    {ok, od_share:id()} | {error, term()}.
create(Client, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        client = Client,
        gri = #gri{type = od_share, id = undefined, aspect = instance},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a share record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(Client :: entity_logic:client(), ShareId :: od_share:id()) ->
    {ok, #od_share{}} | {error, term()}.
get(Client, ShareId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_share, id = ShareId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves public share data from database.
%% @end
%%--------------------------------------------------------------------
-spec get_public_data(Client :: entity_logic:client(), ShareId :: od_share:id()) ->
    {ok, maps:map()} | {error, term()}.
get_public_data(Client, ShareId) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_share, id = ShareId, aspect = instance, scope = public}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Lists all shares (their ids) in database.
%% @end
%%--------------------------------------------------------------------
-spec list(Client :: entity_logic:client()) ->
    {ok, [od_share:id()]} | {error, term()}.
list(Client) ->
    entity_logic:handle(#el_req{
        operation = get,
        client = Client,
        gri = #gri{type = od_share, id = undefined, aspect = list}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Updates information of given share (currently only name is supported).
%% Has two variants:
%% 1) Share Name is given explicitly
%% 2) Share name is provided in a proper Data object.
%% @end
%%--------------------------------------------------------------------
-spec update(Client :: entity_logic:client(), ShareId :: od_share:id(),
    Data :: #{}) -> ok | {error, term()}.
update(Client, ShareId, NewName) when is_binary(NewName) ->
    update(Client, ShareId, #{<<"name">> => NewName});
update(Client, ShareId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        client = Client,
        gri = #gri{type = od_share, id = ShareId, aspect = instance},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given share from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(Client :: entity_logic:client(), ShareId :: od_share:id()) ->
    ok | {error, term()}.
delete(Client, ShareId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        client = Client,
        gri = #gri{type = od_share, id = ShareId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a share exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(ShareId :: od_share:id()) -> boolean().
exists(ShareId) ->
    {ok, Exists} = od_share:exists(ShareId),
    Exists.


%%--------------------------------------------------------------------
%% @doc Returns public access URL for given share that points to onezone.
%% OneZone will then redirect such clients to one of providers that support the
%% parent space of the share.
%%--------------------------------------------------------------------
-spec share_id_to_public_url(ShareId :: binary()) -> binary().
share_id_to_public_url(ShareId) ->
    oz_worker:get_uri(filename:join(<<?PUBLIC_SHARE_PATH>>, ShareId)).


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
            provider_connection:is_online(ProviderId)
        end, maps:keys(Providers)),
    % But if there are none, choose one of inactive
    Choice = case length(Online) of
        0 -> Offline;
        _ -> Online
    end,
    ChosenProvider = lists:nth(rand:uniform(length(Choice)), Choice),
    {ok, ProviderURL} = provider_logic:get_url(ChosenProvider),
    str_utils:format_bin("~s~s", [
        ProviderURL, ?PROVIDER_PUBLIC_SHARE_PATH(ShareId)
    ]).