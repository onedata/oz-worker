%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module implementing the business logic for shares in OneZone.
%%% This module serves as a buffer between the database and the REST API.
%%% @end
%%%-------------------------------------------------------------------
-module(share_logic).
-author("Lukasz Opiola").

-include("datastore/oz_datastore_models_def.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([create/4, exists/1, modify/2, remove/1]).
-export([get_data/2, get_parent/1]).
-export([list/0]).
-export([share_id_to_public_url/1, share_id_to_redirect_url/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a share with given id (ShareId is client side generated).
%% Throws exception when call to the datastore fails,
%% or given member doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec create(ShareId :: binary(),
    Name :: binary(), RootFileId :: binary(), ParentSpaceId :: binary()) ->
    {ok, ShareId :: binary()} | no_return().
create(ShareId, Name, RootFileId, ParentSpaceId) ->
    true = space_logic:exists(ParentSpaceId),

    Share = #share{
        name = Name,
        parent_space = ParentSpaceId,
        root_file_id = RootFileId,
        public_url = share_id_to_public_url(ShareId)
    },
    {ok, ShareId} = share:save(#document{key = ShareId, value = Share}),

    {ok, _} = space:update(ParentSpaceId, fun(SpaceDoc) ->
        Shares = SpaceDoc#space.shares,
        {ok, SpaceDoc#space{shares = [ShareId | Shares]}}
    end),

    {ok, ShareId}.


%%--------------------------------------------------------------------
%% @doc Returns whether a Share exists.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec exists(ShareId :: binary()) -> boolean().
exists(ShareId) ->
    share:exists(ShareId).


%%--------------------------------------------------------------------
%% @doc Modify share details (currently only name can be modified).
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec modify(ShareId :: binary(), NewName :: binary()) -> ok.
modify(ShareId, NewName) ->
    {ok, _} = share:update(ShareId, fun(ShareDoc) ->
        {ok, ShareDoc#share{name = NewName}}
    end),
    ok.


%%--------------------------------------------------------------------
%% @doc Removes a share.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec remove(ShareId :: binary()) -> true.
remove(ShareId) ->
    {ok, #document{
        value = #share{
            parent_space = ParentSpaceId
        }}} = share:get(ShareId),
    ok = share:delete(ShareId),
    {ok, _} = space:update(ParentSpaceId, fun(SpaceDoc) ->
        Shares = SpaceDoc#space.shares,
        {ok, SpaceDoc#space{shares = Shares -- [ShareId]}}
    end),
    true.


%%--------------------------------------------------------------------
%% @doc Returns details about the Share.
%% Throws exception when call to the datastore fails, or space doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec get_data(ShareId :: binary(),
    Client :: {user, UserId :: binary()} | provider) ->
    {ok, [proplists:property()]}.
get_data(ShareId, _Client) ->
    {ok, #document{
        value = #share{
            name = Name,
            public_url = PublicURL,
            root_file_id = RootFileId,
            parent_space = ParentSpace
        }}} = share:get(ShareId),
    {ok, [
        {shareId, ShareId},
        {name, Name},
        {public_url, PublicURL},
        {root_file_id, RootFileId},
        {parent_space, ParentSpace}
    ]}.


%%--------------------------------------------------------------------
%% @doc Returns the parent space of given share.
%% Throws exception when call to the datastore fails.
%% @end
%%--------------------------------------------------------------------
-spec get_parent(ShareId :: binary()) ->
    {ok, undefined | binary()} | datastore:get_error().
get_parent(ShareId) ->
    case share:get(ShareId) of
        {ok, #document{value = #share{parent_space = ParentSpaceId}}} ->
            {ok, ParentSpaceId};
        Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc Returns a list of all shaces (their ids).
%%--------------------------------------------------------------------
-spec list() -> {ok, [binary()]}.
list() ->
    {ok, ShareDocs} = share:list(),
    ShareIds = lists:map(fun(#document{key = ShareId}) ->
        ShareId
    end, ShareDocs),
    {ok, ShareIds}.


%%--------------------------------------------------------------------
%% @doc Returns public access URL for given share that points to onezone.
%% OneZone will then redirect such clients to one of providers that support the
%% parent space of the share.
%%--------------------------------------------------------------------
-spec share_id_to_public_url(ShareId :: binary()) -> binary().
share_id_to_public_url(ShareId) ->
    OZHostname = dns_query_handler:get_canonical_hostname(),
    str_utils:format_bin("https://~s/share/~s", [OZHostname, ShareId]).


%%--------------------------------------------------------------------
%% @doc Returns public access URL for given share that points to one of
%% providers that support the parent space of the share.
%%--------------------------------------------------------------------
-spec share_id_to_redirect_url(ShareId :: binary()) -> binary().
share_id_to_redirect_url(ShareId) ->
    {ok, #document{
        value = #share{
            parent_space = ParentSpaceId
        }}} = share:get(ShareId),
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
