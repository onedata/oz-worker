%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates all share logic functionality.
%%% In most cases, it is a wrapper for entity_logic functions.
%%% @end
%%%-------------------------------------------------------------------
-module(share_logic).
-author("Lukasz Opiola").

-include("http/gui_paths.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/logging.hrl").

-export([
    create/5, create/2
]).
-export([
    get/2,
    get_public_data/2,
    get_space/2,
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
    choose_provider_for_public_view/1
]).

% Time for which a provider choice for public view is cached, per space.
-define(CHOSEN_PROVIDER_CACHE_TTL, timer:seconds(30)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new share document in database based on Share Id, Name,
%% RootFileId and parent SpaceId.
%% @end
%%--------------------------------------------------------------------
-spec create(Auth :: aai:auth(), ShareId :: od_share:id(),
    Name :: binary(), RootFileId :: binary(), SpaceId :: od_space:id()) ->
    {ok, od_share:id()} | errors:error().
create(Auth, ShareId, Name, RootFileId, SpaceId) ->
    create(Auth, #{
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
-spec create(Auth :: aai:auth(), Data :: #{}) ->
    {ok, od_share:id()} | errors:error().
create(Auth, Data) ->
    ?CREATE_RETURN_ID(entity_logic:handle(#el_req{
        operation = create,
        auth = Auth,
        gri = #gri{type = od_share, id = undefined, aspect = instance},
        data = Data
    })).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a share record from database.
%% @end
%%--------------------------------------------------------------------
-spec get(Auth :: aai:auth(), ShareId :: od_share:id()) ->
    {ok, #od_share{}} | errors:error().
get(Auth, ShareId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_share, id = ShareId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves public share data from database.
%% @end
%%--------------------------------------------------------------------
-spec get_public_data(Auth :: aai:auth(), ShareId :: od_share:id()) ->
    {ok, map()} | errors:error().
get_public_data(Auth, ShareId) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
        gri = #gri{type = od_share, id = ShareId, aspect = instance, scope = public}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Retrieves the space in which given share was created.
%% @end
%%--------------------------------------------------------------------
-spec get_space(aai:auth(), od_share:id()) -> {ok, od_space:id()} | errors:error().
get_space(Auth, ShareId) ->
    case get(Auth, ShareId) of
        {ok, #od_share{space = SpaceId}} -> {ok, SpaceId};
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Lists all shares (their ids) in database.
%% @end
%%--------------------------------------------------------------------
-spec list(Auth :: aai:auth()) ->
    {ok, [od_share:id()]} | errors:error().
list(Auth) ->
    entity_logic:handle(#el_req{
        operation = get,
        auth = Auth,
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
-spec update(Auth :: aai:auth(), ShareId :: od_share:id(),
    Data :: #{}) -> ok | errors:error().
update(Auth, ShareId, NewName) when is_binary(NewName) ->
    update(Auth, ShareId, #{<<"name">> => NewName});
update(Auth, ShareId, Data) ->
    entity_logic:handle(#el_req{
        operation = update,
        auth = Auth,
        gri = #gri{type = od_share, id = ShareId, aspect = instance},
        data = Data
    }).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given share from database.
%% @end
%%--------------------------------------------------------------------
-spec delete(Auth :: aai:auth(), ShareId :: od_share:id()) ->
    ok | errors:error().
delete(Auth, ShareId) ->
    entity_logic:handle(#el_req{
        operation = delete,
        auth = Auth,
        gri = #gri{type = od_share, id = ShareId, aspect = instance}
    }).


%%--------------------------------------------------------------------
%% @doc
%% Returns whether a share exists.
%% @end
%%--------------------------------------------------------------------
-spec exists(od_share:id()) -> boolean().
exists(ShareId) ->
    {ok, Exists} = od_share:exists(ShareId),
    Exists.


%%--------------------------------------------------------------------
%% @doc
%% Returns public access URL for given share that points to Onezone. Onezone
%% will then redirect clients to one of providers that support the
%% parent space of the share.
%% @end
%%--------------------------------------------------------------------
-spec share_id_to_public_url(od_share:id()) -> binary().
share_id_to_public_url(ShareId) ->
    oz_worker:get_uri(?PUBLIC_SHARE_URN(ShareId)).


%%--------------------------------------------------------------------
%% @doc
%% Chooses a provider to handle viewing of a public share. Online providers that
%% are in newest version are preferred, then any online provider. Returns the
%% provider Id and its version. If there are no online providers, undefined
%% values are returned.
%% This operation is performed each time a public share is visited. To minimize
%% the cost (as each choice requires several request to the database), it is
%% cached for some time per space. There is an additional check in case the
%% cached, chosen provider has gone offline in the meantime, which repeats the
%% procedure if needed.
%% @end
%%--------------------------------------------------------------------
-spec choose_provider_for_public_view(od_share:id()) ->
    {od_provider:id() | undefined, onedata:release_version() | undefined}.
choose_provider_for_public_view(ShareId) ->
    {ok, SpaceId} = get_space(?ROOT, ShareId),
    {ok, Result} = simple_cache:get({chosen_provider_for_public_view, SpaceId}, fun() ->
        {true, choose_provider_for_space(SpaceId), ?CHOSEN_PROVIDER_CACHE_TTL}
    end),
    case Result of
        {undefined, undefined} ->
            Result;
        {ChosenProviderId, _} ->
            case provider_connection:is_online(ChosenProviderId) of
                true ->
                    Result;
                false ->
                    simple_cache:clear({chosen_provider_for_public_view, SpaceId}),
                    choose_provider_for_public_view(ShareId)
            end
    end.


%% @private
-spec choose_provider_for_space(od_space:id()) ->
    {od_provider:id() | undefined, onedata:release_version() | undefined}.
choose_provider_for_space(SpaceId) ->
    {ok, Providers} = space_logic:get_eff_providers(?ROOT, SpaceId),
    <<OzWorkerMajorVersion:6/binary, _/binary>> = oz_worker:get_release_version(),
    EligibleProviders = lists:filtermap(fun(ProviderId) ->
        case provider_connection:is_online(ProviderId) of
            false ->
                false;
            true ->
                {ok, Version} = cluster_logic:get_worker_release_version(?ROOT, ProviderId),
                VersionClassification = case Version of
                    <<OzWorkerMajorVersion:6/binary, _/binary>> -> up_to_date;
                    _ -> legacy
                end,
                {true, {ProviderId, Version, VersionClassification}}
        end
    end, Providers),
    UpToDateProviders = [UpToDateProv || UpToDateProv = {_, _, up_to_date} <- EligibleProviders],

    {ChosenProviderId, ChosenProviderVersion, _} = case {EligibleProviders, UpToDateProviders} of
        {[], _} -> {undefined, undefined, undefined};
        {_, []} -> lists_utils:random_element(EligibleProviders);
        {_, _} -> lists_utils:random_element(UpToDateProviders)
    end,
    {ChosenProviderId, ChosenProviderVersion}.