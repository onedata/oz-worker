%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles a centralized REST endpoint that redirects REST
%%% requests regarding data access and management (typical for Oneprovider)
%%% to a suitable provider.
%%% In essence, it is a router that works like this:
%%%
%%%             https://$ONEZONE_HOST/api/v3/onezone/data/$FILE_ID[/{...}]
%%%                                                   |
%%%                                                   v
%%%     https://$ONEPROVIDER_HOST/api/v3/oneprovider/data/$FILE_ID[/{...}]
%%%
%%%     where {...} is an arbitrary subpath (none may be given as well).
%%%
%%% The endpoint does not check the correctness of the subpath - the target
%%% Oneprovider will.
%%%
%%% TODO VFS-13183 add swaggers and tests for this endpoint
%%% TODO VFS-13183 mention that an access token suitable for A PROVIDER is needed
%%% TODO VFS-13183 consider choosing the provider based in infer_token_scope from the provided access token
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(private_data_redirector).
-author("Lukasz Opiola").

-behaviour(cowboy_handler).

-include("http/rest.hrl").
-include("entity_logic.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").

%%% API
-export([init/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
init(Req, State) ->
    try
        {ok, handle_unsafe(Req), State}
    catch Class:Reason:Stacktrace ->
        Error = ?examine_exception(Class, Reason, Stacktrace),
        {ok, reply_with_error(Error, Req), State}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec reply_with_error(errors:error(), cowboy_req:req()) -> cowboy_req:req().
reply_with_error(Error, Req) ->
    ResponseBody = json_utils:encode(#{<<"error">> => errors:to_json(Error)}),
    cowboy_req:reply(errors:to_http_code(Error), #{}, ResponseBody, Req).


%% @private
-spec handle_unsafe(cowboy_req:req()) -> cowboy_req:req().
handle_unsafe(Req) ->
    ObjectId = cowboy_req:binding(file_id, Req),
    Result = try
        SpaceId = ?check(resolve_space_id(ObjectId)),
        {ChosenProviderId, _} = ?check(od_space:choose_provider_for_request_handling(SpaceId)),
        {ok, ChosenProviderId}
    catch
        Class:Reason:Stacktrace ->
            ?debug_exception("Error while redirecting to space data", Class, Reason, Stacktrace),
            ?examine_exception(Class, Reason, Stacktrace)
    end,

    case Result of
        {ok, ProviderId} ->
            cowboy_req:reply(?HTTP_307_TEMPORARY_REDIRECT, #{
                ?HDR_LOCATION => build_provider_rest_endpoint(ProviderId, ObjectId, Req)
            }, <<>>, Req);
        {error, _} = Error ->
            reply_with_error(Error, Req)
    end.


%% @private
-spec resolve_space_id(file_id:objectid()) -> {ok, od_space:id()} | errors:error().
resolve_space_id(ObjectId) ->
    try
        CandidateSpaceId = case file_id:objectid_to_guid(ObjectId) of
            {error, badarg} ->
                ObjectId;
            {ok, FileGuid} ->
                {_, SpaceId} = file_id:unpack_guid(FileGuid),
                SpaceId
        end,
        {ok, true} = od_space:exists(CandidateSpaceId),
        {ok, CandidateSpaceId}
    catch _:_ ->
        ?ERR_BAD_DATA(?err_ctx(), <<"FileId">>, undefined)
    end.


%% @private
-spec build_provider_rest_endpoint(od_provider:id(), file_id:objectid(), cowboy_req:req()) -> binary().
build_provider_rest_endpoint(ProviderId, ObjectId, Req) ->
    {ok, Domain} = cluster_logic:get_domain(ProviderId),
    SubPath = case cowboy_req:path_info(Req) of
        [] -> <<"">>;
        Tokens -> filename:join([<<"/">> | Tokens])
    end,
    Qs = case cowboy_req:qs(Req) of
        <<"">> -> <<"">>;
        Bin -> <<"?", Bin/binary>>
    end,
    str_utils:format_bin("https://~ts/api/v3/oneprovider/data/~ts~ts~ts", [
        Domain, ObjectId, SubPath, Qs
    ]).
