%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles a centralized REST endpoint for fetching information and
%%% content of any shared file/directory. It redirects to a corresponding REST
%%% endpoint in one of the supporting providers. In essence, it is a router that
%%% works like this:
%%%
%%%     https://$ONEZONE_HOST/api/v3/onezone/shares/data/$FILE_ID[/{...}]
%%%                                     |
%%%                                     v
%%%     https://$ONEPROVIDER_HOST/api/v3/oneprovider/data/$FILE_ID[/{...}]
%%%
%%%     where {...} is an arbitrary subpath (none may be given as well).
%%%
%%% The endpoint does not check the correctness of the subpath - the target
%%% Oneprovider will.
%%% @end
%%%-------------------------------------------------------------------
-module(shared_data_redirector).
-author("Lukasz Opiola").

-include("http/rest.hrl").
-include("entity_logic.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").

%%% API
-export([handle/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec handle(file_id:objectid(), cowboy_req:req()) -> rest_handler:rest_resp().
handle(ObjectId, Req) ->
    Result = try
        case resolve_share_id(ObjectId) of
            ?ERR = Err1 ->
                Err1;
            {ok, ShareId} ->
                case ?check(od_share:choose_provider_for_public_share_handling(ShareId)) of
                    % there is no suitable, online provider
                    {undefined, _} -> ?ERR_SERVICE_UNAVAILABLE(?err_ctx());
                    % only a legacy provider is available, but providers before 20.02 do
                    % not support public REST access to shared file/directory contents
                    {_, <<"18.02", _/binary>>} -> ?ERR_NOT_IMPLEMENTED(?err_ctx());
                    {_, <<"19.02", _/binary>>} -> ?ERR_NOT_IMPLEMENTED(?err_ctx());
                    {ChosenProviderId, _} -> {ok, ChosenProviderId}
                end
        end
    catch
        Class:Reason:Stacktrace ->
            ?debug_exception("Error while redirecting to public share", Class, Reason, Stacktrace),
            ?examine_exception(Class, Reason, Stacktrace)
    end,

    case Result of
        {ok, ProviderId} ->
            #rest_resp{code = ?HTTP_307_TEMPORARY_REDIRECT, headers = #{
                ?HDR_LOCATION => build_provider_rest_endpoint(ProviderId, ObjectId, Req)
            }};
        {error, _} = Error ->
            rest_translator:response(undefined, Error)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec resolve_share_id(file_id:objectid()) -> {ok, od_share:id()} | errors:error().
resolve_share_id(ObjectId) ->
    try
        case file_id:objectid_to_guid(ObjectId) of
            {error, badarg} ->
                ?ERR_BAD_DATA(?err_ctx(), <<"FileId">>, undefined);
            {ok, FileGuid} ->
                case file_id:unpack_share_guid(FileGuid) of
                    {_, _, undefined} ->
                        ?ERR_BAD_DATA(?err_ctx(), <<"FileId">>, undefined);
                    {_, _, ShareId} ->
                        {ok, ShareId}
                end
        end
    catch _:_ ->
        ?ERR_BAD_DATA(?err_ctx(), <<"FileId">>, undefined)
    end.


%% @private
-spec build_provider_rest_endpoint(od_provider:id(), file_id:objectid(), cowboy_req:req()) -> binary().
build_provider_rest_endpoint(ProviderId, ObjectId, Req) ->
    {ok, Domain} = cluster_logic:get_domain(ProviderId),
    Subpath = case cowboy_req:path_info(Req) of
        [] -> <<"">>;
        Tokens -> filename:join([<<"/">> | Tokens])
    end,
    Qs = case cowboy_req:qs(Req) of
        <<"">> -> <<"">>;
        Bin -> <<"?", Bin/binary>>
    end,
    str_utils:format_bin("https://~ts/api/v3/oneprovider/data/~ts~ts~ts", [
        Domain, ObjectId, Subpath, Qs
    ]).
