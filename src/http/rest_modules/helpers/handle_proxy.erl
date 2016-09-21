%%%--------------------------------------------------------------------
%%% @author Tomasz Lichon
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Proxy for contact with handle service
%%% @end
%%%--------------------------------------------------------------------
-module(handle_proxy).
-author("Tomasz Lichon").

-include("datastore/oz_datastore_models_def.hrl").
-include("registered_names.hrl").

-type public_url() :: binary().

%% API
-export([register_handle/4, unregister_handle/1, modify_handle/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Register handle in external handle service
%% @end
%%--------------------------------------------------------------------
-spec register_handle(handle_service:id(), handle:resource_type(),
    handle:resource_id(), handle:metadata()) -> {ok, handle:public_handle()}.
register_handle(HandleServiceId, ResourceType, ResourceId, Metadata) ->
    {ok, #document{value = #handle_service{
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties}}
    } = handle_service:get(HandleServiceId),
    Url = get_redirect_url(ResourceType, ResourceId),
    Body = [
        {<<"url">>, Url},
        {<<"serviceProperties">>, ServiceProperties},
        {<<"metadata">>, [{<<"onedata_dc">>, Metadata}]}
    ],
    Headers = [{<<"content-type">>, <<"application/json">>}, {<<"accept">>, <<"application/json">>}],
    Type = proplists:get_value(<<"type">>, ServiceProperties),
    case Type of
        <<"DOI">> ->
            Prefix = proplists:get_value(<<"prefix">>, ServiceProperties),
            DoiId = base64url:encode(crypto:rand_bytes(5)),
            DoiHandle = <<Prefix/binary, "%2F", DoiId/binary>>,
            {ok, 201, _, _} = handle_proxy_client:put(ProxyEndpoint, <<"/handle?hndl=", DoiHandle/binary>>, Headers, json_utils:encode(Body)),
            {ok, DoiHandle};
        _ ->
            {ok, 201, ResponseHeaders, _} = handle_proxy_client:put(ProxyEndpoint, <<"/handle">>, Headers, json_utils:encode(Body)),
            {ok, proplists:get_value(<<"location">>, ResponseHeaders)}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Unregister handle from external handle service
%% @end
%%--------------------------------------------------------------------
-spec unregister_handle(handle:id()) -> ok.
unregister_handle(HandleId)  ->
    {ok, #document{value = #handle{handle_service_id = HandleServiceId, public_handle = PublicHandle}}} =
        handle:get(HandleId),
    {ok, #document{value = #handle_service{
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties}}
    } = handle_service:get(HandleServiceId),
%%    Body = [
%%        {<<"serviceProperties">>, ServiceProperties}
%%    ],
    Body = ServiceProperties, %todo use above Body after fixing proxy
    Headers = [{<<"content-type">>, <<"application/json">>}, {<<"accept">>, <<"application/json">>}],
    Type = proplists:get_value(<<"type">>, ServiceProperties),
    {ok, 200, _, _} =
        case Type of
            <<"DOI">> ->
                handle_proxy_client:delete(ProxyEndpoint, <<"/handle?hndl=", PublicHandle/binary>>, Headers,  json_utils:encode(Body));
            _ ->
                handle_proxy_client:delete(ProxyEndpoint, <<"/handle">>, Headers, json_utils:encode(Body))
        end,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Modify handle in external handle service
%% @end
%%--------------------------------------------------------------------
-spec modify_handle(handle:id(), handle:resource_type(), handle:resource_id(), handle:metadata()) ->
    ok.
modify_handle(_HandleId, undefined, undefined, undefined)  ->
    ok;
modify_handle(HandleId, NewResourceType, NewResourceId, NewMetadata)  ->
    {ok, #document{value = #handle{handle_service_id = HandleServiceId,
        resource_type = ResourceType, resource_id = ResourceId, public_handle = PublicHandle,
        metadata = Metadata}}} =
        handle:get(HandleId),
    {ok, #document{value = #handle_service{
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties}}
    } = handle_service:get(HandleServiceId),
    case (NewResourceType =:= undefined orelse NewResourceType =:= ResourceType)
        andalso (NewResourceId =:= undefined orelse NewResourceId =:= ResourceId)
        andalso (NewMetadata =:= undefined orelse NewMetadata =:= Metadata)
    of
        true ->
            ok;
        false ->
            FinalResourceType = utils:ensure_defined(NewResourceType, undefined, ResourceType),
            FinalResourceId = utils:ensure_defined(NewResourceId, undefined, ResourceId),
            FinalMetadata = utils:ensure_defined(NewMetadata, undefined, Metadata),
            FinalUrl = get_redirect_url(FinalResourceType, FinalResourceId),
            Body = [
                {<<"url">>, FinalUrl},
                {<<"serviceProperties">>, ServiceProperties},
                {<<"metadata">>, [{<<"onedata_dc">>, FinalMetadata}]}
            ],
            Headers = [{<<"content-type">>, <<"application/json">>}, {<<"accept">>, <<"application/json">>}],
            Type = proplists:get_value(<<"type">>, ServiceProperties),
            {ok, 204, _, _} =
                case Type of
                    <<"DOI">> ->
                        handle_proxy_client:patch(ProxyEndpoint, <<"/handle?hndl=", PublicHandle/binary>>, Headers, json_utils:encode(Body));
                    _ ->
                        handle_proxy_client:patch(ProxyEndpoint, <<"/handle">>, Headers, json_utils:encode(Body))
                end,
            ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Get public url for share.
%% @end
%%--------------------------------------------------------------------
-spec get_redirect_url(handle:resource_type(), handle:resource_id()) -> public_url().
get_redirect_url(<<"Share">>, ShareId) ->
    {ok, #document{value = #share{public_url = PublicUrl}}} = share:get(ShareId),
    PublicUrl.