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
-spec register_handle(od_handle_service:id(), od_handle:resource_type(),
    od_handle:resource_id(), od_handle:metadata()) -> {ok, od_handle:public_handle()}.
register_handle(HandleServiceId, ResourceType, ResourceId, Metadata) ->
    {ok, #document{value = #od_handle_service{
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties}}
    } = od_handle_service:get(HandleServiceId),
    Url = get_redirect_url(ResourceType, ResourceId),
    Body = #{
        <<"url">> => Url,
        <<"serviceProperties">> => ServiceProperties,
        <<"metadata">> => #{<<"onedata_dc">> => Metadata}
    },
    Headers = [{<<"content-type">>, <<"application/json">>}, {<<"accept">>, <<"application/json">>}],
    Type = maps:get(<<"type">>, ServiceProperties),
    case Type of
        <<"DOI">> ->
            Prefix = maps:get(<<"prefix">>, ServiceProperties),
            DoiId = base64url:encode(crypto:strong_rand_bytes(5)),
            DoiHandle = <<Prefix/binary, "/", DoiId/binary>>,
            DoiHandleEncoded = http_utils:url_encode(DoiHandle),
            {ok, 201, _, _} = handle_proxy_client:put(ProxyEndpoint, <<"/handle?hndl=", DoiHandleEncoded/binary>>, Headers, json_utils:encode_map(Body)),
            {ok, DoiHandle};
        _ ->
            {ok, 201, ResponseHeaders, _} = handle_proxy_client:put(ProxyEndpoint, <<"/handle">>, Headers, json_utils:encode_map(Body)),
            {ok, maps:get(<<"location">>, ResponseHeaders)}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Unregister handle from external handle service
%% @end
%%--------------------------------------------------------------------
-spec unregister_handle(od_handle:id()) -> ok.
unregister_handle(HandleId) ->
    {ok, #document{value = #od_handle{handle_service = HandleServiceId, public_handle = PublicHandle}}} =
        od_handle:get(HandleId),
    {ok, #document{value = #od_handle_service{
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties}}
    } = od_handle_service:get(HandleServiceId),
%%    Body = [
%%        {<<"serviceProperties">>, ServiceProperties}
%%    ],
    Body = ServiceProperties, %todo use above Body after fixing proxy
    Headers = [{<<"content-type">>, <<"application/json">>}, {<<"accept">>, <<"application/json">>}],
    Type = maps:get(<<"type">>, ServiceProperties),
    {ok, 200, _, _} =
        case Type of
            <<"DOI">> ->
                PublicHandleEncoded = http_utils:url_encode(PublicHandle),
                handle_proxy_client:delete(ProxyEndpoint, <<"/handle?hndl=", PublicHandleEncoded/binary>>, Headers, json_utils:encode_map(Body));
            _ ->
                handle_proxy_client:delete(ProxyEndpoint, <<"/handle">>, Headers, json_utils:encode_map(Body))
        end,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Modify handle in external handle service
%% @end
%%--------------------------------------------------------------------
-spec modify_handle(od_handle:id(), od_handle:resource_type(), od_handle:resource_id(), od_handle:metadata()) ->
    ok.
modify_handle(_HandleId, undefined, undefined, undefined) ->
    ok;
modify_handle(HandleId, NewResourceType, NewResourceId, NewMetadata) ->
    {ok, #document{value = #od_handle{handle_service = HandleServiceId,
        resource_type = ResourceType, resource_id = ResourceId, public_handle = PublicHandle,
        metadata = Metadata}}} =
        od_handle:get(HandleId),
    {ok, #document{value = #od_handle_service{
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties}}
    } = od_handle_service:get(HandleServiceId),
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
            Body = #{
                <<"url">> => FinalUrl,
                <<"serviceProperties">> => ServiceProperties,
                <<"metadata">> => #{<<"onedata_dc">> => FinalMetadata}
            },
            Headers = [{<<"content-type">>, <<"application/json">>}, {<<"accept">>, <<"application/json">>}],
            Type = maps:get(<<"type">>, ServiceProperties),
            {ok, 204, _, _} =
                case Type of
                    <<"DOI">> ->
                        PublicHandleEncoded = http_utils:url_encode(PublicHandle),
                        handle_proxy_client:patch(ProxyEndpoint, <<"/handle?hndl=", PublicHandleEncoded/binary>>, Headers, json_utils:encode_map(Body));
                    _ ->
                        handle_proxy_client:patch(ProxyEndpoint, <<"/handle">>, Headers, json_utils:encode_map(Body))
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
-spec get_redirect_url(od_handle:resource_type(), od_handle:resource_id()) -> public_url().
get_redirect_url(<<"Share">>, ShareId) ->
    {ok, #document{value = #od_share{public_url = PublicUrl}}} = od_share:get(ShareId),
    PublicUrl.