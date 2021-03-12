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

-include("datastore/oz_datastore_models.hrl").
-include("registered_names.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/errors.hrl").

-type public_url() :: binary().

-define(RANDOM_ID(), base64url:encode(crypto:strong_rand_bytes(5))).
-define(DOI_DC_IDENTIFIER(Hndl), <<"doi:", Hndl/binary>>).

%% API
-export([register_handle/4, unregister_handle/1, modify_handle/2]).

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
        name = HandleServiceName,
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties}}
    } = od_handle_service:get(HandleServiceId),
    Url = get_redirect_url(ResourceType, ResourceId),
    Body = json_utils:encode(#{
        <<"url">> => Url,
        <<"serviceProperties">> => ServiceProperties,
        <<"metadata">> => #{<<"onedata_dc">> => Metadata}
    }),
    Headers = #{?HDR_CONTENT_TYPE => <<"application/json">>, ?HDR_ACCEPT => <<"application/json">>},
    Type = maps:get(<<"type">>, ServiceProperties),
    case Type of
        <<"DOI">> ->
            Prefix = maps:get(<<"prefix">>, ServiceProperties),
            DoiId = ?RANDOM_ID(),
            DoiHandle = <<Prefix/binary, "/", DoiId/binary>>,
            DoiHandleEncoded = http_utils:url_encode(DoiHandle),
            case handle_proxy_client:put(ProxyEndpoint, <<"/handle?hndl=", DoiHandleEncoded/binary>>, Headers, Body) of
                {ok, 201, _, _} ->
                    {ok, ?DOI_DC_IDENTIFIER(DoiHandle)};
                Other ->
                    ?error("Error registering a ~s handle, handle proxy '~ts' (~ts) returned:~n~p", [
                        Type, HandleServiceName, ProxyEndpoint, Other
                    ]),
                    throw(?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(HandleServiceName))
            end;
        _ -> % <<"PID">> and other types
            DoiId = ?RANDOM_ID(),
            case handle_proxy_client:put(ProxyEndpoint, <<"/handle?hndl=", DoiId/binary>>, Headers, Body) of
                {ok, 201, _, RespJSON} ->
                    PidHandle = maps:get(<<"handle">>, json_utils:decode(RespJSON)),
                    {ok, PidHandle};
                Other ->
                    ?error("Error registering a ~s handle, handle proxy '~ts' (~ts) returned:~n~p", [
                        Type, HandleServiceName, ProxyEndpoint, Other
                    ]),
                    throw(?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(HandleServiceName))
            end
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
    Body = ServiceProperties, %TODO VFS-7415 use above Body after fixing proxy
    Headers = #{?HDR_CONTENT_TYPE => <<"application/json">>, ?HDR_ACCEPT => <<"application/json">>},
    Type = maps:get(<<"type">>, ServiceProperties),
    {ok, 200, _, _} =
        case Type of
            <<"DOI">> ->
                PublicHandleEncoded = http_utils:url_encode(PublicHandle),
                handle_proxy_client:delete(ProxyEndpoint, <<"/handle?hndl=", PublicHandleEncoded/binary>>, Headers, json_utils:encode(Body));
            _ ->
                handle_proxy_client:delete(ProxyEndpoint, <<"/handle">>, Headers, json_utils:encode(Body))
        end,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Modify handle in external handle service
%% @end
%%--------------------------------------------------------------------
-spec modify_handle(od_handle:id(), od_handle:metadata()) ->
    ok.
modify_handle(HandleId, NewMetadata) ->
    {ok, #document{value = #od_handle{
        handle_service = HandleServiceId, public_handle = PublicHandle,
        resource_type = ResourceType, resource_id = ResourceId
    }}} = od_handle:get(HandleId),
    {ok, #document{value = #od_handle_service{
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties}}
    } = od_handle_service:get(HandleServiceId),
    Url = get_redirect_url(ResourceType, ResourceId),
    Body = #{
        <<"url">> => Url,
        <<"serviceProperties">> => ServiceProperties,
        <<"metadata">> => #{<<"onedata_dc">> => NewMetadata}
    },
    Headers = #{?HDR_CONTENT_TYPE => <<"application/json">>, ?HDR_ACCEPT => <<"application/json">>},
    Type = maps:get(<<"type">>, ServiceProperties),
    {ok, 204, _, _} =
        case Type of
            <<"DOI">> ->
                PublicHandleEncoded = http_utils:url_encode(PublicHandle),
                handle_proxy_client:patch(ProxyEndpoint, <<"/handle?hndl=", PublicHandleEncoded/binary>>, Headers, json_utils:encode(Body));
            _ ->
                handle_proxy_client:patch(ProxyEndpoint, <<"/handle">>, Headers, json_utils:encode(Body))
        end,
    ok.

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
