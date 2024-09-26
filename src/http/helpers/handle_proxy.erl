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

-define(RANDOM_ID(), base62:from_base64(base64url:encode(crypto:strong_rand_bytes(6)))).
-define(DOI_DC_IDENTIFIER(Handle), <<"doi:", Handle/binary>>).

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
    od_handle:resource_id(), od_handle:raw_metadata()) -> {ok, od_handle:public_handle()}.
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
            DoiHandle = <<Prefix/binary, "/", (?RANDOM_ID())/binary>>,
            DoiHandleEncoded = http_utils:url_encode(DoiHandle),
            case handle_proxy_client:put(ProxyEndpoint, <<"/handle?hndl=", DoiHandleEncoded/binary>>, Headers, Body) of
                {ok, 201, _, _} ->
                    {ok, ?DOI_DC_IDENTIFIER(DoiHandle)};
                HttpCallResult ->
                    ?error(?autoformat_with_msg("Error registering a handle", [
                        Type, HandleServiceName, ProxyEndpoint, HttpCallResult
                    ])),
                    throw(?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(HandleServiceName))
            end;
        _ -> % <<"PID">> and other types
            PidHandle = ?RANDOM_ID(),
            case handle_proxy_client:put(ProxyEndpoint, <<"/handle?hndl=", PidHandle/binary>>, Headers, Body) of
                {ok, 201, _, RespJSON} ->
                    PublicHandle = maps:get(<<"handle">>, json_utils:decode(RespJSON)),
                    {ok, PublicHandle};
                HttpCallResult ->
                    ?error(?autoformat_with_msg("Error registering a handle", [
                        Type, HandleServiceName, ProxyEndpoint, HttpCallResult
                    ])),
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
    {ok, #document{value = #od_handle{
        handle_service = HandleServiceId,
        public_handle = PublicHandle
    }}} = od_handle:get(HandleId),
    {ok, #document{value = #od_handle_service{
        name = HandleServiceName,
        proxy_endpoint = ProxyEndpoint,
        service_properties = ServiceProperties
    }}} = od_handle_service:get(HandleServiceId),
    % NOTE: unlike during creation, the service properties are passed on the
    % root level of the body object (rather than nested under the "serviceProperties" key)
    Body = ServiceProperties,
    Headers = #{?HDR_CONTENT_TYPE => <<"application/json">>, ?HDR_ACCEPT => <<"application/json">>},
    Type = maps:get(<<"type">>, ServiceProperties),
    % NOTE: the public handle is built differently for different types of identifiers;
    % @see register_handle/4
    Handle = case Type of
        <<"DOI">> ->
            % the public handle looks like this: "oai:21.T15999/abcd"
            % the handle for the above is: "21.T15999/abcd"   (includes prefix!)
            ?DOI_DC_IDENTIFIER(DoiHandle) = PublicHandle,
            DoiHandle;
        _ -> % <<"PID">> and other types
            % the public handle looks like this: "http://hdl.handle.net/21.T15999/abcd"
            % the handle for the above is: "abcd"   (doesn't include prefix!)
            Prefix = maps:get(<<"prefix">>, ServiceProperties),
            [_ServiceUrl, PidHandle] = binary:split(PublicHandle, <<Prefix/binary, "/">>),
            PidHandle
    end,
    HandleEncoded = http_utils:url_encode(Handle),
    case handle_proxy_client:delete(
        ProxyEndpoint, <<"/handle?hndl=", HandleEncoded/binary>>, Headers, json_utils:encode(Body)
    ) of
        {ok, 200, _, _} ->
            ok;
        HttpCallResult ->
            ?error(?autoformat_with_msg("Error unregistering a handle", [
                Type, HandleServiceName, ProxyEndpoint, HttpCallResult
            ])),
            throw(?ERROR_EXTERNAL_SERVICE_OPERATION_FAILED(HandleServiceName))
    end.

%%--------------------------------------------------------------------
%% @doc
%% Modify handle in external handle service
%% @end
%%--------------------------------------------------------------------
-spec modify_handle(od_handle:id(), od_handle:raw_metadata()) ->
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
    PublicHandleEncoded = http_utils:url_encode(PublicHandle),
    {ok, 204, _, _} = handle_proxy_client:patch(
        ProxyEndpoint, <<"/handle?hndl=", PublicHandleEncoded/binary>>, Headers, json_utils:encode(Body)
    ),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec get_redirect_url(od_handle:resource_type(), od_handle:resource_id()) -> public_url().
get_redirect_url(<<"Share">>, ShareId) ->
    share_logic:build_public_url(ShareId).
