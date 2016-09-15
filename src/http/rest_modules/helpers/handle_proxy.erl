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
-export([register_handle/3, unregister_handle/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Register handle in external handle service
%% @end
%%--------------------------------------------------------------------
-spec register_handle(handle_service:id(), handle:resource_type(),
    handle:resource_id()) -> {ok, handle:public_handle()}.
register_handle(HandleServiceId, ResourceType, ResourceId) ->
    {ok, #document{value = #handle_service{
        proxy_endpoint = ProxyEndpoint,
        service_description = ServiceProperties}}
    } = handle_service:get(HandleServiceId),
    Url = get_redirect_url(ResourceType, ResourceId),
    Metadata = get_metadata(),
    Body = [
        {<<"url">>, Url},
        {<<"serviceProperties">>, ServiceProperties},
        {<<"metadata">>, Metadata}
    ],
    Headers = [{<<"content-type">>, <<"application/json">>}, {<<"accept">>, <<"application/json">>}],
    Type = proplists:get_value(<<"type">>, ServiceProperties),
    Handle =
        case Type of
            <<"DOI">> ->
                Prefix = proplists:get_value(<<"prefix">>, ServiceProperties),
                DoiId = base64url:encode(crypto:rand_bytes(5)),
                DoiHandle = <<Prefix/binary, "%2F", DoiId/binary>>,
                {ok, 201, _, _} = http_client:put(<<ProxyEndpoint/binary, "/handle?hndl=", DoiHandle/binary>>, Headers, json_utils:encode(Body)),
                DoiHandle;
            _ ->
                {ok, 201, ResponseHeaders, _} = http_client:put(<<ProxyEndpoint/binary, "/handle">>, Headers, json_utils:encode(Body)),
                proplists:get_value(<<"location">>, ResponseHeaders)
        end,
    {ok, Handle}.

%%--------------------------------------------------------------------
%% @doc
%% Unregister handle from external handle service
%% @end
%%--------------------------------------------------------------------
-spec unregister_handle(handle:id()) -> ok.
unregister_handle(HandleId)  ->
    {ok, #document{value = #handle{handle_service_id = HandleServiceId, handle = PublicHandle}}} =
        handle:get(HandleId),
    {ok, #document{value = #handle_service{
        proxy_endpoint = ProxyEndpoint,
        service_description = ServiceProperties}}
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
                http_client:delete(<<ProxyEndpoint/binary, "/handle?hndl=", PublicHandle/binary>>, Headers,  json_utils:encode(Body));
            _ ->
                http_client:delete(<<ProxyEndpoint/binary, "/handle">>, Headers, json_utils:encode(Body))
        end,
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Get metadata of public file share.
%% @end
%%--------------------------------------------------------------------
-spec get_metadata() -> list().
get_metadata() ->
    [  %todo get from file
        {<<"cdmi_size">>, 0},
        {<<"onedata_json">>, {struct, []}},
        {<<"onedata_rdf">>, {struct, []}},
        {<<"onedata_dc">>, <<"<?xml version=\"1.0\"?>",
            "<metadata xmlns:xsi=\"http:\/\/www.w3.org\/2001\/XMLSchema-instance\" xmlns:dc=\"http:\/\/purl.org\/dc\/elements\/1.1\/\">"
            "<dc:title>Test dataset<\/dc:title>",
            "<dc:creator>John Johnson<\/dc:creator>",
            "<dc:creator>Jane Doe<\/dc:creator>",
            "<dc:subject>Test of datacite<\/dc:subject>",
            "<dc:description>Lorem ipsum lorem ipusm<\/dc:description>",
            "<dc:publisher>Onedata<\/dc:publisher>",
            "<dc:publisher>EGI<\/dc:publisher>",
            "<dc:date>2016<\/dc:date>",
            "<dc:format>application\/pdf<\/dc:format>",
            "<dc:identifier>onedata:LKJHASKFJHASLKDJHKJHuah132easd<\/dc:identifier>",
            "<dc:language>eng<\/dc:language>",
            "<dc:rights>CC-0<\/dc:rights>",
            "<\/metadata>">>}
    ].

%%--------------------------------------------------------------------
%% @doc
%% Get public url for share.
%% @end
%%--------------------------------------------------------------------
-spec get_redirect_url(handle:resource_type(), handle:resource_id()) -> public_url().
get_redirect_url(_ResourceType, ResourceId) ->
    <<"https://", (list_to_binary(application:get_env(?APP_Name, public_domain_for_opendata, "onedata.org")))/binary,
        "/share/", ResourceId/binary>>. %todo change to #share.public_url