%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called to handle
%%% GUI package uploads from op_worker or op_panel services.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_upload).
-author("Lukasz Opiola").


-include("http/rest.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/logging.hrl").

%% Cowboy API
-export([handle_service_gui_upload/1, handle_harvester_gui_upload/1]).

-define(UPLOAD_BUFFER_SIZE, 262144).
%% Maximum timeout after which body read from request is passed to
%% upload handler process. Note that the body is returned immediately
%% if its size reaches the buffer size (UPLOAD_BUFFER_SIZE).
-define(UPLOAD_READ_PERIOD, 5000).
%% Timeout for reading UPLOAD_BUFFER_SIZE bytes from socket.
-define(UPLOAD_READ_TIMEOUT, 120000).

-define(UPLOADED_PACKAGE_NAME, "gui_static.tar.gz").


-spec handle_service_gui_upload(cowboy_req:req()) -> cowboy_req:req() | no_return().
handle_service_gui_upload(Req) ->
    ServiceShortname = cowboy_req:binding(service, Req),
    ClusterId = cowboy_req:binding(cluster_id, Req),

    Service = try
        onedata:service_from_shortname(ServiceShortname)
    catch error:badarg ->
        throw(?HTTP_404_NOT_FOUND)
    end,
    lists:member(Service, [?OP_WORKER, ?OP_PANEL]) orelse throw(?HTTP_400_BAD_REQUEST),

    ProviderId = try cluster_logic:get(?ROOT, ClusterId) of
        {ok, #od_cluster{type = ?ONEPROVIDER}} -> ClusterId;
        _ -> throw(?HTTP_404_NOT_FOUND)
    catch _:_ ->
        throw(?HTTP_404_NOT_FOUND)
    end,

    case auth_logic:authorize_by_macaroons(Req) of
        {true, ?PROVIDER(ProviderId)} ->
            ?info("Received GUI upload for ~p:~s", [Service, ClusterId]),
            TempDir = mochitemp:mkdtemp(),
            UploadPath = filename:join([TempDir, ?UPLOADED_PACKAGE_NAME]),
            Req2 = try
                process_multipart(Req, UploadPath)
            catch Type:Message ->
                ?error_stacktrace("Error while streaming file upload from ~p:~s - ~p:~p", [
                    Service, ClusterId, Type, Message
                ]),
                throw(?HTTP_500_INTERNAL_SERVER_ERROR)
            end,
            Req3 = case gui_static:deploy_package(Service, UploadPath) of
                {ok, _} ->
                    cowboy_req:reply(?HTTP_200_OK, Req2);
                {error, _} = Error ->
                    ?debug("Discarding GUI upload from ~p:~s due to ~p", [
                        Service, ClusterId, Error
                    ]),
                    cowboy_req:reply(?HTTP_400_BAD_REQUEST, Req2)
            end,
            mochitemp:rmtempdir(TempDir),
            Req3;
        {true, _} ->
            throw(?HTTP_403_FORBIDDEN);
        _ ->
            throw(?HTTP_401_UNAUTHORIZED)
    end.


-spec handle_harvester_gui_upload(cowboy_req:req()) -> cowboy_req:req() | no_return().
handle_harvester_gui_upload(Req) ->
    HarvesterId = cowboy_req:binding(harvester_id, Req),

    case harvester_logic:get(?ROOT, HarvesterId) of
        {ok, _} -> ok;
        _ -> throw(?HTTP_404_NOT_FOUND)
    end,
    
    Token = case cowboy_req:header(<<"x-auth-token">>, Req, undefined)  of
        undefined -> throw(?HTTP_401_UNAUTHORIZED);
        T -> T
    end,   

    case auth_logic:authorize_by_onezone_gui_macaroon(Token) of
        {true, ?USER(UserId), _} ->
            case harvester_logic:has_eff_user(HarvesterId, UserId) of
                true -> ok;
                _ -> throw(?HTTP_403_FORBIDDEN)
            end,
            ?info("Received GUI upload for ~p:~s", [harvester, HarvesterId]),
            TempDir = mochitemp:mkdtemp(),
            UploadPath = filename:join([TempDir, ?UPLOADED_PACKAGE_NAME]),
            Req2 = try
                process_multipart(Req, UploadPath)
            catch Type:Message ->
                ?error_stacktrace("Error while streaming file upload from ~p:~s - ~p:~p", [
                    harvester, HarvesterId, Type, Message
                ]),
                throw(?HTTP_500_INTERNAL_SERVER_ERROR)
            end,
            Req3 = case gui_static:deploy_package(harvester, UploadPath) of
                {ok, GuiHash} ->
                    ok = gui_static:link_service_gui(harvester, HarvesterId, GuiHash),
                    cowboy_req:reply(?HTTP_200_OK, Req2);
                {error, _} = Error ->
                    ?debug("Discarding GUI upload from ~p:~s due to ~p", [
                        harvester, HarvesterId, Error
                    ]),
                    cowboy_req:reply(?HTTP_400_BAD_REQUEST, Req2)
            end,
            mochitemp:rmtempdir(TempDir),
            Req3;
        _ ->
            throw(?HTTP_401_UNAUTHORIZED)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Processes a multipart HTTP form and saves uploaded file body to given location.
%% @end
%%--------------------------------------------------------------------
-spec process_multipart(cowboy_req:req(), file:name_all()) -> cowboy_req:req().
process_multipart(Req, UploadPath) ->
    case cowboy_req:read_part(Req) of
        {ok, Headers, Req2} ->
            case cow_multipart:form_data(Headers) of
                {data, _FieldName} ->
                    % Multipart parameters are ignored
                    {ok, _, Req3} = cowboy_req:read_part_body(Req2),
                    process_multipart(Req3, UploadPath);
                {file, _FieldName, _Filename, _CType} ->
                    % Set options for reading from socket
                    Opts = #{
                        % length is chunk size - how much the cowboy read
                        % function returns at once.
                        length => ?UPLOAD_BUFFER_SIZE,
                        % Maximum timeout after which body read from request
                        % is passed to upload handler process.
                        % Note that the body is returned immediately
                        % if its size reaches the buffer size (length above).
                        period => ?UPLOAD_READ_PERIOD,
                        % read timeout - the read will fail if read_length
                        % is not satisfied within this time.
                        timeout => ?UPLOAD_READ_TIMEOUT
                    },
                    {ok, IoDevice} = file:open(UploadPath, [write]),
                    Req3 = stream_file(Req2, IoDevice, Opts),
                    process_multipart(Req3, UploadPath)
            end;
        {done, Req2} ->
            Req2
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stream a file upload to given IoDevice.
%% @end
%%--------------------------------------------------------------------
-spec stream_file(Req :: cowboy_req:req(), file:io_device(),
    cowboy_req:read_body_opts()) -> cowboy_req:req().
stream_file(Req, IoDevice, Opts) ->
    case cowboy_req:read_part_body(Req, Opts) of
        {ok, Body, Req2} ->
            ok = file:write(IoDevice, Body),
            file:close(IoDevice),
            Req2;
        {more, Body, Req2} ->
            ok = file:write(IoDevice, Body),
            stream_file(Req2, IoDevice, Opts)
    end.

