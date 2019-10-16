%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module implements dynamic_page_behaviour and is called to handle
%%% GUI package uploads from OP worker/panel services or harvesters.
%%%
%%% Possible errors:
%%%     400 - the uploaded GUI package is not verified (checksum is not whitelisted)
%%%     401 - not authorized
%%%     413 - uploaded GUI package is too large
%%%     415 - uploaded GUI package is in wrong format (non .tar.gz)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(page_gui_upload).
-author("Lukasz Opiola").

-behaviour(dynamic_page_behaviour).

-include("http/rest.hrl").
-include("datastore/oz_datastore_models.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/privileges.hrl").

%% Cowboy API
-export([handle/2]).

-define(UPLOAD_BUFFER_SIZE, 262144).
%% Maximum timeout after which body read from request is passed to
%% upload handler process. Note that the body is returned immediately
%% if its size reaches the buffer size (UPLOAD_BUFFER_SIZE).
-define(UPLOAD_READ_PERIOD, 5000).
%% Timeout for reading UPLOAD_BUFFER_SIZE bytes from socket.
-define(UPLOAD_READ_TIMEOUT, 120000).

-define(UPLOADED_PACKAGE_NAME, "gui_static.tar.gz").

%% ====================================================================
%% Cowboy API functions
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link dynamic_page_behaviour} callback handle/2.
%% @end
%%--------------------------------------------------------------------
-spec handle(gui:method(), cowboy_req:req()) -> cowboy_req:req().
handle(<<"POST">>, Req) ->
    try
        handle_gui_upload(Req)
    catch
        throw:{error, _} = Error ->
            cowboy_req:reply(errors:to_http_code(Error), #{}, json_utils:encode(errors:to_json(Error)), Req);
        throw:Code when is_integer(Code) ->
            cowboy_req:reply(Code, Req);
        Type:Reason ->
            ?error_stacktrace("Error while processing GUI upload - ~p:~p", [Type, Reason]),
            cowboy_req:reply(?HTTP_500_INTERNAL_SERVER_ERROR, Req)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @private
-spec handle_gui_upload(cowboy_req:req()) -> cowboy_req:req() | no_return().
handle_gui_upload(Req) ->
    GuiPrefix = cowboy_req:binding(gui_prefix, Req),
    GuiId = cowboy_req:binding(gui_id, Req),

    GuiType = try
        onedata:gui_by_prefix(GuiPrefix)
    catch error:badarg ->
        throw(?HTTP_404_NOT_FOUND)
    end,

    ServiceReleaseVersion = validate_and_authorize(GuiType, GuiId, Req),

    case stream_and_deploy_package(Req, GuiType, GuiId, ServiceReleaseVersion) of
        {ok, GuiHash, Req2} ->
            % Harvester GUIs are linked during upload, service GUIs are linked
            % during cluster version update
            GuiType =:= ?HARVESTER_GUI andalso gui_static:link_gui(GuiType, GuiId, GuiHash),
            cowboy_req:reply(?HTTP_200_OK, Req2);
        {error, _} = Error ->
            cowboy_req:reply(errors:to_http_code(Error), #{}, json_utils:encode(errors:to_json(Error)), Req)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Validates and authorizes GUI upload request based on GUI type and Id.
%% @end
%%--------------------------------------------------------------------
-spec validate_and_authorize(onedata:gui(), gui_static:gui_id(), cowboy_req:req()) ->
    onedata:release_version() | no_return().
validate_and_authorize(?HARVESTER_GUI, HarvesterId, Req) ->
    harvester_logic:exists(HarvesterId) orelse throw(?HTTP_404_NOT_FOUND),
    case token_auth:check_token_auth(Req) of
        {true, ?USER(UserId)} ->
            case harvester_logic:has_eff_privilege(HarvesterId, UserId, ?HARVESTER_UPDATE)
                orelse user_logic:has_eff_oz_privilege(UserId, ?OZ_HARVESTERS_UPDATE) of
                true ->
                    % Harvester packages are versioned along with Onezone
                    oz_worker:get_release_version();
                _ ->
                    throw(?HTTP_403_FORBIDDEN)
            end;
        _ ->
            throw(?HTTP_401_UNAUTHORIZED)
    end;
% Covers all service GUIs
validate_and_authorize(GuiType, ClusterId, Req) ->
    Service = onedata:service_by_gui(GuiType, ClusterId),
    lists:member(Service, [?OP_WORKER, ?OP_PANEL]) orelse throw(?HTTP_404_NOT_FOUND),
    Cluster = try cluster_logic:get(?ROOT, ClusterId) of
        {ok, #od_cluster{type = ?ONEPROVIDER} = Cl} -> Cl;
        _ -> throw(?HTTP_404_NOT_FOUND)
    catch _:_ ->
        throw(?HTTP_404_NOT_FOUND)
    end,

    {ReleaseVersion, _, _} = case Service of
        ?OP_WORKER -> Cluster#od_cluster.worker_version;
        ?OP_PANEL -> Cluster#od_cluster.onepanel_version
    end,

    case token_auth:check_token_auth(Req) of
        {true, ?PROVIDER(ClusterId)} ->
            ReleaseVersion;
        {true, _} ->
            throw(?HTTP_403_FORBIDDEN);
        _ ->
            throw(?HTTP_401_UNAUTHORIZED)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reads package from Req and deploys it on cluster nodes.
%% @end
%%--------------------------------------------------------------------
-spec stream_and_deploy_package(cowboy_req:req(), onedata:gui(),
    gui_static:gui_id(), onedata:release_version()) ->
    {ok, onedata:gui_hash(), cowboy_req:req()} | {error, term()}.
stream_and_deploy_package(Req, GuiType, GuiId, ServiceReleaseVersion) ->
    GuiPrefix = onedata:gui_prefix(GuiType),
    ?debug("Received GUI upload for ~s:~s", [GuiPrefix, GuiId]),
    TempDir = mochitemp:mkdtemp(),
    UploadPath = filename:join([TempDir, ?UPLOADED_PACKAGE_NAME]),

    try
        Req2 = process_multipart(Req, UploadPath),
        case gui_static:deploy_package(GuiType, ServiceReleaseVersion, UploadPath) of
            {ok, GuiHash} ->
                {ok, GuiHash, Req2};
            {error, _} = Error ->
                Error
        end
    catch Type:Message ->
        ?error_stacktrace("Error while streaming GUI upload for ~s:~s - ~p:~p", [
            GuiPrefix, GuiId, Type, Message
        ]),
        ?ERROR_INTERNAL_SERVER_ERROR
    after
        mochitemp:rmtempdir(TempDir)
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
