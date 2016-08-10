%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This worker periodically refreshes crucial data published to dht.
%%% @end
%%%-------------------------------------------------------------------
-module(identity_publisher_worker).
-author("Michal Zmuda").

-behaviour(worker_plugin_behaviour).

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("public_key/include/public_key.hrl").

%% worker_plugin_behaviour callbacks
-export([init/1, handle/1, cleanup/0]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initialises the worker and schedules stream presence checks.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: worker_host:plugin_state()} | {error, Reason :: term()}.
init(_Args) ->
    {ok, KeyFile} = application:get_env(?APP_Name, identity_key_file),
    {ok, CertFile} = application:get_env(?APP_Name, identity_cert_file),
    {ok, Domain} = application:get_env(?APP_Name, http_domain),
    identity:ensure_identity_cert_created(KeyFile, CertFile, Domain),

    schedule_cert_refresh(),
    {ok, #{}}.

%%--------------------------------------------------------------------
%% @doc
%% Handles requests.
%% @end
%%--------------------------------------------------------------------
-spec handle(Request :: term()) -> ok | {error, Reason :: term()} | no_return().
handle(healthcheck) ->
    ok;
%%    DecodedCertificate = read_oz_cert(),
%%    case identity:verify_with_dht(DecodedCertificate) of
%%        ok -> ok;
%%        {error, Reason} -> {error, {cert_not_published, Reason}}
%%    end;

handle(refresh_published_pubkey) ->
    try
        ?debug("Refreshing published public key"),
        {ok, CertFile} = application:get_env(?APP_Name, identity_cert_file),
        DecodedCertificate = identity:read_cert(CertFile),
        ok = identity:publish(DecodedCertificate)
    catch
        E:R ->
            %% exceptions are handled as we have to ensure refresh is scheduled
            ?error_stacktrace("Unexpected refresh failure due to ~p:~p", [E, R])
    end,
    schedule_cert_refresh();

handle(_Request) ->
    ?log_bad_request(_Request).

%%--------------------------------------------------------------------
%% @doc
%% Cleans up the worker.
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok | {error, Reason :: term()}.
cleanup() ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @private
%% Schedules public key refresh.
%% @end
%%--------------------------------------------------------------------
-spec schedule_cert_refresh() -> ok.
schedule_cert_refresh() ->
    {ok, _} = timer:send_after(refresh_interval(), whereis(?MODULE),
        {timer, refresh_published_pubkey}),
    ok.

-spec refresh_interval() -> integer().
refresh_interval() ->
    {ok, Interval} = application:get_env(?APP_Name, public_key_refresh_interval_seconds),
    timer:seconds(Interval).
