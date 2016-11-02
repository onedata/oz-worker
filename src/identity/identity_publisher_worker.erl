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
-export([init/1, handle/1, cleanup/0, start_refreshing/0]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts scheduled refreshing of owned public keys.
%% @end
%%--------------------------------------------------------------------
-spec start_refreshing() -> ok.
start_refreshing() ->
    {ok, KeyFile} = application:get_env(?APP_Name, identity_key_file),
    {ok, CertFile} = application:get_env(?APP_Name, identity_cert_file),
    {ok, Domain} = application:get_env(?APP_Name, http_domain),
    ok = identity_utils:ensure_synced_cert_present(KeyFile, CertFile, Domain),
    refresh(),
    schedule_cert_refresh().

%%--------------------------------------------------------------------
%% @doc
%% Initialises the worker and schedules stream presence checks.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: worker_host:plugin_state()} | {error, Reason :: term()}.
init(_Args) ->
    {ok, #{}}.

%%--------------------------------------------------------------------
%% @doc
%% Handles requests.
%% @end
%%--------------------------------------------------------------------
-spec handle(Request :: term()) -> ok | {error, Reason :: term()} | no_return().
handle(healthcheck) ->
    ok;

handle(refresh_published_pubkey) ->
    try
        ok = refresh()
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

-spec refresh() -> ok | {error, Reason :: term()}.
refresh() ->
    ?debug("Refreshing published public key"),
    {ok, CertFile} = application:get_env(?APP_Name, identity_cert_file),
    DecodedCertificate = identity_utils:read_cert(CertFile),
    identity:publish(DecodedCertificate),

    {ok, Docs} = owned_identity:list(),
    Results = utils:pmap(fun(#document{value = #owned_identity{id = ID, encoded_public_key = Encoded}}) ->
        case plugins:apply(identity_repository, publish, [ID, Encoded]) of
            {error, Reason} -> #{id => ID, encoded => Encoded, error => Reason};
            ok -> ok
        end
    end, Docs),

    case lists:usort(Results) of
        [] -> ?warning("No certs to publish from this OZ");
        [ok] -> ok;
        UniqueResults ->
            Errors = UniqueResults -- [ok],
            ?warning("Errors on publishing owned certs ~p", [Errors]),
            {error, {at_least_one_publish_failed, Errors}}
    end.
