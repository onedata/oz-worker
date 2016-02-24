%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin for DNS worker.
%%% @end
%%%-------------------------------------------------------------------
-module(dns_worker_plugin).
-author("Michal Zmuda").

-behavior(dns_worker_plugin_behaviour).

-include_lib("ctool/include/logging.hrl").

-export([resolve/3]).

%%--------------------------------------------------------------------
%% @doc
%% {@link dns_worker_plugin_behaviour} callback resolve/3.
%% @end
%%--------------------------------------------------------------------
-spec resolve(Method :: atom(), Domain :: string(), LBAdvice :: term()) ->
    dns_handler_behaviour:handler_reply().

resolve(Method, Domain, LBAdvice) ->
    dns_query_handler:Method(Domain, LBAdvice).