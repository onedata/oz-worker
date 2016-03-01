%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(subscriptions).
-author("Michal Zmuda").

-include("registered_names.hrl").
-include("datastore/oz_datastore_models_def.hrl").
-include_lib("ctool/include/logging.hrl").

-export([renew/3]).

-spec renew(ProviderID :: binary(), LastSeenSeq :: non_neg_integer(), Endpoint :: binary()) -> no_return().
renew(ProviderID, LastSeenSeq, Endpoint) ->
    ?info("Adding subscription ~p", [[ProviderID, LastSeenSeq, Endpoint]]),
    worker_proxy:call({subscriptions_worker, node()}, {provider_subscribe, ProviderID, Endpoint, LastSeenSeq}).


