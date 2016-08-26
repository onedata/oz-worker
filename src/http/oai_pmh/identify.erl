%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(identify).
-author("Jakub Kudzia").

%% API
-export([]).

-include("http/handlers/oai.hrl").

-record(response, {
   repositoryName :: binary(),
    baseURL :: binary(), % todo ? <<"www.onedata.org">>
    protocolVersion :: ?SUPPORTED_VERSION,
    earliestDatestamp :: term(), %TODO date in format of oai
    deletedRecord :: no | transient | persistent, % todo maybe shoudl be binary
    granularity :: oai_granularity(),
    adminEmail :: [binary()],
    compression, % todo
    desription % todo
}).