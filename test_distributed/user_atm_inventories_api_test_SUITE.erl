%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Empty test SUITE to be implemented in the future.
%%% @end
%%%-------------------------------------------------------------------
-module(user_atm_inventories_api_test_SUITE).
-author("Lukasz Opiola").

-include("registered_names.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([all/0]).
-export([dummy_test/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

all() -> ?ALL([dummy_test]).

dummy_test(_Config) ->
    ok.
