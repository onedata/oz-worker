%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @doc
%%% WRITEME
%%% @end
%%%-------------------------------------------------------------------
-module(oai_protocol_plugin).
-author("Jakub Kudzia").
-behaviour(protocol_plugin_behaviour).

%% API
-export([routes/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns routes to cdmi protocol.
%% @end
%%--------------------------------------------------------------------
-spec routes() -> [{Route :: string(), protocol_plugin_behaviour:handler()}].
routes() ->
    [
        {"/oai_pmh/[...]", #{
            handler => oai_get_handler
        }},
        {"/oai_pmh", #{
            handler => oai_post_handler
        }}
    ].


%%%===================================================================
%%% Internal functions
%%%===================================================================
