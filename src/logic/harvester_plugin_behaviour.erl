%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour specifies the API for an harvester plugin.
%%% @end
%%%-------------------------------------------------------------------
-module(harvester_plugin_behaviour).


-callback submit_entry(Endpoint :: binary(), IndexId :: binary(), Id :: binary(), Data :: binary()) -> 
    ok | {error, term()}.

-callback delete_entry(Endpoint :: binary(), IndexId :: binary(), Id :: binary()) -> 
    ok | {error, term()}.

-callback query(Endpoint :: binary(), IndexId :: binary(), Request :: binary()) -> 
    {ok, binary()} | {error, term()}.
