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


%%--------------------------------------------------------------------
%% @doc
%% Submits given Data to server located at Endpoint.
%% @end
%%--------------------------------------------------------------------
-callback submit_entry(Endpoint :: binary(), HarvesterId :: binary(), 
    Id :: binary(), Data :: binary()) -> ok | {error, term()}.

%%--------------------------------------------------------------------
%% @doc
%% Deletes entry in server located at Endpoint. 
%% @end
%%--------------------------------------------------------------------
-callback delete_entry(Endpoint :: binary(), HarvesterId :: binary(),  
    Id :: binary()) -> ok | {error, term()}.


%%--------------------------------------------------------------------
%% @doc
%% Performs query to server located at Endpoint. 
%% Data must conform to specification in query_validator/0.
%% @end
%%--------------------------------------------------------------------
-callback query(Endpoint :: binary(), IndexId :: binary(), Data :: #{}) -> 
    {ok, maps:map()} | {error, term()}.

%%--------------------------------------------------------------------
%% @doc
%% Returns query validity verificator.
%% Returns a map with 'required', 'optional' and 'at_least_one' keys.
%% Under each of them, there is a map:
%%      Key => {type_verificator, value_verificator}
%% Which means how value of given Key should be validated.
%% @end
%%--------------------------------------------------------------------
-callback query_validator() -> entity_logic:validity_verificator().