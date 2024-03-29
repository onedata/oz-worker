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
-module(harvesting_backend_behaviour).


%%--------------------------------------------------------------------
%% @doc
%% Returns plugin name.
%% @end
%%--------------------------------------------------------------------
-callback get_name() -> binary().


%%--------------------------------------------------------------------
%% @doc
%% Checks availability of server located at Endpoint.
%% @end
%%--------------------------------------------------------------------
-callback ping(od_harvester:endpoint()) -> ok | {error, term()}.


%%--------------------------------------------------------------------
%% @doc
%% Creates index with given schema in server located at Endpoint.
%% @end
%%--------------------------------------------------------------------
-callback create_index(od_harvester:endpoint(), od_harvester:index_id(), 
    od_harvester:index(), od_harvester:schema()) -> ok | {error, term()}.


%%--------------------------------------------------------------------
%% @doc
%% Deletes given index from server located at Endpoint.
%% @end
%%--------------------------------------------------------------------
-callback delete_index(od_harvester:endpoint(), od_harvester:index_id()) ->
    ok | {error, term()}.


%%--------------------------------------------------------------------
%% @doc
%% Submits given batch in given index to server located at Endpoint.
%% @end
%%--------------------------------------------------------------------
-callback submit_batch(od_harvester:endpoint(), od_harvester:id(), od_harvester:indices(), 
    od_harvester:batch()) -> {ok, [{od_harvester:index_id(), od_harvester:index_submit_response()}]}.


%%--------------------------------------------------------------------
%% @doc
%% Performs query to server located at Endpoint. 
%% Data must conform to specification in query_validator/0.
%% @end
%%--------------------------------------------------------------------
-callback query_index(od_harvester:endpoint(), od_harvester:index_id(),
    Data :: map()) -> {ok, map()} | {error, term()}.


%%--------------------------------------------------------------------
%% @doc
%% Returns query validity verificator.
%% Returns a map with 'required', 'optional' and 'at_least_one' keys.
%% Under each of them, there is a map:
%%      Key => {type_verificator, value_verificator}
%% Which means how value of given Key should be validated.
%% @end
%%--------------------------------------------------------------------
-callback query_validator() -> entity_logic_sanitizer:sanitizer_spec().

