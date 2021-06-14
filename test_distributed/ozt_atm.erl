%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common functions related to automation to be used in in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_atm).
-author("Lukasz Opiola").

-include("ozt.hrl").
-include_lib("ctool/include/automation/automation.hrl").

%% API
-export([gen_example_id/0]).
-export([gen_example_name/0]).
-export([gen_example_summary/0]).
-export([gen_example_description/0]).
-export([gen_example_store_type/0]).
-export([gen_example_data_spec/0, gen_example_data_spec/1]).
-export([gen_example_initial_value/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec gen_example_id() -> automation:id().
gen_example_id() ->
    ?UNIQUE_STRING.

-spec gen_example_name() -> automation:name().
gen_example_name() ->
    ?UNIQUE_STRING.


-spec gen_example_summary() -> automation:summary().
gen_example_summary() ->
    lists_utils:random_element([<<>>, ?RAND_STR(rand:uniform(50))]).


-spec gen_example_description() -> automation:description().
gen_example_description() ->
    lists_utils:random_element([<<>>, ?RAND_STR(rand:uniform(1000) + 50)]).


-spec gen_example_store_type() -> automation:store_type().
gen_example_store_type() ->
    lists_utils:random_element(automation:all_store_types()).


-spec gen_example_data_spec() -> atm_data_spec:record().
gen_example_data_spec() ->
    gen_example_data_spec(lists_utils:random_element(atm_data_type:all_data_types())).

-spec gen_example_data_spec(atm_data_type:type()) -> atm_data_spec:record().
gen_example_data_spec(atm_file_type) ->
    #atm_data_spec{
        type = atm_file_type,
        value_constraints = lists_utils:random_element([#{}, #{file_type => lists_utils:random_element(['REG', 'DIR', 'ANY'])}])
    };
gen_example_data_spec(atm_store_credentials_type) ->
    #atm_data_spec{
        type = atm_store_credentials_type,
        value_constraints = #{store_type => gen_example_store_type()}
    };
gen_example_data_spec(DataType) ->
    #atm_data_spec{
        type = DataType,
        value_constraints = #{}
    }.


-spec gen_example_initial_value(atm_data_type:type()) -> json_utils:json_term().
gen_example_initial_value(atm_integer_type) ->
    ?RAND_INT(0, 1000);
gen_example_initial_value(atm_string_type) ->
    ?RAND_STR(?RAND_INT(1, 25));
gen_example_initial_value(atm_object_type) ->
    lists_utils:random_element([#{}, #{<<"key">> => 984.222}, #{<<"key">> => #{<<"nested">> => 984.222}}]);
%% @TODO VFS-7687 Implement all automation data types and validators
gen_example_initial_value(atm_file_type) ->
    #{<<"atm_file_type">> => <<"value">>};
gen_example_initial_value(atm_histogram_type) ->
    [1, 2, 3, 4];
gen_example_initial_value(atm_dataset_type) ->
    #{<<"atm_dataset_type">> => <<"value">>};
gen_example_initial_value(atm_archive_type) ->
    #{<<"atm_archive_type">> => <<"value">>};
gen_example_initial_value(atm_store_credentials_type) ->
    undefined;
gen_example_initial_value(atm_onedatafs_credentials_type) ->
    undefined.
