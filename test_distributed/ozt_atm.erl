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
-export([gen_example_data_spec/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec gen_example_id() -> automation:id().
gen_example_id() ->
    ?RAND_STR().

-spec gen_example_name() -> automation:name().
gen_example_name() ->
    ?RAND_STR(rand:uniform(10) + 2).


-spec gen_example_summary() -> automation:summary().
gen_example_summary() ->
    lists_utils:random_element([<<>>, ?RAND_STR(rand:uniform(50))]).


-spec gen_example_description() -> automation:description().
gen_example_description() ->
    lists_utils:random_element([<<>>, ?RAND_STR(rand:uniform(1000) + 50)]).


-spec gen_example_store_type() -> automation:store_type().
gen_example_store_type() ->
    lists_utils:random_element([single_value, list, map, tree_forest, range, histogram]).


-spec gen_example_data_spec() -> atm_data_spec:record().
gen_example_data_spec() ->
    RandomType = lists_utils:random_element([
        atm_integer_type, atm_string_type, atm_object_type, atm_file_type, atm_histogram_type,
        atm_dataset_type, atm_archive_type, atm_store_credentials_type, atm_onedatafs_credentials_type
    ]),
    RandomValueConstraints = case RandomType of
        atm_file_type ->
            lists_utils:random_element([#{}, #{file_type => lists_utils:random_element(['REG', 'DIR', 'ANY'])}]);
        atm_store_credentials_type ->
            #{store_type => gen_example_store_type()};
        _ ->
            #{}
    end,
    #atm_data_spec{
        type = RandomType,
        value_constraints = RandomValueConstraints
    }.
