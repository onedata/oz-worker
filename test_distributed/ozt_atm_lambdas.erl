%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for manipulating automation lambdas of oz-worker service in CT tests.
%%% NOTE: to use ozt* modules, you must call ozt:init_per_suite/1 in the
%%% SUITE's init_per_suite/1 function.
%%% @end
%%%-------------------------------------------------------------------
-module(ozt_atm_lambdas).
-author("Lukasz Opiola").

-include("ozt.hrl").
-include_lib("ctool/include/automation/automation.hrl").

%% API
-export([list/0]).
-export([gen_example_data/0, gen_example_data/1]).
-export([create/1, create/2, create/3]).
-export([get/1]).
-export([get_atm_inventories/1]).
-export([add_to_inventory/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec list() -> [od_atm_lambda:id()].
list() ->
    {ok, AtmInventories} = ?assertMatch({ok, _}, ozt:rpc(atm_lambda_logic, list, [?ROOT])),
    AtmInventories.


-spec gen_example_data() -> entity_logic:data().
gen_example_data() ->
    #{
        <<"name">> => gen_example_data(name),
        <<"summary">> => gen_example_data(summary),
        <<"description">> => gen_example_data(description),

        <<"engine">> => gen_example_data(engine),
        <<"operationRef">> => gen_example_data(operation_ref),

        <<"executionOptions">> => gen_example_data(execution_options),
        <<"argumentSpecs">> => gen_example_data(argument_specs),
        <<"resultSpecs">> => gen_example_data(result_specs)
    }.


-spec gen_example_data(atom()) -> json_utils:json_term().
gen_example_data(name) -> <<"atm_lambda-", (?UNIQUE_STRING)/binary>>;
gen_example_data(summary) -> lists_utils:random_element([<<>>, str_utils:rand_hex(rand:uniform(50))]);
gen_example_data(description) -> lists_utils:random_element([<<>>, str_utils:rand_hex(rand:uniform(1000) + 50)]);
gen_example_data(engine) -> jsonable_record:to_json(
    lists_utils:random_element(atm_lambda_engine_type:allowed_types_for_custom_lambdas()),
    atm_lambda_engine_type
);
gen_example_data(operation_ref) -> str_utils:rand_hex(rand:uniform(15));
gen_example_data(execution_options) -> atm_lambda_execution_options:to_json(
    lists_utils:random_element([
        #atm_lambda_execution_options{},
        #atm_lambda_execution_options{
            readonly = lists_utils:random_element([true, false]),
            mount_space_options = #atm_mount_space_options{
                mount_oneclient = lists_utils:random_element([true, false]),
                mount_point = str_utils:rand_hex(rand:uniform(15)),
                oneclient_options = str_utils:rand_hex(rand:uniform(10))
            }
        }
    ])
);
gen_example_data(argument_specs) -> lists:map(fun(_) ->
    atm_lambda_argument_spec:to_json(#atm_lambda_argument_spec{
        name = ?UNIQUE_STRING,
        data_spec = atm_data_spec:from_json(gen_example_data(data_spec)),
        is_batch = lists_utils:random_element([true, false]),
        is_optional = lists_utils:random_element([true, false]),
        default_value = lists_utils:random_element([true, false, 6, #{}, <<"binary">>, #{<<"key">> => 984.222}])
    })
end, lists:seq(1, rand:uniform(5) - 1));
gen_example_data(result_specs) -> lists:map(fun(_) ->
    atm_lambda_result_spec:to_json(#atm_lambda_result_spec{
        name = ?UNIQUE_STRING,
        data_spec = atm_data_spec:from_json(gen_example_data(data_spec)),
        is_batch = lists_utils:random_element([true, false])
    })
end, lists:seq(1, rand:uniform(5) - 1));
gen_example_data(data_spec) ->
    RandomType = lists_utils:random_element([
        atm_integer_type, atm_string_type, atm_object_type, atm_file_type, atm_histogram_type,
        atm_dataset_type, atm_archive_type, atm_store_credentials_type, atm_onedatafs_credentials_type
    ]),
    RandomValueConstraints = case RandomType of
        atm_file_type ->
            lists_utils:random_element([#{}, #{file_type => lists_utils:random_element(['REG', 'DIR', 'ANY'])}]);
        atm_store_credentials_type ->
            #{store_type => lists_utils:random_element([single_value, list, map, forest, range, histogram])};
        _ ->
            #{}
    end,
    atm_data_spec:to_json(#atm_data_spec{
        type = RandomType,
        value_constraints = RandomValueConstraints
    }).


-spec create(od_atm_inventory:id()) -> od_atm_lambda:id().
create(AtmInventoryId) ->
    create(AtmInventoryId, gen_example_data()).

-spec create(od_atm_inventory:id(), entity_logic:data()) -> od_atm_lambda:id().
create(AtmInventoryId, Data) ->
    create(?ROOT, AtmInventoryId, Data).

-spec create(aai:auth(), od_atm_inventory:id(), entity_logic:data()) -> od_atm_lambda:id().
create(Auth, AtmInventoryId, Data) ->
    {ok, AtmLambdaId} = ?assertMatch({ok, _}, ozt:rpc(atm_lambda_logic, create, [Auth, Data#{
        <<"atmInventoryId">> => AtmInventoryId
    }])),
    AtmLambdaId.


-spec get(od_atm_lambda:id()) -> od_atm_lambda:record().
get(AtmLambdaId) ->
    {ok, AtmLambda} = ?assertMatch({ok, _}, ozt:rpc(atm_lambda_logic, get, [?ROOT, AtmLambdaId])),
    AtmLambda.


-spec get_atm_inventories(od_atm_lambda:id()) -> [od_atm_inventory:id()].
get_atm_inventories(AtmLambdaId) ->
    {ok, AtmInventories} = ?assertMatch({ok, _}, ozt:rpc(atm_lambda_logic, get_atm_inventories, [?ROOT, AtmLambdaId])),
    AtmInventories.


-spec add_to_inventory(od_atm_lambda:id(), od_atm_inventory:id()) -> ok.
add_to_inventory(AtmLambdaId, AtmInventoryId) ->
    ?assertMatch(ok, ozt:rpc(atm_lambda_logic, add_to_inventory, [?ROOT, AtmLambdaId, AtmInventoryId])).
