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
-export([create/1, create/2, create/3]).
-export([get/1]).
-export([exists/1]).
-export([get_atm_inventories/1]).
-export([add_to_inventory/2]).
%% Example data generation
-export([gen_example_data/0]).
-export([gen_example_operation_spec/0, gen_example_argument_specs/0, gen_example_result_specs/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec list() -> [od_atm_lambda:id()].
list() ->
    {ok, AtmLambdas} = ?assertMatch({ok, _}, ozt:rpc(atm_lambda_logic, list, [?ROOT])),
    AtmLambdas.


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


-spec exists(od_atm_lambda:id()) -> boolean().
exists(AtmLambdaId) ->
    ozt:rpc(atm_lambda_logic, exists, [AtmLambdaId]).


-spec get_atm_inventories(od_atm_lambda:id()) -> [od_atm_inventory:id()].
get_atm_inventories(AtmLambdaId) ->
    {ok, AtmInventories} = ?assertMatch({ok, _}, ozt:rpc(atm_lambda_logic, get_atm_inventories, [?ROOT, AtmLambdaId])),
    AtmInventories.


-spec add_to_inventory(od_atm_lambda:id(), od_atm_inventory:id()) -> ok.
add_to_inventory(AtmLambdaId, AtmInventoryId) ->
    ?assertMatch(ok, ozt:rpc(atm_lambda_logic, add_to_inventory, [?ROOT, AtmLambdaId, AtmInventoryId])).

%%%===================================================================
%%% Example data generation
%%%===================================================================

-spec gen_example_data() -> entity_logic:data().
gen_example_data() ->
    #{
        <<"name">> => ozt_atm:gen_example_name(),
        <<"summary">> => ozt_atm:gen_example_summary(),
        <<"description">> => ozt_atm:gen_example_description(),

        <<"operationSpec">> => gen_example_operation_spec(),
        <<"argumentSpecs">> => gen_example_argument_specs(),
        <<"resultSpecs">> => gen_example_result_specs()
    }.


-spec gen_example_operation_spec() -> json_utils:json_term().
gen_example_operation_spec() -> jsonable_record:to_json(
    lists_utils:random_element([
        #atm_openfaas_operation_spec{
            docker_image = ?RAND_STR(),
            docker_execution_options = #atm_docker_execution_options{
                readonly = ?RAND_BOOL(),
                mount_oneclient = ?RAND_BOOL(),
                oneclient_mount_point = <<"/a/b/c/d/", (?RAND_STR())/binary>>,
                oneclient_options = lists_utils:random_element([<<"">>, <<"--a --b">>])
            }
        },
        #atm_workflow_operation_spec{
            atm_workflow_id = ?RAND_STR()
        },
        #atm_user_form_operation_spec{
            user_form_id = ?RAND_STR()
        }
    ]), atm_lambda_operation_spec).


-spec gen_example_argument_specs() -> json_utils:json_term().
gen_example_argument_specs() -> lists:map(fun(_) ->
    jsonable_record:to_json(#atm_lambda_argument_spec{
        name = ?UNIQUE_STRING,
        data_spec = ozt_atm:gen_example_data_spec(),
        is_batch = ?RAND_BOOL(),
        is_optional = ?RAND_BOOL(),
        default_value = lists_utils:random_element([true, false, 6, #{}, <<"binary">>, #{<<"key">> => 984.222}])
    }, atm_lambda_argument_spec)
end, lists:seq(1, ?RAND_INT(0, 5))).


-spec gen_example_result_specs() -> json_utils:json_term().
gen_example_result_specs() -> lists:map(fun(_) ->
    jsonable_record:to_json(#atm_lambda_result_spec{
        name = ?UNIQUE_STRING,
        data_spec = ozt_atm:gen_example_data_spec(),
        is_batch = ?RAND_BOOL()
    }, atm_lambda_result_spec)
end, lists:seq(1, ?RAND_INT(0, 5))).