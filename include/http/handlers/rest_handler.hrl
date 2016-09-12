%%%-------------------------------------------------------------------
%%% @author Konrad Zemek
%%% @copyright (C): 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Common definitions for REST handlers.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(HANDLERS_REST_HANDLER_HRL).
-define(HANDLERS_REST_HANDLER_HRL, true).

%% Types describing atoms relating to REST methods.
-type accept_method() :: post | patch | put.
-type method() :: accept_method() | get | delete.

%% Type of data passed into accept_resource handlers.
-type json_string() :: atom() | binary().
-type json_number() :: integer() | float().
-type json_array() :: [json_term()].
-type json_object() :: [{json_string(), json_term()}].
-type json_term() :: json_string() | json_number() | json_array() | json_object().
-type qs_data() :: [{binary(), binary() | true}].
-type data() :: json_object() | qs_data().

%% A description of REST request's client.
-record(client, {
    type :: user | provider | undefined, %% client's type
    id = <<"">> :: binary()  %% client's ID in the database
}).

%% A record describing the state of REST request.
-record(rstate, {
    module :: undefined | module(),              %% identifier of the REST module handling request's details
    root :: undefined | atom(),                  %% name of the root resource
    resource :: undefined | atom(),              %% name of the requested resource
    methods :: undefined | [method()],           %% an array of REST methods the resource accepts
    client :: undefined | rest_handler:client(), %% the authenticated client's data
    noauth = [] :: [atom()]          %% list of methods not requiring a proper TLS authentication
}).

-type rstate() :: #rstate{}.


-endif. %% HANDLERS_REST_HANDLER_HRL
